#lang racket/base
(require racket/match
         racket/format
         racket/set)
(module+ test
  (require chk))

;; Utility
(define (snoc l x) (append l (list x)))
(define (sort-symbols l)
  (sort l string<=? #:key symbol->string))
(define (set->sorted-list s)
  (sort-symbols (set->list s)))

(define fresh-box (make-parameter #f))
(define-syntax-rule (with-fresh b ...)
  (begin
    (when (fresh-box)
      (error 'with-fresh "Should not use with-fresh twice in same context!"))
    (parameterize ([fresh-box (box 0)])
      b ...)))
(define (freshen s)
  (define tfb (fresh-box))
  (define fresh-count (unbox tfb))
  (set-box! tfb (add1 fresh-count))
  (string->symbol (~a s fresh-count)))

;; At a high-level, a program is written in a direct-style where the
;; communication primitives are synchronous and return values. The
;; program has a set of top-level functions and a distinguished
;; initial function. It assumed that execution begins via the initial
;; function receive some extra arguments.
(struct dp:program (n->fun in) #:transparent)
(struct df:fun (args body) #:transparent)

(struct direct-expr () #:transparent)
(struct de:var direct-expr (v) #:transparent)
(struct de:con direct-expr (b) #:transparent)
(struct de:let direct-expr (x xe be) #:transparent)
(struct de:if direct-expr (ce te fe) #:transparent)
(struct de:app direct-expr (op args) #:transparent)
(struct de:send direct-expr (e) #:transparent)
(struct de:recv direct-expr () #:transparent)

(define (de:let* x*xes be)
  (for/fold ([be be]) ([x*xe (in-list (reverse x*xes))])
    (de:let (car x*xe) (cdr x*xe) be)))
(define _seq_ (gensym '_))
(define (de:seq f s)
  (de:let _seq_ f s))

(module+ test
  (define de:add-some-numbers
    (dp:program
     (hasheq
      'zero?
      (df:fun
       '(x)
       (de:app '= (list (de:con 0) (de:var 'x))))
      'add3
      (df:fun
       '(x y z)
       (de:app '+
               (list (de:var 'x)
                     (de:app '+ (list (de:var 'y)
                                      (de:var 'z))))))
      'add-some-numbers
      (df:fun
       '()
       (de:app 'add3
               (list (de:recv)
                     (de:seq (de:send (de:con 1))
                             (de:recv))
                     (de:if (de:app 'zero? (list (de:recv)))
                            (de:recv)
                            (de:con 0))))))
     'add-some-numbers)))

;; At a low-level, a program is a mapping from named states (a finite
;; set of labels) to a handler function that consumes the state
;; representation, the most recent message, and returns a named state,
;; a new state representation, and a block of messages to send. There
;; is a special initial handler that receives the initial state and an
;; empty message, which is ignored.
(struct hp:program (n->handler in) #:transparent)
(struct hh:handler (st msg ht) #:transparent)

(struct handle-tail () #:transparent)
(struct ht:let handle-tail (x xe ht) #:transparent)
(struct ht:if handle-tail (ce tt ft) #:transparent)
(struct ht:wait handle-tail (ns st msgs) #:transparent)
(struct ht:stop handle-tail (st msgs) #:transparent)

(struct handle-expr () #:transparent)
;; XXX rename ha:
(struct he:var handle-expr (v) #:transparent)
(struct he:con handle-expr (b) #:transparent)
(struct he:app handle-expr (op args) #:transparent)

;; The compiler transforms a direct program into a handler program
(define (merge-non-overlapping-hash x y)
  (for/fold ([x x]) ([(k v) (in-hash y)])
    (when (hash-has-key? x k)
      (error 'merge-non-overlapping-hash "Hash cannot overlap!"))
    (hash-set x k v)))
(define (merge-non-overlapping-hashes xs)
  (for/fold ([y (hasheq)]) ([x (in-list xs)])
    (merge-non-overlapping-hash x y)))

(define (dp-anf dp)
  (struct anf-res (nvs e))
  (define (da-anf n->fun ρ tail? e)
    (match e
      [(de:var v)
       (anf-res '() (hash-ref ρ v))]
      [(de:con b)
       (anf-res '() e)]
      [(de:let x xe be)
       (match-define (anf-res nvs1 xe1)
         (da-anf n->fun ρ #f xe))
       (match-define (anf-res nvs2 be1)
         (da-anf n->fun (hash-set ρ x xe1) tail? be))
       (anf-res (append nvs1 nvs2) be1)]
      [(de:if ce te fe)
       (match-define (anf-res nvs1 ce1)
         (da-anf n->fun ρ #f ce))
       (define te1 (de-anf n->fun ρ tail? te))
       (define fe1 (de-anf n->fun ρ tail? fe))
       (define e1 (de:if ce1 te1 fe1))
       (cond
         [tail?
          (anf-res nvs1 e1)]
         [else
          (define nv (freshen 'anf-if))
          (anf-res (snoc nvs1 (cons nv e1)) (de:var nv))])]
      [(de:app op aes)
       (match-define (anf-res nvs1 aes1)
         (for/fold ([nvs0 '()]
                    [raes0 '()]
                    #:result (anf-res nvs0 (reverse raes0)))
                   ([ae (in-list aes)])
           (match-define (anf-res nvs1 ae1)
             (da-anf n->fun ρ #f ae))
           (values (append nvs0 nvs1)
                   (cons ae1 raes0))))
       (cond
         [(hash-ref n->fun op #f)
          (match-define (anf-res nvs2 call1)
            (df-anf da-anf n->fun ρ tail? op aes1))
          (anf-res (append nvs1 nvs2) call1)]
         [else
          (define nv (freshen 'anf-app))
          (anf-res (snoc nvs1 (cons nv (de:app op aes1)))
                   (de:var nv))])]
      [(de:send e)
       (define nv (freshen 'anf-send))
       (match-define (anf-res nvs1 e1) (da-anf n->fun ρ #f e))
       (anf-res (snoc nvs1 (cons nv (de:send e1))) (de:var nv))]
      [(de:recv)
       (cond
         [tail?
          (anf-res '() e)]
         [else
          (define nv (freshen 'anf-recv))
          (anf-res (list (cons nv (de:recv))) (de:var nv))])]))
  (define (de-anf n->fun ρ tail? e)
    (match-define (anf-res nvs e1) (da-anf n->fun ρ tail? e))
    (de:let* nvs e1))
  (define (df-anf d*-anf n->fun ρ tail? op vals)
    (match-define (df:fun args body) (hash-ref n->fun op))
    (define ρ1 (for/fold ([ρ ρ]) ([a (in-list args)]
                                  [v (in-list vals)])
                 (hash-set ρ a v)))
    (d*-anf (hash-remove n->fun op) ρ1 tail? body))

  (with-fresh
    (match-define (dp:program n->fun in) dp)
    (match-define (df:fun args _) (hash-ref n->fun in))
    (define e1 (df-anf de-anf n->fun (hasheq) #t in
                       (for/list ([a (in-list args)])
                         (de:var a))))
    (df:fun args e1)))
(module+ test
  (chk
   (dp-anf de:add-some-numbers)
   (df:fun
    '()
    (de:let* (list (cons 'anf-recv0 (de:recv))
                   (cons 'anf-send1 (de:send (de:con 1)))
                   (cons 'anf-recv2 (de:recv))
                   (cons 'anf-recv3 (de:recv))
                   (cons 'anf-app4
                         (de:app '= (list (de:con 0)
                                          (de:var 'anf-recv3))))
                   (cons 'anf-if6
                         (de:if (de:var 'anf-app4)
                                (de:let 'anf-recv5
                                        (de:recv)
                                        (de:var 'anf-recv5))
                                (de:con 0)))
                   (cons 'anf-app7
                         (de:app '+ (list (de:var 'anf-recv2)
                                          (de:var 'anf-if6))))
                   (cons 'anf-app8
                         (de:app '+ (list (de:var 'anf-recv0)
                                          (de:var 'anf-app7)))))
             (de:var 'anf-app8)))))

(define (dp-state df)
  (struct st-res (n->h fvs ht) #:transparent)
  (define (da-state e)
    (match e
      [(de:var v) (values (seteq v) (he:var v))]
      [(de:con b) (values (seteq) (he:con b))]))
  (define (das-state es)
    (match es
      ['() (values (seteq) '())]
      [(cons a d)
       (define-values (fvs1 a1) (da-state a))
       (define-values (fvs2 d1) (das-state d))
       (values (set-union fvs1 fvs2) (cons a1 d1))]))
  (define (de-merge kv k e)
    (match e
      [(de:let v va b)
       (de:let v va (de-merge kv k b))]
      [(de:if ca te fe)
       (de:if ca (de-merge kv k te) (de-merge kv k fe))]
      [arg
       (de:let kv arg k)]))
  (define (de-state pre-sends e)
    (match e
      [(and a (or (de:var _) (de:con _)))
       (define-values (a-fvs a1) (da-state a))
       (st-res (hasheq) a-fvs (ht:stop a1 pre-sends))]
      ;; XXX add renaming
      [(de:let v (and va (or (de:var _) (de:con _))) b)
       (define-values (va-fvs va1) (da-state va))
       (match-define (st-res n->h1 b-fvs bt)
         (de-state pre-sends b))
       (st-res n->h1 (set-union va-fvs (set-remove b-fvs v))
               (ht:let v va1 bt))]
      [(de:if ca te fe)
       (define-values (ca-fvs ca1) (da-state ca))
       (match-define (st-res n->h1 te-fvs te1)
         (de-state pre-sends te))
       (match-define (st-res n->h2 fe-fvs fe1)
         (de-state pre-sends fe))
       (st-res (merge-non-overlapping-hash n->h1 n->h2)
               (set-union ca-fvs te-fvs fe-fvs)
               (ht:if ca1 te1 fe1))]
      [(de:let v (de:if ca te fe) b)
       (de-state pre-sends
                 (de:if ca
                        ;; XXX potentially add a jump because this is
                        ;; expensive
                        (de-merge v b te)
                        (de-merge v b fe)))]
      [(de:let v (de:app op args) b)
       (define-values (args-fvs args1) (das-state args))
       (match-define (st-res n->h1 fvs ht)
         (de-state pre-sends b))
       (st-res n->h1 (set-union args-fvs (set-remove fvs v))
               (ht:let v (he:app op args1)
                       ht))]
      [(de:let v (de:send a) b)
       (define-values (a-fvs a1) (da-state a))
       (match-define (st-res n->h1 fvs ht)
         (de-state (snoc pre-sends a1) b))
       (st-res n->h1 (set-union a-fvs (set-remove fvs v)) ht)]
      [(de:let v (de:recv) b)
       (define nh (freshen 'recv))
       (match-define (st-res n->h1 fvs ht)
         (de-state '() b))
       (define fvs1 (set-remove fvs v))
       (define st (set->sorted-list fvs1))
       (st-res
        (hash-set n->h1 nh (hh:handler st v ht))
        fvs1
        (ht:wait nh st pre-sends))]))
  (with-fresh
    (match-define (df:fun args body) df)
    (define nh (freshen 'top))
    (match-define (st-res n->h body-fvs body1)
      (de-state '() body))
    ;; XXX check body-fvs ⊆ args
    (hp:program (hash-set n->h nh (hh:handler args #f body1))
                nh)))

(define (direct->handle d)
  (dp-state (dp-anf d)))
(module+ test
  (chk
   (direct->handle de:add-some-numbers)
   (hp:program
    (hasheq
     'top0
     (hh:handler '() #f (ht:wait 'recv1 '() '()))
     'recv1
     (hh:handler '() 'anf-recv0
                 (ht:wait 'recv2 (list 'anf-recv0)
                          (list (he:con 1))))
     'recv2
     (hh:handler (list 'anf-recv0) 'anf-recv2
                 (ht:wait 'recv3 (list 'anf-recv0 'anf-recv2) '()))
     'recv3
     (hh:handler
      (list 'anf-recv0 'anf-recv2) 'anf-recv3
      (ht:let 'anf-app4
              (he:app '= (list (he:con 0)
                               (he:var 'anf-recv3)))
              (ht:if (he:var 'anf-app4)
                     (ht:wait 'recv4 (list 'anf-recv0 'anf-recv2)
                              '())
                     (ht:let 'anf-if6 (he:con 0)
                             (ht:let 'anf-app7
                                     (he:app '+
                                             (list (he:var 'anf-recv2)
                                                   (he:var 'anf-if6)))
                                     (ht:let 'anf-app8
                                             (he:app '+
                                                     (list (he:var 'anf-recv0)
                                                           (he:var 'anf-app7)))
                                             (ht:stop (he:var 'anf-app8) '())))))))
     'recv4
     (hh:handler
      (list 'anf-recv0 'anf-recv2) 'anf-recv5
      (ht:let 'anf-if6 (he:var 'anf-recv5)
              (ht:let 'anf-app7
                      (he:app '+
                              (list (he:var 'anf-recv2)
                                    (he:var 'anf-if6)))
                      (ht:let 'anf-app8
                              (he:app '+
                                      (list (he:var 'anf-recv0)
                                            (he:var 'anf-app7)))
                              (ht:stop (he:var 'anf-app8) '()))))))
    'top0)))

;; XXX parser for whole-program style

;; XXX epp

;; XXX parser for direct-style

;; XXX evaluator connected to simulator

;; XXX compile to other formats

;; XXX extract verification models
