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
(struct ht:jump handle-tail (ns st msgs) #:transparent)
(struct ht:wait handle-tail (ns st msgs) #:transparent)
(struct ht:stop handle-tail (st msgs) #:transparent)

(struct handle-arg () #:transparent)
(struct ha:var handle-arg (v) #:transparent)
(struct ha:con handle-arg (b) #:transparent)

(struct handle-expr () #:transparent)
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

(define (d-arg? x)
  (or (de:var? x) (de:con? x)))
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
      [(de:var v) (values (seteq v) (ha:var v))]
      [(de:con b) (values (seteq) (ha:con b))]))
  (define (das-state es)
    (match es
      ['() (values (seteq) '())]
      [(cons a d)
       (define-values (fvs1 a1) (da-state a))
       (define-values (fvs2 d1) (das-state d))
       (values (set-union fvs1 fvs2) (cons a1 d1))]))
  (define (de-state pre-sends e k)
    (match e
      [(? d-arg? a)
       (define-values (a-fvs a1) (da-state a))
       (match-define (st-res n->h1 k-fvs kt) (k a1 pre-sends))
       (st-res n->h1 (set-union a-fvs k-fvs) kt)]
      [(de:if ca te fe)
       (define-values (ca-fvs ca1) (da-state ca))
       (match-define (st-res n->h1 te-fvs te1)
         (de-state pre-sends te k))
       (match-define (st-res n->h2 fe-fvs fe1)
         (de-state pre-sends fe k))
       (st-res (merge-non-overlapping-hash n->h1 n->h2)
               (set-union ca-fvs te-fvs fe-fvs)
               (ht:if ca1 te1 fe1))]
      [(de:let v (de:if ca te fe) b)
       (define-values (ca-fvs ca1) (da-state ca))
       (define bh (freshen 'let-if-body))
       (match-define (st-res n->h1 b-fvs bt)
         (de-state '() b k))
       (define b-fvs1 (set-remove b-fvs v))
       (define b-st (cons v (set->sorted-list b-fvs1)))
       (define (bk a pre-sends)
         ;; XXX include pre-sends in fvs
         (st-res (hasheq) b-fvs1
                 (ht:let v a (ht:jump bh b-st pre-sends))))
       (match-define (st-res n->h2 t-fvs tt)
         (de-state pre-sends te bk))
       (match-define (st-res n->h3 f-fvs ft)
         (de-state pre-sends fe bk))
       (define n->h4
         (merge-non-overlapping-hashes (list n->h1 n->h2 n->h3)))
       (define n->h5
         (hash-set n->h4 bh (hh:handler b-st #f bt)))
       (st-res n->h5 (set-union ca-fvs t-fvs f-fvs b-fvs1)
               (ht:if ca1 tt ft))]
      [(de:let v (de:app op args) b)
       (define-values (args-fvs args1) (das-state args))
       (match-define (st-res n->h1 fvs ht)
         (de-state pre-sends b k))
       (st-res n->h1 (set-union args-fvs (set-remove fvs v))
               (ht:let v (he:app op args1)
                       ht))]
      [(de:let v (de:send a) b)
       (define-values (a-fvs a1) (da-state a))
       (match-define (st-res n->h1 fvs ht)
         (de-state (snoc pre-sends a1) b k))
       (st-res n->h1 (set-union a-fvs (set-remove fvs v)) ht)]
      [(de:let v (de:recv) b)
       (define nh (freshen 'recv))
       (match-define (st-res n->h1 fvs ht)
         (de-state '() b k))
       (define fvs1 (set-remove fvs v))
       (define st (set->sorted-list fvs1))
       ;; XXX include pre-sends in fvs
       (st-res
        (hash-set n->h1 nh (hh:handler st v ht))
        fvs1
        (ht:wait nh st pre-sends))]))
  (with-fresh
    (match-define (df:fun args body) df)
    (define nh (freshen 'top))
    (match-define (st-res n->h body-fvs body1)
      (de-state '() body
                (λ (a pre-sends)
                  ;; XXX include pre-sends in fvs
                  (st-res (hasheq) (seteq)
                          (ht:stop a pre-sends)))))
    (unless (subset? body-fvs (list->seteq args))
      (error 'dp-state "Body free variables(~e) beyond args(~e)" body-fvs args))
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
                 (ht:wait 'recv2 '(anf-recv0)
                          (list (ha:con 1))))
     'recv2
     (hh:handler '(anf-recv0) 'anf-recv2
                 (ht:wait 'recv3 '(anf-recv0 anf-recv2) '()))
     'recv3
     (hh:handler
      '(anf-recv0 anf-recv2) 'anf-recv3
      (ht:let 'anf-app4
              (he:app '= (list (ha:con 0)
                               (ha:var 'anf-recv3)))
              (ht:if (ha:var 'anf-app4)
                     (ht:wait 'recv5 '(anf-recv0 anf-recv2) '())
                     (ht:let 'anf-if6 (ha:con 0)
                             (ht:jump 'let-if-body4
                                      '(anf-if6 anf-recv0 anf-recv2) '())))))
     'recv5
     (hh:handler
      '(anf-recv0 anf-recv2) 'anf-recv5
      (ht:let 'anf-if6 (ha:var 'anf-recv5)
              (ht:jump 'let-if-body4
                       '(anf-if6 anf-recv0 anf-recv2) '())))
     'let-if-body4
     (hh:handler
      '(anf-if6 anf-recv0 anf-recv2) #f
      (ht:let 'anf-app7
              (he:app '+
                      (list (ha:var 'anf-recv2)
                            (ha:var 'anf-if6)))
              (ht:let 'anf-app8
                      (he:app '+
                              (list (ha:var 'anf-recv0)
                                    (ha:var 'anf-app7)))
                      (ht:stop (ha:var 'anf-app8) '())))))
    'top0)))

;; XXX parser for whole-program style

;; XXX epp

;; XXX parser for direct-style

;; XXX primitives

;; XXX evaluator connected to simulator

;; XXX compile to other formats (i.e. contract vms)

;; XXX extract verification models (including simple dot graph)
