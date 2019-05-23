#lang racket/base

(provide wp-parse
         wp-emit
         wp-epp
         ;---
         dp-parse
         dp-emit
         direct->handle
         ;---
         hp-parse
         hp-emit
         ht-emit
         hp:program hp:program?
         hh:handler hh:handler?
         handle-tail? ht:set!& ht:if ht:wait* ht:jump ht:wait ht:stop
         handle-arg? ha:con ha:var
         handle-expr? he:app)

(require racket/match
         racket/format
         racket/port
         racket/syntax
         racket/string
         racket/list
         racket/file
         racket/system
         racket/set)
(module+ test
  (require racket/pretty
           chk))

;; Utility
(define (snoc l x) (append l (list x)))
(define (sort-symbols l)
  (sort l string<=? #:key symbol->string))
(define (set->sorted-list s)
  (sort-symbols (set->list s)))
(define (error-on-dupes where msg l)
  (cond
    [(check-duplicates l)
     => (λ (el) (error where msg el))]))

(define (merge-non-overlapping-hash x y)
  (for/fold ([x x]) ([(k v) (in-hash y)])
    (when (hash-has-key? x k)
      (error 'merge-non-overlapping-hash "Hash cannot overlap!"))
    (hash-set x k v)))
(define (merge-non-overlapping-hashes xs)
  (for/fold ([y (hasheq)]) ([x (in-list xs)])
    (merge-non-overlapping-hash x y)))

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

(define _seq_ (gensym '_))

;; Types
;; XXX

;; Primitives
(struct priminfo (rkt))
(define primitive->info
  ;; XXX fill this out and make real
  (hasheq 'random (priminfo random)
          '+ (priminfo +)
          '- (priminfo -)
          '= (priminfo =)
          'equal? (priminfo equal?)
          'msg-cat? (priminfo cons?)
          'msg-cat (priminfo cons)
          'msg-left (priminfo car)
          'msg-right (priminfo cdr)
          'msg-enc (priminfo (λ (m k) (list 'enc m k)))
          'msg-enc?
          (priminfo (λ (x k)
                      (and (list? x) (= (length x) 3) (eq? (first x) 'enc)
                           (eq? (third x) k))))
          'msg-dec
          (priminfo
           (λ (em k)
             (match em
               [(list 'enc im (== k)) im]
               [x (error 'msg-dec "~v ~v" em k)])))))
(define (primitive-op? x)
  (hash-has-key? primitive->info x))

;; Keys and participants
(define (participant? p) (or (symbol? p) (eq? p #f)))

(struct msg-key () #:transparent)
(struct mk:one-way (a) #:transparent)
(struct mk:sym-key (v) #:transparent)
(struct mk:pub-key (p) #:transparent)
(struct mk:pri-key (p) #:transparent)
(define mk-inv
  (match-lambda
    [(? mk:one-way?) #f]
    [(? mk:sym-key? sk) sk]
    [(mk:pub-key p) (mk:pri-key p)]
    [(mk:pri-key p) (mk:pub-key p)]))
(define mk->v
  (match-lambda
    [(mk:one-way a) a]
    [(mk:sym-key k) k]
    [(mk:pub-key p) (format-symbol "~a-pk" p)]
    [(mk:pri-key p) (format-symbol "~a-sk" p)]))
(define (mk-parse se)
  (match se
    ['HASH (mk:one-way 'HASH)]
    [(? symbol? v) (mk:sym-key v)]
    [(list 'pub (? participant? p)) (mk:pub-key p)]
    [(list 'pri (? participant? p)) (mk:pri-key p)]))
(define (mk-emit k)
  (match k
    [(mk:one-way 'HASH) 'HASH]
    [(mk:sym-key v) v]
    [(mk:pub-key p) (list 'pub p)]
    [(mk:pri-key p) (list 'pri p)]))

;; A whole program is one that does not use send/recv directly, but
;; combines all of the agents into a single program with a generic msg
;; operations.
(struct wp:program (part->st n->fun in) #:transparent)
(struct wf:fun (args body) #:transparent)

(struct whole-msg () #:transparent)
(struct wm:cat whole-msg (l r) #:transparent)
(struct wm:var whole-msg (v) #:transparent)
(struct wm:enc whole-msg (x k) #:transparent)
;; XXX Maybe not make these macros
(define (wm:sign m p)
  ;; XXX Maybe do digest-signing
  (wm:enc m (mk:pri-key p)))
(define (wm:seal m p)
  (wm:enc m (mk:pub-key p)))
(define (wm:hash m)
  (wm:enc m (mk:one-way 'HASH)))

(struct whole-expr () #:transparent)
(struct we:var whole-expr (v) #:transparent)
(struct we:con whole-expr (b) #:transparent)
(struct we:let@ whole-expr (@ x xe be) #:transparent)
(struct we:if whole-expr (ce te fe) #:transparent)
(struct we:app whole-expr (op args) #:transparent)
(struct we:msg whole-expr (m) #:transparent)
(struct we:match@ whole-expr (@ e m be) #:transparent)
(struct we:send whole-expr (e) #:transparent)
(struct we:recv whole-expr () #:transparent)

(define we:unit (we:con (void)))
(define (we:seq f s)
  (if (equal? f we:unit) s
      (we:let@ #f _seq_ f s)))

;; XXX type and knowledge checker

(define (wm-parse se)
  (match se
    [(list 'enc m k) (wm:enc (wm-parse m) (mk-parse k))]
    [(list 'sign m (? participant? p)) (wm:sign (wm-parse m) p)]
    [(list 'seal m (? participant? p)) (wm:seal (wm-parse m) p)]
    [(list 'hash m) (wm:hash (wm-parse m))]
    [(list mx my) (wm:cat (wm-parse mx) (wm-parse my))]
    [(? symbol? v) (wm:var v)]))
(define (we-parse operation? se)
  (define (rec se) (we-parse operation? se))
  (match se
    [`(begin) (we:con (void))]
    [`(begin (begin ,@more1) ,@more2)
     (rec `(begin ,@more1 ,@more2))]
    [`(begin (define ,(? symbol? x) ,xe) ,@more)
     (we:let@ #f x (rec xe) (rec `(begin ,@more)))]
    [`(begin (define ,(? symbol? x) @ ,(? participant? at) ,xe)
             ,@more)
     (we:let@ at x (rec xe) (rec `(begin ,@more)))]
    [`(begin [,(? participant? from)
              -> ,(? participant? to) : ,m] ,@more)
     ;; Note: It may be inappropriate to assume that to/from is
     ;; enforced with public keys. It would be possible to reify
     ;; participants names into values, but almost all uses that don't
     ;; do this seal/sign would be wrong.
     (define m0 (if to `(seal ,m ,to) m))
     (define m1 (if from `(sign ,m0 ,from) m0))
     (rec
      `(begin (define ,_seq_ @ ,from (! (msg ,m1)))
              (match @ ,to : (?) as ,m1)
              ,@more))]
    [`(begin (match @ ,(? participant? at) : ,e as ,m) ,@more)
     (we:match@ at (rec e) (wm-parse m)
                (rec `(begin ,@more)))]
    [`(begin ,e) (rec e)]
    [`(cond [,ce ,@te] [else ,@fe])
     (we:if (rec ce) (rec `(begin ,@te)) (rec `(begin ,@fe)))]
    [`(if ,ce ,te ,fe)
     (we:if (rec ce) (rec te) (rec fe))]
    [`(msg ,m) (we:msg (wm-parse m))]
    [`(! ,e) (we:send (rec e))]
    [`(?) (we:recv)]
    [(? number? n) (we:con n)]
    [(? boolean? b) (we:con b)]
    [(? symbol? v) (we:var v)]
    [(cons (? operation? op) args)
     (we:app op (map rec args))]))
(define (wp-parse se)
  (match se
    [`(program
       (participant ,(? participant? p) ,(? symbol? st) ...) ...
       (define (,(? symbol? f) ,(? symbol? args) ...) ,@body)
       ...)
     (error-on-dupes 'wp-parse "Duplicate participant: ~e" p)
     (error-on-dupes 'wp-parse "Duplicate function: ~e" f)
     (define fs (list->seteq f))
     (define (operation? x) (or (primitive-op? x) (set-member? fs x)))
     (wp:program (for/hasheq ([p (in-list p)] [st (in-list st)])
                   (values p st))
                 (for/hasheq ([f (in-list f)] [args (in-list args)] [body (in-list body)])
                   (error-on-dupes 'wp-parse "Duplicate argument: ~e" args)
                   (values f (wf:fun args (we-parse operation? `(begin ,@body)))))
                 (first f))]))

(define (wm-emit wm)
  (match wm
    [(wm:var v) v]
    [(wm:enc m k) (list 'enc (wm-emit m) (mk-emit k))]
    [(wm:cat x y) (list (wm-emit x) (wm-emit y))]))
(define (we-emit1 we)
  (match (we-emit we)
    [(list e) e]
    [more `(begin ,@more)]))
(define (we-emit we)
  (match we
    [(we:var v) (list v)]
    [(we:con (? void?)) (list)]
    [(we:con (? number? n)) (list n)]
    [(we:con (? boolean? b)) (list b)]
    [(we:app op args) (list (cons op (map we-emit1 args)))]
    [(we:if ce te fe)
     (list `(cond
              [,(we-emit1 ce)
               ,@(we-emit te)]
              [else
               ,@(we-emit fe)]))]
    [(we:let@ at x xe be)
     (cons `(define ,x @ ,at ,(we-emit1 xe))
           (we-emit be))]
    [(we:msg m)
     (list `(msg ,(wm-emit m)))]
    [(we:match@ at e m be)
     (cons `(match @ ,at : ,(we-emit1 e) as ,(wm-emit m))
           (we-emit be))]
    [(we:send e)
     (list `(! ,(we-emit1 e)))]
    [(we:recv)
     (list `(?))]))
(define (wp-emit wp)
  (match-define (wp:program part->st n->fun in) wp)
  `(program ,@(for/list ([p (in-list (sort-symbols (hash-keys part->st)))])
                `(participant ,p ,@(hash-ref part->st p)))
            ,@(for/list ([f (in-list (cons in (sort-symbols (remove in (hash-keys n->fun)))))])
                (match-define (wf:fun args body) (hash-ref n->fun f))
                `(define (,f ,@args) ,@(we-emit body)))))

(module+ test
  (define wp:adds-se
    `(program
      (participant Adder x)
      (participant N1 x)
      (participant N2 x)
      (define (main)
        (define n1x @ N1 (random 8))
        (define n2x @ N2 (random 10))
        [N1 -> Adder : (x n1x)]
        [N2 -> Adder : (x n2x)]
        (define z @ Adder (+ n1x n2x))
        [Adder -> N1 : z]
        [Adder -> N2 : z]
        (define n2x @ N1 (- z n1x))
        (define n1x @ N2 (- z n2x))
        [N1 -> N2 : n2x]
        [N2 -> N1 : n1x])))
  (define wp:adds (wp-parse wp:adds-se))
  (chk (wp-parse (wp-emit wp:adds)) wp:adds))

;; XXX add consensus function

;; Note: In formalization, this will be defined not on the program,
;; but on its type derivation.
(define (wp-epp wp)
  ;; XXX generalize Γ to type environment
  (define (we-epp me Γ e)
    (define (rec e) (we-epp me Γ e))
    (match e
      [(we:var v)
       (unless (set-member? Γ v)
         (error 'wp-epp "~e does not know ~e" me v))
       (de:var v)]
      [(we:con b) (de:con b)]
      [(we:let@ @ x xe be)
       (cond
         [(or (not @) (eq? @ me))
          (de:let x (we-epp me Γ xe)
                  (we-epp me (set-add Γ x) be))]
         [else
          (we-epp me Γ be)])]
      [(we:if ce te fe)
       (de:if (rec ce) (rec te) (rec fe))]
      [(we:app op args)
       (de:app op (map rec args))]
      [(we:msg m)
       (define (wm-make m)
         (match m
           [(wm:var v)
            (unless (set-member? Γ v)
              (error 'wp-epp "~e does not know ~e" me v))
            ;; XXX Insert coercion to bytes based on type
            (de:var v)]
           [(wm:cat lm rm)
            (de:app 'msg-cat (list (wm-make lm) (wm-make rm)))]
           [(wm:enc im ek)
            (define ek-v (mk->v ek))
            (cond
              [(set-member? Γ ek-v)
               (de:app 'msg-enc (list (wm-make im) (de:var ek-v)))]
              [else
               (error 'wp-epp "~e does not know enough to construct message ~e: ~e" me m ek)])]))
       (wm-make m)]
      [(we:match@ @ e m be)
       (define (wm-match Γ mv m k)
         (match m
           [(wm:var v)
            (cond
              [(set-member? Γ v)
               (de:ignore-unless
                ;; XXX insert coercion from bytes based on type, we know
                (de:app 'equal? (list mv (de:var v)))
                (~a mv " not equal to " v)
                (k Γ))]
              [else
               ;; XXX insert coercion from bytes based on type, in sender (if we know)
               (de:let v mv (k (set-add Γ v)))])]
           [(wm:cat lm rm)
            (define lv (freshen 'wm-recv-catL))
            (define rv (freshen 'wm-recv-catR))
            (de:ignore-unless
             (de:app 'msg-cat? (list mv))
             (~a mv " not concatenation")
             (de:let* (list (cons lv (de:app 'msg-left (list mv)))
                            (cons rv (de:app 'msg-right (list mv))))
                      (wm-match Γ (de:var lv) lm
                                (λ (Γ1)
                                  (wm-match Γ1 (de:var rv) rm k)))))]
           [(wm:enc im ek)
            (define inv-ek-v (mk->v (mk-inv ek)))
            (cond
              [(set-member? Γ inv-ek-v)
               (define imv (freshen 'wm-recv-dec))
               (define inv-ek-a (de:var inv-ek-v))
               (de:ignore-unless
                (de:app 'msg-enc? (list mv inv-ek-a))
                (~a mv " not encrypted with " inv-ek-a)
                (de:let* (list
                          (cons imv (de:app 'msg-dec (list mv inv-ek-a))))
                         (wm-match Γ (de:var imv) im k)))]
              [(set-member? Γ (mk->v ek))
               (define nv (freshen 'wm-recv-enc-chk))
               (we:let@ me nv (we:msg m)
                        (wm-match (set-add Γ nv) nv (wm:var mv) k))]
              [else
               (error 'wp-epp "~e does not know enough to receive message ~e: ~e or inv" me m ek)])]))
       (cond
         [(or (not @) (eq? @ me))
          (define mv (freshen 'wm-recv))
          (de:let mv (rec e)
                  (wm-match Γ (de:var mv) m
                            (λ (Γ1)
                              (we-epp me Γ1 be))))]
         [else
          (we-epp me Γ be)])]
      [(we:send e)
       (de:send (rec e))]
      [(we:recv)
       (de:recv)]))
  (define (wf-epp p extra-args f)
    (match-define (wf:fun args body) f)
    (define all-args (append extra-args args))
    (define body1
      (we-epp p (list->seteq all-args) body))
    (df:fun all-args body1))
  (with-fresh
    (match-define (wp:program part->st n->fun in) wp)
    (define shared-Ks (list* (mk:one-way 'HASH) (map mk:pub-key (hash-keys part->st))))
    (for/hasheq ([(p st) (in-hash part->st)])
      (define my-Ks (list* (mk:pri-key p) shared-Ks))
      (define args-b (map mk->v my-Ks))
      (define n->fun1
        (for/hasheq ([(n f) (in-hash n->fun)])
          (define args-a (if (eq? n in) st '()))
          (define args (append args-b args-a))
          (values n (wf-epp p args f))))
      (values p (dp:program n->fun1 in)))))

;; XXX add a public (intersection of knowledge) party

(module+ test
  (define epp:dp:adds (wp-epp wp:adds))
  (for ([(p dp) (in-hash epp:dp:adds)])
    (define dp-se (dp-emit dp))
    (printf "\n~a =>\n" p)
    (pretty-print dp-se)))

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
(struct de:ignore direct-expr (why) #:transparent)

(define (de-parse operation? se)
  (define (rec se) (de-parse operation? se))
  (match se
    [`(begin) de:unit]
    [`(begin (begin ,@more1) ,@more2)
     (rec `(begin ,@more1 ,@more2))]
    [`(begin (define ,(? symbol? x) ,xe) ,@more)
     (de:let x (rec xe) (rec `(begin ,@more)))]
    [`(begin ,e) (rec e)]
    [`(begin ,e ,@more) (de:seq (rec e) (rec `(begin ,@more)))]
    [`(cond [,ce ,@te] [else ,@fe])
     (de:if (rec ce) (rec `(begin ,@te)) (rec `(begin ,@fe)))]
    [`(if ,ce ,te ,fe)
     (de:if (rec ce) (rec te) (rec fe))]
    [`(ignore! ,(? string? why)) (de:ignore why)]
    [`(?) (de:recv)]
    [`(! ,e) (de:send (rec e))]
    [(? number? n) (de:con n)]
    [(? boolean? b) (de:con b)]
    [(? symbol? v) (de:var v)]
    [(cons (? operation? op) args)
     (de:app op (map rec args))]))
(define (dp-parse se)
  (match se
    [`(program
       (define (,(? symbol? f) ,(? symbol? args) ...) ,@body)
       ...)
     (error-on-dupes 'dp-parse "Duplicate function: ~e" f)
     (define fs (list->seteq f))
     (define (operation? x) (or (primitive-op? x) (set-member? fs x)))
     (dp:program (for/hasheq ([f (in-list f)] [args (in-list args)] [body (in-list body)])
                   (error-on-dupes 'dp-parse "Duplicate argument: ~e" args)
                   (values f (df:fun args (de-parse operation? `(begin ,@body)))))
                 (first f))]))

(define (de-emit1 we)
  (match (de-emit we)
    [(list e) e]
    [more `(begin ,@more)]))
(define (de-emit de)
  (match de
    [(de:var v) (list v)]
    [(de:con (? void?)) (list)]
    [(de:con (? number? n)) (list n)]
    [(de:con (? boolean? b)) (list b)]
    [(de:app op args) (list (cons op (map de-emit1 args)))]
    [(de:let (== _seq_) xe be)
     (append (de-emit xe) (de-emit be))]
    [(de:let x xe be)
     (cons `(define ,x ,(de-emit1 xe))
           (de-emit be))]
    [(de:if ce te fe)
     (define ces (de-emit1 ce))
     (define tes (de-emit te))
     (define fes (de-emit fe))
     (cond
       [(= (length tes) (length fes) 1)
        (list `(if ,ces ,@tes ,@fes))]
       [(empty? fes)
        (list `(when ,ces ,@tes))]
       [(empty? tes)
        (list `(unless ,ces ,@fes))]
       [else
        (list `(cond [,ces ,@tes] [else ,@fes]))])]
    [(de:send e) (list `(! ,@(de-emit e)))]
    [(de:recv) (list `(?))]
    [(de:ignore why) (list `(ignore! ,why))]))
(define (dp-emit dp)
  (match-define (dp:program n->fun in) dp)
  `(program ,@(for/list ([f (in-list (cons in (sort-symbols (remove in (hash-keys n->fun)))))])
                (match-define (df:fun args body) (hash-ref n->fun f))
                `(define (,f ,@args) ,@(de-emit body)))))

;; XXX checker for direct-style: type correct, no recursion

(define de:unit (de:con (void)))
(define (de:seq f s)
  (if (equal? f de:unit) s
      (de:let _seq_ f s)))
(define (de:ignore-unless c why be)
  (de:seq (de:if c de:unit (de:ignore why)) be))
(define (de:let* x*xes be)
  (for/fold ([be be]) ([x*xe (in-list (reverse x*xes))])
    (de:let (car x*xe) (cdr x*xe) be)))

(module+ test
  (define dp:add-some-numbers-se
    `(program
      (define (add-some-numbers)
        (add3 (?) (begin (! 1) (?))
              (if (zero? (?)) (?) 0)))
      (define (add3 x y z)
        (+ x (+ y z)))
      (define (zero? x)
        (= 0 x))))
  (define dp:add-some-numbers (dp-parse dp:add-some-numbers-se))
  (chk (dp-parse (dp-emit dp:add-some-numbers)) dp:add-some-numbers))

(define (da-anf? da)
  (or (de:var? da) (de:con? da)))
(define (de-anf? de)
  (match de
    [(? da-anf? a) #t]
    [(de:if ce te fe) (and (da-anf? ce) (dt-anf? te) (dt-anf? fe))]
    [(de:app op args) (andmap da-anf? args)]
    [(de:send e) (da-anf? e)]
    [(de:recv) #t]
    [(de:ignore why) #t]))
(define (dt-anf? dt)
  (match dt
    [(? da-anf? a) #t]
    [(de:let x xe be)
     (and (de-anf? xe) (dt-anf? be))]
    [(de:if ce te fe)
     (and (da-anf? ce) (dt-anf? te) (dt-anf? fe))]))
(define (df-anf? df)
  (match-define (df:fun args e) df)
  (dt-anf? e))
(define (dp-anf? dp)
  (match-define (dp:program n->fun in) dp)
  (and (eq? in 'main)
       (= 1 (hash-count n->fun))
       (hash-has-key? n->fun 'main)
       (df-anf? (hash-ref n->fun 'main))))

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
       (anf-res (snoc nvs1 (cons nv (de:send e1))) de:unit)]
      [(de:recv)
       (cond
         [tail?
          (anf-res '() e)]
         [else
          (define nv (freshen 'anf-recv))
          (anf-res (list (cons nv (de:recv))) (de:var nv))])]
      [(de:ignore why)
       (anf-res (list (cons (freshen 'anf-ignore) (de:ignore why)))
                de:unit)]))
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
    (dp:program (hasheq 'main (df:fun args e1)) 'main)))
(module+ test
  (chk
   (dp-emit (dp-anf dp:add-some-numbers))
   `(program
     (define (main)
       (define anf-recv0 (?))
       (define anf-send1 (! 1))
       (define anf-recv2 (?))
       (define anf-recv3 (?))
       (define anf-app4 (= 0 anf-recv3))
       (define anf-if6 (cond [anf-app4 (define anf-recv5 (?)) anf-recv5]
                             [else 0]))
       (define anf-app7 (+ anf-recv2 anf-if6))
       (define anf-app8 (+ anf-recv0 anf-app7))
       anf-app8))))

;; At a low-level, a program is a mapping from named states (a finite
;; set of labels) to a handler function that consumes the state
;; representation, the most recent message, and returns a named state,
;; a new state representation, and a block of messages to send. There
;; is a special initial handler that receives the initial state and an
;; empty message, which is ignored.
(struct hp:program (args vs n->handler in) #:transparent)
(define MSG (gensym 'MSG))
(define HALT (gensym 'HALT))
(struct hh:handler (ht) #:transparent)

(struct handle-tail () #:transparent)
(struct ht:set!& handle-tail (x xe ht) #:transparent)
(struct ht:if handle-tail (ce tt ft) #:transparent)
(struct ht:wait* handle-tail (ns msgs recv?) #:transparent)

(define (ht:jump ns msgs)
  (ht:wait* ns msgs #f))
(define (ht:wait ns msgs)
  (ht:wait* ns msgs #t))
(define (ht:stop msgs)
  (ht:jump HALT msgs))

(struct handle-arg () #:transparent)
(struct ha:var handle-arg (v) #:transparent)
(struct ha:con handle-arg (b) #:transparent)

(struct handle-expr () #:transparent)
(struct he:app handle-expr (op args) #:transparent)

(define (ha-parse se)
  (match se
    [(? symbol? v) (ha:var v)]
    [(? number? n) (ha:con n)]
    [(? boolean? b) (ha:con b)]
    [`(void) (ha:con (void))]))
(define (he-parse se)
  (match se
    [(cons (? primitive-op? op) args)
     (he:app op (map ha-parse args))]
    [x (ha-parse x)]))
(define (ht-parse handler? se)
  (define (rec se) (ht-parse handler? se))
  (match se
    [`(begin (set! ,(? symbol? x) ,xe) ,@more)
     (ht:set!& x (he-parse xe) (rec `(begin ,@more)))]
    [`(begin (cond [,ce ,@tes] [else ,@fes]))
     (ht:if (he-parse ce) (rec `(begin ,@tes)) (rec `(begin ,@fes)))]
    [`(begin (send! ,msgs) ... (,(and which (or 'jump! 'wait!)) ,(? handler? ns)))
     (ht:wait* ns (map ha-parse msgs) (eq? which 'wait!))]))
(define (hp-parse se)
  (match se
    [`(program (,(? symbol? a) ...)
               (define-values (,(? symbol? vs) ...))
               (define (,(and (? symbol? h) (not 'HALT)))
                 ,@body)
               ...)
     (error-on-dupes 'hp-parse "Duplicate handler: ~e" h)
     (error-on-dupes 'hp-parse "Duplicate argument: ~e" a)
     (error-on-dupes 'hp-parse "Duplicate variable: ~e" vs)
     (define hans (list->seteq h))
     (define (handler? x) (or (eq? HALT x) (set-member? hans x)))
     (hp:program a vs
                 (for/hasheq ([h (in-list h)]
                              [body (in-list body)])
                   (values h (hh:handler (ht-parse handler? `(begin ,@body)))))
                 (first h))]))

(define (ha-emit ha)
  (match ha
    [(ha:var v) v]
    [(ha:con (? number? n)) n]
    [(ha:con (? boolean? b)) b]
    [(ha:con (? void?)) `(void)]))
(define (he-emit he)
  (match he
    [(he:app op args)
     `(,op ,@(map ha-emit args))]
    [x (ha-emit x)]))
(define (ht-emit ht)
  (define (sends-emit msgs)
    (if (empty? msgs) '() (list `(send! ,@(map ha-emit msgs)))))
  (match ht
    [(ht:set!& x xe ht)
     (cons `(set! ,x ,(he-emit xe))
           (ht-emit ht))]
    [(ht:if ce tt ft)
     (list `(cond [,(he-emit ce) ,@(ht-emit tt)]
                  [else ,@(ht-emit ft)]))]
    [(ht:wait* ns msgs recv?)
     (append (sends-emit msgs)
             (list `(,(if recv? 'wait! 'jump!) ,ns)))]))
(define (hp-emit hp)
  (match-define (hp:program args vs n->han in) hp)
  `(program (,@args)
            (define-values ,vs)
            ,@(for/list ([f (in-list (cons in (sort-symbols (remove in (hash-keys n->han)))))])
                (match-define (hh:handler ht) (hash-ref n->han f))
                `(define (,f) ,@(ht-emit ht)))))

;; XXX checker for handler-style: type correct, no recursion

;; The compiler transforms a direct program into a handler program
(define (dp-state dp)
  (define n->h (make-hasheq))
  (define (da-state e)
    (match e
      [(de:var v) (ha:var v)]
      [(de:con b) (ha:con b)]))
  (define (das-state es)
    (map da-state es))
  (define (de-state ignore-n pre-sends e k)
    (match e
      [(? da-anf? a)
       (k (da-state a) pre-sends)]
      [(de:if ca te fe)
       (ht:if (da-state ca)
              (de-state ignore-n pre-sends te k)
              (de-state ignore-n pre-sends fe k))]
      [(de:let v (de:if ca te fe) b)
       (define bh (freshen 'let-if-body))
       (hash-set! n->h bh (hh:handler (de-state ignore-n '() b k)))
       (define (bk a pre-sends)
         (ht:set!& v a (ht:jump bh pre-sends)))
       (ht:if (da-state ca) (de-state ignore-n pre-sends te bk)
              (de-state ignore-n pre-sends fe bk))]
      [(de:let v (de:app op args) b)
       (ht:set!& v (he:app op (das-state args))
                 (de-state ignore-n pre-sends b k))]
      [(de:let v (de:send a) b)
       (de-state ignore-n (snoc pre-sends (da-state a)) b k)]
      [(de:let v (de:recv) b)
       (define nh (freshen 'recv))
       (define nh-t (ht:set!& v (ha:var MSG) (de-state nh '() b k)))
       (hash-set! n->h nh (hh:handler nh-t))
       (ht:wait nh pre-sends)]
      [(de:let v (de:ignore why) b)
       (unless ignore-n
         (error 'dp-state "Cannot ignore if no recv"))
       ;; XXX Undo state changes
       (ht:wait ignore-n empty)]))
  (define (collect-vars e)
    (match e
      [(de:if ca te fe)
       (set-union (collect-vars te) (collect-vars fe))]
      [(de:let v ve b)
       (set-add (set-union (collect-vars ve) (collect-vars b)) v)]
      [_
       (seteq)]))
  (with-fresh
    (match-define (dp:program (hash-table ('main df)) 'main) dp)
    (match-define (df:fun args body) df)
    (define nh (freshen 'top))
    ;; Note: It would be possible to do register allocation in this
    ;; step and rename the variables. It may make sense to do this to
    ;; limit the number of global variables that are used in the
    ;; program. However, I think the verification burden may be quite
    ;; high, so I'm not including it for now.
    (define all-vs (set->sorted-list (collect-vars body)))
    (define body-t (de-state #f '() body (λ (a pre-sends) (ht:stop pre-sends))))
    (hash-set! n->h nh (hh:handler body-t))
    (define imm-n->h
      (for/hasheq ([(k v) (in-hash n->h)])
        (values k v)))
    (hp:program args all-vs imm-n->h nh)))

(define (direct->handle dp)
  (define ap (dp-anf dp))
  (unless (dp-anf? ap)
    (error 'direct->handle "Failed to convert to ANF"))
  (dp-state ap))
(module+ test
  (define hp:add-some-numbers (direct->handle dp:add-some-numbers))
  (define hp:add-some-numbers-se (hp-emit hp:add-some-numbers))
  (chk
   hp:add-some-numbers-se
   `(program
     ()
     (define-values (anf-app4 anf-app7 anf-app8 anf-if6 anf-recv0 anf-recv2
                              anf-recv3 anf-recv5 anf-send1))
     (define (top0) (wait! recv1))
     (define (let-if-body4)
       (set! anf-app7 (+ anf-recv2 anf-if6))
       (set! anf-app8 (+ anf-recv0 anf-app7))
       (jump! ,HALT))
     (define (recv1) (set! anf-recv0 ,MSG) (send! 1) (wait! recv2))
     (define (recv2) (set! anf-recv2 ,MSG) (wait! recv3))
     (define (recv3)
       (set! anf-recv3 ,MSG)
       (set! anf-app4 (= 0 anf-recv3))
       (cond
         (anf-app4 (wait! recv5))
         (else (set! anf-if6 0) (jump! let-if-body4))))
     (define (recv5)
       (set! anf-recv5 ,MSG)
       (set! anf-if6 anf-recv5)
       (jump! let-if-body4))))
  (chk (hp-parse hp:add-some-numbers-se) hp:add-some-numbers)

  (chk
   (hp-emit
    (direct->handle
     (dp-parse
      `(program
        (define (main x)
          (define r0 (?))
          (define s0 (! r0))
          (define r1 (?))
          (define s1 (! (msg-cat x r1)))
          (define r2 (?))
          (! (msg-cat r0 r1 r2)))))))
   `(program
     (x)
     (define-values (anf-app4 anf-app7 anf-recv0 anf-recv2 anf-recv5 anf-send1
                              anf-send3 anf-send6))
     (define (top0) (wait! recv1))
     (define (recv1) (set! anf-recv0 ,MSG) (send! anf-recv0) (wait! recv2))
     (define (recv2)
       (set! anf-recv2 ,MSG)
       (set! anf-app4 (msg-cat x anf-recv2))
       (send! anf-app4)
       (wait! recv3))
     (define (recv3)
       (set! anf-recv5 ,MSG)
       (set! anf-app7 (msg-cat anf-recv0 anf-recv2 anf-recv5))
       (send! anf-app7)
       (jump! ,HALT))))

  (define epp:hp:adds
    (for/hasheq ([(r dp) (in-hash epp:dp:adds)])
      (values r (direct->handle dp)))))

;; Evaluation in simulator
(define (hp-eval who send! recv! hp init-vs)
  (define (ha-eval ha)
    (match ha
      [(ha:con b) b]
      [(ha:var v) (hash-ref Σ v)]))
  (define (he-eval he)
    (match he
      [(he:app op args)
       (match (hash-ref primitive->info op #f)
         [(? priminfo? p)
          (apply (priminfo-rkt p)
                 (map ha-eval args))]
         [_
          (error 'he-eval "op ~e not implemented" op)])]
      [_ (ha-eval he)]))
  (define (send*! msgs)
    (for-each send! msgs))
  (define (ht-eval ht)
    (match ht
      [(ht:set!& x xe ht)
       (hash-set! Σ x (he-eval xe))
       (ht-eval ht)]
      [(ht:if ce tt ft)
       (ht-eval (if (he-eval ce) tt ft))]
      [(ht:wait* ns msgs recv?)
       (send*! (map ha-eval msgs))
       (hhn-eval ns (and recv? (recv!)))]))
  (define (hhn-eval n msg-v)
    (cond
      [(eq? n HALT)
       (eprintf "[~a] HALT\n" who)
       (void)]
      [else
       (eprintf "[~a] HHN-EVAL ~a w/ ~a\n" who n msg-v)
       (match-define (hh:handler ht) (hash-ref n->h n))
       (hash-set! Σ MSG msg-v)
       (ht-eval ht)]))
  (match-define (hp:program args vs n->h top) hp)
  (define Σ (make-hasheq))
  (for ([a (in-list args)])
    (hash-set!
     Σ a
     (hash-ref init-vs a
               (λ () (error 'hhn-eval "No value for argument ~e" a)))))

  (hhn-eval top #f))

(module+ test
  (define (simulate/simchain epp-hp-ht r->args)
    (local-require alacrity/simchain)
    (define hostname "localhost")
    (define port-no (+ 65000 (random 32)))
    (define history-p "epp-sim.chain")
    (when (file-exists? history-p)
      (delete-file history-p))
    (define stop-server! (make-simchain-server-t port-no history-p))
    (define role-ts
      (for/list ([(r hp) (in-hash epp-hp-ht)])
        (define arg-vs (hash-ref r->args r))
        (thread
         (λ ()
           (with-handlers ([exn:fail?
                            (λ (x)
                              ((error-display-handler) (exn-message x) x)
                              (kill-all!))])
             (define-values (send-bs! recv-bs!)
               (make-simchain-client/queue hostname port-no))
             (define (send! v) (send-bs! (with-output-to-bytes (λ () (write v)))))
             (define (recv!) (with-input-from-bytes (recv-bs!) read))
             (hp-eval r send! recv! hp arg-vs))))))
    (define (kill-all!)
      (for-each kill-thread role-ts))
    (for-each thread-wait role-ts)
    (stop-server!))

  (define (simulate/rkt epp-hp-ht r->args)
    (define (send! v) (thread-send server-t v))
    (define server-t
      (thread
       (λ ()
         (let loop ()
           (define m (thread-receive))
           (eprintf "Got ~v, broadcasting\n" m)
           (for ([r (in-list role-ts)]
                 #:when (thread-running? r))
             (thread-send r m))
           (loop)))))
    (define role-ts
      (for/list ([(r hp) (in-hash epp-hp-ht)])
        (define arg-vs (hash-ref r->args r))
        (thread
         (λ ()
           (with-handlers ([exn:fail?
                            (λ (x)
                              ((error-display-handler) (exn-message x) x)
                              (kill-all!))])
             (define (recv!) (thread-receive))
             (hp-eval r send! recv! hp arg-vs))))))
    (define (kill-all!)
      (for-each kill-thread role-ts))
    (for-each thread-wait role-ts)
    (kill-thread server-t))

  (let ()
    (define x (random 1024))
    ;; XXX Do this for real
    (define (make-fake-key-pair who) (define x who) (values x x))
    (define-values (Adder-sk Adder-pk) (make-fake-key-pair 'Adder))
    (define-values (N1-sk N1-pk) (make-fake-key-pair 'N1))
    (define-values (N2-sk N2-pk) (make-fake-key-pair 'N2))
    (define HASH (gensym 'hash))
    (define shared-args
      (hasheq 'HASH HASH 'Adder-pk Adder-pk 'N2-pk N2-pk 'N1-pk N1-pk 'x x))
    (simulate/rkt
     epp:hp:adds
     (hasheq
      'Adder (hash-set shared-args 'Adder-sk Adder-sk)
      'N2 (hash-set shared-args 'N2-sk N2-sk)
      'N1 (hash-set shared-args 'N1-sk N1-sk)))))

;; XXX extract to solidity

;; XXX extract to strand space (cpsa)

(define (hp->dot! hp fp)
  (define (add-for-handler! in)
    (match (hash-ref n->handler in #f)
      [#f (void)]
      [(hh:handler ht)
       (hash-remove! n->handler in)
       (add-for-tail! empty empty in ht)]))
  (define (add-for-tail! effs ces from ht)
    (match ht
      [(ht:set!& x xe ht)
       (add-for-tail! (cons (cons x xe) effs) ces from ht)]
      [(ht:if ce tt ft)
       (add-for-tail! effs (cons ce ces) from tt)
       (add-for-tail! effs (cons (list '! ce) ces) from ft)]
      [(ht:wait* ns msgs recv?)
       (add-directed-edge!
        from ns
        (string-join
         (append
          (for/list ([up (in-list (reverse effs))])
            (match-define (cons x xe) up)
            (~a x " := " (render-he xe)))
          (for/list ([ce (in-list (reverse ces))])
            (~a "IF " (match ce
                        [(list '! ce) (~a "!" (render-ha ce))]
                        [_ (render-ha ce)])))
          (for/list ([m (in-list msgs)])
            (~a "+" (render-ha m))))
         "\n"))
       (add-for-handler! ns)]))
  (define (render-he e)
    (match e
      [(he:app op args) (~a "(" op (if (empty? args) "" " ") (string-join (map render-ha args)) ")")]
      [a (render-ha a)]))
  (define (render-ha a)
    (match a
      [(ha:var v) (~a v)]
      [(ha:con c) (~a c)]))
  (define (add-directed-edge! from to label)
    (printf "\"~a\" -> \"~a\" [label = \"~a\"];\n" from to label))
  (match-define (hp:program args vs n->handler-imm in) hp)
  (define n->handler (hash-copy n->handler-imm))
  (with-output-to-file fp #:exists 'replace
    (λ ()
      (printf "digraph {\n")
      (printf "\tnode [shape = doublecircle, label=\"HALT\"] \"~a\";\n" HALT)
      (printf "\tnode [shape = point, label = \"START\" ] \"START\";\n")
      (for ([in (in-hash-keys n->handler-imm)])
        (printf "\tnode [ shape = circle, label = \"~a\" ] \"~a\";\n" in in))
      (add-directed-edge! 'START in "")
      (add-for-handler! in)
      (printf "}\n")))
  (system* (find-executable-path "dot") "-O" "-Tpng" fp))
(module+ test
  (for ([(r hp) (in-hash epp:hp:adds)])
    (hp->dot! hp (~a r ".dot"))))

;; XXX change de:ignore to assume? / guard / rely

;; XXX extract to Z3
