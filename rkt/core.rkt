#lang racket/base
(require racket/match
         racket/format
         racket/list
         racket/set)
(module+ test
  (require chk))

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

;; A whole program is one that does not use send/recv directly, but
;; combines all of the agents into a single program with a generic msg
;; operations.
(struct wp:program (part->st n->fun in) #:transparent)
(struct wf:fun (args body) #:transparent)

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

(struct whole-msg () #:transparent)
(struct wm:cat whole-msg (l r) #:transparent)
(struct wm:var whole-msg (v) #:transparent)
(struct wm:enc whole-msg (x k) #:transparent)
(define (wm:sign m p)
  (wm:enc m (mk:pri-key p)))
(define (wm:seal-for m p)
  (wm:enc m (mk:pub-key p)))
(define (wm:hash m)
  (wm:enc m (mk:one-way)))

(struct whole-expr () #:transparent)
(struct we:var whole-expr (v) #:transparent)
(struct we:con whole-expr (b) #:transparent)
;; @ may be #f for a global operation
(struct we:let@ whole-expr (@ x xe be) #:transparent)
(struct we:if whole-expr (ce te fe) #:transparent)
(struct we:app whole-expr (op args) #:transparent)
(struct we:comm whole-expr (from to m be) #:transparent)
;; XXX message construction operation
;; XXX key management (learn new keys)

;; XXX type and knowledge checker

(define (from-symbol? p) (and (symbol? p) (not (eq? p '*))))
(define (to-symbol? p) (or (from-symbol? p) (eq? p '*)))
(define (wm-parse se)
  (match se
    ;; XXX enc, sign, seal-for, hash
    [(list mx my) (wm:cat (wm-parse mx) (wm-parse my))]
    [(? symbol? v) (wm:var v)]))
(define (we-parse se)
  (match se
    [`(begin) (we:con (void))]
    [`(begin (begin ,@more1) ,@more2)
     (we-parse `(begin ,@more1 ,@more2))]
    [`(begin (define ,(? symbol? x) ,xe) ,@more)
     (we:let@ #f x (we-parse xe) (we-parse `(begin ,@more)))]
    [`(begin (define ,(? symbol? x) @ ,(? from-symbol? @) ,xe)
             ,@more)
     (we:let@ @ x (we-parse xe) (we-parse `(begin ,@more)))]
    [`(begin [,(? from-symbol? from)
              -> ,(? to-symbol? to) : ,m] ,@more)
     (we:comm from (if (eq? to '*) #f to)
              (wm-parse m) (we-parse `(begin ,@more)))]
    [`(begin ,e) (we-parse e)]
    [`(if ,ce ,te ,fe)
     (we:if (we-parse ce) (we-parse te) (we-parse fe))]
    [(? number? n) (we:con n)]
    [(? symbol? v) (we:var v)]
    [(cons (? symbol? op) args)
     (we:app op (map we-parse args))]))
(define (wp-parse se)
  (match se
    [`(program
       ([,(? from-symbol? p) ,(? symbol? st) ...] ...)
       (define (,(? symbol? f) ,(? symbol? args) ...) ,@body)
       ...)
     (error-on-dupes 'wp-parse "Duplicate participant: ~e" p)
     (wp:program (for/hasheq ([p (in-list p)] [st (in-list st)])
                   (values p st))
                 (for/hasheq ([f (in-list f)] [args (in-list args)] [body (in-list body)])
                   (error-on-dupes 'wp-parse "Duplicate argument: ~e" args)
                   (values f (wf:fun args (we-parse `(begin ,@body)))))
                 (first f))]))

(define (wm-emit wm)
  (match wm
    [(wm:var v) v]
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
    [(we:app op args) (list (cons op (map we-emit1 args)))]
    [(we:if ce te fe)
     (list `(if ,(we-emit1 ce)
              ,(we-emit1 te)
              ,(we-emit1 fe)))]
    [(we:let@ at x xe be)
     (cons `(define ,x @ ,at ,(we-emit1 xe))
           (we-emit be))]
    [(we:comm from to m be)
     (cons `[,from -> ,(or to '*) : ,(wm-emit m)]
           (we-emit be))]))
(define (wp-emit wp)
  (match-define (wp:program part->st n->fun in) wp)
  `(program ,(for/list ([p (in-list (sort-symbols (hash-keys part->st)))])
               `[,p ,@(hash-ref part->st p)])
            ,@(for/list ([f (in-list (cons in (sort-symbols (remove in (hash-keys n->fun)))))])
                (match-define (wf:fun args body) (hash-ref n->fun f))
                `(define (,f ,@args) ,@(we-emit body)))))

(module+ test
  (define wp:adds-se
    `(program
      ([Adder x] [N1 x] [N2 x])
      (define (main)
        (define n1x @ N1 (rand 8))
        (define n2x @ N2 (rand 10))
        [N1 -> Adder : (x n1x)]
        [N2 -> Adder : (x n2x)]
        (define z @ Adder (+ n1x n2x))
        [Adder -> N1 : z]
        [Adder -> N2 : z]
        (define n2x @ N1 (- z n1x))
        (define n1x @ N2 (- z n2x))
        [N1 -> N2 : n2x]
        [N2 -> N1 : n1x])))
  (define wp:adds
    (wp-parse wp:adds-se))
  (chk (wp-emit wp:adds) wp:adds-se))

(define (wp-epp wp)
  ;; XXX generalize Γ to type environment and "knows" predicate
  (define (wm-send-epp me Γ from m)
    (cond
      [(not (eq? from me)) de:unit]
      [else
       (define (rec Γ m k)
         (match m
           [(wm:var v)
            (unless (set-member? Γ v)
              (error 'wp-epp "~e does not know ~e" me v))
            ;; XXX Insert coercion to bytes based on type
            (k (de:var v))]
           [(wm:cat lm rm)
            (rec Γ lm
                 (λ (lv)
                   (rec Γ rm
                        (λ (rv)
                          (define mv (freshen 'wm-send-cat))
                          (de:let mv (de:app 'msg-cat (list lv rv))
                                  (k (de:var mv)))))))]
           [(wm:enc im ek)
            (error 'wm-send-epp "XXX enc")]))
       (let ()
         (rec Γ m (λ (mv) (de:send mv))))]))
  (define (wm-recv-epp me Γ to m k)
    (cond
      [(and to (not (eq? to me)))
       (k Γ)]
      [else
       (define (rec Γ mv m k)
         (match m
           [(wm:var v)
            ;; XXX insert coercion from bytes based on type
            (cond
              [(set-member? Γ v)
               (de:ignore-unless
                (de:app 'equal? (list mv (de:var v)))
                (k Γ))]
              [else
               (de:let v mv (k (set-add Γ v)))])]
           [(wm:cat lm rm)
            (define lv (freshen 'wm-recv-catL))
            (define rv (freshen 'wm-recv-catR))
            (de:ignore-unless
             (de:app 'msg-cat? (list mv))
             (de:let* (list (cons lv (de:app 'msg-left (list mv)))
                            (cons rv (de:app 'msg-right (list mv))))
                      (rec Γ (de:var lv) lm
                           (λ (Γ1)
                             (rec Γ1 (de:var rv) rm k)))))]
           [(wm:enc im ek)
            (define inv-ek (mk-inv ek))
            ;; XXX check if really encryption
            (cond
              [(set-member? Γ inv-ek)
               (define imv (freshen 'wm-recv-enc))
               (de:let* (list (cons imv (de:app 'msg-decrypt (list mv (de:con inv-ek)))))
                        (rec Γ (de:var imv) im k))]
              [(set-member? Γ ek)
               (error 'wm-recv "XXX construct message and compare bytes")]
              [else
               (error 'wp-epp "~e does not know enough to receive message ~e: ~e or ~e" me m inv-ek ek)])]))
       (let ()
         (define mv (freshen 'wm-recv))
         (de:let mv (de:recv) (rec Γ (de:var mv) m k)))]))
  (define (we-epp p Γ e)
    (define (rec e) (we-epp p Γ e))
    (match e
      [(we:var v)
       (unless (set-member? Γ v)
         (error 'wp-epp "~e does not know ~e" p v))
       (de:var v)]
      [(we:con b) (de:con b)]
      [(we:let@ @ x xe be)
       (cond
         [(or (not @) (eq? @ p))
          (de:let x (we-epp #f Γ xe)
                  (we-epp p (set-add Γ x) be))]
         [else
          (we-epp p Γ be)])]
      [(we:if ce te fe)
       (de:if (rec ce) (rec te) (rec fe))]
      [(we:app op args)
       (de:app op (map rec args))]
      [(we:comm from to m ke)
       (define m1 (wm:sign (if to (wm:seal-for m to) m) from))
       (de:seq (wm-send-epp p Γ from m1)
               (wm-recv-epp p Γ to m1
                            (λ (Γ1)
                              (we-epp p Γ1 ke))))]))
  (define (wf-epp p extra-args f)
    (match-define (wf:fun args body) f)
    (define all-args (append extra-args args))
    (define body1
      (we-epp p (list->seteq all-args) body))
    (df:fun all-args body1))
  (with-fresh
    (match-define (wp:program part->st n->fun in) wp)
    (define shared-Ks (map mk:pub-key (hash-keys part->st)))
    ;; XXX populate initial key knowledge
    (for/hasheq ([(p st) (in-hash part->st)])
      (define n->fun1
        (for/hasheq ([(n f) (in-hash n->fun)])
          (values n (wf-epp p (if (eq? n in) st '()) f))))
      (values p (dp:program n->fun1 in)))))
(module+ test
  (chk (for/hasheq ([(p dp) (in-hash (wp-epp wp:adds))])
         (values p (dp-emit dp)))
       (hasheq
        'Adder
        `(program
          (define (main x)
            (define wm-recv0 (?))
            (define wm-recv-catL1 (msg-left wm-recv0))
            (define wm-recv-catR2 (msg-right wm-recv0))
            (if (equal? wm-recv-catL1 x)
              (begin
                (define n1x wm-recv-catR2)
                (define wm-recv3 (?))
                (define wm-recv-catL4 (msg-left wm-recv3))
                (define wm-recv-catR5 (msg-right wm-recv3))
                (if (equal? wm-recv-catL4 x)
                  (begin
                    (define n2x wm-recv-catR5)
                    (define z (+ n1x n2x))
                    (! z)
                    (! z))
                  (begin)))
              (begin))))
        'N1
        `(program
          (define (main x)
            (define n1x (rand 8))
            (define wm-send-cat9 (msg-cat x n1x))
            (! wm-send-cat9)
            (define wm-recv10 (?))
            (define z wm-recv10)
            (define n2x (- z n1x))
            (! n2x)
            (define wm-recv11 (?))
            (if (equal? wm-recv11 n1x) (begin) (begin))))
        'N2
        `(program
          (define (main x)
            (define n2x (rand 10))
            (define wm-send-cat6 (msg-cat x n2x))
            (! wm-send-cat6)
            (define wm-recv7 (?))
            (define z wm-recv7)
            (define n1x (- z n2x))
            (define wm-recv8 (?))
            (if (equal? wm-recv8 n2x) (! n1x) (begin)))))))

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
(struct de:ignore direct-expr () #:transparent)

(define (de-parse se)
  (match se
    [`(begin) de:unit]
    [`(begin (begin ,@more1) ,@more2)
     (de-parse `(begin ,@more1 ,@more2))]
    [`(begin (define ,(? symbol? x) ,xe) ,@more)
     (de:let x (de-parse xe) (de-parse `(begin ,@more)))]
    [`(begin ,e) (de-parse e)]
    [`(begin ,e ,@more) (de:seq (de-parse e) (de-parse `(begin ,@more)))]
    [`(if ,ce ,te ,fe) (de:if (de-parse ce) (de-parse te) (de-parse fe))]
    [`(ignore!) (de:ignore)]
    [`(?) (de:recv)]
    [`(! ,e) (de:send (de-parse e))]
    [(? number? n) (de:con n)]
    [(? symbol? v) (de:var v)]
    [(cons (? symbol? op) args) (de:app op (map de-parse args))]))
(define (dp-parse se)
  (match se
    [`(program
       (define (,(? symbol? f) ,(? symbol? args) ...) ,@body)
       ...)
     (dp:program (for/hasheq ([f (in-list f)] [args (in-list args)] [body (in-list body)])
                   (values f (df:fun args (de-parse `(begin ,@body)))))
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
    [(de:app op args) (list (cons op (map de-emit1 args)))]
    [(de:let (== _seq_) xe be)
     (append (de-emit xe) (de-emit be))]
    [(de:let x xe be)
     (cons `(define ,x ,(de-emit1 xe))
           (de-emit be))]
    [(de:if ce te fe)
     (list `(if ,(de-emit1 ce)
              ,(de-emit1 te)
              ,(de-emit1 fe)))]
    [(de:send e) (list `(! ,@(de-emit e)))]
    [(de:recv) (list `(?))]
    [(de:ignore) (list `(ignore!))]))
(define (dp-emit dp)
  (match-define (dp:program n->fun in) dp)
  `(program ,@(for/list ([f (in-list (cons in (sort-symbols (remove in (hash-keys n->fun)))))])
                (match-define (df:fun args body) (hash-ref n->fun f))
                `(define (,f ,@args) ,@(de-emit body)))))

;; XXX checker for direct-style

(define de:unit (de:con (void)))
(define (de:ignore-unless c be)
  (de:if c be (de:ignore)))
(define (de:let* x*xes be)
  (for/fold ([be be]) ([x*xe (in-list (reverse x*xes))])
    (de:let (car x*xe) (cdr x*xe) be)))
(define (de:seq f s)
  (if (equal? f de:unit) s
      (de:let _seq_ f s)))

(module+ test
  (define de:add-some-numbers-se
    `(program
      (define (add-some-numbers)
        (add3 (?) (begin (! 1) (?))
              (if (zero? (?)) (?) 0)))
      (define (add3 x y z)
        (+ x (+ y z)))
      (define (zero? x)
        (= 0 x))))
  (define de:add-some-numbers
    (dp-parse de:add-some-numbers-se))
  (chk (dp-emit de:add-some-numbers)
       de:add-some-numbers-se))

(define (d-arg? x)
  (or (de:var? x) (de:con? x)))

;; XXX anf format checker

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
      [(de:ignore)
       (anf-res (list (cons (freshen 'anf-ignore) (de:ignore)))
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
   (dp-emit (dp-anf de:add-some-numbers))
   `(program
     (define (main)
       (define anf-recv0 (?))
       (define anf-send1 (! 1))
       (define anf-recv2 (?))
       (define anf-recv3 (?))
       (define anf-app4 (= 0 anf-recv3))
       (define anf-if6 (if anf-app4 (begin (define anf-recv5 (?)) anf-recv5) 0))
       (define anf-app7 (+ anf-recv2 anf-if6))
       (define anf-app8 (+ anf-recv0 anf-app7))
       anf-app8))))

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
(struct ht:stop handle-tail (ans msgs) #:transparent)
(struct ht:ignore handle-tail () #:transparent)

(struct handle-arg () #:transparent)
(struct ha:var handle-arg (v) #:transparent)
(struct ha:con handle-arg (b) #:transparent)

(struct handle-expr () #:transparent)
(struct he:app handle-expr (op args) #:transparent)

;; XXX parser for handler-style
(define (ha-emit ha)
  (match ha
    [(ha:var v) v]
    [(ha:con (? number? n)) n]
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
    [(ht:let x xe ht)
     (cons `(define ,x ,(he-emit xe))
           (ht-emit ht))]
    [(ht:if ce tt ft)
     (list `(cond [,(he-emit ce) ,@(ht-emit tt)]
                  [else ,@(ht-emit ft)]))]
    [(ht:jump ns st msgs)
     (append (sends-emit msgs)
             (list `(jump! ,ns ,@st)))]
    [(ht:wait ns st msgs)
     (append (sends-emit msgs)
             (list `(wait! ,ns ,@st)))]
    [(ht:stop ans msgs)
     (append (sends-emit msgs)
             (list `(stop! ,(ha-emit ans))))]
    [(ht:ignore)
     (list `(ignore!))]))
(define (hp-emit hp)
  (match-define (hp:program n->han in) hp)
  ;; XXX topological sort
  `(program ,@(for/list ([f (in-list (cons in (sort-symbols (remove in (hash-keys n->han)))))])
                (match-define (hh:handler st msg ht) (hash-ref n->han f))
                `(define ((,f ,@st) ,msg) ,@(ht-emit ht)))))

;; XXX checker for handle-style

;; The compiler transforms a direct program into a handler program
(define (merge-non-overlapping-hash x y)
  (for/fold ([x x]) ([(k v) (in-hash y)])
    (when (hash-has-key? x k)
      (error 'merge-non-overlapping-hash "Hash cannot overlap!"))
    (hash-set x k v)))
(define (merge-non-overlapping-hashes xs)
  (for/fold ([y (hasheq)]) ([x (in-list xs)])
    (merge-non-overlapping-hash x y)))

(define (dp-state dp)
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
       (st-res
        (hash-set n->h1 nh (hh:handler st v ht))
        fvs1
        (ht:wait nh st pre-sends))]
      [(de:let v (de:ignore) b)
       (st-res (hasheq) (seteq) (ht:ignore))]))
  (with-fresh
    (match-define (dp:program (hash-table ('main df)) 'main) dp)
    (match-define (df:fun args body) df)
    (define nh (freshen 'top))
    (match-define (st-res n->h body-fvs body1)
      (de-state '() body
                (λ (a pre-sends)
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
   (hp-emit (direct->handle de:add-some-numbers))
   `(program
     (define ((top0) #f) (wait! recv1))
     (define ((let-if-body4 anf-if6 anf-recv0 anf-recv2) #f)
       (define anf-app7 (+ anf-recv2 anf-if6))
       (define anf-app8 (+ anf-recv0 anf-app7))
       (stop! anf-app8))
     (define ((recv1) anf-recv0)
       (send! 1)
       (wait! recv2 anf-recv0))
     (define ((recv2 anf-recv0) anf-recv2)
       (wait! recv3 anf-recv0 anf-recv2))
     (define ((recv3 anf-recv0 anf-recv2) anf-recv3)
       (define anf-app4 (= 0 anf-recv3))
       (cond
         [anf-app4 (wait! recv5 anf-recv0 anf-recv2)]
         [else
          (define anf-if6 0)
          (jump! let-if-body4 anf-if6 anf-recv0 anf-recv2)]))
     (define ((recv5 anf-recv0 anf-recv2) anf-recv5)
       (define anf-if6 anf-recv5)
       (jump! let-if-body4 anf-if6 anf-recv0 anf-recv2))))


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
     (define ((top0 x) #f) (wait! recv1 x))
     (define ((recv1 x) anf-recv0)
       (send! anf-recv0) (wait! recv2 anf-recv0 x))
     (define ((recv2 anf-recv0 x) anf-recv2)
       (define anf-app4 (msg-cat x anf-recv2))
       (send! anf-app4)
       (wait! recv3 anf-recv0 anf-recv2))
     (define ((recv3 anf-recv0 anf-recv2) anf-recv5)
       (define anf-app7 (msg-cat anf-recv0 anf-recv2 anf-recv5))
       (send! anf-app7)
       (stop! (void))))))

;; XXX primitives

;; XXX eval connected to simulator

;; XXX compile to other formats (i.e. contract vms)

;; XXX extract verification models (including simple dot graph)
