#lang racket/base
(require racket/list
         racket/match
         racket/set
         fancy-app
         "util/define-data.rkt")
(module+ test
  (require racket/pretty
           chk))

(provide wp-parse ; SExpr -> WholeProgram
         wp-emit  ; WholeProgram -> SExpr
         wp-tag   ; WholeProgram -> WholeProgramTagged
         wp-anf   ; WholeProgramTagged -> WholeProgramAnf
         )

;; -----------------------------------------------

;; Utility
(define (sort-symbols l)
  (sort l string<=? #:key symbol->string))
(define (error-on-dupes where msg l)
  (cond
    [(check-duplicates l)
     => (λ (el) (error where msg el))]))

;; -----------------------------------------------

(define _seq_ (gensym '_))

;; Primitives
(struct priminfo (rkt))
(define primitive->info
  ;; XXX fill this out and make real
  (hasheq 'random (priminfo random)
          'digest (priminfo equal-hash-code)
          '+ (priminfo +)
          '- (priminfo -)
          '* (priminfo *)
          '= (priminfo =)
          '< (priminfo <)
          '<= (priminfo <=)
          'integer? (priminfo integer?)
          'modulo (priminfo modulo)
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

;; Participant Participant -> Bool
(define (can-specify? boss other)
  (or (not boss) (equal? boss other)))

;; -----------------------------------------------

;; Data Definitions

(define-data whole-program
  (wp:program part->st n->fun in))

(define-data whole-function
  (wf:fun args body))

(define-data whole-expr
  (we:var v)
  (we:con b)
  (we:block stmt body)
  (we:if ce te fe)
  (we:app op args))

(define-data whole-stmt
  (ws:def@ @ x xe)
  (ws:send-recv from to xs amt)
  (ws:assert! assume? what why))

(define we:unit (we:con (void)))

(define (we:let@ @ x xe b)
  (we:block (ws:def@ @ x xe) b))

(define (we:seq fe se)
  (if (equal? fe we:unit) se
      (we:let@ #f _seq_ fe se)))

;; Parsing

;; [SExpr -> Bool] SExpr -> WholeExpr
(define (we-parse operation? se)
  (define (we-rec se) (we-parse operation? se))
  (define (ws-rec se) (ws-parse operation? se))
  (match se
    [`(begin) we:unit]
    [`(begin ,e) (we-rec e)]
    [`(begin (begin ,@more1) ,@more2)
     (we-rec `(begin ,@more1 ,@more2))]
    [`(begin ,stmt ,@more)
     (we:block (ws-rec stmt) (we-rec `(begin ,@more)))]
    [`(cond [else ,@fe])
     (we-rec `(begin ,@fe))]
    [`(cond [,ce ,@te] ,more ..1)
     (we:if (we-rec ce) (we-rec `(begin ,@te)) (we-rec `(cond ,@more)))]
    [`(if ,ce ,te ,fe)
     (we:if (we-rec ce) (we-rec te) (we-rec fe))]
    [`(and) (we:con #t)]
    [`(and ,te) (we-rec te)]
    [`(and ,ce ,@more)
     (we-rec `(if ,ce (and ,@more) #f))]
    [`(assert! . ,_)    (we:block (ws-rec se) we:unit)]
    [`(require! . ,_)   (we:block (ws-rec se) we:unit)]
    [`[,_ -> ,_ : . ,_] (we:block (ws-rec se) we:unit)]
    [(? number? n)  (we:con n)]
    [(? boolean? b) (we:con b)]
    [(? symbol? v)  (we:var v)]
    [(cons (? operation? op) args)
     (we:app op (map we-rec args))]))

;; [SExpr -> Bool] SExpr -> WholeStmt
(define (ws-parse operation? se)
  (define (we-rec se) (we-parse operation? se))
  (match se
    [`(define ,(? symbol? x) ,xe)
     (ws:def@ #f x (we-rec xe))]
    [`(@ ,(? participant? at) (define ,(? symbol? x) ,xe))
     (ws:def@ at x (we-rec xe))]
    [`(@ ,(? participant? at) ,xe)
     (ws:def@ at _seq_ (we-rec xe))]
    [`(assert! ,ce)
     (ws:assert! #f (we-rec ce) "Assertion")]
    [`(require! ,ce)
     (ws:assert! #t (we-rec ce) "Require")]
    [`[,(? participant? from) -> ,(? participant? to) : (,(? symbol? xs) ...)]
     (error-on-dupes 'wp-parse "Duplicate send: ~e" xs)
     (ws:send-recv from to xs #f)]
    [`[,(? participant? from) -> ,(? participant? to) : (,(? symbol? xs) ...)
                              #:pay ,(? symbol? amt)]
     (error-on-dupes 'wp-parse "Duplicate send: ~e" (cons amt xs))
     (ws:send-recv from to xs amt)]
    [(cons (? operation?) _)
     (ws:def@ #f _seq_ (we-rec se))]))

;; [SExpr -> Bool] SExpr -> WholeProgram
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
     (wp:program
      (for/hasheq ([p (in-list p)] [st (in-list st)])
        (values p st))
      (for/hasheq ([f (in-list f)] [args (in-list args)] [body (in-list body)])
        (error-on-dupes 'wp-parse "Duplicate argument: ~e" args)
        (values f (wf:fun args (we-parse operation? `(begin ,@body)))))
      (first f))]))

;; Emitting S-Expressions
;; (used for printing)

;; WholeExpr -> SExpr
(define (we-emit1 we)
  (match (we-emit we)
    [(list e) e]
    [more `(begin ,@more)]))

;; WholeExpr -> [Listof SExpr]
(define (we-emit we)
  (match we
    [(we:var v)              (list v)]
    [(we:con (? void?))      (list)]
    [(we:con (? number? n))  (list n)]
    [(we:con (? boolean? b)) (list b)]
    [(we:app op args)        (list (cons op (map we-emit1 args)))]
    [(we:if _ _ _)           (list `(cond ,@(we-cond-emit we)))]
    [(we:block stmt be)      (cons (ws-emit stmt) (we-emit be))]))

;; WholeExpr -> [Listof [List SExpr SExpr]]
(define (we-cond-emit we)
  (match we
    [(we:if ce te fe)
     `([,(we-emit1 ce) ,@(we-emit te)] ,@(we-cond-emit fe))]
    [we
     `([else ,@(we-emit we)])]))

;; WholeStmt -> SExpr
(define (ws-emit ws)
  (match ws
    [(ws:def@ #f x xe) `(define ,x ,(we-emit1 xe))]
    [(ws:def@ at x xe) `(@ ,at (define ,x ,(we-emit1 xe)))]
    [(ws:assert! assume? what why)
     `(,(if assume? 'require! 'assert!) ,(we-emit1 what))]
    [(ws:send-recv from to xs #f)
     `[,from -> ,to : ,xs]]
    [(ws:send-recv from to xs amt)
     `[,from -> ,to : ,xs #:pay ,amt]]))

;; WholeProgram -> SExpr
(define (wp-emit wp)
  (match-define (wp:program part->st n->fun in) wp)
  (define ps (sort-symbols (hash-keys part->st)))
  (define fs (cons in (sort-symbols (remove in (hash-keys n->fun)))))
  `(program
    ,@(for/list ([p (in-list ps)])
        `(participant ,p ,@(hash-ref part->st p)))
    ,@(for/list ([f (in-list fs)])
        (match-define (wf:fun args body) (hash-ref n->fun f))
        `(define (,f ,@args) ,@(we-emit body)))))

;; Testing Parsing and Emitting for Whole-Program
(module+ test
  (define wp:rock-paper-scissors-se
    `(program
      (participant A wager-amount escrow-amount)
      (participant B)
      (define (main)
        ; begin constructor
        (@ A (define wager+escrow-amount (+ wager-amount escrow-amount)))
        (@ A (define a-salted-hand (msg-cat (random-salt) (input-hand))))
        (@ A (define a-commitment (digest a-salted-hand)))
        [A -> #f : (wager-amount a-commitment) #:pay wager+escrow-amount]
        (require! (< wager-amount wager+escrow-amount))
        (define escrow-amount (- wager+escrow-amount wager-amount))
        ; end constructor
        ; begin player1_show_hand
        (@ B (define b-hand (input-hand)))
        [B -> #f : (b-hand) #:pay wager-amount]
        (require! (hand? b-hand))
        ; end player1_show_hand
        ; begin player0_reveal
        [A -> #f : (a-salted-hand)]
        (require! (equal? (digest a-salted-hand) a-commitment))
        (define a-hand (msg-right a-salted-hand))
        (require! (hand? a-hand))
        ;; outcome 0 -> B wins
        ;; outcome 1 -> draw
        ;; outcome 2 -> A wins
        (define outcome (modulo (+ a-hand (- 4 b-hand)) 3))
        (cond [(equal? outcome 2)
               ; A wins
               (define a-gets (+ (* 2 wager-amount) escrow-amount))
               [#f -> A : () #:pay a-gets]]
              [(equal? outcome 0)
               ; B wins
               (define a-gets escrow-amount)
               (define b-gets (* 2 wager-amount))
               [#f -> A : () #:pay a-gets]
               [#f -> B : () #:pay b-gets]]
              [else
               ; draw
               (define a-gets (+ wager-amount escrow-amount))
               (define b-gets wager-amount)
               [#f -> A : () #:pay a-gets]
               [#f -> B : () #:pay b-gets]])
        ; end player0_reveal
        )

      (define (random-salt)
        (random))

      (define (input-hand)
        (random 3))

      (define (hand? h)
        (and (integer? h) (<= 0 h) (< h 3)))))

  (define wp:rock-paper-scissors
    (wp-parse wp:rock-paper-scissors-se))
  (chk (wp-parse (wp-emit wp:rock-paper-scissors))
       wp:rock-paper-scissors))

;; -----------------------------------------------

;; Data Defs for Scope Analysis and Send-Receive Tagging

;; A WholeEnv is a [Hashof Participant PartEnv]
;; A PartEnv is a [Hashof Symbol Symbol]

;; WholeEnv Participant Symbol -> Symbol
(define (env-lookup Γ me v)
  (define Γme
    (hash-ref Γ me
      (λ () (error 'env-lookup "no environment for ~e" me))))
  (hash-ref Γme v
    (λ () (error 'env-lookup "~e does not know ~e" me v))))

;; WholeEnv Participant [Listof [List Symbol Symbol]] -> WholeEnv
(define (env-extend Γ me ents)
  (define (part-extend Γp)
    (for/fold ([Γp Γp])
              ([ent (in-list ents)])
      (match-define (list k v) ent)
      (hash-set Γp k v)))
  (cond
    [(symbol? me)
     (define Γme
       (hash-ref Γ me
         (λ () (error 'env-extend "no environment for ~e" me))))
     (hash-set Γ me (part-extend Γme))]
    [else
     (for/hash ([(p Γp) (in-hash Γ)])
       (cond
         [(can-specify? me p) (values p (part-extend Γp))]
         [else                (values p Γp)]))]))

(define-data whole-program-tagged
  (wpt:program num-send-recv part->st n->fun in))

(define-data whole-function-tagged
  (wft:fun args body))

(define-data whole-expr-tagged
  (wet:var v)
  (wet:con b)
  (wet:block stmt body)
  (wet:if ce te fe)
  (wet:app op args))

(define-data whole-stmt-tagged
  (wst:def@ @ x xe)
  (wst:send-recv tag from to xs amt)
  (wst:assert! assume? what why))

;; WholeProgramTagged -> WholeProgram
(define (wp-untag wp)
  (match-define (wpt:program _ part->st n->fun in) wp)
  (wp:program part->st
              (for/hasheq ([(n fun) (in-hash n->fun)])
                (values n (wf-untag fun)))
              in))
;; WholeFunctionTagged -> WholeFunction
(define (wf-untag wf)
  (match-define (wft:fun args body) wf)
  (wf:fun args (we-untag body)))
;; WholeExprTagged -> WholeExpr
(define (we-untag e)
  (match e
    [(wet:var v)       (we:var v)]
    [(wet:con b)       (we:con b)]
    [(wet:if ce te fe) (we:if (we-untag ce) (we-untag te) (we-untag fe))]
    [(wet:app op args) (we:app op (map we-untag args))]
    [(wet:block s be)  (we:block (ws-untag s) (we-untag be))]))
;; WholeStmtTagged -> WholeStmt
(define (ws-untag s)
  (match s
    [(wst:assert! assume? what why)   (ws:assert! assume? (we-untag what) why)]
    [(wst:def@ at x xe)               (ws:def@ at x (we-untag xe))]
    [(wst:send-recv _ from to xs amt) (ws:send-recv from to xs amt)]))

(define wpt-emit (compose wp-emit wp-untag))
(define wet-emit (compose we-emit we-untag))
(define wst-emit (compose ws-emit ws-untag))

;; -----------------------------------------------

;; Scope Analysis and Send-Receive Tagging

;; WholeProgram -> WholeProgramTagged
(define (wp-tag wp)
  ;; MUTABLE Natural
  ;; i represents the next unused send-recv tag
  (define i 0)
  ;; -> Natural
  (define (next-i) (begin0 i (set! i (add1 i))))
  
  ;; Participant Env WholeExpr -> WholeExprTagged
  (define (we-tag me Γ e)
    (define (we-rec e) (we-tag me Γ e))
    (define (ws-rec s) (ws-tag me Γ s))
    (match e
      [(we:var v)
       (wet:var (env-lookup Γ me v))]
      [(we:con b) (wet:con b)]
      [(we:if ce te fe)
       (wet:if (we-rec ce) (we-rec te) (we-rec fe))]
      [(we:app op args)
       (wet:app op (map we-rec args))]
      [(we:block s be)
       (define-values [Γ* st] (ws-rec s))
       (wet:block st (we-tag me Γ* be))]))

  ;; Participant Env WholeStmt -> (values Env WholeStmtTagged)
  (define (ws-tag me Γ s)
    (match s
      [(ws:assert! assume? what why)
       (values Γ (wst:assert! assume? (we-tag me Γ what) why))]
      [(ws:def@ at x xe)
       (unless (can-specify? me at)
         (error 'wp-tag "~e can't tell ~e what to define" me at))
       (define xt (gensym x))
       (define xet (we-tag at Γ xe))
       (define Γ* (env-extend Γ at (list (list x xt))))
       (values Γ* (wst:def@ at xt xet))]
      [(ws:send-recv from to xs amt)
       (unless (can-specify? me from)
         (error 'wp-tag "~e can't tell ~e what to send" me from))
       (unless (can-specify? me to)
         (error 'wp-tag "~e can't tell ~e what to receive" me to))
       (define tag (next-i))
       (define xts (map (env-lookup Γ from _) xs))
       (define amtt (and amt (env-lookup Γ from amt)))
       (define xs->xts (map list xs xts))
       (define Γ*
         (env-extend Γ to (if amt (cons (list amt amtt) xs->xts) xs->xts)))
       (values Γ* (wst:send-recv tag from to xts amtt))]))

  (define (wf-tag Γ f)
    (match-define (wf:fun args body) f)
    (define argts (map gensym args))
    (define Γ* (env-extend Γ #f (map list args argts)))
    (wft:fun argts (we-tag #f Γ* body)))

  ;; TODO: with-fresh?
  (match-define (wp:program part->st n->fun in) wp)
  (define Γ
    (hash-set
     (for/hash ([(p xs) (in-hash part->st)])
       (define xts (map gensym xs))
       (values p (make-immutable-hash (map cons xs xts))))
     #f
     (hash)))
  (define n->fun*
    (for/hasheq ([(n fun) (in-hash n->fun)])
      (values n (wf-tag Γ fun))))
  (wpt:program
   i
   part->st
   n->fun*
   in))

;; Testing Scope Analysis and Send-Recv Tagging
(module+ test
  (define wpt:rock-paper-scissors
    (wp-tag wp:rock-paper-scissors)))

;; -----------------------------------------------

;; TODO: ANF transformation on the Whole-Program,
;;       before end-point projection

;; WholeExprTagged -> Bool
(define (wa-anf? wa)
  (or (wet:var? wa) (wet:con? wa)))

;; WholeExprTagged -> Bool
(define (we-anf? we)
  (match we
    [(? wa-anf?)       #t]
    [(wet:app op args) (andmap wa-anf? args)]
    [(wet:if ce te fe) (and (wa-anf? ce) (wt-anf? te) (wt-anf? fe))]))

;; WholeExprTagged -> Bool
(define (wt-anf? we)
  (match we
    [(? we-anf? we)      #t]
    [(wet:block stmt be) (and (ws-anf? stmt) (wt-anf? be))]))

;; WholeStmtTagged -> Bool
(define (ws-anf? ws)
  (match ws
    [(wst:assert! _ ce _)      (wa-anf? ce)]
    [(wst:send-recv _ _ _ _ _) #t]
    [(wst:def@ _ _ xe)         (we-anf? xe)]))

;; WholeFunctionTagged -> Bool
(define (wf-anf? wf)
  (match-define (wft:fun _ e) wf)
  (wt-anf? e))

;; WholeProgramTagged -> Bool
(define (wp-anf? wp)
  (match-define (wpt:program _ _ n->fun in) wp)
  (and (eq? in 'main)
       (= 1 (hash-count n->fun))
       (hash-has-key? n->fun 'main)
       (wf-anf? (hash-ref n->fun 'main))))

;; WholeProgramTagged -> WholeProgramAnf
(define (wp-anf wp)
  (match-define (wpt:program num-send-recv part->st n->fun in) wp)
  (define n->fun*
    (for/hasheq ([(n fun) (in-hash n->fun)])
      (values n (wf-anf fun))))
  (wpt:program num-send-recv part->st n->fun* in))

;; WholeFunctionTagged -> WholeFunctionAnf
(define (wf-anf wf)
  (match-define (wft:fun args body) wf)
  (wft:fun args (wt-anf body)))

;; WholeExprTagged -> WholeTailAnf
(define (wt-anf we)
  (cond [(wa-anf? we) we]
        [else
         (define-values [stmts be] (we-anf we))
         (foldr wet:block be stmts)]))

;; WholeExprTagged -> (values [Listof WholeStmtAnf] WholeArgAnf)
(define (wa-anf we)
  (cond [(wa-anf? we) (values '() we)]
        [else
         (define temp (gensym 'anf-temp))
         (define stmts (ws-anf (wst:def@ #f temp we)))
         (values stmts (wet:var temp))]))

;; WholeExprTagged -> (values [Listof WholeStmtAnf] WholeExprAnf)
(define (we-anf we)
  (match we
    [(? wa-anf?) (values '() we)]
    [(wet:app op es)
     (define-values [stmtss as]
       (for/lists [stmtss as] ([e (in-list es)])
         (wa-anf e)))
     (values (append* stmtss) (wet:app op as))]
    [(wet:if ce te fe)
     (define-values [stmts ca] (wa-anf ce))
     (values stmts (wet:if ca (wt-anf te) (wt-anf fe)))]
    [(wet:block stmt be)
     (define stmts1 (ws-anf stmt))
     (define-values [stmts2 be3] (we-anf be))
     (values (append stmts1 stmts2) be3)]))

;; WholeStmtTagged -> [Listof WholeStmtAnf]
(define (ws-anf ws)
  (match ws
    [(wst:send-recv _ _ _ _ _) (list ws)]
    [(wst:assert! assume? ce why)
     (define-values [stmts ca] (wa-anf ce))
     (append stmts (list (wst:assert! assume? ca why)))]
    [(wst:def@ at x xe)
     (define-values [stmts xe-anf] (we-anf xe))
     (append stmts (list (wst:def@ at x xe-anf)))]))

;; Testing ANF on Tagged Whole-Programs
(module+ test
  (define wpt:rock-paper-scissors-anf
    (wp-anf wpt:rock-paper-scissors))

  ;(chk #:? wp-anf? wpt:rock-paper-scissors-anf)
  (pretty-write
   (wpt-emit wpt:rock-paper-scissors-anf)))

;; -----------------------------------------------

