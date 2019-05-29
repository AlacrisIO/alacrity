#lang racket/base

(require racket/list
         racket/match
         "core.rkt"
         "tmp/traverse-s-expr.rkt")
(module+ test
  (require rackunit
           "core-to-sol.rkt"))

;; direct-style-wp-se->core-wp-se : S-Expr -> S-Expr
(define (direct-style-wp-se->core-wp-se dswp-se)
  (match dswp-se
    [`(program (participant ,ps) ... . ,stuff)
     `(program
       (participant contract ,@ps)
       ,@(map (λ (p) `(participant ,p)) ps)
       ,@(direct-style-we-se->core-we-se stuff))]))

;; direct-style-we-se->core-we-se : S-Expr -> S-Expr
(define (direct-style-we-se->core-we-se dswe-se)
  (match dswe-se
    [`(@ ,p (define ,x ,e))
     `(define ,x @ ,p ,e)]
    [`(@ ,p ,e)
     (define temp (gensym 'temp))
     `(begin
        (define ,temp @ ,p ,e)
        [,p -> #f : ,temp]
        ,temp)]
    [`(@deposit ,p ,e)
     (direct-style-we-se->core-we-se `(@ ,p ,e))]
    [`(and ,a ,b)
     `(if ,(direct-style-we-se->core-we-se a)
          ,(direct-style-we-se->core-we-se b)
          #f)]
    [e
     (traverse-s-expr/recur e 0 direct-style-we-se->core-we-se)]))

#|
;; direct-style-wp-se->core-wp-se : S-Expr -> S-Expr
(define (direct-style-wp-se->core-wp-se dswp-se)
  (match dswp-se
    [`(program (participant ,participant-name) ... . ,body)
     `(program
       (participant consensus)
       ,@(map (λ (p) `(participant ,p)) participant-name)
       (define (main)
         ,@(append-map direct-style-stmt-se->core-stmt-se body)))]))

;; direct-style-stmt-se->core-stmt-se : S-Expr -> [Listof S-Expr]
(define (direct-style-stmt-se->core-stmt-se dssm-se)
  (match dssm-se
    [`(define ,x ,e)
     (define-values [pre e*] (direct-style-expr-se->core-expr-se e))
     (append
      pre
      `[(define ,x @ #f ,e*)])]
    [`(@ ,p (define ,x ,e))
     `[(define ,x @ ,p ,e)]]
    [e
     (define-values [pre e*] (direct-style-expr-se->core-expr-se e))
     (append pre `[,e*])]))

;; direct-style-expr-se->core-expr-se : S-Expr -> (values [Listof S-Expr] S-Expr)
(define (direct-style-expr-se->core-expr-se dse-se)
  (define (rec-single e)
    (define-values [pre e*] (direct-style-expr-se->core-expr-se e))
    (cond [(empty? pre) e*]
          [else `(begin ,@pre e*)]))
  (match dse-se
    [(? symbol? x) (values '() x)]
    [`(begin) (values '() `(begin))]
    [`(begin ,e) (direct-style-expr-se->core-expr-se e)]
    [`(begin ,body ...)
     `(begin ,@(append-map direct-style-stmt-se->core-stmt-se body))]
    [`(@ ,p ,e)
     (define temp (gensym 'temp))
     (values `[(define ,temp @ ,p ,e)
               [,p -> #f : ,temp]]
             temp)]
    [`(if ,c ,t ,e)
     (define temp (gensym 'temp))
     (define-values [c-pre c*] (direct-style-expr-se->core-expr-se c))
     (values `[,@c-pre
               (define ,temp ,c*)]
             `(if ,temp ,(rec-single t) ,(rec-single e)))]
    [`(and ,c ,t)
     (define temp (gensym 'temp))
     (define-values [c-pre c*] (direct-style-expr-se->core-expr-se c))
     (values `[,@c-pre
               (define ,temp ,c*)]
             `(and ,temp ,(rec-single t)))]

    [`(,(? symbol? f) . ,as)
     (define-values [as-pre as*]
       (for/lists (as-pre as*) ([a (in-list as)])
         (define-values [a-pre a*] (direct-style-expr-se->core-expr-se a))
         (define temp (gensym 'temp))
         (values `[,@a-pre
                   (define ,temp ,a*)]
                 temp)))
     (values (append* as-pre)
             `(,f . ,as*))]
    ))
|#

;; ---------------------------------------------------------

(module+ test
  (define dswpse
    `(program
      (participant A)
      (participant B)
      (define (main)
        (define wager-amount (@deposit A (input-wager)))
        (define escrow-amount (@deposit A (input-escrow)))
        (@ A (define A-private-sh (msg-cat (random-salt) (input-hand))))
        (define A-commitment (@ A (digest A-private-sh)))
        (define B-wager-amount (@deposit B wager-amount))
        (require (equal? B-wager-amount wager-amount))
        (define B-hand (@ B (input-hand)))
        (define A-sh (@ A A-private-sh))
        (require (equal? (digest A-sh) A-commitment))
        (define A-hand (msg-right A-sh))
        (determine-winner A-hand B-hand wager-amount escrow-amount))
      (define (input-hand) (random 3))
      (define (random-salt) (random))
      (define (input-wager) 10)
      (define (input-escrow) 1)
      (define (determine-winner A-h B-h wager-amount escrow-amount)
        ;; diff = 0 -> B wins
        ;; diff = 1 -> draw
        ;; diff = 2 -> A wins
        (define diff (modulo (+ A-h (- 4 B-h)) 3))
        (cond [(equal? diff 2) (A-wins wager-amount escrow-amount)]
              [(equal? diff 0) (B-wins wager-amount escrow-amount)]
              [else (draw wager-amount escrow-amount)]))
      (define (A-wins wager-amount escrow-amount)
        (transfer A (+ (* 2 wager-amount) escrow-amount)))
      (define (B-wins wager-amount escrow-amount)
        (transfer A escrow-amount)
        (transfer B (* 2 wager-amount)))
      (define (draw wager-amount escrow-amount)
        (transfer A (+ wager-amount escrow-amount))
        (transfer B wager-amount))))

  (define core-wpse (direct-style-wp-se->core-wp-se dswpse))
  (check-match
   core-wpse
   `(program
     (participant contract)
     (participant A)
     (participant B)
     (define (main)
       (define sh1 @ A (msg-cat (random) (random 3)))
       (define sh2 @ B (msg-cat (random) (random 3)))
       (define A-d (begin (define ,temp1 @ A (digest sh1)) [A -> #f : ,temp1] ,temp1))
       (define B-d (begin (define ,temp2 @ B (digest sh2)) [B -> #f : ,temp2] ,temp2))
       (define A-sh (begin (define ,temp3 @ A sh1) [A -> #f : ,temp3] ,temp3))
       (require (equal? (digest A-sh) A-d))
       (define B-sh (begin (define ,temp4 @ B sh2) [B -> #f : ,temp4] ,temp4))
       (require (equal? (digest B-sh) B-d))
       (define A-h (msg-right A-sh))
       (define B-h (msg-right B-sh))
       (determine-winner A-h B-h))
     (define (determine-winner A-h B-h)
       (define diff (modulo (+ A-h (- 4 B-h)) 3))
       (cond [(equal? diff 2) (A-wins)]
             [(equal? diff 0) (B-wins)]
             [else (draw)]))
     (define (A-wins)
       (transfer A (+ (* 2 wager-amount) escrow-amount)))
     (define (B-wins)
       (transfer A escrow-amount)
       (transfer B (* 2 wager-amount)))
     (define (draw)
       (transfer A (+ wager-amount escrow-amount))
       (transfer B wager-amount))))

  (define core-wp (wp-parse core-wpse))
  (define core-contract-dp (hash-ref (wp-epp core-wp) 'contract))
  (define core-contract-hp (direct->handle core-contract-dp))
  (define core-contract-sol (hp-program->sol core-contract-hp))
  core-contract-sol
  )

