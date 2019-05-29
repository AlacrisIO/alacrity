#lang debug racket/base

(provide hp-program->sol)

(require racket/match
         "core.rkt")
(module+ test
  (require chk))

(define (sort-symbols l)
  (sort l string<=? #:key symbol->string))

;; -----------------------------------------------

;; wp-sexpr->sol : SExpr -> Solidity
(define (wp-sexpr->sol se)
  (wp-program->sol (wp-parse se)))

;; wp-program->sol : WP:Program -> [Hashof Symbol Solidity]
(define (wp-program->sol wp)
  (for/hash ([(p dp) (in-hash (wp-epp wp))])
    (values p (dp-program->sol dp))))

;; -----------------------------------------------

;; dp-sexpr->sol : SExpr -> Solidity
(define (dp-sexpr->sol se)
  (dp-program->sol (dp-parse se)))

;; dp-program->sol : DP:Program -> Solidity
(define (dp-program->sol dp)
  (hp-program->sol (direct->handle dp)))

;; -----------------------------------------------

;; hp-sexpr->sol : SExpr -> Solidity
(define (hp-sexpr->sol se)
  (hp-program->sol (hp-parse se)))

;; hp-program->sol : HP:Program -> Solidity
(define (hp-program->sol hp)
  (match-define (hp:program args vs n->handler in) hp)
  (define ns (cons in (sort-symbols (remove in (hash-keys n->handler)))))
  `(contract ,in
     (args ,args)
     (vs ,vs)
     ,@(for/list ([f (in-list ns)])
         (match-define (hh:handler ht) (hash-ref n->handler f))
         (cond
           [(equal? f in)
            `(constructor ,f () ,@(ht->sol ht))]
           [else
            `(function ,f () ,@(ht->sol ht))]))))

;; ht->sol : HT -> Solidity
(define (ht->sol ht)
  (match ht
    [(ht:set!& x xe rht)      (ht-emit ht)]
    [(ht:if ce tt ft)         (ht-emit ht)]
    [(ht:wait* ns msgs recv?) (ht-emit ht)]))

;; -----------------------------------------------

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
  (chk (wp-parse (wp-emit wp:adds)) wp:adds)

  (wp-program->sol wp:adds)
  )
