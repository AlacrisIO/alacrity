#lang racket/base

(require racket/function
         racket/list
         racket/match
         "traverse-s-expr.rkt")
(module+ test
  (require racket/pretty rackunit))

;; A sends `(a b)` to everyone and deposits `p` into the contract
;; the contract sends A `(c d)` and pays A back `q`

;; =====================================
;; Single-payment P2P Style

#;#;#;#;;-------------------
(@ A (define a (compute-a)))
(@ A (define b (compute-b)))
(@ A (define p (compute-p)))
[A -> #f : (a b) payment p]
#;#;#;#;;-------------------
(define c (compute-c))
(define d (compute-d))
(define q (compute-q))
[#f -> A : (c d) payment q]
; decl = ...
;      | (@ participant (define id expr))
;      | (define id expr)
;      | [participant -> participant : (id ...)]
;      | [participant -> participant : (id ...) payment id]

;; In solidity, the `p` becomes `msg.value`, and the `q`
;; becomes the argument to `A.transfer`

;; =====================================
;; Multi-payment P2P Style

#;#;#;#;;--------------------
(@ A (define a (compute-a)))
(@ A (define b (compute-b)))
(@ A (define p (compute-p)))
[A -> #f : (a b (payment p))]
#;#;#;#;;--------------------
(define c (compute-c))
(define d (compute-d))
(define q (compute-q))
[#f -> A : (c b (payment q))]
; decl = ...
;      | (@ participant (define id expr))
;      | (define id expr)
;      | [participant -> participant : (info-or-payment ...)]
; info-or-payment = id
;                 | (payment id)

;; =====================================
;; Expression-piece Direct Style

#;#;#;;----------------------
(define a (@ A (compute-a)))
(define b (@ A (compute-a)))
(define p (@deposit A (compute-p)))
#;#;#;;----------------------
(define c (compute-c))
(define d (compute-d))
(transfer A (compute-q))
; decl = ...
;      | (@ participant (define id expr))
;      | (define id expr)
;      | (transfer participant expr)
; expr = ...
;      | (@ participant expr)
;      | (@deposit participant expr)

;; ---------------------------------------------------------
;; ---------------------------------------------------------
;; ---------------------------------------------------------



;; ExpressionPieceDirectStyleDecl -> MultiPaymentP2PStyleDecl
(define (epds-decl->mpps-decl d)
  (match d
    [`(@ ,p (define ,x ,e))
     `(@ ,p (define ,x ,(epds-expr->mpps-expr e)))]
    [`(define ,x ,e)
     `(define ,x ,(epds-expr->mpps-expr e))]
    [`(transfer ,p ,e)
     (define transfer-amount (gensym 'transfer-amount))
     `(splice (define ,transfer-amount ,e)
              [#f -> ,p : ((payment ,transfer-amount))])]
    [`(require! ,e)
     `(require! ,(epds-expr->mpps-expr e))]
    [`(splice ,@ds)
     `(splice ,@(map epds-decl->mpps-decl ds))]))

;; ExpressionPieceDirectStyleExpr -> MultiPaymentP2PStyleExpr
(define (epds-expr->mpps-expr e)
  (match e
    [`(@ ,p ,e)
     (define temp (gensym 'temp))
     `(begin (@ ,p (define ,temp ,e))
             [,p -> #f : (,temp)]
             ,temp)]
    [`(@deposit ,p ,e)
     (define deposit-amount (gensym 'deposit-amount))
     `(begin (@ ,p (define ,deposit-amount ,e))
             [,p -> #f : ((payment ,deposit-amount))]
             ,deposit-amount)]
    [`(begin ,ds ... ,e)
     `(begin ,@(map epds-decl->mpps-decl ds) ,(epds-expr->mpps-expr e))]
    [_
     (traverse-s-expr/recur e 0 epds-expr->mpps-expr)]))

;; -------------------------------------

;; MultiPaymentP2PStyleDecl -> SinglePaymentP2PStyleDecl
(define (mpps-decl->spps-decl d)
  (match d
    [`(@ ,p (define ,x ,e))
     `(@ ,p (define ,x ,(mpps-expr->spps-expr e)))]
    [`(define ,x ,e)
     `(define ,x ,(mpps-expr->spps-expr e))]
    [`(splice ,@ds)
     `(splice ,@(map mpps-decl->spps-decl ds))]
    [`(require! ,e)
     `(require! ,(mpps-expr->spps-expr e))]
    [`[,p1 -> ,p2 : (,infos/payments ...)]
     (define-values [infos payments] (partition symbol? infos/payments))
     (match payments
       ['()
        `[,p1 -> ,p2 : (,@infos)]]
       [`((payment ,payment-amount))
        `[,p1 -> ,p2 : (,@infos) payment ,payment-amount]]
       [`((payment ,payment-amounts) ...)
        (define payment-total (gensym 'payment-total))
        `(splice
          (@ ,p1 (define payment-total (+ ,@payment-amounts)))
          [,p1 -> ,p2 : (,@infos ,@payment-amounts) payment ,payment-total]
          ; TODO: in a case where p2 is not the consensus,
          ;       how should the consensus enforce this
          ;       require constraint?
          (@ ,p2 (require (equal? ,payment-total (+ ,@payment-amounts)))))])]))

;; MultiPaymentP2PStyleExpr -> SinglePaymentP2PStyleExpr
(define (mpps-expr->spps-expr e)
  (match e
    [`(begin ,ds ... ,e)
     `(begin ,@(map mpps-decl->spps-decl ds) ,(mpps-expr->spps-expr e))]
    [_
     (traverse-s-expr/recur e 0 mpps-expr->spps-expr)]))

;; ExpressionPieceDirectStyleDecl -> SinglePaymentP2PStyleDecl
(define epds-decl->spps-decl (compose mpps-decl->spps-decl epds-decl->mpps-decl))

;; ExpressionPieceDirectStyleExpr -> SinglePaymentP2PStyleExpr
(define epds-expr->spps-expr (compose mpps-expr->spps-expr epds-expr->mpps-expr))

;; ---------------------------------------------------------

(module+ test
  (define rock-paper-scissors-epds-expr
    `(begin
       (define wager-amount (@deposit A (input-wager)))
       (define escrow-amount (@deposit A (input-escrow)))
       (@ A (define A-private-sh (msg-cat (random-salt) (input-hand))))
       (define A-commitment (@ A (digest A-private-sh)))
       (define B-wager-amount (@deposit B wager-amount))
       (require! (equal? B-wager-amount wager-amount))
       (define B-hand (@ B (input-hand)))
       (define A-sh (@ A A-private-sh))
       (require! (equal? (digest A-sh) A-commitment))
       (define A-hand (msg-right A-sh))
       ;; diff = 0 -> B wins
       ;; diff = 1 -> draw
       ;; diff = 2 -> A wins
       (define diff (modulo (+ A-h (- 4 B-h)) 3))
       (cond [(equal? diff 2)
              ; A wins
              (transfer A (+ (* 2 wager-amount) escrow-amount))]
             [(equal? diff 0)
              ; B wins
              (transfer A escrow-amount)
              (transfer B (* 2 wager-amount))]
             [else
              ; draw
              (transfer A (+ wager-amount escrow-amount))
              (transfer B wager-amount)]))))

;; ---------------------------------------------------------

(define (attempt-flatten s)
  (define (as-begin s)
    (match s [`(begin . ,_) s] [_ `(begin ,s)]))
  (define (as-splice s)
    (match s [`(splice . ,_) s] [_ `(splice ,s)]))
  (match s
    [`(begin ,ds ... ,e)
     (match* [(map (compose as-splice attempt-flatten) ds)
              (as-begin (attempt-flatten e))]
       [[`[(splice ,dss1 ...) ...] `(begin ,ds2 ... ,e)]
        (match (append (append* dss1) ds2)
          ['() e]
          [ds `(begin ,@ds ,e)])])]
    [`(splice ,ds ...)
     (match (map (compose as-splice attempt-flatten) ds)
       [`[(splice ,dss ...) ...]
        (match (append* dss)
          ['() `(splice)]
          [`(,d) d]
          [ds `(splice ,@ds)])])]
    [`[,_ -> ,_ : . ,_] s]
    [_
     (traverse-s-expr/recur s 0 attempt-flatten)]))

(module+ test
  (define (show . ss)
    (for ([s (in-list ss)])
      (printf "~a\n" (make-string 50 #\-))
      (pretty-write (attempt-flatten s))))

  (define epds-decl
    `(splice
      (define a (@ A (compute-a)))
      (define b (@ A (compute-a)))
      (define p (@deposit A (compute-p)))
      ; ---
      (define c (compute-c))
      (define d (compute-d))
      (transfer A (compute-q))))

  #;(show
     epds-decl
     (epds-decl->mpps-decl epds-decl)
     (epds-decl->spps-decl epds-decl))

  (show
   rock-paper-scissors-epds-expr
   (epds-expr->spps-expr rock-paper-scissors-epds-expr))
  )
