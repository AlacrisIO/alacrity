#lang agile

(require "expand-util/let-definitions-anf.rkt"
         "expand-util/and-or.rkt"
         "lib.rkt"
         "participant.rkt"
         "define-interaction-direct-style.rkt")

(define (random-hand)
  (random-element '(rock paper scissors)))

(define (hand<? h1 h2)
  (match* [h1 h2]
    [['rock 'paper] #t]
    [['paper 'scissors] #t]
    [['scissors 'rock] #t]
    [[_ _] #f]))

(define-interaction con [p1 p2]
  (define msg1 (@payment p1 (payment 2 1)))
  (define p1-wager+escrow (payment-amount msg1))
  (define p1-wager (payment-value msg1))
  (@assert p1 (< p1-wager p1-wager+escrow) "must send escrow in addition to wager")
  (define msg2 (@payment p2 (payment p1-wager+escrow p1-wager)))
  (define p2-wager+escrow (payment-amount msg2))
  (define p2-wager (payment-value msg2))
  (@assert p2 (< p2-wager p2-wager+escrow) "must send escrow in addition to wager")
  (define p1-escrow (- p1-wager+escrow p1-wager))
  (define p2-escrow (- p2-wager+escrow p2-wager))
  (@assert p2 (and (= p2-wager p1-wager) (= p2-escrow p1-escrow)) "p2 wager and escrow must match p1")
  (synchronous
   (define salted-hand1 (@ p1 (random-salted (random-hand))))
   (define salted-hand2 (@ p2 (random-salted (random-hand)))))
  (define hand1 (salted-value salted-hand1))
  (define hand2 (salted-value salted-hand2))
  (cond
    [(hand<? hand1 hand2) (displayln "p2 wins")]
    [(hand<? hand2 hand1) (displayln "p1 wins")]
    [else                 (displayln "tie")]))
