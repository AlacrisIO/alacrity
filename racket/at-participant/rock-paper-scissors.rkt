#lang agile

(require "lib.rkt"
         "participant.rkt"
         "define-interaction-direct-style.rkt")

;; ---------------------------------------------------------

(define (random-hand)
  (random-element '(rock paper scissors)))

(define (hand<? h1 h2)
  (match* [h1 h2]
    [['rock 'paper] #t]
    [['paper 'scissors] #t]
    [['scissors 'rock] #t]
    [[_ _] #f]))

(define-interaction rock-paper-scissors12 [p1 p2]
  (synchronous
   (define salted-hand1 (@ p1 (random-salted (random-hand))))
   (define salted-hand2 (@ p2 (random-salted (random-hand)))))
  (define hand1 (salted-value salted-hand1))
  (define hand2 (salted-value salted-hand2))
  (cond
    [(hand<? hand1 hand2) (displayln "p2 wins")]
    [(hand<? hand2 hand1) (displayln "p1 wins")]
    [else                 (displayln "tie")]))

(define t12 (thread rock-paper-scissors12))
(define t1 (thread p1))
(define t2 (thread p2))
(thread-wait t12)
(thread-wait t1)
(thread-wait t2)

(define-interaction rock-paper-scissors34 [p3 p4]
  (let loop ()
    (synchronous
     (define salted-hand3 (@ p3 (random-salted (random-hand))))
     (define salted-hand4 (@ p4 (random-salted (random-hand)))))
    (define hand3 (salted-value salted-hand3))
    (define hand4 (salted-value salted-hand4))
    (cond
      [(hand<? hand3 hand4) (displayln "p4 wins")]
      [(hand<? hand4 hand3) (displayln "p3 wins")]
      [else                 (displayln "tie; play again") (loop)])))

(define t34 (thread rock-paper-scissors34))
(define t3 (thread p3))
(define t4 (thread p4))
(thread-wait t34)
(thread-wait t3)
(thread-wait t4)

