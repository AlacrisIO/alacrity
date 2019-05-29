#lang racket

;; ---------------------------------------------------------
#;#;#;#;#;#;#;#;#;#;#;

[A -> #f : x]
...stuff...   ; where there are no sends to A or #f
[A -> #f : y]

  --->

(define temp_x @ A x)
...A-does-stuff...
(define temp_y @ A y)
[A -> #f : (temp_x temp_y)]
(define x temp_x)
...stuff...
(define y temp_y)

;; ---------------------------------------------------------
#;#;#;#;#;#;#;#;#;#;#;#;#;#;

(define ,_seq_ @ A (! (msg (sign x A))))
(match @ #f : (?) as (sign x A))
...stuff...   ;where there are no receives for A or #f
(define ,_seq_ @ A (! (msg (sign y A))))
(match @ #f : (?) as (sign y A))

  --->

(define temp_x @ A x)
...A-does-stuff...
(define temp_y @ A y)
(define ,_seq_ @ A (! (msg (sign (temp_x temp_y) A))))
(match @ #f : (?) as (sign (temp_x temp_y) A))
(define x temp_x)
...stuff...
(define y temp_y)

;; ---------------------------------------------------------

;; WP -> WP
(define (merge-consecutive-actions-by-same-participant wp)
  ('???))

