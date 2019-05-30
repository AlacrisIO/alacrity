(program
  (participant A wager-amount escrow-amount)
  (participant B)
  (define (main)
    ; begin constructor
    (define wager+escrow-amount @ A (+ wager-amount escrow-amount))
    (define a-sh @ A (msg-cat (random-salt) (input-hand)))
    (define a-commitment @ A (digest a-sh))
    [A -> #f : (wager-amount a-commitment) #:pay wager+escrow-amount]
    (require! (< wager-amount wager+escrow-amount))
    (define escrow-amount (- wager+escrow-amount wager-amount))
    ; end constructor
    ; begin player1_show_hand
    (define b-hand @ B (input-hand))
    [B -> #f : (b-hand) #:pay wager-amount]
    (require! (hand? b-hand))
    ; end player1_show_hand
    ; begin player0_reveal
    [A -> #f : (a-sh)]
    (require! (equal? (digest a-sh) a-commitment))
    (define a-hand (msg-right a-sh))
    (require! (hand? a-hand))
    ;; outcome 0 -> B wins
    ;; outcome 1 -> draw
    ;; outcome 2 -> A wins
    (define outcome (modulo (+ A-h (- 4 B-h)) 3))
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

  (define (input-hand)
    (random 3))

  (define (hand? h)
    (and (integer? h) (< h 3))))
