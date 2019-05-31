(program
  (participant A wager-amount escrow-amount)
  (participant B)
  (define (main)
    ; begin constructor
    (@ A (define wager+escrow-amount (+ wager-amount escrow-amount)))
    (@ A (define a-salted-hand (msg-cat (random-salt) (input-hand))))
    (@ A (define a-commitment (digest a-salted-hand)))
    [A -> consensus : (wager-amount a-commitment) #:pay wager+escrow-amount]
    (require! (< wager-amount wager+escrow-amount))
    (define escrow-amount (- wager+escrow-amount wager-amount))
    ; end constructor
    ; begin player1_show_hand
    (@ B (define b-hand (input-hand)))
    [B -> consensus : (b-hand) #:pay wager-amount]
    (require! (hand? b-hand))
    ; end player1_show_hand
    ; begin player0_reveal
    [A -> consensus : (a-salted-hand)]
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
           [consensus -> A : () #:pay a-gets]]
          [(equal? outcome 0)
           ; B wins
           (define a-gets escrow-amount)
           (define b-gets (* 2 wager-amount))
           [consensus -> A : () #:pay a-gets]
           [consensus -> B : () #:pay b-gets]]
          [else
           ; draw
           (define a-gets (+ wager-amount escrow-amount))
           (define b-gets wager-amount)
           [consensus -> A : () #:pay a-gets]
           [consensus -> B : () #:pay b-gets]])
    ; end player0_reveal
    )

  (define (random-salt)
    (random))

  (define (input-hand)
    (random 3))

  (define (hand? h)
    (and (integer? h) (<= 0 h) (< h 3))))
