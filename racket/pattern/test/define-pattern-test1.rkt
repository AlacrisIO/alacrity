#lang agile

(require "../matcher.rkt"
         "../pattern.rkt"
         "../define-pattern.rkt")
(module+ test
  (require rackunit))

;; ---------------------------------------------------------

(define-pattern (Id x) #:bind [x] <- x)
(define-pattern Empty <- '())

(module+ test
  (check-equal? (match 570 with
                  [(Id x) -> x])
                570)
  (check-true (match '() with
                [Empty -> #true]
                [_     -> #false]))
  (check-false (match 73 with
                 [Empty -> #true]
                 [_     -> #false]))
  (check-true (match '() with
                [(Id Empty) -> #true]
                [_          -> #false]))
  (check-false (match 73 with
                 [(Id Empty) -> #true]
                 [_          -> #false])))

;; ---------------------------------------------------------

(define-pattern (Listof p) #:bind [] <-
  {~or '()
       (cons p (Listof p))})

(define-pattern (Listof/len p len) #:bind [len] <-
  {~or {~and '() {~with len 0}}
       (cons p (Listof/len p {~app add1 len}))})

(module+ test
  (check-true (match '() with
                [(Listof {~pred number?}) -> #true]
                [_                        -> #false]))
  (check-equal? (match '() with
                  [(Listof/len {~pred number?} n) -> n])
                0)
  (check-true (match (list 1 2 3) with
                [(Listof {~pred number?}) -> #true]
                [_                        -> #false]))
  (check-equal? (match (list 1 2 3) with
                  [(Listof/len {~pred number?} n) -> n])
                3)
  (check-false (match (list 1 "tooo" 3) with
                 [(Listof {~pred number?}) -> #true]
                 [_                        -> #false]))
  (check-false (match (cons 1 3) with
                 [(Listof {~pred number?}) -> #true]
                 [_                        -> #false]))
  (check-true (match (list "fa" 4 "d") with
                [(Listof x) -> #true]
                [_          -> #false]))
  (check-false (match "Experience 'Fa!' in amazing 4D!" with
                 [(Listof x) -> #true]
                 [_          -> #false]))
  (check-equal? (match (list "do" "re" "mi" "fa" "sol" "la" "ti") with
                  [(Listof/len x n) -> n])
                7)
  )

;; ---------------------------------------------------------

(define-pattern (DCons first rest)
  #:bind [first rest] <-
  {~or {~and '() {~with (cons first rest) '(0)}}
       (cons first rest)})

(module+ test
  (check-equal? (match '() with
                  [(DCons x y) -> (list x y)]
                  [_           -> #false])
                (list 0 '()))
  (check-equal? (match (list 1 2 3) with
                  [(DCons x y) -> (list x y)]
                  [_           -> #false])
               (list 1 '(2 3)))
  (check-equal? (match "won too many" with
                  [(DCons x y) -> (list x y)]
                  [_           -> #false])
               #false)
  (check-equal? (match (list 1 2 6) with
                  [(DCons {~and {~pred number?} x} y) -> (list x y)]
                  [_                                  -> #false])
               (list 1 '(2 6)))
  (check-equal? (match (list "dx/dx" 2 6) with
                  [(DCons {~and {~pred number?} x} y) -> (list x y)]
                  [_                                  -> #false])
               #false)
  (check-equal? (match (list 1 2 6 24) with
                  [(DCons x (DCons v (DCons a (DCons j (DCons dj ddjs))))) ->
                   (list x v a j dj ddjs)]
                  [_ -> #false])
               (list 1 2 6 24 0 '()))
  )

;; ---------------------------------------------------------
