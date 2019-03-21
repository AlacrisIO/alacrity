#lang racket/base

(require "../pattern.rkt"
         "../define-pattern.rkt"
         "../digest-pattern.rkt"
         "../validate/validate.rkt")
(module+ test
  (require rackunit))

(define-pattern None (dbytes #""))
(define-pattern (Some v) #:bind [v] (dlist (dbytes #"\001") v))
(define-pattern (Option v) #:bind [] <-
  {~or None
       (Some v)})

(define-pattern Z (dbytes #""))
(define-pattern (S n) #:bind [n] (dlist (dbytes #"\001") n))
(define-pattern Nat <- {~or Z (S Nat)})

(define (pred n)
  (match n with [(S n-1) -> n-1]))
(define (nat< a b)
  (match* [a b] with
    [[Z (S _)]     -> #true]
    [[_ Z]         -> #false]
    [[(S a) (S b)] -> (nat< a b)]))

;; bits
(define-pattern B0 (dbytes #""))
(define-pattern B1 (dbytes #"\001"))
(define-pattern Bit <- {~or B0 B1})
;; binary numbers as little-endian lists of bits
(define-pattern BEmpty (dbytes #""))
(define-pattern (BCons b rst) #:bind [b rst] (dlist (dbytes #"\001") b rst))
(define-pattern Binary <- {~or BEmpty (BCons Bit Binary)})

(define (numbits bs)
  (match bs with
    [BEmpty        -> Z]
    [(BCons _ rst) -> (S (numbits rst))]))
(define (has-bit? bs i)
  (match i with
    [Z       -> (match bs with
                  [(BCons B1 _) -> #true]
                  [_            -> #false])]
    [(S i-1) -> (match bs with
                  [(BCons _ rst) -> (has-bit? rst i-1)]
                  [_             -> #false])]))

(define-pattern Empty (dbytes #""))
(define-pattern (Leaf v) #:bind [v] (dlist (dbytes #"\001") v))
(define-pattern (Branch h l r) #:bind [h l r] (dlist (dbytes #"\002") h l r))
(define-pattern (Trie v) #:bind [] <-
  {~or Empty
       (Leaf v)
       (Branch Nat (Trie v) (Trie v))})

(define (trie-height t)
  (match t with
    [Empty          -> None]
    [(Leaf v)       -> (Some Z)]
    [(Branch h _ _) -> (Some h)]))

(define (trie-find-opt trie key)
  (match (trie-height trie) with
    [None -> None]
    [(Some height) when (nat< height (numbits key)) ->
     None]
    [(Some height) ->
     (let f ([height height] [trie trie])
       (match trie with
         [Empty        -> None]
         [(Leaf value) -> (Some value)]
         [(Branch {~= height} left right) ->
          (let* ([child-height (pred height)]
                 [child (if (has-bit? key child-height) right left)])
            (f child-height child))]))]))

(define n0 Z)
(define n1 (S n0))
(define n2 (S n1))
(define n3 (S n2))
(define n4 (S n3))
(define n5 (S n4))
(define b0 BEmpty)
(define b1 (BCons B1 b0))
(define b2 (BCons B0 b1))
(define b3 (BCons B1 b1))
(define b4 (BCons B0 b2))
(define b5 (BCons B1 b2))
(define b6 (BCons B0 b3))
(define b7 (BCons B1 b3))
(define b8 (BCons B0 b4))

(module+ test
  (define abcd
    (Branch n2
            (Branch n1 (Leaf (dbytes #"A")) (Leaf (dbytes #"B")))
            (Branch n1 (Leaf (dbytes #"C")) (Leaf (dbytes #"D")))))

  (check-equal? (trie-find-opt abcd b0) (Some (dbytes #"A")))
  (check-equal? (trie-find-opt abcd b1) (Some (dbytes #"B")))
  (check-equal? (trie-find-opt abcd b2) (Some (dbytes #"C")))
  (check-equal? (trie-find-opt abcd b3) (Some (dbytes #"D")))
  (check-equal? (trie-find-opt abcd b4) None)

  (define-validators [fc1 am1 j1]
    {~refine t
             (and (= (trie-find-opt t b0) (Some (dbytes #"A")))
                  (= (trie-find-opt t b1) (Some (dbytes #"B")))
                  (= (trie-find-opt t b2) (Some (dbytes #"C")))
                  (= (trie-find-opt t b3) (Some (dbytes #"D")))
                  (= (trie-find-opt t b4) None))})
  (check-pred fc1 abcd)
  (check-false (am1 abcd))

  ;; missing a key
  (define abd
    (Branch n2
            (Branch n1 (Leaf (dbytes #"A")) (Leaf (dbytes #"B")))
            (Branch n1 Empty (Leaf (dbytes #"D")))))
  (check-false (fc1 abd))
  (define cc1/abd (am1 abd))
  (check-pred hash? cc1/abd)
  (check-true (j1 abd cc1/abd))
  (check-false (j1 abcd cc1/abd))

  ;; extra key
  (define abcde
    (Branch
     n3
     (Branch n2
             (Branch n1 (Leaf (dbytes #"A")) (Leaf (dbytes #"B")))
             (Branch n1 Empty (Leaf (dbytes #"D"))))
     (Leaf (dbytes #"E"))))
  (check-false (fc1 abcde))
  (define cc1/abcde (am1 abcde))
  (check-pred hash? cc1/abcde)
  (check-true (j1 abcde cc1/abcde))
  (check-false (j1 abcd cc1/abcde))
  )
