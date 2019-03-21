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
(define (nat->rkt n)
  (match n with
    [Z -> 0]
    [(S n-1) -> (add1 (nat->rkt n-1))]))

(define (pred n)
  (match n with
    [(S n-1) -> n-1]
    [Z -> (error 'pred)]))
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
(define (rkt->bit i) (match i with [0 -> B0] [1 -> B1]))
(define (bit->rkt b) (match b with [B0 -> 0] [B1 -> 1]))
(define (rkt->bnum i)
  (cond [(zero? i) BEmpty]
        [else
         (define-values [q r] (quotient/remainder i 2))
         (BCons (rkt->bit r) (rkt->bnum q))]))
(define (bnum->rkt bn)
  (match bn with
    [BEmpty -> 0]
    [(BCons b bn) -> (+ (bit->rkt b) (* 2 (bnum->rkt bn)))]))

(define (bnum-add1 bn)
  (match bn with
    [BEmpty         -> (BCons B1 BEmpty)]
    [(BCons B0 rst) -> (BCons B1 rst)]
    [(BCons B1 rst) -> (BCons B0 (bnum-add1 rst))]))

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

(define-pattern (Neg n) #:bind [n] (dlist n))
(define-pattern Int <- {~or Nat (Neg Nat)})

(define (int+ a b)
  (match* [a b] with
    [[{~or Z (Neg Z)} b] -> b]
    [[a {~or Z (Neg Z)}] -> a]
    [[(Neg a) (Neg b)] -> (Neg (int+ a b))]
    [[(S a) (Neg (S b))] -> (int+ a (Neg b))]
    [[(Neg (S a)) (S b)] -> (int+ (Neg a) b)]
    [[(S a) b] -> (S (int+ a b))]))

;; ---------------------------------------------------------

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

(define (trie-key-range trie)
  (match (trie-height trie) with
    [None -> '()]
    [(Some h) ->
     (for/list ([i (in-range (expt 2 (nat->rkt h)))])
       (rkt->bnum i))]))

(define n0 Z)
(define n1 (S n0))
(define n2 (S n1))
(define n3 (S n2))
(define n4 (S n3))
(define n5 (S n4))
(define n6 (S n5))
(define n7 (S n6))
(define n8 (S n7))
(define n9 (S n8))
(define n10 (S n9))
(define b0 BEmpty)
(define b1 (BCons B1 b0))
(define b2 (BCons B0 b1))
(define b3 (BCons B1 b1))
(define b4 (BCons B0 b2))
(define b5 (BCons B1 b2))
(define b6 (BCons B0 b3))
(define b7 (BCons B1 b3))
(define b8 (BCons B0 b4))
(define b9 (BCons B1 b4))
(define b10 (BCons B0 b5))

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

  (define-pattern (Entry balance adjustment) #:bind [balance adjustment]
    (dlist balance adjustment))

  (define-validators [fc2 am2 j2]
    {~and
     (Trie (Entry Int Int))
     {~refine t
              (for/and ([k (in-list (trie-key-range t))])
                (match (trie-find-opt t k) with
                  [(Some (Entry balance adjustment)) ->
                   (match (trie-find-opt t (bnum-add1 k)) with
                     [(Some (Entry after _)) ->
                      (= after (int+ balance adjustment))]
                     [None -> #true])]
                  [None -> #true]))}})

  (define valid2
    (Branch n2
            (Branch n1
                    (Leaf (Entry n0 n1))
                    (Leaf (Entry n1 n5)))
            (Branch n1
                    (Leaf (Entry n6 (Neg n4)))
                    (Leaf (Entry n2 n1)))))
  (define invalid2
    (Branch n2
            (Branch n1
                    (Leaf (Entry n0 n1))
                    (Leaf (Entry n1 n5)))
            (Branch n1
                    (Leaf (Entry n6 (Neg n4)))
                    (Leaf (Entry n10 n1)))))
  (check-true (fc2 valid2))
  (check-false (fc2 invalid2))
  (check-false (am2 valid2))
  (define cc2/invalid (am2 invalid2))
  (check-pred hash? cc2/invalid)
  (check-true (j2 invalid2 cc2/invalid))
  (check-false (j2 valid2 cc2/invalid))
  )
