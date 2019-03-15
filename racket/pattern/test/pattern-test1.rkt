#lang racket/base

(require "../pattern.rkt")
(module+ test
  (require rackunit))

(define (len1 xs)
  (match xs with
    ['()         -> 0]
    [(cons _ xs) -> (add1 (len1 xs))]))

(define (len+ xs n)
  (match xs with
    ['()         -> n]
    [(cons _ xs) -> (len+ xs (add1 n))]))

(define (len2 xs) (len+ xs 0))

(define (app1 xs ys)
  (match xs with
    ['()         -> ys]
    [(cons x xs) -> (cons x (app1 xs ys))]))

(define (rev1 xs)
  (match xs with
    ['()         -> '()]
    [(cons x xs) -> (app1 (rev1 xs) (list x))]))

(define (revapp xs ys)
  (match xs with
    ['()         -> ys]
    [(cons x xs) -> (revapp xs (cons x ys))]))

(define (rev2 xs) (revapp xs '()))

(define (app2 xs ys) (revapp (rev2 xs) ys))

(module+ test
  (check-equal? (len1 (list 8 13 21)) 3)
  (check-equal? (len2 (list 8 13 21)) 3)
  (check-equal? (len1 (list "a" "b" "c" "d" "e" "f" "g")) 7)
  (check-equal? (len2 (list "a" "b" "c" "d" "e" "f" "g")) 7)
  (check-equal? (app1 (list "a" "b" "c") (list 1 2 3)) (list "a" "b" "c" 1 2 3))
  (check-equal? (app2 (list "a" "b" "c") (list 1 2 3)) (list "a" "b" "c" 1 2 3))
  (check-equal? (rev1 (list "do" "re" "mi")) (list "mi" "re" "do"))
  (check-equal? (rev2 (list "do" "re" "mi")) (list "mi" "re" "do"))
  (check-equal? (revapp (list "do" "re" "mi") (list 1 2 3))
                (list "mi" "re" "do" 1 2 3))

  (check-equal? (match (list 1 2 6) with
                  [{~or {~and '() {~with (cons first rest) '(0)}}
                        (cons first rest)} ->
                   (list first rest)])
                (list 1 '(2 6)))
  (check-equal? (match '() with
                  [{~or {~and '() {~with (cons first rest) '(0)}}
                        (cons first rest)} ->
                   (list first rest)])
                (list 0 '()))
  )
