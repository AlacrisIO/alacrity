#lang racket/base

(require "../pattern.rkt"
         "../digest-pattern.rkt")
(module+ test
  (require rackunit))

;; A [DConsListof X] is one of:
;;  - (dbytes #"")
;;  - (dlist (dbytes #"\001") X [DConsListof X])

(define (len1 xs)
  (match xs with
    [(dbytes #"")                  -> 0]
    [(dlist (dbytes #"\001") _ xs) -> (add1 (len1 xs))]))

(define (len+ xs n)
  (match xs with
    [(dbytes #"")                  -> n]
    [(dlist (dbytes #"\001") _ xs) -> (len+ xs (add1 n))]))

(define (len2 xs) (len+ xs 0))

(define (app1 xs ys)
  (match xs with
    [(dbytes #"")                  -> ys]
    [(dlist (dbytes #"\001") x xs) -> (dlist (dbytes #"\001") x (app1 xs ys))]))

(define (rev1 xs)
  (match xs with
    [(dbytes #"")                  -> (dbytes #"")]
    [(dlist (dbytes #"\001") x xs) ->
     (app1 (rev1 xs) (dlist (dbytes #"\001") x (dbytes #"")))]))

(define (revapp xs ys)
  (match xs with
    [(dbytes #"")                  -> ys]
    [(dlist (dbytes #"\001") x xs) ->
     (revapp xs (dlist (dbytes #"\001") x ys))]))

(define (rev2 xs) (revapp xs (dbytes #"")))

(define (app2 xs ys) (revapp (rev2 xs) ys))

(module+ test
  (define (t x)
    (cond [(string? x) (dbytes (string->bytes/utf-8 x))]
          [(null? x) (dbytes #"")]
          [(pair? x)
           (dlist (dbytes #"\001") (t (car x)) (t (cdr x)))]
          [else (equal-hash-code x)]))
  (check-equal? (len1 (t '())) 0)
  (check-equal? (len1 (t (list "a"))) 1)
  (check-equal? (len1 (t (list 8 13 21))) 3)
  (check-equal? (len2 (t (list 8 13 21))) 3)
  (check-equal? (len1 (t (list "a" "b" "c" "d" "e" "f" "g"))) 7)
  (check-equal? (len2 (t (list "a" "b" "c" "d" "e" "f" "g"))) 7)
  (check-equal? (app1 (t (list "a" "b" "c")) (t (list 1 2 3)))
                (t (list "a" "b" "c" 1 2 3)))
  (check-equal? (app2 (t (list "a" "b" "c")) (t (list 1 2 3)))
                (t (list "a" "b" "c" 1 2 3)))
  (check-equal? (rev1 (t (list "do" "re" "mi")))
                (t (list "mi" "re" "do")))
  (check-equal? (rev2 (t (list "do" "re" "mi")))
                (t (list "mi" "re" "do")))
  (check-equal? (revapp (t (list "do" "re" "mi")) (t (list 1 2 3)))
                (t (list "mi" "re" "do" 1 2 3)))
  )
