#lang racket/base

(provide matcher:
         ;---
         any/p
         var/p
         pred/p
         equal/p
         and/p
         refine/p
         ;---
         empty/p
         cons/p
         list*/p
         list/p)

(require racket/list
         racket/match
         (only-in srfi/1 append-reverse)
         syntax/parse/define
         (for-syntax racket/base
                     syntax/parse))
(module+ test
  (require rackunit))

;; -----------------------------------------------

;; A [Matcher X Y (Z ...)] is a function:
;;   X -> [Maybe [List Z ...]]
;;   such that if it passes and the list is there,
;;   the input X value is actually also of type Y.
;;   X is contravariant,
;;   Y is covariant,
;;   each Z is covariant.

(define-match-expander matcher:
  (syntax-parser
    [(_ mer:expr [id:id ...]) #'(app mer (list id ...))]))

;; -----------------------------------------------

;; Generic patterns

;; any/p : [Matcher X X ()]
(define (any/p x) '())

;; var/p : [Matcher X X (X)]
(define (var/p x) (list x))

;; pred/p : [X -> Bool : Y] -> [Matcher X Y ()]
(define ((pred/p y?) x)
  (and (y? x) '()))

;; equal/p : X -> [Matcher Any X ()]
(define ((equal/p x) y)
  (and (equal? x y) '()))

;; and/p : [Matcher X Y (Z ...)] ... -> [Matcher X Y (Z ... ...)]
(define ((and/p . ps) x)
  (let loop ([ps ps] [acc '()])
    (match ps
      ['() (reverse acc)]
      [(cons p ps)
       (define r (p x))
       (and r
            (loop ps (append-reverse r acc)))])))

;; refine/p :
;;   [Matcher X Y (Z ...)]
;;   [[List Z ...] -> Bool]
;;   ->
;;   [Matcher X Y (Z ...)]
(define ((refine/p pat prop) x)
  (define r (pat x))
  (and r (prop r) r))

;; -----------------------------------------------

;; Patterns for data structures, lists and strings

;; empty/p : [Matcher Any Empty ()]
(define (empty/p x)
  (and (empty? x) '()))

;; cons/p :
;;   [Matcher Any X1 (Y1 ...)]
;;   [Matcher Any X2 (Y2 ...)]
;;   ->
;;   [Matcher Any [Cons X1 X2] (Y1 ... Y2 ...)]
(define ((cons/p p1 p2) x)
  (and
   (pair? x)
   (let ([r1 (p1 (car x))])
     (and
      r1
      (let ([r2 (p2 (cdr x))])
        (and
         r2
         (append r1 r2)))))))

;; list*/p :
;;   [Matcher Any X1 (Y1 ...)] ...
;;   [Matcher Any X2 (Y2 ...)]
;;   ->
;;   [Matcher Any [List* X1 ... X2] (Y1 ... ... Y2 ...)]
(define (list*/p p1 . p2s)
  (define-values [p1s lst-p2] (split-at-right (cons p1 p2s) 1))
  (define p2 (first lst-p2))
  (foldr cons/p p2 p1s))

;; list/p :
;;   [Matcher Any X (Y ...)] ...
;;   ->
;;   [Matcher Any [List X ...] (Y ... ...)]
(define ((list/p . ps) xs)
  (and
   (list? xs)
   (let loop ([ps ps] [xs xs] [acc '()])
     (match* [ps xs]
       [['() '()] (reverse acc)]
       [[(cons p ps) (cons x xs)]
        (define r (p x))
        (and r (loop ps xs (append-reverse r acc)))]
       [[_ _] #false]))))

;; -----------------------------------------------

(module+ test
  (check-equal? (empty/p '()) '())
  (check-equal? (empty/p 123) #false)
  (check-equal? ((cons/p any/p any/p) (cons 1 2)) '())
  (check-equal? ((cons/p any/p any/p) "the words") #false)
  (check-equal? ((cons/p var/p var/p) (cons 1 2)) (list 1 2))
  (check-equal? ((cons/p var/p (cons/p var/p var/p))
                 (list "a" "b" "c" 1 2 3))
                (list "a" "b" (list "c" 1 2 3)))
  (check-equal? ((list*/p var/p var/p var/p)
                 (list 1 2 3 "do" "re" "mi"))
                (list 1 2 (list 3 "do" "re" "mi")))
  
  (check-equal? ((list/p (equal/p 1) var/p (equal/p 3))
                 (list 1 'boo! 3))
                (list 'boo!))
  (check-equal? ((list/p (equal/p 1) var/p (equal/p 3))
                 (list 1 2 5))
                #false)
  )
