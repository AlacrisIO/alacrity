#lang racket/base

(require "../pattern.rkt"
         "../digest-pattern.rkt"
         "../define-pattern.rkt")
(module+ test
  (require rackunit
           (only-in "../digest-matcher.rkt"
                    record-dag
                    with-recorded-dag)))

;; A [Treeof X] is one of:
;;  - (dbytes #"")
;;  - (dlist (dbytes #"\001") X)
;;  - (dlist (dbytes #"\002") [Treeof X] [Treeof X])
(define-pattern TreeEmpty <- (dbytes #""))
(define-pattern (TreeLeaf x) #:bind [x] <- (dlist (dbytes #"\001") x))
(define-pattern (TreeBranch x y) #:bind [x y] <-
  (dlist (dbytes #"\002") x y))

;; A PathThere is one of:
;;  - (dbytes #"")
;;  - (dlist (dbytes #"\001") StepThere PathThere)
(define-pattern PathThereEmpty <- (dbytes #""))
(define-pattern (PathThereCons step rest) #:bind [step rest] <-
  (dlist (dbytes #"\001") step rest))

;; A StepThere is one of:
;;  - (dbytes #"")
;;  - (dbytes #"\001")
(define-pattern StepLeft <- (dbytes #""))
(define-pattern StepRight <- (dbytes #"\001"))

;; [Treeof X] PathThere -> [Treeof X]
(define (follow-path-there t p)
  (match p with
    [PathThereEmpty            -> t]
    [(PathThereCons step rest) ->
     (follow-path-there (follow-step-there t step) rest)]))

;; [Treeof X] StepThere -> [Treeof X]
(define (follow-step-there t s)
  (match t with
    [(TreeBranch left right) ->
     (match s with
       [StepLeft  -> left]
       [StepRight -> right])]))

;; [Treeof X] PathThere X -> Bool
(define (check-path-to t p x)
  (= (follow-path-there t p) (dlist (dbytes #"\001") x)))

(module+ test
  (define (t x)
    (cond [(bytes? x) (dbytes x)]
          [(string? x) (dbytes (string->bytes/utf-8 x))]
          [(equal? 0 x) (dbytes #"")]
          [(byte? x) (dbytes (bytes x))]
          [(list? x) (apply dlist (map t x))]
          [else (equal-hash-code x)]))
  (define (p x)
    (foldr (λ (x y) (dlist (dbytes #"\001") x y))
           (dbytes #"")
           (map t x)))

  (check-equal? (follow-path-there (t 0) (p '())) (t 0))
  (check-equal? (follow-path-there (t '(1 "ello")) (p '()))
                (t '(1 "ello")))
  (check-equal? (follow-path-there (t '(2 (1 "ello") (1 "there")))
                                   (p '()))
                (t '(2 (1 "ello") (1 "there"))))
  (check-equal? (follow-path-there (t '(2 (1 "ello") (1 "there")))
                                   (p '(0)))
                (t '(1 "ello")))
  (check-equal? (follow-path-there (t '(2 (1 "ello") (1 "there")))
                                   (p '(1)))
                (t '(1 "there")))

  (check-equal? (follow-path-there
                 (t '(2 (2 (1 "a") (1 "b")) (2 (1 "c") (1 "d"))))
                 (p '(0 0)))
                (t '(1 "a")))
  (check-equal? (follow-path-there
                 (t '(2 (2 (1 "a") (1 "b")) (2 (1 "c") (1 "d"))))
                 (p '(0 1)))
                (t '(1 "b")))
  (check-equal? (follow-path-there
                 (t '(2 (2 (1 "a") (1 "b")) (2 (1 "c") (1 "d"))))
                 (p '(1 0)))
                (t '(1 "c")))
  (check-equal? (follow-path-there
                 (t '(2 (2 (1 "a") (1 "b")) (2 (1 "c") (1 "d"))))
                 (p '(1 1)))
                (t '(1 "d")))

  (define-check (check-path-to/record t p x b-exp)
    (define-values [dag b] (record-dag (check-path-to t p x)))
    (check-equal? b b-exp)
    (check-equal? (with-recorded-dag dag (check-path-to t p x))
                  b))

  (check-path-to/record (t '(2 (2 (1 "a") (1 "b")) (2 (1 "c") (1 "d"))))
                        (p '(0 0))
                        (t "a")
                        #t)
  (check-path-to/record (t '(2 (2 (1 "a") (1 "b")) (2 (1 "c") (1 "d"))))
                        (p '(0 1))
                        (t "b")
                        #t)
  (check-path-to/record (t '(2 (2 (1 "a") (1 "b")) (2 (1 "c") (1 "d"))))
                        (p '(1 0))
                        (t "c")
                        #t)
  (check-path-to/record (t '(2 (2 (1 "a") (1 "b")) (2 (1 "c") (1 "d"))))
                        (p '(1 1))
                        (t "d")
                        #t)
  (check-path-to/record (t '(2 (2 (1 "a") (1 "b")) (2 (1 "c") (1 "d"))))
                        (p '(0 0))
                        (t "b")
                        #f)
  )
