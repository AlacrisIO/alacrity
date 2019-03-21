#lang racket/base

(provide define-validators)

(require syntax/parse/define
         "../pattern.rkt"
         (only-in "../digest-matcher.rkt" record-dag with-recorded-dag)
         (for-syntax racket/base
                     syntax/parse))
(module+ test
  (require rackunit
           "../define-pattern.rkt"
           "../digest-pattern.rkt"))

;; ---------------------------------------------------------

;; (define-validators [fact-checker-name argument-maker-name judge-name]
;;   story-pattern)
;; Input:
;;  * pattern that can be applied to values of type Story
;; Output defines:
;;  * type Counterclaim
;;  * fact-checker-name : Story -> Bool
;;  * argument-maker-name : Story -> [Maybe Counterclaim]
;;  * judge-name : Story Counterclaim -> Bool
;;    Produces #true when the counterclaim is valid and proves the story wrong,
;;    produces #false when the counterclaim fails to prove the story wrong.

(define-simple-macro
  (define-validators [fact-checker-name:id
                      argument-maker-name:id
                      judge-name:id]
    story-pattern:expr)
  (begin
    ;; Story -> Bool
    (define (fact-checker-name s)
      (match s with
        [story-pattern -> #true]
        [_             -> #false]))
    ;; A Counterclaim is a RecordedDag as returned by `record-dag`
    ;; Story -> [Maybe Counterclaim]
    (define (argument-maker-name s)
      (define-values [dag s-valid?]
        (record-dag (fact-checker-name s)))
      (cond
        [s-valid? #false]
        [else     dag]))
    ;; Story Counterclaim -> Bool
    (define (judge-name s c)
      (with-handlers ([exn:fail? (λ (e) #false)])
        (define s-valid?
          (with-recorded-dag c (fact-checker-name s)))
        (not s-valid?)))))

;; ---------------------------------------------------------

(module+ test
  (define-pattern Empty (dbytes #""))
  (define-pattern (Cons fst rst) #:bind [fst rst]
    (dlist (dbytes #"\001") fst rst))
  (define-pattern (Listof elem) #:bind [] <-
    {~or Empty
         (Cons elem (Listof elem))})

  (define-pattern (Ok a) #:bind [a] (dlist (dbytes #"") a))
  (define-pattern (Err a) #:bind [a] (dlist (dbytes #"\001") a))
  (define-pattern (Result a b) #:bind [] <-
    {~or (Ok a) (Err b)})

  (define-validators [fact-checker1 argument-maker1 judge1]
    (Listof (Ok (dbytes _))))

  (check-equal? (argument-maker1 Empty) #false)
  (check-equal? (argument-maker1 (Cons (Ok (dbytes #"ollo?")) Empty)) #false)
  (define its-hello
    (argument-maker1 (Cons (Err (dbytes #"It's 'hello'.")) Empty)))
  (check-pred hash? its-hello)
  (check hash-has-key? its-hello (Cons (Err (dbytes #"It's 'hello'.")) Empty))
  (check hash-has-key? its-hello (Err (dbytes #"It's 'hello'.")))
  (check-false (hash-has-key? its-hello
                              (dbytes #"It's 'hello'.")))
  (check-true (judge1 (Cons (Err (dbytes #"It's 'hello'.")) Empty) its-hello))
  (check-false (judge1 (Cons (Ok (dbytes #"It's 'hello'.")) Empty) its-hello))
  (check-false (judge1 (Cons (Err (dbytes #"It must be *HELOOLO*.")) Empty)
                       its-hello))
  )
