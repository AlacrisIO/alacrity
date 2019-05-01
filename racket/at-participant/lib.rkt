#lang racket/base

(provide raise-participant-failure
         implies
         digest
         (struct-out signed)
         sign
         signed-valid?
         signed-signers
         empty-table table-has-key? table-empty? table-ref table-set! table-add!
         random-element
         (struct-out salted) random-salt random-salted
         in-repeat
         channel-get/timeout)

(require (only-in racket/base
                  [make-hash empty-table]
                  [hash-has-key? table-has-key?]
                  [hash-empty? table-empty?]
                  [hash-ref table-ref]
                  [hash-set! table-set!])
         racket/bool
         syntax/parse/define)

(struct exn:fail:participant-failure exn:fail [participant])

(define (raise-participant-failure participant msg)
  (raise (exn:fail:participant-failure msg
                                       (current-continuation-marks)
                                       participant)))

(define-simple-macro (implies guard:expr body:expr)
  (if guard body #true))

(define (table-add! t k v)
  (when (table-has-key? t k)
    (error 'table-add! "key already exists"))
  (table-set! t k v))

;; TODO: replace with an actual cryptographic hash function,
;;       ideally something that can be efficient/cheap on the
;;       chosen consensus backend
(define digest equal-hash-code)

(struct signed [signatures value] #:transparent)

(define (sign p v)
  (signed (list p) v))

(define (signed-signers s)
  ;; TODO
  (signed-signatures s))

(define (signed-valid? s)
  (and (signed? s)
       (let ([sigs (signed-signatures s)]
             [dv (digest (signed-value s))])
         (for/and ([sig (in-list sigs)])
           (signature-valid? sig #:digest dv)))))

(define (signature-valid? sig #:digest d)
  ;; TODO
  #true)

(define (random-element list)
  (list-ref list (random (length list))))

(define (random-salt)
  (random))

(struct salted [salt value])

(define (random-salted v)
  (salted (random-salt) v))

(define-simple-macro (in-repeat expr:expr)
  (in-producer (λ () expr)))

(define (channel-get/timeout timeout ch)
  (unless (or (false? timeout)
              (and (real? timeout) (not (negative? timeout))))
    (raise-argument-error 'channel-get/timeout "(or/c #f (and/c real? (not/c negative?)))" 0 timeout ch))
  (unless (channel? ch)
    (raise-argument-error 'channel-get/timeout "channel" 1 timeout ch))
  (sync/timeout timeout ch))

