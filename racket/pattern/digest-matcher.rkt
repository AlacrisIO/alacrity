#lang racket/base

(provide dbytes? dbytes dbytes/p
         dlist? dlist dlist/p
         dtagged? dtagged dtagged/p
         record-dag
         with-recorded-dag)
(module+ private
  (provide ref deref))

(require racket/match
         racket/set
         syntax/parse/define
         "matcher.rkt"
         "util/product-sum.rkt")

;; A Digest is an Integer
;; representing the hash of some data

;; An Item is one of:
;;  - Bytes
;;  - [Listof Digest]

;; returns two values:
;;  - the dag
;;  - the result of the body
(define-simple-macro (record-dag body:expr ...+)
  (call/record-dag (λ () body ...)))

(define-simple-macro (with-recorded-dag dag body:expr ...+)
  (call-with-recorded-dag dag (λ () body ...)))

;; ambient-dag : [Parameterof [HashTableof Digest Item]]
(define ambient-dag
  (make-parameter (make-hash)))

;; relevant-dag : [Parameterof [Maybe [MutableSetof Digest]]]
;; used by call/record-dag
(define relevant-keys
  (make-parameter #f))
(define (relevant-key! k)
  (define rks (relevant-keys))
  (when rks (set-add! rks k)))

;; call/record-dag :
;;   [-> X] -> (values [ImmutableHashTableof Digest Item] X)
(define (call/record-dag f)
  (define dag (ambient-dag))
  (define keys (mutable-set))
  (parameterize ([ambient-dag dag]
                 [relevant-keys keys])
    (define x (f))
    (values (for/hash ([k (in-set keys)])
              (values k (hash-ref dag k)))
            x)))

;; call-with-recorded-dag :
;;   [ImmutableHashTableof Digest Item]
;;   [-> X]
;;   ->
;;   X
(define (call-with-recorded-dag dag f)
  (parameterize ([ambient-dag dag])
    (f)))

;; deref : Digest -> Item
(define (deref d)
  (relevant-key! d)
  (hash-ref (ambient-dag) d))

;; ref : Item -> Digest
(define (ref i)
  (define d (equal-hash-code i))
  (define dag (ambient-dag))
  (when (not (immutable? dag)) (hash-set! dag d i))
  d)

;; dbytes? : Digest -> Bool
(define (dbytes? d) (bytes? (deref d)))

;; dlist? : Digest -> Bool
(define (dlist? d) (list? (deref d)))

;; dtagged? : Digest -> Bool
(define (dtagged? d) (sum? (deref d)))

;; dbytes : Bytes -> Digest
(define (dbytes b) (ref b))

;; dlist : Digest ... -> Digest
(define (dlist . ds) (ref ds))

;; dtagged : Natural Digest -> Digest
(define (dtagged tag d) (ref (tagged tag d)))

;; dbytes/p : [Matcher Bytes Bytes (Z ...)] -> [Matcher Digest Digest (Z ...)]
(define (dbytes/p p)
  (compose (and/p (pred/p bytes?) p) deref))

;; dlist/p :
;;  [Matcher Digest Digest (Z ...)]
;;  ...
;;  ->
;;  [Matcher Digest Digest (Z ... ...)]
(define (dlist/p . ps)
  (compose (apply list/p ps) deref))

;; dtagged/p
;;   [Matcher Natural Natural (Z1 ...)]
;;   [Matcher Digest Digest (Z2 ...)]
;;   ->
;;   [Matcher Digest Digest (Z1 ... Z2 ...)]
(define ((dtagged/p p-tag p-elem) x)
  (match (deref x)
    [(tagged (app p-tag (and (not #f) vs1))
             (app p-elem (and (not #f) vs2)))
     (append vs1 vs2)]
    [_ #f]))

