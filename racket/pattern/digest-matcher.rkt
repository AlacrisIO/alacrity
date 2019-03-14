#lang racket/base

(provide dbytes? dbytes dbytes/p
         dlist? dlist dlist/p)

(require "matcher.rkt")

;; A Digest is an Integer
;; representing the hash of some data

;; An Item is one of:
;;  - Bytes
;;  - [Listof Digest]

;; ambient-dag : [MutableHashTableof Digest Item]
(define ambient-dag
  (make-hash))

;; deref : Digest -> Item
(define (deref d) (hash-ref ambient-dag d))

;; ref : Item -> Digest
(define (ref i)
  (define d (equal-hash-code i))
  (hash-set! ambient-dag d i)
  d)

;; dbytes? : Digest -> Bool
(define (dbytes? d) (bytes? (deref d)))

;; dlist? : Digest -> Bool
(define (dlist? d) (list? (deref d)))

;; dbytes : Bytes -> Digest
(define (dbytes b) (ref b))

;; dlist : Digest ... -> Digest
(define (dlist . ds) (ref ds))

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

