#lang racket/base

(provide stx-traverse/recur)

(require racket/list
         racket/struct
         "stx-e-restore.rkt")

;; stx-traverse/recur : Stx [Stx -> Stx] -> Stx
;; Note that rec must be able to take both syntax and
;; non-stx-but-containing-stx values, in other words,
;; rec must be able to take something that might come
;; from syntax-e
(define (stx-traverse/recur stx rec)
  (define e (stx-e stx))
  (restore
   stx
   (cond
     [(atomic-literal? e) stx]
     [(cons? e)
      (cons (rec (car e))
            (rec (cdr e)))]
     [(box? e)
      (box-immutable (rec (unbox e)))]
     [(vector? e)
      (apply vector-immutable (map rec (vector->list e)))]
     [(hash? e)
      (for/hash ([(k v) (in-hash e)])
        (values k (rec v)))]
     [(prefab-struct-key e)
      (define key (prefab-struct-key e))
      (apply make-prefab-struct key (map rec (struct->list e)))]
     [else
      (error 'stx-traverse "unrecognized syntax: ~v" stx)])))

;; atomic-literal? : E -> Boolean
(define (atomic-literal? e)
  (or (null? e) (boolean? e) (number? e) (symbol? e) (keyword? e)
      (string? e) (bytes? e)
      (regexp? e) (byte-regexp? e)))

