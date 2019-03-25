#lang racket/base

(provide dbytes dlist dtagged)

(require "pattern.rkt"
         "digest-matcher.rkt"
         (prefix-in mer- "digest-matcher.rkt")
         (for-syntax racket/base
                     syntax/parse))

(define-syntax dbytes
  (normal+base-pattern
   (make-var-like-transformer #'mer-dbytes)
   (syntax-class
    #:attributes [matcher [out 1]]
    [pattern {~var || (pattern-data-constructor #'dbytes/p)}])))

(define-syntax dlist
  (normal+base-pattern
   (make-var-like-transformer #'mer-dlist)
   (syntax-class
    #:attributes [matcher [out 1]]
    [pattern {~var || (pattern-data-constructor #'dlist/p)}])))

(define-syntax dtagged
  (normal+base-pattern
   (make-var-like-transformer #'mer-dtagged)
   (syntax-class
    #:attributes [matcher [out 1]]
    [pattern {~var || (pattern-data-constructor #'dtagged/p)}])))
