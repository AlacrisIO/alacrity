#lang racket/base

(provide and or)

(require syntax/parse/define
         (for-syntax racket/base))

(define-syntax-parser and
  [(_) #'#true]
  [(_ a:expr) #'a]
  [(_ a:expr . rst) #'(if a (and . rst) #false)])

(define-syntax-parser or
  [(_) #'#false]
  [(_ a:expr) #'a]
  [(_ a:expr . rst) #'(let ([tmp a])
                        (if tmp tmp (or . rst)))])

