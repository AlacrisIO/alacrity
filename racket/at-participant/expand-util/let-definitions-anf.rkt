#lang agile

(provide let-definitions->quote-anf)

(require "anf.rkt")
(module+ test
  (require rackunit))

(define-simple-macro (let-definitions->quote-anf e ...+)
  #:with anf (expand-let-defs->anf (attribute e))
  'anf)

(module+ test
  (check-equal?
   (let-definitions->quote-anf (define x 3) (define y 4) x)
   '(let-values (((x) '3)) (let-values (((y) '4)) x))))
