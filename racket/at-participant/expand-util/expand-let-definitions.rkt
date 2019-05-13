#lang racket/base

(provide (for-syntax expand-let-definitions))

(require "let-definitions.rkt"
         (for-syntax racket/base))

;; -----------------------------------------------

(begin-for-syntax
  ;; Expands a series of definitions into a nested
  ;; let where the scope of a definition only
  ;; includes expressions that come after it
  (define (expand-let-definitions es [ctx #f])
    (local-expand #`(let () (let-definitions . #,es))
                  'expression
                  '()
                  ctx)))

;; -----------------------------------------------

(module* test racket/base
  (require (submod "..")
           (for-syntax racket/base
                       rackunit))

  (begin-for-syntax
    (check-equal?
     (syntax->datum
      (expand-let-definitions
       (list
        #'(define x 1)
        #'(define (f y) (+ x y))
        #'(define x (+ x 1))
        #'x)))
     '(let-values ()
        (let-values ([(x) '1])
          (let-values ([(f) (lambda (y) (#%app + x y))])
            (let-values ([(x) (#%app + x '1)])
              x)))))
    ))
