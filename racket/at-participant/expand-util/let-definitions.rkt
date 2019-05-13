#lang agile

(provide let-definitions)

(require (for-syntax racket/base
                     racket/syntax
                     syntax/parse
                     syntax/context))
(module+ test
  (require rackunit))

;; -----------------------------------------------

;; (let-definitions
;;   (define x a)
;;   ...
;;   b)
;; turns into an expression equivalent to
;; (let* ([x a]
;;        ...)
;;   b)
;; where the scope of a definition only includes
;; expressions that come after it
(define-syntax let-definitions
  (syntax-parser
    [(_)    #'(#%plain-app void)]
    [(_ e)  #'e]
    [(l e . rst)
     #:do [(define e1
             (local-expand
              #'e
              (generate-expand-context)
              (list #'define-values #'define-syntaxes #'begin)))]
     (syntax-track-origin
      (syntax-parse e1
        #:literals [define-values define-syntaxes begin]
        [(begin e2 ...)
         #'(let-definitions e2 ... . rst)]
        [(define-values [x ...] e2)
         #'(let-values ([(x ...) e2])
             (let-definitions . rst))]
        [(define-syntaxes [x ...] e2)
         #'(let-syntaxes ([(x ...) e2])
             (let-definitions . rst))]
        [e2
         #:with tmp (syntax-property (generate-temporary) 'unused #t)
         #'(let-values ([(tmp) e2])
             (let-definitions . rst))])
      e1
      #'l)]))

;; -----------------------------------------------

(module+ test
  (check-equal? (let-definitions
                 (define x 1)
                 (define (f y) (+ x y))
                 (define x (+ x 1))
                 (f x))
                3))
