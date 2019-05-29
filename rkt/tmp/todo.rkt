#lang racket/base

(provide TODO)

(require (for-syntax racket/base
                     syntax/parse
                     todo-list))

(define-syntax TODO
  (lambda (stx)
    (syntax-parse stx
      [{~or (_ msg:str)
            {~and {~or :id (_)} {~parse msg (format "~a" (syntax-line stx))}}}
       ;; Expand a TODO to a runtime error
       (define runtime
         (syntax/loc stx
           (error 'msg)))
       ;; Attach a notice that it is a TODO to be filled out
       (syntax-property runtime 'todo (syntax->datum #'msg))])))

