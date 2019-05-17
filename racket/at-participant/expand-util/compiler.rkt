#lang agile

(require rackunit
         syntax/parse/define
         racket/pretty
         "frozen-at.rkt"
         "../lib.rkt"
         "expand-let-definitions.rkt"
         "and-or.rkt"
         "anf.rkt"
         "flatten-scope.rkt"
         "handler-form.rkt"
         (for-syntax racket/base
                     syntax/stx))

(begin-for-syntax
  ;; [StxListof Id] -> InternalDefinitionContext
  (define (intdef-ctx-with ids-stx)
    (define ids (stx->list ids-stx))
    (define ctx (syntax-local-make-definition-context))
    (syntax-local-bind-syntaxes ids #f ctx)
    ctx))

(define-syntax-parser @
  #:literals [define]
  [(_ participant:id (define ~! x:id expression:expr))
   #`(define x
       #,(add-frozen-at-definition #'expression (syntax-e #'participant)))]
  [(_ participant:id expression:expr)
   (add-frozen-at-expression #'expression (syntax-e #'participant))])

(define-syntax-parser program
  [(_ [p:id ...] e:expr ...)
   #:with expr1 (expand-let-definitions (attribute e))
   #:with [n expr2] (add-frozen-at-static-tags #'expr1)
   #:with [expr3p ...]
   (for/list ([p (in-list (attribute p))])
     (project-at-participant #'expr2 (syntax-e p) #'send #'receive))
   #:with expr3C (project-at-participant #'expr2 #f #f #'receive)
   #:with expr3C-anf (expand-expr->anf #'expr3C (intdef-ctx-with #'(receive)))
   #:with expr3C-flat:anf-expr->flatten-scope #'expr3C-anf
   #:with expr3C-flat-stmt-expr:semiflat->handler-form
   #'(begin expr3C-flat.stmt ... expr3C-flat.expr)
   ;---
   #'(begin
       (displayln "consensus expr")
       (pretty-write 'expr3C)
       (displayln "consensus flat")
       (pretty-write 'expr3C-flat-stmt-expr)
       (displayln "consensus handlers")
       (pretty-write '(program
                       expr3C-flat-stmt-expr.initial
                       (handlers expr3C-flat-stmt-expr.handler ...)
                       (functions expr3C-flat-stmt-expr.function ...)))
       (begin
         (displayln 'p)
         (pretty-write 'expr3p))
       ...)])

(module+ test
  (program [A B]
   (@ A (define sh1 (list (random) (random 3))))
   (@ B (define sh2 (list (random) (random 3))))
   (define A-d (@ A (digest sh1)))
   (define B-d (@ B (digest sh2)))
   (define A-sh (@ A sh1))
   (define B-sh (@ B sh2))
   (and (equal? (digest A-sh) A-d)
        (equal? (digest B-sh) B-d)))
  )
