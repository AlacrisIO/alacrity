#lang racket/base

(provide (for-syntax lit
                     anf-arg
                     anf-expr
                     expr->anf
                     expand-expr->anf
                     expand-let-defs->anf))

(require "expand-let-definitions.rkt"
         (for-syntax racket/base
                     racket/syntax
                     syntax/parse))

;; -----------------------------------------------

(begin-for-syntax
  (define-syntax-class lit
    [pattern {~or :str :char :boolean :number}])

  (define-syntax-class anf-arg
    #:literals [#%expression quote]
    [pattern (#%expression :anf-arg)]
    [pattern :id]
    [pattern :lit]
    [pattern (quote _)])

  (define-syntax-class anf-expr
    #:literal-sets [kernel-literals]
    [pattern (#%expression :anf-expr)]
    [pattern :anf-arg]
    [pattern (let-values ([(:id ...) :anf-expr] ...) :anf-expr)]
    [pattern (if :anf-arg :anf-expr :anf-expr)]
    [pattern (#%plain-app :anf-arg :anf-arg ...)]))

;; -----------------------------------------------

(begin-for-syntax
  (define-syntax-class expr->anf-arg
    #:attributes [[binding 1] arg]
    ; expr->anf-arg applied with `e` produces `bindings` and `anf`
    ; such that:
    ;  * `(binding ...)` matches `([(:id ...) :anf-expr] ...)`
    ;  * `anf` matches `:anf-arg`
    ;  * `(let-values (binding ...) anf)` is equivalent to `e`
    [pattern arg:anf-arg
      #:with [binding ...] '()]
    [pattern e:expr->anf
      #:with tmp (generate-temporary #'e)
      #:with [binding ...] #'[[(tmp) e.anf]]
      #:with arg #'tmp])

  (define-syntax-class expr->anf
    #:attributes [anf]
    #:literal-sets [kernel-literals]
    [pattern anf:anf-arg]
    [pattern (exp:#%expression a:expr->anf)
      #:with anf #'(exp a.anf)]
    [pattern (let-values ~! ([(x:id ...) a:expr->anf] ...) b:expr->anf)
      #:with anf #'(let-values ([(x ...) a.anf] ...) b.anf)]
    [pattern (if c:expr->anf-arg ~! t:expr->anf e:expr->anf)
      #:with anf #'(let-values (c.binding ...)
                     (if c.arg t.anf e.anf))]
    [pattern (#%plain-app f:anf-arg a:anf-arg ...)
      #:with anf #'(#%plain-app f a ...)]
    [pattern (#%plain-app f:expr->anf-arg ~! a:expr->anf-arg ...)
      #:with anf #'(let-values (f.binding ... a.binding ... ...)
                     (#%plain-app f.arg a.arg ...))]))

;; -----------------------------------------------

(begin-for-syntax
  (define (expand-expr->anf expr [ctx #f])
    (syntax-parse (local-expand expr 'expression '() ctx)
      [e:expr->anf
       #'e.anf]))

  (define (expand-let-defs->anf es [ctx #f])
    (syntax-parse (expand-let-definitions es ctx)
      [e:expr->anf
       #'e.anf])))

;; -----------------------------------------------
