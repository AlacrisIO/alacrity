#lang racket/base

(provide (for-syntax semiflat-stmt
                     semiflat-expr
                     anf-expr->flatten-scope))

(require "anf.rkt"
         (for-syntax racket/base
                     syntax/id-table
                     syntax/parse
                     "stx-traverse.rkt"))
(module+ test
  (require rackunit
           (for-syntax rackunit)))

(begin-for-syntax
  (define-syntax-class semiflat-stmt
    #:literals [set!]
    [pattern (set! :id :semiflat-expr)]
    [pattern :semiflat-expr])

  (define-syntax-class semiflat-expr
    #:literals [#%expression #%plain-app if begin]
    [pattern :anf-arg]
    [pattern (#%expression :semiflat-expr)]
    [pattern (#%plain-app :anf-arg ...)]
    [pattern (if :anf-arg :semiflat-expr :semiflat-expr)]
    [pattern (begin :semiflat-stmt ... :semiflat-expr)])

  ;; anf-expr->flatten-scope
  ;; Consumes an expression in ANF form, and flattens it to replace
  ;; scoped variables with global temps and replace let bindings with
  ;; set! statements on those temps

  (define-syntax-class
    (anf-arg->flatten-scope [tbl (make-immutable-free-id-table)])
    #:attributes [expr]
    #:literals [#%expression]
    [pattern a:anf-arg
      #:with expr (let loop ([a #'a])
                    (cond [(identifier? a)
                           (free-id-table-ref tbl a a)]
                          [else
                           (stx-traverse/recur a loop)]))])

  (define-syntax-class
    (anf-expr->flatten-scope [tbl (make-immutable-free-id-table)])
    #:attributes [[temp 1] [stmt 1] expr]
    #:literal-sets [kernel-literals]
    [pattern {~var || (anf-arg->flatten-scope tbl)}
      #:with [temp ...] '()
      #:with [stmt ...] '()]
    [pattern (exp:#%expression ~! {~var e (anf-expr->flatten-scope tbl)})
      #:with [temp ...] (attribute e.temp)
      #:with [stmt ...] (attribute e.stmt)
      #:with expr #'(exp e.expr)]
    [pattern (#%plain-app ~! {~var f (anf-arg->flatten-scope tbl)}
                          {~var a (anf-arg->flatten-scope tbl)}
                          ...)
      #:with [temp ...] '()
      #:with [stmt ...] '()
      #:with expr #'(#%plain-app f.expr a.expr ...)]

    [pattern (let-values ~!
                 ([(x:id) {~var a (anf-expr->flatten-scope tbl)}] ...)
               b)
      #:do [(define xs (syntax->list #'[x ...]))
            (define x-temps (generate-temporaries xs))
            (define b-tbl
              (for/fold ([tbl tbl]) ([x (in-list xs)] [temp (in-list x-temps)])
                (free-id-table-set tbl x temp)))]
      #:with {~var b* (anf-expr->flatten-scope b-tbl)} #'b
      #:with [x-temp ...] x-temps
      #:with [temp ...] #'[a.temp ... ... x-temp ... b*.temp ...]
      #:with [stmt ...] #'[{~@ a.stmt
                               ...
                               (set! x-temp a.expr)}
                           ...
                           b*.stmt
                           ...]
      #:with expr #'b*.expr]

    [pattern (if ~! {~var c (anf-arg->flatten-scope tbl)}
                 {~var t (anf-expr->flatten-scope tbl)}
                 {~var e (anf-expr->flatten-scope tbl)})
      #:with [temp ...] #'[t.temp ... e.temp ...]
      #:with [stmt ...] '()
      #:with expr #'(if c.expr
                        (begin t.stmt ... t.expr)
                        (begin e.stmt ... e.expr))]))

;; ---------------------------------------------------------

(module+ test
  (begin-for-syntax
    (check-true (syntax-parse #'(let-values ([(x) '1] [(y) '2] [(z) '3])
                                  (let-values ([(a) (#%plain-app f x y z)])
                                    (if a
                                        (#%plain-app g a x y)
                                        (#%plain-app h y z))))
                  [e:anf-expr->flatten-scope
                   #:with se:semiflat-expr #'(begin e.stmt ... e.expr)
                   #true]))
    ))