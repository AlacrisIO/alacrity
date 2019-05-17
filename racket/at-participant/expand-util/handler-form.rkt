#lang racket/base

(provide (for-syntax semiflat->handler-form))

(require "frozen-at.rkt"
         "anf.rkt"
         "flatten-scope.rkt"
         (for-syntax racket/base
                     racket/syntax
                     syntax/parse))

(begin-for-syntax
  ;; Matches a syntax object that does not contain any frozen-at-expression
  ;; or frozen-receive-expression sub-expressions
  (define-syntax-class pure
    #:literal-sets [kernel-literals]
    [pattern e:frozen-at-expression
      #:cut
      #:fail-when #'e.inner
      (format "at tag ~a for ~a makes this not pure"
              (attribute e.static-tag)
              (attribute e.participant))]
    [pattern e:frozen-receive-expression
      #:cut
      #:fail-when #'e.inner
      (format "receive ~a for ~a makes this not pure"
              (attribute e.static-tag)
              (attribute e.participant))]
    [pattern :anf-arg]
    [pattern (begin :pure ...)]
    [pattern (#%expression :pure)]
    [pattern (#%plain-app :pure :pure ...)]
    [pattern (if :pure :pure :pure)]
    [pattern (set! :id :pure)])

  ;; Matches a syntax object that passes semiflat-stmt, and produces:
  ;;  * initial:semiflat-stmt
  ;;  * ([handler.i:nat handler.param:id handler.body:semiflat-expr] ...)
  ;;  * ([function.name:id function.param:id function.body:semiflat-expr] ...)
  (define-syntax-class (semiflat->handler-form [k #f])
    #:attributes
    [initial
     [handler 1] [handler.i 1] [handler.param 1] [handler.body 1]
     [function 1] [function.name 1] [function.param 1] [function.body 1]]
    #:literals [#%expression set! if #%plain-app begin]
    [pattern e:pure
      #:with initial (cond [k #`(#%plain-app #,k e)]
                           [else #'e])
      #:with (handler:handler-entry ...) '()
      #:with (function:function-entry ...) '()]
    [pattern (begin)
      #:with initial #'(begin)
      #:with (handler:handler-entry ...) '()
      #:with (function:function-entry ...) '()]
    [pattern (begin {~var || (semiflat->handler-form k)})]
    [pattern (begin a:pure ... {~var b (semiflat->handler-form k)})
      #:with initial #'(begin a ... b.initial)
      #:with (handler:handler-entry ...) #'(b.handler ...)
      #:with (function:function-entry ...) #'(b.function ...)]
    [pattern (begin a:pure ...
                    {~and
                     {~not :pure}
                     {~or (set! ~! x:id b)
                          {~and b {~parse x (generate-temporary 'temp)}}}}
                    c ...+)
      #:with k-rst-name (generate-temporary 'k)
      #:with k-rst-param (generate-temporary #'x)
      #:with {~var k-rst-expr (semiflat->handler-form k)} #'(begin c ...)
      #:with {~var b2 (semiflat->handler-form #'k-rst-name)} #'b
      #:with initial #'(begin a ... b2.initial)
      #:with (handler:handler-entry ...)
      #'(k-rst-expr.handler ... b2.handler ...)
      #:with (function:function-entry ...)
      #'([k-rst-name k-rst-param (begin (set! x k-rst-param) k-rst-expr.initial)]
         k-rst-expr.function ...
         b2.function ...)]
    [pattern (if c:anf-arg t e)
      #:declare t (semiflat->handler-form k)
      #:declare e (semiflat->handler-form k)
      #:with initial #'(if c t.initial e.initial)
      #:with (handler:handler-entry ...) #'(t.handler ... e.handler ...)
      #:with (function:function-entry ...) #'(t.function ... e.function ...)]
    [pattern e:frozen-receive-expression
      ;; TODO: generate handlers for timeout cases too
      #:with i (attribute e.static-tag)
      #:with param (generate-temporary)
      #:with check-s
      #`(#%plain-app assert-equal state 'i)
      #:with check-p
      #`(#%plain-app assert-equal msg.sender '#,(attribute e.participant))
      #:with initial #'(begin)
      #:with (handler:handler-entry ...)
      (cond [k #`([i
                   param
                   (begin check-s check-p (#%plain-app #,k param))])]
            [else #`([i
                      param
                      (begin check-s check-p)])])
      #:with (function:function-entry ...) '()])

  (define-syntax-class function-entry
    [pattern (name:id param:id body:expr)])

  (define-syntax-class handler-entry
    [pattern (i:nat param:id body:expr)])
  )
