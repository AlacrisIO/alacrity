#lang racket/base

(provide define-pattern <-)

(require racket/list
         (only-in racket/match match-lambda)
         syntax/parse/define
         "matcher.rkt"
         "pattern.rkt"
         (for-syntax racket/base
                     racket/list
                     racket/syntax
                     syntax/parse))
(module+ test
  (require rackunit))

(define-syntax <-
  (λ (stx)
    (raise-syntax-error #f "used out of context" stx)))

;; ---------------------------------------------------------

(define ((wrap/p p) x)
  (define r (p x))
  (and r (list r)))

(define ((unwrap/p p) x)
  (define r (p x))
  (and r (append* r)))

;; ---------------------------------------------------------

(begin-for-syntax
  (define-syntax-class (pattern-parameter param-matcher param-var)
    [pattern :id
      #:with matcher param-matcher
      #:with [out ...]
      (list
       (syntax-local-identifier-as-binding
        (syntax-local-introduce param-var)))])
  (define-syntax-class (pattern-body ctor-matcher indexes [ctx #f])
    [pattern (_ p ...)
      #:declare p (full-pattern ctx)
      #:with matcher #`(#,ctor-matcher p.matcher ...)
      #:with [out ...]
      (append*
       (for/list ([i (in-list indexes)])
         (list-ref (attribute p.out) i)))])
  )

(define-syntax-parser define-pattern
  #:literals [<-]
  [(_ name:id
      {~optional {~and pat-only? <-}}
      {~and exp:expr pat:full-pattern})
   #:fail-when (and (pair? (attribute pat.out)) (car (attribute pat.out)))
   "unexpected free variable in pattern"
   #:attr name/v (and (not (attribute pat-only?)) (generate-temporary #'name))
   #:with name/p (generate-temporary #'name)
   #:with make-pattern-transformer
   (if (attribute pat-only?) #'base-pattern-transformer #'normal+base-pattern)
   #'(begin
       (~? (define name/v exp))
       (define name/p pat.matcher)
       (define-syntax name
         (make-pattern-transformer
          (~? (make-var-like-transformer #'name/v))
          (syntax-class
           #:attributes [matcher [out 1]]
           [pattern {~var || (pattern-data-id-constructor #'name/p)}]))))]
  [(_ (name:id param:id ...)
      #:bind
      [{~and attr:id
             {~fail #:unless
                    (member #'attr (attribute param) bound-identifier=?)}}
       ...]
      {~optional {~and pat-only? <-}}
      {~and exp pat})
   #:attr name/v (and (not (attribute pat-only?)) (generate-temporary #'name))
   #:with [param/p ...] (generate-temporaries #'[param ...])
   #:with [param/wp ...] (generate-temporaries #'[param ...])
   #:with [param-x ...] (generate-temporaries #'[param ...])
   #:with [index ...]
   (for/list ([attr (in-list (attribute attr))])
     (index-of (attribute param) attr bound-identifier=?))
   #:with [attr-x ...]
   (for/list ([i (in-list (syntax->datum #'(index ...)))])
     (list-ref (attribute param-x) i))
   #:with make-pattern-transformer
   (if (attribute pat-only?) #'base-pattern-transformer #'normal+base-pattern)
   #'(begin
       (~? (define (name/v param ...) exp))
       (define ((name/p param/p ...) x)
         (let ([param/wp (wrap/p param/p)] ...)
           (let-syntax
               ([param
                 (base-pattern-transformer
                  (syntax-class
                   #:attributes [matcher [out 1]]
                   [pattern
                       {~var || (pattern-parameter #'param/wp #'param-x)}]))]
                ...)
             (match x with
               [pat -> (append attr-x ...)]
               [_ -> #false]))))
       (define-syntax name
         (make-pattern-transformer
          (~? (make-var-like-transformer #'name/v))
          (syntax-class
           #:attributes [matcher [out 1]]
           [pattern {~var || (pattern-body #'name/p '(index ...))}]))))]
  #;[(_ (name:id param:id ...)
      pat)
   #:with name/p (generate-temporary #'name)
   #:with [param/p ...] (generate-temporaries #'[param ...])
   #:with [param/wp ...] (generate-temporaries #'[param ...])
   #:with [param-x ...] (generate-temporaries #'[param ...])
   #:do [(define ctx (syntax-local-make-definition-context))
         (syntax-local-bind-syntaxes (attribute param/wp) #f ctx)
         (syntax-local-bind-syntaxes (attribute param-x) #f ctx)]
   #:with [pat* [param/wp* param-x*] ...]
   (internal-definition-context-introduce ctx #'[pat [param/wp param-x] ...])
   #:do [(syntax-local-bind-syntaxes
           (attribute param)
           #'(values
              (base-pattern-transformer
               (syntax-class
                #:attributes [matcher [out 1]]
                [pattern
                    {~var || (pattern-parameter #'param/wp* #'param-x*)}]))
              ...)
           ctx)]
   #:with {~var pat** (full-pattern ctx)} #'pat*
   #:with [index ...]
   (for/list ([out (in-list (attribute pat**.out))])
     (index-of (attribute param-x*) out free-identifier=?))
   #'(begin
       (define (name/p param/p ...)
         (let ([param/wp* (wrap/p param/p)] ...)
           (match-lambda
             [(app pat**.matcher (list pat**.out ...))
              (append pat**.out ...)])))
       (define-syntax name
         (base-pattern-transformer
          (syntax-class
           #:attributes [matcher [out 1]]
           [pattern {~var || (pattern-body #'name/p '(index ...))}]))))])

(define-simple-macro (pattern-definition-body pat:full-pattern [param-x:id ...])
  #:do [(define param-syms (syntax->datum #'(param-x ...)))]
  #:fail-when (for/or ([out (in-list (attribute pat.out))])
                (and (not (member (syntax-e out) param-syms))
                     out))
  "unexpected free variable in pattern"
  #:with [param-x* ...]
  (for/list ([param-x (in-list (attribute param-x))])
    (define param-sym (syntax-e param-x))
    (define out
      (for/first ([out (in-list (attribute pat.out))]
                  #:when (equal? (syntax-e out) param-sym))
        out))
    (unless out
      (raise-syntax-error 'define-pattern "unused pattern parameter" param-x))
    out)
  (match-lambda
    [(app pat.matcher (list pat.out ...)) (append param-x* ...)]
    [_ #false]))

;; ---------------------------------------------------------

