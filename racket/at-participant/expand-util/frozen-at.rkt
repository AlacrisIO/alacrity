#lang racket/base

(provide (for-syntax add-frozen-at-expression
                     add-frozen-at-definition
                     add-frozen-receive-expression
                     add-frozen-at-static-tags
                     project-at-participant))

(require (for-syntax racket/base
                     racket/list
                     syntax/parse
                     "stx-traverse.rkt"))

(begin-for-syntax
  (define frozen-at-expression-prop (gensym 'frozen-at-expression))
  (define frozen-at-definition-prop (gensym 'frozen-at-definition))
  (define frozen-receive-expression-prop (gensym 'frozen-receive-expression))

  ;; Static tags only go on expressions, not definitions
  (define frozen-at-static-tag-prop (gensym 'frozen-at-static-tag))

  ;; ExpressionStx Symbol -> ExpressionStx
  (define (add-frozen-at-expression inner participant
                                    #:static-tag [static-tag #f])
    (define exp-id
      (syntax-property
       (syntax-property
        #'#%expression
        frozen-at-expression-prop participant)
       frozen-at-static-tag-prop static-tag))
    #`(#,exp-id #,inner))

  ;; ExpressionStx Symbol -> ExpressionStx
  ;; This goes on the RHS (right-hand side) of definitions
  (define (add-frozen-at-definition inner participant)
    (define exp-id
      (syntax-property
       #'#%expression
       frozen-at-definition-prop participant))
    #`(#,exp-id #,inner))

  ;; ExpressionStx Symbol -> ExpressionStx
  ;; This goes on calls to the receive function
  (define (add-frozen-receive-expression inner participant static-tag)
    (define exp-id
      (syntax-property
       (syntax-property
        #'#%expression
        frozen-receive-expression-prop participant)
       frozen-at-static-tag-prop static-tag))
    #`(#,exp-id #,inner))

  (define-syntax-class frozen-at-expression
    #:attributes [inner participant static-tag]
    #:literals [#%expression]
    [pattern (exp:#%expression inner)
      #:do [(define v (syntax-property #'exp frozen-at-expression-prop))]
      #:when v
      #:attr participant v
      #:attr static-tag (syntax-property #'exp frozen-at-static-tag-prop)])

  (define-syntax-class frozen-at-definition-rhs
    #:attributes [inner participant]
    #:literals [#%expression]
    [pattern (exp:#%expression inner)
      #:do [(define v (syntax-property #'exp frozen-at-definition-prop))]
      #:when v
      #:attr participant v])

  (define-syntax-class frozen-receive-expression
    #:attributes [inner participant static-tag]
    #:literals [#%expression]
    [pattern (exp:#%expression inner)
      #:do [(define v (syntax-property #'exp frozen-receive-expression-prop))]
      #:when v
      #:attr participant v
      #:attr static-tag (syntax-property #'exp frozen-at-static-tag-prop)])

  ;; Static tags only go on expressions, not definitions
  ;; The i variable is a number such that the integers from
  ;; i and up are unused so far for tags
  (define (add-frozen-at-static-tags stx)
    (define i 0)
    (let loop ([stx stx])
      (syntax-parse stx
        [fa:frozen-at-expression
         (add-frozen-at-expression
          #'fa.inner
          (attribute fa.participant)
          #:static-tag (begin0 i (set! i (add1 i))))]
        [_
         (stx-traverse/recur stx loop)])))

  ;; Projecting to a participant includes:
  ;;  * deleting at-definitions (marked ids within `let-values`) that belong to
  ;;    other participants
  ;;  * transorming at-expressions into either send-and-return or receive calls
  (define (project-at-participant stx participant send-and-return-id receive-id)
    ;; deals with send-and-return
    (define (send-and-return static-tag inner)
      #`(#,send-and-return-id '#,static-tag #,inner))

    ;; deals with receiving from another participant
    (define (receive-from other static-tag)
      (add-frozen-receive-expression
       #`(#,receive-id '#,other '#,static-tag)
       other
       static-tag))
    
    ;; deals with expressions
    (define (proj-expr stx)
      (syntax-parse stx
        #:literals [let-values]
        [fa:frozen-at-expression
         (cond
           [(equal? (attribute fa.participant) participant)
            (send-and-return (attribute fa.static-tag)
                             (attribute fa.inner))]
           [else
            (receive-from (attribute fa.participant)
                          (attribute fa.static-tag))])]
        [(let-values (clause ...) body)
         #`(let-values #,(append-map proj-lv-clause (attribute clause))
             #,(proj-expr #'body))]
        [_
         (stx-traverse/recur stx proj-expr)]))

    ;; deals with definititions as let-values clauses
    (define (proj-lv-clause stx)
      (syntax-parse stx
        [[(x:id ...) rhs:frozen-at-definition-rhs ~!]
         (cond
           [(equal? (attribute rhs.participant) participant)
            (list #`[(x ...) rhs.inner])]
           [else
            '()])]
        [[(x:id ...) e:expr]
         (list #`[(x ...) #,(proj-expr #'e)])]))
    
    (proj-expr stx))

  ;; Converting to handler form includes:
  ;;  * converting to ANF
  ;;    expand-expr->anf
  ;;  * flattening the expression to replace scoped variables with global temps
  ;;    and replace let bindings with set! statements on those temps
  ;;    anf-expr->flatten-scope
  ;;  * breaking up the expression into blocks, where each block can only have
  ;;    `receive` expressions at the very beginning
  ;;  * turning each block into two functions which can be called from the
  ;;    outside, where both function names are completely determined by the
  ;;    participant and the static tag:
  ;;     * one function for the participant to call to send the value
  ;;     * one function for someone else to call when the participant has
  ;;       timed out
  )

;; -----------------------------------------------

(module* test racket/base
  (require rackunit
           syntax/parse/define
           racket/pretty
           (submod "..")
           "../lib.rkt"
           "expand-let-definitions.rkt"
           "and-or.rkt"
           "anf.rkt"
           "flatten-scope.rkt"
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
     #:with expr2 (add-frozen-at-static-tags #'expr1)
     #:with [expr3p ...]
     (for/list ([p (in-list (attribute p))])
       (project-at-participant #'expr2 (syntax-e p) #'send #'receive))
     #:with expr3C (project-at-participant #'expr2 #f #f #'receive)
     #:with expr3C-anf (expand-expr->anf #'expr3C (intdef-ctx-with #'(receive)))
     #:with expr3C-flat:anf-expr->flatten-scope #'expr3C-anf
     #'(begin
         (displayln "consensus expr")
         (pretty-write 'expr3C)
         (displayln "consensus flat")
         (pretty-write '(begin expr3C-flat.stmt ... expr3C-flat.expr))
         (begin
           (displayln 'p)
           (pretty-write 'expr3p))
         ...)])

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

