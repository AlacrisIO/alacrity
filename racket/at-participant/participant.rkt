#lang racket/base

(provide with-participant
         @
         @may
         @signed
         @assert
         @log
         synchronous
         (for-syntax get-current-participant
                     participant-id))

(require racket/stxparam
         syntax/parse/define
         "lib.rkt"
         (for-syntax racket/base
                     racket/match
                     racket/syntax
                     syntax/parse))

(begin-for-syntax
  (provide (struct-out participant))

  (struct participant [name send-and-return-id receive-from-id]))

;; current-participant : [SyntaxParameterof [Maybe Symbol]]
(define-syntax-parameter current-participant #f)

(define-simple-macro
  (with-participant [name:id send-and-return-id receive-from-id] body:expr ...+)
  (syntax-parameterize
      ([current-participant
        (participant 'name #'send-and-return-id #'receive-from-id)])
    body ...))

(begin-for-syntax
  (define (get-current-participant)
    (syntax-parameter-value #'current-participant))

  (define-syntax-class participant-id
    #:attributes [me? cur-name cur-send-and-return cur-receive-from]
    [pattern p-id:id
      #:do [(define p-sym (syntax-e #'p-id))
            (define cur (get-current-participant))
            (match-define
              (participant cur-sym cur-send-and-return-id cur-receive-from-id)
              cur)]
      #:attr me? (equal? p-sym cur-sym)
      #:attr cur-name cur-sym
      #:attr cur-send-and-return cur-send-and-return-id
      #:attr cur-receive-from cur-receive-from-id]))

(define-syntax-parser @
  #:literals [define]
  [(_ p:participant-id (def:define ~! x:id expr:expr))
   (cond
     [(attribute p.me?) #'(def x expr)]
     [else              #'(begin)])]
  [(_ p:participant-id expr:expr)
   (cond
     [(attribute p.me?) #'(p.cur-send-and-return expr)]
     ; TODO: if don't receive within timeout,
     ;       send message on blockchain forcing them to send
     [else              #'(p.cur-receive-from 'p)])])

(define-syntax-parser @signed
  #:literals [define]
  [(_ p:participant-id expr:expr)
   (cond
     [(attribute p.me?) #'(p.cur-send-and-return (sign 'p expr))]
     ; TODO: if don't receive within timeout,
     ;       send message on blockchain forcing them to send
     [else              #'(p.cur-receive-from 'p)])])

(define-syntax-parser @may
  #:literals [define]
  [(_ p:participant-id expr:expr)
   (cond
     ; TODO: how is `@may` different from `@` / `@must`?
     [(attribute p.me?) #'(p.cur-send-and-return expr)]
     [else              #'(p.cur-receive-from 'p)])])

(define-simple-macro
  (@assert p:participant-id expr:expr msg-fmt:expr arg:expr ...)
  (unless expr
    (raise-participant-failure
     'p
     (format (string-append "~a: " msg-fmt) 'p arg ...))))

(define-simple-macro
  (@log expr:expr)
  #:do [(define cur (get-current-participant))]
  #:with cur-sym (participant-name cur)
  (printf "~a: ~a\n" 'cur-sym expr))

;; ---------------------------------------------------------

(begin-for-syntax
  (define-syntax-class sync-clause
    #:literals [define @]
    [pattern (define x:id (@ p:participant-id e:expr))
      #:with tmp (generate-temporary #'x)
      #:with digest-id (generate-temporary #'x)
      #:with [phase1 ...] #'[(@ p (define tmp e))
                             (define digest-id (@ p (digest tmp)))]
      #:with [phase2 ...] #'[(define x (@ p tmp))
                             (@assert p (equal? (digest x) digest-id)
                                      (string-append
                                       "value for ~a doesn't match digest\n"
                                       "  expected-digest: ~v\n"
                                       "  actual-digest:   ~v\n"
                                       "  actual-value:  ~v")
                                      'x
                                      digest-id
                                      (digest x)
                                      x)]]))

(define-simple-macro (synchronous clause:sync-clause ...)
  (begin
    clause.phase1 ... ...
    clause.phase2 ... ...))

