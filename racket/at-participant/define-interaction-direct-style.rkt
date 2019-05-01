#lang agile

(provide (all-from-out "lib.rkt")
         define-interaction @ @signed @may @assert synchronous
         @log)

(require racket/stxparam
         syntax/parse/define
         racket/async-channel
         alexis/multicast
         "lib.rkt"
         "participant.rkt"
         (for-syntax racket/base
                     racket/match
                     racket/syntax
                     syntax/parse))

(define-simple-macro
  (define-interaction consensus:id [participant:id ...] body:expr ...+)
  #:with consensus->participants
  (generate-temporary #'consensus)
  #:with [participant->consensus ...]
  (generate-temporaries #'[participant ...])
  #:with [receiver-from-participant ...]
  (generate-temporaries #'[participant ...])
  (begin
    (define consensus->participants (make-multicast-channel))
    (define participant->consensus (make-channel))
    ...
    (define (consensus)
      (define receivers (hash (~@ 'participant participant->consensus) ...))
      (define (send-and-return v)
        (error 'consensus "no `@`, no sending"))
      (define (receive-from p)
        (define v (channel-get (hash-ref receivers p)))
        (multicast-channel-put consensus->participants v)
        (match v
          [(cons (== p) v) v]))
      (with-participant [consensus send-and-return receive-from]
        body ...))
    (define (participant)
      (define myself->consensus participant->consensus)
      (define receiver-from-consensus
        (make-multicast-receiver consensus->participants))
      (define (send-and-return v)
        (define msg (cons 'participant v))
        (channel-put myself->consensus msg)
        (unless (eq? (async-channel-get receiver-from-consensus) msg)
          (error 'bad))
        v)
      (define (receive-from p)
        (match (async-channel-get receiver-from-consensus)
          [(cons (== p) v) v]))
      (with-participant [participant send-and-return receive-from] body ...))
    ...
    ))

