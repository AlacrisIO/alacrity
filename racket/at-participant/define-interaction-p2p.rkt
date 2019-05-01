#lang agile

(provide (all-from-out "lib.rkt")
         define-interaction @ @signed @may @assert synchronous
         @log)

(require racket/stxparam
         syntax/parse/define
         "lib.rkt"
         "participant.rkt"
         "multicast-synchronous-channel.rkt"
         (for-syntax racket/base
                     racket/match
                     racket/syntax
                     syntax/parse))

(define-simple-macro
  (define-interaction consensus:id [participant:id ...] body:expr ...+)
  #:with [participant->others ...]
  (generate-temporaries #'[participant ...])
  #:with [participant->consensus ...]
  (generate-temporaries #'[participant ...])
  #:with [receiver-from-participant ...]
  (generate-temporaries #'[participant ...])
  (begin
    (define participant->others (make-multicast-synchronous-channel))
    ...
    (define participant->consensus (make-channel))
    ...
    (define (consensus)
      (define receivers (hash (~@ 'participant participant->consensus) ...))
      (define (send-and-return v)
        (error 'consensus "no `@`, no sending"))
      (define (receive-from p)
        (match (channel-get (hash-ref receivers p))
          [(cons (== p) v) v]))
      (with-participant [consensus send-and-return receive-from]
        body ...))
    (define (participant)
      (define myself->others participant->others)
      (define receiver-from-participant
        (and (not (eq? participant->others myself->others))
             (make-multicast-synchronous-receiver participant->others)))
      ...
      (define receivers (hash (~@ 'participant receiver-from-participant) ...))
      (define (send-and-return v)
        (multicast-synchronous-channel-put myself->others (cons 'participant v))
        v)
      (define (receive-from p)
        (match (channel-get (hash-ref receivers p))
          [(cons (== p) v) v]))
      (with-participant [participant send-and-return receive-from] body ...))
    ...
    ))

