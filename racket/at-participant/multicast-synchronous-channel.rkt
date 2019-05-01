#lang racket/base

(provide make-multicast-synchronous-channel
         make-multicast-synchronous-receiver
         multicast-synchronous-channel-put)

(require racket/set)

(struct multicast-synchronous-channel [channels])

;; -> MSC
(define (make-multicast-synchronous-channel)
  (multicast-synchronous-channel (mutable-set)))

;; MSC -> Channel
(define (make-multicast-synchronous-receiver msc)
  (define ch (make-channel))
  (set-add! (multicast-synchronous-channel-channels msc) ch)
  ch)

;; MSC Any -> Void
(define (multicast-synchronous-channel-put msc v)
  (define chs (multicast-synchronous-channel-channels msc))
  (for ([ch (in-set chs)])
    (channel-put ch v)))
