#lang racket/base
(require racket/tcp
         racket/match
         racket/file
         racket/contract
         racket/list)
(module+ test
  (require chk))

(define-syntax-rule (forever . b)
  (let loop () (begin (begin . b) (loop))))

(define (make-tcp-server port-no ready-sema make-handle-client)
  (define l (tcp-listen port-no 16 #t #f))
  (semaphore-post ready-sema)
  (forever
   (define-values (from to) (tcp-accept l))
   (thread (make-handle-client from to))))

(struct bob (token data) #:transparent)
(define mt #"")
(define big-endian? #t)
(define payload-len-width 2)
(define (make-bob-reader from)
  (define buffer (make-bytes (add1 payload-len-width) 0))
  (λ ()
    (read-bytes! buffer from)
    (define token (bytes-ref buffer 0))
    (define payload-len (integer-bytes->integer buffer #f big-endian? 1))
    (bob token (read-bytes payload-len from))))
(define (make-bob-writer to)
  (define buffer (make-bytes payload-len-width 0))
  (λ (b)
    (match-define (bob token data) b)
    (write-byte token to)
    (integer->integer-bytes (bytes-length data) payload-len-width #f big-endian? buffer)
    (write-bytes buffer to)
    (write-bytes data to)
    (flush-output to)))

(define (((make-make-bob-clienth request-ch) from to))
  (define bob-read (make-bob-reader from))
  (define bob-write (make-bob-writer to))
  (forever
   #;(eprintf "[BC] Reading req\n")
   (define req (bob-read))
   #;(eprintf "[BC] Sending req to handler\n")
   (channel-put request-ch (cons req (current-thread)))
   #;(eprintf "[BC] Receiving reply\n")
   (define reply (thread-receive))
   #;(eprintf "[BC] Writing reply\n")
   (bob-write reply)))
(define (make-bob-server-t port-no ready-sema initial-st handle-req done-f)
  (define req-ch (make-channel))
  (define serve-t (thread (λ () (make-tcp-server port-no ready-sema (make-make-bob-clienth req-ch)))))
  (define done-sema (make-semaphore))
  (define bob-t
    (thread (λ ()
              (let loop ([st initial-st])
                #;(eprintf "[BS] Waiting for req or done\n")
                (sync
                 (handle-evt done-sema
                             (λ _
                               (kill-thread serve-t)
                               #;(eprintf "[BS] Finishing\n")
                               (done-f st)))
                 (handle-evt req-ch
                             (match-lambda
                               [(cons req rt)
                                #;(eprintf "[BS] Handling req\n")
                                (define-values (reply new-st) (handle-req st req))
                                #;(eprintf "[BS] Returning reply to client\n")
                                (thread-send rt reply)
                                (loop new-st)])))))))
  (λ ()
    (semaphore-post done-sema)
    (thread-wait bob-t)))

(define time-bs-len 8)
(define (time-bs? x)
  (and (bytes? x) (= (bytes-length x) time-bs-len)))
(define (time-decode time-bs)
  (floating-point-bytes->real time-bs big-endian?))
(define (time-encode since-time)
  (real->floating-point-bytes since-time time-bs-len big-endian?))

(define chain/c (listof (cons/c real? bytes?)))
(define (chain-decode p-or-bs)
  (define from
    (match p-or-bs
      [(? input-port? p) p]
      [(? bytes? bs) (open-input-bytes bs)]))
  (define bob-read (make-bob-reader from))
  (let loop ()
    (match (bob-read)
      [(bob (== EMPTY) (== mt)) '()]
      [(bob (== TIME) t-bs)
       (match-define (bob (== MSG) m) (bob-read))
       (cons (cons (time-decode t-bs) m) (loop))]
      [x
       (error 'chain-decode "Illegal entry: ~e" x)])))
(define (chain-encode c [f-or-to #f])
  (define to (or f-or-to (open-output-bytes)))
  (define bob-write (make-bob-writer to))
  (let loop ([c c])
    (match c
      ['() (bob-write (bob EMPTY mt))]
      [(cons (cons t m) c)
       (bob-write (bob TIME (time-encode t)))
       (bob-write (bob MSG m))
       (loop c)]))
  (close-output-port to)
  (unless f-or-to
    (get-output-bytes to)))

(match-define (list FAIL OK POST GET REPLY EMPTY TIME MSG) (build-list 8 add1))
(define (make-simchain-server-t port-no history-p)
  (define initial-chain
    (if (file-exists? history-p)
      (call-with-input-file history-p chain-decode)
      '()))
  (define (handle-req chain req)
    (match req
      [(bob (== POST) message)
       (values (bob OK mt) (cons (cons (current-inexact-milliseconds) message) chain))]
      [(bob (== GET) (? time-bs? since-time-bs))
       (define since-time (time-decode since-time-bs))
       (define since-chain
         (takef chain (λ (t*m) (< since-time (car t*m)))))
       (values (bob REPLY (chain-encode since-chain)) chain)]
      [_ (values (bob FAIL mt) chain)]))
  (define (done-f last-chain)
    (call-with-output-file history-p (λ (op) (chain-encode last-chain op))
      #:exists 'replace))
  (define ready-sema (make-semaphore))
  (begin0 (make-bob-server-t port-no ready-sema initial-chain handle-req done-f)
    (semaphore-wait ready-sema)))

(struct simreq ())
(struct req:get simreq (since-time))
(struct req:post simreq (msg))
(define (make-simchain-client hostname port-no)
  (define-values (from to) (tcp-connect hostname port-no))
  (define bob-read (make-bob-reader from))
  (define bob-write (make-bob-writer to))
  (match-lambda
    [(req:get since-time)
     #;(eprintf "[C] Write get\n")
     (bob-write (bob GET (time-encode since-time)))
     #;(eprintf "[C] Read reply\n")
     (match-define (bob (== REPLY) chain-bs) (bob-read))
     #;(eprintf "[C] Decode\n")
     (chain-decode chain-bs)]
    [(req:post msg)
     #;(eprintf "[C] Write post\n")
     (bob-write (bob POST msg))
     #;(eprintf "[C] Read ok\n")
     (match-define (bob (== OK) (== mt)) (bob-read))
     (void)]))


(module+ test
  (define test-port 60123)
  (define test-history "test.chain")
  (define m1 #"Hey! Listen!")
  (define m2 #"Yo! Raps!")
  (when (file-exists? test-history)
    (delete-file test-history))
  (eprintf "[T] Starting server...\n")
  (define stop1! (make-simchain-server-t (+ 0 test-port) test-history))
  (define cl1 (make-simchain-client "localhost" (+ 0 test-port)))
  (define cl2 (make-simchain-client "localhost" (+ 0 test-port)))
  (eprintf "[T] Posting test\n")
  (chk (cl1 (req:post m1)) (void))
  (eprintf "[T] Getting test\n")
  (chk (map cdr (cl2 (req:get 0))) (list m1))
  (eprintf "[T] Stopping server...\n")
  (stop1!)
  (define stop2! (make-simchain-server-t (+ 1 test-port) test-history))
  (define cl3 (make-simchain-client "localhost" (+ 1 test-port)))
  (chk (map cdr (cl3 (req:get 0))) (list m1))
  (chk (cl3 (req:post m2)) (void))
  (chk (map cdr (cl3 (req:get 0))) (list m2 m1))
  (chk (map cdr (cl3 (req:get (car (second (cl3 (req:get 0))))))) (list m2))
  (stop2!))

(define (make-simchain-client/queue hostname port-no)
  (local-require data/queue)
  (define simc (make-simchain-client hostname port-no))
  (define (send! m) (simc (req:post m)))
  (define q (make-queue))
  (define last-t 0)
  (define (recv!)
    (cond
      [(queue-empty? q)
       (eprintf "SIMCQ - Queue empty, polling.\n")
       (for-each (λ (m) (enqueue-front! q m))
                 (simc (req:get last-t)))
       (recv!)]
      [else
       (match-define (cons new-t m) (dequeue! q))
       (set! last-t new-t)
       m]))
  (values send! recv!))

(module+ test
  (define stop3! (make-simchain-server-t (+ 2 test-port) test-history))
  (define-values (cl4-send! cl4-recv!)
    (make-simchain-client/queue "localhost" (+ 2 test-port)))  
  (chk (list (cl4-recv!)
             (cl4-recv!)
             (begin (cl4-send! #"A") (cl4-send! #"B") (cl4-send! #"C"))
             (cl4-recv!)
             (cl4-recv!)
             (cl4-recv!))
       (list m1 m2 (void) #"A" #"B" #"C"))
  (stop3!))

(provide
 (contract-out
  [make-simchain-server-t
   (-> port-number? path-string?
       (-> void?))]
  [struct req:get ([since-time real?])]
  [struct req:post ([msg bytes?])]
  [make-simchain-client
   (-> string? port-number?
       (->i ([req simreq?])
            [reply (req)
                   (cond
                     [(req:get? req) chain/c]
                     [(req:post? req) (void)]
                     [else none/c])]))]
  [make-simchain-client/queue
   (-> string? port-number?
       (values (-> bytes? void?)
               (-> bytes?)))]))

(module+ main
  (require racket/cmdline)
  (command-line #:program "simchain"
                #:args (port-no-s history-s)
                (define port-no (string->number port-no-s))
                (unless (port-number? port-no)
                  (error 'simchain "Expected valid port number"))
                (thread-wait (make-simchain-server-t port-no history-s))))
