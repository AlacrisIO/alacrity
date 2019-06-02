#lang racket/base
(require racket/match
         racket/format
         racket/port
         racket/syntax
         racket/string
         racket/list
         racket/file
         racket/system
         racket/set
         racket/pretty
         syntax/parse/define
         (for-syntax racket/base
                     syntax/parse))
(module+ test
  (require chk))

;; Utility
(define (snoc l x) (append l (list x)))
(define (sort-symbols l)
  (sort l string<=? #:key symbol->string))
(define (set->sorted-list s)
  (sort-symbols (set->list s)))
(define (error-on-dupes where msg l)
  (cond
    [(check-duplicates l)
     => (λ (el) (error where msg el))]))

(define (merge-non-overlapping-hash x y)
  (for/fold ([x x]) ([(k v) (in-hash y)])
    (when (hash-has-key? x k)
      (error 'merge-non-overlapping-hash "Hash cannot overlap!"))
    (hash-set x k v)))
(define (merge-non-overlapping-hashes xs)
  (for/fold ([y (hasheq)]) ([x (in-list xs)])
    (merge-non-overlapping-hash x y)))

(define fresh-box (make-parameter #f))
(define-syntax-rule (with-fresh b ...)
  (begin
    (when (fresh-box)
      (error 'with-fresh "Should not use with-fresh twice in same context!"))
    (parameterize ([fresh-box (box 0)])
      b ...)))
(define (freshen s)
  (define tfb (fresh-box))
  (define fresh-count (unbox tfb))
  (set-box! tfb (add1 fresh-count))
  (string->symbol (~a s fresh-count)))

(define _seq_ (gensym '_))

;; Types
(struct ty () #:transparent)
(struct ty:unit ty () #:transparent)
(struct ty:int ty (signed? width) #:transparent)
(struct ty:address ty () #:transparent)
(struct ty:participant ty () #:transparent)
(struct ty:array ty (element-ty size) #:transparent)
(struct ty:key:asym () #:transparent)
(struct ty:key:sym () #:transparent)
(struct ty:map ty (key-ty val-ty) #:transparent)
;; A normal primitive
(struct ty:prim ty (dom rng) #:transparent)
;; A primitive that is polymorphic in the width of the integers given.
(struct ty:int-prim ty (f) #:transparent)

;; Primitives
(define prim->ty (make-hasheq))
(define (se:primitive? x)
  (and (symbol? x) (hash-has-key? prim->ty x)))
(define prim->rkt (make-hasheq))
(define-simple-macro (define-primitive op:id ty:expr)
  (begin (hash-set! prim->ty 'op ty)))

(define (two-intW->one-intW dom) 'XXX)
(define-primitive + (ty:int-prim two-intW->one-intW))
(define-primitive - (ty:int-prim two-intW->one-intW))
(define-primitive * (ty:int-prim two-intW->one-intW))
(define-primitive quotient (ty:int-prim two-intW->one-intW))
(define-primitive modulo (ty:int-prim two-intW->one-intW))
(define (two-intW->bool dom) 'XXX)
(define-primitive = (ty:int-prim two-intW->bool))

(define-primitive participant->address
  (ty:prim (list (ty:participant)) (ty:address)))
(define-primitive participant->pub-key
  (ty:prim (list (ty:participant)) (ty:key:asym)))
(define-primitive participant->pri-key
  (ty:prim (list (ty:participant)) (ty:key:asym)))
(define-primitive contract-address
  (ty:prim (list) (ty:address)))

;; Constants
(define (se:constant? x)
  (or (exact-integer? x)
      (boolean? x)
      (void? x)))

;; Actor
(struct actor () #:transparent)
(struct actor:role actor (r) #:transparent)
(struct actor:all actor () #:transparent)
(define (se:actor? x)
  (or (eq? x 'ALL)
      (and (symbol? x)
           )))
(define (se->actor x)
  (match x
    ['ALL (actor:all)]
    [(and (? symbol?)
          (app symbol->string (regexp #rx"^@")))
     (actor:role x)]))
(define (actor->se x)
  (match x
    [(actor:all) 'ALL]
    [(actor:role x) x]))

;; Whole-Protocol Expressions
(struct we () #:transparent)
(struct we:con we (b) #:transparent)
(struct we:prim-app we (p rands) #:transparent)
(struct we:if we (ce te fe) #:transparent)
(struct we:var we (x) #:transparent)
(struct we:fun-app we (f roles rands) #:transparent)
(struct we:assert! we (rely? ce) #:transparent)
(struct we:let-values@ we (where xs xe ke) #:transparent)
(struct we:consensus! we (initiator from-vs ctc-e to-vs ke) #:transparent)
;; XXX classify / declassify
;; XXX transfer!

(module+ test
  (define RPS-se
    `(program
      ;; Each participant starts knowing their hand. If we wanted, we
      ;; could add a primitive like "query-user", or assume that
      ;; primitives can be passed as arguments to allow Alacrity to
      ;; interact with Javascript, but this would require a kind of
      ;; modal type, which would be annoying.

      ;; A starts knowing the wager and escrow amount, which means
      ;; that it gets to set the terms of the wager. If we added those
      ;; to B's initial knowledge as well, then it would mean that B
      ;; will thinks it knows them, so after the first message, if it
      ;; is inconsistent, then B will back out (because an assumption
      ;; is violated.)

      ;; XXX Don't make these globals, instead make a main expression
      ;; with them provided?

      ;; XXX Add types, including security classification types where
      ;; {} means no one has access, {@A} means just A, and so on.
      (participant @A wager-amount escrow-amount A-hand)
      (participant @B B-hand)

      ;; This would be the result of a `define-enum` macro.
      (define-values (ROCK PAPER SCISSORS) (values 0 1 2))
      (define (hand? x)
        (or (= x ROCK) (= x PAPER) (= x SCISSORS)))
      ;; Ditto
      (define-values (B_WINS DRAW A_WINS) (values 0 1 2))
      (define (outcome? x)
        (or (= x B_WINS) (= x DRAW) (= x A_WINS)))
      
      (define (RPS-outcome A-hand B-hand)
        (define A-valid? (hand? A-hand))
        (define B-valid? (hand? B-hand))
        ;; HONEST is a special value that is true when we trust
        ;; all participants. This is never true in production,
        ;; but it is true in one mode of the Z3 spec.
        ;;
        ;; XXX Does this make sense?
        (guarantee! (implies HONEST (and A-valid? B-valid?)))
        (define o
          (cond
            [(and A-valid? B-valid?)
             (modulo (+ A-hand (- 4 B-hand)) 3)]
            ;; The cheater loses.
            [A-valid? A_WINS]
            [B-valid? B_WINS]
            ;; If both cheat, then it is a draw.
            [else DRAW]))
        (guarantee! (outcome? o))
        o)

      ;; Standard library functions
      (define (precommit x)
        (define reveal (msg-cat (random) x))
        (define commitment (digest reveal))
        (values reveal commitment))
      (define (check-commit commitment reveal)
        (rely! (equal? commitment (digest reveal)))
        (msg-cat-right reveal))
      
      (define (main)
        ;; A and B both rely on being passed correct arguments. The
        ;; contract won't innately trust this, but we'll prove that
        ;; honest participants won't cause a distrustful contract to
        ;; fail.
        (@A (rely! (hand? A-hand)))
        (@B (rely! (hand? B-hand)))
        ;; A creates the precommitment.
        (@A define-values (A-reveal A-commit) (precommit A-hand))
        ;; A consensus action is always initiated by one participant
        ;; which transmits the values of some variables. At the
        ;; contract, some work can happen, such as transfering
        ;; resources as in this case. At all other participants, the
        ;; values of the variables are received. In this case, B, gets
        ;; (wa ea A-c). If B already knew wa and ea, then it would
        ;; verify that the values are the same.
        ;;
        ;; In the direct compilation output, this is going to be a
        ;; Solidity method, invoked by the A JS code and monitored by
        ;; the B JS code. In other compilation modes, everyone is
        ;; going to execute this code and check each other's work.

        ;; XXX Can we detect the #:in based on what variables are free
        ;; in the consensus, then commute consensus blocks forward &
        ;; backward to combine adjacent senders. If so, then this is
        ;; like an intermediate form.
        (consensus!
         #:in @A (wager-amount escrow-amount A-commit)
         (transfer! @A CTC (+ wager-amount escrow-amount)))
        ;; It is now time for B to send its value.
        (consensus!
         #:in @B (B-hand)
         ;; We don't check that B is a valid hand, because we have to
         ;; handle what happens if A is dishonest no matter what, so
         ;; we just trust B for now and will punish it later for
         ;; lying.
         (transfer! @B CTC wager-amount))
        ;; In this next message, the values that A sends are a subset
        ;; of what the contract binds and what B receives.
        (consensus!
         #:in @A (A-reveal)
         ;; This #:out spec says what B learns
         ;;
         ;; XXX Maybe this is not necessary
         #:out (A-reveal A-hand outcome A-gets B-gets)
         ;; At the contract, we verify A's commitment, compute the
         ;; outcome, and finalize the transfers.
         (define A-hand (check-commit A-commit A-reveal))
         (define outcome (RPS-outcome A-hand B-hand))
         (guarantee!
          (implies (not (hand? A-hand))
                   (not (equal? outcome A_WINS))))
         (guarantee!
          (implies (not (hand? B-hand))
                   (not (equal? outcome B_WINS))))
         (define-values (A-gets B-gets)
           (cond
             [(equal? outcome A_WINS)
              (values (+ (* 2 wager-amount) escrow-amount) 0)]
             [(equal? outcome B_WINS)
              (values escrow-amount (* 2 wager-amount))]
             [else
              (guarantee! outcome DRAW)
              (values (+ wager-amount escrow-amount) wager-amount)]))
         (transfer! CTC @A A-gets)
         (transfer! CTC @B B-gets))
        ;; The program returns the outcome.
        outcome)
      (main))))

;; Compilation

;; XXX The consensus blocks should list the free variables so we can
;; create a hash of all of them as the on-contract storage.

;; Z3
(define prim->z3 (make-hasheq))

;; The Z3 program should verify the follows claims:

;; - guarantee! in participant is true
;; - guarantee! in contract is true
;; - rely! in contract is true with honest participants
;;   (i.e. participants rely!'s are trusted)

;; NOTE: rely! is checked at run-time, but trusted in Z3

;; - balance of CTC is 0 on halt
