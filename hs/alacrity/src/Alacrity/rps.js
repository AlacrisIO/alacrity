/**
 * $1. Could use `StructType`?
 * $2. Possibly not parsing `(@ A (define-values (SA-commit SA-salt) (precommit SA-hand)))` correctly.
 * $3. Is there a reason why some `(@ A ...)` have multiple sub-expressions?
 * $4. Not confident in my JS-LIKE syntax for `(@ A ...)` expressions.
 */

// #lang alacrity/exe
// (require "stdlib.ala")

// (define-values (ROCK PAPER SCISSORS) (values 0 1 2))
// (define (hand? x)
//   (or (= x ROCK) (or (= x PAPER) (= x SCISSORS))))
var ROCK = 0, PAPER = 1, SCISSORS = 2;
let hand = x => {
  return x === ROCK || x === PAPER || x === SCISSORS;
}

// (define-values (B_WINS DRAW A_WINS) (values 0 1 2))
// (define (outcome? x)
//   (or (= x B_WINS) (or (= x DRAW) (= x A_WINS))))
var B_WINS = 0, DRAW = 1, A_WINS = 2;
let outcome = x => {
  return x === B_WINS || x === DRAW || x === A_WINS;
}

// (define (RPS-outcome A-hand B-hand) : outcome?
//   (define A-valid? (hand? A-hand))
//   (define B-valid? (hand? B-hand))
//   (cond [(and A-valid? B-valid?)
//          (modulo (+ A-hand (- 4 B-hand)) 3)]
//         [A-valid? A_WINS]
//         [B-valid? B_WINS]
//         [else DRAW]))
let RPS_outcome = (A_hand, B_hand) => {
  var A_valid = hand(A_hand);
  var B_valid = hand(B_hand);
  if (A_valid && B_valid) {
    return (A_hand + (4 - B_hand)) % 3;
  }
  else if (A_valid) {
    return A_WINS;
  }
  else if (B_valid) {
    return B_WINS;
  }
  else {
    return DRAW;
  }
}

// #:participants
// (define-participant A
//   [Swager-amount : int]
//   [Sescrow-amount : int]
//   [SA-hand : int])
// (define-participant B
//   [SB-hand : int])
// $1. `StructType`?
let A = (Swager_amount, Sescrow_amount, SA_hand)  => {
  return {
    "Swager_amount": Swager_amount,
    "Sescrow_amount": Sescrow_amount,
    "SA_hand": SA_hand
  }
}
let B = (SB_hand) => {
  return { "SB_hand": SB_hand }
}

// #:main
let main = () => {
// (@ A (assert! (hand? SA-hand)))
// (@ A (define-values (SA-commit SA-salt) (precommit SA-hand)))
// (@ A (define-values (wager-amount escrow-amount A-commit)
//        (values (declassify Swager-amount)
//                (declassify Sescrow-amount)
//                (declassify SA-commit))))
// (@ A (publish! wager-amount escrow-amount A-commit)
//      (pay! (+ wager-amount escrow-amount)))
A @ assert(hand(SA_hand));
// $2. Not sure
// A @ (let SA_commit = SA_salt => {
//   return precommit SA_hand;
// });
A @ (var SA_commit = precommit, SA_salt = SA_hand);
A @ (var wager_amount = declassify(Swager_amount),
         escrow_amount = declassify(Sescrow_amount),
         A_commit = declassify(SA_commit));
// $3. Is there a reason why this is combined?
A @ (publish(wager_amount, escrow_amount, A_commit));
    (pay(wager_amount + escrow_amount));

// (@ B (assert! (hand? SB-hand)))
// (@ B (define B-hand (declassify SB-hand)))
// (@ B (publish! B-hand) (pay! wager-amount)
//      (assert! (implies (HONEST) (hand? B-hand))))
B @ (assert(hand(SB_hand)));
B @ (var B_hand = declassify(SB_hand));
// $3
B @ (publish(B_hand))
    (pay(wager_amount))
    (assert((HONEST) implies hand(B_hand)));

// (@ A (define A-salt (declassify SA-salt)))
// (@ A (define A-hand (declassify SA-hand)))
// (@ A (publish! A-salt A-hand) (pay! 0)
//      (check-commit! A-commit A-salt A-hand)
//      (assert! (implies (HONEST) (hand? A-hand)))
//      (define outcome (RPS-outcome A-hand B-hand))
//      (assert! (implies (= outcome A_WINS) (hand? A-hand)))
//      (assert! (implies (= outcome B_WINS) (hand? B-hand)))
//      (define-values (A-gets B-gets)
//        (cond
//          [(= outcome A_WINS)
//           (values (+ (* 2 wager-amount) escrow-amount) 0)]
//          [(= outcome B_WINS)
//           (values escrow-amount (* 2 wager-amount))]
//          [else
//           (values (+ wager-amount escrow-amount) wager-amount)]))
//      (transfer! A A-gets)
//      (transfer! B B-gets))
A @ (var A_salt = declassify(SA_salt));
A @ (var A_hand = declassify(SA_hand));
// $3
A @ (publish(A_salt, A_hand))
    (pay(0))
    (check_commit(A_commit, A_salt, A_hand))
    (assert((HONEST) implies hand(A_hand)))
    (var outcome = RPS_outcome(A_hand, B_hand))
    (let gets = () => {
      if (outcome === A_WINS) {
        return [((2 * wager_amount) + escrow_amount), 0];
      }
      else if (outcome === B_WINS) {
        return [escrow_amount, (2 * wager_amount)];
      }
      else {
        return [(wager_amount + escrow_amount), wager_amount];
      }
    })
    (var A_gets = gets[0], B_gets = gets[1])
    (transfer(A, A_gets))
    (transfer(B, B_gets));

// outcome
outcome
}
