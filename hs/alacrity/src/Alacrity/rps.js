/**
 * $1. Is `require!` a special concept as well?
 * $2. Should we have an operator for implies?
 */

// #lang alacrity/exe
// (require "stdlib.ala")

// (define-values (ROCK PAPER SCISSORS) (values 0 1 2))
// (define (hand? x)
//   (or (= x ROCK) (or (= x PAPER) (= x SCISSORS))))
const ROCK = 0, PAPER = 1, SCISSORS = 2;
const hand = x => {
  x == ROCK || x == PAPER || x == SCISSORS;
}

// (define-values (B_WINS DRAW A_WINS) (values 0 1 2))
// (define (outcome? x)
//   (or (= x B_WINS) (or (= x DRAW) (= x A_WINS))))
const B_WINS = 0, DRAW = 1, A_WINS = 2;
const outcome = x => {
  x == B_WINS || x == DRAW || x == A_WINS;
}

// (define (RPS-outcome A-hand B-hand) : outcome?
//   (define A-valid? (hand? A-hand))
//   (define B-valid? (hand? B-hand))
//   (cond [(and A-valid? B-valid?)
//          (modulo (+ A-hand (- 4 B-hand)) 3)]
//         [A-valid? A_WINS]
//         [B-valid? B_WINS]
//         [else DRAW]))
const RPS_outcome = (A_hand, B_hand) => {
  const A_valid = hand(A_hand);
  const B_valid = hand(B_hand);
  if (A_valid && B_valid) {
    (A_hand + (4 - B_hand)) % 3;
  }
  else if (A_valid) {
    A_WINS;
  }
  else if (B_valid) {
    B_WINS;
  }
  else {
    DRAW;
  }
}

// #:participants
// (define-participant A
//   [wager-amount : uint256]
//   [escrow-amount : uint256]
//   [A-hand : uint256])
// (define-participant B
//   [B-hand : uint256])
define_participant A = {
  wager_amount : uint256,
  escrow_amount : uint256,
  A_hand : uint256
}
define_participant B = {
  B_hand : uint256
}

// #:main
const main = () => {

// (@ A (assume! (hand? A-hand)))
A @ { assume(hand(A_hand)); }

// (@ B (assume! (hand? B-hand)))
B @ { assume(hand B_hand); }

// (@ A (define-values (A-commit A-salt) (precommit A-hand)))
A @ { const A_commit = precommit, A_salt = A_hand; }

// (@ A (declassify! wager-amount))
A @ { declassify(wager_amount); }

// (@ A (declassify! escrow-amount))
A @ { declassify(escrow_amount); }

// (@ A (declassify! A-commit))
A @ { declassify(A_commit); }

// (@ A (publish! wager-amount escrow-amount A-commit)
//      (pay! (+ wager-amount escrow-amount)))
A @ { publish! {wager_amount, escrow_amount, A_commit};
      pay! (wager_amount + escrow_amount); }

// (@ B (declassify! B-hand))
B @ { declassify(B_hand); }

// (@ B (publish! B-hand) (pay! wager-amount)
//      (require! (hand? B-hand)))
// $1. Is `require!` a special concept as well?
B @ { publish! {B_hand};
      pay! wager_amount;
      require! hand(B_hand); }

// (@ A (declassify! A-salt))
A @ { declassify(A_salt); }

// (@ A (declassify! A-hand))
A @ { declassify(A_hand); }

// (@ A (publish! A-salt A-hand) (pay! 0)
//      (check-commit! A-commit A-salt A-hand)
//      (require! (hand? A-hand))
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
A @ { publish! {A_salt, A_hand};
      pay! 0;
      check_commit(A_commit, A_salt, A_hand);
      require! hand(A_hand);
      const outcome = RPS_outcome(A_hand, B_hand);
      // $2
      assert((outcome = A_WINS) implies hand(A_hand));
      assert((outcome = B_WINS) implies hand(B_hand));
      const gets = () => {
        if (outcome == A_WINS) {
          ((2 * wager_amount) + escrow_amount), 0;
        }
        else if (outcome == B_WINS) {
          escrow_amount, (2 * wager_amount);
        }
        else {
          (wager_amount + escrow_amount), wager_amount;
        }
      };
      const A_gets, B_gets = gets();
      transfer(A, A_gets);
      transfer(B, B_gets); }

// (possible? (= outcome A_WINS))
possible(outcome = A_WINS)

// (possible? (= outcome B_WINS))
possible(outcome = B_WINS)

// outcome
outcome
}
