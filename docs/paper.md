# Alacrity: A DSL for Simple, Formally-Verified DApps

Jay McCarthy <jay@alacris.io>

Head of Research, Alacris

Associate Professor, University of Massachusetts Lowell

# Introduction

Alacrity is a domain-specific language for writing decentralized
applications. It provides automatic solutions to a few key problems
faced by blockchain developers: verifying that the DApp is
trustworthy, ensuring the smart contract is consistent with
client-side software, and abstracting over different blockchains. 

Alacrity programs embed descriptions of properties about the behavior
of the application. Safety properties assert mistakes do not occur and
liveness properties assert desired outcomes do occur. The compiler
automatically adds soundness properties that assert the application
does not violate fundamental expectations of all DApps, such as that
they conserve resources. The compiler automatically verifies
correctness of these properties using the Z3 SMT theorem prover
without intervention from programmers. This ensures that Alacrity
programs are not susceptible to attacks that steal their resources,
and ensures that untrusting participants can rely on the integrity and
valid of the Alacrity program.

Alacrity programs incorporate the client-side behavior of
participants, as well as on-chain behavior of the contract. The
Alacrity compiler uses end-point projection to extract programs for
each party and the contract, while guaranteeing each side makes the
same assumptions about application state and communication
protocols. This ensures that attacks do not exist that exploit
the slightly different semantics of blockchain virtual machines and
client-side programming languages.

Alacrity uses a blockchain-agnostic model of computation that allows
programs to target different chains, including scaling
solutions. This ensures that DApps can be designed independently of
the deployment details and not be tied to the particular vagaries of
any one platform.

The core philosophy of Alacrity is to design a highly constrained
programming language that makes it easy to automatically prove the
structural components of desirable properties about DApps and makes it
possible to easily prove the user-specific components of those
properties. This is in contrast to designing an unconstrained language
and providing a novel new proving technique.

In this article, we walk through an example Alacrity program and show
how Alacrity performs each of these functions.

# External References and Example Program
 
The Alacrity repository is located at:

https://github.com/AlacrisIO/alacrity

In this article, we will repeatedly refer to a simple example program:

https://github.com/AlacrisIO/alacrity/blob/master/examples/rps-auto/rps/rps.ala

And the results of Alacrity's compilation of the program:

https://github.com/AlacrisIO/alacrity/tree/master/examples/rps-auto/build

This program models a wager on the result of a game of
Rock-Paper-Scissors between Alice and Bob. In the game, Alice
transfers a wager and an escrow into the contract along with a
commitment of her hand (via hashing it with a random salt.) Next, Bob
transfers the wager and reveals his hand. Then, Alice reveals her hand
by publishing the salt and the hand. At this point, the outcome of the
game is determined and the winner receives both wagers, while Alice
receives back the escrow.

# Computation Model & Program Walkthrough

Alacrity programs define the interactions between a set of
participants as they reach consensus on the results of computations
based on variables initially known only to one participant. The number
of interactions and participants are fixed, finite, and known at the
beginning of the program.

In our example program, the participants are `A` ("Alice") and `B`
("Bob"). Alice knows the amount of the wager, the amount of escrow it
will deposit in the contract, and her hand. Bob knows his hand. We
express this in the language by writing:

```
participant A {
    uint256 wagerAmount,
    uint256 escrowAmount,
    uint256 handA }

participant B {
    uint256 handB }
```

In Alacrity, each participant performs a block of local actions until
they reach a point at which a consensus is necessary. After resolving
the consensus block, they return to local actions. Local actions are
assumed to be run by all participants, but some actions may be
annotated with the single party that takes them.

In our example program, in the first local block each participants
makes claims about their initial knowledge via the `assume!` form,
which is a kind of assertion. (In the Verification section, we will
discuss the subtleties of the various kinds of assertion in Alacrity.)
These two claims merely state that the hand values (which are
arbitrary unsigned integers) are within the fixed range of the
enumeration of hand values.

```
main {
    @A assume! isHand(handA);
    @B assume! isHand(handB);
```

Next, Alice computes the commitment by calling a library function
`precommit` and receiving the multiple values it returns. It binds
these values to the new constants `commitA` (the commitment) and
`saltA` (the random salt.)

```
    @A const commitA, saltA = precommit(handA);
```

At this point, the first consensual action will occur. Alice needs to
publish the terms of the bet (`wagerAmount` and `escrowAmount`) as
well as her commitment (`commitA`). She also needs to actually
transfer this amount of resources into the contract's
account. Finally, all parties need to agree that Alice actually sent
this information.

There is a small wrinkle, however. Alacrity uses an information-flow
security type system to ensure that participants do not accidentally
reveal secret information. All information that only one participant
knows is assumed to be secret. In this case, Alice's knowledge of the
terms of the bet is secret. However, we do not hold the commitment
secret, because it is the result of a hashing algorithm which does not
reveal the values of the inputs, so Alacrity considers it public
information automatically. Therefore, Alice needs to first declassify
this information before performing the transfer.

```
    @A declassify! wagerAmount;
    @A declassify! escrowAmount;
    @A publish! wagerAmount, escrowAmount, commitA
       w/ (wagerAmount + escrowAmount);
    return;
```

The first two lines perform the declassification, while the next two
perform the publishing and payment to the contract. The last line
(`return;`) finishes the consensual block and returns to the next
local block. After this statement, it is now consensual knowledge that
Alice shared these three values and transferred the appropriate
amount.

There is no next local block, however, because there's no additional
computation necessary. Instead, we move immediately to the next
consensus block, which is initiated by Bob. In Alacrity, there is
always exactly one participant that initiates a consensus block. In
this block, Bob declassifies his hand, publishes it, and transfers the
wager amount (which he has just learned from the last consensus
block.)

```
    @B declassify! handB;
    @B publish! handB w/ wagerAmount;
    require! isHand(handB);
    return;
```

This consensus block, however, does not immediately return to local
control. In addition to verifying that Bob actually transferred the
wager amount; all parties also claim that Bob's hand is valid. In this
case, we use a new kind of claim (`require!`) rather than the one used
initially by Bob (`assume!`). Although Bob has already checked that
the hand is valid, that claim was not consensual, so Alice cannot rely
on it. Thus, Alice needs to verify the claim as soon as she learns the
value. If Bob were dishonest and did not actually check the claim in
the program, it would be consensually verified at this point and Bob's
attempt to publish it would be rejected.

The last consensus block is where a lot of action is going to
happen. We will break it down into a number of steps.

First, Alice publishes the inputs to the commitment, after
declassifying them, and we consensually verify that the earlier
commitment actually is made from these inputs:

```
    @A declassify! saltA;
    @A declassify! handA;
    @A publish! saltA, handA w/ 0;
    check_commit(commitA, saltA, handA);
```

This block indicates that the consensus retains knowledge of the prior
commitment by Alice, because the `commitA` variable is still in scope
in the consensus. Once the consensus knows Alice's hand, and that it
is the same as was committed earlier, we can check that it is valid
and determine the winner.

```
    require! isHand(handA);
    const outcome = winner(handA, handB);
    [....]
```

Once the winner is known, we can compute the winnings and transfer
them to the appropriate parties:

```
    const getsA, getsB =
          if (outcome == A_WINS) {
              values (2 * wagerAmount), 0 }
          else if (outcome == B_WINS) {
              values 0, (2 * wagerAmount) }
          else {
              values wagerAmount, wagerAmount };
    transfer! A <- (escrowAmount + getsA);
    transfer! B <- getsB;
    return;
```

The computation is now over and the two parties simply return the
final outcome:

```
    [....]
    outcome }
```

This program never explicitly deals with the low-level issues of
writing blockchain programs: there are no block numbers, gas
calculations, calling contract methods, subscribing to contract
events, and so on. From Alacrity's perspective, a blockchain is simply
mutual knowledge about a monotonically increasing list of values,
where the validity of each block of values depends on the previous
values. We could express this as the following type:

```
Block := List Value
Chain := List Block

Contract := {
    append : Chain x Block -> Maybe Chain
}
```

An `append` operation only succeeds (returns `Just next_chain`) if the
values are consistent with the constraints imposed by the consensus
block. The prior chain is an argument to the `append` function because
the prior blocks may influence the constraints on the current
block.

In practice, we assume a slightly simpler model of a blockchain
contract by abstracting chains into a state type specific to the
particular contract.

```
Contract State := {
    initial : State
    observe : State x Block -> Maybe State
}
```

Although it is possible for this state to be the entire chain, it is
often more efficient to select a smaller type. From a particular
chain, it is always possible to discover the current state by folding
the `observe` function over the blocks on the chain.

Similarly, participants in a decentralized application are modeled as
agents with private knowledge that maybe react to blocks they observe
on the chain.

```
Participant Internal := {
    start : Internal x Maybe Block
    react : Internal x State x Block
         -> Internal x Maybe Block
```

The `start` object represents the initial private of the participant
and whether they are the first publisher. The `react` function updates
their internal state based on the current state of the contract and
the most recent message, as well as potentially publishes another
message. Again, given a chain and a participant, we can always
determine how that participant would react to each action.

This computation model (`Contract` and `Participant`) defines the
expectations that Alacrity has on blockchains and client platforms
that it deploys to. In practice, the complexity of the `observe`
function determines whether a particular Alacrity program could deploy
to a particular chain. We have purposefully design the computational
abilities of Alacrity to map to the lowest-common-denominator chains.

# Analysis

The Alacrity compiler performs some analyses on the source program to
verify a number of essential safety properties. Each of these analyses
can be expressed as a constraint on Alacrity programs.

Alacrity is type-safe, so all operations must receive the correct
number and type of arguments. The type system is simple, however, so
it is always possible determine the types of intermediate expressions
based on the types of the arguments. The only annotations programmers
are required to add are on the initial knowledge of the
participants. This ensures that there are no unsafe operations
performed on illegal input, which could lead to errors like buffer
overflows or type confusion.

Alacrity mandates that all interactions and computations are
finite. This is enforced by not including looping forms like `while`
and `for` and disallowing recursive functions. This ensures that all
computations can run within estimable bounds.

Alacrity's model assumes that all interactions have a consensual next
initiator. This means that local actions can only determine the values
in the interactions, not structure of the interaction. In practice,
this means that `if` statements must either only produce values, or
they must be located inside of consensus blocks. This is an essential
property to avoid confusion where participants have disunity on the
state of the program.

Actors in Alacrity (the participants and the contract) always require
a single next action that will be run. In programming language theory
parlance, this means that the continuation of every statement must be
statically knowable. This is enforced by an A-Normal Form-style
transformation that exposes the continuation of every expression. The
most subtle aspect of this is that the continuations of impure `if`
statements must be inlined into the two branches, duplicating code, to
enforce the previously mentioned conditions on `if`s. The compiler
does extra purity analysis to turn `if`s in the source code into
conditional moves in the intermediate representations to avoid code
growth.

Alacrity's type system is information-flow sensitive. As mentioned
before, all initial knowledge of participants is marked as secret by
Alacrity. Any transformation that involves secret information in any
way produces secret information. Most importantly, this means that `if
Secret then Public else Public` produces secret information, not
public, because a secret value was used to compute the branch
taken. When Alacrity programmers attempt to publish information, the
compiler refuses to continue if the information is secret; instead,
the programmer must explicitly declassify it before sending. We could
remove these annotations by always assuming that published information
is implicitly declassified, but we view manual declassification
annotations as a fundamental step in security auditing: programmers
should have to explicitly decide when something is free to release to
the public.

Alacrity's variable scope rules are subtle because programs involve
the actions of many parties. During type checking, the compiler must
ensure that each participant is only relying on values that they
possess, whether because they initially knew them or gained them via
publications by other participants.

Each of these analyses work together to form a basic kind of soundness
for Alacrity programs that the rest of the compiler suite rely on.

# Verification

Alacrity programs embed statements of logical properties of their
correctness. In addition to these program-specific properties,
Alacrity automatically embeds claims that resources are preserved and
the contract's balance is zero at the end of the program.

The Alacrity compiler proves these properties by representing the
program as an SMT problem and
delivers it to an SMT solver (e.g. Z3) for verification.

(Skip this paragraph if you do not need an introduction to SMT.)
Satisfiability modulo theories (SMT) is a decision problem on logical
formulas and sets of equational theories. SMT can be seen as an
optimization of satisfiability (SAT). A SAT problem concerns a set of
a boolean variables ($x_0$, $x_1$, ... $x_n$) and a formula over them
($x_0 \vee \neg x_1 \implies x_2$). The SAT solver determines if there
is an assignment of the variables to values such that the formula
evaluates to true. SAT was the first problem be proved to be
NP-complete. If NP does not equal P, then SAT is intractable and there
is no solution that is not exponential. SMT generalizes SAT by adding
"sorts", which are types to normal programmers, functions that operate
on these new sorts, equations that relate different functions
together. For example, in SMT, `boolean` is a sort, `not` is a
function from a boolean to boolean and `not (not x) = x` is an
equation. Rich SMT solvers have many more theories, such as a theory
of natural number, bit vectors, arrays, and so on. They also give
users the ability to define new sorts and new equational theories over
them. The main thing that a SMT solver does is determine if a formula
is satisfiability (i.e. there exists an assignment of variables to
values where the formula is true). Most SMT solvers (and Z3, which we
use, in particular) also provide the ability to derive models, which
are the actual values that satisfy the formula. (It is important to
understand that simply determining if an assignment exists does not
entail that you know the values.)

Given the low-computation complexity of Alacrity and the A-Normal Form
of the intermediate language, representing Alacrity programs as SMT
problems is simple: each variable definition in the intermediate
language becomes a variable in the SMT problems of the appropriate
sort and is constrained to be equal the right-hand side of the
variable definition. We extend the set of sorts and theories to deal
with the particular kinds of values (like message digests and byte
strings) used in Alacrity programs. (See the `build/rps.z3` file for
an example Z3 verification session.)

Alacrity verifies the correctness of each property from the
perspective of each participant, as well as the contract, and under a
"trusted" and an "untrusted" mode. In the "trusted" perspective,
participants' SMT problem includes the actions for all participants;
while in the "untrusted" perspective, these actions are ignored and
only the particular participant's actions are included. These
different modes correspond to trust because if Alice's SMT problem
includes Bob's actions, then Alice is trusting that Bob will actually
perform them. In contrast, if Alice's problem does not include them,
then from the perspective of the SMT problem, the values Bob publishes
are completely unconstrained.

Alacrity supports four kinds of properties:

**Assumptions** (`assume!`) are checked at runtime (and if false, the
program aborts) and assumed to be true when included in the SMT
problem. This means that they become SMT assertions. In our example
program, Alice and Bob both assume that their hands are valid. These
statements cannot be verified by Z3, because they are based on values
from outside of the Alacrity program.

**Assertions** (`assert!`) are ignored at runtime and are verified by the
SMT problem. They are verified by asserting their negation and
checking for satisfiability. If the assertion is true, there should be
no assignment of the variables in the program that make the statement
false, so the SMT solver should return an `UNSAT` result. In our
example program, we include two assertions:
  
```
    assert! ((outcome == A_WINS) => isHand(handA));
    assert! ((outcome == B_WINS) => isHand(handB));
```
  
These establish that if Alice or Bob submit an invalid hand, then they
are not the winner. This property would be false if we incorrectly
implemented the outcome calculation and did not check for validity of
hands inside it. This is a logical property of the game itself and
helps to establish trust in the Alacrity program on the part of
users. Additionally, Alacrity automatically generates an assertion
that the balance of the contract is zero at the end of the program
run. This ensures that no resources are lost by the contract.
  
Assertions are used by Alacrity programmers to check that
program-specific safety properties are respected by the program.
  
**Requirements** (`require!`) are checked at runtime (and if false,
the program aborts) and behave differently in the SMT problem
depending on the mode. In trusted mode, they behave as assertions and
are verified; while in untrusted mode, they are assumed to be
true. This may sound counter-intuitive, because lack of trust seems to
suggest that they are suspect and should be checked. However,
untrusted mode really refers to not assuming that the other
participants followed the program. This means that we can't rely on
any particular value being produced and need to make some sort of
assumption on what it is; hence, the requirement states the
assumptions that are necessary for the continuation to be correct. In
contrast, trusted mode actually proves that these assumptions are met
by the actual participants, which is why trusted requirements are
treated with suspicion and verified. In our example, we use
requirements to verify that Alice and Bob's hands are valid and that
Alice actually submits the random salt and hand that she previously
committed to. In addition to these requirements specified by the
programmer, Alacrity automatically generates requirements that the
amount transferred to the consensus at each interaction is the same as
is specified in the program (e.g., Bob actually transmits the wager
amount.)

**Possibilities** (`possible?`) are ignored at runtime and are checked
for satisfiability in the SMT problem. Unlike assertions, these are
not negated in the SMT problem. This means that we are verifying that
it is possible for some values of inputs to arrive at the truth of the
statement. In our example program, we explore six possibilities
(abstracted with a function in the real code):
  
```
    possible? ((handA == ROCK) && (outcome == A_WINS));
    possible? ((handA == PAPER) && (outcome == A_WINS));
    possible? ((handA == SCISSORS) && (outcome == A_WINS));
    possible? ((handB == ROCK) && (outcome == B_WINS));
    possible? ((handB == PAPER) && (outcome == B_WINS));
    possible? ((handB == SCISSORS) && (outcome == B_WINS));
```
  
These establish that the game is fair and it is possible for both
Alice and Bob to be the winner. This would be false if we incorrectly
implemented the outcome calculation such that one party always won; or
if there was a flaw in the communication such that Bob could observe
Alice's hand and always win or if Alice could observe Bob's hand and
change her commitment.
  
Possibilities are used by Alacrity programmers to check that
program-specific liveness properties are respected by the program,
thereby increasing trust in the game.

---

The verification offered by Alacrity lowers the degree of trust that
users need to place in decentralized applications. Rather than
auditing their entire source code, they must only inspect the
assertions and possibilities.

# End-Point Projection

XXX

# Future Work

The fundamentals of Alacrity are all in place, but there remains more
work to be done for it to be sufficient for all decentralized
applications.

Although Alacrity's model is blockchain agnostic, we currently only
target Ethereum. We plan to target Tezos and Nervos, as well as
demonstrate a backend for Bitcoin Cash Script.

Presently, our Ethereum backend generates Solidity code rather than
bytecode directly. Given that we use such a restricted form of
Solidity, we intend to generate Ethereum and take over optimization of
the code directly.

The computational fragment of Alacrity is quite limited, with a small
number of types and operations. We intend to extend the type system
and standard library to incorporate more functionality. We will
add simply-typed functions (to ensure termination) and loops with
provable bounds.

The aforementioned limitations are fundamentally engineering problems
where we understand the solutions and need to spend the time and money
to develop them. There are, however, more interesting theoretical
problems we are working on next. Each of these involves relaxing one
of the constraints on the Alacrity computation model.

Many DApps do not have a single designated next action in all cases,
but instead offer a choice between two different continuations
(including different actors in each.) Alacrity does not support
`CHOICE`-style interactions. The fundamental challenge is ensuring
that when Alice has a choice between two options, Bob can reliably
learn which Alice chose. This suggests that choices are always
expressed in terms of multiple next consensus blocks, perhaps with
differing requirements. In the context of our example, we would want
to allow Alice to recover her deposit if Bob refuses to play after
some timeout.

Similarly, most DApps are not of finite length. Indeed, most existing
DApps are specifically infinitely wrong. We will support this by using
a Hoare logic rule on `WHILE`s inside of the interaction logic. The
Hoare invariant will be used to do modular verification of the
predecessor blocks, loop body block, and successor blocks of the
loop. In the context of our example, we should enable a variant of the
game where a draw results in more games, until a winner is chosen.

In Alacrity, participants represents particular keys on the blockchain
we deploy to. The set of participants is fixed at the beginning of the
program and embedded into the protocol state. Most DApps do not
involve a predetermine set of participants, but instead involve a
dynamically known set of participants drawn from some set of
participant classes. For example, a blackjack game involves the house
and a set of players. The main problem this presents for the Alacrity
model is that Alice may wish to post a block, but is unable to because
Eve posted first. We need to update the Alacrity model to allow Alice
to update her block based on the new state resulting from Eve's and
try again. It is likely that this is a special case of a `WHILE` and a
`CHOICE`.

Finally, we are interested in exploring the semantics of decentralized
applications that concurrent operate on multiple consensus chains
(rather than a single network) and only partially share information
(rather than only distinguishing between `Public` and `Secret`.)

# Conclusion

Alacrity is a new domain-specific language specialized for trustworthy
decentralized applications. Its blockchain-agnostic model frees
developers from lock-in to specific platform. Its verification
strategy increases the reliability and trustworthiness of Alacrity
programs over manually developed DApps. Alacrity's use of end-point
projection ensures that on-chain and client-side computations are
synchronized and agree on all fundamental parts of program operation.

Although Alacrity is usable today, it is a work-in-progress with a lot
of room from growth and development. You can start using it today by
visiting
[AlacrisIO/alacrity](https://github.com/AlacrisIO/alacrity). We will
be running a workshop in Boston in Fall 2019. Please visit
[alacris.io](https://alacris.io) for more information!
