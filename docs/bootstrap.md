# Bootstrap Plan for Alacrity

This is the plan to bootstrap Alacrity into existence.

[[__TOC__]]

## The Big Picture

We are planning to develop Alacrity in such a way that
at every point starting from our *Minimal Viable Product* (MVP),
we will have a useful product creating value for users and generating revenue for us.

Our MVP is a compiler for multiparty computations that use a blockchain smart contract
to keep all parties honest. It outputs a smart contract in "direct style" that matches
how a developer might straightforwardly write that contract by hand in Solidity,
as well as the associated client code that the same developer might write by hand in JavaScript.
From that MVP, we will extend our system in two dimensions:
(1) vertically into layers of functionality that extend how large problems Alacrity can handle,
in way that increases in scope and performance,
from a low-level model of what a blockchain provides,
to a logic-based model for specifying scalable, portable, interoperable, secure applications, and
(2) horizontally into broader features
that extend the expressiveness of the language and the kind of problems it can solve,
from simple interactions between two participants using one token,
to complex multi-asset trades across time within an unlimited pool of participants.

### Vertical Layers of Functionality

Our MVP is a direct-style compiler for Alacrity:
the supported language fragment closely models what a smart contract does
and how clients interact with it.
Above and below this compiler, we will build many layers of functionality.
These layers can be seen as the stages of a "rocket" that will launch when we have them all,
providing our full secure scaling and interoperability solution.
but even before all layers are available, we will create value for our users and customers,
by providing a service that they can't get from anyone else.
The list of layers below must be seen as extending up and down in time
from our MVP, the direct-style Alacrity compiler.
They are annotated with due dates according to our best plan,
which will be updated as we refine our plan:

  * zero-knowledge proofs for verification of arbitrary Alacrity interactions (end of 2021?)
  * Sufficient models of existing blockchains for interoperability [#110](https://github.com/AlacrisIO/meta/issues/110) (July 2020)
  * Side-chain Markets with MKB [#54](https://github.com/AlacrisIO/meta/issues/54) (June 2020) ⇐ *Beta network that enables a Token event*
  * Operator Markets (May 2020)
  * Plasma Chains (April 2020)
  * Game-semantic enforcement of contractual obligations as logical predicates (March 2020)
  * Logical queries about a monotonic verifiable data structure (February 2020)
  * Alacrity VM in Alacrity [#59](https://github.com/AlacrisIO/meta/issues/59) [#60](https://github.com/AlacrisIO/meta/issues/60) [#131](https://github.com/AlacrisIO/meta/issues/131) (January 2020)
  * Time Compression [#129](https://github.com/AlacrisIO/meta/issues/129) (December 2019)
  * Space Compression [#128](https://github.com/AlacrisIO/meta/issues/128) (November 2019)
  * State Channels [#56](https://github.com/AlacrisIO/meta/issues/56) (October 2019)
  * **Direct-style Alacrity** (September 2019) ⇐ *MVP, base layer from with other layers extend up and down*
  * Auctions for posting messages despite FOMO3D-style block-buying attacks (October 2019)
  * Support for Bitcoin Cash [#112](https://github.com/AlacrisIO/meta/issues/112), Tezos, Cardano, Zilliqa, Hedera, Algorand, etc., as well as Ethereum (first one in November 2019)
  * Operator markets to post messages (?)
  * Operator markets for GAS computation / insurance
  * Multi-datacenter redundancy for robust and secure DApp deployments (April 2020)
  * Admin console and alerts to monitor operators (December 2020)
  * Data analytics to measure and optimize operations from an economic point of view (March 2021)

### Horizontal Breadth of Features

Our system will grow features from the initial MVP along the following multiple dimensions:

  * *Formal Verification of Automatically Derived Properties*:
     * MVP: we statically verify that accounts are balanced at the end of the execution (August 2019)
     * We must document what other things we verify (September 2019)
     * Game-theoretic liveness properties of the DApp:
       if all partake honestly and competently (i.e. if no future failure event),
       the DApp makes progress and completes its intended transactions with suitable mutual profit
       (i.e. future completion event)
       (September 2019)
     * Game-theoretic safety properties of the DApp:
       assuming the blockchain is honest and we can post transactions in time,
       whichever player we are, at any point, whatever other players do,
       we can reach a point where all current transactions are either complete or rolled back,
       and we can get our money out, in finite time and for bounded gas
       (October 2019)
     * Fairness? No, game-theoretic profit properties of the DApp, in terms of
       probabilistic expectations of costs and benefits in each participant's model:
       even accounting for fixed gas cost and fixed loss of opportunity for the capital,
       then, if the user's valuation verifies suitable hypotheses (to make explicit to the user),
       the user will profit from using the DApp if the other users cooperate,
       which they are also interested in doing if *their* valuations verify
       the suitable complementary hypotheses
       (e.g. seller vs buyer has opposite utility functions for the given quantity of cash vs the item sold)
       (May 2020)
     * User-defined assertions, according to a good understandable model for what they mean,
       both in a liveness or a safety context
       (December 2019)

  * *Verification of User Properties*:
     * MVP: we check the validity of all user assertions (August 2019)
     * We must document how exactly we interpret assertions (September 2019)
     * Presumably, our system-enforced properties and user properties should be expressible
       in a common logical framework involving modal logic for the temporal aspect (also epistemic?).
       Explain this framework (January 2020).

  * *Multiple Parties*
     * MVP: we enable the specification of financial interactions between two participants (August 2019)
     * A static finite number of participants in a DApp (September 2019)
     * A dynamic finite number of participants in a DApp (October 2019)
     * An open set of participants in each of many roles in a DApp (March 2020)
     * Formally verify the difference between cooperative flows that can use state channels
       and adversarial flows that must use a censorship-resistant blockchain
       (or side chain *market*, if a single operator can be adversarial) (July 2020)

  * *Non-linearity in Control Flow*:
     * MVP: Choice by one party to default or fail to respond by a timeout (September 2019)
     * While loops and/or recursive tail calls, with user-specified loop invariants
       (September 2019 ?)
     * Choice by one party (or randomness?) between alternatives
       that affect the control flow (September 2019)
     * Race condition between multiple participants,
       typically in a contract where who wins the race ultimately doesn't matter:
       side-chain operators post independent transactions;
       higher bidders override lower bidders;
       a CRDT merges the results; etc.
       (February 2020)
     * Verification that these choices span all possibilities;
       drop checks that are not necessary considering the interest of the actor,
       e.g. if an actor may pick an amount for what they receive, it's their fault
       if they pick less than the maximum allowed, so the amount needs not be explicitly tested
       (September 2019, and maintained as we add more possibilities)

  * *End-Point Projection Backends*:
     * MVP: we automatically generate JavaScript client, Solidity contract and Z3 proof requirements
       from a single specification (August 2019)
     * We will also generate code for Bitcoin Cash, Tezos, Cardano, Zilliqa, Hedera, Algorand
       (first one in November 2019)
     * At some point, we will generate direct optimized EVM bytecode instead of Solidity;
       first with limitations corresponding to stack depth limits in bytecode instructions;
       later with spilling strategies if data doesn't fit in the directly accessible stack
       (August 2020)

  * *Gas cost optimization*
     * MVP: Use user-level merklization instead of system storage to save on gas
       (August 2019)
     * Generate EVM code that shortcuts the Solidity ABIs, instead of using Solidity
       (December 2019)
     * Have estimates show that our fees are lower than our savings
       (April 2020)

  * *DApp Upgrade*
     * Code migration within one contract by participant/operator agreement (August 2020)
     * Support for centralized upgradable contracts (September 2020)
     * Asset migration from one contract to the next (October 2020)
     * Support for centralized upgradable contracts with grace period for migration (October 2020)

  * *Type System*
     * ADTs
     * many integer types
     * string/bytes types?
     * abstract asset types
     * higher-order module
     * implicits or typeclasses

  * *Tooling*
     * property-based
     * debug
     * editor
     * better err msg
     * better ui integration
     * better monitoring ...
     * better macros for haskell or types for racket


## Chronological Details

### Status pre-MVP: Having a Compiler At All

That's where we are: we can compile some language to Solidity and JavaScript.

The language can barely compile the "Rock, Paper, Scissors" example,
though it is missing a crucial feature before it is production-ready (support for timeouts),
as well as cleanups, simplifications and documentation to make it usable outside our team.

### TO BE CONTINUED...

For each task, list tasks it directly depends on, tasks that directly depend on it,
a time estimate, an estimated delivery date, and a detailed description...
or just create an issue for each of them, and discuss the issue there and reference it here.
