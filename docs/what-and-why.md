[//]: # (title: What & Why)

## What Is Alacrity?

Alacrity is a domain-specific language for trustworthy decentralized applications (DApps).
It makes it significantly easier to develop DApps, by:

   - Allowing you to write code at a level of abstraction closer to your domain of expertise,
     so you don't have to deal with the fragile details of Blockchain security.

   - Generating not just a smart contract (in Solidity), but also
     matching client and server code (in JavaScript).

   - Generating and verifying a logical model for your DApp,
     so you can rule out critical mistakes and vulnerabilities in your code.


## The Syntax and Semantics of Alacrity

Alacrity is a dialect of JavaScript,
restricted to its pure functional fragment,
then extended with primitives for multiparty computations with an arbiter.

For the sake of familiarity, Alacrity strives to reuse the syntax of JavaScript.
Where it does, it strives to also preserve the semantics of JavaScript,
so that no bad surprise shall arise from the discrepancy.
The subset of JavaScript we use is the pure functional fragment:
you cannot re-assign an existing variable,
and you can only use `const` and neither `let` nor `var`.

For the sake of safety, Alacrity enforces a strict strong type discipline on programs.
Where this requires extensions to JavaScript,
we strive to follow the syntax and semantics of ReasonML.
Alacrity is currently restricted to simple types and first-order functions without recursion;
in the future, we will add parametric and ad-hoc polymorphism,
higher-order functions, modules and kinds, refinement types, etc.

Finally, we introduce new primitives specific to the domain of multiparty computations
where participants are held accountable by an arbiter,
which in practice will be embodied by a smart blockchain on a blockchain:

   - Currently, programs must specially declare a set of `participant`s and a `main {}` block.
     In the future, these may be replaced by suitably annotated functions
     that can express composable interactions.

   - Programs may contain domain-specific actions such as `publish!`
     (making some data public, done by a participant, possibly with a deposit to the contract),
     or `transfer!` (transfering money out of the contract, done by the arbiter).

   - Program statements including variable declarations, assertions and other actions,
     may be annotated by the name of the participant on whose computer the statement takes place,
     as in `@Alice const nonce = random();`.

Even though we compile to JavaScript for the client side DApps code,
we do not allow any escape hatches in the language.
The reason for being strict is to ensure DApps correctness,
which cannot be ensured if the model is broken through the escape hatch.

We are still growing our language, so if you find yourself in a situation
where you really need that escape hatch, drop us a line
and we'll see how your specific use case can be supported through the existing primitives
or we'll prioritize adding a new construct into the language
in a way that does not break any stated properties of the language.



### The Power of Simplicity

We strive to establish trust through simplicity:

  - By tailoring the language to the intended applications,
    we can keep the code simpler and shorter, when other languages
    force *extrinsic complexity* into programs to overcome the *impedance mismatch*
    between what the language makes easy and what the application needs.

  - Simpler and shorter isn't just easier, faster and cheaper to write:
    it's also easier, faster and cheaper to audit,
    either manually by humans or automatically using formal methods.
    This means many new DApps become feasible that were not tractable before,
    and in the end can be trusted to be safe.


### Formal Methods

We use a cascading style of verification to help establish trust by users,
by allowing an application's trusted code base to be very small,
while enabling the application to be deployed in a wide variety of contexts.
We will verify guarantees about program execution, compilation, correctness, security, and efficiency.
It uses a suite of verification methods, like
type theory, theorem proving, model checking, the strand spaces method, and
dynamical system simulation to simplify DApps development.
