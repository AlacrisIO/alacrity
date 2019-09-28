[//]: # (title: What & Why)

## What Is Alacrity?

Alacrity is a domain-specific language for trustworthy decentralized applications.
We use a cascading style of verification to help establish trust by users,
by allowing an application's trusted code base to be very small,
while enabling the application to be deployed in a wide variety of contexts.
We will verify guarantees about program execution, compilation, correctness, security, and efficiency.
It uses a suite of verification methods, like
type theory, theorem proving, model checking, the strand spaces method, and
dynamical system simulation to simplify DApps development.

By writing your next DApp in Alacrity, you can guarantee, that
  - the application is correct on both smart contract side and client side and
  - the client interacts with the smart contract correctly.

Alacrity strives to reuse the familiar syntax of JavaScript.
Where types and other functional programming features are used, we strive to reuse
the syntactic extensions to JavaScript syntax that were introduced by ReasonML.
In some places we introduce our own syntactic and semantic extensions.

Even though we compile to JavaScript for the client side DApps code,
we do not allow any escape hatches in the language.
The reason for being strict is to ensure DApps correctness,
which cannot be ensured if the model is broken through the escape hatch.

We are still growing our language, so if you find yourself in a situation
where you really need that escape hatch, drop us a line
and we'll see how your specific use case can be supported through the existing primitives
or we'll prioritize adding a new construct into the language
in a way that does not break any stated properties of the langugage.
