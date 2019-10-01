[//]: # (title: Overview)

## Overall Structure of an Alacrity Program

As in the example program [rps.ala](../examples/rps-auto/rps/rps.ala),
the structure of an alacrity program is:

```alacrity
#lang alacrity/exe

...type and function declarations...

...participant declarations...

...main function...
```

The first line is a header, intended for compatibility with a past and future implementation in Racket.

After it, you may have a series of [type](type.md) and [function](function.md) declarations.

Then, declare the participants. They are typically called `A` and `B` or `Alice` and `Bob`,
but really can be any name. Each participant starts with some private parameters,
declared as a (possibly empty) comma-separated list of variable declarations.
If you want other participants to know these parameters and agree on them,
you have to explicitly `publish!` them afterwards.
The current syntax is `parameterType parameterName`,
but in the near future may be changed to `parameterName : parameterType`.
For example, in the following interaction,
Bob is selling a secret to Alice for an agreed upon amount,
after having committed

```alacrity
participant Alice {
  uint256 bobsDigest,
  uint256 price
}

participant Bob {
  uint256 bobsSecret
}
```

For now, we only safely support interactions with two participants.
In the near future, we will add support for interactions with more than two participants,
and with a variable set of participants.

Finally, declare the `main` body of the code:

```alacrity
main {
  @Alice publish! bobsDigest, price w/ price;
  commit;

  @Bob publish! secret w/ 0;
  require! digest(bobsSecret) == bobsDigest;
  transfer! Bob <- price;
  commit;

  0
}
```

In this body of code, some actions are taken by Alice (and are prefixed with `@Alice`),
some actions are taken by Bob (and are prefixed with `@Bob`), and
some actions are verified consensually: they are not only computed by both parties,
but also validated via their smart contract.
Actions taken by Alice notably include `publish!`ing data and sending money with it (the `w/` clause),
whereas Bob publishes data without sending money (`w/ 0`).
After action by one party and corresponding consensual actions,
a `commit` statement ensures that the computation reaches a consensual state
before actions may be taken by the next party.
At the end of the interaction, Bob has released his secret to Alice,
and Alice has paid the agreed upon price for it.
If Alice never puts down the money, nothing happen except Bob leaving the interaction after waiting.
If Alice puts down her money but Bob never reveals his secret,
then Bob times out and Alice gets her money back.

In a further future, we will introduce nesting and composition of transactions.



Primitive                             | Example
--------------------------------------|--------------------------------
Integers                              |  `23`, `-23`
Integer Addition                      |  `23 + 1`
Integer Division/Multiplication       |  `2 / 23 * 1`
Comparison                            |  `>`, `<`, `>=`, `=<`
Boolean operations                    |  `!`, `&&`, <code>&#124;&#124;</code>
Block Comments                        |  `/* Comment here */`
Line Comments                         |  `// Comment from here to the end of this line`
