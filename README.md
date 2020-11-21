# Alacrity

Alacrity is a domain-specific language for trustworthy decentralized
applications. We use a cascading style of verification to
help establish trust by users, by allowing an application's
trusted code base to be very small,
while enabling the application to be deployed in a wide variety of contexts.
We will verify guarantees about program execution, compilation, correctness,
security, and efficiency. It uses a suite of verification methods,
like type theory, theorem proving, model checking, the strand spaces
method, and dynamical system simulation.

### Copyright and License

Copyright 2019 Alacris, Ltd. All rights reserved.
Alacrity is distributed under the Apache License, version 2.0. See the file [LICENSE](LICENSE).

### Prerequisites

- [Stack](https://docs.haskellstack.org/en/stable/README/)
- [z3](https://github.com/Z3Prover/z3)
- [solc](https://github.com/ethereum/solidity)
- [geth](https://geth.ethereum.org/)

### Being worked on

You can watch on our [github repository](https://github.com/AlacrisIO/alacrity)
what we are currently working on.

As of June 2019, see notably the [compiler we are writing in Haskell](hs/alacrity/)
and the [Rock, Papers, Scissors demo](examples/rps-auto) we are using as a benchmark application.


### Tips for Developers

If you're using Emacs:
```
(add-to-list 'auto-mode-alist '("\\.mjs$" . javascript-mode))
(add-to-list 'auto-mode-alist '("\\.ala$" . javascript-mode))
```
