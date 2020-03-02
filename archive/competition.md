# Competition for Alacrity

There are many competitors writing DSLs to write smart contracts—but already,
these languages only handle the smart-contract, and not the entire DApp.
Any bug in the client or server code, or any discrepancy between that code and the contract,
and the users’ assets will disappear as surely as if the bug were in the contract.
However few competitor seem to even be aware of the issue;
a couple of these languages allow for a little bit more integration
with a language to build client code than others (Plutus, Michelson),
but it still remains very low-level and error-prone,
checking data types of messages but not the state of the computation.

Our DSL, Alacrity, can be used to specify and formally verify
contract and client code at the same time, and
make sure they are generated in a coherent way,
using End-Point Projection, a technique unique to us.
A few of these languages are well-suited to formal verification (e.g. Scilla).
Most aren’t, worse or verify the wrong things (Simplicity).
Some companies like CertiK or Runtime Verification
have built great tools and framework to verify smart contracts.
However, once again they tend to only verify the smart contract, and not the entire DApp.
They also seem not to take into account the adversarial aspect of the blockchain
in the properties they verify and thus miss key safety properties.
No one but us verifies both client and contract.

Also, almost all of these smart contract languages are chain-specific.
One language, DAML, is portable, but is designed for permissioned blockchains;
it is excellent on the financial contract side,
but despite being so much better than all other solutions for permissioned blockchains,
it is still not at all suited for the adversarial environment of permissionless blockchains
(no doubt it could become so if they learn from us,
just like we can improve our support for financial transactions by learning from them).

Some companies claim to be building an “Operating System”,
but what they mean is very different from us.
For instance, Zeppelin OS does an amazing job at building tools on the Ethereum ecosystem;
but their vision has no cross-chain portability;
they write contract and client code separately in Solidity and Javascript;
they do not offer deployment services.

* [Scilla](https://scilla-lang.org/)
  is the smart contract language for Zilliqa;
  it features total functions with eventual message sends.
  There is a version embedded in Coq, but it is not used in production.
  On the other hand, they have a good IDE for developers, [savant](https://savant-ide.zilliqa.com/).

* [Plutus](https://cardanodocs.com/technical/plutus/introduction/)
  is the smart contract language for Cardano.
  It is a richly statically typed functional language developed by IOHK.
  It can use Haskell as a metalanguage to write applications
  in which most of the application is run off-chain but some parts are run in a contract.

* [Marlowe](https://iohk.io/blog/marlowe-financial-contracts-on-blockchain/)
  is another domain-specific language for smart contract language embedded in Plutus.
  It has relevant primitives, but (a) it can only deal with finitary code with
  a fixed number of inputs and outputs, and (b) it is somewhat insufficiently typed
  (no labels of types to distinguish amounts and timeouts, for ensuring resources are linear, etc.).
  There is a "simulation environment" that allows interpreted client-style actions,
  which is done by dynamic evaluation without type protection either.

* [Pact](https://github.com/kadena-io/pact)
  is the smart contract language for Kadena.
  It has a Lisp syntax but Haskell-like types (its metalanguage is Haskell);
  it features builtin key management, and each contract having a small relational database
  instead of some ad-hoc low-level storage.

* [RhoLang](https://github.com/rchain/rchain/tree/master/rholang)
  is the smart contract language for R-Chain.
  It is a distributed calculus (distant descendant of the Pi-calculus),
  with some reflective features and session typing.

* [Solidity](https://solidity.readthedocs.io/)
  is the main smart contract language for Ethereum.
  Its design is not very principled, but it has evolved a lot of practical features
  based on the experience of the largest user base so far.

* [Simplicity](https://blockstream.com/simplicity.pdf)
  is the main smart contract language for Blockstream's Liquid Network.
  It is a functional language that can express finitary computations only,
  rebuilding everything from bits and pairs.

* [Michelson](https://www.michelson-lang.com/)
  is the native smart contract language for Tezos.
  It is a simple typed functional combinators virtual machine.

* [Liquidity](http://www.liquidity-lang.org/)
  is a smart contract language for Tezos.
  It brings a nice functional language syntax in the style of OCaml on top of Michelson,
  but is semantically the same.

* [DAML](https://daml.com/) is the smart contract language of Digital Asset.
  It looks like a great language for private permissioned blockchains,
  where trust is enforced socially, with a good data model and a good transaction model;
  its documentation bridges the gap between financial accounting and programming,
  and makes for good DSL documentation;
  the language is proudly a variant of Haskell, and seems all around well designed;
  However, DAML doesn't seem particularly suited to writing
  contracts for public permissionless blockchains: no good obvious cost model,
  use of wall clock time instead of block depth,
  a privacy model that seems to assume a trusted third party to enforce it, etc.,
  no notion of separate computation for each participant vs for the consensus.
  In the end, it's yet another language for contracts only, not for entire DApps.
  It looks like you'd have to use continuation-passing style to represent
  any complex interaction.
  Interestingly, we may use some of their [examples](https://github.com/digital-asset/ex-models)
  as benchmarks to compare against.

* [Dovetail](https://community.tibco.com/wiki/project-dovetail) by TIBCO
  tries to abstract smart contract creation using a graphical language.
  Its backends are all for permissioned blockchains, however.

* HyperLedger's [AccordProject](https://docs.accordproject.org/) is meant
  for partially automating legal contracts;
  as such it supposes there is an ultimate authority outside the blockchain
  and is utterly inappropriate for a permissionless blockchain.
  The business logic part, Ergo, is somewhat higher-level than solidity,
  but ultimately still an imperative procedural language.

* [Corda](https://docs.corda.net/tutorial-contract.html) is for permissioned blockchains only.
  Its contracts are written in Kotlin (or Java, or any other JVM language).
  This is just not applicable.

* [Smart Contracts History](https://infominer.id/bitcoin-history/smart-contracts/#smart-contract-history)
  is a resource with lots of information about the design of languages to specify "smart contracts",
  not all of them backed by a blockchain (indeed, going back to the 1960s).

* [Cosmos](https://cosmos.network/developers) offers
  the [ABCI](https://tendermint.com/docs/spec/abci/),
  a way to build your own Tendermint blockchain with its own extensions written in Go
  for application-specific consensual computations.
  The biggest problem with this approach is economical:
  to trust such an application, you need to trust a network of staked validators who all run this code;
  this is not a recipe for either decentralization or success;
  instead of enjoying a large, heavily capitalized validation network,
  you have a small one that can be cheaply corrupted, taken over;
  there are high infrastructure costs to running a validator node running across time,
  and keeping it secure in an adversarial environment with rapid bitrot,
  and this balkanization of validator networks is not capital-efficient.
  Technically, Go is a great language to write servers, but a dangerous language
  in which to write consensual computations, and not at all a good language to write DApps
  that have both consensual and private components that must work perfectly in unison.
  Importantly, the Go language and its existing toolchain aren't designed to ensure that
  a Go program will behave in a deterministic way without side-effect;
  that is why each program much be socially vouched for, and the validators for each application
  must remain separate from the others: if arbitrary application code were to run on the same server
  using this language and toolchain, malicious applications could disrupt other applications
  and steal resources. Thus, the economic limitation of the Cosmos architecture above
  is a direct consequence of the technical limitation of its application language Go.

* [Statebox](https://statebox.org/) uses Category Theory to model computations as Petri Nets.
  Their approach seems quite powerful, and particularly fit to model the bounded processes
  involved in most ordinary transactions.
  They don't seem to currently consider many of the concerns weren't interested in,
  and many interesting DApps are not expressible within that computational model,
  but the model could probably be extended to address these issues if the authors were to care.
  A serious competitor, if they are not too enamored with their current unextended model.

* [Enigma](https://enigma.co/discovery-documentation/) like us uses the notion of multiparty computation, and uses Intel SGX to run the private encrypted computations in a market-selected trusted worker. The final outcome of the computation is necessarily more expensive than if done privately on a state channel; so it's only interesting if you can trust the platform to run computations privately, that would be quite expensive if done with homomorphic encryption, that you don't care if the NSA sees. (They also don't have End-Point Projection.) But if your application fits their slightly weird security model, it could be somewhat interesting.

* [Taxa](https://taxa.network/) — also uses a TEE; similar to Enigma?

* [cashscript](https://github.com/Bitcoin-com/cashscript) allows you to metaprogram
  bitcoin cash scripts from typescript.
  This is indeed is the right thing to do if you don't know yet have
  a higher-level programming model for contracts (or covenants, or whatever they are called):
  it gives you latitude to experiment and build things up, in the same language that you use
  in client code.
  But Alacrity provides exactly the missing higher-level programming model.

* [DeepSEA/Blockchain](https://certik.org/deepsea_blockchain.html)
  is a general-purpose language for writing code at the level of abstraction of C,
  but with proofs of correctness using separation logic in Coq.
  You can do anything with it, including proving smart contract,
  but you're going to have to have a large team of PhD-level logicians do it the hard way;
  and then you'll have to separately prove your client and server code,
  and prove that they work together.
  The approach just doesn't scale.
  But maybe if someone actually uses it, they will build new abstractions on top of it,
  that are more usable.

* [Actus](https://www.actusfrf.org/)

* [Common Accord](http://www.commonaccord.org/)

* Works by [Fritz Henglein](http://hjemmesider.diku.dk/~henglein/).

* See what O(1)labs has, etc.

* [Abridged](https://abridged.github.io/splash/)

* [Agoric](https://agoric.com) is Mark Miller's take on a smart contract language:
  [E-rights](https://erights.org/) over ocaps over vats over (blockchain, solo, quorum, etc.).
  While Agoric's approach is similar to Alacrity's in some ways (abstraction, layering),
  in other ways, it is very different in ways that inspire me doubt:
  (1) Agoric is object-oriented when Alacrity being functional,
  which will make it harder to prove correctness of programs;
  (2) Agoric requires three complex layers of abstraction *before* they can even start writing DApps,
  whereas Alacrity is useful at each simple layer of abstraction, for a growing set of DApps;
  (3) Agoric is retrofitting a previous model (E) on top of blockchains,
  rather than trying to seek what the essence of either blockchains or DApps is;
  (4) their vats provide isolation against process or network errors
  (and quorum vats provide much needed redundancy
  that is on Alacris's roadmap but that we don't handle yet),
  but they have no builtin support for incentive alignment in an adversarial environment.
  In the end, instead of being able to provide a language that minimizes the complexity of writing DApps,
  they end up introducing a complex, arbitrary language (E, just with JavaScript syntax)
  in between the contracts and the blockchain, with its impedance mismatch, with no obvious gain.
  The vat layer is very relevant; the four dimensions of rights in erights are interesting,
  though not always relevant.

* [Archetype](https://docs.archetype-lang.org/) looks very interesting.
  Currently Tezos-focused, but the approach seems to be general enough
  that it could be easily ported to other platforms.
  Like Alacrity, Archetype features extraction to several outputs;
  it doesn't currently seem to output client and server, but it probably could.
  Archetype seems competitive with the usual crowd of "formally verified" languages,
  but doesn't seem to handle the adversarial aspect of interactions, that is central to Alacrity.

* [AxLang](https://axoni.com/axlang/) by axoni

* [CX](https://www.skycoin.com/cx/)

* [vyper](https://github.com/ethereum/vyper)

* [radicle](http://radicle.xyz/)

* [Mainframe OS](https://docs.mainframeos.com/docs/faq)

* [Rell](https://chromapolis.com/preview/index.html) is the SQL-like relational blockchain language used by the Chromia blockchain. What is the cost model for it??? They don't seem to understand what a DApp is.

* [TVM](https://github.com/ton-blockchain/ton/blob/master/doc/tvm.tex) TON Virtual Machine

* [Second State](https://docs.secondstate.io/)

* [Motoko](https://sdk.dfinity.org/language-guide/index.html)

* Algorand: [Smart Contracts](https://developer.algorand.org/docs/asc), [TEAL](https://developer.algorand.org/docs/teal). TEAL looks like a much nicer variant of Bitcoin script: using uint64 instead of crazy signed 32 bits without MININT of BTC.

* Parity's [ink!](https://github.com/paritytech/ink/) for smart contracts on Substrate / Polka Dot (Rust eDSL compiled to Wasm)

* [Chialisp](https://www.chia.net/2019/11/27/chialisp.en.html), https://twitter.com/bramcohen/status/1202700142370902016 -- the Lisp is embedded in Python, and not super Lispy.

* [Lexon](http://demo.lexon.tech/apps/editor/)

* ??? Is there anything useful in this article? [Impossibility of the Obama-Trump contract](https://eprint.iacr.org/2018/252.pdf) - looks like they assume a successful 51% attack, which makes the entire article moot.

* https://www.cs.tau.ac.il/~odedp/modularity-for-decidability.pdf
* https://www.csail.mit.edu/event/type-system-resource-bounds-type-preserving-compilation-and-its-application-ethereum-smart
* https://arxiv.org/pdf/1812.08829.pdf
* https://eprint.iacr.org/2018/416.pdf

* https://www.aztecprotocol.com/
* https://www.theblockcrypto.com/post/52004/look-there-where-emerging-use-cases-for-zkps-in-2019

