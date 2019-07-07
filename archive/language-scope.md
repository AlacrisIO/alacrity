# Design for the DSL

[[_TOC_]]

## Overview of language features

We aim at growing a language that allows developers to write
distributed multi-party applications that can manage and trade assets
in a way that is safe by construction while minimizing trust requirements between parties.

The general approach to growing the language will be as follows:

  * The language starts with the simplest constructs that make analyzing correctness possible
    yet that allow to develop non-trivial applications.

  * At some point, the language grows into a general-purpose distributed calculus:
    actors can send messages to other actors and receive messages from them.
    In this arbitrary functional programming can be expressed,
    either on top of channel communication using CPS,
    or more directly as more primitive functions.

  * However, for ease of reasoning, our DSL will explicitly various levels of language
    that restrict the expressiveness of programs to stay within subsets that are tractable
    to automatically analyze and prove correct:
    finitary computations, primitive recursive functions,
    computations that can use existing channels but not create new ones,
    computations that satisfy some type restriction, including effect types or session types,
    etc.

To specifically support blockchain applications, the language will have
the following features, extensions and restrictions:

  * Game Theory will model whether the interests of players are indeed aligned,
    and whether the game is safe to play for each player.

  * A notion of location, cell or membrane, makes it possible to group computational actors
    and associate them with the game players that control them.

  * Access control restricts what code may send and/or receive message
    on any given channel -- usually based on the name of the channel matching a digest based on
    location or identity of the player who controls the channel, or the content of the code.

  * The notion of location may also be used to reflect how
    messages to some abstract entity are actually implemented
    in terms of lower-level protocols involving messages to some more concrete entities.

  * Some variant of modal logic subsuming both epistemic logic and temporal logic will express
    the knowledge distributed in the system, i.e. "actor A knew about event E at time T".

  * Cryptographic primitives implement the above epistemic logic, using disgests and signatures.

  * The language distinguishes its finitary fragment, which can be used to generate zkSNARKs
    and other similar circuits for zero-knowledge proofs, as well as public interactive proofs.

  * The language also distinguishes the fragment that extends the previous with full recursion,
    from which interactive proofs of arbitrary depth can be extracted, or finitary fragments can
    be obtained by bounding the recursion depth (and possibly by using recursive zkSNARKs).

  * The language has a notion of some (all?) entities being marshalable into strings of bits (bytes?),
    and hence messageable, digestible, signable, usable in distributed proofs, etc.

  * The language has a verifiable fragment, which has the full recursion and the encryption primitives.

  * There is a verifier for the verifiable fragment written in the verifiable fragment itself,
    which allows for side-chains to include arbitrary contracts.
    Gas limitations on the main chain are reflected into limitations to how big verifiable computation
    steps can be on the side-chain.

  * The language has a cost model that allows to charge on-chain computations
    when it is used as the native language of a blockchain or side-chain.
    When compiling to another lower-level smart contract virtual machine,
    a precise cost can be predicted for each transaction.

### Syntax (V1, V2)

For V1, and until the design settles, we will use Lisp-style S-expression as our minimal syntax.
Thus, we are free to experiment and make rapid changes
without the burden of having to maintain a heavy syntactic toolset
and rewrite all programs in subtle ways.

Once the design is stable, we will change the syntax in V2 to be Javascript-like,
using the syntax of Facebook Flow and/or ReasonML where applicable for the typing.

### Basic Computation Infrastructure (V1)

To do any computations, we need the basic ability to define and compose functions.
For that, we'll reuse the lambda calculus,
whether as inherited from Coq, OCaml, Racket, or reimplemented as a new VM
(e.g. for reflection purposes, see below).
```
(λ _var_ _body)
_var_
(_function_ _argument_)
```

Other primitives like `define`, `let`, `let*`, `letrec`, `letrec*`,
are syntactic sugar on top of it.

Some type systems may be used to restrict what functions can be used in various context.
Arrows `x -> y` will indicate pure total functions, and
arrows written `x ==> y` will indicate functions from `x` to `y` with side-effects
(such as communicating with other network nodes, failure, partiality, etc.).

Tuples (cartesian product) and sums (coproduct) may also be considered primitive constructs
instead of building them the hard way on top of the lambda-calculus.
The minimum primitives for V1 might be pairs:
```
`*` : Type -> Type -> Type
`,` : {a, b: Type} a -> b -> a * b
fst : {a, b: Type} a * b -> a
snd : {a, b: Type} a * b -> b
```

In a more advanced variant (V1.5), structures with an arbitrary number of named fields,
or sums with enumerated cases, are available.
Also, code modules can be represented as such structures.

TODO: figure out how type declarations inside modules are typed.
OCaml style? Coq style with dependent types? Something in-between?
An untyped second-class entity that is flattened out before typing?

When defining a first-class virtual machine for the purpose of reflective verification or implementation,
we shall reduce everything to [SKI combinators](https://en.wikipedia.org/wiki/SKI_combinator_calculus),
[BCKW combinators](https://en.wikipedia.org/wiki/B,_C,_K,_W_system), or some similar variant.


### Distributed Calculus (V1)

The first set of features is a distributed calculus, possibly similar to
[rholang](https://developer.rchain.coop/assets/rholang-spec-0.2.pdf).

Communication:
```
send : out_channel * data ==> unit
receive : in_channel ==> data
```

Private channels:
```
new_channel : unit ==> in_channel * out_channel
```
Separating the send and receive capabilities allows to easily
expose only the send capability for public channels (see below).
Note how the two presentations, single name or separate capabilities, are mutually expressible:
just combine the two capabilities in a pair, or use send and receive functions to wrap a channel name.

Public channel:
```
initialize_public_channel : code ==> out_channel
public_channel : digest -> out_channel
```

`initialize_public_channel` takes some code representing a function of type
`in_channel * out_channel ==> unit`, computes the digest for that code;
if the public channel corresponding to that digest was already registered, it aborts,
but it is wasn't yet, it spawns a thread in which it calls the function
with the input and output channels for that public channel.
Anyone can send messages to the channel, but only the matching code can reply to it,
which is instantiated only once.

Computations can be spawned in parallel:
```
par : (a ==> b) * (a' ==> b') -> (a * a') ==> (b * b')
```

TODO: We want ways to restrict creation of channels, so as to simplify reasoning.
Maybe an effect system can help?
A more first-order approach, with a fixed (per-program) set of channels between actors?


### Game Theory (V2)

The computation is divided between "parties", that each control some of the actors.
Each party has its own "preferences"
that orders the outcomes of computations,
often but not necessarily according to some explicit "utility function".
A "rational" party will take the actions that lead to preferred outcomes,
maximizing its utility function if any.

The basic theorems about computations will be safety and liveness theorems.
The liveness theorem for the overall computation says
assuming the parties have and act according to their declared preferences,
that the computation will make progress, such that
all participants will reach a strictly preferred state.
The safety theorem for one party will ensure that by playing the game with the provided strategy,
the party won't be worse off at the end than it is in the beginning,
whatever the other parties do or fail to do,
even if they fail to behave according to declared preferences and strategy.

The outcomes are associated to *some* stable states;
there may be intermediate states in which the outcomes are indeterminate,
that may depend on the actions of others.
In a good game, the intermediate states may be reduced away up to a player doing his part,
and the game may be seen as implementing a simpler higher-level game
(see my thesis on implementation).
Furthermore, what is "stable" depends on the player;
players may know their outcomes asynchronously.
The reduction used may differ for each player's safety theorem as well as for
the overall system's liveness theorem.

Game Theory Primitives:
```
(defrole _role_ _parent_roles_ _arity_constraint_ _utility_function_)
(defactor _actor_ _roles_ _resources_)
(payoff _actor_ _utility_)
(transfer _actor_ _recipient_ _asset_  _amount_)
(define-exchange-rate _asset1_ _asset2_ _ratio_)
(define-interaction ((_actor_ _roles_) ...) body ...)
```

A same role may be played by several actors.
An actor may play several roles (and/or one role, but roles have multiple inheritance).
The role defines which parts of which interactions an actor who plays the role may take.
An interaction may involve several agents with the same role, but will be symmetric in those roles,
modulo the fact that per-role-instance parameters may differ.

In the most general case, preferences are ordinal:
only matters the preferred order between the various available options;
the order need not even be a total order, but may even be a partial order.
In the simplest case, preferences are cardinal:
each user has his reference asset and
his own conversion rate between each asset and that reference asset,
and his utility is a simple number, sum total of the value of his assets at given rate.
In between, a player's utility may have to be modeled as Pareto-improvement
along multiple incommensurable axes, one per asset.

For artificial games, simulated agents, etc.,
explicit payoff definitions can specify utility matrices.
But the usual way of tracking utility is by explicitly recognizing asset transfers as such.
Exchange ratio between assets can also be represented as intervals of certainty
for use in computing collaterals.

### Location (V1)

Location can be specified.
```
(at _location_ _body_ ...)
```

When instantiating the code for one location, you must provide the parameters
for that location and that location only.

By default, it's just a label.

In V2, we may attach macros to specific labels?
Thus, code at a location can be processed by a specific compiler
handling some additional aspects in a specific way.
For instance, "sending a message to the MKB" would be rewritten into
using the MKB protocol, sending messages in parallel to hundreds of registrars, etc.,
which might be "as easy as" sending the message to a "runtime reflection proxy"
that handles the protocol.

### Refinement (V1)

Refinement allows to specify how one more complex lower-level program
implements another simpler higher-level program, in a correct way.
Refinement is an important way that we can abstract away the complexity of communication protocols.
Complex interactions with a low-level network can be summarized as
simpler interactions between higher-level abstractions thereof.

Thus, communicating with the main-chain to achieve consensus,
communicating with the MKB, communicating with other validators for a side-chain contract,
with other redundant trusted data centers within an application or to other applications, etc.,
can be summarized as sending a single message to an abstract entity standing for
the network of servers that implement and validate
the consensus, the MKB, the side-chain, the application, etc.
The high-level interaction can offer a simple, understandable model
of the protocol in which the users are participating,
at level of abstraction that they care about,
stripped of distracting low-level details.
At the same time, the lower-level steps that implement these abstractions can be discussed;
thus, when there are many implementation strategies with various tradeoffs,
programmers can specify which strategy they want to use,
that they anticipate makes most sense in practice for the users.

Primitives:
```
(define-refinement ...)
```

`define-refinement` links two programs
(contained in some first-class notion of module or unit, to be identified),
such that invocations to the abstract program will actually be implemented
as invocations of the concrete one.
Users who don't need to deal with the details of the concrete program
will interact with the abstract program.

### Refinement Correctness (V2)

When declaring a refinement, the program is only correct
if the concrete program indeed correctly implements the abstract program.
When proving the program correct in a proof assistant like Coq,
the refinement generates according proof obligations.
For a refinement to be correct,
there must exist a correspondance between the concrete program
and the abstract program it refines.
Furthermore, this correspondance must satisfy various specific properties:
it must be an *implementation*, and it must preserve *safety* for each participant,
and *liveness* for the overall computation.

An *implementation* is a total function from a subset of the states of the concrete program
to the states of the abstract program.
A concrete is said to be observable if it has a corresponding abstract state,
that is then said to be its interpretation.
The implementation is *sound* (the most basic correctness property)
if it preserves preserves transitions and their composition:
any concrete transition between two observable concrete states
must have a corresponding abstract transition, and
the correspondance between transitions preserve composition.
In terms of Category Theory, the interpretation correspondance is a functor
from the full subcategory of observable states among concrete states to the abstract states.

*Game Theoretic Liveness*: if everyone follows a strategy that maximizes their utility as specified,
then the concrete game will indeed make abstract progress.

*Game Theoretic Safety*: for each actor, if the actor plays his utility maximizing strategy,
then, whatever other people may do, he will reach a stable concrete state corresponding
to an abstract state in which his personal utility is defined (not necessarily that of other actors)
and though he may fail to have won anything, he did not lose more than some fixed gas plus time wasted.

An abstract transition that corresponds to sending one abstract message
can correspond a protocol involving thousands of concrete messages and concrete transitions,
with complex remedial protocols if any party fails,
including interactive proofs that involve long challenge periods.
Therefore the worst-case latency for progress in the abstract computation can therefore be quite large,
even though it remains finite, assuming the overall system remains within parameters.

When writing a refinement, hypotheses about the environment may be added or refined.
Whatever hypotheses are made at each level of abstraction need to be made clearly visible
to the security auditors who have to decide whether they make sense
and apply to the real-life situations in which the programs will be deployed.

### Transactions (V1, V2)

Transactions ensure that either all of a set of computations successfully succeed, or none of them.
When refining transactions, the liveness property supposes all participants are honest,
and the safety property ensure success or rollback per-user, i.e. for each user,
his assets either reach the new state or remain at the same state.
Moreover, the safety property may be up to:
(1) some bounded amount of GAS to be paid, and
(2) the user being paid damage compensation from some other party's collateral
    that they prefer to some lost asset
    (with a phantom transaction to virtually recover the lost asset from the compensation payment?)

Primitives:
```
(with-transaction (options ...) body ...)
```

The options to `with-transaction` help specify what implementation strategies can be used
to implement the transaction:
acceptable delay to confirmation; acceptable compensation payment; acceptable validation networks.

### Automated Refinement (V1, V2)

One way to achieve refinement is by automatically transforming the abstract program
through a metaprogram (macro or compiler) that yields the refined concrete program.
We want this ability in V1 or V1.5, so users can crucially simplify application development:
they can focus on the high-level aspect of their programs, and let the metaprograms do the rest.

Thus, we may define a "reflective" model that automatically captures abstract messages
that cross some location boundary and implements them in terms of a complex concrete protocol,
that may involve broadcasts, multicasts, acknowledgement from multiple targets, resends,
many lamport clocks and timeouts, a changing set of communication targets, etc.
Interacting with "the" application may involve partaking in a PAXOS or RAFT protocol
to determine "the" current leader for the application among participating servers.
Communicating with anonymized participants may involve Onion Routing or Packet Mixing. etc.

For V2 or V3, we want correctness proofs for the metapprograms.
The proof obligations for the metaprogram's output are the same as
if the resulting refinement had been written manually;
but these proofs (or large swaths thereof) can be uniformly provided
using the metaprogram's proof of correctness.
This suggests we use a deep embedding in Coq.

### Access Control (V2)

We can express access control as send and receive capabilities,
that can be wrapped in functional filters.

Maybe dynamic access to e.g. current location or identity,
can help write some filters, in a refinement of a computation without such access?
Or just some static proof obligations that access constraints won't be violated?
Or some Aspect-Oriented Programming style orthogonal specification of access controls?
One that can be orthogonally refined?

We possibly don't need that for V1, but it will be soon necessary to specify which actor
has access to which information, which keys, etc.

### Modal Logic (V2)

The logic comes with a parametrized modal operator:

```
(knows-before _actor_ _time_ _event_)
```

The modal operator indicates that a given actor has seen the event before the given time.
By matching anonymous variables, the modal operator can omit what actor or what time is concerned.

Modal logic can be modeled in Coq by having predicates operate on a history of execution
as an additional implicit argument
(being a coinductive type for a stream of all events past and future, plus present cursor?).

Time is a tricky thing to represent, as there are many mutually inconsistent clocks.
There is the "wall clock" that is probably the most meaningful basis to use
for an application as a whole.
However, within a blockchain "smart contract" for small durations of seconds to minutes to hours,
say within a day, the clock implicit in the chain height is more reliable and less susceptible
to short-term manipulation by indelicate miners or network attackers.
For long term deadlines, the notion of wall clock maintained by the blockchain
(when it does maintain one) is probably more precise and less susceptible to variation
due to difficulty adjustment or protocol upgrades.
The loose conversions between the many clocks must be handled explicitly by the logic.

### Linear Logic (V2)

Linear Logic very nicely models notions such as resources that must be preserved:
non-fungible resources must at all time be owned by a unique owner;
variables and data structure cells holding fungible resources
cannot themselves be either introduced out of thin air or dropped,
and the amounts they hold also cannot be arbitrarily modified, but
have to be transfered from one variable to the other.

Note that linear logic can also be expressed in the game semantics,
allowing for interactive proofs of linear properties.
This might be useful when verifying whether a protocol using linear logic was respected.
Linear logic can also express various security properties that might matter
in a distributed application.

https://github.com/ppedrot/ll-coq
https://github.com/hypotext/linear-logic
http://www.cs.nuim.ie/~jpower/Research/LinearLogic/
https://www.sciencedirect.com/science/article/pii/S157106611830080X
http://gallais.github.io/proof-search-ILLWiL/pdf/lps.pdf

### Cryptography (V1)

Cryptographic signatures plus timestamps provide a way to prove timely knowledge of events,
via some axiom that say that if some key signed some data by some date,
the actor that has the key saw that data by said date.
```
(sign _key_ _data_)
```

Digests allow for merkleized data structures, etc.
The basic axiom is that, *in any actual execution*,
two digests are equal if and only if their preimages are equal.
But there is no (practical) function to compute the preimage from the digest.
```
(digest _data_)
```

### Formalizing Cryptographic Safety (V2)

How should we formalize the non-collision of cryptographic elements
(digests, location uuids, keys, nonces, etc.),
and other hypotheses upon which the security of the system rests?

One solution is to make non-collison a global axiom.
However, this solution doesn't work in a classical proof setting:
the cryptographic digest function's domain space being finite and smaller than
the space of messages, the pigeonhole principle ensures there do exist collisions,
so that you can easily prove a contradiction (and from there, anything) in Coq.
And this contradiction can be found even without looking at the details of the function,
just knowing that its output can be included
in finite messages communicated along the distributed system.
Similarly, public keys can be inverted in finite time,
the birthday paradox ensures random nonces will eventually repeat, etc.

Yet, a global non-collision axiom is a good axiom to use in suitably concrete or abstract systems.
In an extremely concrete system, where a "proof" is a fully expanded and completed computation,
then finding a proof that there is a collision is the same as actually finding the collision:
a practical improbability. This is exactly how we run the code in practice!
For the same reason, this is not how we want to reason about our systems to make attacks harder:
by the time we are even able to talk about an attack in this way, it's already too late.
At the opposite, in an extremely abstract system, where we can
hide the details of the cryptographical computations behind abstract types,
so we can safely assume non-collision as well as some abstract messageability.
However, we cannot instantiate the cryptographic primitives and still
transport proofs to lower levels of abstractions, or we are back with the usual contradictions.

The solution is that we want to write our programs in the system that abstracts away cryptography,
and when we instantiate those systems "down" toward the concrete, we make some systematic tweaks
to the translation.
One systematic translation may consist in transforming hashing from a pure function
to an effectful function that builds a context that accumulates
the values that are required to be distinct.
The actual "safety axiom" then becomes context-dependent:
given a past, present, future or potential trace of execution,
the axiom states that the context built our of that trace of execution doesn't contain any collisions.
This translation reminds me of non-standard analysis, where "standard" objects
model those you see in practice, while "non-standard" objects may have extraordinary properties,
but cannot affect the inter-relationships between "standard" objects;
there are restrictions on what you can say and how you can reason about non-standard objects,
but using them makes reasoning easier, whereas there is a mechanical translation
between formulas that have "non-standard" objects and formulas that don't,
such that in the end non-standard analysis can be reduced to standard analysis,
and can introduce no contradiction, only make things simpler at times.
Another potential translation would be a "double negation" translation,
wherein instead of `foo->bottom`, the negation of `foo` is written `foo->broken`
where `broken` is the negation of the security property.
Formalizing `broken` may involve including variables introduced in `foo`,
at which point this translation is might end up being the same as the previous one above.
Yet another approach is to compute and maintain along the design some kind of security parameter
that corresponds to the probability of a successful brute-force attack;
bounded quantifiers include a numeric bound on how much "brute force" is involved in the operation.
As long as normal operations have a large enough margin of maneuver, the system is robust.
This approach may actually be a refinement of the above methods.

Actually, cryptographic safety can be formalized in terms of hypotheses in temporal logic;
but then, temporal logic itself may have to be translated monadically as above.
Still, this insight might provide a nicer factoring of how the logic is implemented.

In any case, most applications should be using an abstract view of the code
and its security invariants to prove high-level security properties.
Low-level security properties are to be obtained and proven by an automatic translation as above.

### Finitary Data (V1)

Constants of the language include finite integers,
finite cartesian products and sums thereof (records and variants).
Actually, finite integers can be built from bits and products,
so a minimal version of that language would look just like this:

```gerbil-scheme
(defrecord left (value))
(defrecord right (value))
(def (either? x) (or (left? value) (right? value)))
(def (either-value x) (cond ((left? x) (left-value x)) ((right? x) (right-value x))))

(def (constant?/min x)
  (or (boolean? x)
      (and (pair? x) (constant? (car x)) (constant? (cdr x)))
      (and (either? x) (constant? (either-value x)))))
```

For a full language, we will want like arbitrary large integers as a supported primitive type,
and possibly distinct primitive types for integers modulo n for arbitrary n.
Booleans, bytes, possibly "characters" (unicode codepoints?), would also be their own types.
While we're at it, distinguished unit and empty types might be nice.
You will also want arbitrary records and sums rather than just binary product and binary sum.
There is some notion of user-defined types, that are reduced to the above,
but may have user-defined methods for parsing and printing them.

Records with a list of fields and named sums with a list of cases
can be reduced to simple sums of bits.
Fixed-length vectors can be represented as fixed-shaped trees of elements,
and strings are fixed-length vectors of fixed-length integer bytes.
When we move from prototype to production, we probably want to add
special primitives that make the common elements more efficient.

```gerbil-scheme
(def (constant? x)
  (or (integer? x) (boolean? x) (character? x) (string? x)
      (and (record? x) (every constant? (record->field-list x)))
      (and (choice? x) (constant? (choice->value x)))
      (and (vector? x) (every constant? (vector->list x)))))
```

### Inductive Data Types (V1)

With finitary data types as primitives, we can further build inductive data types.
These data types are defined by recursive equations;
for instance, lists and trees.

Arbitrary finite sums and products.

Interesting feature:
Some data structures can include elements provided by various actors privately,
not known to other actors (unless and until somehow explicitly revealed
by the choosing actor as part of the protocol).
Other actors can only see the digest.
We can statically reason about such data structures even though
we cannot dynamically know about them, or only in a partially blinded fashion.
Sometimes, the full correctness of the data structure is dependent upon all actors being honest,
and can only be assumed during the liveness proof,
whereas the safety proof can only use some weaker partial correctness hypothesis.

### Data Structure Library (V1, V2)

The standard library, if not the language, provides the usual bricks for verifiable data structures:
shallow merklized data structures for which short (logarithmic size) merkle proofs can be provided.
These include tries (big-endian patricia trees), finite maps,
account maps that preserve total amount or make token creation and deletion explicit
(or maybe this can be done in a more general way by general operators on lenses).

The language supports explicitly taking an inductive data structure and reducing it to the finitary case
by specifying small bounds on the size or depth parameters.
This can be used to check large datastructures defined recursively, in O(1).
Recursive SNARKs can also be used to check inductive data structures with verification in O(1).

### Finitary Computations (V2)

One clearly distinguished finitary fragment of the language is finitary computations:
these computations manipulate finitary data and have no unbounded recursion.
This fragment is essentially isomorphic to [Simplicity](https://github.com/ElementsProject/simplicity).
Such a finitary base language is especially valuable
because zkSNARKs can be extracted from expressions and functions written in that fragment;
it an be checked in O(1) with , or be implemented with FPGA or in silicon, etc.
The usual finite arithmetic primitives are available.

The language also distinguishes the fragment that extends the previous with full recursion,
from which interactive proofs of arbitrary depth can be extracted.
Finitary fragments can be obtained by bounding the recursion depth
(and possibly by using recursive zkSNARKs).

### Homomorphic Encryption (V2)

Thanks to our having a finitary computation fragment,
we can add support for homomorphic encryption to compute functions in that fragment
and verify arbitrary such computations with zkSNARKs or zkSTARKs, etc.

### Secure Multiparty Computations (V2)

Building homomorphic encryption,
we will add in-language support for secure multiparty computations,
wherein parties each may contribute encrypted data that others cannot decode,
yet is used to compute a common encrypted result,
from which in turn data fragments can be extracted that are either visible to all,
or encrypted with the key of a particular participant.

One typical example would be a card game where each participant secretly provides
part of a randomness seed, and the homomorphic encryption then computes a encrypted deck of card,
from which it extracts hands encrypted for each player,
and an encrypted continuation for the players to be able to continue to play the game.
Participants would thus be able to play Poker in a state channel,
while an on-chain contract would make it possible to prevent any participant from stalling the game.
(Idea: partner with [CypherPoker](https://github.com/monicanagent/cypherpoker.js).)

Other uses might include coordinating complex auctions between distrustful parties
that each have elaborate bidding strategies to which each participant is committed
yet without revealing the strategy to other participants.

TODO: see [David Evans' work](http://www.mightbeevil.org/) on secure multiparty computation,
such as [Obliv-C](https://oblivc.org/).

### Marshaling (V1)

The language has a notion of some (all?) entities being marshalable into strings of bits (bytes?),
and hence messageable, digestible, signable, usable in distributed proofs, etc.

```
marshal : forall 'a . 'a -> bytes
unmarshal : forall 'a . bytes -> 'a
```

### Contracts (V1)

The language has a verifiable fragment, which has full recursion support
and includes all the usual arithmetic and encryption primitives.
It is possible to specify contracts as arbitrary predicates on the trace of execution;
in particular, it is possible to specify that the behavior of an actor in each step of the trace
does indeed follow the semantics of the agreed-upon program.
The contract clauses can be verified using interactive proofs on merklized data structures
representing the computation its trace of execution including its a snapshot of its state at each step.
For further privacy, the data can be encrypted and the final result computed using homomorphic encryption,
so that observers can't know who's making what claim.

From the contract are automatically extracted at the same time:

* the regular code that plays the game honestly for each party.

* the vigilante code that monitors the blockchains for bad behavior.
  (fact checker?)

* the smart contract that holds participants accountable,
  with a judge verifying interactive proofs of claims of good or bad behavior.

* the mutual knowledge registrar that enforces the integrity of the data

* the smart lawyers that play the game of interactive proofs.

* test cases that provide full code coverage for all the above.

* the smart counsel code that can explain the current situation,
  what are the current options that make sense,
  why some options don't make sense or are less advantageous, etc.

TODO: Primitives?

## Smart Contract Extraction (V1)

We don't want to write the checking of Merkle proofs manually:
it's a tedious and error-prone task.
Instead, we will provide is a way to automatically "lift" a program `A->B`
into a "program with proof" `A*proof_of_A -> B*proof_of_B`,
where this `*` is a dependent product and the proofs are Merkle proofs that can be verified
by a smart contract, itself generated from the type description.

Thus, for a proof of membership or non-membership of an element in an OCaml trie,
you'd lift the code of the `mem` function and the recursive function `find_opt` that it calls,
and the lifted variant would return, with each frame of each function call along the way,
would return all data necessary to constitute a verifiable Merkle proof.

A simple implementation of the above lifting process is a good first step for our V1.
First, we consider the case of an elementary-enough computation,
where the existential parameters have somehow been determined on the client side,
and we only need to feed them to the judge in a verifiable way.
The elementary-enough computation deals with a small subset of the Directed-Acyclic-Graph (DAG)
of data involved in the contract.
When the computation parameters have been determined, the code that verifies their validity
is run in the client in a special mode that records that DAG fragment as it goes.
This same code was compiled in such a way that every time a DAG pointer is followed,
the hash and corresponding value are added to the fragment-in-progress as a side-effect;
when the evaluation completes, the fragment is finalized,
and a serialized representation of the DAG can be produced.
The code registered on the blockchain as "the contract", which acts as the judge for the case,
can run essentially the same code as the client,
except that it is instrumented to consume rather than produces the DAG fragment:
is that in a first pass, the DAG fragment is reconstituted from the serialized representation;
then the judge evaluates the function called with the specified parameters;
if at any point, the evaluation tries to access data outside the reconstituted DAG fragment,
the judge rejects the claim as invalid. If the claim is valid, either it wins the case,
or it advances the argument such that it is now the other party's turn to make a counter-claim
(that itself will have to be validated).

In applications that build a side-chain, the above "elementary enough computations"
can be seen as the microcode of a larger computation:
the microcode specifies how to process a single step of the computation
with local manipulations of the verifiable data structures;
the verifiable data structures encode the macrocode, data heap, control stack...
and execution trace of the computation with each verifiable step indexed in a patricia merkle trie.

As we improve our system, we will want to elaborate how code gets compiled in our V2:

  * When writing the application, it might not always be obvious how best to divide
    the computation between microcode and macrocode.
    Some of it can be automated, whereas some of it might be better left under the control
    of the programmer via suitable annotations.
    When collecting data from a function call frame as part of the verifiable execution trace,
    identify which of the data is easy and cheap to reconstitute from context
    from that is hard, expensive or impossible to reconstitute from context.
    The easy part can be dropped from the explicit trace object,
    to be reconstituted from context when checking the trace.
    Note that whether an execution trace is checked bottom-up or top-down may impact
    what is or isn't "easy" to recompute:
    for instance, checking a merkle tree bottom-up means we don't have to pass along
    the intermediate hashes as part of the proof.
    Ideally, those "cheap" things can be determined in a fully automatic fashion;
    if not, simple annotations will hopefully be enough to specify them.

  * Instead of explicitly verifying properties of trees or graphs, private contracts may use
    zk-SNARKs to argue cases in such a way that the judge can indeed check who is right, yet
    without revealing any data to the public about the case being handled: even the input
    data can be encrypted, with static keys for an argument between a small finite number of parties,
    or with more expensive dynamic keys for an argument with a large number of parties.
    Optimization would then be whatever reduces the size of the circuits involved in building
    the (potentially recursive) zk-SNARKs.

Note that given the definition for a function that is used in a contractual computation,
we want to generate code both for the unlifted (regular) function *and*
the lifted (proof-generating) function:
for performance reasons, regular execution is what is usually used;
the proof-generating variant is only called when an anomaly is detected
by the vigilante running the regular variant,
or when another party makes bogus claims that have to be disproven.

## Reflective evaluation (V2)

There is a verifier for the verifiable fragment written in the verifiable fragment itself,
which allows for side-chains to include arbitrary contracts.
Gas limitations on the main chain are reflected into limitations to how big verifiable computation
steps can be on the side-chain.

### Cost Model (V1)

The verifiable fragment of the language has a cost model
that allows to charge on-chain computations
when it is used as the native language of a blockchain or side-chain.

When compiling the language to some low-level smart contract virtual machine,
a precise cost in gas can be predicted for each transaction,
and the cost model for a program can be deduced from that.
In particular, when compiling programs for a side-chain
where the language is reflectively evaluated by an interpreter (or compiler?)
written in the language itself,
then the cost model for programs is deduced from the composition of
the underlying virtual machine implementation and the evaluator.

If and when we ever have a standalone evaluator, e.g. for the MKB (and/or a CKB on top of it),
we will have to pick our own base cost structure.
Just like with the EVM, operations would have a cost proportional
to the worst-time evaluation cost on a typical CPU (itself growing with the size of the data),
except this cost evolves with CPU architecture.
Explicitly using a compiler rather than an interpreter enables
much more realistic cost models;
but then the reference compiler used for computing costs
must itself be verifiable by the main chain contract.
The cost structure may be defined per topic, included as part of the topic definition.
Updates to the cost computation algorithm for new topics would be soft forks;
retroactive updates to the cost computation algorithm for old topics would be hard forks.
Alternatively, we could let the cost float, and use the MKB as an oracle for its own price,
i.e. the 2/3 registrars with median price get paid what they asked,
those at the extreme get paid less than the lowest of the previous.

### Benchmark Applications (V1, V2)

To illustrate the DSL, we will write a collection of benchmark applications.
They will serve as tutorials so that users can understand the basic concepts.
Their complexity will increase slowly to illustrate how to grow programs in the DSL.
Along the way, we will grow a standard library of reusable primitives
that users can leverage in their applications.
Thus, these benchmark applications will guide both users and implementers
in their experience with the language.

See the [examples](examples/index.md) directory.

## Competition

* [Scilla](https://scilla-lang.org/)
  is the smart contract language for Zilliqa.
  It is embedded in Coq, and features total functions with eventual message sends.

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

* DeepSea, other Coq-based systems...

* [Actus](https://www.actusfrf.org/), works by [Fritz Henglein](http://hjemmesider.diku.dk/~henglein/).

* See what Statebox has, what O(1)labs has, etc.

* [E](https://erights.org/): capabilities and smart contracts from before blockchain.
