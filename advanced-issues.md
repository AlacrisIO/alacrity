# Advanced Issues

Here are advanced issues that we may or may not want to solve in the language frontend
after we have a stable language backend.

## A Coin Flipping Example

Let's consider a simple game of coin flipping.

We might want to write it as follows:

```
function salt () : unit =[random]=> uint256 {
    return random_uint256();
}

// Players is a typeclass for a interaction or game with some set of players.
// Assumed is that at the start of the game, the players should deposit their game stake
// *plus* a collateral that will cover any court fees in case of misbehavior and dispute.
// When all players have deposited stake and collateral, the game actually starts and all are committed;
// until then, anyone can leave, though they may first have to wait for a timeout.
// (Note that when compiling to a lower-level language, the typeclass is typically translated to
// an additional parameter that remains implicit in the higher-level language, being filled in
// by the type system.)
function atomically_mutualize (data) : Players ==> @for_each 'data => @consensual per_player<'data> {
  @for_each {
    let @verifiable @public commitment = digest(data);
    sync (); // wait for everyone to publish their commitment.
    publish data; // As part of publish, others verify that the data fits its declared type.
    // TODO Optimization: if messages are somehow synchronous (e.g. posting to a blockchain consensus),
    // the last one may reveal the data directly without needing to first commit in a separate message.
    // In particular, when there are only two players, the "last one" is the other-than-first-one.
    // In an iterated game, this can halve the number of messages, noticeably reducing blockchain fees.
    // Should the programmer be responsible for that optimization? Then what API makes it reasonable?
    // Should merging messages and optimizing away commitments be done at a lower-level?
    // Is it done statically or dynamically? Again, what model makes that trivial?
    sync ();
    verify commitment; // the commitment must to be the digest of data, as per its verifiable definition.
    return @consensual all_values(data);
  }
}

function trusted_randomness () : Players ==> @consensual uint256 {
  @for_each {
    @consensual return reduce(logxor, atomically_mutualize(@for_each salt ()));
  }
}

type hand = Rock | Paper | Scissors

function hands_beat(hand0, hand1) : hand => hand => bool {
  return (hand0 = Scissors && hand1 = Paper)
    || (hand0 = Rock && hand1 = Scissors)
    || (hand0 = Paper && hand1 = Rock)
}

// TwoPlayers is a typeclass that specializes Players in the case there are only two players,
// Player0 and Player1. The vector returned by atomically_mutualize will be of size 2.
function rock_paper_scissors (amount) : TwoPlayers ==> amount => @consensual game_result {
  @consensual {
    let [(_, hand0), (_, hand1)] = atomically_mutualize(@for_each (salt (), input hand));
    if (hand0 = hand1) {
      game_is_draw();
    } else if (beats_hand (hand0, hand1)) {
      wins(Player0);
    } else {
      wins(Player1);
    }
  }
}
```

In the above example, `Players` is some kind of typeclass
that provides the notion of there being many players,
as well as the primitives
`@for_each` `@verifiable` `@public` `sync` `publish` `verify` and `consensual`.

The `@for_each` block marks some part of the algorithm as working similarly
on each of the players in the `Players` pool.

The `@public` attribute works as if `publish h` was called after the definition of `h`
which itself means that `h` will be shared with all other players:
messages will be sent to each of the other players, signed by the player,
encrypted for the recipients, containing an identifier for the current data
(hash of the line of source code, global context frame, etc., including identity of players)
followed by the actual data.
The call to the `sync` macro waits until all published values are received
before proceeding to the next step;
some variant may take a list of variables as parameter,
or the identifier of a scope (representing all the variables within that scope only).

The `verify` primitive
(I would use the term `promise` or `commitment` but they mean something different
in the context of computer science),
remembers the definition of `h` in terms of `r`,
which was marked as `@verifiable`.
Note thta the verification is done by the *other* players,
and if the verification fails, the player who made the false promise is punished:
his name is added to the list of failed players,
who will lose their security deposit in favor of other players —
some global notion of blame offered by the entire `Players` notion.

`@consensual` means that everyone can thereupon verify that
the return value is indeed the result of this computation,
and that a contract can be written based on this answer.
That everyone will indeed get the same answer and that it is verifiable
by the contract should be proven correct by the implementation,
and can be used as part of further verifications:
it must be computable in a deterministic way from information that is itself consensual.

In the end, it can all be translated into a lower-level message exchanges,
targetting a variant of the Pi calculus,
probably closer to the Kell calculus (for locations)
and/or to the Rho calculus (for the digestible reification of computations).

NB: Maybe we should use some variant of the quasiquote and unquote like xapping syntax
for SIMD vs MIMD fragments of code as in the Connection Machine's *Lisp (starlisp) ?
<https://dl.acm.org/citation.cfm?id=319870>.
In this general theme, see also the talk by Guy Steele about the semantics of the
notation used in CS articles for action on vectors of data.
