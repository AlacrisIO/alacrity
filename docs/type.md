[//]: # (title: Type!)

So far, Alacrity is simply typed.
Currently, the only base types are [bool](boolean.md), [uint256](number.md) and [bytes](bytes.md).
Currently, the only type combinator is [pair](tuple.md).
All data types are combinations of the above.

Then there are [functions](function.md).
For now only first-order functions are allowed;
there are no function as first-class values, and
no need for explicit function types in the language so far.
A function has a list of parameters as input, each constrained to a data type;
it has one output, also constrained to a data type,
that may be a (nested) pair to return multiple values.

In the future, we will support richer types;
we will probably copying features from
OCaml (ReasonML), Haskell, Purescript, Scala, etc.

New types can be declared.
Currently, the only type declaration is [enum](enum.md), and
it actually declares an initial subset of the [integers](number.md).
