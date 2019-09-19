[//]: # (title: Variant!)

Behold, the crown jewel of Alacrity data structures!

Most data structures in most languages are about "this **and** that". A variant allows us to express "this **or** that".

```alacrity
type myResponseVariant =
  | Yes
  | No
  | PrettyMuch;

let areYouCrushingIt = Yes;
```

`Yes`, `No` and `PrettyMuch` aren't strings, nor references, nor some special data type. They're called "constructors" (or "tag"). The `|` bar separates each constructor.

**Note**: a variant's constructors need to be capitalized.

## Usage

Along with a variant comes one of the most important features of Alacrity, the `switch` expression.

An Alacrity `switch` is visually similar to other languages' `switch` (aka a large `if/elseif/elseif...`). It allows you to check every possible case of a variant. To use it, enumerate every variant constructor of the particular variant you'd like to use, each followed by an `=>` and the expression corresponding to that case.

```alacrity
let message =
  switch (areYouCrushingIt) {
  | No => "No worries. Keep going!"
  | Yes => "Great!"
  | PrettyMuch => "Nice!"
  };
/* message is "Great!" */
```

A variant has an extremely rich amount of type system assistance. For example, we'll give you a type error if you've forgotten to cover a case of your variant, or if two cases are redundant. Be sure to check out switch and pattern-matching in a [later section](patternmatching)!
