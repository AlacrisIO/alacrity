---
title: Pattern Matching!
---

_Make sure you've read up on [Variant](variant) first_.

**We're finally here**! Pattern matching is one of _the_ best features of the language. It's like destructuring, but comes with even more help from the type system.

## Usage

Consider a variant:

```reason
type payload =
  | BadResult(int)
  | GoodResult(string)
  | NoResult;
```

While using the `switch` expression on it, you can "destructure" it:
