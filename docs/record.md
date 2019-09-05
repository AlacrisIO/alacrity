---
title: Record
---

Records are like JavaScript objects but are

- lighter
- immutable by default
- fixed in field names and types
- very fast
- a bit more rigidly typed

## Usage

Type (mandatory):

```alacrity
type person = {
  age: int,
  name: string
};
```

Value (this will be inferred to be of type `person`):

```alacrity
let me = {
  age: 5,
  name: "Big Reason"
};
```