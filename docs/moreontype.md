[//]: # (title: More on Type)

## Type Argument!

Types can accept parameters, akin to generics in other languages. It's as if a type is a function that takes in arguments and returns a new type! The parameters **need** to start with `'`.

The use-case of a parameterized type is to kill duplications. Before:

```alacrity
/* this is a tuple of 3 items, explained next */
type intCoordinates = (int, int, int);
type floatCoordinates = (float, float, float);

let buddy: intCoordinates = (10, 20, 20);
```
