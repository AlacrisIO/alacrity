[//]: # (title: Enum)

Enumerations serve the purpose of representing a group of named constants in a programming language.
For example the 4 suits in a deck of playing cards may be 4 enumerators
named Club, Diamond, Heart, and Spade, belonging to an enumerated type named Suit.
Other examples include natural enumerated types
(like the planets, days of the week, colors, directions, etc.).

Enums are used when we know all possible values at compile time, such as
choices on a menu, rounding modes, command line flags, etc.
It is not necessary that the set of constants in an enum type stay fixed for all time.

The main objective of enum is to define our own data types (Enumerated Data Types).

```alacrity
enum Suit {
	CLUB, DIAMOND, HEART, SPADE
}
```

For now, an `enum` defines `uint256` constants as well as a validation function
(with the same name as the `enum`)
that takes a `uint256` and tests whether its value is a valid member of the `enum`.
In the future, each `enum` will introduce a new type, with its own representation,
and explicit conversion functions to and from integers, etc.
In the future, there will also be functions automatically defined on the client/server side,
but *not* on the contract side, to convert between string and `enum` constants.
In the future, the validation function may be generated with a name different from that of the `enum`.
