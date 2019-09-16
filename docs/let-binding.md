[//]: # (title: Let Binding)

A "let binding", in other languages, might be called a "variable declaration". `let` gives names to values. They can be seen and referenced by code that comes after them.

```alacrity
let greeting = "hello!";
let score = 10;
let newScore = 10 + score;
```

## Block Scope

Bindings can be scoped through `{}`.

```alacrity
if (displayGreeting) {
  let message = "Enjoying the docs so far?";
  doSomethingWith(message);
};
/* `message` not accessible here! */
```

## Bindings Are Immutable

"Immutable" as in, "doesn't change". Once a binding refers to a value, it cannot refer to anything else (unless it
explicitly contains a mutable value, discussed later). However, you may create a new binding of the same name which *shadows* the previous binding; from that point onward, the binding will refer to the newly assigned value.
