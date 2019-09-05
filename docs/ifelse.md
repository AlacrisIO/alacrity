---
title: If-Else
---

Alacrity `if`s are expressions; they're evaluated to their body's content:

```alacrity
let message = if (isMorning) {
  "Good morning!"
} else {
  "Hello!"
};
```

**Note:** an `if-else` expression without the final `else` branch implicitly gives `()`. So this:

```alacrity
if (voted) {
  doSomething()
};
```

is basically the same as:

```alacrity
if (voted) {
  doSomething()
} else {
  ()
};
```