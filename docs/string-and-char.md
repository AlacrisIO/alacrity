---
title: String & Char
---

## String

Reason strings are delimited using **double** quotes (single quotes are reserved for the character type below).

```alacrity
let greeting = "Hello world!";
let multilineGreeting = "Hello
 world!";
```

Special characters in the string need to be escaped:

```alacrity
let oneSlash = "\\";
```

To concatenate strings, use `++`:

```alacrity
let greetings = "Hello " ++ "world!";
```

### Quoted String

There's a special syntax for string that allows

- multiline string just like before
- no special character escaping
- hooks for special pre-processors

```alacrity
let greetingAndOneSlash = {|Hello
World
\
Hehe...
|};
```