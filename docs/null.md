[//]: # (title: Null, Undefined & Option)

Alacrity itself doesn't have the notion of `null` or `undefined`. This is a _great_ thing, as it wipes out an entire category of bugs. No more `undefined is not a function`, and `cannot access foo of undefined`!

However, the **concept** of a potentially nonexistent value is still useful, and safely exists in our language.

We represent the existence and nonexistence of a value by wrapping it with the `option` type. Here's its definition from the standard library:

```reason
type option('a) = None | Some('a)
```
