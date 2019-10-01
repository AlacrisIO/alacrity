[//]: # (title: Function)

Functions are declared with a variant of the traditional javascript syntax:

```alacrity
function funName(arg1, arg2) : returnType {
    body...;
    result }
```

With Alacrity's current only "direct-style" backend,
functions are inlined at compile-time and may not recurse.

For now only first-order functions are allowed:
there are no function as first-class values, and
functions themselves cannot be used as parameters or as return values.
There is thus no need for explicit function types in the language so far.
A function has a list of parameters as input, each constrained to a [data type](type.md);
it has one output, also constrained to a [data type](type.md),
that may be a (nested) [pair](tuple.md) to return multiple values.
