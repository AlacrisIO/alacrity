[//]: # (title: Function)

Functions are declared with a variant of the traditional javascript syntax:

```alacrity
function funName(arg1, arg2) : returnType {
    body...;
    result }
```

With Alacrity's current only "direct-style" backend,
functions are inlined at compile-time and may not recurse.
