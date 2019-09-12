[//]: # (title: Type!)

Types are the highlight of Alacrity! Here, you get a glimpse of why so many are excited about them.

This section briefly introduces the types syntax so that you can power through the subsequent sections without getting confused. More advanced topics on types can be found in the [More On Type](moreontype) section.

## Annotations

This let-binding doesn't contain any written type:

```alacrity
let score = 10;
```

Reason knows that `score` is an `int`, judging by the value `10`. This is called **inference**.

But types can also be explicitly written down by choice:

```alacrity
let score: int = 10;
```

You can also wrap any expression in parentheses and annotate it:

```alacrity
let myInt = 5;
let myInt: int = 5;
let myInt = (5: int) + (4: int);
let add = (x: int, y: int) : int => x + y;
```
