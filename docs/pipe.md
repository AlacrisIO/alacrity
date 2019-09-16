[//]: # (title: Pipe First)

`->` is a convenient operator that allows you to "flip" your code inside-out. `a(b)` becomes `b->a`. It's a piece of syntax that doesn't have any runtime cost.

Imagine you have the following:

```alacrity
validateAge(getAge(parseData(person)))
```

This is slightly hard to read, since you need to read the code from the innermost part, to the outer parts. Use Pipe First to streamline it

```alacrity
person
  ->parseData
  ->getAge
  ->validateAge
```

Basically, `parseData(person)` is transformed into `person->parseData`, and `getAge(person->parseData)` is transformed into `person->parseData->getAge`, etc.
