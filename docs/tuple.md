[//]: # (title: Tuple)

We currently only support pairs. In the future, we will support arbitrary tuples.

Tuples are

- immutable
- ordered
- fix-sized at creation time
- heterogeneous (can contain different types of values)

```alacrity
let ageAndName = (24, "Lil' Reason");
let my2dCoordinates = (20.0, 30.5);
```

Tuples' types can be used in type annotations as well. Tuple types visually resemble tuples values.

```alacrity
let ageAndName: (int, string) = (24, "Lil' Reason");
/* a tuple type alias */
type coord2d = (float, float);
let my3dCoordinates: coord2d = (20.0, 30.5);
```

**Note**: there's no tuple of size 1. You'd just use the value itself.
