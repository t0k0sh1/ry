# Collection Reference

Ry supports tuples, lists, maps, sets, and iterators.

## Summary

| Type | Literal | Mutability | Notes |
|---|---|---|---|
| Tuple | `(1, "x")`, `(42,)` | value is fixed-size | heterogeneous; fields accessed as `.0`, `.1`, ... |
| `List<T>` | `[1, 2, 3]` | mutable elements and length | homogeneous dynamic array |
| `Map<K, V>` | `{"a": 1}` | mutable entries | hash map |
| `Set<T>` | `{1, 2, 3}` | mutable membership | unique elements |
| `Iterator<T>` | `iter(xs)` | consumed lazily | sequential access via iterator helpers |

Empty list/set literals need type context:

```ry
xs: List<int> = []
seen: Set<str> = {}
```

## Tuple

Tuples are fixed-length heterogeneous values.

```ry
single = (42,)                 # trailing comma required
pair: (int, str) = (1, "one")

print(pair.0)                  # 1
print(pair.1)                  # one
print((1, 2) == (1, 2))        # true
```

Rules:

- Tuple elements are accessed with numeric fields such as `.0`.
- `==` and `!=` compare element-wise.
- Ordering comparisons are not supported.
- Out-of-range tuple field access is a compile error.

## List

Lists are homogeneous dynamic arrays.

```ry
xs = [1, 2, 3]
xs: List<int> = []
xs[0] = 99
xs[-1] = 42                   # negative index wraps from the end
```

Indexing:

- `xs[i]` aborts on out-of-range access.
- negative indices wrap from the end before bounds checking.
- `xs[i]?` returns `Option<T>` instead of aborting; see [Operators](operators.md#option-returning-index-form-mk--xsi).
- `xs[a..b]` returns a slice.

Common operations:

| Operation | Result |
|---|---|
| `len(xs)` | element count |
| `xs + ys`, `xs += ys` | concatenate lists with the same element type |
| `append(xs, v)` / `append!(xs, v)` | non-mutating / mutating append form |
| `pop(xs)` | removes and returns the last element |
| `get(xs, i)` | `Option<T>` safe index lookup |
| `slice(xs, start, end)` | sub-list |
| `take(xs, n)` | first `n` elements |
| `filter(xs, pred)` | selected elements |
| `map(xs, fn)` | transformed elements |
| `reduce(xs, init, fn)` / `fold(xs, init, fn)` | accumulated value |
| `sort(xs)` / `sort!(xs)` | sorted copy / in-place sort |
| `reverse(xs)` / `reverse!(xs)` | reversed copy / in-place reverse |
| `first(xs)`, `last(xs)` | first / last element |
| `isEmpty(xs)` | `len(xs) == 0` |
| `enumerate(xs)` | pairs of index and element |
| `zip(xs, ys)` | paired elements |
| `insert(xs, i, v)`, `removeAt(xs, i)`, `remove(xs, v)` | positional or value mutation |
| `distinct(xs)`, `flat(xs)` | duplicate removal / one-level flattening |

### filter

`filter(xs, pred)` returns the elements for which `pred(element)` is true.

### map

`map(xs, fn)` returns a list formed by applying `fn` to each element.

### sequence

`sequence(xs)` folds `List<Result<T, E>>` to `Result<List<T>, E>` or `List<Option<T>>` to `Option<List<T>>`, short-circuiting on the first `Err` or `None`.

### sort

`sort(xs)` returns a sorted copy. `sort!(xs)` sorts in place.

### In-Place Mutating Variants

Names ending in `!` mutate their input collection directly. Non-`!` variants return a new value and leave the original binding unchanged except for normal copy-on-write effects.

Lists support `for` iteration, equality, printing, nested indexing, and chained field/index assignment:

```ry
record Point:
    x: int
    y: int

pts = [Point(1, 2)]
pts[0].x += 1
```

## Map

Maps store key/value pairs.

```ry
m = {"a": 1, "b": 2}
m["a"] = 10
print(m["a"])
```

Lookup:

- `m[k]` aborts when the key is absent.
- `m[k]?` returns `Option<V>`.
- `get(m, k)` is the named safe lookup form.
- `hasKey(m, k)` checks for a key.
- `contains(m, k)` is membership-style key lookup.

Common operations:

| Operation | Result |
|---|---|
| `len(m)` | entry count |
| `keys(m)` | `List<K>` |
| `values(m)` | `List<V>` |
| `items(m)` | key/value pairs |
| `remove(m, k)` | removes an entry |
| `m + n`, `m += n`, `merge(m, n)` | merge maps; right side wins on key collision |
| `getPath(m, path)` | nested lookup in JSON-shaped maps/lists |
| `setPath(m, path, value)` | nested write in JSON-shaped maps/lists |

For `Map<str, any>`, dot sugar can access string keys where supported:

```ry
data: Map<str, any> = {"user": {"name": "Ada"}}
print(data.user.name)
```

`getPath` / `setPath` support dotted paths, escaped dots, and negative-index list segments. Leaf writes into a list index are intentionally limited; use direct list indexing when mutating list leaves.

## Set

Sets store unique elements.

```ry
s = {1, 2, 3}
add(s, 4)
remove(s, 2)
print(3 in s)
```

Common operations:

| Operation | Result |
|---|---|
| `len(s)` | element count |
| `add(s, v)` | insert element |
| `remove(s, v)` | remove element |
| `s + t`, `s += t`, `union(s, t)` | union |
| `intersection(s, t)` | common elements |
| `difference(s, t)` | elements in `s` not in `t` |
| `symmetricDifference(s, t)` | elements in exactly one set |
| `isSubset(s, t)` / `isSuperset(s, t)` | set relation |

Sets support equality, printing, `for` iteration, and membership with `in`.

## Iterator

Iterators are lazy sequential views over collections.

```ry
it = iter([1, 2, 3])
print(next(it))        # Some(1)
print(toList(it))      # remaining elements
```

Iterator chains are lazy until consumed:

```ry
ys = iter([1, 2, 3, 4])
    .filter((x: int) => x % 2 == 0)
    .map((x: int) => x * 10)
    .toList()
```

Use `for` loops when all elements should be consumed in order.

## Copy-on-Write

Lists, maps, and sets use copy-on-write for shared collection storage. Mutating through one variable first ensures the collection is unique, so aliases keep their previous value.

```ry
a = [1, 2]
b = a
a[0] = 99
print(b[0])   # 1
```

CoW can trigger on element assignment, insertion, removal, append, map updates, set membership changes, and nested path updates.

## Constraints

- Collection literals must have a single element type, except tuples.
- Empty list/set literals need type context.
- Map keys must be hashable and comparable.
- List and set element operations are best supported for scalar types and collections documented in the individual function references.
- Indexing failures abort unless using the `?` safe form or named `get`.

## Related

- [Built-in Functions](builtins.md)
- [Operators](operators.md)
- [Types](types.md)
