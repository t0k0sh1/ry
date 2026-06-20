# Collection Reference (Tuple, List, Map, Set)

## Tuple

### Overview

A fixed-length combination of heterogeneous values. Implemented as a stack-allocated value type using LLVM literal StructType.

### Syntax

```ry
t = (42,)                      # single-element tuple (trailing comma required)
t = (1, 3.14)
t: (int, float) = (1, 3.14)
```

### Type Annotation

```ry
single: (int,) = (42,)                     # trailing comma required for single-element
pair: (int, str) = (42, "hello")
triple: (int, float, bool) = (1, 2.0, true)
```

### Comparison

Tuples support `==` and `!=` via element-wise comparison.

```ry
print((1, 2) == (1, 2))    # true
print((1, 2) != (3, 4))    # true
print(("a", 1) == ("b", 1))  # false
```

### Element Access

Access elements using numeric indices `.0`, `.1`, etc.

```ry
t = (10, 3.14)
print(t.0)   # 10
print(t.1)   # 3.14
```

### Function Return Values

```ry
fn swap(a: int, b: int) -> (int, int):
    return (b, a)

result = swap(1, 2)
print(result.0)   # 2
print(result.1)   # 1
```

### Constraints and Errors

| Constraint | Details |
|------|------|
| Out-of-range index | Compile error |
| Comparison operators | Only `==` and `!=` are supported; `<`, `<=`, `>`, `>=` are not |

---

## List

### Overview

A variable-length sequence of elements of the same type. Allocated on the heap.

### Syntax

```ry
xs = [1, 2, 3]
xs: List<int> = [1, 2, 3]
xs = [1, 2, 3,]          # trailing comma allowed
```

### Empty List

An empty list requires a type annotation so the element type can be determined:

```ry
xs: List<int> = []
xs: List<str> = []
```

### Concatenation

Lists can be concatenated with `+` and `+=`:

```ry
a = [1, 2, 3]
b = [4, 5, 6]
c = a + b       # [1, 2, 3, 4, 5, 6]
a += [7, 8]     # a is now [1, 2, 3, 7, 8]
```

Both operands must have the same element type.

### Supported Element Types

`int`, `float`, `bool`, `str`

### Equality

Lists support `==` and `!=`. Two lists are equal when they have the same length and all corresponding elements are equal.

```ry
[1, 2, 3] == [1, 2, 3]   # true
[1, 2, 3] != [1, 2, 4]   # true
[1, 2]    != [1, 2, 3]   # true (different lengths)
```

### Index Access

```ry
xs = [1, 2, 3]
print(xs[0])   # 1
print(xs[2])   # 3
```

Negative indices wrap around from the end (Python-style):

```ry
xs = [10, 20, 30]
print(xs[-1])   # 30 (last element)
print(xs[-2])   # 20
print(xs[-3])   # 10
```

Out-of-bounds access (including negative indices that exceed the list length) raises a runtime error.

### Safe Index Access (`xs[i]?`)

`xs[i]?` returns `Option<T>` instead of aborting on out-of-range access. Negative-index wrap is applied first; only when the wrap result is still out of range does the expression evaluate to `None`. See [operators.md](operators.md#option-returning-index-form-mk--xsi) for the cross-collection spec.

```ry
xs = [10, 20, 30]
print(xs[1]?)        # Some(20)
print(xs[100]?)      # None
print(xs[-1]?)       # Some(30)   — negative wrap to last
print(xs[-100]?)     # None        — wrap result still out of range
print(xs[100]? ?? 0) # 0           — coalesce with default (note the space before ??)
```

Restrictions: read-only — `xs[i]? = v` is a compile error; range-index `xs[a..b]?` is a compile error.

For an explicit named-function alternative with the same Option-returning semantics, use [`get(xs, i)`](#get).

### Index Assignment

```ry
xs = [1, 2, 3]
xs[0] = 99
print(xs[0])   # 99
xs[-1] = 42    # assigns to last element
print(xs[2])   # 42
```

### Chained Index and Field Assignment

Index and field assignment compose: the left-hand side of `=` / `+=` / `-=` /
`*=` / `/=` / `//=` / `%=` / `**=` / `&=` / `|=` / `^=` / `<<=` / `>>=` can be
any postfix chain rooted at a mutable variable.

```ry
record Point:
  x: int
  y: int

pts = [Point(1, 2), Point(3, 4)]
pts[0].x = 99           # list-of-records field update
pts[0].x += 1
print(pts[0].x)         # 100

grid = [[1, 2], [3, 4]]
grid[0][1] = 99         # nested list element
print(grid[0])          # [1, 99]

m: Map<str, Map<str, int>> = {"a": {"x": 1}}
m["a"]["x"] = 42        # nested map element
```

Compound assignment evaluates each index exactly once, so `xs[f()] += 1` calls
`f()` a single time. Compound assignment to a missing map key is a runtime
error — insert the key first if you want to accumulate:

```ry
m = {"a": 1}
m["a"] += 10            # OK  → {"a": 11}
m["b"] += 10            # runtime error: compound assignment to missing map key
```

> **Nested writes are isolated**: chained writes like `grid[i][j] = v` and
> `r.items[i] = v` (through a record field containing a list) privatize
> every level on the LHS path whose reference count > 1 before the
> mutation lands, so aliases of the outer container — or any inner
> container on the path — observe the pre-write state. See
> [Path Copy-on-Write](#path-copy-on-write) below for the details.

### len

```ry
xs = [1, 2, 3]
print(len(xs))   # 3
```

### print

```ry
xs = [1, 2, 3]
print(xs)   # [1, 2, 3]
```

### for Iteration

```ry
xs = [10, 20, 30]
for x in xs:
    print(x)
# 10
# 20
# 30
```

### append

Adds an element to the end of the list. This is a mutating operation.

```ry
xs = [1, 2]
xs.append(3)
print(xs)   # [1, 2, 3]
```

### pop

Removes and returns the last element. Returns `None` if the list is empty.

```ry
xs = [1, 2, 3]
v = xs.pop()
print(v)    # Some(3)
print(xs)   # [1, 2]
```

### get

Two overloads are available:

- `get(list, index) -> Option<T>`: Returns `Some(value)` for an in-range index, or `None` for an out-of-range one. Negative indices wrap around from the end, matching [`xs[i]?`](#safe-index-access-xsi) semantics.
- `get(list, index, default) -> T`: Returns the value at the index, or `default` for any out-of-range case (including a negative index whose wrap is still out of range). The `default` expression is evaluated lazily — it runs only on the out-of-range path, so passing a function call as the default does not invoke it when the index is in range.

```ry
xs = [10, 20, 30]
print(get(xs, 1))        # Some(20)
print(xs.get(100))       # None
print(get(xs, -1))       # Some(30)  — negative wrap to last
print(xs.get(-4))        # None       — wrap result still out of range
print(get(xs, 100, 0))   # 0
print(xs.get(-1, 0))     # 30
```

This is the named-function counterpart of [`xs[i]?`](#safe-index-access-xsi) and shares its wrap-then-bounds-check rule; it leaves the abort-on-out-of-range behavior of `xs[i]` unchanged. The same shape is available on `Map<K, V>` — see [`get`](#get-1) under Map.

### reverse

Returns a new list with elements in reverse order. The original list is not modified. Also works on strings.

```ry
xs = [1, 2, 3]
print(reverse(xs))   # [3, 2, 1]
print(xs)            # [1, 2, 3] (unchanged)
```

### slice

Returns a new sub-list covering `[start, end)` (end exclusive). Negative indices are resolved as `len(list) + idx` (Python-style, consistent with `lst[-1]`). The resolved range is then silently clamped to `[0, len(list)]`.

```ry
xs = [1, 2, 3, 4, 5]
print(slice(xs, 1, 3))     # [2, 3]
print(slice(xs, 0, 100))   # [1, 2, 3, 4, 5] (end clamped)
print(slice(xs, -2, 5))    # [4, 5]  (negative start wraps to index 3)
print(slice(xs, -4, -1))   # [2, 3, 4]  (both bounds wrap)
```

You can also use the `..` range operator as a list index for an equivalent, more concise syntax. `lst[a..b]` is inclusive on both ends and is equivalent to `slice(lst, a, b + 1)`. Negative indices wrap against the list length (like `lst[-1]`), and out-of-bounds bounds are clamped.

```ry
xs = [1, 2, 3, 4, 5]
print(xs[1..3])     # [2, 3, 4]  (inclusive)
print(xs[-3..-1])   # [3, 4, 5]  (negative wrap)
print(xs[1..100])   # [2, 3, 4, 5] (clamped)
```

### take

Returns a new list with the first `count` elements. If `count` exceeds the list length, returns a copy of the entire list. If `count <= 0`, returns an empty list. The original list is not modified.

```ry
xs = [1, 2, 3, 4, 5]
ys = xs.take(3)
print(ys)   # [1, 2, 3]
print(xs.take(10))   # [1, 2, 3, 4, 5] (clamped)
print(xs.take(0))    # []
```

### tap

Calls the given function on each element (ignoring any return value), then returns the original list unchanged. Useful for debugging or inserting side effects in a method chain.

```ry
xs = [1, 2, 3]
ys = xs.tap((x: int) => print(x)).map((x: int) => x * 2)
# prints 1, 2, 3, then ys = [2, 4, 6]
```

### filter

Returns a new list containing only elements that satisfy the predicate. The original list is not modified.

```ry
xs = [1, 2, 3, 4, 5]
ys = xs.filter((x: int) => x > 3)
print(ys)   # [4, 5]
```

### map

Returns a new list with each element transformed by the given function. The output element type can differ from the input. The original list is not modified.

```ry
xs = [1, 2, 3]
ys = xs.map((x: int) => x * 2)
print(ys)   # [2, 4, 6]
```

### sort

Returns a new sorted list. Default is ascending order. A custom comparator can be provided. The original list is not modified. The sort is **stable** (equal elements preserve their original order). Internally uses TimSort.

```ry
xs = [3, 1, 2]
print(xs.sort())   # [1, 2, 3]

# Descending order with comparator
desc = xs.sort((a: int, b: int) => a > b)
print(desc)   # [3, 2, 1]
```

### Chaining filter, map, sort

These functions return new lists, so they can be chained via UFCS.

```ry
xs = [5, 3, 1, 4, 2]
result = xs.filter((x: int) => x > 1).map((x: int) => x * 10).sort()
print(result)   # [20, 30, 40, 50]
```

### reduce

Reduces a list to a single value using an accumulator function, starting with the first element. Returns `Option<T>` — `Some(result)` for a non-empty list, or `None` for an empty list. Use `fold` when you have an explicit initial value.

```ry
xs = [1, 2, 3, 4, 5]
total = reduce(xs, (a: int, b: int) => a + b)
print(total)   # Some(15)
```

Unwrap with the `??` operator (or pattern match) to recover a plain value:

```ry
xs = [1, 2, 3, 4, 5]
print((reduce(xs, (a: int, b: int) => a + b)) ?? 0)   # 15

empty: List<int> = []
print((reduce(empty, (a: int, b: int) => a + b)) ?? -1)   # -1
```

Type annotations on lambda parameters are optional:

```ry
xs = [1, 2, 3, 4, 5]
print(reduce(xs, (a, b) => a + b))   # Some(15)
```

Calling `reduce` with three arguments (Python/JS style) is a compile error that suggests `fold`:

```ry
# error: reduce() takes 2 arguments, not 3; to use an initial value, call fold(list, init, fn) instead
reduce([1, 2, 3], 0, (a: int, b: int) => a + b)
```

### fold

Folds a list to a single value using an accumulator function and an explicit initial value. Returns the accumulator value directly (not wrapped in `Option`) — on an empty list the initial value is returned unchanged.

```ry
xs = [1, 2, 3, 4, 5]
total = fold(xs, 0, (a: int, b: int) => a + b)
print(total)   # 15
```

Type annotations on lambda parameters are optional:

```ry
xs = [1, 2, 3, 4, 5]
print(fold(xs, 0, (a, b) => a + b))   # 15
```

The initial value must have the same type as the accumulator function's return type; mismatches are a compile error: `fold() initial value type must match function return type`.

### sequence

Folds a list of `Result` or `Option` into a single `Result` or `Option` of list, short-circuiting on the first `Err` / `None`. Same as Haskell/Scala `sequence`. `xs.map(f).sequence()` is the `traverse(f)` equivalent.

```ry
fn mkResult(n: int, fail: bool) -> Result<int, Error>:
  if fail:
    return Err(Error("boom"))
  return Ok(n)

# All Ok → Ok(list)
xs = [mkResult(1, false), mkResult(2, false), mkResult(3, false)]
case sequence(xs):
  Ok(v): print(v)            # [1, 2, 3]
  Err(e): print(e.message)

# First Err short-circuits
ys = [mkResult(1, false), mkResult(0, true), mkResult(3, false)]
case sequence(ys):
  Ok(v): print(v)
  Err(e): print(e.message)   # "boom"
```

The dual form on `Option`:

```ry
fn mkOption(n: int, isNone: bool) -> Option<int>:
  if isNone:
    return None
  return Some(n)

xs = [mkOption(10, false), mkOption(20, false), mkOption(30, false)]
case sequence(xs):
  Some(v): print(v)          # [10, 20, 30]
  None: print("missing")

ys = [mkOption(1, false), mkOption(0, true), mkOption(3, false)]
case sequence(ys):
  Some(v): print(v)
  None: print("missing")     # "missing"
```

UFCS form is also supported: `xs.sequence()`.

Empty list returns `Ok([])` for `List<Result<T, E>>` and `Some([])` for `List<Option<T>>`.

Note: `xs.map(f).sequence()` allocates a temporary `List<Result<T, E>>` (or `List<Option<T>>`) before folding, so it makes two passes over the input. A future `traverse(f)` combinator would fuse map and sequence into a single pass; until then prefer it for short pipelines and reach for an explicit `for` + `case` loop when allocation cost matters.

Type errors:
- `sequence([1, 2, 3])` (or any list whose element is not `Result` or `Option`) is a compile error: `sequence() requires a list of Result or Option`.
- `sequence(42)` (non-list argument) produces the same error.

### any

Returns `true` if at least one element satisfies the predicate.

```ry
xs = [1, 2, 3, 4, 5]
print(any(xs, (x: int) => x > 4))   # true
print(any(xs, (x: int) => x > 9))   # false
```

### all

Returns `true` if every element satisfies the predicate.

```ry
xs = [2, 4, 6]
print(all(xs, (x: int) => x > 0))   # true
print(all(xs, (x: int) => x > 3))   # false
```

### sum

Returns the sum of all elements. Accepts either a single numeric list, or two or more scalars passed directly (`sum(a, b, ...)`, unbounded). The element type may be `int`, `float`, or `u8`; in the variadic form all arguments must share one type, and the result has that type.

```ry
xs = [1, 2, 3, 4, 5]
print(sum(xs))             # 15
print(sum(3, 5))           # 8    (variadic)
print(sum(1, 2, 3, 4))     # 10
print(sum(1.0, 2.0, 3.0))  # 6.0
```

### min

Returns the minimum element. Accepts either a single numeric list, or two or more scalars passed directly (`min(a, b, ...)`, unbounded). The element type may be `int` or `float`; in the variadic form all arguments must share one type. Calling `min` on an empty list is a runtime error: `runtime error: min() on empty list` (the variadic form always has at least two arguments, so it is never empty).

```ry
xs = [3, 1, 4, 1, 5]
print(min(xs))        # 1
print(min(3, 5))      # 3    (variadic)
print(min(3, 5, 1))   # 1
print(min(1.5, 2.5))  # 1.5
```

### max

Returns the maximum element. Accepts either a single numeric list, or two or more scalars passed directly (`max(a, b, ...)`, unbounded). The element type may be `int` or `float`; in the variadic form all arguments must share one type. Calling `max` on an empty list is a runtime error: `runtime error: max() on empty list` (the variadic form always has at least two arguments, so it is never empty).

```ry
xs = [3, 1, 4, 1, 5]
print(max(xs))        # 5
print(max(3, 5))      # 5    (variadic)
print(max(3, 1, 5))   # 5
print(max(1.5, 2.5))  # 2.5
```

### first

Returns the first element. Returns `None` if the list is empty.

```ry
xs = [10, 20, 30]
print(first(xs))   # Some(10)
```

### last

Returns the last element. Returns `None` if the list is empty.

```ry
xs = [10, 20, 30]
print(last(xs))   # Some(30)
```

### isEmpty

Returns `true` if the container has no elements. Accepts lists, maps, sets, and strings.

```ry
xs = [1, 2, 3]
print(isEmpty(xs))        # false
empty: List<int> = []
print(isEmpty(empty))     # true
print(isEmpty(""))        # true  (str support, #831)
print(isEmpty("hello"))   # false
```

### enumerate

Returns a list of `(index, element)` tuples.

```ry
xs = [10, 20, 30]
pairs = enumerate(xs)
# pairs = [(0, 10), (1, 20), (2, 30)]

# Tuple destructuring in for loop
for i, x in enumerate(xs):
    print(f"{i}: {x}")    # 0: 10, 1: 20, 2: 30
```

### zip

Combines two lists into a list of `(elem1, elem2)` tuples. The result length equals the shorter list.

```ry
xs = [1, 2, 3]
ys = ["a", "b", "c"]
pairs = zip(xs, ys)
# pairs = [(1, "a"), (2, "b"), (3, "c")]

# Tuple destructuring in for loop
for a, b in zip(xs, ys):
    print(f"{a}: {b}")    # 1: a, 2: b, 3: c
```

### insert

Inserts an element at the specified index. Elements at and after the index are shifted right.

```ry
xs = [1, 2, 3]
insert(xs, 1, 99)
print(xs)   # [1, 99, 2, 3]
```

### removeAt

Removes and returns the element at the specified index. Elements after the index are shifted left.

```ry
xs = [1, 2, 3, 4]
v = removeAt(xs, 1)
print(v)    # 2
print(xs)   # [1, 3, 4]
```

### remove

Removes the first occurrence of the specified value from the list. Does nothing if the value is not found. This is a mutating operation. Passing a list of non-string pointer elements (e.g. `List<List<T>>`, `List<Map<K, V>>`, `List<Set<T>>`, `List<fn(...) -> R>`) is a compile error: `remove() is only supported for lists of primitive values or strings`.

```ry
xs = [1, 2, 3, 2, 4]
remove(xs, 2)
print(xs)   # [1, 3, 2, 4]
```

### distinct

Returns a new list with duplicate elements removed. The original order is preserved (first occurrence kept). The original list is not modified. Passing a list of non-string pointer elements (e.g. `List<List<T>>`, `List<Map<K, V>>`, `List<Set<T>>`, `List<fn(...) -> R>`) is a compile error: `distinct() is only supported for lists of primitive values or strings`.

```ry
xs = [1, 2, 3, 2, 1, 4]
print(distinct(xs))   # [1, 2, 3, 4]
print(xs)             # [1, 2, 3, 2, 1, 4] (unchanged)
```

### flat

Flattens a nested list (list of lists) by one level. Returns a new list. The original list is not modified. Passing a non-nested list (e.g. `List<int>`) is a compile error: `flat() requires a list of lists`.

```ry
xs = [[1, 2], [3, 4]]
print(flat(xs))   # [1, 2, 3, 4]
print(xs)         # [[1, 2], [3, 4]] (unchanged)
```

### In-Place Mutating Variants

Some list operations have two forms: a non-mutating version that returns a new list, and an in-place variant whose name ends with `!`. Use the `!` form when you intend to mutate the receiver and want to make that intent explicit at the call site; use the non-mutating form when you want to preserve the original.

| In-place (`!`) | Non-mutating equivalent | Difference |
|------|-----|-----|
| `append!(xs, v)` / `append(xs, v)` | `appended(xs, v)` | `append!` / `append` mutate in place and return `Unit`; `appended` returns a new list |
| `sort!(xs)` / `sort!(xs, cmp)` | `sort(xs)` / `sort(xs, cmp)` | `sort!` mutates `xs` in place; `sort` returns a new sorted list |
| `reverse!(xs)` | `reverse(xs)` | `reverse!` mutates `xs` in place; `reverse` returns a new reversed list |

All `!` variants participate in Copy-on-Write: if `xs` is shared (reference count > 1), the outer buffer is cloned once before the in-place mutation so that aliases are not affected (see [Copy-on-Write (CoW) Semantics](#copy-on-write-cow-semantics)).

```ry
xs = [3, 1, 2]
sort!(xs)
print(xs)         # [1, 2, 3]

ys = [1, 2, 3]
reverse!(ys)
print(ys)         # [3, 2, 1]

zs = [1, 2]
append!(zs, 3)
print(zs)         # [1, 2, 3]

# Non-mutating variant: appended returns a new list
a = [1, 2]
b = appended(a, 3)
print(a)          # [1, 2] (unchanged)
print(b)          # [1, 2, 3]
```

**When to use which**:

- Prefer `sort`, `reverse`, `appended` when readers benefit from seeing that the original is preserved (e.g. functional pipelines, or when the original is still needed afterwards).
- Prefer `sort!`, `reverse!`, `append!` when the original is genuinely being replaced, to avoid the allocation of an intermediate copy and to make the mutation explicit at the call site.

### Operation Complexity

| Operation | Complexity |
|------|--------|
| `xs[i]` index access | O(1) |
| `append` / `append!` | O(1) amortized |
| `pop` | O(1) |
| `first`, `last` | O(1) |
| `insert`, `removeAt` | O(n) |
| `sort` / `sort!` | O(n log n) |
| `take` | O(n) |
| `tap` | O(n) |
| `filter`, `map`, `reduce`, `fold`, `sequence` | O(n) |
| `reverse` / `reverse!` | O(n) |
| `distinct` | O(n) |
| `len` | O(1) |

### Constraints and Errors

| Constraint | Details |
|------|------|
| All elements must be the same type | Mixed types cause a compile error |
| Empty list `[]` | Requires type annotation (e.g., `xs: List<int> = []`) |
| Out-of-range access | Runtime error (process terminates with exit code 1) |

---

## Copy-on-Write (CoW) Semantics

All collection types (List, Map, Set) use **Copy-on-Write** semantics when managed by ARC. This means:

- **Assignment shares data**: `b = a` does not copy the collection — both variables reference the same data. The reference count is incremented.
- **Mutation triggers a path-walking copy**: When a shared collection is mutated, every level on the LHS path whose reference count > 1 is cloned before the mutation lands. A top-level write (`b.append(...)`, `b[i] = v` with `b` shared) clones only the outermost container. A chained write (`b[i][j] = v`, `r.items[i] = v`, `m[k1][k2] = v`) walks from the root down and clones each intervening container that is still shared, so aliases of the outer container — or any inner container on the path — are isolated from the mutation.
- **Unique owners mutate in-place**: When a collection (or the container at some hop) has only one reference (`strong_count == 1`), the CoW check skips the clone for that level and mutates in-place with zero copy overhead.

```ry
a = [1, 2, 3]       # strong_count = 1
b = a                # strong_count = 2 (shared)
append(b, 4)         # strong_count > 1 → copy outer buffer, then mutate
                     # a = [1, 2, 3]  (strong_count = 1)
                     # b = [1, 2, 3, 4]  (strong_count = 1, new allocation)

c = [10, 20]         # strong_count = 1
append(c, 30)        # strong_count == 1 → mutate in-place (no copy)
```

### Path Copy-on-Write

Path CoW handles chained writes through nested collections — the writer walks
from the LHS root variable (or record field) down to the leaf write site and
privatizes each level whose reference count is greater than one. This gives
strict aliasing isolation between aliases sharing the outer container and any
inner container on the write path.

```ry
a = [[1, 2], [3, 4]]
b = a                    # outer list shared: strong_count = 2
b[0][0] = 99             # walks: clone outer (a and b now have their own
                         # outer); clone b's inner[0] (a's inner[0] keeps [1, 2]);
                         # write 99 into the clone
# a = [[1, 2], [3, 4]]
# b = [[99, 2], [3, 4]]
```

Record fields with ARC-managed collection values participate in path CoW
the same way as direct index hops. Record-to-record assignment (`r2 = r1`)
retains each ARC field so a subsequent `r2.items[i] = v` observes the
refcount > 1 at the field slot and clones before mutating:

```ry
record Box:
  items: List<int>

r1 = Box([1, 2, 3])
r2 = r1
r2.items[0] = 99
# r1.items[0] == 1
# r2.items[0] == 99
```

**Not supported** as the root of a path-CoW chain: method-call lvalues
(`f().x[i] = v`) and chains that interleave an index hop inside a record
field walk (`rec.arr[0].items[i] = v`). These shapes produce a compile-time
error; assign the intermediate value to a local variable first.

### Operations that trigger CoW

| Type | Mutating operations |
|------|-------------------|
| **List** | `append()` / `append!()`, `pop()`, `insert()`, `remove()`, `removeAt()`, `sort!()`, `reverse!()`, index assignment (`xs[i] = val`) |
| **Map** | `remove()`, index assignment (`m[key] = val`) |
| **Set** | `add()`, `remove()` |

Non-mutating operations (`appended`, `slice`, `take`, `filter`, `map`, `sort`, `reverse`, `get`, `items`, etc.) create new collections and do not trigger CoW.

---

## Map

### Overview

A key-value mapping. Allocated on the heap.

### Syntax

```ry
m = {"a": 1, "b": 2}
m: Map<str, int> = {"a": 1, "b": 2}
m = {"a": 1, "b": 2,}         # trailing comma allowed
```

### Equality

Maps support `==` and `!=`. Two maps are equal when they have the same number of entries and every key-value pair in one map exists with an equal value in the other.

```ry
{"a": 1, "b": 2} == {"a": 1, "b": 2}   # true
{"a": 1}         != {"a": 2}            # true (different values)
{"a": 1}         != {"b": 1}            # true (different keys)
```

### Key Access

```ry
m = {"a": 1, "b": 2}
print(m["a"])   # 1
```

Accessing a missing key aborts at runtime. Use `m[k]?` or `get(m, k)` for safe lookup.

### Safe Key Access (`m[k]?`)

`m[k]?` returns `Option<V>` instead of aborting on a missing key. This is a dedicated postfix syntax — not a sugar for `get(m, k)` — and follows the same shape on `List<T>`. See [operators.md](operators.md#option-returning-index-form-mk--xsi) for the cross-collection spec.

```ry
m = {"a": 1, "b": 2}
case m["a"]?:
  Some(v): print(v)            # 1
  None:    print("missing")
print(m["z"]? ?? -1)            # -1 — coalesce with default (note the space before ??)
```

Restrictions: read-only — `m[k]? = v` is a compile error (use `m[k] = v` to insert / update).

For an explicit named-function alternative with the same Option-returning semantics, use [`get(m, k)`](#get-1).

### Insert and Update

```ry
m = {"a": 1}
m["b"] = 2     # Insert new entry
m["a"] = 99    # Update existing entry
```

### len

```ry
m = {"a": 1, "b": 2, "c": 3}
print(len(m))   # 3
```

### print

```ry
m = {"a": 1, "b": 2}
print(m)   # {a: 1, b: 2}
```

### hasKey

```ry
m = {"a": 1, "b": 2}
print(m.hasKey("a"))   # true
print(m.hasKey("z"))   # false
```

### contains

Equivalent to `hasKey`. Both free-function and UFCS forms are available.

```ry
m = {"a": 1, "b": 2}
print(contains(m, "a"))   # true
print(m.contains("a"))    # true (UFCS)
print(m.contains("z"))    # false
```

### keys

Returns a list of all keys in the map.

```ry
m = {"a": 1, "b": 2, "c": 3}
print(keys(m))   # ["a", "b", "c"]
```

### values

Returns a list of all values in the map.

```ry
m = {"a": 1, "b": 2, "c": 3}
print(values(m))   # [1, 2, 3]
```

### items

Returns a list of `(key, value)` tuples for all entries in the map.

```ry
m = {"a": 1, "b": 2}
pairs = items(m)
# pairs = [("a", 1), ("b", 2)]
```

### remove (Map)

Removes the entry with the specified key from the map. Does nothing if the key does not exist.

```ry
m = {"a": 1, "b": 2}
remove(m, "a")
print(m)   # {b: 2}
```

### get

Two overloads are available:

- `get(map, key) -> Option<V>`: Returns `Some(value)` if the key exists, or `None` if it does not.
- `get(map, key, default) -> V`: Returns the value for the key, or `default` if the key does not exist. The `default` expression is evaluated lazily — it runs only when the key is missing, so passing a function call as the default does not invoke it for keys that are present.

```ry
m = {"a": 1, "b": 2}
print(get(m, "a"))       # Some(1)
print(get(m, "z"))       # None
print(get(m, "a", 0))   # 1
print(get(m, "z", 0))   # 0
```

### Merge with `+` and `+=`

`+` returns a new map that combines all entries from both maps; the original maps are not modified. When keys overlap, values from the right-hand side take precedence (rhs-wins). `+=` rebinds the left-hand variable to the merged result (`m1 += m2` is equivalent to `m1 = m1 + m2`).

```ry
m1 = {"a": 1, "b": 2}
m2 = {"b": 99, "c": 3}
m3 = m1 + m2
print(m3["a"])   # 1
print(m3["b"])   # 99  ← rhs wins
print(m3["c"])   # 3

m1 += m2         # same as m1 = m1 + m2
```

Both maps must have the same key type and value type.

### merge

Equivalent to `m1 + m2`. Returns a new map combining all entries from both maps with rhs-wins semantics. The original maps are not modified.

```ry
m1 = {"a": 1, "b": 2}
m2 = {"b": 99, "c": 3}
m3 = merge(m1, m2)
print(m3["a"])   # 1
print(m3["b"])   # 99
print(m3["c"])   # 3
```

### Constraints and Errors

| Constraint | Details |
|------|------|
| All keys must be the same type | Mixed key types cause a compile error |
| All values must be the same type | Mixed value types cause a compile error |
| Empty map | Type annotation is required (e.g., `m: Map<str, int> = {"a": 1}`) |
| Accessing a non-existent key | Runtime error (process terminates with exit code 1) |
| Key lookup | Hash table (O(1) average) |
| Capacity overflow | Automatically doubles in size |

---

## Set

### Overview

A collection that holds elements of the same type without duplicates. Allocated on the heap.

### Syntax

```ry
s = {1, 2, 3}
s: Set<int> = {1, 2, 3}
s = {1, 2, 3,}               # trailing comma allowed
```

### Supported Element Types

`int`, `float`, `bool`, `str`

### Equality

Sets support `==` and `!=`. Equality is order-independent — two sets are equal when they contain exactly the same elements.

```ry
{1, 2, 3} == {3, 2, 1}   # true (order does not matter)
{1, 2}    != {1, 2, 3}   # true (different sizes)
```

### in Operator (Membership Check)

```ry
s = {1, 2, 3}
print(2 in s)   # true
print(5 in s)   # false
```

### len

```ry
s = {1, 2, 3}
print(len(s))   # 3
```

### print

```ry
s = {1, 2, 3}
print(s)   # {1, 2, 3}
```

### add (Add Element)

Duplicate elements are ignored when added.

```ry
s = {1, 2, 3}
s.add(4)         # Add
s.add(1)         # Ignored because it already exists
print(len(s))    # 4
```

### remove (Remove Element)

```ry
s = {1, 2, 3}
s.remove(2)
print(2 in s)   # false
```

### for Iteration

```ry
s = {10, 20, 30}
for x in s:
    print(x)
```

### Empty Set

An empty set requires a type annotation.

```ry
s: Set<int> = {}
```

### Function Parameters

```ry
fn hasValue(s: Set<int>, v: int) -> bool:
    return v in s
```

### Union with `+` and `+=`

`+` returns a new set containing all elements from both sets; the original sets are not modified. `+=` rebinds the left-hand variable to the union result (`a += b` is equivalent to `a = a + b`).

```ry
a = {1, 2, 3}
b = {3, 4, 5}
c = a + b            # {1, 2, 3, 4, 5}

a += b               # same as a = a + b
```

Both sets must have the same element type.

### union

Equivalent to `s1 + s2`. Returns a new set containing all elements from both sets. The original sets are not modified.

```ry
a = {1, 2, 3}
b = {3, 4, 5}
print(union(a, b))   # {1, 2, 3, 4, 5}
```

### intersection

Returns a new set containing only elements present in both sets.

```ry
a = {1, 2, 3}
b = {2, 3, 4}
print(intersection(a, b))   # {2, 3}
```

### difference

Returns a new set containing elements in the first set but not in the second.

```ry
a = {1, 2, 3}
b = {2, 3, 4}
print(difference(a, b))   # {1}
```

### symmetricDifference

Returns a new set containing elements that are in either set but not in both.

```ry
a = {1, 2, 3}
b = {2, 3, 4}
print(symmetricDifference(a, b))   # {1, 4}
```

### isSubset

Returns `true` if all elements of the first set are contained in the second set.

```ry
a = {1, 2}
b = {1, 2, 3}
print(isSubset(a, b))   # true
print(isSubset(b, a))   # false
```

### isSuperset

Returns `true` if the first set contains all elements of the second set.

```ry
a = {1, 2, 3}
b = {1, 2}
print(isSuperset(a, b))   # true
print(isSuperset(b, a))   # false
```

### Constraints and Errors

| Constraint | Details |
|------|------|
| All elements must be the same type | Mixed types cause a compile error |
| Empty set `{}` | Type annotation is required |
| Element lookup | Hash table (O(1) average) |
| Capacity overflow | Automatically doubles in size |

---

## Iterator

### Overview

A lazy iterator abstraction that enables efficient data transformation pipelines. Iterators do not copy or materialize intermediate results — each element is processed on demand.

### Creating Iterators

Use `iter()` to create an iterator from any collection:

```ry
xs = [1, 2, 3, 4, 5]
it = xs.iter()           # Iterator<int>

s = {10, 20, 30}
sit = s.iter()           # Iterator<int>

m = {"a": 1, "b": 2}
mit = m.iter()           # Iterator<(str, int)>
```

### Lazy Method Chaining

Iterator methods return new iterators, forming a pipeline that is only evaluated when consumed:

| Method | Description |
|--------|-------------|
| `.filter(function)` | Yields only elements where the predicate returns `true` |
| `.map(function)` | Transforms each element using the given function |
| `.take(count)` | Yields at most `count` elements |

```ry
result = [1, 2, 3, 4, 5]
    .iter()
    .filter((x: int) => x > 2)
    .map((x: int) => x * 2)
    .take(2)
    .toList()   # [6, 8]
```

### Consuming Iterators

| Method | Description |
|--------|-------------|
| `.toList()` | Collects all elements into a `List<T>` |
| `.next()` | Returns the next element as `Option<T>` |

```ry
it = [10, 20].iter()
print(it.next())   # Some(10)
print(it.next())   # Some(20)
print(it.next())   # None
```

### For Loop Support

Iterators can be used directly in `for` loops:

```ry
for x in [1, 2, 3].iter().filter((x: int) => x > 1):
    print(x)
# 2
# 3

for k, v in {"a": 1, "b": 2}.iter():
    print(k)
```
