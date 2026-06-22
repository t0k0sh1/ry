# Operator Reference

## Precedence Table

Lower numbers indicate higher precedence (evaluated first).

| Precedence | Operator | Description | Associativity |
|---|---|---|---|
| 0 | `?` | Error propagation (postfix) | Left |
| 1 | `()` | Grouping | -- |
| 2 | `+x` `-x` `~x` | Unary plus, unary minus, bitwise NOT | Right |
| 3 | `**` | Exponentiation | Right |
| 3.5 | `as` | Type cast | Left |
| 4 | `*` `/` `%` `//` | Multiplication, division, modulo, integer division | Left |
| 5 | `+` `-` | Addition, subtraction | Left |
| 6 | `<<` `>>` `>>>` | Bit shift | Left |
| 7 | `&` | Bitwise AND | Left |
| 8 | `^` | Bitwise XOR | Left |
| 9 | `\|` | Bitwise OR | Left |
| 9.5 | `..` | Range (inclusive) | Left |
| 10 | `==` `!=` `<` `<=` `>` `>=` `in` `not in` | Comparison, membership | Left |
| 11 | `not` | Logical NOT | Right |
| 12 | `and` | Logical AND | Left |
| 13 | `or` | Logical OR | Left |
| 13.5 | `??` | Null coalescing | Left |

## Arithmetic Operators

| Operator | Description | Example |
|---|---|---|
| `+` | Addition / string concatenation | `1 + 2` -> `3`, `"a" + "b"` -> `"ab"`, `"x" + 1` -> `"x1"` |
| `-` | Subtraction | `5 - 3` -> `2` |
| `*` | Multiplication / string repetition | `4 * 3` -> `12`, `"ab" * 3` -> `"ababab"` |
| `/` | Division: `int`/`float` → always `float` (IEEE 754); low-level integers (`i32`, `u8`, …) → integer division, same type | `7 / 2` -> `3.5`, `7i32 / 2i32` -> `3i32`, `7 / 0` -> `inf`, `0 / 0` -> `nan` |
| `//` | Floor division (toward -∞) | `7 // 2` -> `3`, `-7 // 2` -> `-4` |
| `%` | Modulo | `7 % 3` -> `1` |
| `**` | Exponentiation (always float) | `2 ** 10` -> `1024.0` |
| `-x` | Unary minus | `-5`, `-3.14` |
| `+x` | Unary plus | `+5` (no sign change) |

```ry
a = 10 // 3    # 3 (int)
b = 10 / 3     # 3.3333... (float)
c = 2 ** 8     # 256.0 (float)
s = "foo" + "bar"  # "foobar"
t = "val=" + 42    # "val=42"
u = 3.14 + "!"    # "3.14!"
```

When one operand of `+` is `str` and the other is `int`, `float`, or `bool`, the non-`str` operand is automatically converted to its string representation and concatenated.

For `int` and `float` operands, `/` always produces a `float` and follows IEEE 754: `x / 0` evaluates to `±inf` (sign follows the dividend), and `0 / 0` evaluates to `nan`. For `int` operands, `//` and `%` retain integer semantics and raise a runtime error when the divisor is zero, or when the dividend is the `int` minimum and the divisor is `-1` (the result would overflow `int`). For low-level integer types (`i8`..`i64`, `u8`..`u64`), `/` performs integer division and returns the same type (mixing low-level and `int` in one expression is a type error).

## Comparison Operators

All return `bool`.

| Operator | Description |
|---|---|
| `==` | Equal |
| `!=` | Not equal |
| `<` | Less than |
| `<=` | Less than or equal |
| `>` | Greater than |
| `>=` | Greater than or equal |

- Can be used with numeric types (int / float) and bool.
- `str` values are compared lexicographically (byte order).
- Record types support `==` and `!=` with auto-generated field-by-field comparison (see [Record Reference](records.md#comparison--)).
- Tuple types support `==` and `!=` with element-wise comparison.
- `List<T>` and `Map<K, V>` support `==` and `!=` for all element/value types including records, tuples, and nested collections (`List<List<T>>`, `Map<str, List<T>>`, `Map<Point, int>`, `Map<(int, int), str>`, etc.). Map key types may be primitive or complex (records, tuples, nested collections); function-typed keys are not supported.
- `Set<T>` supports `==` and `!=` for all element types including records, tuples, and nested collections (`Set<Point>`, `Set<List<int>>`, `Set<Set<int>>`, etc.). Comparison is order-independent (set semantics). Note: element types must themselves be equatable (closures are not supported).
- The `in` operator is used for membership checks on sets, lists, and maps (`x in s`), and for substring checks on strings (`sub in s`).
- The `not in` operator is the negation of `in` (`x not in s`).
- For maps, `in` checks whether the key exists.
- For strings, `in` returns `true` when the left operand is a substring of the right operand. An empty string is always a substring of any string.
- For lists, passing a list of non-string pointer elements (e.g. `List<List<T>>`, `List<Map<K, V>>`, `List<Set<T>>`, `List<fn(...) -> R>`) is a compile error: `'in' operator is only supported for lists of primitive values or strings` (and the analogous error for `not in`). `List<any>` supports heterogeneous primitive values (`int`, `float`, `bool`, `str`) via semantic equality; collection-, function-, and resource-backed elements are not accepted by `any` itself (see [types.md — any Type](types.md#any-type)).

```ry
x = 3 < 5       # true
y = "abc" < "abd"  # true (lexicographic)
s = {1, 2, 3}
z = 2 in s      # true
w = 4 not in s  # true
xs = [1, 2, 3]
a = 2 in xs     # true (list linear search)
m = {"a": 1}
b = "a" in m    # true (map key lookup)
c = "world" in "hello world"  # true (substring check)
d = "xyz" not in "hello world"  # true
e = "" in "hello"  # true (empty string is always a substring)
```

## Logical Operators

| Operator | Description | Type |
|---|---|---|
| `and` | Logical AND | `bool` x `bool` -> `bool` |
| `or` | Logical OR | `bool` x `bool` -> `bool` |
| `not` | Logical NOT | `bool` -> `bool` |

```ry
a = true and false   # false
b = true or false    # true
c = not true         # false
```

## Bitwise Operators

Only available for `int` type. Applying to `float` or `bool` causes a compile error.

| Operator | Description | Example |
|---|---|---|
| `&` | Bitwise AND | `0b1100 & 0b1010` -> `0b1000` |
| `\|` | Bitwise OR | `0b1100 \| 0b1010` -> `0b1110` |
| `^` | Bitwise XOR | `0b1100 ^ 0b1010` -> `0b0110` |
| `~` | Bitwise NOT (unary) | `~0` -> `-1` |
| `<<` | Left shift | `1 << 4` -> `16` |
| `>>` | Arithmetic right shift | `16 >> 2` -> `4` |
| `>>>` | Logical right shift | `-1 >>> 1` -> `9223372036854775807` |

```ry
flags = 0b0001 | 0b0010   # 3
masked = flags & 0b0011   # 3
shifted = 1 << 8          # 256
```

## Error Propagation Operator (`?`)

The postfix `?` operator has two forms:

1. **Unwrap / short-circuit form**: applied to a `Result<T, E>` or `Option<T>` expression — unwraps the inner value on the happy path and short-circuits on the unhappy path.
2. **Option-returning index form**: applied directly to a Map or List index expression `m[k]?` / `xs[i]?` — returns `Option<V>` / `Option<T>` instead of aborting on missing key / out-of-range index.

### Unwrap / short-circuit form

| Operand | Happy path | Unhappy path |
|---|---|---|
| `Result<T, E>` | evaluates to the `Ok` inner value `v` | the enclosing function returns `Err(e)` |
| `Option<T>` | evaluates to the `Some` inner value `v` | the enclosing function returns `None` |

When used inside a function, the operand type must match the enclosing function's return type:

- `?` on a `Result` value requires the enclosing function to return `Result`.
- `?` on an `Option` value requires the enclosing function to return `Option`.

```ry
fn safeDivide(a: int, b: int) -> Result<int, Error>:
    if b == 0:
        return Err(Error("division by zero"))
    return Ok(a // b)

fn compute(a: int, b: int, c: int) -> Result<int, Error>:
    x = safeDivide(a, b)?    # returns Err early if b == 0
    y = safeDivide(x, c)?
    return Ok(y + 1)

fn safeGet(xs: List<int>, i: int) -> Option<int>:
    if i < 0 or i >= xs.len():
        return none
    return Some(xs[i])

fn firstPlusSecond(xs: List<int>) -> Option<int>:
    a = safeGet(xs, 0)?    # returns None early if out of range
    b = safeGet(xs, 1)?
    return Some(a + b)
```

### Option-returning index form (`m[k]?` / `xs[i]?`)

When `?` immediately follows a Map or List index expression, it changes the semantics from "abort on miss" to "produce an `Option`":

| Receiver | `m[k]` / `xs[i]` (no `?`) | `m[k]?` / `xs[i]?` |
|---|---|---|
| `Map<K, V>` — key present | `V` | `Some(v): Option<V>` |
| `Map<K, V>` — key missing | runtime abort | `None: Option<V>` |
| `List<T>` — index in range | `T` (after negative-index wrap) | `Some(v): Option<T>` |
| `List<T>` — index out of range (after wrap) | runtime abort | `None: Option<T>` |

```ry
m = {"a": 1, "b": 2}
case m["a"]?:
  Some(v): print(v)        # 1
  None:    print("none")
case m["z"]?:
  Some(v): print(v)
  None:    print("none")   # none

# Combine with the `??` null-coalescing operator for a default value.
# IMPORTANT: leave a space between the two operators.
print(m["z"]? ?? -1)       # -1

xs = [10, 20, 30]
print(xs[1]?)              # Some(20)
print(xs[100]?)            # None
print(xs[-1]?)             # Some(30)  — negative-index wrap is preserved
print(xs[-100]?)           # None       — wrap result still out of range
```

Scope and restrictions (v0.0.25):

- Receivers supported: `Map<K, V>` and `List<T>`.
- Receivers rejected at compile time: `str` (use `charAt(s, i)`), fixed-length arrays (use `as int` cast on the bounds-checked index), range slice `xs[a..b]?` (no Option-wrapped slice yet), `any`-typed nested `v[k]?` (tracked as #1701).
- Write-form `m[k]? = v` (and `m[k]?.field = v`, `mm[k]?[k2] = v`) is a compile error — `?` produces an Option, not a slot.
- Negative-index wrap applies first; the `?` only catches the post-wrap out-of-range case (so `xs[-1]?` on a non-empty list always returns `Some(last)`).

**Footgun — `??` greedy tokenization**: The lexer tokenizes `??` greedily as a single `QuestionQuestion` token. Writing `m["k"]?? default` (no space between `?` and `?`) parses as `m["k"]` (which aborts on a missing key) followed by `?? default`. To get the Option-returning form coalesced with a default, **always leave a space**: `m["k"]? ?? default`.

```ry
m = {"a": 1}
print(m["a"]? ?? 99)       # 1   — Option-returning form coalesces missing → 99
# print(m["z"]?? 99)       # ABORT — `m["z"]` aborts before `??` is reached
```

### At the top level

`?` can also be used directly at the top level of a script. There, `Err(e)` and `None` are treated as fatal errors: the error message is written to stderr and the process exits with status `1`.

```ry
fn mk() -> Result<int, Error>:
    return Err(Error("something broke"))

v = mk()?   # prints "error: something broke" to stderr and exits with 1

x: int? = none
y = x?      # prints "error: unexpected None" to stderr and exits with 1
```

At the top level, a `Result`'s `Err` type must be `Error` (so its `message` field can be printed).

---

## `case:` Conditional Expression

```ry
x = case:
    condition : trueValue
    _ : falseValue
```

Evaluates conditions from top to bottom and returns the expression from the first truthy arm. All result expressions must have the same type. The `_ :` wildcard arm is required, so the expression always produces a value.

```ry
x = case:
    3 > 2 : 10
    _ : 20     # 10

s = case:
    false : "yes"
    _ : "no"  # "no"

# Nested ternaries flatten into multiple arms
y = case:
    true : 2
    false : 1
    _ : 3     # 2
```

---

## Range Operator

The `..` operator creates an inclusive integer range.

```ry
xs = 1 .. 5    # [1, 2, 3, 4, 5]

for i in 1 .. 3:
    print(i)       # 1 2 3
```

The result is a `List<int>` containing all integers from the left operand to the right operand (inclusive).

When used as a list index (`lst[a..b]`), the range denotes an inclusive subrange: negative `a`/`b` wrap against the list length (like `lst[-1]`), and out-of-bounds bounds are clamped. `lst[a..b]` is equivalent to `slice(lst, a, b + 1)`. Non-list receivers (`str`, maps, fixed-length arrays) reject range-indexing at compile time.

```ry
xs = [10, 20, 30, 40, 50]
print(xs[1..3])    # [20, 30, 40]
print(xs[-2..-1])  # [40, 50]
```

---

## Null Coalescing Operator (`??`)

```ry
x = optionalVal ?? defaultVal
```

The `??` operator accepts either an `Option<T>` or a `Result<T, E>` on the left-hand side:

| Left-hand side | Result |
|---|---|
| `Some(v)` | `v` |
| `None` | `defaultVal` |
| `Ok(v)` | `v` |
| `Err(_)` | `defaultVal` (the error value is discarded) |

The right-hand operand must have the same type as the `Option`'s inner type (or the `Result`'s `Ok` type).

```ry
a: int? = Some(10)
b: int? = none

print(a ?? 0)    # 10
print(b ?? 0)    # 0

# int is a stdlib builtin returning Result<int, Error>
i = int("42") ?? -1      # 42 on success, -1 on Err
j = int("nope") ?? -1    # -1 — the Err value is discarded
```

---

## Compound Assignment Operators

Shorthand for updating a variable. `x op= y` is equivalent to `x = x op y`.

| Operator | Equivalent Expression |
|---|---|
| `x += y` | `x = x + y` |
| `x -= y` | `x = x - y` |
| `x *= y` | `x = x * y` |
| `x /= y` | `x = x / y` |
| `x %= y` | `x = x % y` |
| `x //= y` | `x = x // y` |
| `x **= y` | `x = x ** y` |
| `x &= y` | `x = x & y` |
| `x \|= y` | `x = x \| y` |
| `x ^= y` | `x = x ^ y` |
| `x <<= y` | `x = x << y` |
| `x >>= y` | `x = x >> y` |

```ry
x = 10
x += 5    # x = 15
x -= 3    # x = 12
x *= 2    # x = 24
x //= 3  # x = 8
x &= 6   # x = 0
```

Compound assignment is allowed on any lvalue — plain variables, list or map
elements, record fields, and arbitrarily nested chains:

```ry
xs = [1, 2, 3]
xs[0] += 10              # list element

record Point:
  x: int
  y: int
p = Point(1, 2)
p.x *= 5                 # record field

pts = [Point(1, 2), Point(3, 4)]
pts[0].x -= 1            # chained: list-of-records field
```

For collection types, `+=` uses the collection's `+` semantics:

```ry
# List concatenation
xs: List<int> = [1, 2]
xs += [3, 4]             # xs = [1, 2, 3, 4]

# Map merge (rhs-wins on key collision)
m: Map<str, int> = {"a": 1}
m += {"a": 99, "b": 2}  # m = {"a": 99, "b": 2}

# Set union
s: Set<int> = {1, 2}
s += {2, 3}              # s = {1, 2, 3}
```

Each index expression on a chained LHS is evaluated exactly once. Compound
assignment to a missing map key (`m["absent"] += 1`) is a runtime error.

## Increment / Decrement Operators

Postfix-only, statement-level operators for incrementing or decrementing a variable by 1. These are desugared to `x = x + 1` and `x = x - 1` respectively.

| Operator | Equivalent Expression |
|---|---|
| `x++` | `x = x + 1` |
| `x--` | `x = x - 1` |

```ry
count = 0
count++       # count = 1
count++       # count = 2
count--       # count = 1

f = 1.5
f++           # f = 2.5 (int 1 is promoted to float)
```

> **Note**: `++` / `--` can only be used as statements, not as expressions.
> `@const` variables cannot be incremented/decremented (immutability is enforced).

---

## Type Rules for Operations

| Operation | Left Type | Right Type | Result Type |
|---|---|---|---|
| `+ - *` | int | int | int |
| `+ - *` | float | int / float | float |
| `+ - *` | int | float | float |
| `/` | any numeric | any numeric | float |
| `//` | int | int | int |
| `//` | float or int (one is float) | -- | float |
| `**` | any numeric | any numeric | float |
| `%` | int | int | int |
| `%` | float or int (one is float) | -- | float |
| `+` | str | str | str |
| `+` | str | int / float / bool | str |
| `+` | int / float / bool | str | str |
| `+` | List\<T\> | List\<T\> | List\<T\> (concatenation) |
| `+` | Map\<K, V\> | Map\<K, V\> | Map\<K, V\> (merge, rhs-wins) |
| `+` | Set\<T\> | Set\<T\> | Set\<T\> (union) |
| `== != < <= > >=` | numeric / bool / str | same type | bool |
| `*` | str | int | str |
| `in` | any | Set\<T\> / List\<T\> / Map\<K, V\> | bool |
| `in` | str | str | bool (substring check) |
| `not in` | any | Set\<T\> / List\<T\> / Map\<K, V\> | bool |
| `not in` | str | str | bool (substring check) |
| `& \| ^ ~ << >> >>>` | int | int | int |
| `and or not` | bool | bool | bool |

> **Note:** `bool` is not a numeric type. Using `bool` as an operand of arithmetic
> operators (`+`, `-`, `*`, `/`, `//`, `%`, `**`, unary `-`) or bitwise operators
> (`&`, `|`, `^`, `~`, `<<`, `>>`) is a compile error. Use `bool as int` to
> explicitly convert before arithmetic or bitwise operations.

## Operator Overloading

You can define operator behavior for user-defined types.

### Syntax

```ry
# Binary operator (2 parameters)
fn operator+(a: MyType, b: MyType) -> MyType:
    ...

# Unary operator (1 parameter)
fn operator-(a: MyType) -> MyType:
    ...
```

### Overloadable Operators

| Category | Operators |
|---|---|
| Arithmetic (binary) | `+` `-` `*` `/` `%` `**` `//` |
| Comparison (binary) | `==` `!=` `<` `<=` `>` `>=` |
| Bitwise (binary) | `&` `\|` `^` `<<` `>>` `>>>` |
| Logical (binary) | `and` `or` |
| Membership | `in` |
| Subscript | `[]` (read), `[]=` (write) |
| Call | `()` |
| Cast | `as` |
| Unary | `-` `~` `not` |
| Compound assignment | `+=` `-=` `*=` `/=` `%=` `//=` `**=` `&=` `\|=` `^=` `<<=` `>>=` |

### Return Type Constraints

Comparison and logical operators must return `bool`:

| Category | Operators | Required Return Type |
|---|---|---|
| Comparison | `==` `!=` `<` `<=` `>` `>=` | `bool` |
| Logical | `and` `or` `not` | `bool` |
| Membership | `in` | `bool` |
| Cast | `as` | Required (target type) |

```ry
# OK
fn operator==(a: Vec2, b: Vec2) -> bool:
    return a.x == b.x and a.y == b.y

# Error: comparison operator '==' must return 'bool', but returns 'int'
fn operator==(a: Vec2, b: Vec2) -> int:
    return 42
```

Arithmetic and bitwise operators have no return type constraint.

### Distinguishing Binary and Unary

Distinguished by the number of parameters.

```ry
# Binary -
fn operator-(a: Vec2, b: Vec2) -> Vec2:
    return Vec2(a.x - b.x, a.y - b.y)

# Unary -
fn operator-(v: Vec2) -> Vec2:
    return Vec2(-v.x, -v.y)
```

### Compound Assignment Operator Overloading

Compound assignment operators (`+=`, `-=`, etc.) can be independently overloaded. This enables in-place optimization for large data structures.

```ry
record Matrix:
    data: List
    rows: int
    cols: int

fn operator+=(a: Matrix, b: Matrix) -> Matrix:
    for i in range(len(a.data)):
        a.data[i] = a.data[i] + b.data[i]
    return a
```

#### Resolution Priority

When `x += y` is evaluated:

1. If `operator+=` is defined for the types → call it directly
2. If `operator+=` is not defined but `operator+` is → fall back to `x = x + y`
3. If neither is defined (for non-builtin types) → compile error

```ry
record Vec2:
    x: float
    y: float

fn operator+=(a: Vec2, b: Vec2) -> Vec2:
    return Vec2(a.x + b.x, a.y + b.y)

v = Vec2(1.0, 2.0)
v += Vec2(3.0, 4.0)  # calls operator+= directly
# v.x == 4.0, v.y == 6.0
```

Compound assignment operators require exactly 2 parameters and have no return type constraint.

### Subscript Operator Overloading

The `[]` (read) and `[]=` (write) operators enable custom subscript behavior for user-defined types. Multi-index access (e.g., `m[row, col]`) is supported.

```ry
record Grid:
    a: int
    b: int
    c: int
    d: int

# Read: requires 2+ parameters (object + indices)
fn operator[](g: Grid, row: int, col: int) -> int:
    if row == 0 and col == 0:
        return g.a
    if row == 0 and col == 1:
        return g.b
    if row == 1 and col == 0:
        return g.c
    return g.d

# Write: requires 3+ parameters (object + indices + value)
fn operator[]=(g: Grid, row: int, col: int, value: int):
    ...

g = Grid(1, 2, 3, 4)
print(g[0, 1])    # 2
g[1, 0] = 99
```

User-defined subscript operators are tried first; if no match is found, built-in subscript behavior (for lists, maps, and arrays) is used as a fallback.

### Membership Operator Overloading

The `in` operator can be overloaded to define custom membership tests. Must return `bool`.

```ry
record Range:
    lo: int
    hi: int

fn operator in(value: int, r: Range) -> bool:
    return value >= r.lo and value < r.hi

r = Range(1, 10)
print(5 in r)       # true
print(15 not in r)  # true
```

User-defined `in` operators are tried first; if no match is found, built-in behavior (for sets, maps, lists, and strings) is used as a fallback. `not in` is automatically supported when `in` is defined.

### Call Operator Overloading

The `()` operator enables records to behave as callable objects. Requires at least 2 parameters (object + arguments).

```ry
record Adder:
    base: int

fn operator()(a: Adder, x: int) -> int:
    return a.base + x

add5 = Adder(5)
print(add5(10))    # 15
```

When a variable holding a record value is called like a function, the compiler tries `operator()` overloads first. If no match is found, other call resolution strategies (functions, constructors, lambdas) take precedence.

### Cast Operator Overloading

The `as` operator can be overloaded to define custom type conversions. Takes exactly 1 parameter (the source value) and must specify a return type (the target type). Dispatch matches by source type and return type.

```ry
record Celsius:
    value: int

record Fahrenheit:
    value: int

fn operator as(c: Celsius) -> Fahrenheit:
    return Fahrenheit(c.value * 9 // 5 + 32)

c = Celsius(100)
f = c as Fahrenheit   # Fahrenheit(212)
```

The target type can be any type the compiler can resolve, including generic types:

```ry
record Temperature:
    value: int

fn operator as(t: Temperature) -> int?:
    return Some(t.value)

t = Temperature(42)
result: int? = t as int?   # Some(42)
```

User-defined `as` operators are tried first; if no match is found, built-in casts (int, float, bool, str, etc.) are used as a fallback.
