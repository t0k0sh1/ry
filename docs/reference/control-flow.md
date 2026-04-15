[English](control-flow.md) | [日本語](../ja/reference/control-flow.md) | [繁體中文](../zh/reference/control-flow.md)

# Control Flow Reference

## if / else

### Statement Syntax

```python
if condition:
    # then block
else:
    # else block (optional)
```

### Expression Forms

`if` can also be used as an expression that produces a value. Two forms are supported:

**Single-expression form** (`=>`):

```python
x = if condition => true_value else false_value
```

Examples:

```python
abs_val = if x > 0 => x else -x
label = if score >= 90 => "A" else "B"
```

The `else` branch in the single-expression form takes a value directly (without `=>`). Both branches must produce the same type, and `else` is required.

**Block form** (`:`):

```python
x = if condition:
    compute_something()
else:
    compute_other()
```

In the block form, each block must end with an expression statement (tail-expression semantics). The `else` branch is required, and both branches must produce the same type.

For multi-branch conditionals with values, use `case:` instead (see below).

### Condition Types

| Type | Falsy Value | Truthy Value |
|---|---|---|
| `bool` | `false` | `true` |
| `int` | `0` | non-zero |
| `float` | `0.0` | non-zero |

Only `bool`, integer, and `float` types may appear in a condition. `str`,
`List`, `Map`, `Set`, iterators, closures, records, `Option`, and `Result`
cannot be used directly as conditions. For collections and strings, write
the length check explicitly:

```python
xs = [1, 2, 3]
# ✗ error: value of this type cannot be used as a boolean condition
# if xs:
#     print("non-empty")
# ✓ explicit length check
if length(xs) > 0:
    print("non-empty")
# ✓ equivalent using is_empty
if not is_empty(xs):
    print("non-empty")
```

For `Option` and `Result`, pattern-match the variants explicitly with
`case` instead of using them as conditions. These rules apply equally to
`while`, `case` arms, and the unary `not` operator.

### Example

```python
x = 10

if x > 5:
    print("big")
else:
    print("small or equal")
```

### Scope Rules

- Each `if` / `else` block has its own independent block scope.
- Variables declared inside a block are not accessible outside the block.

```python
if true:
    y = 42
# y is not accessible here
```

---

## while

### Syntax

```python
while condition:
    # loop body
```

Repeats the loop body while the condition is `true`.

### Example

```python
i = 0
while i < 5:
    print(i)
    i += 1
```

### Combining with break / continue

```python
i = 0
while true:
    if i >= 3:
        break
    i += 1
```

---

## for

### Syntax

```python
# List / set iteration
for x in iterable_expr:
    # x is assigned each element

# range (starting from 0)
for i in range(n):
    # i = 0, 1, ..., n-1

# range (with start and end)
for i in range(start, end):
    # i = start, start+1, ..., end-1

# range (with step)
for i in range(start, end, step):
    # i = start, start+step, start+2*step, ...
```

### String Iteration

A `for` loop over a `str` yields each **Unicode code point** as a single-character `str`. Multi-byte UTF-8 sequences (including CJK characters and emoji) are decoded correctly; bytes within a multi-byte character are never split.

This is **code-point** iteration, not **grapheme-cluster** iteration: user-perceived characters that span multiple code points — combining-mark sequences (e.g., base letter + U+0301) and ZWJ emoji sequences (e.g., family or skin-tone compositions) — are yielded as several iterations, one per code point. If you need grapheme-cluster-aware iteration, decompose the string with a future segmentation helper rather than relying on `for c in s:`.

```python
for c in "hello":
    print(c)               # h, e, l, l, o

for c in "こんにちは":
    print(c)               # こ, ん, に, ち, は  (not individual bytes)

for c in "a🙂b":
    print(c)               # a, 🙂, b
```

The loop variable is typed as `str`, so you can pass it to other string functions:

```python
for c in "abc":
    print(to_upper(c))     # A, B, C
```

Iterating an empty string runs the loop body zero times. `enumerate` and `zip` also accept `str` arguments and yield the same code-point units:

```python
for i, c in enumerate("abc"):
    print(i, c)

for a, b in zip("abc", "xyz"):
    print(a + b)           # ax, by, cz
```

### Map Key-Value Iteration

```python
for k, v in map_expr:
    # k is the key, v is the value for each entry
```

### Tuple Destructuring

When iterating over a list of tuples, you can destructure into N variables matching the tuple's element count. Use `_` to discard a value.

```python
xs = [10, 20, 30]

for i, x in enumerate(xs):
    print(f"{i}: {x}")    # 0: 10, 1: 20, 2: 30

for a, b in zip([1, 2], [10, 20]):
    print(a + b)          # 11, 22

for _, x in enumerate(xs):
    print(x)              # index discarded

# N-element destructuring (3+ variables)
triples = [(1, 2, 3), (4, 5, 6)]
for a, b, c in triples:
    print(a + b + c)      # 6, 15

for a, _, c in triples:
    print(a + c)          # 4, 10 (middle element discarded)
```

### Range Operator (`..`)

The `..` operator creates an inclusive integer range. `1 .. 5` produces `[1, 2, 3, 4, 5]`.

```python
for i in 1 .. 5:
    print(i)     # 1 2 3 4 5
```

### Example

```python
xs = [10, 20, 30]
for x in xs:
    print(x)

s = {1, 2, 3}
for x in s:
    print(x)

for i in range(5):
    print(i)     # 0 1 2 3 4

for i in range(2, 6):
    print(i)     # 2 3 4 5

for i in range(0, 10, 2):
    print(i)     # 0 2 4 6 8

for i in range(10, 0, -3):
    print(i)     # 10 7 4 1

# Map iteration
m = {"a": 1, "b": 2}
for k, v in m:
    print(k)
    print(v)

# Range operator
for i in 1 .. 3:
    print(i)     # 1 2 3
```

---

## async / await

`async function` declares a function that runs concurrently. Calling an `async function` returns `Task<T>`. Use `await` inside another `async function` or `block_on()` from synchronous context to wait for the result.

```python
async function add(a: int, b: int) -> int:
    return a + b

# From synchronous context, use block_on()
t: Task<int> = add(20, 22)
print(block_on(t))                  # 42
print(block_on(add(1, 2)))          # 3

# Inside async function, use await
async function double_add(a: int, b: int) -> int:
    result = await add(a, b)
    return result * 2
```

### Rules

- `async function name(...) -> T:` is declared with the awaited result type `T`.
- Calling an `async function` immediately returns `Task<T>`.
- `await expr` requires `expr` to be `Task<T>` and produces `T`.
- `await` can only be used inside an `async function`. Use `block_on(task)` from synchronous context.
- `block_on(task)` blocks the current thread until the task completes and returns the result.
- `async function ... -> Unit` is supported; `block_on(task)` is the primary way to wait when no value is produced.
- Tasks run on the runtime worker pool; they are not implemented as one OS thread per task.
- `async` lambdas and `async @native function` are not supported in v1.

---

## `@parallel for`

`@parallel` can be attached only to counted `for` loops over `range(...)` or integer `..` ranges. The loop body runs in parallel chunks on the runtime worker pool.

```python
@parallel
for i in range(8):
    print(i)
```

### Constraints

- Only `range(...)` and integer `..` loops are supported.
- Destructuring iteration is not supported.
- Assigning to outer mutable bindings is rejected.
- `break` and `continue` are rejected.
- Indexed assignment and field assignment inside the loop body are rejected in v1.

Use `available_parallelism()` to inspect the runtime worker count.

---

## break

- Immediately exits the innermost loop (`while` or `for`).
- Using it outside a loop causes a compile error.

```python
for i in range(10):
    if i == 5:
        break    # Exits when i == 5
    print(i)     # 0 1 2 3 4
```

### Error Example

```python
# break outside a loop is a compile error
break   # Error: break outside loop
```

---

## continue

- Ends the current iteration of the innermost loop and skips to the next iteration.
- Using it outside a loop causes a compile error.

```python
for i in range(5):
    if i == 2:
        continue   # Skip i == 2
    print(i)       # 0 1 3 4
```

---

## `...` (Ellipsis)

- A no-op statement that does nothing. Used as a placeholder for empty blocks.
- Can be used in any block: function body, `if`/`else`, `while`, `for`, `case` arm, etc.

```python
function not_yet():
    ...

if true:
    ...
else:
    ...
```

---

## case

`case` unifies multi-branch conditional flow (formerly `when`) and pattern
matching (formerly `match`) into a single construct. Two forms are supported:

- `case:` — no subject, each arm is a condition expression (replaces `when:`)
- `case <expr>:` — with a subject, each arm is a pattern (replaces `match`)

Both forms support a block body (`:`) and an expression body (`=>`).

> **Note**: The `when` and `match` keywords were removed in favor of the
> unified `case` construct. Legacy Ry code using `when` / `match` must be
> migrated.

### case without subject

Use `case:` for multi-branch conditional flow without a subject value.

#### Syntax

```python
case:
    condition:
        # body
    condition:
        # body
    _:
        # fallback body
```

#### Example

```python
x = 0

case:
    x > 0:
        print("positive")
    x < 0:
        print("negative")
    _:
        print("zero")
```

The arms are evaluated from top to bottom and the first arm whose condition
is truthy is executed. The wildcard arm `_:` is optional for statements.

For the expression form of `case:`, see the Expression Forms section below.

---

## case with subject (pattern matching)

### Syntax

```python
case expression:
    pattern:
        # body
    pattern if guard_condition:
        # guarded body
    _:
        # wildcard (matches anything)
```

### Pattern Types

| Pattern | Example | Description |
|----------|-----|------|
| Wildcard | `_` | Matches anything |
| Literal | `0`, `"hello"`, `true` | Equality comparison |
| Variable binding | `n` | Matches anything and binds to a variable |
| enum variant | `Color::Red` | Compares enum tag (simple enum) |
| ADT enum variant | `Shape::Circle(r)` | Matches an enum variant with associated data and binds it |
| `Some(x)` | `Some(v)` | When Option has a value, binds the inner value |
| `None` | `None` | When Option has no value |
| `Ok(x)` | `Ok(v)` | When Result is Ok, binds the inner value |
| `Err(x)` | `Err(e)` | When Result is Err, binds the error value |
| Tuple pattern | `(a, b)`, `(1, n)` | Matches a tuple by element; binds, tests literals, or ignores (`_`) each position |
| OR pattern | `1 \| 2 \| 3` | Matches if any alternative matches |

### Guard Clause

A guard condition can be specified in the form `pattern if condition:`. The arm is executed only when the pattern matches and the guard condition is true.

### OR Pattern

Multiple patterns can be combined with `|` to match any of them. Variable bindings (`n`, `Some(x)`, `Ok(v)`, `Err(e)`) are not allowed in OR patterns.

```python
case x:
    1 | 2 | 3:
        print("small")
    _:
        print("other")

# Enum OR pattern
case color:
    Color::Red | Color::Blue:
        print("warm or cool")
    Color::Green:
        print("green")
```

### Exhaustiveness Checking

- enum types: Must cover all variants or include `_`. OR patterns count each alternative individually.
- Option types: Must cover both `Some` and `None` or include `_`.
- bool type: Must cover both `true` and `false` or include `_`.
- int / float / str literals: `_` is required.
- Guarded arms do not count toward exhaustiveness.

### Example

```python
# enum pattern match
enum Color:
    Red
    Green
    Blue

case color:
    Color::Red:
        print("red")
    Color::Green:
        print("green")
    Color::Blue:
        print("blue")

# Option pattern match
x: Option<int> = Some(42)
case x:
    Some(v):
        print(v)
    None:
        print("nothing")

# Result pattern match
function divide(a: int, b: int) -> Result<int, Error>:
    if b == 0:
        return Err(Error("division by zero"))
    return Ok(a // b)

case divide(10, 2):
    Ok(v):
        print(v)         # 5
    Err(e):
        print(e.message)

# Literal pattern match
case x:
    0:
        print("zero")
    1:
        print("one")
    _:
        print("other")

# Guard clause
case x:
    n if n > 0:
        print("positive")
    n if n < 0:
        print("negative")
    _:
        print("zero")
```

### ADT Enum Pattern Matching

When an enum variant carries associated data, use a binding pattern to extract the value(s).

```python
enum Shape:
    Circle(float)
    Rectangle(float, float)
    Point

s = Shape::Circle(3.14)
case s:
    Shape::Circle(r):
        print(r)        # 3.14
    Shape::Rectangle(w, h):
        print(w)
        print(h)
    Shape::Point:
        print("point")
```

Multi-field variants bind each field to a separate name in declaration order.

### Tuple Pattern Matching

Tuple patterns destructure a tuple subject by element position. Each element may be a variable binding, a literal, or a wildcard (`_`). Nested patterns (e.g., `Some(v)` inside a tuple element) are also supported.

```python
# Binding pattern — bind both elements
t = (10, 20)
case t:
    (x, y):
        print(x)   # 10
        print(y)   # 20

# Mixed literal + binding
point = (0, 99)
case point:
    (0, n):
        print(n)   # 99
    _:
        print("other")

# Wildcard
pair = (55, 77)
case pair:
    (_, second):
        print(second)  # 77

# Guard clause
case t:
    (a, b) if a > b:
        print("first bigger")
    (a, b):
        print("other")

# 1-tuple (trailing comma required)
single = (42,)
case single:
    (v,):
        print(v)   # 42

# Nested: Option inside a tuple
opt: Option<int> = Some(7)
pair2 = (opt, 0)
case pair2:
    (Some(v), _):
        print(v)   # 7
    (None, _):
        print("none")
```

**Exhaustiveness**: A tuple pattern where every element is a variable or `_` (irrefutable) is treated as exhaustive — no wildcard arm is required.

**Syntax rules**:

| Syntax | Meaning |
|--------|---------|
| `(a, b)` | 2-tuple pattern |
| `(v,)` | 1-tuple pattern — the trailing comma is required |
| `(p)` | Grouping — equivalent to just `p`; **not** a 1-tuple |
| `()` | Not supported (parse error) |

**Restrictions**: Variable bindings are not allowed inside OR patterns. `(1, x) | (2, y)` is rejected at parse time.

### Expression Forms

Both `case:` and `case <expr>:` can be used as expressions by replacing `:` with `=>` in each arm. Each arm provides a single expression whose value becomes the result.

```python
# case: expression (no subject)
label = case:
    x > 100 => "huge"
    x > 10  => "big"
    x > 0   => "small"
    _       => "non-positive"
```

Pattern-matching expression form:

#### Syntax

```python
result = case expression:
    pattern => value_expression
    pattern if guard => value_expression
    _ => default_value
```

All patterns supported in `case:` statements are also supported in `case` expressions: literals, variable bindings, enums, ADT enums, `Some`/`None`, `Ok`/`Err`, tuple patterns, OR patterns, guards, and wildcards.

`case` expressions must be exhaustive (same rules as `case:` statements).

#### Examples

```python
# Option
value = case opt:
    Some(v) => v
    None    => 0

# Enum
label = case direction:
    Direction::North => "N"
    Direction::South => "S"
    Direction::East  => "E"
    Direction::West  => "W"

# Guard
grade = case score:
    n if n >= 90 => "A"
    n if n >= 80 => "B"
    _            => "F"

# OR pattern
kind = case x:
    1 | 2 | 3 => "small"
    _          => "large"

# ADT enum
area = case shape:
    Shape::Circle(r)  => 3.14 * r * r
    Shape::Rectangle(w, h) => w * h
    Shape::Point      => 0.0

# Tuple pattern
t = (3, 4)
sum = case t:
    (a, b) => a + b
    _ => 0
```

### Scope Rules

- Each `case` arm has its own block scope.
- Variables bound by variable binding patterns (`n`), `Some(x)`, `Ok(v)`, `Err(e)`, or tuple patterns `(a, b)` are only valid within that arm.

---

## Scope Rules

### Block Scope

- Each block of `if` / `else` / `while` / `for` / `case` has a block scope.
- Variables declared inside a block go out of scope when the block ends.

```python
for i in range(3):
    tmp = i * 2
# tmp is not accessible here

if true:
    a = 1
# a is not accessible here
```

### Inner Scope Reassignment

- Assigning to a variable inside an inner scope modifies the outer variable (Python-style scoping).
- There is no shadowing — the inner assignment changes the same variable.

```python
x = 10
if true:
    x = 99   # Modifies the outer x
    print(x)     # 99
print(x)         # 99
```
