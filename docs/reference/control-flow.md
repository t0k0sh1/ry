[English](control-flow.md) | [日本語](../ja/reference/control-flow.md) | [繁體中文](../zh/reference/control-flow.md)

# Control Flow Reference

## if / elif / else

### Syntax

```python
if condition:
    # then block
elif condition:
    # elif block (can have multiple)
else:
    # else block (optional)
```

### Condition Types

| Type | Falsy Value | Truthy Value |
|---|---|---|
| `bool` | `false` | `true` |
| `int` | `0` | non-zero |

`float` and `str` cannot be used directly as conditions.

### Example

```python
x = 10

if x > 5:
    print("big")
elif x == 5:
    print("five")
else:
    print("small")
```

### Scope Rules

- Each `if` / `elif` / `else` block has its own independent block scope.
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

`async fn` declares a function that runs concurrently. Calling an `async fn` returns `Task<T>`. Use `await` inside another `async fn` or `block_on()` from synchronous context to wait for the result.

```python
async fn add(a: int, b: int) -> int:
    return a + b

# From synchronous context, use block_on()
t: Task<int> = add(20, 22)
print(block_on(t))                  # 42
print(block_on(add(1, 2)))          # 3

# Inside async fn, use await
async fn double_add(a: int, b: int) -> int:
    result = await add(a, b)
    return result * 2
```

### Rules

- `async fn name(...) -> T:` is declared with the awaited result type `T`.
- Calling an `async fn` immediately returns `Task<T>`.
- `await expr` requires `expr` to be `Task<T>` and produces `T`.
- `await` can only be used inside an `async fn`. Use `block_on(task)` from synchronous context.
- `block_on(task)` blocks the current thread until the task completes and returns the result.
- `async fn ... -> Unit` is supported; `block_on(task)` is the primary way to wait when no value is produced.
- Tasks run on the runtime worker pool; they are not implemented as one OS thread per task.
- `async` lambdas and `async @native fn` are not supported in v1.

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
- Can be used in any block: function body, `if`/`elif`/`else`, `while`, `for`, `match case`, etc.

```python
fn not_yet():
    ...

if true:
    ...
else:
    ...
```

---

## match

### Syntax

```python
match expression:
    case pattern:
        # body
    case pattern if guard_condition:
        # guarded body
    case _:
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
| OR pattern | `1 \| 2 \| 3` | Matches if any alternative matches |

### Guard Clause

A guard condition can be specified in the form `case pattern if condition:`. The arm is executed only when the pattern matches and the guard condition is true.

### OR Pattern

Multiple patterns can be combined with `|` to match any of them. Variable bindings (`n`, `Some(x)`, `Ok(v)`, `Err(e)`) are not allowed in OR patterns.

```python
match x:
    case 1 | 2 | 3:
        print("small")
    case _:
        print("other")

# Enum OR pattern
match color:
    case Color::Red | Color::Blue:
        print("warm or cool")
    case Color::Green:
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
# enum match
enum Color:
    Red
    Green
    Blue

match color:
    case Color::Red:
        print("red")
    case Color::Green:
        print("green")
    case Color::Blue:
        print("blue")

# Option match
x: Option<int> = Some(42)
match x:
    case Some(v):
        print(v)
    case None:
        print("nothing")

# Result match
fn divide(a: int, b: int) -> Result<int, Error>:
    if b == 0:
        return Err(Error("division by zero"))
    return Ok(a // b)

match divide(10, 2):
    case Ok(v):
        print(v)         # 5
    case Err(e):
        print(e.message)

# Literal match
match x:
    case 0:
        print("zero")
    case 1:
        print("one")
    case _:
        print("other")

# Guard clause
match x:
    case n if n > 0:
        print("positive")
    case n if n < 0:
        print("negative")
    case _:
        print("zero")
```

### ADT Enum Match

When an enum variant carries associated data, use a binding pattern to extract the value(s).

```python
enum Shape:
    Circle(float)
    Rectangle(float, float)
    Point

s = Shape::Circle(3.14)
match s:
    case Shape::Circle(r):
        print(r)        # 3.14
    case Shape::Rectangle(w, h):
        print(w)
        print(h)
    case Shape::Point:
        print("point")
```

Multi-field variants bind each field to a separate name in declaration order.

### Scope Rules

- Each `case` arm has its own block scope.
- Variables bound by variable binding patterns (`n`), `Some(x)`, `Ok(v)`, or `Err(e)` are only valid within that arm.

---

## Scope Rules

### Block Scope

- Each block of `if` / `elif` / `else` / `while` / `for` / `match` has a block scope.
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
