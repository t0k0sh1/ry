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
let x = 10

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
    let y = 42
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
let i = 0
while i < 5:
    print(i)
    i += 1
```

### Combining with break / continue

```python
let i = 0
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

### Example

```python
let xs = [10, 20, 30]
for x in xs:
    print(x)

let s = {1, 2, 3}
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
```

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
| enum variant | `Color::Red` | Compares enum tag |
| `Some(x)` | `Some(v)` | When Option has a value, binds the inner value |
| `None` | `None` | When Option has no value |
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
let x: Option<int> = Some(42)
match x:
    case Some(v):
        print(v)
    case None:
        print("nothing")

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

### Scope Rules

- Each `case` arm has its own block scope.
- Variables bound by variable binding patterns (`n`) or `Some(x)` are only valid within that arm.

---

## Scope Rules

### Block Scope

- Each block of `if` / `elif` / `else` / `while` / `for` / `match` has a block scope.
- Variables declared inside a block go out of scope when the block ends.

```python
for i in range(3):
    let tmp = i * 2
# tmp is not accessible here

if true:
    let a = 1
# a is not accessible here
```

### Shadowing

- Declaring a variable with the same name as an outer variable in an inner scope causes the inner variable to be referenced within the inner scope.
- After leaving the inner scope, the outer variable is accessible again.

```python
let x = 10
if true:
    let x = 99   # Shadows the outer x
    print(x)     # 99
print(x)         # 10
```
