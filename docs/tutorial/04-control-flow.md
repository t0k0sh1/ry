[English](04-control-flow.md) | [日本語](../ja/tutorial/04-control-flow.md) | [繁體中文](../zh/tutorial/04-control-flow.md)

# Control Flow

[<- Prev: Operators](03-operators.md) | [Next: Functions ->](05-functions.md)

---

## if / elif / else

Use `if` to branch execution based on conditions.

```python
let x = 10

if x > 0:
    print(x)
elif x == 0:
    print(0)
else:
    print(-1)
```

- `elif` and `else` are optional.
- Conditions are not limited to `bool` values. For `int`, `0` is treated as false and non-`0` as true.
- `if` statements can be nested.

```python
let a = 5
let b = 3

if a > 0:
    if b > 0:
        print(a + b)   # 8
```

---

## while Loop

Repeatedly executes a block as long as the condition is true.

```python
var i = 3
while i > 0:
    print(i)
    i = i - 1
# 3
# 2
# 1
```

---

## for Loop and range

You can iterate over a list or using `range`.

```python
for x in [1, 2, 3]:
    print(x)
# 1
# 2
# 3
```

`range(n)` generates integers from `0` to `n - 1`.

```python
for i in range(5):
    print(i)
# 0
# 1
# 2
# 3
# 4
```

`range(start, end)` generates integers from `start` to `end - 1`.

```python
for i in range(2, 5):
    print(i)
# 2
# 3
# 4
```

The `..` range operator creates an inclusive range: `1 .. 3` produces `[1, 2, 3]`.

```python
for i in 1 .. 3:
    print(i)
# 1
# 2
# 3
```

You can iterate over map key-value pairs with `for k, v in map`:

```python
let m = {"x": 10, "y": 20}
for k, v in m:
    print(k)
    print(v)
```

---

## break and continue

`break` immediately exits the loop. `continue` skips the current iteration and proceeds to the next one.

```python
for i in range(10):
    if i == 5:
        break
    if i % 2 == 0:
        continue
    print(i)
# 1
# 3
```

They can also be used with `while` loops.

```python
var n = 0
while true:
    n = n + 1
    if n % 2 == 0:
        continue
    if n > 7:
        break
    print(n)
# 1
# 3
# 5
# 7
```

> **Note**: In nested loops, `break` / `continue` only affect the innermost loop. Using them outside a loop results in a compile error.

---

## Nesting Example

`for` and `while` loops can be nested.

```python
for i in range(1, 4):
    for j in range(1, 4):
        if j == 2:
            continue
        print(i * 10 + j)
# 11
# 13
# 21
# 23
# 31
# 33
```

---

## Scope Rules

Control flow blocks have their own scope.

### Block Scope

Variables declared inside a block cannot be referenced from outside the block.

```python
if true:
    let inner = 42
# Referencing inner here causes a compile error
```

### Referencing and Reassigning Outer Variables

You can reference and reassign outer variables from within a block.

```python
var count = 0
for i in range(5):
    count = count + i
print(count)   # 10
```

### Shadowing

If you declare a variable with the same name as an outer variable inside a block, the new variable is used within that block (shadowing). The outer variable remains unchanged.

```python
let x = 1
if true:
    let x = 99
    print(x)   # 99
print(x)       # 1
```

---

## match

`match` is a construct for branching based on a value. It can safely handle enums and Options.

```python
enum Color:
    Red
    Green
    Blue

let c = Color::Green
match c:
    case Color::Red:
        print("red")
    case Color::Green:
        print("green")
    case Color::Blue:
        print("blue")
# green
```

### Option Matching

Use `match` to safely handle both the `Some` and `None` cases.

```python
let x: Option<int> = Some(42)
match x:
    case Some(v):
        print(v)
    case None:
        print("nothing")
# 42
```

### Wildcards and Literals

`_` is a wildcard pattern that matches anything. You can also match against literal values (numbers, strings, booleans).

```python
let n = 5
match n:
    case 0:
        print("zero")
    case 1:
        print("one")
    case _:
        print("other")
# other
```

### Guard Clauses

You can add guard conditions with `if`.

```python
match n:
    case x if x > 0:
        print("positive")
    case x if x < 0:
        print("negative")
    case _:
        print("zero")
```

> **Note**: `match` must be exhaustive. For enums, all variants must be covered. For Options, both `Some` and `None` are required. For literals, a `_` wildcard is needed.

---

[<- Prev: Operators](03-operators.md) | [Next: Functions ->](05-functions.md)
