[English](08-advanced.md) | [日本語](../ja/tutorial/08-advanced.md) | [繁體中文](../zh/tutorial/08-advanced.md)

# Advanced Features

[<- Prev: Collections](07-collections.md) | [Next: Modules ->](09-modules.md)

---

## Lambda Functions

Lambda functions are a syntax for writing functions as expressions. They use the form `(parameters) -> expression`. The return type is automatically inferred.

### Single-Expression Lambda

```python
let double = (x: int) -> x * 2
print(double(5))  # 10

let add = (a: int, b: int) -> a + b
print(add(3, 4))  # 7
```

### No-Parameter Lambda

```python
let answer = () -> 42
print(answer())  # 42
```

### Multi-Line Lambda

You can write multiple statements by adding a newline after `->` and indenting.

```python
let abs = (x: int) ->
    if x < 0:
        return -x
    return x

print(abs(-5))  # 5
print(abs(3))   # 3
```

---

## Closures

Lambda functions can capture variables from the scope in which they are defined.

```python
let offset = 10
let add_offset = (x: int) -> x + offset
print(add_offset(5))  # 15
```

---

## Higher-Order Functions

You can define functions that take other functions as arguments. Function types are written as `fn(parameter_types) -> return_type`.

```python
fn apply(f: fn(int) -> int, x: int) -> int:
    return f(x)

let double = (x: int) -> x * 2
print(apply(double, 3))                # 6
print(apply((n: int) -> n + 1, 10))    # 11
```

---

## Functions as Values

Named functions can also be bound to variables or passed as arguments.

```python
fn square(x: int) -> int:
    return x * x

fn apply(f: fn(int) -> int, x: int) -> int:
    return f(x)

# Pass a named function as an argument
print(apply(square, 4))  # 16

# Bind to a variable
let sq = square
print(sq(5))  # 25
```

---

## UFCS (Uniform Function Call Syntax)

With UFCS, you can write `f(a, b)` as `a.f(b)`. This enables method-chaining-style notation.

```python
fn add(a: int, b: int) -> int:
    return a + b

let x = 1
print(x.add(2))   # add(x, 2) -> 3
```

### Chained Calls

```python
fn double(n: int) -> int:
    return n * 2

print(x.add(2).double())   # double(add(x, 2)) -> 6
```

---

## Operator Overloading

You can define operators for custom types using the `fn operator` syntax.

### Binary Operators

Takes two parameters.

```python
type Vec2:
    x: int
    y: int

fn operator+(a: Vec2, b: Vec2) -> Vec2:
    return Vec2(a.x + b.x, a.y + b.y)

fn operator==(a: Vec2, b: Vec2) -> bool:
    return a.x == b.x and a.y == b.y

let v1 = Vec2(1, 2)
let v2 = Vec2(3, 4)
let v3 = v1 + v2
print(v3.x)       # 4
print(v1 == v2)   # false
```

### Unary Operators

Takes one parameter.

```python
fn operator-(v: Vec2) -> Vec2:
    return Vec2(-v.x, -v.y)
```

### Supported Operators

| Category | Operators |
|----------|-----------|
| Arithmetic | `+`, `-`, `*`, `/`, `%`, `**`, `//` |
| Comparison | `==`, `!=`, `<`, `<=`, `>`, `>=` |
| Bitwise | `&`, `\|`, `^`, `~`, `<<`, `>>` |
| Logical | `and`, `or`, `not` |

---

## Option Type

A type that represents whether a value exists or not. It takes either `Some(value)` or `None`.

```python
let x: Option<int> = Some(42)
print(x)   # Some(42)

let y: Option<int> = None
print(y)   # None
```

### unwrap

Use `unwrap` to extract the inner value. Calling `unwrap` on `None` causes a runtime error.

```python
let v = unwrap(x)   # 42
# unwrap(y) -> runtime error
```

---

## F-String (String Interpolation)

Use `f"..."` to embed expressions directly inside strings. Expressions are placed in `{}`.

```python
let name = "Alice"
print(f"Hello {name}")   # Hello Alice

let x = 3
let y = 4
print(f"{x} + {y} = {x + y}")   # 3 + 4 = 7
```

Use `{{` and `}}` to include literal braces.

```python
print(f"{{escaped}}")   # {escaped}
```

---

## Type Casting (`as`)

Convert between types explicitly with `as`.

```python
let x = 42 as float     # 42.0
let y = 3.14 as int      # 3 (truncated)
let s = 42 as str         # "42"
let b = true as int       # 1
```

---

## Result Type

`Result<T, E>` is used for functions that may fail. Return `Ok(value)` for success and `Err(error)` for failure.

```python
fn divide(a: int, b: int) -> Result<int, str>:
    if b == 0:
        return Err("division by zero")
    return Ok(a // b)
```

Use `match` to handle the result.

```python
let r = divide(10, 0)
match r:
    case Ok(v):
        print(v)
    case Err(e):
        print(e)   # division by zero
```

---

[<- Prev: Collections](07-collections.md) | [Next: Modules ->](09-modules.md)
