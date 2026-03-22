[English](functions.md) | [日本語](../ja/reference/functions.md) | [繁體中文](../zh/reference/functions.md)

# Function Reference

## Function Definition Syntax

```python
fn function_name(param_name: type, ...) -> return_type:
    # body
    return value
```

- Parameter types are required.
- Return type is optional (defaults to `Unit` when omitted).
- The body is an indented block.
- Functions can have `require` (precondition) and `ensure` (postcondition) clauses. See [Design by Contract](contracts.md).

> **Naming convention**: Function names and parameter names must use snake_case (e.g., `add`, `get_value`, `map_list`). The compiler enforces this convention.

```python
fn add(a: int, b: int) -> int:
    return a + b

fn greet(name: str):
    print("Hello, " + name)   # Return type is Unit
```

---

## Parameter and Return Types

| Item | Description |
|---|---|
| Parameter type | Required. All parameters must have type annotations |
| Return type | Optional. Defaults to `Unit` (equivalent to void) when omitted |
| `Unit` | Return type for functions that return no value |

```python
fn no_return(x: int):      # Return type Unit (omitted)
    print(x)

fn get_value() -> int:     # Return type int
    return 42
```

---

## Recursion

Functions can call themselves.

```python
fn factorial(n: int) -> int:
    if n <= 1:
        return 1
    return n * factorial(n - 1)
```

---

## Overloading

Multiple functions with the same name can be defined if they differ in the number or types of parameters.

### Rules

- Functions with the same name can be defined if the number or types of parameters differ.
- The appropriate function is selected at the call site based on the argument types and count.
- Overloading by return type alone is not allowed.

```python
fn area(side: int) -> int:
    return side * side

fn area(w: int, h: int) -> int:
    return w * h

let a = area(5)       # 25
let b = area(3, 4)    # 12
```

---

## Unit Type Functions

Functions without a return value return `Unit`. The return type can be omitted.

```python
fn log(msg: str):
    print(msg)

fn log_typed(msg: str) -> Unit:
    print(msg)
```

---

## Tasks And Async Functions

`Task<T>` is the built-in handle type for concurrent work. `async fn` returns `Task<T>`, `await` extracts `T`, and `join(task)` is the blocking function-form equivalent of `await task`.

```python
async fn add(a: int, b: int) -> int:
    return a + b

let t: Task<int> = add(20, 22)
print(await t)          # 42
await add(1, 2)         # waits and discards the result
print(join(add(1, 2)))  # 3
```

### Rules

- `async fn name(...) -> T:` is declared with the awaited result type `T`.
- Calling an `async fn` immediately returns `Task<T>`.
- `await expr` requires `expr` to be `Task<T>` and produces `T`.
- `await` is allowed anywhere an expression is allowed, and `await expr` is also allowed as a statement.
- `async fn ... -> Unit` is supported; `await task` is the primary way to wait when no value is produced.
- Tasks run on the runtime worker pool; they are not implemented as one OS thread per task.
- `async` lambdas and `async @native fn` are not supported in v1.

`Channel<T>` is the built-in handle type for blocking message passing between tasks. Create channels with `channel[T]()` or `channel[T](capacity)`, send values with `send(ch, value)`, use `try_send(ch, value)` for a non-blocking send attempt, use `recv(ch)` for strict receive, use `recv_opt(ch)` for close-aware receive, use `try_recv(ch)` for a non-blocking receive attempt, iterate consumers with `for x in ch:`, and terminate a channel with `close(ch)`.

---

## Lambda Functions

Anonymous functions can be defined inline.

### Syntax

```python
# Single expression (the expression value is returned; return type is inferred)
fn(param_name: type, ...): expression

# Multi-line block
fn(param_name: type, ...):
    # multiple statements
    return value

# With explicit return type (optional)
fn(param_name: type, ...) -> return_type: expression
```

### Example

```python
let double = fn(x: int): x * 2
let result = double(5)   # 10

let add = fn(a: int, b: int): a + b
let sum = add(3, 4)      # 7

# Multi-line lambda
let abs = fn(x: int):
    if x < 0:
        return -x
    return x
```

---

## Closures

Lambda functions **capture by value** the variables from the outer scope at the time of definition.

```python
var base = 10
let add_base = fn(x: int): x + base   # Captures base by value

base = 99          # Does not affect the captured value
let r = add_base(5)   # 15 (uses base = 10 from capture time)
```

### Capture Rules

| Item | Details |
|---|---|
| Capture method | Capture by value (copy) |
| Capture timing | At lambda definition time |
| Effect of outer variable changes | None (because it is a copy) |

---

## Function Type

A type for treating functions as values.

### Syntax

```python
fn(param_type1, param_type2, ...) -> return_type
```

### Example

```python
let f: fn(int) -> int = fn(x: int): x * 2
let g: fn(int, int) -> int = fn(a: int, b: int): a + b

fn apply(func: fn(int) -> int, x: int) -> int:
    return func(x)

let result = apply(f, 5)   # 10
```

---

## Higher-Order Functions

Functions can accept functions as arguments or return them as values.

```python
fn map_list(xs: List<int>, f: fn(int) -> int) -> List<int>:
    var result: List<int> = []
    for x in xs:
        result += [f(x)]
    return result

let doubled = map_list([1, 2, 3], fn(x: int): x * 2)
# [2, 4, 6]
```

---

## UFCS (Uniform Function Call Syntax)

`a.f(b)` can be used to call `f(a, b)`. Useful for method chaining.

### Syntax

```python
# Normal call
f(a, b)

# UFCS call (equivalent)
a.f(b)
```

### Chaining

```python
fn double(x: int) -> int:
    return x * 2

fn add_one(x: int) -> int:
    return x + 1

let result = 5.double().add_one()   # double(5) -> 10, add_one(10) -> 11
```

### Mixing with Field Access

Field access (`.field`) and UFCS (`.method()`) use the same dot notation but are distinguished by the presence of arguments.

```python
let p = Point(3, 4)
let len = p.x.to_float()   # Field access + UFCS
```

---

## Operator Overloading

You can define operator behavior for user-defined types.

### Syntax

```python
# Binary operator (2 parameters)
fn operator<op>(a: type, b: type) -> return_type:
    ...

# Unary operator (1 parameter)
fn operator<op>(a: type) -> return_type:
    ...
```

### Overloadable Operators

| Category | Operators |
|---|---|
| Arithmetic (binary) | `+` `-` `*` `/` `%` `**` `//` |
| Comparison (binary) | `==` `!=` `<` `<=` `>` `>=` |
| Bitwise (binary) | `&` `\|` `^` `<<` `>>` |
| Logical (binary) | `and` `or` |
| Unary | `-` `~` `not` |

### Distinguishing Binary and Unary

Distinguished by the number of parameters.

```python
record Vec2:
    x: float
    y: float

# Binary +
fn operator+(a: Vec2, b: Vec2) -> Vec2:
    return Vec2(a.x + b.x, a.y + b.y)

# Unary -
fn operator-(v: Vec2) -> Vec2:
    return Vec2(-v.x, -v.y)

# Comparison
fn operator==(a: Vec2, b: Vec2) -> bool:
    return a.x == b.x and a.y == b.y

let v1 = Vec2(1.0, 2.0)
let v2 = Vec2(3.0, 4.0)
let v3 = v1 + v2    # Vec2(4.0, 6.0)
let v4 = -v1        # Vec2(-1.0, -2.0)
```
