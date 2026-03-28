[English](functions.md) | [日本語](../ja/reference/functions.md) | [繁體中文](../zh/reference/functions.md)

# Function Reference

## Function Definition Syntax

```python
fn function_name(param_name: type, ...) -> return_type:
    # body
    return value
```

- Parameter types are optional. When omitted, the parameter is treated as `any` type.
- Return type is optional. When omitted, the return type is **inferred from the body** (both named functions and lambdas). If the function has no `return` statement, the return type is inferred as `Unit`. Use `-> any` explicitly for functions that should accept any return type.
- The body is an indented block.
- Functions with an explicit return type (other than `Unit` or `any`) must have a `return` statement on all control flow paths. The compiler reports an error if any path is missing a return.
- Functions can have `require` (precondition) and `ensure` (postcondition) clauses. See [Design by Contract](contracts.md).

> **Naming convention**: Function names and parameter names must use snake_case (e.g., `add`, `get_value`, `map_list`). The compiler enforces this convention.

```python
fn add(a: int, b: int) -> int:
    return a + b

fn greet(name: str) -> Unit:
    print("Hello, " + name)   # Return type is Unit (explicit)
```

---

## Parameter and Return Types

| Item | Description |
|---|---|
| Parameter type | Optional. Defaults to `any` when the `: type` annotation is omitted |
| Return type | Optional. Inferred from the body when omitted (inferred as `Unit` if no `return` statement) |
| `Unit` | Return type for functions that return no value |

> **Note**: Function parameters are **immutable**. You cannot reassign a parameter inside the function body. This ensures that parameter values at entry are always available for postcondition checks (see [Design by Contract](contracts.md)).

```python
fn no_return(x: int) -> Unit:  # Return type Unit (explicit)
    print(x)

fn get_value() -> int:     # Return type int
    return 42

fn identity(x) -> any:    # Parameter type any (omitted)
    return x
```

### Type Omission and `any`

When a parameter type annotation is omitted, the parameter is treated as `any` — a dynamic type that accepts any primitive value at runtime. This is similar to Python's untyped parameters.

```python
# All parameters default to any
fn add(a, b):
    return a + b

add(1, 2)              # 3 (int + int)
add("hello", " world") # "hello world" (str + str)
add(1, 2.0)            # 3.0 (int + float)
```

You can also use `any` explicitly in type annotations:

```python
fn identity(x: any) -> any:
    return x
```

### Return Type Inference

When the return type is omitted, it is inferred from the `return` statements in the body:

```python
fn double(x: int):     # return type inferred as int
    return x * 2

fn greet(name: str):   # return type inferred as Unit (no return)
    print("Hello, " + name)
```

To explicitly allow any return type, use `-> any`:

```python
fn flexible(x: any) -> any:
    return x    # can return int, float, str, etc.
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

a = area(5)       # 25
b = area(3, 4)    # 12
```

### Resolution Priority

When multiple overloads match a call, the compiler selects the most specific one using the following priority (highest first):

1. **Exact type match** — argument type matches parameter type exactly
2. **Implicit widening** — safe widening conversion (`byte` → `int`, `byte` → `float`, `int` → `float`)
3. **Union type match** — argument type is a member of a union parameter type
4. **`any` type match** — parameter type is `any` (accepts anything)

The overload with the most exact matches wins. If two or more overloads have equal specificity, the compiler reports an ambiguity error.

Low-level numeric types (`i8`, `i16`, `i32`, `i64`, `u8`–`u64`, `f32`) do **not** participate in implicit widening — they require explicit `as` casts.

```python
fn process(x: int) -> str:
    return "int"

fn process(x) -> str:          # x: any
    return "any"

process(42)       # "int" — exact match (int) beats any
process("hello")  # "any" — no exact match for str, falls back to any
```

```python
fn double(x: float) -> float:
    return x * 2.0

double(5)         # OK — int is implicitly widened to float, returns 10.0
```

---

## Unit Type Functions

Functions without a return value return `Unit`. The return type can be omitted (inferred as `Unit`) or explicitly specified with `-> Unit`.

```python
fn log(msg: str) -> Unit:
    print(msg)
```

---

## Tasks And Async Functions

`Task<T>` is the built-in handle type for concurrent work. `async fn` returns `Task<T>`, `await` extracts `T`, and `join(task)` is the blocking function-form equivalent of `await task`.

```python
async fn add(a: int, b: int) -> int:
    return a + b

t: Task<int> = add(20, 22)
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

---

## Lambda Functions

Anonymous functions can be defined inline.

### Syntax

```python
# Single expression (return type inferred from expression)
fn(param_name: type, ...) => expression

# Parameter type can be omitted (defaults to any)
fn(param_name, ...) => expression

# Multi-line block
fn(param_name: type, ...):
    # multiple statements
    return value

# With explicit return type (optional)
fn(param_name: type, ...) -> return_type => expression
```

### Example

```python
double = fn(x: int) => x * 2
result = double(5)   # 10

add = fn(a: int, b: int) => a + b
sum = add(3, 4)      # 7

# Multi-line lambda
abs = fn(x: int):
    if x < 0:
        return -x
    return x
```

---

## Closures

Lambda functions **capture by value** the variables from the outer scope at the time of definition.

```python
base = 10
add_base = fn(x: int) => x + base   # Captures base by value

base = 99          # Does not affect the captured value
r = add_base(5)   # 15 (uses base = 10 from capture time)
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
f: fn(int) -> int = fn(x: int) => x * 2
g: fn(int, int) -> int = fn(a: int, b: int) => a + b

fn apply(func: fn(int) -> int, x: int) -> int:
    return func(x)

result = apply(f, 5)   # 10
```

---

## Higher-Order Functions

Functions can accept functions as arguments or return them as values.

```python
fn map_list(xs: List<int>, f: fn(int) -> int) -> List<int>:
    result: List<int> = []
    for x in xs:
        result += [f(x)]
    return result

doubled = map_list([1, 2, 3], fn(x: int) => x * 2)
# [2, 4, 6]
```

---

## Generic Functions

Functions can have type parameters, enabling type-safe reuse without code duplication.

### Syntax

```python
fn name<T, U>(param1: T, param2: U) -> T:
    # body using T, U as types
```

### Example

```python
fn identity<T>(x: T) -> T:
    return x

# Explicit type argument
result = identity[int](42)      # 42
result = identity[str]("hello") # "hello"

# Type inference (type argument deduced from actual argument)
result = identity(42)           # T = int, result = 42
result = identity("hello")     # T = str, result = "hello"
```

### Multiple Type Parameters

```python
fn pick_first<T, U>(a: T, b: U) -> T:
    return a

result = pick_first(1, "x")       # T = int, U = str, result = 1
result = pick_first("hello", 42)  # T = str, U = int, result = "hello"
```

### How It Works

Generic functions use **monomorphization**: a specialized version of the function is generated for each unique combination of type arguments. The same instantiation is cached and reused across multiple calls.

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

result = 5.double().add_one()   # double(5) -> 10, add_one(10) -> 11
```

### Mixing with Field Access

Field access (`.field`) and UFCS (`.method()`) use the same dot notation but are distinguished by the presence of arguments.

```python
p = Point(3, 4)
length = p.x.to_float()   # Field access + UFCS
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

### Return Type Constraints

Comparison and logical operators must return `bool`:

| Category | Operators | Required Return Type |
|---|---|---|
| Comparison | `==` `!=` `<` `<=` `>` `>=` | `bool` |
| Logical | `and` `or` `not` | `bool` |

```python
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

v1 = Vec2(1.0, 2.0)
v2 = Vec2(3.0, 4.0)
v3 = v1 + v2    # Vec2(4.0, 6.0)
v4 = -v1        # Vec2(-1.0, -2.0)
```
