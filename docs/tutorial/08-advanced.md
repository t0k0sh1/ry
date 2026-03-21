[English](08-advanced.md) | [日本語](../ja/tutorial/08-advanced.md) | [繁體中文](../zh/tutorial/08-advanced.md)

# Advanced Features

[<- Prev: Collections](07-collections.md) | [Next: Packages ->](09-modules.md)

---

## Lambda Functions

Lambda functions are a syntax for writing functions as expressions. They use the form `fn(parameters): expression`. The return type is automatically inferred.

### Single-Expression Lambda

```python
let double = fn(x: int): x * 2
print(double(5))  # 10

let add = fn(a: int, b: int): a + b
print(add(3, 4))  # 7
```

### No-Parameter Lambda

```python
let answer = fn(): 42
print(answer())  # 42
```

### Multi-Line Lambda

You can write multiple statements by adding a newline after `:` and indenting.

```python
let abs = fn(x: int):
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
let add_offset = fn(x: int): x + offset
print(add_offset(5))  # 15
```

---

## Higher-Order Functions

You can define functions that take other functions as arguments. Function types are written as `fn(parameter_types) -> return_type`.

```python
fn apply(f: fn(int) -> int, x: int) -> int:
    return f(x)

let double = fn(x: int): x * 2
print(apply(double, 3))                # 6
print(apply(fn(n: int): n + 1, 10))    # 11
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
record Vec2:
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

### Extracting the Value

Use `match` to safely extract the inner value and handle the `None` case.

```python
match x:
    case Some(v):
        print(v)    # 42
    case None:
        print("nothing")
```

---

## Concurrency Basics

`Task<T>` is the runtime handle for concurrent work. Use `spawn` for explicit task creation, `async fn` for task-returning functions, and `await` or `join(task)` to wait for completion.

```python
fn square(x: int) -> int:
    return x * x

let t: Task<int> = spawn square(12)
print(await t)   # 144

async fn add(a: int, b: int) -> int:
    return a + b

print(await add(20, 22))   # 42
await add(1, 2)            # statement form also works
```

`@parallel` can be applied to counted `for` loops over `range(...)` or integer `..` ranges:

```python
@parallel
for i in range(8):
    print(i)
```

In v1, `spawn` does not support `Unit`-returning calls, and `@parallel for` rejects `break`, `continue`, and writes to outer mutable variables.

For channels, `recv(ch)` is the strict form and raises on a closed drained channel, while `recv_opt(ch)` returns `Some(value)` or `None` instead. `for x in ch:` is the close-aware consumer form and ends normally once the channel is closed and drained. For `Channel<Unit>`, `recv_opt(ch)` returns `bool` and `for _ in ch:` can be used to consume values.

---

## Networking (TCP Sockets)

Ry provides TCP socket support through the `std.net` module. The `send`, `recv`, and `close` functions are overloaded to work with both channels and TCP sockets.

```python
from std.net import bind, listen, accept, connect
from std.io import str_to_bytes, bytes_to_str

fn echo_server(port: int) -> str:
    match bind("127.0.0.1", port):
        case Some(server):
            listen(server, 1)
            match accept(server):
                case Some(conn):
                    let data: List<byte> = recv(conn, 4096)
                    send(conn, data)
                    close(conn)
                case None:
                    ...
            close(server)
        case None:
            ...
    return "done"

let t: Task<str> = spawn echo_server(8080)

match connect("127.0.0.1", 8080):
    case Some(conn):
        send(conn, str_to_bytes("hello"))
        let resp: List<byte> = recv(conn, 4096)
        print(bytes_to_str(resp))   # hello
        close(conn)
    case None:
        print("connect failed")

join(t)
```

See [Network Reference](../reference/net.md) for the full API.

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

## Enum with Associated Data (ADT)

Enum variants can carry associated values. This lets a single enum represent a family of different shapes of data.

```python
enum Shape:
    Circle(float)
    Rectangle(float, float)
    Point
```

### Constructing ADT Variants

```python
let c = Shape::Circle(3.14)
let r = Shape::Rectangle(4.0, 5.0)
let p = Shape::Point
```

### Matching ADT Variants

Use `case` with a binding pattern to extract the associated data.

```python
fn describe(s: Shape) -> str:
    match s:
        case Shape::Circle(r):
            return f"circle with radius {r}"
        case Shape::Rectangle(w, h):
            return f"rectangle {w}x{h}"
        case Shape::Point:
            return "point"

print(describe(Shape::Circle(3.14)))         # circle with radius 3.14
print(describe(Shape::Rectangle(4.0, 5.0)))  # rectangle 4.0x5.0
```

---

## Generic Enum

Enums can take type parameters, making them reusable across different payload types.

```python
enum MyOption<T>:
    MySome(T)
    MyNone
```

### Usage

```python
let a = MyOption<int>::MySome(42)
let b: MyOption<int> = MyOption<int>::MyNone

match a:
    case MyOption::MySome(v):
        print(v)      # 42
    case MyOption::MyNone:
        print("none")
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

[<- Prev: Collections](07-collections.md) | [Next: Packages ->](09-modules.md)
