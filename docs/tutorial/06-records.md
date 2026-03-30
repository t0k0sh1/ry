[English](06-records.md) | [日本語](../ja/tutorial/06-records.md) | [繁體中文](../zh/tutorial/06-records.md)

# Records and Enums

[<- Prev: Functions](05-functions.md) | [Next: Collections and Iterators ->](07-collections.md)

---

## Defining Records

Records are defined with the `record` keyword. Each field is described in the `name: type` format.

```python
record Point:
    x: int
    y: int
```

Records are value types allocated on the stack.

---

## Creating Instances

Create an instance by calling the record name as a function. Arguments are specified in the order the fields are defined.

```python
p = Point(10, 20)
```

---

## Field Access (Dot Notation)

Fields are accessed using dot notation.

```python
p = Point(10, 20)
print(p.x)   # 10
print(p.y)   # 20
```

> **Note**: Passing a record directly to `print` causes an error. Pass individual fields instead.

---

## Field Assignment

Fields of mutable variables (declared without `@const`) can be reassigned.

```python
p = Point(10, 20)
p.x = 100
print(p.x)   # 100
```

> **Note**: Assigning to fields of a `@const` variable causes a compile error.

---

## Records as Function Parameters

Records can be passed as function arguments.

```python
record Point:
    x: int
    y: int

fn distance_x(a: Point, b: Point) -> int:
    return a.x - b.x

p1 = Point(10, 3)
p2 = Point(4, 7)
print(distance_x(p1, p2))   # 6
```

---

## Nested Records

A record's field can be another record.

```python
record Point:
    x: int
    y: int

record Line:
    start: Point
    end: Point

line = Line(Point(0, 0), Point(10, 5))
print(line.start.x)   # 0
print(line.end.x)     # 10
```

You can access nested fields by chaining dot notation.

---

## Enums

Enums are defined with the `enum` keyword. Each variant is treated as a named constant.

### Definition

```python
enum Color:
    Red
    Green
    Blue
```

### Usage

Variants are accessed with `::`.

```python
c = Color::Red
print(c)   # Red
```

### Comparison

Variants can be compared using `==` and `!=`.

```python
when:
    c == Color::Red:
        print("red!")
    c == Color::Green:
        print("green!")
    else:
        print("blue!")
```

### Function Parameters

Enum names can be used as function parameter types.

```python
fn describe(c: Color) -> str:
    if c == Color::Red:
        return "warm"
    return "cool"
```

---

## Enum with Associated Data (ADT)

Enum variants can carry associated values. This lets a single enum represent a family of different shapes of data — a pattern known as an **Algebraic Data Type (ADT)**.

```python
enum Shape:
    Circle(radius: float)
    Rectangle(width: float, height: float)
    Point
```

Named fields are documentation-only — they make definitions self-describing. Unnamed syntax (`Circle(float)`) is also valid.

### Constructing ADT Variants

Construction is always positional, regardless of whether fields are named.

```python
c = Shape::Circle(3.14)
r = Shape::Rectangle(4.0, 5.0)
p = Shape::Point
```

### Matching ADT Variants

Use `when` with `case` to extract the associated data. Bindings use your chosen variable names, not the field names. This connects directly with the pattern matching you learned in [Control Flow](04-control-flow.md).

```python
fn describe(s: Shape) -> str:
    when s:
        case Shape::Circle(r):
            return f"circle with radius {r}"
        case Shape::Rectangle(w, h):
            return f"rectangle {w}x{h}"
        case Shape::Point:
            return "point"

print(describe(Shape::Circle(3.14)))         # circle with radius 3.14
print(describe(Shape::Rectangle(4.0, 5.0)))  # rectangle 4.0x5.0
```

> **Why ADTs?** They let you model data that can be "one of several shapes" in a type-safe way. The compiler ensures you handle every variant when pattern matching, catching missing cases at compile time.

---

## Generic Enums

Enums can take type parameters, making them reusable across different payload types.

```python
enum MyOption<T>:
    MySome(T)
    MyNone
```

### Usage

```python
a = MyOption<int>::MySome(42)
b: MyOption<int> = MyOption<int>::MyNone

when a:
    case MyOption::MySome(v):
        print(v)      # 42
    case MyOption::MyNone:
        print("none")
```

> **Note**: Ry's built-in `Option<T>` and `Result<T, E>` types work exactly like this. You will learn about them in [Error Handling](08-error-handling.md).

---

## Operator Overloading

You can define operators for custom types using the `fn operator` syntax. This lets your records work naturally with `+`, `==`, and other operators.

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

v1 = Vec2(1, 2)
v2 = Vec2(3, 4)
v3 = v1 + v2
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

> **Why operator overloading?** It gives domain types natural syntax. A `Vec2 + Vec2` reads better than `vec2_add(a, b)`, and `==` lets your types work seamlessly with `when` and comparisons.

---

## Exercises

1. **ADT**: Define an `Animal` enum with variants `Dog(name: str)`, `Cat(name: str, indoor: bool)`, and `Fish`. Write a `describe(a: Animal) -> str` function that uses `when` to return a description for each variant.

2. **Operator overloading**: Define a `Money` record with `amount: int` and `currency: str`. Overload `+` so that adding two `Money` values with the same currency returns a new `Money` with the summed amount.

---

[<- Prev: Functions](05-functions.md) | [Next: Collections and Iterators ->](07-collections.md)
