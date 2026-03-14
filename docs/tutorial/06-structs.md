[English](06-structs.md) | [日本語](../ja/tutorial/06-structs.md) | [繁體中文](../zh/tutorial/06-structs.md)

# Structs and Enums

[<- Prev: Functions](05-functions.md) | [Next: Collections ->](07-collections.md)

---

## Defining Structs with type

Structs are defined with the `type` keyword. Each field is described in the `name: type` format.

```python
type Point:
    x: int
    y: int
```

Structs are value types allocated on the stack.

---

## Using Constructors

Create an instance by calling the struct name as a function. Arguments are specified in the order the fields are defined.

```python
let p = Point(10, 20)
```

---

## Field Access (Dot Notation)

Fields are accessed using dot notation.

```python
let p = Point(10, 20)
print(p.x)   # 10
print(p.y)   # 20
```

> **Note**: Passing a struct directly to `print` causes an error. Pass individual fields instead.

---

## Field Assignment

Fields of variables declared with `var` can be reassigned.

```python
var p = Point(10, 20)
p.x = 100
print(p.x)   # 100
```

> **Note**: Assigning to fields of a `let` variable causes a compile error.

---

## Structs as Function Parameters

Structs can be passed as function arguments.

```python
type Point:
    x: int
    y: int

fn distance_x(a: Point, b: Point) -> int:
    return a.x - b.x

let p1 = Point(10, 3)
let p2 = Point(4, 7)
print(distance_x(p1, p2))   # 6
```

---

## Nested Structs

A struct's field can be another struct.

```python
type Point:
    x: int
    y: int

type Line:
    start: Point
    end: Point

let line = Line(Point(0, 0), Point(10, 5))
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
let c = Color::Red
print(c)   # Red
```

### Comparison

Variants can be compared using `==` and `!=`.

```python
if c == Color::Red:
    print("red!")
elif c == Color::Green:
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

[<- Prev: Functions](05-functions.md) | [Next: Collections ->](07-collections.md)
