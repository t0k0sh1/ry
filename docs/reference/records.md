[English](records.md) | [日本語](../ja/reference/structs.md) | [繁體中文](../zh/reference/structs.md)

# Record Reference

## Overview

Records are value types allocated on the stack. They are defined with the `record` keyword. Structs can have `invariant` clauses for Design by Contract. See [Design by Contract](contracts.md).

> **Naming convention**: Record names must use PascalCase (e.g., `Point`, `Rectangle`). Field names must use snake_case. The compiler enforces these conventions.

---

## Definition Syntax

```python
record TypeName:
    field_name: type
    field_name: type
```

### Example

```python
record Point:
    x: int
    y: int

record Rectangle:
    width: float
    height: float
```

---

## Constructor

Arguments are passed in the order of field definitions. Named arguments are not supported.

```python
p = Point(10, 20)
r = Rectangle(3.0, 4.5)
```

---

## Field Access

Fields are read using dot notation.

```python
p = Point(10, 20)
print(p.x)   # 10
print(p.y)   # 20
```

---

## Field Assignment

| Variable Declaration | Field Assignment |
|---------|--------------|
| Mutable (no `@const`) | Allowed      |
| `@const`   | Compile error |

```python
p = Point(10, 20)
p.x = 100    # OK: mutable variable

@const
q = Point(10, 20)
q.x = 100    # Error: fields of @const variables cannot be modified
```

### Chained and Deep Field Assignment

Field assignment composes across nested records, collection elements, and
compound operators. The left-hand side can be any postfix chain rooted at a
mutable variable.

```python
record Inner:
    val: int

record Outer:
    inner: Inner
    tag: str

o = Outer(Inner(1), "t")
o.inner.val = 42              # deep field write
o.inner.val += 1              # compound form
print(o.inner.val)            # 43

pts = [Point(1, 2), Point(3, 4)]
pts[0].x = 99                 # list-of-records field update
pts[0].x *= 2
print(pts[0].x)               # 198
```

See [collections → Chained Index and Field Assignment](collections.md) for
the full matrix and the aliasing caveat for nested collections inside records.

---

## Usage as Function Parameters and Return Values

```python
function distance(p: Point) -> float:
    return (p.x * p.x + p.y * p.y) as float

function make_point(x: int, y: int) -> Point:
    return Point(x, y)
```

---

## Nested Records

Records can be used as fields of other records.

```python
record Point:
    x: int
    y: int

record Circle:
    center: Point
    radius: float

c = Circle(Point(0, 0), 1.0)
print(c.center.x)   # 0
```

---

## Comparison (`==` / `!=`)

Record types automatically support `==` and `!=` operators. Comparison is performed field-by-field (structural equality).

```python
record Point:
    x: int
    y: int

p1 = Point(10, 20)
p2 = Point(10, 20)
p3 = Point(30, 40)

print(p1 == p2)  # true
print(p1 != p3)  # true
```

- All fields are compared in order. For `==`, all fields must be equal. For `!=`, at least one field must differ.
- Nested records are compared recursively.
- If a user-defined `operator==` or `operator!=` is provided, it takes precedence over the auto-generated version.

---

## Record Subtyping (Inheritance)

Records support single inheritance using the `<` syntax. A child record inherits all fields from its parent.

### Syntax

```python
record ChildName < ParentName:
    child_field: type
```

### Example

```python
record HttpError < Error:
    status: int
    url: str
```

### Field Inheritance

- The child record inherits all parent fields at the beginning of its layout.
- The constructor takes parent fields first, then child-specific fields.

```python
err = HttpError("not found", 404, 404, "/api")
print(err.message)  # "not found" (inherited from Error)
print(err.status)   # 404 (own field)
```

### Subtype Coercion

A child value can be passed where the parent type is expected. The child is automatically sliced to extract the parent-prefix fields (value-type slicing).

```python
function handle(e: Error) -> str:
    return e.message

err = HttpError("fail", 500, 500, "/api")
handle(err)  # OK — HttpError coerced to Error
```

### Deep Inheritance

Records can form inheritance chains. Each level inherits all ancestor fields.

```python
record DetailedHttpError < HttpError:
    detail: str

# Constructor: Error fields + HttpError fields + own fields
derr = DetailedHttpError("fail", 500, 500, "/x", "server crash")
handle(derr)  # OK — coerced to Error (grandparent)
```

### Rules

| Rule | Details |
|------|------|
| Single inheritance only | `record A < B:` — one parent only |
| Deep inheritance | `record C < B:` where `record B < A:` — allowed |
| Name collision | Child field with same name as parent field → compile error |
| Auto `==` / `to_str` | Includes all inherited fields |
| Invariant inheritance | Parent `invariant:` clauses are checked when constructing or modifying child records |
| Subtype coercion | Applies to: function args, return, `Err()`, field assignment, `?` operator |
| Generic bounds | `<T: RecordName>` constrains type parameter to subtypes of the record |
| `@const` | Applies to all fields including inherited |

---

## Constraints and Errors

| Constraint | Details |
|------|------|
| Duplicate field names | Compile error |
| Field assignment on `@const` variables | Compile error |

```python
# Error example: Duplicate field names
record Bad:
    x: int
    x: int   # Error
```

---

## Enumerations (enum)

### Overview

Enumerations are a set of named constants. By default, they are represented as sequential i64 integers (0, 1, 2, ...). Explicit integer values can also be assigned.

### Definition Syntax

```python
enum TypeName:
    VariantName
    VariantName
    ...
```

### Example

```python
enum Color:
    Red
    Green
    Blue
```

### Variant Access

Variants are accessed using the `::` operator.

```python
c = Color::Red
print(c)   # Red
```

### Comparison

Since enum values are integers, they can be compared directly with `==` / `!=`.

```python
print(Color::Red == Color::Red)    # true
print(Color::Red != Color::Green)  # true
```

### Usage in if Statements

```python
c = Color::Green
case:
    c == Color::Red:
        print("red")
    c == Color::Green:
        print("green")
    _:
        print("blue")
```

### Function Parameters

Use the enum name as the type name.

```python
function is_red(c: Color) -> bool:
    return c == Color::Red

print(is_red(Color::Red))    # true
print(is_red(Color::Green))  # false
```

### print

`print()` outputs the variant name.

```python
c = Color::Blue
print(c)   # Blue
```

### Explicit Values

Simple enum variants can be assigned explicit integer values for use cases like HTTP status codes or bitmask patterns.

```python
enum HttpStatus:
    Ok = 200
    NotFound = 404
    InternalError = 500

s = HttpStatus::NotFound
print(s)                              # NotFound
print(s == HttpStatus::NotFound)      # true
```

```python
enum Permission:
    Read = 1
    Write = 2
    Execute = 4
```

Rules:
- Only simple enums (no ADT variants with associated data) support explicit values.
- Values must be integer literals (negative values are allowed).
- If any variant has an explicit value, all variants must have explicit values (no mixing auto and manual).
- Duplicate values are a compile error.
- `print()` displays the variant name, not the integer value.

### Named Fields in ADT Variants

ADT variant fields can optionally include names for documentation purposes. Named fields make definitions self-describing without changing construction or pattern matching semantics.

```python
enum Shape:
    Circle(radius: float)
    Rect(width: float, height: float)
    Point
```

- Construction is always positional: `Shape::Circle(3.14)`, not `Shape::Circle(radius: 3.14)`.
- Pattern matching binds user-chosen variable names: `case Shape::Circle(r):`.
- Field names must be `snake_case`. Mixing named and unnamed fields within a single variant is not allowed.
- Unnamed syntax (`Circle(float)`) remains valid.

### ADT Enum Equality

ADT enum values support `==` and `!=`. Comparison is structural: the variant tag is checked first; if the tags differ the values are unequal. When the tags match, every payload field is compared in order using the same rules as record field comparison.

```python
enum Shape:
    Circle(float)
    Rect(float, float)
    Point

Shape::Circle(1.0) == Shape::Circle(1.0)  # true
Shape::Circle(1.0) == Shape::Circle(2.0)  # false — same variant, different payload
Shape::Circle(1.0) == Shape::Point        # false — different variant
Shape::Point       == Shape::Point        # true  — no payload
```

Payload fields with function types are not equatable; comparing values whose matching variant carries a `function` payload is a compile-time error.

### Constraints and Errors

| Constraint | Details |
|------|------|
| Variant access requires `EnumName::VariantName` | The `::` operator is required |
| Variant values | Auto-assigned (0, 1, 2, ...) by default, or explicitly specified with `= value` |
| Comparison uses integer comparison | `==`, `!=` can be used |
| ADT comparison | Structural: tag then payload field-by-field; function-typed fields are a compile error |
| Named field names | Must be `snake_case`; no duplicates within a variant; no mixing named/unnamed |
