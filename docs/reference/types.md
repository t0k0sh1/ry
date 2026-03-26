[English](types.md) | [日本語](../ja/reference/types.md) | [繁體中文](../zh/reference/types.md)

# Type Reference

## Type List

| Type | Internal Representation | Literal Examples | Description |
|---|---|---|---|
| `int` | i64 | `42`, `-7`, `0xFF`, `0b1010` | 64-bit signed integer |
| `byte` | i8 | (no dedicated literal) | Unsigned 8-bit integer (0-255). Used with type annotation `b: byte = 42` |
| `float` | f64 | `3.14`, `0.5` | 64-bit floating-point number |
| `bool` | i1 | `true`, `false` | Boolean value |
| `str` | ptr | `"hello"`, `""`, `"a\nb"` | String (immutable byte sequence on the heap) |
| `Unit` | void | (no return value) | Implicit return type when return type is omitted |
| `Option<T>` | `{ i1, T }` | `Some(42)`, `None` | A type that may or may not contain a value |
| `(T1, T2, ...)` | LLVM StructType (literal) | `(1, 3.14)` | Tuple type |
| `List<T>` | ptr (heap) | `[1, 2, 3]` | Dynamic array |
| `Map<K, V>` | ptr (heap) | `{"a": 1}` | Hash map |
| `Set<T>` | ptr (heap) | `{1, 2, 3}` | Set with no duplicates |
| `fn(T1, T2) -> R` | ptr (function pointer) | `fn(x: int): x * 2` | Function type |
| User-defined type | LLVM StructType (named) | `record Point: ...` | Struct defined with the `record` keyword |
| `enum` | i64 / tagged union | `Color::Red`, `Shape::Circle(3.14)` | Enumeration defined with the `enum` keyword (supports associated data) |
| `Error` | `{ ptr, i64 }` | `Error("msg")`, `Error("msg", 404)` | Built-in error type |
| `any` | `{ i64, [8 x i8] }` | `x: any = 42` | Tagged union that can hold any primitive value |
| `T1 \| T2` | `{ i64, [N x i8] }` | `int \| str` | Union type (holds one of multiple types) |
| Int literal | i64 | `42`, `0 \| 1` | Int literal type (value constraint) |
| String literal | ptr | `"N" \| "S"` | String literal type (value constraint) |
| Range | i64 | `1..12`, `-10..10` | Range type (inclusive integer range constraint) |

## Type Annotation Syntax

You can explicitly specify the type when declaring a variable. The annotation can be omitted when the type is inferrable.

```python
x: int = 42
b: byte = 255
f: float = 3.14
s: str = "hello"
b: bool = true
opt: Option<int> = Some(10)
t: (int, float) = (1, 3.14)
xs: List<int> = [1, 2, 3]
m: Map<str, int> = {"a": 1}
s: Set<int> = {1, 2, 3}
fn_val: fn(int) -> int = fn(x: int): x * 2
u: int | str = 42
a: any = 42
```

## Available Type Names

| Type Name | Notes |
|---|---|
| `int` | Built-in scalar type |
| `byte` | Built-in scalar type (unsigned 0-255) |
| `float` | Built-in scalar type |
| `bool` | Built-in scalar type |
| `str` | Built-in string type |
| `Unit` | Return type of functions that return no value |
| `Option<T>` | Generic type (T is any type) |
| `(T1, T2, ...)` | Tuple type (arbitrary number and combination of element types) |
| `List<T>` | Generic dynamic array type |
| `Map<K, V>` | Generic hash map type |
| `Set<T>` | Generic set type |
| `fn(T1, ...) -> R` | Function type |
| `Error` | Built-in error type (`message: str`, `code: int`) |
| `any` | Built-in type that can hold any primitive value (`int`, `float`, `bool`, `str`) |
| `T1 \| T2 \| ...` | Union type (one of multiple types separated by `\|`) |
| User-defined type name | Type declared with the `record` or `enum` keyword |

## Type Aliases

The `type` keyword creates a new name for an existing type. The alias is fully interchangeable with the original type.

```python
type Meters = float
type StringList = List<str>

d: Meters = 3.14
names: StringList = ["Alice", "Bob"]
```

> **Naming convention**: Type alias names must use PascalCase (e.g., `Meters`, `StringList`). The compiler enforces this convention.

Type aliases also work with function types, literal types, and range types:

```python
type Callback = fn(int, int) -> int

add: Callback = fn(a: int, b: int): a + b
print(add(3, 4))    # 7
```

```python
type Month = 1..12
type Direction = "N" | "S" | "E" | "W"
type Digit = 0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9

m: Month = 6
d: Direction = "N"
n: Digit = 5
```

---

## Literal Types

A literal type restricts a variable to specific constant values. The compiler checks these constraints at compile time for constant values, and emits runtime checks for dynamic values.

### Int Literal Type

```python
x: 42 = 42           # single literal type
y: 0 | 1 = 0         # union of int literals
z: 0 | 1 = 0
z = 1                     # OK
# z = 2                   # compile error (constant) or runtime error (dynamic)
```

### String Literal Type

```python
dir: "N" | "S" | "E" | "W" = "N"
# @const bad: "N" | "S" = "X"    # compile error
```

### Constraint Checking

- **Compile time**: If the assigned value is a constant (`ConstantInt` or string literal), the constraint is checked at compile time and a compile error is raised on violation.
- **Runtime**: If the value is dynamic (e.g., from a function call), the constraint is checked at runtime and the program exits with an error on violation.

---

## Range Type

A range type constrains an integer variable to a contiguous range of values (inclusive on both ends).

```python
month: 1..12 = 6       # OK
# @const bad: 1..12 = 0       # compile error: out of range
# @const bad: 1..12 = 13      # compile error: out of range

t: -10..10 = -5        # negative ranges are supported
```

### With Mutable Variables (Runtime Check)

```python
x: 1..12 = 6
x = 12                      # OK
# x = dynamic_value()       # runtime check: exits if out of range
```

### In Function Parameters

```python
fn set_month(m: 1..12) -> int:
    return m

set_month(6)                # OK
# set_month(13)             # compile error (constant argument)
```

---

## `none` Keyword and Option Type Shorthand

The `none` keyword represents the absence of a value for Option types, equivalent to `None`.

The `T?` syntax is a shorthand for `Option<T>`.

```python
x: int? = 42       # equivalent to Option<int>
y: int? = none      # equivalent to None

fn find(xs: List<int>, val: int) -> int?:
    for x in xs:
        if x == val:
            return Some(x)
    return none
```

---

## F-String (String Interpolation)

String interpolation with the `f"..."` syntax. Expressions inside `{}` are evaluated and converted to strings.

```python
name = "world"
print(f"Hello {name}")     # Hello world

a = 1
b = 2
print(f"{a} + {b} = {a + b}")   # 1 + 2 = 3
```

### Supported Types in Interpolation

Any expression that evaluates to `int`, `float`, `bool`, or `str` can be used inside `{}`.

### Escape Sequences

| Sequence | Output |
|---|---|
| `{{` | `{` (literal brace) |
| `}}` | `}` (literal brace) |
| `\n` `\r` `\t` `\\` `\"` | Same as regular strings |

```python
print(f"{{braces}}")   # {braces}
```

## Type Casting (`as`)

Explicit type conversion using the `as` keyword.

```python
x = 42 as float     # 42.0
y = 3.14 as int      # 3
z = 1 as bool        # true
s = 42 as str         # "42"
b = 255 as byte       # byte value 255
```

### Supported Casts

| From | To | Behavior |
|---|---|---|
| `int` | `float` | `SIToFP` |
| `float` | `int` | Truncation (`FPToSI`) |
| `int` | `bool` | `0` -> `false`, non-zero -> `true` |
| `bool` | `int` | `false` -> `0`, `true` -> `1` |
| `int` / `float` / `bool` | `str` | String representation |
| `int` | `byte` | Truncation (lower 8 bits) |
| `byte` | `int` | Zero extension |

Unsupported casts (e.g. `str as int`) cause a compile error. Use `to_int()` / `to_float()` for string-to-number conversions.

## Enum with Associated Data (ADT)

Enum variants can carry associated data by specifying types in parentheses after the variant name. Variants without parentheses remain simple tags.

```python
enum Shape:
    Circle(float)
    Rectangle(float, float)
    Point
```

### Constructor

Use the `EnumName::Variant(value)` syntax to construct a variant with data.

```python
c = Shape::Circle(3.14)
r = Shape::Rectangle(4.0, 5.0)
p = Shape::Point
```

### Pattern Matching with Binding

Use `case EnumName::Variant(binding):` to extract the associated data.

```python
match c:
    case Shape::Circle(r):
        print(r)            # 3.14
    case Shape::Rectangle(w, h):
        print(w)
        print(h)
    case Shape::Point:
        print("point")
```

### Internal Representation

An ADT enum is stored as a tagged union: `{ i64 tag, [N x i8] data }` where `N` is sized to fit the largest variant's payload.

---

## Generic Enum

An enum can have type parameters using angle-bracket syntax `<T>`. This allows the same enum shape to hold different payload types.

```python
enum MyOption<T>:
    MySome(T)
    MyNone
```

### Usage

Instantiate by providing a concrete type argument. The type argument is required when the compiler cannot infer it.

```python
a = MyOption<int>::MySome(42)
b = MyOption<int>::MyNone

match a:
    case MyOption::MySome(v):
        print(v)      # 42
    case MyOption::MyNone:
        print("none")
```

---

## Error Type

A built-in type for error handling. `Error` has two fields: `message` (str) and `code` (int).

```python
e = Error("something went wrong")       # code defaults to 0
e2 = Error("not found", 404)            # explicit code

print(e.message)   # something went wrong
print(e2.code)     # 404
print(e2)          # Error: not found (code: 404)
```

### Error Handling with Result

Functions that can fail return `Result<V, E>`:

```python
fn divide(a: int, b: int) -> Result<int, Error>:
    if b == 0:
        return Err(Error("division by zero"))
    return Ok(a // b)

match divide(10, 2):
    case Ok(v):
        print(v)            # 5
    case Err(e):
        print(e.message)
```

When the return value is not meaningful, use `Result<Unit, Error>`:

```python
fn save(path: str, data: str) -> Result<Unit, Error>:
    return Ok(0 as byte)   # Unit placeholder

match save("/tmp/test.txt", "hello"):
    case Ok(_):
        print("saved")
    case Err(e):
        print(e.message)
```

### Result Type

`Result<V, E>` is a built-in parameterized type with two constructors:

- `Ok(value)` — success variant
- `Err(error)` — error variant

It is used with `match` for exhaustive error handling. Both `Ok` and `Err` cases must be covered (or use `_` wildcard).

**Test matchers:**
- `expect(x).to_be_ok()` — asserts the result is `Ok`
- `expect(x).to_be_err()` — asserts the result is `Err`

### Internal Representation

`Error` is represented as `{ ptr message, i64 code }`.
`Result<V, E>` is represented as `{ i1 isOk, V okValue, E errValue }`.

## Union Type

You can declare a variable that may hold one of multiple types using `|`.

```python
x: int | str = 42
x = "hello"     # Reassignment is allowed (any type in the union)
print(x)        # hello
```

### Usage in Function Parameters and Return Types

```python
fn show(x: int | str) -> int:
    print(x)
    return 0

fn get_val(flag: bool) -> int | str:
    if flag:
        return 42
    return "hello"
```

### Internal Representation

A union type is represented as `{ i64 tag, [N x i8] data }`. The `tag` indicates the index of each component type (sorted alphabetically), and `data` is a byte array sized to the largest component type.

### Constraints

- Assigning a type not included in the union causes a compile error
- `int | str` and `str | int` are the same type (normalized)
- When printing a union value with `print()`, the value is displayed using the appropriate type based on the runtime tag

## Type Rules (Type Conversion in Operations)

| Operation | Left | Right | Result Type | Notes |
|---|---|---|---|---|
| `+` `-` `*` | int | int | int | |
| `+` `-` `*` | byte | byte or int | int | byte is ZExt-promoted to int during operations |
| `+` `-` `*` | float or int | float or int (one is float) | float | Implicit float promotion |
| `/` | any numeric | any numeric | float | Always float |
| `//` | any numeric | any numeric | int or float | Floor division (toward -∞); int for int operands, float if either is float |
| `**` | any numeric | any numeric | float | Uses libm `pow` |
| `%` | int | int | int | |
| `%` | float or int | float or int (one is float) | float | |
| `+` | str | str | str | String concatenation |
| `==` `!=` `<` `<=` `>` `>=` | str | str | bool | Lexicographic comparison |
| `==` `!=` `<` `<=` `>` `>=` | numeric or bool | numeric or bool | bool | |
| `in` | any | Set<T> | bool | Whether the element is in the set |
| `&` `\|` `^` `~` `<<` `>>` | int | int | int | Error for float |

### Escape Sequences (in str Literals)

| Sequence | Meaning |
|---|---|
| `\n` | Newline |
| `\r` | Carriage return |
| `\t` | Tab |
| `\\` | Backslash |
| `\"` | Double quote |
| `\0` | Null character |

## Type Safety Constraints

- **No implicit type conversions** -- Mixing `int` and `float` triggers float promotion, but no other implicit conversions exist. `byte` is automatically promoted to `int` during operations (ZExt). Narrowing conversion from an `int` literal to `byte` is only allowed with a type annotation `b: byte = 42`.
- **Variable types are fixed at declaration** -- A variable declared as `int` cannot be reassigned a `float` value.
- **Bitwise operations are for `int` only** -- Applying bitwise operations to `float` or `bool` causes a compile error.
- **Non-`bool` types can be used in conditions** -- `if` conditions accept `int` (0 = false, non-zero = true) and other types besides `bool`.
