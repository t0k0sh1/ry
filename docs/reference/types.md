[English](types.md) | [日本語](../ja/reference/types.md) | [繁體中文](../zh/reference/types.md)

# Type Reference

## Type List

| Type | Internal Representation | Literal Examples | Description |
|---|---|---|---|
| `int` | i64 | `42`, `-7`, `0xFF`, `0b1010` | 64-bit signed integer |
| `byte` | i8 | (no dedicated literal) | Unsigned 8-bit integer (0-255). Used with type annotation `let b: byte = 42` |
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
| User-defined type | LLVM StructType (named) | `type Point: ...` | Struct defined with the `type` keyword |
| `enum` | i64 / tagged union | `Color::Red`, `Shape::Circle(3.14)` | Enumeration defined with the `enum` keyword (supports associated data) |
| `Result<T, E>` | `{ i64, [N x i8] }` | `Ok(42)`, `Err("fail")` | Result type for error handling |
| `T1 \| T2` | `{ i64, [N x i8] }` | `int \| str` | Union type (holds one of multiple types) |

## Type Annotation Syntax

You can explicitly specify the type when declaring a variable. The annotation can be omitted when the type is inferrable.

```python
let x: int = 42
let b: byte = 255
let f: float = 3.14
let s: str = "hello"
let b: bool = true
let opt: Option<int> = Some(10)
let t: (int, float) = (1, 3.14)
let xs: List<int> = [1, 2, 3]
let m: Map<str, int> = {"a": 1}
let s: Set<int> = {1, 2, 3}
let fn_val: fn(int) -> int = fn(x: int): x * 2
let u: int | str = 42
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
| `Result<T, E>` | Result type (T = Ok type, E = Err type) |
| `T1 \| T2 \| ...` | Union type (one of multiple types separated by `\|`) |
| User-defined type name | Type declared with the `type` or `enum` keyword |

## F-String (String Interpolation)

String interpolation with the `f"..."` syntax. Expressions inside `{}` are evaluated and converted to strings.

```python
let name = "world"
print(f"Hello {name}")     # Hello world

let a = 1
let b = 2
print(f"{a} + {b} = {a + b}")   # 1 + 2 = 3
```

### Supported Types in Interpolation

Any expression that evaluates to `int`, `float`, `bool`, or `str` can be used inside `{}`.

### Escape Sequences

| Sequence | Output |
|---|---|
| `{{` | `{` (literal brace) |
| `}}` | `}` (literal brace) |
| `\n` `\t` `\\` `\"` | Same as regular strings |

```python
print(f"{{braces}}")   # {braces}
```

## Type Casting (`as`)

Explicit type conversion using the `as` keyword.

```python
let x = 42 as float     # 42.0
let y = 3.14 as int      # 3
let z = 1 as bool        # true
let s = 42 as str         # "42"
let b = 255 as byte       # byte value 255
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
let c = Shape::Circle(3.14)
let r = Shape::Rectangle(4.0, 5.0)
let p = Shape::Point
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
let a = MyOption<int>::MySome(42)
let b = MyOption<int>::MyNone

match a:
    case MyOption::MySome(v):
        print(v)      # 42
    case MyOption::MyNone:
        print("none")
```

---

## Result Type

A type for recoverable error handling. A `Result<T, E>` value is either `Ok(value)` (success) or `Err(error)` (failure).

```python
fn divide(a: int, b: int) -> Result<int, str>:
    if b == 0:
        return Err("division by zero")
    return Ok(a // b)

let r = divide(10, 2)
match r:
    case Ok(v):
        print(v)      # 5
    case Err(e):
        print(e)
```

### Internal Representation

`Result<T, E>` is represented as `{ i64 tag, [max(sizeof(T), sizeof(E)) x i8] data }`. Tag 0 = Ok, Tag 1 = Err.

### Pattern Matching

Use `match` with `Ok(binding)` and `Err(binding)` patterns. Both patterns are required for exhaustive matching.

## Union Type

You can declare a variable that may hold one of multiple types using `|`.

```python
let x: int | str = 42
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
| `//` | any numeric | any numeric | int | Float input is truncated |
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
| `\t` | Tab |
| `\\` | Backslash |
| `\"` | Double quote |
| `\0` | Null character |

## Type Safety Constraints

- **No implicit type conversions** -- Mixing `int` and `float` triggers float promotion, but no other implicit conversions exist. `byte` is automatically promoted to `int` during operations (ZExt). Narrowing conversion from an `int` literal to `byte` is only allowed with a type annotation `let b: byte = 42`.
- **Variable types are fixed at declaration** -- A variable declared as `int` cannot be reassigned a `float` value.
- **Bitwise operations are for `int` only** -- Applying bitwise operations to `float` or `bool` causes a compile error.
- **Non-`bool` types can be used in conditions** -- `if` conditions accept `int` (0 = false, non-zero = true) and other types besides `bool`.
