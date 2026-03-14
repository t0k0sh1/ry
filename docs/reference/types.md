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
| `Result<T, E>` | `{ i64, [N x i8] }` | `Ok(42)`, `Err("fail")` | A type that represents success or failure |
| `fn(T1, T2) -> R` | ptr (function pointer) | `(x: int) -> x * 2` | Function type |
| User-defined type | LLVM StructType (named) | `type Point: ...` | Struct defined with the `type` keyword |
| `enum` | i64 | `Color::Red` | Enumeration defined with the `enum` keyword |
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
let res: Result<int, str> = Ok(42)
let t: (int, float) = (1, 3.14)
let xs: List<int> = [1, 2, 3]
let m: Map<str, int> = {"a": 1}
let s: Set<int> = {1, 2, 3}
let fn_val: fn(int) -> int = (x: int) -> x * 2
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
| `Result<T, E>` | Generic result type (T is success type, E is error type) |
| `(T1, T2, ...)` | Tuple type (arbitrary number and combination of element types) |
| `List<T>` | Generic dynamic array type |
| `Map<K, V>` | Generic hash map type |
| `Set<T>` | Generic set type |
| `fn(T1, ...) -> R` | Function type |
| `T1 \| T2 \| ...` | Union type (one of multiple types separated by `\|`) |
| User-defined type name | Type declared with the `type` or `enum` keyword |

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

## Result Type

A type that represents either success (`Ok`) or failure (`Err`).

```python
let r: Result<int, str> = Ok(42)
let e: Result<int, str> = Err("not found")

match r:
    case Ok(v):
        print(v)       # 42
    case Err(msg):
        print(msg)
```

### Constructors and Functions

| Function | Description |
|---|---|
| `Ok(value)` | Constructs a success result |
| `Err(value)` | Constructs an error result |
| `is_ok(r)` | Returns `true` if `r` is `Ok` |
| `is_err(r)` | Returns `true` if `r` is `Err` |
| `unwrap_or(r, default)` | Returns the `Ok` value, or `default` if `Err` |

### Internal Representation

`Result<T, E>` is represented as `{ i64 tag, [max(sizeof(T), sizeof(E)) x i8] data }`. The `tag` is `0` for `Ok` and `1` for `Err`.

### Match Exhaustiveness

When matching on a `Result`, both `Ok` and `Err` patterns must be covered (or use `_`).

---

## String Interpolation (f-string)

F-strings allow embedding expressions inside string literals using `{}`.

```python
let name = "world"
let n = 42
print(f"hello {name}, n = {n}")   # hello world, n = 42
```

### Supported Types in Interpolation

| Type | Format |
|---|---|
| `str` | As-is |
| `int` | `%lld` |
| `float` | `%g` |
| `bool` | `true` / `false` |
| `byte` | Unsigned integer |

### Escaping Braces

Use `{{` and `}}` to include literal braces.

```python
print(f"{{literal braces}}")   # {literal braces}
```

### Expression Support

Any expression can be used inside `{}`.

```python
let a = 10
let b = 20
print(f"sum = {a + b}")   # sum = 30
```

---

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
