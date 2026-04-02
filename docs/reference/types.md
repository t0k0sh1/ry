[English](types.md) | [日本語](../ja/reference/types.md) | [繁體中文](../zh/reference/types.md)

# Type Reference

## Type List

| Type | Internal Representation | Literal Examples | Description |
|---|---|---|---|
| `int` | i64 | `42`, `-7`, `0xFF`, `0b1010`, `100_000` | 64-bit signed integer |
| `u8` | i8 | (no dedicated literal) | Unsigned 8-bit integer (0-255). Used with type annotation `b: u8 = 42` |
| `float` | f64 | `3.14`, `0.5`, `.5`, `3.14_159` | 64-bit floating-point number |
| `bool` | i1 | `true`, `false` | Boolean value |
| `str` | ptr | `"hello"`, `""`, `"a\nb"` | String (immutable byte sequence on the heap) |
| `Unit` | void | (no return value) | Return type for functions with no return value. Must be specified explicitly with `-> Unit` |
| `Option<T>` | `{ i1, T }` | `Some(42)`, `None` | A type that may or may not contain a value |
| `(T1, T2, ...)` | LLVM StructType (literal) | `(1, 3.14)` | Tuple type |
| `List<T>` | ptr (heap) | `[1, 2, 3]` | Dynamic array |
| `Map<K, V>` | ptr (heap) | `{"a": 1}` | Hash map |
| `Set<T>` | ptr (heap) | `{1, 2, 3}` | Set with no duplicates |
| `function(T1, T2) -> R` | ptr (function pointer) | `(x: int) => x * 2` | Function type |
| User-defined type | LLVM StructType (named) | `record Point: ...` | Struct defined with the `record` keyword |
| `enum` | i64 / tagged union | `Color::Red`, `Shape::Circle(3.14)` | Enumeration defined with the `enum` keyword (supports associated data) |
| `Error` | `{ ptr, i64 }` | `Error("msg")`, `Error("msg", 404)` | Built-in error type |
| `any` | `{ i64, [8 x i8] }` | `x: any = 42` | Tagged union that can hold any primitive value |
| `T1 \| T2` | `{ i64, [N x i8] }` | `int \| str` | Union type (holds one of multiple types) |
| Int literal | i64 | `42`, `0 \| 1` | Int literal type (value constraint) |
| String literal | ptr | `"N" \| "S"` | String literal type (value constraint) |
| Range | i64 | `1..12`, `-10..10` | Range type (inclusive integer range constraint) |
| `i8` | i8 | `x: i8 = 42`, `x = 42i8` | 8-bit signed integer (low-level, no implicit conversion) |
| `i16` | i16 | `x: i16 = 100`, `x = 100i16` | 16-bit signed integer (low-level, no implicit conversion) |
| `i32` | i32 | `x: i32 = 42`, `x = 42i32` | 32-bit signed integer (low-level, no implicit conversion) |
| `i64` | i64 | `x: i64 = 100`, `x = 100i64` | 64-bit signed integer (low-level, no implicit conversion) |
| `u8` | i8 | `x: u8 = 200`, `x = 200u8` | 8-bit unsigned integer (low-level, no implicit conversion) |
| `u16` | i16 | `x: u16 = 60000`, `x = 60000u16` | 16-bit unsigned integer (low-level, no implicit conversion) |
| `u32` | i32 | `x: u32 = 3000000000`, `x = 100u32` | 32-bit unsigned integer (low-level, no implicit conversion) |
| `u64` | i64 | `x: u64 = 100`, `x = 100u64` | 64-bit unsigned integer (low-level, no implicit conversion) |
| `f32` | float | `x: f32 = 3.14`, `x = 3.14f32` | 32-bit floating-point (low-level, no implicit conversion) |
| `weak T` | ptr (header) | `weak s` | Weak reference to an ARC-managed value (does not prevent deallocation) |
| `Regex` | ptr | `/[a-z]+/`, `/\d{3}/` | Regular expression pattern (created via regex literal syntax) |
| `Result<T, E>` | `{ i1, T/E }` | `Ok(42)`, `Err(Error("fail"))` | A type representing success (`Ok`) or failure (`Err`) |
| `Task<T>` | ptr | (returned by async functions) | Asynchronous task handle (used with `await` and `block_on`) |
| `Iterator<T>` | ptr | (created by `iter()`) | Lazy iterator for sequential element access |
| `T[N]` | `[N x T]` | `buf: i32[8]` | Fixed-length contiguous array of low-level type T with N elements (stack-allocated) |

## Type Annotation Syntax

You can explicitly specify the type when declaring a variable. The annotation can be omitted when the type is inferrable.

```python
x: int = 42
b: u8 = 255
f: float = 3.14
s: str = "hello"
b: bool = true
opt: Option<int> = Some(10)
t: (int, float) = (1, 3.14)
xs: List<int> = [1, 2, 3]
m: Map<str, int> = {"a": 1}
s: Set<int> = {1, 2, 3}
fn_val: function(int) -> int = (x: int) => x * 2
rx: Regex = /[0-9]+/
u: int | str = 42
a: any = 42
```

## Available Type Names

| Type Name | Notes |
|---|---|
| `int` | Built-in scalar type |
| `u8` | Built-in scalar type (unsigned 0-255) |
| `float` | Built-in scalar type |
| `bool` | Built-in scalar type |
| `str` | Built-in string type |
| `Unit` | Return type of functions that return no value |
| `Option<T>` | Generic type (T is any type) |
| `(T1, T2, ...)` | Tuple type (arbitrary number and combination of element types) |
| `List<T>` | Generic dynamic array type |
| `Map<K, V>` | Generic hash map type |
| `Set<T>` | Generic set type |
| `function(T1, ...) -> R` | Function type |
| `Error` | Built-in error type (`message: str`, `code: int`) |
| `any` | Built-in type that can hold any primitive value (`int`, `float`, `bool`, `str`) or `Unit`. Supports implicit conversion: concrete values are automatically wrapped when assigned to `any`, and `any` values are automatically unwrapped (with runtime type check) when assigned to a concrete type. `any(int)` → `float` auto-promotion is supported. See [any Type](#any-type) for details |
| `T1 \| T2 \| ...` | Union type (one of multiple types separated by `\|`) |
| `i8` | Low-level 8-bit signed integer (no implicit conversion) |
| `i16` | Low-level 16-bit signed integer (no implicit conversion) |
| `i32` | Low-level 32-bit signed integer (no implicit conversion) |
| `i64` | Low-level 64-bit signed integer (no implicit conversion) |
| `u8` | Low-level 8-bit unsigned integer (no implicit conversion) |
| `u16` | Low-level 16-bit unsigned integer (no implicit conversion) |
| `u32` | Low-level 32-bit unsigned integer (no implicit conversion) |
| `u64` | Low-level 64-bit unsigned integer (no implicit conversion) |
| `f32` | Low-level 32-bit floating-point (no implicit conversion) |
| `T[N]` | Fixed-length array of low-level type `T` with `N` elements. Stack-allocated, contiguous memory. Supports index read/write and `length()` |
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
type Callback = function(int, int) -> int

add: Callback = function(a: int, b: int) => a + b
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
function set_month(m: 1..12) -> int:
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

function find(xs: List<int>, val: int) -> int?:
    for x in xs:
        if x == val:
            return Some(x)
    return none
```

---

## Weak References (`weak T`)

A `weak` reference is a non-owning reference to an ARC-managed value. Unlike strong references, weak references do not increment the strong reference count. When the last strong reference is released, the referenced object is deallocated — and any surviving weak references automatically become `None`.

Weak references are the user-facing mechanism for breaking reference cycles.

### Creating a Weak Reference

Use the `weak` keyword in both type annotation and expression position:

```python
s = "hello"
w: weak str = weak s
```

The type `weak T` is a new type constructor where `T` must be an ARC-managed type (currently `str`, `List<T>`, `Map<K, V>`, `Set<T>`).

### Accessing a Weak Reference (Upgrade)

Accessing a weak variable automatically performs an **upgrade** — an atomic check-and-increment of the strong reference count. The result is always `Option<T>`:

- `Some(value)` if the referent is still alive (strong count > 0)
- `None` if the referent has been deallocated (strong count == 0)

```python
s = "alive"
w: weak str = weak s
match w:
  case Some(v):
    print(v)           # "alive"
  case None:
    print("deallocated")
```

The coalesce operator (`??`) also works with weak references:

```python
w: weak str = weak s
val = w ?? "default"
```

### Reassignment

Weak references can be reassigned. The old weak reference is released and the new one is retained:

```python
a = "first"
b = "second"
w: weak str = weak a
w = weak b
```

### Thread Safety

The upgrade operation uses a compare-and-swap (CAS) loop internally, making it safe to use across threads. This is essential since the strong reference may be released concurrently.

### Scope Cleanup

Weak references are automatically released when they go out of scope. If both strong and weak reference counts reach zero, the ARC header is freed.

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

Any expression that evaluates to `int`, `float`, `bool`, `str`, a record type, a tuple, or a collection type (`List`, `Map`, `Set`) can be used inside `{}`.

```python
xs = [1, 2, 3]
print(f"items: {xs}")     # items: [1, 2, 3]

t = (1, "hello")
print(f"tuple: {t}")      # tuple: (1, hello)
```

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
b = 255 as u8         # u8 value 255
```

### Supported Casts

| From | To | Behavior |
|---|---|---|
| `int` | `float` | `SIToFP` |
| `float` | `int` | Truncation (`FPToSI`) |
| `int` | `bool` | `0` -> `false`, non-zero -> `true` |
| `bool` | `int` | `false` -> `0`, `true` -> `1` |
| `int` / `float` / `bool` | `str` | String representation |
| `int` | `u8` | Truncation (lower 8 bits) |
| `u8` | `int` | Zero extension |

| `int` | `i8` / `i16` / `i32` / `i64` | Truncation (or identity for i64) |
| `i8` / `i16` / `i32` / `i64` | `int` | Sign extension (`SExt`) |
| `int` | `u8` / `u16` / `u32` / `u64` | Truncation (or identity for u64) |
| `u8` / `u16` / `u32` / `u64` | `int` | Zero extension (`ZExt`) |
| signed | signed (wider) | Sign extension (`SExt`) |
| signed | signed (narrower) | Truncation |
| unsigned | unsigned/signed (wider) | Zero extension (`ZExt`) |
| unsigned | unsigned/signed (narrower) | Truncation |
| signed / unsigned int | `float` | `SIToFP` / `UIToFP` then `f64` |
| `float` | signed / unsigned int | `FPToSI` / `FPToUI` |
| `float` | `f32` | `FPTrunc` |
| `f32` | `float` | `FPExt` |
| signed int | `f32` | `SIToFP` |
| unsigned int | `f32` | `UIToFP` |
| `f32` | signed / unsigned int | `FPToSI` / `FPToUI` |

The target type of `as` supports the full type syntax, including generic types:

```python
x = value as Option<int>
y = data as Map<str, int>
```

Any `as` cast (including with generics) must be a built-in cast or have a matching user-defined `operator as`, otherwise it is a compile error. Use `to_int()` / `to_float()` for string-to-number conversions.

## Enum with Associated Data (ADT)

Enum variants can carry associated data by specifying types in parentheses after the variant name. Variants without parentheses remain simple tags.

```python
enum Shape:
    Circle(float)
    Rectangle(float, float)
    Point
```

### Named Fields

Variants can optionally use named fields for documentation clarity. Named fields make variant definitions self-describing but do not change runtime behavior — construction and pattern matching remain positional.

```python
enum Shape:
    Circle(radius: float)
    Rectangle(width: float, height: float)
    Point
```

Rules:
- Field names must be `snake_case`.
- Within a single variant, all fields must be either named or unnamed (no mixing).
- Duplicate field names within a variant are a compile error.

### Constructor

Use the `EnumName::Variant(value)` syntax to construct a variant with data. Arguments are always positional, even when fields are named.

```python
c = Shape::Circle(3.14)
r = Shape::Rectangle(4.0, 5.0)
p = Shape::Point
```

### Pattern Matching with Binding

Use `case EnumName::Variant(binding):` to extract the associated data. Bindings use user-chosen variable names, not field names.

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
function divide(a: int, b: int) -> Result<int, Error>:
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
function save(path: str, data: str) -> Result<Unit, Error>:
    return Ok(0 as u8)   # Unit placeholder

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
function show(x: int | str) -> int:
    print(x)
    return 0

function get_val(flag: bool) -> int | str:
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

## any Type

The `any` type is a built-in dynamic type that can hold any primitive value. It follows Python's approach of allowing flexible typing — when you don't need static type guarantees, `any` lets you write code that works with multiple types without explicit generics or union types.

### Supported Types

`any` can hold the following types:

| Type | Tag | Description |
|------|-----|-------------|
| `int` | 0 | 64-bit signed integer |
| `float` | 1 | 64-bit floating-point number |
| `bool` | 2 | Boolean value |
| `str` | 3 | String |
| `Unit` | 4 | Unit value (for functions with no return value) |

`any` **cannot** hold collection types (`List`, `Map`, `Set`), resource types (`TcpListener`, `TcpStream`, etc.), function pointers, or user-defined types (`record`, `enum`).

### Internal Representation

`any` is implemented as a tagged union:

```
{ i64 tag, [8 x i8] data }   // 16 bytes total
```

The `tag` field identifies the stored type, and the `data` field holds the value (up to 8 bytes).

### Wrapping and Unwrapping

Concrete values are automatically **wrapped** when assigned to `any`, and `any` values are automatically **unwrapped** when assigned to a concrete type.

```python
# Wrapping: concrete → any
x: any = 42          # int is wrapped into any
x = "hello"          # reassignment with a different type is allowed

# Unwrapping: any → concrete
function get_value() -> any:
    return 42
n: int = get_value()  # any(int) is unwrapped to int

# int → float auto-promotion during unwrap
f: float = get_value()  # any(int) is unwrapped and promoted to float
```

If the runtime type does not match the target type (e.g., unwrapping `any(str)` into an `int` variable), a **runtime error** occurs.

### Reassignment

An `any` variable can be reassigned to a value of any supported type:

```python
x: any = 42
x = 3.14       # OK: now holds float
x = "hello"    # OK: now holds str
x = true       # OK: now holds bool
```

### Arithmetic Operations

When both operands are `any`, arithmetic operations dispatch at runtime based on the actual types:

| Operation | Types | Result |
|-----------|-------|--------|
| `+` | int + int | int |
| `+` | float + float | float |
| `+` | int + float | float |
| `+` | str + str | str (concatenation) |
| `-` | numeric | int or float |
| `*` | numeric | int or float |
| `*` | str * int / int * str | str (repetition) |
| `/` | numeric | float (always) |
| `//` | int // int | int |
| `//` | with float | float |
| `%` | numeric | int or float |
| `**` | numeric | float (always) |
| unary `-` | int | int |
| unary `-` | float | float |

When one operand is `any` and the other is a concrete type, the concrete value is automatically wrapped before the operation.

```python
x: any = 10
y: any = x + 20    # 20 is auto-wrapped; result is any(int) = 30
```

Incompatible type combinations (e.g., `str - int`) cause a **runtime error**.

### Comparison Operations

| Operation | Behavior |
|-----------|----------|
| `==`, `!=` | Works for same types; int/float mixing is allowed |
| `<`, `<=`, `>`, `>=` | Numeric (int/float mixing allowed) and string (lexicographic) |

```python
x: any = 3
y: any = 3.0
print(x == y)    # true (int/float comparison)
```

Type mismatches in comparison (e.g., `int < str`) cause a **runtime error**.

### String Conversion

`any` values support `print()` and f-string interpolation:

```python
x: any = 42
print(x)              # 42
print(f"value: {x}")  # value: 42
```

Conversion rules: `int` → decimal string, `float` → `%g` format, `bool` → `"true"`/`"false"`, `str` → as-is, `Unit` → `"Unit"`.

### Passing any to Typed Functions

An `any` value can be passed to a function with concrete parameter types. The value is automatically unwrapped with a runtime type check:

```python
function add_one(x: int) -> int:
    return x + 1

v: any = 42
result = add_one(v)   # any(int) is unwrapped to int; result is 43
```

---

## Type Rules (Type Conversion in Operations)

| Operation | Left | Right | Result Type | Notes |
|---|---|---|---|---|
| `+` `-` `*` | int | int | int | |
| `+` `-` `*` | u8 | u8 | u8 | Low-level type: native-width unsigned operations, no implicit promotion |
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
| `+` `-` `*` | i32 | i32 | i32 | Low-level types: no implicit conversion, same type required |
| `/` `//` | i32 | i32 | i32 | Signed integer division (`SDiv`) |
| `/` `//` | u32 | u32 | u32 | Unsigned integer division (`UDiv`) |
| `%` | i32 | i32 | i32 | Signed remainder (`SRem`) |
| `%` | u32 | u32 | u32 | Unsigned remainder (`URem`) |
| `+` `-` `*` `/` | f32 | f32 | f32 | |
| `==` `!=` | i32/u32 | i32/u32 | bool | Sign-agnostic equality |
| `<` `<=` `>` `>=` | i32 | i32 | bool | Signed comparison (`ICMP_SLT` etc.) |
| `<` `<=` `>` `>=` | u32 | u32 | bool | Unsigned comparison (`ICMP_ULT` etc.) |
| `>>` | i32 | i32 | i32 | Arithmetic right shift (sign-preserving) |
| `>>` | u32 | u32 | u32 | Logical right shift (zero-fill) |
| `**` | low-level | any | error | Power operator not supported for low-level types |
| mixed | low-level | different | error | Mixing low-level and high-level types is a compile error |

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

- **Implicit widening conversions** -- Safe widening conversions are supported in function calls: `u8` → `int`, `u8` → `float`, `int` → `float`. For binary operators, mixing `int` and `float` triggers float promotion. `u8` is a low-level type that operates at native width with unsigned semantics; mixing `u8` with `int` in binary operators is a compile error. Narrowing conversions (e.g., `float` → `int`) are not allowed implicitly. Narrowing conversion from an `int` literal to `u8` is only allowed with a type annotation `b: u8 = 42`.
- **Variable types are fixed at declaration** -- A variable declared as `int` cannot be reassigned a `float` value.
- **Bitwise operations are for `int` only** -- Applying bitwise operations to `float` or `bool` causes a compile error.
- **Non-`bool` types can be used in conditions** -- `if` conditions accept `int` (0 = false, non-zero = true) and other types besides `bool`.
- **Numeric literal separators** -- Underscores can be used as visual separators in numeric literals: `100_000`, `0xFF_FF`, `0b1010_0101`, `3.14_159`. Underscores must appear between digits (no leading, trailing, or consecutive underscores).
- **Numeric literal suffixes** -- Low-level types can be specified via literal suffixes: `42i32`, `255u8`, `3.14f32`, `.5f32`, `0xFFu8`, `0b1010u8`. An integer literal with a float suffix (`42f32`) produces a float value. A float literal with an integer suffix (`3.14i32`) is a compile error. Out-of-range values (e.g., `256u8`, `129i8`) are also compile errors.
- **Low-level numeric types (`i8`, `i16`, `i32`, `i64`, `u8`, `u16`, `u32`, `u64`, `f32`) have no implicit conversions** -- Mixing low-level types with each other or with high-level types (`int`, `float`) causes a compile error. Use explicit `as` casts. The `/` operator on low-level integers performs integer division (like Rust), not float division. Signed types use `SDiv`/`SRem`, unsigned types use `UDiv`/`URem`.
- **Signed vs unsigned** -- Signed types (`i8`, `i16`, `i32`, `i64`) use signed comparison (`ICMP_SLT` etc.) and arithmetic right shift (`AShr`). Unsigned types (`u8`, `u16`, `u32`, `u64`) use unsigned comparison (`ICMP_ULT` etc.) and logical right shift (`LShr`). The `>>>` operator always performs logical shift regardless of signedness.
- **`int` arithmetic overflow is a runtime error** -- Arithmetic (`+`, `-`, `*`, unary `-`) on the high-level `int` type raises a runtime error on overflow, similar to Swift's default behavior. This prevents silent data corruption from two's complement wrapping. Constant expressions that overflow are caught at compile time.
- **Low-level integer overflow wraps around** -- Arithmetic on low-level integer types uses Ry-defined two's complement wrapping on overflow for signed types and modular arithmetic for unsigned types. For example, `i32` max value `2147483647 + 1` wraps to `-2147483648`. For explicit overflow control, use `checked_add/sub/mul` (returns `Result<T, Error>`), `saturating_add/sub/mul` (clamps to type bounds), or `wrapping_add/sub/mul` (self-documenting wrapping). See [Function Reference](functions.md#checkedsaturating-arithmetic).
