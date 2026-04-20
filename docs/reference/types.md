[English](types.md) | [日本語](../ja/reference/types.md) | [繁體中文](../zh/reference/types.md)

# Type Reference

## Type List

| Type | Internal Representation | Literal Examples | Description |
|---|---|---|---|
| `int` | i64 | `42`, `-7`, `0xFF`, `0b1010`, `100_000` | 64-bit signed integer |
| `float` | f64 | `3.14`, `0.5`, `3.14_159`, `1e10`, `1.5e-3`, `2.5E+2` | 64-bit floating-point number (scientific notation supported) |
| `bool` | i1 | `true`, `false` | Boolean value |
| `str` | ptr | `"hello"`, `""`, `"a\nb"` | String (immutable byte sequence on the heap). Internally a pointer to the data portion of a `StringHeader` (`{strong_count, weak_count, byte_len, data[], '\0'}`). Supports embedded NUL bytes (#1022). |
| `Unit` | void | (no return value) | Return type for functions with no return value. Must be specified explicitly with `-> Unit` |
| `Option<T>` | `{ i1, T }` | `Some(42)`, `None` | A type that may or may not contain a value |
| `(T1, T2, ...)` | LLVM StructType (literal) | `(1, 3.14)` | Tuple type |
| `List<T>` | ptr (heap) | `[1, 2, 3]` | Dynamic array |
| `Map<K, V>` | ptr (heap) | `{"a": 1}` | Hash map |
| `Set<T>` | ptr (heap) | `{1, 2, 3}` | Set with no duplicates |
| `function(T1, T2) -> R` | ptr (function pointer) | `(x: int) => x * 2` | Function type |
| User-defined type | LLVM StructType (named) | `record Point: ...` | Record defined with the `record` keyword |
| `enum` | i64 / tagged union | `Color::Red`, `Shape::Circle(3.14)` | Enumeration defined with the `enum` keyword (supports associated data) |
| `Error` | `{ ptr, i64 }` | `Error("msg")`, `Error("msg", 404)` | Built-in error type |
| `Type` | `{ i64, ptr }` | `type_of(42)` | Compile-time type identity returned by `type_of`. See [Type](#type) |
| `any` | `{ i64, [8 x i8] }` | `x: any = 42` | Tagged union that can hold any primitive value |
| `T1 \| T2` | `{ i64, [N x i8] }` | `int \| str` | Union type (holds one of multiple types) |
| Int literal | i64 | `42`, `0 \| 1` | Int literal type (value constraint) |
| String literal | ptr | `"N" \| "S"` | String literal type (value constraint). The handle points to an immortal `StringHeader` global. |
| Range | i64 | `1..12`, `-10..10` | Range type (inclusive integer range constraint) |
| `i8` | i8 | `x: i8 = 42`, `x = 42i8` | 8-bit signed integer (low-level, no implicit conversion) |
| `i16` | i16 | `x: i16 = 100`, `x = 100i16` | 16-bit signed integer (low-level, no implicit conversion) |
| `i32` | i32 | `x: i32 = 42`, `x = 42i32` | 32-bit signed integer (low-level, no implicit conversion) |
| `i64` | i64 | `x: i64 = 100`, `x = 100i64` | 64-bit signed integer (low-level, no implicit conversion) |
| `u8` | i8 | `x: u8 = 200`, `x = 200u8` | 8-bit unsigned integer (low-level, no implicit conversion) |
| `u16` | i16 | `x: u16 = 60000`, `x = 60000u16` | 16-bit unsigned integer (low-level, no implicit conversion) |
| `u32` | i32 | `x: u32 = 4294967295`, `x = 100u32` | 32-bit unsigned integer (low-level, no implicit conversion) |
| `u64` | i64 | `x: u64 = 18446744073709551615`, `x = 0xFFFFFFFFFFFFFFFFu64` | 64-bit unsigned integer up to 2^64 − 1 (low-level, no implicit conversion) |
| `f32` | float | `x: f32 = 3.14`, `x = 1e10f32` | 32-bit floating-point (low-level, no implicit conversion) |
| `weak T` | ptr (header) | `weak s` | Weak reference to an ARC-managed value (does not prevent deallocation) |
| `Regex` | ptr | `/[a-z]+/`, `/\d{3}/` | Regular expression pattern (created via regex literal syntax) |
| `Result<T, E>` | `{ i1, T, E }` | `Ok(42)`, `Err(Error("fail"))` | A type representing success (`Ok`) or failure (`Err`). Both `T` and `E` slots are always present in the struct; only the active variant is meaningful |
| `Task<T>` | ptr | (returned by async functions) | Asynchronous task handle (used with `await` and `block_on`) |
| `Iterator<T>` | ptr | (created by `iter()`) | Lazy iterator for sequential element access |
| `T[N]` | `[N x T]` | `buf: i32[8]` | Fixed-length contiguous array of low-level type T with N elements (stack-allocated) |

## Type Annotation Syntax

You can explicitly specify the type when declaring a variable. The annotation can be omitted when the type is inferrable.

```ry
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

```ry
type Meters = float
type StringList = List<str>

d: Meters = 3.14
names: StringList = ["Alice", "Bob"]
```

> **Naming convention**: Type alias names must use PascalCase (e.g., `Meters`, `StringList`). The compiler enforces this convention.

Type aliases also work with function types, literal types, and range types:

```ry
type Callback = function(int, int) -> int

add: Callback = function(a: int, b: int) => a + b
print(add(3, 4))    # 7
```

```ry
type Month = 1..12
type Direction = "N" | "S" | "E" | "W"
type Digit = 0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9

m: Month = 6
d: Direction = "N"
n: Digit = 5
```

Type aliases can also target union types (including primitive and user-defined types), and the alias behaves identically to the inlined union:

```ry
type Simple = int | str | bool

x: Simple = 42
y: Simple = "hello"
z: Simple = true

function describe(v: Simple) -> str:
  return to_str(v)
```

Nested aliases whose union components are themselves aliases are flattened transparently, and duplicate members are deduplicated. The following three forms are equivalent:

```ry
type A = int | str
type B = A | bool          # same as `int | str | bool`
type C = B | int           # same as `int | str | bool` (int is deduplicated)

x: B = 42
y: B = "hello"
z: B = true
```

---

## Numeric Literals

### Integer Literals

Decimal, hexadecimal (`0x`/`0X`), and binary (`0b`/`0B`) forms are accepted. Underscores are allowed between digits as a visual separator (`1_000_000`, `0xFFFF_FFFF`). Octal literals (`0o...`) are not supported; use `0x...` or `0b...` instead.

The accepted magnitude is determined by the target type:

| Target | Range |
|---|---|
| bare `int` / `i64` | `-9_223_372_036_854_775_808 .. 9_223_372_036_854_775_807` (i64) |
| `i8` / `i16` / `i32` | corresponding signed range |
| `u8` / `u16` / `u32` | `0 .. 2^N - 1` |
| `u64` | `0 .. 18_446_744_073_709_551_615` (2^64 − 1) |

Large unsigned literals require either a suffix (`18446744073709551615u64`) or a type annotation on the receiving variable (`x: u64 = 18446744073709551615`). Negative literals arrive as a unary minus on a non-negative magnitude, so `-1i8` is accepted while `-1u8` is rejected. The bare `int` minimum `-9223372036854775808` (INT64_MIN) can be written directly as a literal; the positive form `9223372036854775808` (without the leading `-`) is rejected.

```ry
max_u64: u64 = 18446744073709551615     # 2^64 - 1
mask:    u64 = 0xFFFF_FFFF_FFFF_FFFF    # same value via hex
word:    u32 = 4294967295               # 2^32 - 1
```

### Float Literals

```text
FloatLiteral := DecDigits '.' DecDigits Exponent? FloatSuffix?
             |  DecDigits Exponent FloatSuffix?
Exponent     := ('e' | 'E') ('+' | '-')? DecDigits
FloatSuffix  := 'f32' | 'f64'
```

Scientific notation is supported anywhere a float is expected:

```ry
avogadro  = 6.022e23
planck    = 6.626e-34
light_spd = 2.998E8
big       = 1e10f32
```

Overflowing exponents produce `+Inf`/`-Inf` (not a compile error). Note that the runtime `to_float()` converter is stricter: it returns `Err(Error)` on overflow rather than producing `+Inf`.

---

## Literal Types

A literal type restricts a variable to specific constant values. The compiler checks these constraints at compile time for constant values, and emits runtime checks for dynamic values.

### Int Literal Type

```ry
x: 42 = 42           # single literal type
y: 0 | 1 = 0         # union of int literals
z: 0 | 1 = 0
z = 1                     # OK
# z = 2                   # compile error (constant) or runtime error (dynamic)
```

### String Literal Type

```ry
dir: "N" | "S" | "E" | "W" = "N"
# @const bad: "N" | "S" = "X"    # compile error
```

### Constraint Checking

- **Compile time**: If the assigned value is a constant (`ConstantInt` or string literal), the constraint is checked at compile time and a compile error is raised on violation.
- **Runtime**: If the value is dynamic (e.g., from a function call), the constraint is checked at runtime and the program exits with an error on violation.

---

## Range Type

A range type constrains an integer variable to a contiguous range of values (inclusive on both ends).

```ry
month: 1..12 = 6       # OK
# @const bad: 1..12 = 0       # compile error: out of range
# @const bad: 1..12 = 13      # compile error: out of range

t: -10..10 = -5        # negative ranges are supported
```

### With Mutable Variables (Runtime Check)

```ry
x: 1..12 = 6
x = 12                      # OK
# x = dynamic_value()       # runtime check: exits if out of range
```

### In Function Parameters

```ry
function set_month(m: 1..12) -> int:
    return m

set_month(6)                # OK
# set_month(13)             # compile error (constant argument)
```

---

## `none` Keyword and Option Type Shorthand

The `none` keyword represents the absence of a value for Option types, equivalent to `None`.

The `T?` syntax is a shorthand for `Option<T>`.

```ry
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

Use `weak` (a contextual identifier, not a reserved keyword) in both type annotation and expression position:

```ry
s = "hello"
w: weak str = weak s
```

The type `weak T` is a new type constructor where `T` must be an ARC-managed type (currently `str`, `List<T>`, `Map<K, V>`, `Set<T>`). `T` may also be a type alias that resolves to one of these managed types — the compiler resolves aliases to their canonical form at the point the weak variable is declared.

```ry
type MyStr = str
s = "hello"
w: weak MyStr = weak s   # MyStr resolves to str — works correctly
```

### Accessing a Weak Reference (Upgrade)

Accessing a weak variable automatically performs an **upgrade** — an atomic check-and-increment of the strong reference count. The result is always `Option<T>`:

- `Some(value)` if the referent is still alive (strong count > 0)
- `None` if the referent has been deallocated (strong count == 0)

```ry
s = "alive"
w: weak str = weak s
case w:
  Some(v):
    print(v)           # "alive"
  None:
    print("deallocated")
```

The coalesce operator (`??`) also works with weak references:

```ry
w: weak str = weak s
val = w ?? "default"
```

### Reassignment

Weak references can be reassigned. The old weak reference is released and the new one is retained:

```ry
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

```ry
name = "world"
print(f"Hello {name}")     # Hello world

a = 1
b = 2
print(f"{a} + {b} = {a + b}")   # 1 + 2 = 3
```

### Supported Types in Interpolation

Any expression that evaluates to `int`, `float`, `bool`, `str`, a record type, a tuple, or a collection type (`List`, `Map`, `Set`) can be used inside `{}`.

```ry
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

```ry
print(f"{{braces}}")   # {braces}
```

## Type Casting (`as`)

Explicit type conversion using the `as` keyword.

```ry
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
| `float` | `int` | Truncation (`FPToSI`). NaN / ±inf / out-of-range values raise a runtime error. |
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
| `float` | signed / unsigned int | `FPToSI` / `FPToUI`. NaN / ±inf / out-of-range values raise a runtime error. |
| `float` | `f32` | `FPTrunc` |
| `f32` | `float` | `FPExt` |
| signed int | `f32` | `SIToFP` |
| unsigned int | `f32` | `UIToFP` |
| `f32` | signed / unsigned int | `FPToSI` / `FPToUI`. NaN / ±inf / out-of-range values raise a runtime error. |

The target type of `as` supports the full type syntax, including generic types:

```ry
x = value as Option<int>
y = data as Map<str, int>
```

Any `as` cast (including with generics) must be a built-in cast or have a matching user-defined `operator as`, otherwise it is a compile error. Use `to_int()` / `to_float()` for string-to-number conversions.

### Float → Integer Runtime Checks

Every float-to-integer conversion (`float`/`f32` → `int`/`i8`/`i16`/`i32`/`i64`/`u8`/`u16`/`u32`/`u64`, including the implicit coercion when assigning a `float` to an `int`-typed variable or using a compound operator such as `/=` on an `int`) is guarded at runtime. If the source value is `NaN`, `±inf`, or outside the target integer's representable range, the program prints `runtime error: cannot convert <value> to <type>` to standard error and exits with status `1`. The guards use half-open intervals (`[-2^(W-1), 2^(W-1))` for signed `W`-bit and `[0, 2^W)` for unsigned `W`-bit) so that exactly-representable boundaries such as `INT64_MIN` are accepted.

## Enum with Associated Data (ADT)

Enum variants can carry associated data by specifying types in parentheses after the variant name. Variants without parentheses remain simple tags.

```ry
enum Shape:
    Circle(float)
    Rectangle(float, float)
    Point
```

### Named Fields

Variants can optionally use named fields for documentation clarity. Named fields make variant definitions self-describing but do not change runtime behavior — construction and pattern matching remain positional.

```ry
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

```ry
c = Shape::Circle(3.14)
r = Shape::Rectangle(4.0, 5.0)
p = Shape::Point
```

### Pattern Matching with Binding

Use `case EnumName::Variant(binding):` to extract the associated data. Bindings use user-chosen variable names, not field names.

```ry
case c:
    Shape::Circle(r):
        print(r)            # 3.14
    Shape::Rectangle(w, h):
        print(w)
        print(h)
    Shape::Point:
        print("point")
```

### Equality

ADT enums support `==` and `!=`. Comparison is structural: first the variant tag is compared; if the tags differ the values are unequal. If the tags match, every payload field is compared in order (the same field-by-field semantics used for records).

```ry
Shape::Circle(1.0) == Shape::Circle(1.0)   # true
Shape::Circle(1.0) == Shape::Circle(2.0)   # false — same tag, different payload
Shape::Circle(1.0) == Shape::Point         # false — different tag
Shape::Point       == Shape::Point         # true  — no payload, tag equality
```

Variants with no payload (e.g. `Point`) compare by tag only, which is always enough.
Payload fields that are themselves ADT enums, records, collections, or strings are compared recursively.
Payload fields with function types are not equatable; comparing two values whose matching variant carries a `function` payload is a compile-time error.

### Internal Representation

An ADT enum is stored as a tagged union: `{ i64 tag, [N x i8] data }` where `N` is sized to fit the largest variant's payload.

---

## Generic Enum

An enum can have type parameters using angle-bracket syntax `<T>`. This allows the same enum shape to hold different payload types.

```ry
enum MyOption<T>:
    MySome(T)
    MyNone
```

### Usage

Instantiate by providing a concrete type argument. The type argument is required when the compiler cannot infer it.

```ry
a = MyOption<int>::MySome(42)
b = MyOption<int>::MyNone

case a:
    MyOption::MySome(v):
        print(v)      # 42
    MyOption::MyNone:
        print("none")
```

A generic enum can also be used as a function parameter, return type, or let-binding type annotation. The type argument must be supplied wherever the enum appears in the signature:

```ry
function unwrap_or_int(opt: MyOption<int>, default: int) -> int:
    case opt:
        MyOption::MySome(v):
            return v
        MyOption::MyNone:
            return default
    return default

# Inside a generic function, the type parameter is substituted into nested generics:
function unwrap_or<T>(opt: MyOption<T>, default: T) -> T:
    case opt:
        MyOption::MySome(v):
            return v
        MyOption::MyNone:
            return default
    return default
```

Writing a generic enum without any type argument in a signature (for example `opt: MyOption`) is a compile error. Always qualify it as `MyOption<int>`, `MyOption<T>`, etc.

### Recursive Enum Limitation

Directly referencing an enum type inside one of its own variant fields is not supported because the resulting layout has unbounded size. The following fails to compile with a diagnostic that points to wrapper types:

```ry
enum Tree:
    Leaf(int)
    Node(int, Tree, Tree)   # error: self-referential field requires infinite storage
```

Wrap the recursive field in an indirection type — `List<T>`, `Map<K, V>`, or `Set<T>` — which is stored as a pointer and therefore has a fixed layout:

```ry
enum Tree:
    Leaf(int)
    Node(int, List<Tree>)   # OK — List payload is boxed
```

---

## Error Type

A built-in type for error handling. `Error` has two fields: `message` (str) and `code` (int).

```ry
e = Error("something went wrong")       # code defaults to 0
e2 = Error("not found", 404)            # explicit code

print(e.message)   # something went wrong
print(e2.code)     # 404
print(e2)          # Error: not found (code: 404)
```

### Error Handling with Result

Functions that can fail return `Result<V, E>`:

```ry
function divide(a: int, b: int) -> Result<int, Error>:
    if b == 0:
        return Err(Error("division by zero"))
    return Ok(a // b)

case divide(10, 2):
    Ok(v):
        print(v)            # 5
    Err(e):
        print(e.message)
```

When the return value is not meaningful, use `Result<Unit, Error>`:

```ry
function save(path: str, data: str) -> Result<Unit, Error>:
    return Ok(0 as u8)   # Unit placeholder

case save("/tmp/test.txt", "hello"):
    Ok(_):
        print("saved")
    Err(e):
        print(e.message)
```

### Result Type

`Result<V, E>` is a built-in parameterized type with two constructors:

- `Ok(value)` — success variant
- `Err(error)` — error variant

It is used with `case` for exhaustive error handling. Both `Ok` and `Err` cases must be covered (or use `_` wildcard).

**Equality:**
`Result<T, E>` supports `==` and `!=`. Two results are equal when both variants match (`Ok`/`Ok` or `Err`/`Err`) and the inner values are equal.

```ry
function make_ok(v: int) -> Result<int, Error>: return Ok(v)
make_ok(42) == make_ok(42)   # true
make_ok(1)  == make_ok(2)    # false
make_ok(1)  != Err(Error("e"))  # true
```

**Test matchers:**
- `expect(x).to_be_ok()` — asserts the result is `Ok`
- `expect(x).to_be_err()` — asserts the result is `Err`

### Internal Representation

`Error` is represented as `{ ptr message, i64 code }`.
`Result<V, E>` is represented as `{ i1 isOk, V okValue, E errValue }`.

## Type

`Type` is the value returned by the built-in [`type_of`](builtins.md#type_of) function. It represents the compile-time identity of a type and allows reflective comparison at run time.

```ry
print(to_str(type_of(42)))          # int
print(to_str(type_of([1, 2, 3])))   # List

print(type_of(42) == type_of(100))  # true
print(type_of(42) == type_of(3.14)) # false
```

Key properties:

- Each distinct type definition (primitive, collection, record, enum, `Option`, `Result`, `function`, `Type` itself, etc.) receives a unique identity at compile time.
- `==` / `!=` on `Type` values compare identities, not display names. Two different records (or a record and an enum with the same name) are always distinguishable.
- `print` and `to_str` display the human-readable type name (for example, `"int"`, `"List"`, `"Point"`, `"i32"`).
- Low-level numeric types (`i8`, `i16`, …, `f32`) are distinguished from `int` / `float`.
- Collection generics collapse to their base name: `type_of([1, 2])` returns `"List"`, not `"List<int>"`.
- `Type` is reflective: `type_of(type_of(x))` returns the `Type` value that represents `Type` itself.

### Internal Representation

`Type` is represented as `{ i64 id, ptr name }`. The `id` field is used for equality and the `name` field is used for display. Both fields are populated at compile time by `type_of`.

## Union Type

You can declare a variable that may hold one of multiple types using `|`.

```ry
x: int | str = 42
x = "hello"     # Reassignment is allowed (any type in the union)
print(x)        # hello
```

### Usage in Function Parameters and Return Types

```ry
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

### Equality

Union types support `==` and `!=` for all comparable variant types, including primitives (`int`, `float`, `str`, `bool`), collections (`List`, `Map`, `Set`), records, ADT enums, and nested unions. Two union values are equal when they hold the same variant (same tag) and the inner values are equal.

```ry
x: int | str = 42
y: int | str = 42
x == y   # true

z: int | str = "42"
x == z   # false (different tags: int vs str)

a: List<int> | int = [1, 2, 3]
b: List<int> | int = [1, 2, 3]
a == b   # true (same tag, element-wise equal)
```

### Constraints

- Assigning a type not included in the union causes a compile error
- `int | str` and `str | int` are the same type (normalized)
- When printing a union value with `print()`, the value is displayed using the appropriate type based on the runtime tag
- `==` and `!=` support all comparable variant types; function-typed (closure) variants are not supported

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

```ry
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

```ry
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

```ry
x: any = 10
y: any = x + 20    # 20 is auto-wrapped; result is any(int) = 30
```

Incompatible type combinations (e.g., `str - int`) cause a **runtime error**.

### Comparison Operations

| Operation | Behavior |
|-----------|----------|
| `==`, `!=` | Works for same types; int/float mixing is allowed |
| `<`, `<=`, `>`, `>=` | Numeric (int/float mixing allowed) and string (lexicographic) |

```ry
x: any = 3
y: any = 3.0
print(x == y)    # true (int/float comparison)
```

Type mismatches in comparison (e.g., `int < str`) cause a **runtime error**.

### String Conversion

`any` values support `print()` and f-string interpolation:

```ry
x: any = 42
print(x)              # 42
print(f"value: {x}")  # value: 42
```

Conversion rules: `int` → decimal string, `float` → `%g` format, `bool` → `"true"`/`"false"`, `str` → as-is, `Unit` → `"Unit"`.

### Passing any to Typed Functions

An `any` value can be passed to a function with concrete parameter types. The value is automatically unwrapped with a runtime type check:

```ry
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

- **Implicit widening conversions** -- Safe widening conversions are supported in function calls: `u8` → `int`, `u8` → `float`, `int` → `float`. For binary operators, mixing `int` and `float` triggers float promotion. `u8` is a low-level type that operates at native width with unsigned semantics; mixing `u8` with `int` in binary operators is a compile error. Narrowing conversion from an `int` literal to `u8` is only allowed with a type annotation `b: u8 = 42`.
- **Implicit `int` ↔ `float` coercion at assignment-like sites** -- High-level `int` and `float` variables accept cross-type values implicitly at declaration (`x: int = 3.14`), reassignment (`x = 2 ** 3` where `x: int`), and compound assignment (`x **= 2`, `x /= 2`, `x += 3.14` where `x: int`). This also applies to record field compound assign (`r.n **= 2`) and collection-element compound assign (`xs[0] **= 2`, `m["k"] **= 2`). Narrowing (`float` → `int`) truncates toward zero, matching `x as int`. Low-level types (`i64`, `f32`, etc.) do not participate and still require explicit `as` casts. Function return values also participate in this coercion (both widening and narrowing) when the declared return type is high-level `int` or `float`; low-level return types still require explicit `as` casts. Function arguments and `if`-expression branch unification still reject narrowing; use an explicit `as` cast at those boundaries.
- **Variable types are fixed at declaration** -- A variable declared as `int` cannot later be reassigned to hold a `float` reference (the binding's type doesn't change); float values assigned to an `int` are coerced per the rule above.
- **Arithmetic operations require `int` or `float`** -- Using a `bool` operand with arithmetic operators (`+`, `-`, `*`, `/`, `//`, `%`, `**`, unary `-`) is a compile error. Use `bool as int` for explicit conversion.
- **Bitwise operations are for `int` only** -- Applying bitwise operators (`&`, `|`, `^`, `~`, `<<`, `>>`) to `float` or `bool` is a compile error. Use `bool as int` for explicit conversion.
- **Non-`bool` types can be used in conditions** -- `if` conditions accept `int` (0 = false, non-zero = true) and other types besides `bool`.
- **Numeric literal separators** -- Underscores can be used as visual separators in numeric literals: `100_000`, `0xFF_FF`, `0b1010_0101`, `3.14_159`. Underscores must appear between digits (no leading, trailing, or consecutive underscores).
- **Numeric literal suffixes** -- Low-level types can be specified via literal suffixes: `42i32`, `255u8`, `3.14f32`, `.5f32`, `0xFFu8`, `0b1010u8`. An integer literal with a float suffix (`42f32`) produces a float value. A float literal with an integer suffix (`3.14i32`) is a compile error. Out-of-range values (e.g., `256u8`, `129i8`) are also compile errors.
- **Low-level numeric types (`i8`, `i16`, `i32`, `i64`, `u8`, `u16`, `u32`, `u64`, `f32`) have no implicit conversions** -- Mixing low-level types with each other or with high-level types (`int`, `float`) causes a compile error. Use explicit `as` casts. The `/` operator on low-level integers performs integer division (like Rust), not float division. Signed types use `SDiv`/`SRem`, unsigned types use `UDiv`/`URem`.
- **Signed vs unsigned** -- Signed types (`i8`, `i16`, `i32`, `i64`) use signed comparison (`ICMP_SLT` etc.) and arithmetic right shift (`AShr`). Unsigned types (`u8`, `u16`, `u32`, `u64`) use unsigned comparison (`ICMP_ULT` etc.) and logical right shift (`LShr`). The `>>>` operator always performs logical shift regardless of signedness.
- **`int` arithmetic overflow is a runtime error** -- Arithmetic (`+`, `-`, `*`, unary `-`) on the high-level `int` type raises a runtime error on overflow, similar to Swift's default behavior. This prevents silent data corruption from two's complement wrapping. Constant expressions that overflow are caught at compile time.
- **Low-level integer overflow wraps around** -- Arithmetic on low-level integer types uses Ry-defined two's complement wrapping on overflow for signed types and modular arithmetic for unsigned types. For example, `2147483647i32 + 1i32` wraps to `-2147483648`. For explicit overflow control, use `checked_add/sub/mul` (returns `Result<T, Error>`), `saturating_add/sub/mul` (clamps to type bounds), or `wrapping_add/sub/mul` (self-documenting wrapping). See [Function Reference](functions.md#checkedsaturating-arithmetic).
