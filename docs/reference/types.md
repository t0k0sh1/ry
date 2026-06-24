# Type Reference

## Type List

| Type | Internal Representation | Literal Examples | Description |
|---|---|---|---|
| `int` | i64 | `42`, `-7`, `0xFF`, `0b1010`, `100_000` | 64-bit signed integer |
| `float` | f64 | `3.14`, `0.5`, `3.14_159`, `1e10`, `1.5e-3`, `2.5E+2` | 64-bit floating-point number (scientific notation supported) |
| `bool` | i1 | `true`, `false` | Boolean value |
| `str` | ptr | `"hello"`, `""`, `"a\nb"`, `"""multi\nline"""` | String (immutable byte sequence on the heap). Internally a pointer to the data portion of a `StringHeader` (`{strong_count, weak_count, byte_len, data[], '\0'}`). Supports embedded NUL bytes (#1022). Block string literals (`"""..."""`) are documented in [builtins-string.md](builtins-string.md). |
| `Unit` | void | (no return value) | Return type for functions with no return value. Must be specified explicitly with `-> Unit` |
| `Option<T>` | `{ i1, T }` | `Some(42)`, `None` | A type that may or may not contain a value |
| `(T1, T2, ...)` | LLVM StructType (literal) | `(1, 3.14)` | Tuple type |
| `List<T>` | ptr (heap) | `[1, 2, 3]` | Dynamic array |
| `Map<K, V>` | ptr (heap) | `{"a": 1}` | Hash map |
| `Set<T>` | ptr (heap) | `{1, 2, 3}` | Set with no duplicates |
| `fn(T1, T2) -> R` | ptr (function pointer) | `(x: int) => x * 2` | Function type |
| User-defined type | LLVM StructType (named) | `record Point: ...` | Record defined with the `record` keyword |
| `enum` | i64 / tagged union | `Color::Red`, `Shape::Circle(3.14)` | Enumeration defined with the `enum` keyword (supports associated data) |
| `Error` | `{ ptr, i64 }` | `Error("msg")`, `Error("msg", 404)` | Built-in error type |
| `Type` | `{ i64, ptr }` | `typeOf(42)` | Compile-time type identity returned by `typeOf`. See [Type](#type) |
| `any` | `{ i64, [8 x i8] }` | `x: any = 42` | Tagged union that can hold any primitive, collection, record, or enum value |
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
| `Task<T>` | ptr | (returned by async fns) | Asynchronous task handle (used with `await` and `blockOn`) |
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
fnVal: fn(int) -> int = (x: int) => x * 2
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
| `fn(T1, ...) -> R` | Function type |
| `Error` | Built-in error type (`message: str`, `code: int`) |
| `any` | Built-in type that can hold any primitive value (`int`, `float`, `bool`, `str`), `Unit`, collections (`List<T>`, `Map<K, V>`, `Set<T>`), records, or enum values (organic enums plus built-in `Option<T>` / `Result<T, E>`). Supports implicit conversion: concrete values are automatically wrapped when assigned to `any`, and `any` values are automatically unwrapped (with runtime type check) when assigned to a concrete type. `any(int)` → `float` auto-promotion is supported. See [any Type](#any-type) for details |
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
| `T[N]` | Fixed-length array of low-level type `T` with `N` elements. Stack-allocated, contiguous memory. Supports index read/write and `len()` |
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
type Callback = fn(int, int) -> int

add: Callback = fn(a: int, b: int) => a + b
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

fn describe(v: Simple) -> str:
  return str(v)
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
maxU64: u64 = 18446744073709551615      # 2^64 - 1
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
lightSpd  = 2.998E8
big       = 1e10f32
```

Overflowing exponents produce `+Inf`/`-Inf` (not a compile error). Note that the runtime `float()` converter is stricter: it returns `Err(Error)` on overflow rather than producing `+Inf`.

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
# x = dynamicValue()        # runtime check: exits if out of range
```

### In Function Parameters

```ry
fn setMonth(m: 1..12) -> int:
    return m

setMonth(6)                # OK
# setMonth(13)             # compile error (constant argument)
```

---

## `none` Keyword and Option Type Shorthand

The `none` keyword represents the absence of a value for Option types, equivalent to `None`.

The `T?` syntax is a shorthand for `Option<T>`.

```ry
x: int? = 42       # equivalent to Option<int>
y: int? = none      # equivalent to None

fn find(xs: List<int>, val: int) -> int?:
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

### Restrictions

`weak T` cannot appear as a function or lambda return type. The strong owner that keeps the referent alive is typically a local variable inside the function body; it would be released when the function returns, making the returned `weak T` immediately dangling. Bind the `weak` reference at the call site instead, where the strong owner is in scope:

```ry
# Compile error: return type cannot be 'weak T'
fn make() -> weak str:
  return weak "hi"

# Bind weak at the call site where the strong owner stays alive
s = "hi"
w: weak str = weak s
```

The same restriction applies to lambdas (`(x: str) -> weak str => x` is rejected).

The check is a parser-level syntax check on the outermost return-type node, so the following deferred shapes are **not diagnosed at parse time** today:

- Type aliases that hide a `weak T` (`type W = weak str; fn make() -> W: ...`).
- Wrapped return types (`fn make() -> List<weak str>:`, `fn make() -> Option<weak str>:`, etc.).
- Function-type annotations (`f: fn() -> weak str = ...`).

These shapes compile but reproduce the same latent behavior the parser-level check exists to prevent — the returned weak reference loses its `weak T` static type on the caller side and behaves as a plain `T` (e.g. `str`-fallback), with no `case`-based auto-upgrade to `Option<T>`. The lifetime soundness is not enforced by the compiler; whether the referent is alive at access time depends on incidental ARC retains that may or may not exist.

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

Any `as` cast (including with generics) must be a built-in cast or have a matching user-defined `operator as`, otherwise it is a compile error. Use `int()` / `float()` for string-to-number conversions.

### Float → Integer Runtime Checks

Every float-to-integer conversion (`float`/`f32` → `int`/`i8`/`i16`/`i32`/`i64`/`u8`/`u16`/`u32`/`u64`, including the implicit coercion when assigning a `float` to an `int`-typed variable or using a compound operator such as `/=` on an `int`) is guarded at runtime. If the source value is `NaN`, `±inf`, or outside the target integer's representable range, the program prints `runtime error: cannot convert <value> to <type>` to standard error and exits with status `1`. The guards use half-open intervals (`[-2^(W-1), 2^(W-1))` for signed `W`-bit and `[0, 2^W)` for unsigned `W`-bit) so that exactly-representable boundaries such as `INT64_MIN` are accepted.

```ry
(1.0 / 0.0) as int       # runtime error: cannot convert inf to int
(0.0 / 0.0) as i32       # runtime error: cannot convert nan to i32
(-1.0) as u8             # runtime error: cannot convert -1 to u8
```

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
- Field names must be `camelCase`.
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
Payload fields with function types are not equatable; comparing two values whose matching variant carries an `fn(...)` payload is a compile-time error.

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

A generic enum can also be used as a function parameter, return type, or variable binding. The type argument must be supplied wherever the enum appears in the signature:

```ry
fn unwrapOrInt(opt: MyOption<int>, default: int) -> int:
    case opt:
        MyOption::MySome(v):
            return v
        MyOption::MyNone:
            return default
    return default

# Inside a generic function, the type parameter is substituted into nested generics:
fn unwrapOr<T>(opt: MyOption<T>, default: T) -> T:
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

Wrap the recursive field in an indirection type — `List<T>`, `Map<K, V>`, `Set<T>`, `Task<T>`, or `Channel<T>` — which is stored as a pointer and therefore has a fixed layout:

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
fn divide(a: int, b: int) -> Result<int, Error>:
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
fn save(path: str, data: str) -> Result<Unit, Error>:
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
fn makeOk(v: int) -> Result<int, Error>: return Ok(v)
makeOk(42) == makeOk(42)   # true
makeOk(1)  == makeOk(2)    # false
makeOk(1)  != Err(Error("e"))  # true
```

**Test matchers:**
- `expect(x).toBeOk()` — asserts the result is `Ok`
- `expect(x).toBeErr()` — asserts the result is `Err`

### Internal Representation

`Error` is represented as `{ ptr message, i64 code }`.
`Result<V, E>` is represented as `{ i1 isOk, V okValue, E errValue }`.

## Type

`Type` is the value returned by the built-in [`typeOf`](builtins.md#typeOf) function. It represents the compile-time identity of a type and allows reflective comparison at run time.

```ry
print(str(typeOf(42)))          # int
print(str(typeOf([1, 2, 3])))   # List

print(typeOf(42) == typeOf(100))  # true
print(typeOf(42) == typeOf(3.14)) # false
```

Key properties:

- Each distinct type definition (primitive, collection, record, enum, `Option`, `Result`, `fn` (function type), `Type` itself, etc.) receives a unique identity at compile time.
- `==` / `!=` on `Type` values compare identities, not display names. Two different records (or a record and an enum with the same name) are always distinguishable.
- `print` and `str` display the human-readable type name (for example, `"int"`, `"List"`, `"Point"`, `"i32"`).
- Low-level numeric types (`i8`, `i16`, …, `f32`) are distinguished from `int` / `float`.
- Collection generics collapse to their base name: `typeOf([1, 2])` returns `"List"`, not `"List<int>"`.
- `Type` is reflective: `typeOf(typeOf(x))` returns the `Type` value that represents `Type` itself.

### Internal Representation

`Type` is represented as `{ i64 id, ptr name }`. The `id` field is used for equality and the `name` field is used for display. Both fields are populated at compile time by `typeOf`.

## Union Type

You can declare a variable that may hold one of multiple types using `|`.

```ry
x: int | str = 42
x = "hello"     # Reassignment is allowed (any type in the union)
print(x)        # hello
```

### Usage in Function Parameters and Return Types

```ry
fn show(x: int | str) -> int:
    print(x)
    return 0

fn getVal(flag: bool) -> int | str:
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

Ry is statically typed; `any` is not a general-purpose dynamic variable or a substitute for inferred types or union types. `any` is a runtime-tagged, type-erased slot — `x = 1` infers `int` and remains statically typed for its lifetime, while `x: any = 1` is a deliberate erasure operation that stores the value alongside a runtime tag.

Intended use cases:

- JSON / JSON5 transport (`json.load[Map<str, any>]`, `json5.load`) and dynamic-shape map traversal (`Map<str, any>.getPath`).
- FFI and native return placeholders, where the concrete return type is dispatched by a custom emitter rather than declared statically.
- Plugin or dynamic-dispatch boundaries that produce heterogeneous values.
- Diagnostics and runtime type-introspection.

For ordinary application variables, use inferred types (`x = 1`) or explicit concrete types (`x: int = 1`); prefer union types (`int | str`) when the variant set is known.

To recover a concrete type from an `any` value safely, use [`asType[T]` / `isType[T]`](#explicit-any-conversion-astype--istype).

### Supported Types

`any` can hold the following types:

| Type | Tag | Description |
|------|-----|-------------|
| `int` | 0 | 64-bit signed integer |
| `float` | 1 | 64-bit floating-point number |
| `bool` | 2 | Boolean value |
| `str` | 3 | String |
| `Unit` | 4 | Unit value (for functions with no return value) |
| `List<T>` | 5 | List of any element type (element type is erased — see below) |
| `Map<K, V>` | 6 | Map (key/value types are erased) |
| `Set<T>` | 7 | Set (element type is erased) |
| `record` | 8 | User-defined record type (carries a per-type descriptor — see [Records](#records-in-any) below) |
| `enum` | 9 | Enum value — organic `enum` types plus built-in `Option<T>` / `Result<T, E>` (carries a per-type descriptor — see [Enums](#enums-in-any) below) |

`any` **cannot** hold resource types (`TcpListener`, `TcpStream`, etc.) or function pointers.

### Internal Representation

`any` is implemented as a tagged union:

```
{ i64 tag, [8 x i8] data }   // 16 bytes total
```

The `tag` field identifies the stored type, and the `data` field is interpreted accordingly:

- **Value tags (`int` / `float` / `bool` / `Unit`)** — `data` holds the value directly (up to 8 bytes); no heap allocation.
- **`str` tag** — `data` holds a pointer to the StringHeader-prefixed buffer.
- **Collection tags (`List` / `Map` / `Set`)** — `data` holds a pointer to the underlying collection header.
- **`record` tag** — `data` holds a pointer to a heap-allocated box laid out as `[ ArcHeader (16B) ][ descriptor ptr (8B) ][ record struct ]`. The descriptor is a per-record-type global carrying the destructor, equality function, and type name, so type identity survives even when the static type is erased to `any` across function boundaries.
- **`enum` tag** — `data` holds a pointer to a heap-allocated box laid out as `[ ArcHeader (16B) ][ descriptor ptr (8B) ][ enum payload ]`, identical in shape to the `record` box. The payload is the enum's native representation (`i64` discriminant for simple enums; ADT discriminated-union struct for ADT / `Option<T>` / `Result<T, E>`). The descriptor carries the destructor (releases ARC fields per active variant), equality function (variant-wise deep compare), and type name (e.g. `Option<int>`, `Result<List<int>, str>`, or the user-declared enum name). Even simple enums use this boxed form so the source-level enum identity is preserved across `any` round-trips.

**ARC retention semantics**: in all reference-holding cases (`str`, collections, `record` box, `enum` box) the wrapped value is **retained** on wrap (incrementing the underlying ARC count) and released when the enclosing `any` slot goes out of scope. Literal-backed strings are marked `ARC_IMMORTAL`, so retain/release on those become no-ops.

### Element-Type Metadata is Erased

When a collection is wrapped in `any`, the element type (e.g. `List<int>` vs `List<str>`) is **not preserved** at runtime. Only the outer collection kind survives. This has two consequences:

- **Implicit unwrap** trusts the static type annotation: `xs: List<int> = anyVal` is accepted whenever the dynamic tag matches `List`, regardless of the original element type. If the elements are not actually `int` at runtime, the per-element operations later in the program will misbehave; the unwrap site itself does not catch this.
- **Deep equality** (`anyA == anyB` where both hold a collection) compares length and the data buffer byte-by-byte at an 8-byte stride. This is exact for primitive lists (`List<int>` / `List<float>` / `List<bool>`); for `List<str>` and `List<nested-collection>` it reduces to header-pointer identity, which is conservative (may report `false` for two logically equal collections). For `Map` and `Set`, equality is **pointer identity only** — hashing requires the key/element type, which is erased.
- **String conversion** of a collection-holding `any` emits an opaque marker (`<List>`, `<Map>`, `<Set>`) rather than rendering elements. To get a typed printout, unwrap explicitly: `xs: List<int> = anyVal; print(xs)`.

Unwrap to a concrete type before doing per-element work. See [Explicit any conversion](#explicit-any-conversion-astype--istype) for safe recovery.

### Records in `any`

User-defined `record` types can be assigned to `any`. Unlike collections, each record type carries a per-type descriptor that preserves the dynamic type identity across function boundaries:

```ry
record Point:
  x: int
  y: int

p: any = Point(1, 2)        # wrap
q: Point = p                # exact-type unwrap restores fields
print(q.x)                  # 1

# Cross-function boundary
fn makePoint() -> any:
  return Point(7, 9)
a: any = makePoint()
r: Point = a                # the descriptor in the box ensures correct release
```

- **Wrap cost**: ~24 bytes overhead per record value (`ArcHeader` 16B + `descriptor ptr` 8B) plus the record struct itself, heap-allocated and reference-counted.
- **Equality (`==`)** compares the two boxes' descriptor pointers first; if they match, the descriptor-resident equality function dispatches to a field-wise deep comparison, identical to the typed `Point == Point` path. Two `any` holding records of different types are always unequal — `Dog == Animal` is `false` even when the `Dog` carries the same `name` / `legs` as the `Animal`, because identity is keyed on the dynamic descriptor.
- **`str` / f-string interpolation** emits a `<TypeName>` marker (e.g. `<Point>`) using the descriptor's type name — more informative than the opaque collection markers.
- **Subtype unwrap**: a child record stored in `any` can be unwrapped as any of its ancestor types. The descriptor carries a `parent_desc` chain that the runtime walks at unwrap time; if the expected type appears anywhere in the chain the unwrap succeeds and projects the ancestor's fields out of the box, otherwise the unwrap traps with a clear `any record type mismatch` error.

```ry
record Animal:
  name: str
  legs: int

record Dog < Animal:
  breed: str

record GuideDog < Dog:
  trainerId: int

g: any = GuideDog("Buddy", 4, "GoldenRetriever", 42)
a: Animal = g               # walks GuideDog -> Dog -> Animal; reads name / legs only
d: Dog = g                  # walks GuideDog -> Dog; reads name / legs / breed
print(a.name)               # "Buddy"

# Cross-function boundary still works — descriptor lives in the box
fn makeAnyDog() -> any:
  return Dog("Spot", 4, "Beagle")
spotted: Animal = makeAnyDog()
print(spotted.name)         # "Spot"

# Unrelated types still trap at runtime
p: any = Point(1, 2)
# nope: Animal = p          # runtime error: any record type mismatch
```

Subtype coercion on the typed path (`fn f(a: Animal): ...; f(dogValue)`) is unchanged — the runtime walk only applies to `any → record` unwrap sites.

### Enums in `any`

Enum values can be assigned to `any` — including organic `enum` declarations (with or without payloads) and the built-in `Option<T>` / `Result<T, E>` types. The box layout is the same as for records: a per-enum descriptor preserves the dynamic type identity (including the full generic parameterization, e.g. `Option<int>` is distinct from `Option<str>`) across function boundaries.

```ry
enum Color:
  Red
  Green
  Blue

enum Shape:
  Circle(float)
  Rect(int, int)
  Dot

c: any = Color::Red             # wrap a simple enum
back: Color = c                 # unwrap restores the original enum value
print(back == Color::Red)       # true

s: any = Shape::Rect(3, 4)      # wrap an ADT enum with payload
sBack: Shape = s
print(sBack == Shape::Rect(3, 4))  # true

# Option<T> and Result<T, E> round-trip preserves their generic parameters
o: any = Some(42)
backOpt: Option<int> = o        # exact-type unwrap restores the Option
res: Result<int, str> = Ok(7)
r: any = res
backRes: Result<int, str> = r   # unwrapping into Result<int, str>

# Cross-function boundary
fn makeAnyShape() -> any:
  return Shape::Circle(1.5)
sh: Shape = makeAnyShape()
```

- **Wrap cost**: ~24 bytes overhead per enum value (`ArcHeader` 16B + `descriptor ptr` 8B) plus the enum payload, heap-allocated and reference-counted. Simple enums (no payload) are also boxed for consistency, so the source-level enum identity survives the round trip.
- **Equality (`==`)** compares the two boxes' descriptor pointers first; only matching descriptors proceed to the descriptor-resident equality function, which switches on the discriminant and compares per-variant payloads. Two `any` holding enums of different types are always unequal — `Color::Red == Mode::On` is `false` even when their underlying discriminant value would coincide, because identity is keyed on the dynamic descriptor.
- **`str` / f-string interpolation** emits a `<TypeName>` marker (e.g. `<Color>`, `<Option<int>>`, `<Result<int, str>>`) using the descriptor's type name — parallel to the record markers.
- **Type-mismatch unwrap** traps with `runtime error: any enum type mismatch (expected <Expected>, got a different enum type)`. Enums do not participate in record-style subtype unwrap chains — there is no `parent_desc` walk, only descriptor identity.

### Wrapping and Unwrapping

Concrete values are automatically **wrapped** when assigned to `any`, and `any` values are automatically **unwrapped** when assigned to a concrete type.

```ry
# Wrapping: concrete → any (the type is erased at the assignment site)
age: any = 42          # int wrapped; static type is now erased
name: any = "Alice"    # str wrapped independently

# Unwrapping: any → concrete
fn getValue() -> any:
    return 42
n: int = getValue()  # any(int) is unwrapped to int

# int → float auto-promotion during unwrap
f: float = getValue()  # any(int) is unwrapped and promoted to float
```

If the runtime type does not match the target type (e.g., unwrapping `any(str)` into an `int` variable), a **runtime error** occurs.

#### Mixed-type literals in typed `any` collections

When the annotation is `Map<K, any>` / `List<any>` / `Set<any>`, each element in a literal is individually wrapped into `any` during the declaration or reassignment — elements with different concrete types can coexist in the same literal:

```ry
m: Map<str, any> = {"a": 1, "b": "two", "c": true}   # int / str / bool values
xs: List<any> = [1, "x", true]
s:  Set<any>  = {1, "x", true}

# Reassignment uses the same path
m = {"k": 42, "v": false}
```

The wrap is only triggered by the `any` element annotation on the destination. With a concrete element annotation, the strict same-type check still applies:

```ry
# m: Map<str, int> = {"a": 1, "b": "two"}   # error: map values must all have the same type
```

`Map<any, V>` (any-typed keys) is currently out of scope; mixed-key literals continue to be rejected by the strict same-type check.

### Reassignment

An `any` variable can be reassigned to a value of any supported type. This is a property of the type-erasure mechanism, not a recommended pattern for ordinary variables — prefer inferred types or concrete annotations instead.

```ry
x: any = 42
x = 3.14       # now holds float
x = "hello"    # now holds str
x = true       # now holds bool
```

### Arithmetic Operations

> **Deprecated (#2316)**: Direct arithmetic operators (`+`, `-`, `*`, `/`, `//`, `%`, `**`, unary `-`) on `any` values are deprecated and emit a compile-time warning on stderr. They continue to work during the deprecation window. Use [`asType[T]`](#explicit-any-conversion-astype--istype) to narrow before operating:
>
> ```ry
> x: any = 10
> y: any = 3
> # Deprecated: direct arithmetic on any
> # z: any = x + y
>
> # Preferred: narrow first
> case asType[int](x):
>     Ok(xi):
>         case asType[int](y):
>             Ok(yi):
>                 z: int = xi + yi
>                 print(z)
>             Err(e):
>                 print(e.message)
>     Err(e):
>         print(e.message)
> ```
>
> The warning is per-operator, deduplicated, and one-time per compilation. Strict-mode escalation to a hard error is planned for #2322.

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

`==` and `!=` on `any` are **retained** and do not emit a deprecation warning. The runtime returns `false` on type mismatch (different tags) rather than trapping, so dynamic equality is safe. For collections held in `any`, equality reduces to pointer identity — see [Element-Type Metadata is Erased](#element-type-metadata-is-erased) for the rationale.

> **Deprecated (#2316)**: Ordering comparisons (`<`, `<=`, `>`, `>=`) on `any` values are deprecated and emit a compile-time warning on stderr. They trap at runtime on type mismatch (e.g., `int < str`). Use [`asType[T]`](#explicit-any-conversion-astype--istype) to narrow before comparing:
>
> ```ry
> x: any = 3
> y: any = 4
> # Deprecated: direct ordering on any
> # print(x < y)
>
> # Preferred: narrow first
> case asType[int](x):
>     Ok(xi):
>         case asType[int](y):
>             Ok(yi):
>                 print(xi < yi)
>             Err(e): print(e.message)
>     Err(e): print(e.message)
> ```

| Operation | Behavior |
|-----------|----------|
| `==`, `!=` | Retained. Same-type compare; int/float mixing allowed; type-mismatch returns `false` |
| `<`, `<=`, `>`, `>=` | Deprecated (warned). Numeric (int/float mixing allowed) and string (lexicographic); type-mismatch traps at runtime |

```ry
x: any = 3
y: any = 3.0
print(x == y)    # true (int/float comparison)
```

### String Conversion

`print(anyVal)`, `str(anyVal)`, and f-string interpolation `f"{anyVal}"` on `any` values are **retained** and do not emit a deprecation warning (#2316).

`any` values support `print()` and f-string interpolation:

```ry
x: any = 42
print(x)              # 42
print(f"value: {x}")  # value: 42
```

Conversion rules: `int` → decimal string, `float` → `%g` format, `bool` → `"true"`/`"false"`, `str` → as-is, `Unit` → `"Unit"`, `List` / `Map` / `Set` → opaque marker (`<List>` / `<Map>` / `<Set>`) — see "Element-Type Metadata is Erased" above for the rationale.

### Passing any to Typed Functions

An `any` value can be passed to a function with concrete parameter types. The value is automatically unwrapped with a runtime type check:

```ry
fn addOne(x: int) -> int:
    return x + 1

v: any = 42
result = addOne(v)   # any(int) is unwrapped to int; result is 43
```

### Explicit any conversion (`asType` / `isType`)

Implicit unwrap (`x: T = anyVal`) panics with `_Exit(1)` on tag mismatch — acceptable when the `any` value is produced in the same scope, fatal for externally-parsed data (`json5.load[Map<str, any>]` results, mixed `List<any>` cells, etc.). Use `asType[T]` and `isType[T]` to recover concrete types safely:

```ry
@public
@native
fn asType<T>(value: any) -> Result<T, Error>

@public
@native
fn isType<T>(value: any) -> bool
```

- **`asType[T](v)`** is a checked cast. On tag/descriptor match it returns `Ok(value)`; on mismatch it returns `Err(Error{message: "asType[T]: ..."})` instead of crashing. The supported `T` set matches `json.load[T]` / `json5.load[T]`: `int`, `float` (accepts both `Float` and `Int` source tags), `bool`, `str`, user-defined `record` (descriptor-aware: subtype unwrap succeeds, unrelated record types `Err`), `Option<T>`, `List<any>`, `Map<str, any>`, `Set<any>`, and typed `List<T>` / `Map<str, V>` reconstructed from a JSON-shape `List<any>` / `Map<str, any>` source. `Result<T, E>`, organic enums, `Set<T>` (T ≠ any), and `Map<NonStr, _>` return a "target not yet supported" `Err`. **Typed-collection source restriction**: `asType[List<int>](v)` (and similar typed targets) on a `v` that was wrapped from a native typed collection — e.g. `xs: List<int> = [...]; v: any = xs` — returns `Err` rather than reconstructing, because the per-element walk assumes `List<any>` 16-byte stride. Use the implicit assignment path (`xs: List<int> = v`) for native-source typed unwraps until the typed-collection direct path is implemented.
- **`isType[T](v)`** is a runtime tag test. Returns `true` for matching tag (records walk the descriptor chain, so `isType[Parent](anyHoldingChild)` is `true`). `isType[any]` is always `true`. Element types of collections are erased — `isType[List<int>]` and `isType[List<any>]` are equivalent. `Option<T>`, `Result<T, E>`, and organic enum targets are rejected at compile time because the runtime tag (`RyAnyTag::Enum`) cannot distinguish enum types without a descriptor compare (use case-based unwrap on the underlying enum value instead).

#### Recovering typed values from JSON-shaped data

```ry
from json5 import load

case load[Map<str, any>](text):
  Ok(m):
    case asType[int](m["age"]):
      Ok(age): print(f"age = {age}")
      Err(e): print(f"bad age field: {e.message}")
    case asType[str](m["name"]):
      Ok(name): print(f"name = {name}")
      Err(e): print(f"bad name field: {e.message}")
  Err(e): print(f"parse error: {e.message}")
```

The error message includes the caller label and target type, so failures are self-documenting:

```text
asType[int]: expected int
asType[float]: expected float or int
asType[AnyRecPoint]: expected record AnyRecPoint, got a different record type
```

#### Records and Option in any: native vs JSON-shaped sources

`asType[T]` handles two source shapes:

- **Native-tagged source** (`v: any = SomeRecord(...)` or `v: any = Some(42)`): the runtime tag is `Record` / `Enum` and the descriptor identifies the concrete type. `asType` verifies the descriptor (subtype walk for records, equality for enums) and returns `Ok` directly, or `Err` if the descriptor does not match.
- **JSON-shaped source** (`v: any = parsed_map_or_null` from `json5.load[Map<str, any>]`): the runtime tag is `Map` (for records) or `Unit` (for `Option<T> = None`); `asType` walks the fields by name (records) or maps `Unit → Ok(None)` (Option) and reconstructs the typed value.

`isType[T]` only inspects the runtime tag (no JSON-shape interpretation): `isType[Record]` on an `any`-holding-Map returns `false`. Use `asType[T]` when you want either shape accepted, `isType[T]` when you want strict tag identity.

```ry
v: any = AnyRecPoint(1, 2)
case asType[AnyRecPoint](v):
  Ok(p): expect(p.x).toEq(1)        # native record-tagged → Ok
  Err(_): fail("unreachable")

case asType[AnyRecPlain](v):
  Ok(_): fail("unreachable")
  Err(e): expect(e.message).toEq(  # different record type → Err
    "asType[AnyRecPlain]: expected record AnyRecPlain, got a different record type")
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
- **`int` arithmetic overflow is a runtime error** -- Arithmetic (`+`, `-`, `*`, unary `-`, `//`, `%`) on the high-level `int` type raises a runtime error on overflow, similar to Swift's default behavior. This prevents silent data corruption from two's complement wrapping. `math.abs` traps when called on the minimum representable `int` for the same reason. Constant expressions that overflow are caught at compile time.
- **Low-level integer overflow wraps around** -- Arithmetic on low-level integer types uses Ry-defined two's complement wrapping on overflow for signed types and modular arithmetic for unsigned types. For example, `2147483647i32 + 1i32` wraps to `-2147483648`. For explicit overflow control, use `checkedAdd/sub/mul` (returns `Result<T, Error>`), `saturatingAdd/sub/mul` (clamps to type bounds), or `wrappingAdd/sub/mul` (self-documenting wrapping). See [Function Reference](functions.md#checkedsaturating-arithmetic).
