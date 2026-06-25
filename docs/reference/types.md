# Type Reference

This page is the compact reference for Ry type syntax and type rules. Internal implementation details belong in architecture docs unless they affect user-visible behavior.

## Type List

| Type | Examples | Notes |
|---|---|---|
| `int` | `42`, `0xFF`, `100_000` | 64-bit signed integer |
| `float` | `3.14`, `1e10`, `1.5e-3` | 64-bit floating point |
| `bool` | `true`, `false` | Boolean |
| `str` | `"hello"`, `"""multi\nline"""` | immutable string; embedded NUL bytes are supported |
| `Unit` | function with no value result | must be written explicitly as `-> Unit` when used |
| `Option<T>` | `Some(42)`, `None`, `int?` | optional value |
| `Result<T, E>` | `Ok(42)`, `Err(Error("fail"))` | success or error |
| `Error` | `Error("msg")`, `Error("msg", 404)` | built-in error payload |
| `(T1, T2, ...)` | `(1, "x")`, `(42,)` | tuple |
| `List<T>` | `[1, 2, 3]` | dynamic array |
| `Map<K, V>` | `{"a": 1}` | hash map |
| `Set<T>` | `{1, 2, 3}` | unique elements |
| `fn(T1, T2) -> R` | `(x: int) => x * 2` | function type |
| user record | `record Point: ...` | named user-defined type |
| enum / ADT | `Color::Red`, `Shape::Circle(3.14)` | simple or associated-data enum |
| `Type` | `typeOf(42)` | runtime type identity value |
| `any` | `x: any = 42` | tagged dynamic value |
| `T1 | T2` | `int | str` | union type |
| literal type | `0 | 1`, `"N" | "S"` | value-constrained literal union |
| range type | `1..12`, `-10..10` | inclusive integer range constraint |
| low-level ints | `i8`, `i16`, `i32`, `i64`, `u8`, `u16`, `u32`, `u64` | no implicit conversion except documented widening through normal APIs |
| `f32` | `1.0f32` | low-level float |
| `weak T` | `weak s` | weak reference to an ARC-managed value |
| `Regex` | `/[a-z]+/` | regex literal type |
| `Task<T>` | returned by `async fn` | used with `await` / `blockOn` |
| `Iterator<T>` | `iter(xs)` | lazy element stream |
| `T[N]` | `buf: i32[8]` | fixed-length low-level array |

## Annotation Syntax

```ry
x: int = 42
xs: List<int> = [1, 2, 3]
m: Map<str, int> = {"a": 1}
f: fn(int) -> int = (x: int) => x * 2
u: int | str = 42
a: any = 42
```

Annotations may be omitted when the compiler can infer the type. Empty collection literals usually need annotation.

## Type Aliases

`type` creates a new name for an existing type. Alias names use PascalCase.

```ry
type Meters = float
type StringList = List<str>
type Callback = fn(int, int) -> int
type Month = 1..12
type Direction = "N" | "S" | "E" | "W"
```

Aliases are interchangeable with their target type. Union aliases flatten nested aliases and deduplicate duplicate members.

## Literals And Constraints

Integer literals support decimal, hex, binary, underscores, signs, and low-level suffixes such as `i32` or `u64`. Float literals support decimal and scientific notation, plus `f32`.

Literal and range types constrain values:

```ry
type Flag = 0 | 1
type Direction = "N" | "S" | "E" | "W"
type Month = 1..12

@const ok: Month = 6
# @const bad: Month = 13   # compile error
```

Constant values are checked at compile time. Dynamic values are checked at runtime when assigned to a constrained type or passed to a constrained parameter.

## Option Shorthand

`T?` is shorthand for `Option<T>`.

```ry
fn findName(id: int) -> str?:
    if id == 1:
        return Some("Ada")
    return None
```

`none` is a contextual identifier used by Option-related syntax; use the actual accepted spelling in code examples from the compiler/tests.

## Weak References

`weak T` creates a non-owning reference to an ARC-managed value such as `str`, `List<T>`, `Map<K, V>`, or `Set<T>`.

```ry
s = "hello"
w: weak str = weak s
strong: str? = w.upgrade()
```

Rules:

- `weak T` does not keep the target alive.
- Upgrade returns `Option<T>`.
- Reassignment is allowed between compatible weak values.
- Weak references cannot be returned as `weak T`.
- Bind weak references at a call site where the strong owner stays alive.

### Restrictions

`weak T` cannot be used as a function or lambda return type. Returning a weak reference would leave the caller with a reference to storage whose strong owner may already be gone.

## String Interpolation

F-strings embed expressions in string literals.

```ry
name = "Ada"
print(f"hello {name}")
```

Supported interpolated values use the same string-conversion behavior as `str(...)`. Use normal string escape sequences inside string literals.

## Casts

Use `as` for explicit casts.

```ry
x = 42
y = x as float
```

Supported casts include numeric casts, low-level integer/float casts, and casts implemented by overloads. Float-to-integer casts perform runtime checks for NaN, infinity, and range.

## Enums And ADTs

Enums can be simple or carry associated data.

```ry
enum Color:
    Red
    Green
    Blue

enum Shape:
    Circle(radius: float)
    Rect(width: float, height: float)
```

Associated-data variants can use positional or named fields. Pattern matching binds fields:

```ry
case shape:
    Shape::Circle(r):
        print(r)
    Shape::Rect(width=w, height=h):
        print(w * h)
```

Enums support equality when their payloads support equality. Recursive generic enums have implementation limits; prefer explicit non-recursive shapes unless tests cover the intended recursion.

## Error And Result

`Error` carries a message and optional code. `Result<T, E>` represents `Ok(T)` or `Err(E)`.

```ry
fn parseCount(s: str) -> Result<int, Error>:
    return int(s)

res = parseCount("42")
case res:
    Ok(v):
        print(v)
    Err(e):
        print(e)
```

Use `?` for supported error-propagation forms; see [Operators](operators.md#error-propagation-operator-).

### Result Type

`Result<T, E>` is the standard error-as-value type. `Ok(value)` carries success and `Err(error)` carries failure.

## Type

`Type` is the value returned by `typeOf(...)`. Distinct primitive, collection, record, enum, Option, Result, and function types have distinct identities and can be compared with `==`.

## Union Types

`T1 | T2` holds one of several member types.

```ry
fn show(v: int | str) -> str:
    return str(v)
```

Rules:

- duplicate members are deduplicated
- aliases flatten into their target members
- equality requires the active member type and value to match
- operations must be valid for the active type or require narrowing/conversion

## any Type

`any` can hold primitive values, strings, collections, records, enums, `Option`, `Result`, and `Unit`.

```ry
v: any = 42
i: int = v       # runtime type check
```

Rules:

- concrete values are wrapped automatically when assigned to `any`
- assigning `any` to a concrete type unwraps with a runtime type check
- `int` stored in `any` may unwrap to `float`
- direct arithmetic and ordering on `any` are rejected by strict-any rules
- pass `any` to typed functions only after explicit recovery to the target type
- collection element-type metadata is not preserved for every dynamic boundary; recover explicitly when crossing JSON-shaped data

Use `isType[T](v)` and `asType[T](v)` for explicit dynamic recovery where available.

Untyped function parameters defaulting to implicit `any` are deprecated. Prefer a concrete annotation or explicit `: any`.

### Arithmetic and Ordering Operations

Direct arithmetic and ordering on `any` are rejected. Recover a concrete type first, then apply the operation:

```ry
v: any = 1
x: int = v
print(x + 1)
```

### Untyped Parameters (Deprecated)

Omitting a parameter type creates an implicit `any` parameter and emits a warning. Write a concrete annotation or `: any` explicitly. Strict-any rules reject arithmetic, ordering, and implicit unwrap operations on implicit-any parameters.

## Type Rules

Common conversions:

| From | To | Rule |
|---|---|---|
| low-level unsigned/small int | `int` | safe widening where accepted |
| `int` | `float` | safe widening |
| concrete `T` | `any` | automatic wrap |
| `any` | concrete `T` | runtime checked unwrap |
| `T` | union containing `T` | automatic wrap into union |

Low-level integer and float types generally do not implicitly convert to each other. Use explicit casts.

## Type Safety Constraints

- Empty collections require type context.
- Map keys must be compatible with hashing/equality.
- `any` is not a license for unchecked operations; strict-any rejects direct arithmetic, ordering, and implicit unwrapping in unsafe positions.
- Literal/range constraints are enforced at compile time for constants and at runtime for dynamic values.
- Function parameter type omission is deprecated.

## Related

- [Collections](collections.md)
- [Operators](operators.md)
- [Functions](functions.md)
- [Records and Enums](records.md)
- [Strict-any](strict-any.md)
