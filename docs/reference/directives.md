[English](directives.md) | [日本語](../ja/reference/directives.md) | [繁體中文](../zh/reference/directives.md)

# Directives

Directives are compile-time metadata annotations that can be attached to declarations. They use the `@name` syntax, similar to Java annotations.

## Syntax

```
@name
@name(key=value, ...)
```

Directives are placed before the target declaration. Multiple directives can be stacked.

## Supported Targets

Directives can be applied to the following declarations:

- `fn` - Function definitions
- `record` - Struct definitions
- `let` / `var` - Variable declarations
- Fields within a `record` definition

## Built-in Directives

### `@deprecated`

Marks a declaration as deprecated. When a deprecated entity is used (called, referenced, or accessed), a compile-time warning is emitted.

**On functions:**

```
@deprecated
fn old_function() -> int:
    return 42

print(old_function())   # warning: 'old_function' is deprecated
```

**On types:**

```
@deprecated
record OldPoint:
    x: int
    y: int

let p = OldPoint(1, 2)  # warning: 'OldPoint' is deprecated
```

**On variables:**

```
@deprecated
let old_value = 99

print(old_value)         # warning: 'old_value' is deprecated
```

**On fields:**

```
record Config:
    @deprecated
    old_setting: int
    new_setting: int

let c = Config(1, 2)
print(c.old_setting)     # warning: 'Config.old_setting' is deprecated
print(c.new_setting)     # no warning
```

### `@native`

Declares a function whose implementation is provided by the runtime (built-in). The function must not have a body.

**Basic syntax:**

```
@native
fn contains(s: str, sub: str) -> bool

print(contains("hello world", "world"))  # true
```

**Operator overloads:**

```
@native
fn operator+(a: str, b: str) -> str

print("hello" + " world")  # hello world
```

**UFCS-compatible:**

```
@native
fn to_upper(s: str) -> str

print("hello".to_upper())  # HELLO
```

**Argument count validation:**

When a `@native` declaration includes a type signature, the compiler validates the number of arguments at call sites. Overloaded functions (e.g., `range` with 1, 2, or 3 arguments) are supported — any matching overload passes validation.

```
@native
fn range(n: int) -> List<int>
@native
fn range(start: int, end: int) -> List<int>

print(len(range(5)))       # OK: matches 1-arg overload
print(len(range(1, 10)))   # OK: matches 2-arg overload
print(len(range()))        # Error: expects 1 or 2 argument(s), but got 0
```

**Standard library declarations (`core/`):**

The `core/` directory contains `@native` declarations for all built-in functions, organized by category:

| File | Contents |
|---|---|
| `core/builtins.ry` | `print`, `len`, `range`, `enumerate`, `zip`, `exit`, `args` |
| `core/str.ry` | `contains`, `starts_with`, `ends_with`, `find`, `substring`, `char_at`, `replace`, `to_upper`, `to_lower`, `trim`, `trim_start`, `trim_end`, `repeat`, `split`, `join` |
| `core/convert.ry` | `to_int`, `to_float`, `to_str` |
| `core/list.ry` | `append`, `pop`, `insert`, `remove_at`, `slice`, `distinct`, `flatten`, `sort`, `first`, `last`, `is_empty` |
| `core/map.ry` | `keys`, `values`, `items`, `has_key`, `get`, `merge` |
| `core/set.ry` | `add`, `remove`, `union`, `intersection`, `difference`, `symmetric_difference`, `is_subset`, `is_superset` |
| `core/higher_order.ry` | `filter`, `map`, `reduce`, `fold`, `any`, `all`, `sum`, `min`, `max` |

These files are automatically loaded as a prelude when the `core/` directory is found relative to the `ry` executable. The prelude enables argument count validation for built-in function calls.

**Constraints:**
- `@native` functions must not have a body (no `:` after the signature).
- Providing a body causes a parse error: `@native function must not have a body`.
- The declared function must correspond to an existing built-in; otherwise the call will fail at compile time.

**Future extensions:**
- `@native("libfoo.so")` — FFI binding to external shared libraries.

### Parameters (future extension)

Directives support an optional parameter syntax for future extensions:

```
@deprecated(reason="use new_api instead")
fn old_api() -> int:
    return 0
```

Currently, parameters are parsed but not used by the `@deprecated` directive.

## Notes

- Deprecated entities still function normally; only a warning is emitted.
- Warnings are emitted at the point of use, not at the definition.
- Defining a deprecated entity without using it produces no warnings.
- Unknown directive names cause a parse error.
- Directives on unsupported targets (e.g., `if`, `while`) cause a parse error.
