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

- `function` - Function definitions
- `record` - Struct definitions
- Variable declarations (with or without `@const`)
- Fields within a `record` definition
- `for` - Counted loops only for `@parallel`
- `it` - Test case definitions (for `@each` and `@property` only)

## Built-in Directives

### `@deprecated`

Marks a declaration as deprecated. When a deprecated entity is used (called, referenced, or accessed), a compile-time warning is emitted.

**On functions:**

```
@deprecated
function old_function() -> int:
    return 42

print(old_function())   # warning: 'old_function' is deprecated
```

**On types:**

```
@deprecated
record OldPoint:
    x: int
    y: int

@const
p = OldPoint(1, 2)  # warning: 'OldPoint' is deprecated
```

**On variables:**

```
@deprecated
@const
old_value = 99

print(old_value)         # warning: 'old_value' is deprecated
```

**On fields:**

```
record Config:
    @deprecated
    old_setting: int
    new_setting: int

@const
c = Config(1, 2)
print(c.old_setting)     # warning: 'Config.old_setting' is deprecated
print(c.new_setting)     # no warning
```

### `@const`

Marks a variable as immutable. Variables declared with `@const` cannot be reassigned after initialization. Without `@const`, variables are mutable by default.

```
@const
x = 42
# x = 10   # Error: cannot reassign @const variable
```

**With type annotation:**

```
@const
name: str = "hello"
```

**Tuple destructuring:**

```
@const
a, b = (1, 2)
```

### `@native`

Declares a function whose implementation is provided by the runtime (built-in). The function must not have a body.

**Basic syntax:**

```
@native
function contains(string: str, substring: str) -> bool

print(contains("hello world", "world"))  # true
```

**Operator overloads:**

```
@native
function operator+(a: str, b: str) -> str

print("hello" + " world")  # hello world
```

**UFCS-compatible:**

```
@native
function to_upper(string: str) -> str

print("hello".to_upper())  # HELLO
```

**Argument count validation:**

When a `@native` declaration includes a type signature, the compiler validates the number of arguments at call sites. Overloaded functions (e.g., `range` with 1, 2, or 3 arguments) are supported — any matching overload passes validation.

```
@native
function range(n: int) -> List<int>
@native
function range(start: int, end: int) -> List<int>

print(length(range(5)))       # OK: matches 1-arg overload
print(length(range(1, 10)))   # OK: matches 2-arg overload
print(length(range()))        # Error: expects 1 or 2 argument(s), but got 0
```

**Standard library declarations (`core/`):**

The `core/` directory contains `@native` declarations for all built-in functions, organized by category:

| File | Contents |
|---|---|
| `core/builtins.ry` | `print`, `length`, `range`, `enumerate`, `zip`, `exit`, `args`, `available_parallelism`, `sleep` |
| `core/str.ry` | `contains`, `starts_with`, `ends_with`, `find`, `substring`, `char_at`, `replace`, `to_upper`, `to_lower`, `trim`, `trim_start`, `trim_end`, `repeat`, `reverse`, `split`, `join` |
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

### `@parallel`

Marks a counted `for` loop for parallel execution.

```
@parallel
for i in range(8):
    work(i)
```

**Supported target:**

- `for` statements only

**Constraints:**

- Only a single `@parallel` directive is allowed on a `for` statement.
- The iterable must be `range(...)` or an integer `..` range.
- Destructuring iteration is not supported.
- Assigning to outer mutable variables is rejected.
- `break`, `continue`, indexed assignment, and field assignment inside the loop body are rejected in v1.

### `@each`

Enables parameterized testing by running an `it` block multiple times with different parameters.

**Syntax:**

```
@each([(arg1, arg2, ...), ...])
it("description with {0} and {1}", function(param1: type, param2: type):
    # test body
)
```

**Supported target:** `it` calls only

**Constraints:**
- The argument must be a list of tuples
- Tuple arity must match the lambda parameter count
- Placeholders `{0}`, `{1}`, ... in the description string are replaced with stringified values

### `@property`

Enables property-based testing by generating random inputs for an `it` block.

**Syntax:**

```
@property(count=100)
it("property name", function(a: int, b: int):
    # test body with random values
)
```

**Supported target:** `it` calls only

**Parameters:**

| Parameter | Type | Default | Description |
|-----------|------|---------|-------------|
| `count` | int | 100 | Number of random trials |

**Supported parameter types:**

| Type | Range |
|------|-------|
| `int` | -1000 to 1000 |
| `float` | -1000.0 to 1000.0 |
| `bool` | true or false |
| `str` | Random ASCII, 0-20 characters |

On failure, the counterexample (parameter values that caused the failure) is printed.

### `@inline`

Provides inlining hints to the LLVM optimizer. By default, marks the function for aggressive inlining.

**Basic usage (always inline):**

```
@inline
function add(a: int, b: int) -> int:
    return a + b
```

**With mode parameter:**

```
@inline(mode="always")
function hot_path(x: int) -> int:
    return x * 2 + 1

@inline(mode="hint")
function medium_path(x: int) -> int:
    return x + 1

@inline(mode="never")
function cold_error_handler(msg: str):
    print("ERROR: " + msg)
```

**Modes:**

| Mode | LLVM Attribute | Description |
|------|---------------|-------------|
| `always` (default) | `AlwaysInline` | Always inline this function |
| `hint` | `InlineHint` | Suggest inlining to the optimizer |
| `never` | `NoInline` | Never inline this function |

**Constraints:**
- `@inline` cannot be used with `@native` (native functions have no body to inline).
- An unknown mode value causes a compile error.

### Parameters (future extension)

Directives support an optional parameter syntax for future extensions:

```
@deprecated(reason="use new_api instead")
function old_api() -> int:
    return 0
```

Currently, parameters are parsed but not used by the `@deprecated` directive.

## Notes

- Deprecated entities still function normally; only a warning is emitted.
- Warnings are emitted at the point of use, not at the definition.
- Defining a deprecated entity without using it produces no warnings.
- Unknown directive names cause a parse error.
- Directives on unsupported targets (e.g., `if`, `while`) cause a parse error. `@parallel` is the only directive supported on `for`.
