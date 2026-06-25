# Directives

Directives are compile-time instructions attached to declarations or selected statements with `@name` syntax.

```ry
@name
@name(key=value, ...)
```

Multiple directives may be stacked. A declaration may carry each directive at most once; duplicates are rejected.

## Targets

Supported targets:

| Target | Notes |
|---|---|
| `fn` / `async fn` | includes test functions with `@it` / `@describe` |
| `record`, record field | `@doc`, `@deprecated`, and visibility-related metadata |
| `enum` | `@public` and `@doc`; enum variants do not carry `@doc` |
| `type` | type aliases |
| variable declaration | with or without `@const` |
| `for` | currently used by `@parallel`; user-defined directives may declare `target=["for"]` |
| `@directive` declaration | user-defined directive declarations may themselves be public or documented |

## Built-ins

| Directive | Target | Defined by | Purpose |
|---|---|---|---|
| `@deprecated` | functions, types, variables, fields | `share/std/core/directive.ry` | emits a warning when the entity is used |
| `@const` | variables | `share/std/core/directive.ry` | makes a variable immutable after initialization |
| `@public` | functions, records, enums, type aliases, variables, directive declarations | compiler built-in | exposes a definition across package boundaries |
| `@doc("...")` | functions, records, fields, enums, type aliases, directive declarations | compiler built-in | stores Markdown documentation metadata |
| `@native` / `@native("lib")` | bodyless functions | compiler built-in | declares runtime-implemented functions |
| `@parallel` | counted `for` loops | `share/std/core/directive.ry` | runs eligible loops in parallel |
| `@inline` | functions | `share/std/core/directive.ry` | requests inlining |

Testing directives are documented in [Testing](testing.md): `@it`, `@describe`, `@each`, `@property`, `@skip`, `@only`, `@todo`, `@timeout`, `@beforeEach`, `@afterEach`, `@beforeAll`, and `@afterAll`.

## Core Directives

### `@deprecated`

Marks a declaration as deprecated. Uses emit compile-time warnings.

```ry
@deprecated
fn oldApi() -> int:
    return 1

print(oldApi())   # warning
```

### `@const`

Marks a variable as immutable after initialization.

```ry
@const
x = 42

@const (a, b) = (1, 2)
(_, c) = (3, 4)
```

Top-level `@const` values are visible to later top-level functions in the same file. Mutating a top-level const record through a field is also rejected.

### `@public`

Makes a definition visible across package boundaries. Without `@public`, definitions are package-internal.

```ry
@public
fn add(a: int, b: int) -> int:
    return a + b
```

The leading `_` in an identifier has no visibility meaning. See [Modules](modules.md#visibility), [Visibility Guide](../guide/visibility.md), and [Glossary](glossary.md#visibility-scopes).

### `@doc`

Stores one positional string argument as Markdown documentation metadata.

```ry
@doc("""
Returns a TCP stream on success.

## Parameters

- `host`: host name or IP address
- `port`: TCP port
""")
fn tcpConnect(host: str, port: int) -> TcpStream?
```

Prefer Markdown sections such as `## Parameters`, `## Returns`, and `## Examples` over Javadoc-style tags. Empty strings are accepted. Duplicate `@doc` directives are rejected.

### `@native`

Declares a bodyless function implemented by runtime code.

```ry
@native
fn contains(s: str, needle: str) -> bool

@native("base64")
fn encode(s: str) -> str
```

Rules:

- `@native` functions must not have a body.
- Bare `@native` resolves through built-in or process-linked symbols.
- `@native("lib")` loads `libry_<lib>.dylib` on macOS or `libry_<lib>.so` on Linux.
- Overload resolution follows ordinary call resolution, including safe widening such as `int -> float`.
- First-class native function values are allowed only for names with exactly one overload. Use a lambda to select one overload explicitly.

Stdlib declarations under `share/std/` are the canonical signatures for validation, but dispatcher code and tests remain the source of truth for custom-emitter return shapes.

### `@parallel`

Marks an eligible counted `for` loop for parallel execution.

```ry
@parallel
for i in range(8):
    work(i)
```

Constraints:

- iterable must be `range(...)` or an integer `..` range
- destructuring iteration is not supported
- assigning to outer mutable variables is rejected
- `break`, `continue`, indexed assignment, and field assignment inside the loop body are rejected
- nested function definitions inside the loop body are rejected

### `@inline`

Requests inlining for a function. The compiler may ignore the request when the function cannot be safely inlined.

## User-Defined Directives

Directives can be declared with `@directive`.

```ry
@directive(target=["fn"])
@public
fn route(path: str)

@route("/health")
fn health() -> str:
    return "ok"
```

Rules:

- A directive declaration has no body.
- Built-in directive names cannot be redefined.
- Declaring the same directive name twice in one program is rejected.
- A target mismatch is a silent no-op: a directive whose declared target does not match the attached declaration is accepted but ignored.
- Public user-defined directives can be imported across package boundaries.

Most built-in directives are declared in `share/std/`; compiler bootstrap directives such as `@directive`, `@native`, `@public`, and `@doc` have no `.ry` declaration.

## Parameters

Directive arguments are parsed as positional and named values. Built-ins currently consume only the arguments documented above; unused parsed parameters should not be relied on for behavior.

## Related

- [Testing](testing.md)
- [Modules](modules.md)
- [Visibility Guide](../guide/visibility.md)
- [Project docs generator](project.md#ry-docs---static-html-api-documentation-generator)
