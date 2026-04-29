# Directives

Directives are compile-time instructions attached to declarations using the `@name` syntax. They are conceptually similar to Python's decorators, Java/Kotlin's annotations, and Rust's attributes, but Ry uses the term "directive" to avoid collision with "type annotation" used elsewhere in the language reference.

## Syntax

```
@name
@name(key=value, ...)
```

Directives are placed before the target declaration. Multiple directives can be stacked.

## Supported Targets

Directives can be applied to the following declarations:

- `fn` - Function definitions (including named test functions with the `@it` / `@describe` directive)
- `record` - Record definitions
- Variable declarations (with or without `@const`)
- Fields within a `record` definition
- `for` - Counted loops; among built-ins, only `@parallel` targets `for`. User-defined directives declaring `target=["for"]` may also be applied to `for` statements; directives with a different target are silently ignored per the target-mismatch rule.
- `it` / `describe` calls (legacy lambda form) - Test cases and test groups for `@each` and `@property`

## Built-in Directives

### `@deprecated`

Marks a declaration as deprecated. When a deprecated entity is used (called, referenced, or accessed), a compile-time warning is emitted.

**Defined as:** Declared in `share/std/core/directive.ry` (implicitly imported via `share/std/builtins.ry`).

**On functions:**

```
@deprecated
fn oldFunction() -> int:
    return 42

print(oldFunction())   # warning: 'oldFunction' is deprecated
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
oldValue = 99

print(oldValue)         # warning: 'oldValue' is deprecated
```

**On fields:**

```
record Config:
    @deprecated
    oldSetting: int
    newSetting: int

@const
c = Config(1, 2)
print(c.oldSetting)     # warning: 'Config.oldSetting' is deprecated
print(c.newSetting)     # no warning
```

### `@const`

Marks a variable as immutable. Variables declared with `@const` cannot be reassigned after initialization. Without `@const`, variables are mutable by default.

**Defined as:** Declared in `share/std/core/directive.ry` (implicitly imported via `share/std/builtins.ry`).

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

Both the bare form and the parenthesized form are supported. The parenthesized form also works without `@const` to declare the names as mutable.

```
@const
a, b = (1, 2)

@const (c, d) = (3, 4)        # same meaning, parenthesized pattern

(e, f) = (5, 6)               # declares mutable e, f
(_, g) = (7, 8)               # `_` skips a component
```

**Top-level `@const` and functions.** A top-level `@const` declaration is visible from any top-level function defined after it in the same source file, and the immutability is enforced for every reference — including field mutations through a top-level `@const` record. See the "Top-Level Variables and `@const` in Function Bodies" section in [functions.md](functions.md) for details.

### `@native`

Declares a function whose implementation is provided by the runtime. The function must not have a body.

**Defined as:** Compiler built-in (bootstrap; permanent C++ implementation).

An optional string argument specifies the shared library module name. When a `@native("libname")` function is called, the JIT dynamically loads the corresponding shared library (`libry_<libname>.dylib` on macOS, `libry_<libname>.so` on Linux) and resolves the runtime symbol from it:

```ry
@native              # built-in (statically linked into the process)
@native("base64")    # dynamically loaded from libry_base64.dylib/.so
```

**Basic syntax:**

```
@native
fn contains(string: str, substring: str) -> bool

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
fn toUpper(string: str) -> str

print("hello".toUpper())  # HELLO
```

**Argument count validation:**

When a `@native` declaration includes a type signature, the compiler validates the number of arguments at call sites. Overloaded functions (e.g., `range` with 1, 2, or 3 arguments) are supported — any matching overload passes validation.

```
@native
fn range(count: int) -> List<int>
@native
fn range(start: int, end: int) -> List<int>
@native
fn range(start: int, end: int, step: int) -> List<int>

print(len(range(5)))          # OK: matches 1-arg overload
print(len(range(1, 10)))      # OK: matches 2-arg overload
print(len(range(1, 10, 2)))   # OK: matches 3-arg overload
print(len(range()))           # Error: range() takes 1, 2, or 3 arguments
```

**Argument type resolution and implicit widening:**

`@native` overload resolution mirrors [user-defined overload resolution](functions.md#resolution-priority): exact-type matches are preferred, with safe implicit widening (`u8 → int`, `u8 → float`, `int → float`) as a fallback. For instance, `sqrt(4)` widens the `int` to `float` and calls `sqrt(float) -> float`, while `pow(2, 3)` still dispatches to the `(int, int) -> int` overload and returns `8`. Low-level integer types (`i8`, `i16`, …) require explicit `as` casts.

**Standard library declarations (`share/std/`):**

`@native` declarations for all built-in functions live under `share/std/` relative to the `ry` executable, organized by category. These files are automatically loaded as a prelude and enable argument count validation for built-in function calls. For the full function reference, see [Builtins](builtins.md), [Builtins — String](builtins-string.md), and [Collections](collections.md).

**Constraints:**
- `@native` functions must not have a body (no `:` after the signature).
- Providing a body causes a parse error: `@native fn must not have a body`.
- For bare `@native`, the declared function must correspond to an existing built-in; otherwise the call will fail at compile time. For `@native("libname")`, the function is compiled based on the declared signature and will fail at JIT link time if the symbol cannot be resolved from the loaded library.

**Library specification:**
- `@native("libname")` specifies that the native function lives in a shared library named `libry_<libname>.dylib` (macOS) or `libry_<libname>.so` (Linux). At JIT startup, the required shared libraries are loaded from the following search paths (in order):
  1. `exe/../lib/` — installed layout
  2. `exe/lib/` — development/build layout
  3. `$RY_HOME/lib/` — user-installed environment
- Both `@native` (static) and `@native("libname")` (dynamic) declarations register for argument-count validation and call resolution. The difference is only in how the runtime symbol is provided to the JIT.
- The runtime function name follows the convention `__ry_<libname>_<functionName>` (e.g., `@native("base64") fn encode(...)` → `__ry_base64_encode`). This works for both stdlib packages and user-defined native libraries.

### `@parallel`

Marks a counted `for` loop for parallel execution.

**Defined as:** Declared in `share/std/core/directive.ry` (implicitly imported via `share/std/builtins.ry`).

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
- Nested function definitions (`fn` statements) inside the loop body are not allowed.

### `@each`

Enables parameterized testing by running a test multiple times with different parameters.

**Defined as:** Declared in `share/std/testing/testing.ry`. Test files must add `from testing import each` (or include it in the existing `from testing import` line) at the top.

**Syntax (on a named function, preferred):**

```ry
@each([(arg1, arg2, ...), ...])
@it("should handle {0} and {1}")
fn testHandle(param1: type, param2: type):
    # test body
```

**Syntax (on a legacy `it` lambda):**

```ry
@each([(arg1, arg2, ...), ...])
it("should handle {0} and {1}", (param1: type, param2: type):
    # test body
)
```

> **Note**: The legacy lambda form is deprecated. Prefer the named function form with `@it`. See [Testing Reference](testing.md) for details.

The argument can be any expression that evaluates to a list of tuples, including a function call:

```ry
@each(makeInputs())
@it("should handle {0}")
fn testHandle(x: int):
    # test body
```

**Supported targets:** functions with the `@it` directive, or legacy `it` calls.

**Constraints:**
- The argument must evaluate to a list of tuples
- Tuple arity must match the function parameter count
- Placeholders `{0}`, `{1}`, ... in the description string are replaced with stringified values

### `@property`

Enables property-based testing by generating random inputs for a test.

**Defined as:** Declared in `share/std/testing/testing.ry`. Test files must add `from testing import property` (or include it in the existing `from testing import` line) at the top.

**Syntax (on a named function, preferred):**

```ry
@property(count=100)
@it("should verify property name")
fn testProperty(a: int, b: int):
    # test body with random values
```

**Syntax (on a legacy `it` lambda):**

```ry
@property(count=100)
it("should verify property name", (a: int, b: int):
    # test body with random values
)
```

> **Note**: The legacy lambda form is deprecated. Prefer the named function form with `@it`. See [Testing Reference](testing.md) for details.

**Supported targets:** functions with the `@it` directive, or legacy `it` calls.

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

### `@it`

Declares a test case by attaching the directive to a named function. The function body becomes the test body and is executed by `ry test`. See [Testing Reference](testing.md) for the full specification.

**Defined as:** Declared in `share/std/testing/testing.ry`. Test files must add `from testing import it, describe` at the top.

**Syntax:**

```ry
@it("description")
fn testName():
    # assertions
```

**Basic example:**

```ry
@it("should add 1 + 2 = 3")
fn testAdd():
    expect(1 + 2).toEq(3)
```

**Composed with `@each` or `@property`:**

```ry
@each([(1, 2, 3), (0, 0, 0), (-1, 1, 0)])
@it("should add {0} + {1} = {2}")
fn testAddEach(a: int, b: int, expected: int):
    expect(a + b).toEq(expected)

@property(count=100)
@it("should verify addition is commutative")
fn testCommutative(a: int, b: int):
    expect(a + b).toEq(b + a)
```

**Supported target:** `fn` declarations only.

**Constraints:**
- Only valid in `*.test.ry` files executed with `ry test`
- The function must not have a return type annotation
- When combined with `@each`, the function's parameter list must match the tuple arity
- When combined with `@property`, each parameter type must be one of the supported generator types (`int`, `float`, `bool`, `str`)

### `@describe`

Groups a set of related tests by attaching the directive to a named function. Inner `@it` functions declared in the body belong to the group, and variables declared directly in the body act as shared setup captured by every inner `@it`. Unlike the legacy lambda form, `@describe` groups **may be nested**; output is indented proportionally to nesting depth.

**Defined as:** Declared in `share/std/testing/testing.ry`. Test files must add `from testing import it, describe` at the top.

**Syntax:**

```ry
@describe("group name")
fn groupName():
    @it("nested test")
    fn testNested():
        # assertions
```

**Basic example:**

```ry
@describe("arithmetic")
fn arithmeticTests():
    @it("should subtract")
    fn testSub():
        expect(10 - 3).toEq(7)

    @it("should multiply")
    fn testMul():
        expect(4 * 5).toEq(20)
```

**Shared setup:**

Variables declared in the outer `@describe` body are automatically captured by every inner `@it` function.

```ry
@describe("shared setup")
fn sharedSetupTests():
    base = 100
    offset = 5

    @it("should use base")
    fn testBase():
        expect(base).toEq(100)

    @it("should use base and offset")
    fn testCombined():
        expect(base + offset).toEq(105)
```

**Nested groups:**

```ry
@describe("outer")
fn outer():
    @describe("inner")
    fn inner():
        @it("should pass deeply nested test")
        fn testDeep():
            expect(1 + 1).toEq(2)
```

**Supported target:** `fn` declarations only. The function must not have parameters or a return type annotation.

### `@inline`

Provides inlining hints to the LLVM optimizer. By default, marks the function for aggressive inlining.

**Defined as:** Declared in `share/std/core/directive.ry` (implicitly imported via `share/std/builtins.ry`).

**Basic usage (always inline):**

```
@inline
fn add(a: int, b: int) -> int:
    return a + b
```

**With mode parameter:**

```
@inline(mode="always")
fn hotPath(x: int) -> int:
    return x * 2 + 1

@inline(mode="hint")
fn mediumPath(x: int) -> int:
    return x + 1

@inline(mode="never")
fn coldErrorHandler(msg: str):
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
@deprecated(reason="use newApi instead")
fn oldApi() -> int:
    return 0
```

Currently, parameters are parsed but not used by the `@deprecated` directive.

## User-defined directives

Packages can declare their own compile-time directives with the `@directive(...)` declaration syntax. A user-defined directive becomes available in any source file that imports it; applying it without importing produces an `unknown directive` error.

### Defining a directive

A directive declaration specifies what kinds of nodes it can be applied to (`target`). The declaration is signature-only — the body and return type are both forbidden.

```ry
@directive(target=["function"])
fn logged(label: str)
```

**`target` parameter:**

| Value | Applies to |
|-------|-----------|
| `"function"` | `fn` declarations |
| `"record"` | `record` declarations |
| `"field"` | Fields inside a `record` body |
| `"statement"` | Top-level statements |
| `"for"` | `for` loops (used by `@parallel`) |

Multiple targets are allowed via the list form: `target=["function", "record"]`. The bare-string form `target="function"` is sugar for `target=["function"]`.

**Constraints:**
- `target` is required and named-only.
- `@directive` must be the sole directive on the `fn` — it cannot be stacked with other directives.
- The `fn` must not have a body (no `:`-introduced block) and no return type (`->` is forbidden).
- Declaring a `@directive` whose name collides with a built-in (e.g. `@native`, `@each`, `@property`, `@inline`, `@parallel`, `@const`, `@deprecated`) is rejected at compile time. Declaring the same directive name twice in one program is also rejected.

**Parameters:**

Parameters use Ry's standard type syntax (`str`, `int`, `bool`, `list`, etc.). Type annotations are optional and default to `any` when omitted; however, a parameter with a default value **must** carry an explicit type annotation. Every parameter (whether required or defaulted) may be passed either positionally — in declaration order — or by name at the use site. Defaulted parameters may also be omitted, in which case the declared default is used. Required parameters must precede defaulted parameters in the declaration.

```ry
@directive(target=["function"])
fn logged(label: str)                     # required (positional or named)

@directive(target=["function"])
fn cached(ttl: int = 60)                  # defaulted (positional, named, or omitted)
```

**Use site:**

```ry
from mypkg import logged, cached

@logged("hello")                          # required by position
fn targetFn() -> int:
    return 1

@logged(label="hello")                    # required by name
fn targetFn2() -> int:
    return 1

@cached()                                 # use default
fn slowFn() -> int:
    return compute()

@cached(3600)                             # positional override
fn fastFn() -> int:
    return 7

@cached(ttl=3600)                         # named override
fn otherFn() -> int:
    return 42
```

Each parameter may be supplied either positionally or by name, but not both forms for the same parameter — `@logged("hello", label="hi")` is rejected as a duplicate, and likewise `@cached(3600, ttl=7200)` is rejected. `@logged(unknown="y")` is rejected: only declared parameter names may appear at the use site. `@logged()` is rejected when `label` is required (defaulted parameters may still be omitted).

### Target mismatch is a silent no-op

Applying a user-defined directive to a node that is not in its declared `target=[...]` list is a **silent no-op**: compilation and execution proceed normally, no warning or error is produced, and any future effects of the directive (metadata registration, runtime hooks) are suppressed. Argument validation is also skipped for the mismatching application, so a missing required argument does not surface a diagnostic when the target is wrong.

```ry
@directive(target=["function"])
fn audit(label: str)

@audit("hello")          # function-only directive applied to a record
record User:             # → silently ignored, User compiles normally
    id: int

@audit                   # missing required argument — also silent because
record Other:            #   the target still doesn't match
    id: int
```

When the target *does* match, every constraint applies as usual: missing required arguments, unknown named arguments, and duplicate bindings are all rejected at compile time.

This silent-no-op resolution is intentional for v0.0.15 to support tag-style usage of user-defined directives. A later minor version may upgrade the diagnostic to a warning.

#### Parser-level restrictions

The compiler built-in directive `@native` cannot be applied to `for` statements or function-call statements; doing so is a parse error. The same `@parallel` directive applied more than once on a single `for` statement is also a parse error. All user-defined directives — those declared via `@directive(target=[...]) fn name(...)` — are accepted at both sites: when the declared target list matches the use site (`"for"` for `for` statements, `"statement"` for function-call statements) the directive is processed normally; when it does not match, the directive is silently ignored per the target-mismatch rule above.

### Export and import

Directive declarations participate in the standard module system. A directive declared in `pkg/mod.ry` is exported by name and can be imported with `from pkg import directiveName`. Directive names beginning with `_` are private to the declaring package and cannot be imported.

Most built-in directives are now declared in `share/std/`. Core directives (`@deprecated`, `@const`, `@inline`, `@parallel`) are declared in `share/std/core/directive.ry` and are re-exported via `share/std/builtins.ry`, which means they are implicitly available without an explicit import. Testing directives (`@it`, `@describe`, `@each`, `@property`) are declared in `share/std/testing/testing.ry`; test files that use them must add `from testing import it, describe, each, property` (or the subset they need) at the top. Only `@directive` and `@native` remain as compiler built-ins (see "Bootstrap rule" below).

### Bootstrap rule

`@directive` itself and `@native` are **compiler built-ins** and cannot be redeclared in `.ry` source. (`@native` is registered in `src/directive_meta.cpp`'s built-in registry; `@directive` is handled as a hardcoded special form in the parser.) The reason is self-referential: the `@directive(...)` declaration syntax binds a directive to its C++ implementation through `@native`, and `@native` cannot mark its own declaration. These two directives remain permanently in C++.

All other built-in directives (`@deprecated`, `@const`, `@inline`, `@parallel`, `@each`, `@property`) are now declared in `share/std/` (the core ones in `share/std/core/directive.ry`, the testing ones in `share/std/testing/testing.ry`). Only `@directive` and `@native` remain in the C++ registry due to the bootstrap constraint.

## Notes

- Deprecated entities still function normally; only a warning is emitted.
- Warnings are emitted at the point of use, not at the definition.
- Defining a deprecated entity without using it produces no warnings.
- Unknown directive names cause a parse error.
- Directives on unsupported targets (e.g., `if`, `while`) cause a parse error. Among built-in directives, `@parallel` is the only one targeting `for`; user-defined directives may also declare `target=["for"]`.
