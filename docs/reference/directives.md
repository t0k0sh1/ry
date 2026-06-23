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
- `enum` - Enum definitions (currently `@public` and `@doc` target enums)
- `type` - Type alias declarations (currently `@public` and `@doc` target type aliases)
- Variable declarations (with or without `@const`)
- Fields within a `record` definition
- `for` - Counted loops; among built-ins, only `@parallel` targets `for`. User-defined directives declaring `target=["for"]` may also be applied to `for` statements; directives with a different target are silently ignored per the target-mismatch rule.
- `@directive` declarations themselves (so a directive declaration can be marked `@public` for cross-package import or carry a `@doc` documentation string)

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

### `@public`

Marks a definition as visible across package boundaries. Without `@public`, every definition is **package-internal** — visible only within the same package (the directory tree rooted at the nearest ancestor `package.toml`). Adding `@public` lifts the definition to **universe** scope so it can be imported from any package.

**Defined as:** Compiler built-in. Registered in `src/directive_meta.cpp`'s built-in registry; there is no `.ry` declaration for `@public` in `share/std/core/directive.ry`.

**Applicable to:** function (`fn`), record, enum, `type` alias, variable declarations (with or without `@const`), and `@directive` declarations.

The directive accepts no arguments. It is placed on its own line immediately above the declaration, and multiple directives may be stacked.

```ry
@public
fn add(a: int, b: int) -> int:
    return a + b

@public
record Point:
    x: int
    y: int

@public
@const
PI = 3.14159
```

**Cross-package import behavior:**

Given `mylib/calc.ry` (where `mylib/` contains a `package.toml` of its own):

```ry
# mylib/calc.ry
@public
fn add(a: int, b: int) -> int:
    return a + b

fn helper(n: int) -> int:    # package-internal — no @public
    return n * 2
```

From a different package (importer outside `mylib/`):

```ry
from mylib import add        # OK — add is @public
from mylib import helper     # Error — 'helper' is not @public
from mylib                   # wildcard: add is callable; helper is co-located in the linkage
                             # unit so a @public facade in mylib can call it (REQ-B3 wrapper
                             # pattern, see docs/guide/visibility.md)
```

From inside the same package as `mylib/calc.ry`, both `add` and `helper` are importable regardless of `@public`.

The leading `_` underscore on an identifier carries **no** visibility meaning; visibility is controlled exclusively by `@public`. See [Modules — Visibility](modules.md#visibility) for the full rules and [Glossary — Visibility scopes](glossary.md#visibility-scopes) for the underlying scope vocabulary.

### `@doc`

Attaches a Markdown documentation string to a declaration. The argument is preserved as metadata; the compiler does not parse Markdown itself. A documentation generator is out of scope for the current release — `@doc` exists today so the documentation lives next to the declaration and is available to future tooling.

**Defined as:** Compiler built-in. Registered in `src/directive_meta.cpp`'s built-in registry; there is no `.ry` declaration for `@doc`.

**Applicable to:** function (`fn` and `async fn`), record, record fields, enum, `type` alias, and `@directive` declarations. `@doc` cannot be applied to `for` loops, function-call statements, or enum variants.

The directive takes exactly one positional string argument. Empty strings (`@doc("")`) are accepted. The argument may be a single-line string (`"..."`) or a triple-quoted [block string](builtins-string.md#block-string-literal-) (`"""..."""`) — block strings are the canonical form for multi-line Markdown bodies because they read naturally and preserve indentation through `ry fmt`.

```ry
@doc("Returns the absolute value of x.")
@public
fn abs(x: int) -> int:
  if x < 0:
    return -x
  return x

@doc("""
A point in 2D space.

Components use floating-point coordinates so the same record can be reused
for both pixel and world-space geometry.
""")
record Point:
  @doc("Horizontal coordinate.")
  x: float
  @doc("Vertical coordinate.")
  y: float

@doc("""
Returns a stream on success, or `None` on failure.

## Parameters

- `host`: host name or IP address
- `port`: TCP port
""")
fn tcpConnect(host: str, port: int) -> TcpStream?
```

**Convention:** Prefer Markdown sections (`## Parameters`, `## Returns`, `## Examples`) over Javadoc-style `@param` / `@return` tags. Ry signatures already carry names and types, so a separate parameter syntax would duplicate that information.

**Duplicate rejection:** A declaration may carry at most one `@doc`. Stacking two `@doc` directives on the same declaration is rejected with `duplicate directive '@doc' on the same declaration`. This rule generalises to every directive — `@public @public`, `@deprecated @deprecated`, and any future duplicate is rejected the same way.

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

**First-class function values:**

A `@native` function imported with `from <module> import <name>` can be used as a first-class function value — bound to a variable, passed as an argument, or returned from a function — provided it has **exactly one overload**:

```ry
from convert import int
from str import startsWith

xs: List<str> = ["1", "2", "3"]
results = xs.map(int)              # ok: int has a single (str) overload
f = int
print(f("42"))                       # Ok(42)

g = startsWith                       # ok: full-arity 3-param binding
print(g("hello", "he", false))       # default arg must be supplied here
```

Names with multiple overloads (e.g. `str` over `int`/`float`/`bool`, and most `math`-module custom-emitter natives such as `abs`/`pow`/`round`/`log`) are rejected at compile time with `ambiguous reference to @native function 'X': multiple overloads exist; wrap in a lambda to select one`. Wrap them in a lambda to pin the desired overload:

```ry
fmt = (n: int) => str(n)           # picks the (int) overload explicitly
[1, 2, 3].map(fmt)                   # ["1", "2", "3"]
```

When a `@native` function declares default arguments, the materialized binding is **full-arity** — the default-omission shortcut is only available on the original direct call.

**Standard library declarations (`share/std/`):**

`@native` declarations for all built-in functions live under `share/std/` relative to the `ry` executable, organized by category. These files form the canonical [`ry.lang`](modules.md#canonical-module-paths) prelude and are automatically loaded into every program; they enable argument count validation for built-in function calls. For the full function reference, see [Builtins](builtins.md), [Builtins — String](builtins-string.md), and [Collections](collections.md).

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
- The runtime function name follows the convention `__ry_<libname>_<symbol>` (e.g., `@native("base64") fn encode(...)` → `__ry_base64_encode`). For most modules the symbol mirrors the Ry function name verbatim (`filesystem::listDir` → `__ry_filesystem_listDir`); legacy modules such as `base64` and `string` still use snake_case C symbols (`encodeUrlSafe` → `__ry_base64_encode_url_safe`). See `modules.md` for the full mapping table. This convention works for both stdlib modules and user-defined native libraries.

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

**Syntax:**

```ry
from testing import it, each, expect

@each([(1, 2), (3, 6), (5, 10)])
@it("should double {0} as {1}")
fn testDouble(input: int, expected: int):
    expect(input * 2).toEq(expected)
```

The argument can be any expression that evaluates to a list of tuples, including a function call:

```ry
from testing import it, each, expect

fn makeInputs() -> List<(int, int)>:
    return [(1, 1), (2, 4), (3, 9)]

@each(makeInputs())
@it("should square {0} as {1}")
fn testSquare(n: int, expected: int):
    expect(n * n).toEq(expected)
```

**Supported targets:** functions with the `@it` directive.

**Constraints:**
- The argument must evaluate to a list of tuples
- Tuple arity must match the function parameter count
- Placeholders `{0}`, `{1}`, ... in the description string are replaced with stringified values

### `@property`

Enables property-based testing by generating random inputs for a test.

**Defined as:** Declared in `share/std/testing/testing.ry`. Test files must add `from testing import property` (or include it in the existing `from testing import` line) at the top.

**Syntax:**

```ry
from testing import it, property, expect

@property(count=100)
@it("should verify multiplication is commutative")
fn testCommutative(a: int, b: int):
    expect(a * b).toEq(b * a)
```

**Supported targets:** functions with the `@it` directive.

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
from testing import it, expect

@it("should pass simple assertion")
fn testCase():
    expect(true).toBeTrue()
```

**Basic example:**

```ry
from testing import it, expect

@it("should add 1 + 2 = 3")
fn testAdd():
    expect(1 + 2).toEq(3)
```

**Composed with `@each` or `@property`:**

```ry
from testing import it, each, property, expect

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

Groups a set of related tests by attaching the directive to a named function. Inner `@it` functions declared in the body belong to the group, and variables declared directly in the body act as shared setup captured by every inner `@it`. `@describe` groups **may be nested**; output is indented proportionally to nesting depth.

**Defined as:** Declared in `share/std/testing/testing.ry`. Test files must add `from testing import it, describe` at the top.

**Syntax:**

```ry
from testing import it, describe, expect

@describe("group name")
fn groupName():
    @it("should pass nested test")
    fn testNested():
        expect(true).toBeTrue()
```

**Basic example:**

```ry
from testing import it, describe, expect

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
from testing import it, describe, expect

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
from testing import it, describe, expect

@describe("outer")
fn outer():
    @describe("inner")
    fn inner():
        @it("should pass deeply nested test")
        fn testDeep():
            expect(1 + 1).toEq(2)
```

**Supported target:** `fn` declarations only. The function must not have parameters or a return type annotation.

### `@skip`

Marks an `@it` test as skipped. The function body is not executed; the test is reported as `~ <name> (skipped)` (gray) and counted as `skipped` in the summary. The exit code is unaffected (only `failed` counts).

**Defined as:** Declared in `share/std/testing/testing.ry`. Test files must add `from testing import skip` at the top (or use a wildcard `from testing`).

```ry
from testing import it, expect, skip

@skip
@it("temporarily disabled while bug #123 is open")
fn skipped():
    expect(1).toEq(2)
```

Composes with `@each` and `@property` — `@skip @each @it(...)` and `@skip @property @it(...)` skip the entire loop without running any iteration.

**Supported target:** functions with the `@it` directive only. Attaching `@skip` to `@describe` is a compile error in the current MVP.

**Mutual exclusion:** `@skip` combined with `@only` or `@todo` is a compile error.

### `@only`

When at least one `@only` appears in a test file, every `@it` in that file **without** `@only` is implicitly skipped. Useful for focused TDD on a single failing case without commenting out or deleting other tests.

**Defined as:** Declared in `share/std/testing/testing.ry`. Test files must add `from testing import only` at the top (or use a wildcard `from testing`).

```ry
from testing import it, expect, only

@only
@it("the one failing case I am currently debugging")
fn focused():
    expect(1 + 1).toEq(2)

@it("this is implicitly skipped because the file has @only")
fn other():
    expect(1).toEq(1)
```

Composes with `@each` and `@property`. The focus filter is **per file** — it does not affect other test files.

In outline mode (`ry test --outline`), all tests are shown regardless of `@only`, so the focus filter does not hide them; only the suffix `(@only)` is added so the focused tests stand out.

**Supported target:** functions with the `@it` directive only. Attaching `@only` to `@describe` is a compile error in the current MVP.

**Mutual exclusion:** `@only` combined with `@skip` or `@todo` is a compile error.

### `@todo`

Marks an `@it` test as a not-yet-implemented placeholder. The function body is **never emitted** by codegen, so it may reference undefined identifiers, omit a `return`, or otherwise fail compilation; only the directive itself is validated. Reported as `? <name> (todo)` (cyan) and counted as `todo`.

**Defined as:** Declared in `share/std/testing/testing.ry`. Test files must add `from testing import todo` at the top (or use a wildcard `from testing`).

```ry
from testing import it, todo

@todo
@it("upcoming feature, body not yet written")
fn upcoming():
    notYetDefined()   # body is never compiled
```

Composes with `@each` and `@property` — the loop body is never emitted; the test is counted as a single `todo` regardless of how many iterations the `@each` table or `@property` count would have produced.

**Supported target:** functions with the `@it` directive only. Attaching `@todo` to `@describe` is a compile error in the current MVP.

**Mutual exclusion:** `@todo` combined with `@skip` or `@only` is a compile error.

### `@timeout`

Marks an `@it` test as having a per-test timeout in milliseconds. If the body
takes longer than the specified `ms` to complete, the test is marked as
**failed** with a "(timeout after Nms)" suffix, and execution continues with
the next test. This contrasts with the file-level alarm (60s; 300s under
ASan) which terminates the entire test process on expiry.

**Defined as:** Declared in `share/std/testing/testing.ry`. Test files must
add `from testing import timeout` at the top (or use a wildcard `from
testing`).

```ry
from testing import it, expect, timeout

@timeout(1000)
@it("completes within 1 second")
fn completesWithinOneSecond():
    expensiveOperation()
    expect(result).toEq(expected)
```

**Argument constraints (validated at compile time):**

- `ms` must be a **positive integer literal** (not zero, not negative, not a
  non-literal expression). Identifiers, function calls, and string literals
  are rejected.

**Supported target:** functions with the `@it` directive only.

**Mutual exclusion:** `@timeout` combined with `@each` or `@property` is a
compile error in the current MVP. The timer applies to one invocation of
the test body; loop-style runners cannot share a single timer budget across
iterations without ambiguity. The compiler emits one of the following
diagnostics (from `src/codegen_test.cpp`):

```text
error: @timeout cannot be combined with @each on fn '<function_name>'
error: @timeout cannot be combined with @property on fn '<function_name>'
```

See [Feature interactions](testing.md#feature-interactions) in the testing
reference for the full mutual-exclusion matrix.

**Composition with `@skip` / `@todo`:** these directives suppress body
execution, so the timer never starts — there is no conflict. `@only`
affects test selection only and is orthogonal to `@timeout`.

**Implementation:** The timer is delivered via `setitimer(ITIMER_REAL, ms)`
+ `SIGALRM`; the signal handler routes back into the test runner via
`siglongjmp`, so a hung test does NOT take down the test process. A test
with `@timeout(N)` is structured around **two** independent `sigsetjmp`
landing pads — one for the body (file `@beforeEach` → describe
`@beforeEach` → test body) and one for `@afterEach` (describe `@afterEach`
→ file `@afterEach`) — so each phase gets its own fresh `N`-ms budget
and a hung `@afterEach` cannot block subsequent tests.

**Known limitation (ARC):** when `@timeout` fires mid-test, the runtime
unwinds via `siglongjmp` and **skips ARC release** for objects allocated
inside the body whose phase fired (test body for a body timeout,
`@afterEach` body for an `@afterEach` timeout). The over-retained
objects are leaked, not reclaimed by destructors — this is a leak, NOT
a use-after-free: subsequent tests cannot observe the leaked memory,
but ASan / leak detectors may report it. The leak is reclaimed at
process exit. This is an accepted trade-off — calling C++ destructors
from a signal-driven longjmp would be undefined behavior.

**Composition with `@afterEach`:** when `@timeout` fires mid-test
(body phase), the runtime unwinds the body via `siglongjmp`, then
**runs `@afterEach` under its own fresh `N`-ms budget** (#1781). If
`@afterEach` itself does not complete within that budget, a secondary
failure line `@afterEach (timeout after Nms)` is emitted alongside the
body's timeout line and the test runner proceeds to the next `@it`. The
worst-case wall-clock per test is therefore `2N`. See `@afterEach`
below.

### `@beforeEach`

Runs before every `@it` inside the enclosing `@describe`. Used for
per-test setup (resetting state, allocating fresh fixtures, etc.).

**Defined as:** Declared in `share/std/testing/testing.ry`. Test
files must add `from testing import beforeEach` at the top (or use
a wildcard `from testing`).

```ry
from testing import describe, it, beforeEach, expect

@describe("counter starts fresh each test")
fn counterTests():
    counter = 99

    @beforeEach
    fn reset():
        counter = 0

    @it("first sees counter == 0 (not 99)")
    fn first():
        expect(counter).toEq(0)

    @it("second also sees counter == 0")
    fn second():
        expect(counter).toEq(0)
```

**Supported target:** functions inside a `@describe` body or at file
top level, with no parameters and no declared return type.

**Constraints:**

- At most one `@beforeEach` per describe, and at most one per file at
  top level
- Cannot coexist on the same function with `@it`, `@describe`,
  `@timeout`, `@skip`, `@only`, `@todo`, `@each`, `@property`, or any
  other lifecycle hook directive
- Hooks declared outside a `@describe` (file top level) wrap every
  test in the file and cascade with describe-level hooks; see
  [Execution order](testing.md#execution-order)
- The hook body cannot introduce new named variables (see
  [Lifecycle Hooks](testing.md#lifecycle-hooks) for the re-emission
  constraint)

**Implementation:** Hook bodies are not emitted as standalone LLVM
functions; the AST body is stashed and inlined into the describe
scope before each `@it` call. Describe-scope variable mutation
therefore propagates to subsequent `@it` invocations (Ry's
accumulating semantics — see the testing reference for contrast with
Jest).

### `@afterEach`

Runs after every `@it` inside the enclosing `@describe`. Used for
per-test cleanup. Runs even when the test body fails, times out, or
when `@beforeEach` mid-runs — see _Composition with `@timeout`_ and
_Composition with failing assertions_ below.

**Defined as:** Declared in `share/std/testing/testing.ry`.

```ry
from testing import describe, it, afterEach, expect

@describe("cleanup after each test")
fn cleanupTests():
    log = ""

    @afterEach
    fn appendMarker():
        log = log + "AE;"

    @it("first test")
    fn first():
        expect(log).toEq("")

    @it("second test sees prior afterEach")
    fn second():
        expect(log).toEq("AE;")
```

**Supported target:** functions inside a `@describe` body or at file
top level, with no parameters and no declared return type. Same
constraints as `@beforeEach`.

**Composition with `@timeout`:** `@afterEach` runs even after a body
`@timeout(N)` fires (#1781). The codegen wraps `@afterEach` in its own
`sigsetjmp` landing pad with a fresh `N`-ms `setitimer` budget, so a
body timeout siglongjmps out of the body, lands in the `@afterEach`
phase, and cleanup proceeds. If `@afterEach` itself does not complete
within its `N`-ms budget, a secondary failure line
`@afterEach (timeout after Nms)` is printed alongside the body's
outcome line and execution moves on to the next `@it` instead of
hanging the process. Because the body / `@beforeEach` may be partially
complete when `@afterEach` runs in the timeout path, `@afterEach`
should be written to tolerate **partially set-up state** (e.g. guard
on whether a handle is non-nil before closing it). See also the
`@timeout` ARC limitation above — objects allocated inside an
`@afterEach` body that itself times out are leaked, not reclaimed.

**Composition with failing assertions:** when an `expect` fails, the
test is marked failed but execution within the test body stops at
the failing assertion. `@afterEach` still runs for the failed test
because the test function returns normally (the failure is recorded
via a flag, not an exception).

### `@beforeAll`

Runs **once** before the first `@it` inside the enclosing
`@describe`. Used for one-time setup that all tests share.

**Defined as:** Declared in `share/std/testing/testing.ry`.

```ry
from testing import describe, it, beforeAll, expect

@describe("seed shared resource once")
fn sharedResourceTests():
    setupCount = 0

    @beforeAll
    fn seed():
        setupCount = setupCount + 1

    @it("setupCount == 1")
    fn first():
        expect(setupCount).toEq(1)

    @it("setupCount still == 1 (beforeAll did not re-run)")
    fn second():
        expect(setupCount).toEq(1)
```

**Position independence:** `@beforeAll` runs before the first `@it`
even when declared **after** the `@it` lexically. The describe body
is scanned for hooks before any tests are emitted.

**Supported target:** functions inside a `@describe` body or at file
top level, with no parameters and no declared return type. Same
constraints as `@beforeEach`.

### `@afterAll`

Runs **once** after the last `@it` inside the enclosing `@describe`.
Used for one-time teardown.

**Defined as:** Declared in `share/std/testing/testing.ry`.

```ry
from testing import describe, it, beforeAll, afterAll, expect

@describe("shared handle opened once, closed once")
fn teardownTests():
    openCount = 0
    closeCount = 0

    @beforeAll
    fn open():
        openCount = openCount + 1

    @afterAll
    fn close():
        closeCount = closeCount + 1

    @it("first test")
    fn first():
        expect(openCount).toEq(1)

    @it("second test")
    fn second():
        expect(openCount).toEq(1)
```

**Supported target:** functions inside a `@describe` body or at file
top level, with no parameters and no declared return type. Same
constraints as `@beforeEach`.

**Note:** `@afterAll` runs after the last `@it` regardless of
whether individual tests failed. It does **not** run if the entire
test process is terminated by an external signal.

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

Modules can declare their own compile-time directives with the `@directive(...)` declaration syntax. A user-defined directive becomes available in any source file that imports it; applying it without importing produces an `unknown directive` error.

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
from mymodule import logged, cached

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

Directive declarations participate in the standard module system. A directive declared in `mymodule/mod.ry` is exported by name and can be imported with `from mymodule import directiveName`. Like other definitions, directives are package-internal by default and require [`@public`](#public) to be importable across package boundaries (see [Modules — Visibility](modules.md#visibility)).

Most built-in directives are now declared in `share/std/`. Core directives (`@deprecated`, `@const`, `@inline`, `@parallel`) are declared in `share/std/core/directive.ry` and are re-exported via `share/std/builtins.ry`, which means they are implicitly available without an explicit import. Testing directives (`@it`, `@describe`, `@each`, `@property`) are declared in `share/std/testing/testing.ry`; test files that use them must add `from testing import it, describe, each, property` (or the subset they need) at the top. The compiler built-ins `@directive`, `@native`, and `@public` have no `share/std/` declaration (see "Bootstrap rule" below).

### Bootstrap rule

`@directive`, `@native`, and `@public` are **compiler built-ins** and have no declaration in `share/std/`. The reasons differ:

- `@directive` and `@native` are tied together by self-reference: the `@directive(...)` declaration syntax binds a directive to its C++ implementation through `@native`, and `@native` cannot mark its own declaration. (`@native` is registered in `src/directive_meta.cpp`'s built-in registry; `@directive` is handled as a hardcoded special form in the parser.) These two directives remain permanently in C++.
- `@public` is registered in `src/directive_meta.cpp` purely so the parser can validate its placement before the visibility model has any user-defined directives to consult — it provides parser-level metadata, not a self-reference workaround.

All other built-in directives (`@deprecated`, `@const`, `@inline`, `@parallel`, `@each`, `@property`) are declared in `share/std/` (the core ones in `share/std/core/directive.ry`, the testing ones in `share/std/testing/testing.ry`).

## Notes

- Deprecated entities still function normally; only a warning is emitted.
- Warnings are emitted at the point of use, not at the definition.
- Defining a deprecated entity without using it produces no warnings.
- Unknown directive names cause a parse error.
- Directives on unsupported targets (e.g., `if`, `while`) cause a parse error. Among built-in directives, `@parallel` is the only one targeting `for`; user-defined directives may also declare `target=["for"]`.
