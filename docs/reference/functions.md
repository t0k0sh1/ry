# Function Reference

## Function Definition Syntax

```ry
fn functionName(paramName: type, ...) -> returnType:
    # body
    return value
```

- Parameter types are optional. When omitted, the parameter is treated as `any` type.
- A trailing comma after the last parameter is allowed: `fn f(a: int, b: int,) -> int:`.
- Return type is optional. When omitted, the return type is **inferred from the body** (both named functions and lambdas). If the function has no `return` statement, the return type is inferred as `Unit`. Use `-> any` explicitly for functions that should accept any return type.
- The body is an indented block.
- Functions with an explicit return type (other than `Unit` or `any`) must have a `return` statement on all control flow paths. The compiler reports an error if any path is missing a return.
- Functions can have `require` (precondition) and `ensure` (postcondition) clauses. See [Design by Contract](contracts.md).

> **Naming convention**: Function names and parameter names must use camelCase (e.g., `add`, `getValue`, `mapList`). See [Naming Conventions](naming.md) for the full convention. The compiler enforces this rule.

```ry
fn add(a: int, b: int) -> int:
    return a + b

fn greet(name: str) -> Unit:
    print("Hello, " + name)   # Return type is Unit (explicit)
```

---

## Parameter and Return Types

| Item | Description |
|---|---|
| Parameter type | Optional. Defaults to `any` when the `: type` annotation is omitted |
| Return type | Optional. Inferred from the body when omitted (inferred as `Unit` if no `return` statement) |
| `Unit` | Return type for functions that return no value |

> **Note**: Function parameters are **immutable**. You cannot reassign a parameter inside the function body. This ensures that parameter values at entry are always available for postcondition checks (see [Design by Contract](contracts.md)).

```ry
fn noReturn(x: int) -> Unit:  # Return type Unit (explicit)
    print(x)

fn getValue() -> int:     # Return type int
    return 42

fn identity(x) -> any:    # Parameter type any (omitted)
    return x
```

### Type Omission and `any`

When a parameter type annotation is omitted, the parameter is treated as `any` — a dynamic type that accepts any primitive value at runtime. This is similar to Python's untyped parameters.

```ry
# All parameters default to any
fn add(a, b):
    return a + b

add(1, 2)              # 3 (int + int)
add("hello", " world") # "hello world" (str + str)
add(1, 2.0)            # 3.0 (int + float)
```

You can also use `any` explicitly in type annotations:

```ry
fn identity(x: any) -> any:
    return x
```

### Return Type Inference

When the return type is omitted, it is inferred from the `return` statements in the body:

```ry
fn double(x: int):     # return type inferred as int
    return x * 2

fn greet(name: str):   # return type inferred as Unit (no return)
    print("Hello, " + name)
```

To explicitly allow any return type, use `-> any`:

```ry
fn flexible(x: any) -> any:
    return x    # can return int, float, str, etc.
```

---

## Nested Functions

Functions can be defined inside other functions. A nested function is only visible within its enclosing function's scope — it cannot be called from outside.

```ry
fn outer() -> int:
    fn helper() -> int:
        return 42
    return helper()

outer()     # 42
# helper()  # error: undefined function
```

Same-named nested functions in sibling scopes do not collide:

```ry
fn foo() -> int:
    fn helper() -> int:
        return 1
    return helper()

fn bar() -> int:
    fn helper() -> int:
        return 2
    return helper()

foo()   # 1
bar()   # 2
```

Nested functions can be used as values and passed to higher-order functions. Mutual recursion between nested functions in the same scope also works (the compiler forward-declares them).

### Closure Capture

Nested named functions can capture variables from enclosing scopes, just like lambdas. When a nested function references an outer variable, it becomes a closure:

```ry
fn makeAdder(base: int) -> fn(int) -> int:
    fn add(x: int) -> int:
        return x + base
    return add

add10 = makeAdder(10)
add10(5)   # 15
```

Capture rules:

- Captures are **by value** (same as lambdas). The value is copied at the point the closure is created.
- Captured variables **cannot be reassigned** inside the nested function body.
- ARC-managed values (strings, lists, etc.) are properly retained and released.
- If a nested function has no captures, it remains a plain function pointer (no overhead).
- Multi-level capture works: a deeply nested function can reference variables from any enclosing scope.

---

## Recursion

Functions can call themselves.

```ry
fn factorial(n: int) -> int:
    if n <= 1:
        return 1
    return n * factorial(n - 1)
```

### Mutual Recursion

Functions can call each other regardless of definition order. The compiler forward-declares functions with explicit return types before processing function bodies — this applies both at the top level and inside another function body (nested functions) — provided all referenced types are already known (primitive types are always available; record/enum types must be defined earlier in the file).

```ry
fn isEven(n: int) -> bool:
    if n == 0:
        return true
    return isOdd(n - 1)       # calls isOdd defined below

fn isOdd(n: int) -> bool:
    if n == 0:
        return false
    return isEven(n - 1)      # calls isEven defined above
```

**Requirements for forward references:**

- The function must have an **explicit return type** annotation (`-> type`). Functions with inferred return types cannot be forward-referenced.
- The function must be defined at the **top level** or inside another function body. Forward references work within the same scope level.
- All parameter and return types must be resolvable at the point of forward declaration (e.g., record types must be defined before the functions that use them).

### Top-Level Variables and `@const` in Function Bodies

Top-level `let` bindings and `@const` declarations are visible from any top-level function — including nested functions and lambdas inside those functions — as long as the declaration appears **textually before** the referencing function in the same source file.

```ry
@const
PI: float = 3.14

@const
MAX_RETRIES: int = 5

counter: int = 0

fn area(radius: float) -> float:
    return PI * radius * radius            # reads top-level @const

fn clampRetries(n: int) -> int:
    if n > MAX_RETRIES:
        return MAX_RETRIES
    return n

fn bump():
    counter = counter + 1                  # writes top-level mutable `let`
```

**Rules:**

- **Source-order strict.** A function body cannot reference a top-level binding declared after it in the same file. Move the binding above the function, or wrap the binding in a helper function called lazily.
- **`@const` is read-only.** Reassignment or field mutation (`P.x = 99` for a top-level `@const P: Point`) is rejected at compile time.
- **Mutable `let` writes are write-through.** Assigning to a top-level mutable variable from inside a function actually mutates the top-level binding — it does not create a local with the same name.
- **Nested blocks are not module-level.** A `let` inside a top-level `if`, `while`, or `for` block is local to that block and is not visible from functions.

**Limitations (v0.0.8):**

- A parallel `for` block cannot assign to a top-level mutable variable (data-race avoidance).
- Top-level `weak` references and resource-typed bindings (file/regex handles) cannot yet be accessed from function bodies — track these use cases in follow-up issues if you need them.

### Tail Call Optimization

The compiler automatically detects self-recursive tail calls — where the last action in a function is a call to itself — and applies LLVM's `musttail` optimization. This guarantees that tail-recursive functions use constant stack space, preventing stack overflow for deep recursion.

```ry
fn sumTo(n: int, acc: int) -> int:
    if n <= 0:
        return acc
    return sumTo(n - 1, acc + n)    # tail call → optimized

sumTo(1000000, 0)    # works without stack overflow
```

**Conditions for TCO:**

- The function calls itself directly in a `return` statement (`return f(args)`)
- The call result is returned without any further computation (`return n * f(n-1)` is NOT a tail call)
- The function has no `ensure` (postcondition) clauses

Mutual recursion (A calls B, B calls A) is not currently optimized for tail calls.

---

## Overloading

Multiple functions with the same name can be defined if they differ in the number or types of parameters.

### Rules

- Functions with the same name can be defined if the number or types of parameters differ.
- The appropriate function is selected at the call site based on the argument types and count.
- Overloading by return type alone is not allowed.

```ry
fn area(side: int) -> int:
    return side * side

fn area(w: int, h: int) -> int:
    return w * h

a = area(5)       # 25
b = area(3, 4)    # 12
```

### Resolution Priority

When multiple overloads match a call, the compiler selects the most specific one using the following priority (highest first):

1. **Exact type match** — argument type matches parameter type exactly
2. **Implicit widening** — safe widening conversion (`u8` → `int`, `u8` → `float`, `int` → `float`)
3. **Union type match** — argument type is a member of a union parameter type
4. **`any` type match** — parameter type is `any` (accepts anything)

The overload with the most exact matches wins. If two or more overloads have equal specificity, the compiler reports an ambiguity error.

Low-level numeric types (`i8`, `i16`, `i32`, `i64`, `u8`–`u64`, `f32`) do **not** participate in implicit widening — they require explicit `as` casts.

```ry
fn process(x: int) -> str:
    return "int"

fn process(x) -> str:          # x: any
    return "any"

process(42)       # "int" — exact match (int) beats any
process("hello")  # "any" — no exact match for str, falls back to any
```

```ry
fn double(x: float) -> float:
    return x * 2.0

double(5)         # OK — int is implicitly widened to float, returns 10.0
```

---

## Default Arguments

Parameters can have default values, allowing callers to omit trailing arguments.

### Syntax

```ry
fn connect(host: str, port: int = 8080, timeout: int = 30):
    # ...

connect("localhost")                    # port=8080, timeout=30
connect("localhost", 3000)              # port=3000, timeout=30
connect("localhost", 3000, 10000)       # port=3000, timeout=10000
```

### Rules

- Default parameters must come after all non-default parameters.
- A parameter with a default value **must** have an explicit type annotation (e.g., `x: int = 10`; `x = 10` is a compile error).
- Default values must be compile-time constant expressions (literals and `@const` variables).
- If a function with default arguments creates an ambiguous overload (overlapping arity with matching types), the compiler reports an error.

```ry
# Error: ambiguous overload
fn calc(x: int, y: int = 0) -> int:
    return x + y
fn calc(x: int) -> int:      # conflicts with calc(int) from above
    return x * 2
```

### Limitations

- Default arguments are not supported in **generic functions** or **lambda expressions**.

---

## Reserved Built-in Names

Top-level user functions cannot reuse names that the standard library reserves for built-ins. Names such as `sum`, `min`, `max`, `len`, `range`, `print`, `enumerate`, `zip`, `map`, `filter`, `fold`, `reduce`, `iter`, `Ok`, `Err`, `Some`, `None`, `Error`, and others dispatch directly through the compiler's built-in chain. A user `fn sum(...)` at the top level would never be called — the body would be silently ignored. The compiler rejects such declarations at definition time with `cannot declare function 'sum': name is reserved for a built-in function`.

```ry
# Error: cannot declare function 'sum': name is reserved for a built-in function
fn sum(values: List<int>) -> int:
    return 999
```

The check applies to:

- Top-level non-`@native` `fn` declarations.
- Top-level generic `fn` templates (e.g. `fn map<T, U>(...)`).
- `from <module> import <name> as <reserved>` import aliases.

The check does **not** apply to:

- `@native` declarations (the stdlib's own implementations).
- Nested `fn`s inside another function body — these are scope-local and cannot be observed by the built-in dispatcher.
- Functions defined in a user module accessed via qualified import (`<mod>.<name>` dispatch consults the module's namespace directly).
- Type-aware extension points such as `fn toStr(p: MyRecord)` — for record types, the codegen consults the user definition first, so `toStr` is not part of the reserved set. See [Records](records.md) for the record `toStr` override pattern.

---

## Unit Type Functions

Functions without a return value return `Unit`. The return type can be omitted (inferred as `Unit`) or explicitly specified with `-> Unit`.

```ry
fn log(msg: str) -> Unit:
    print(msg)
```

---

## Tasks And Async Functions

`Task<T>` is the built-in handle type for concurrent work. `async fn` returns `Task<T>`, `await` extracts `T` inside another `async fn`, and `blockOn(task)` blocks from synchronous context until the task completes.

```ry
async fn add(a: int, b: int) -> int:
    return a + b

# From synchronous context, use blockOn()
t: Task<int> = add(20, 22)
print(blockOn(t))                  # 42
blockOn(add(1, 2))                 # waits and discards the result

# Inside async fn, use await
async fn doubleAdd(a: int, b: int) -> int:
    return (await add(a, b)) * 2
```

### Rules

- `async fn name(...) -> T:` is declared with the awaited result type `T`.
- Calling an `async fn` immediately returns `Task<T>`.
- `await expr` requires `expr` to be `Task<T>` and produces `T`.
- `await` can only be used inside an `async fn`. Use `blockOn(task)` from synchronous context.
- `blockOn(task)` blocks the current thread until the task completes and returns the result.
- `async fn ... -> Unit` is supported; `blockOn(task)` is the primary way to wait when no value is produced.
- Tasks run on the runtime worker pool; they are not implemented as one OS thread per task.
- `async` lambdas and `async @native fn` are not supported in v1.

---

## Lambda Functions

Anonymous functions can be defined inline.

### Syntax

```ry
# Single expression (return type inferred from expression)
 (paramName: type, ...) => expression

# Parameter type can be omitted (defaults to any)
 (paramName, ...) => expression

# Single-parameter shorthand (paren-less, untyped, single expression)
 paramName => expression

# Multi-line block
(paramName: type, ...):
    # multiple statements
    return value

# With explicit return type (optional)
 (paramName: type, ...) -> returnType => expression

# Untyped params with explicit return type annotation
# Params default to `any`; the body result is unwrapped to the declared type at runtime.
 (paramName, ...) -> returnType => expression
```

### Example

```ry
double = (x: int) => x * 2
result = double(5)   # 10

add = (a: int, b: int) => a + b
sum = add(3, 4)      # 7

# Single-parameter shorthand: paren-less, untyped form
inc = x => x + 1
xs = [1, 2, 3, 4]
big = xs.filter(x => x > 2)   # [3, 4]

# Multi-line lambda
abs = (x: int):
    if x < 0:
        return -x
    return x
```

### Limitations of the paren-less shorthand

The bare `paramName => expression` form is restricted to the simplest case to keep the
grammar unambiguous:

- **Exactly one parameter** — `s, t => s + t` is rejected (multi-arg conflicts with tuple destructuring).
- **No parameter type annotation** — `s: str => s` is rejected (conflicts with module-global typed declaration syntax).
- **Single-expression body only** — block-style bodies require parens: `(s):\n    return s`.
- The parameter type defaults to `any` (same as `(s) => ...`); add parens to declare a concrete type.

---

## Closures

Lambda functions **capture by value** the variables from the outer scope at the time of definition. The closure receives its own independent copy at capture time, and the captured variable cannot be reassigned inside the closure.

### Outer changes do not affect the closure

Because the closure holds a copy, reassigning the original variable after the closure is defined has no effect on the captured value:

```ry
base = 10
addBase = (x: int) => x + base   # Captures base by value (copy of 10)

base = 99          # Does not affect the captured value
r = addBase(5)   # 15 (uses base = 10 from capture time)
```

### Captured variables are effectively final

Captured variables **cannot be reassigned** inside the closure. Attempting to do so produces a compile error:

```ry
counter = 0
inc = ():
    counter += 1    # Compile error: cannot modify captured variable 'counter' inside closure

inc()
```

**Field assignment on captured records is allowed**, since it modifies the copy's internal state rather than reassigning the variable itself:

```ry
record Point:
    x: int
    y: int

p = Point(0, 0)
move = ():
    p.x = p.x + 1    # OK — modifies the captured copy's field
```

> **Note**: Field modifications apply to the closure's copy only — the outer variable is unaffected.

### Capture Rules

| Item | Details |
|---|---|
| Capture method | Capture by value (copy) |
| Capture timing | At lambda definition time |
| Reassignment of captured variables | Not allowed (compile error) |
| Field assignment on captured records | Allowed (modifies the copy only) |
| Effect of outer variable changes | None (the closure holds its own copy) |

> **Note for Python/JavaScript users**: In JavaScript, closures capture variables by reference, so changes to a captured variable are reflected outside the closure. In Python, closures can access outer variables, and rebinding an outer name (such as `counter += x`) requires declaring it `nonlocal`. In Ry, closures always capture by value, and captured variables are effectively final — they cannot be reassigned inside the closure. This is intentional — it ensures safety and predictability, especially in concurrent or higher-order contexts.

---

## Function Type

A type for treating functions as values.

### Syntax

```ry
fn(paramType1, paramType2, ...) -> returnType
```

### Example

```ry
f: fn(int) -> int = (x: int) => x * 2
g: fn(int, int) -> int = (a: int, b: int) => a + b

fn apply(func: fn(int) -> int, x: int) -> int:
    return func(x)

result = apply(f, 5)   # 10
```

### String Representation

`print()`, `toStr()`, and f-string interpolation all produce `"<closure>"` for function values:

```ry
f = (x: int) => x + 1
print(f)              # <closure>
s = toStr(f)         # "<closure>"
msg = f"fn={f}"       # "fn=<closure>"
```

> **Note**: Equality comparison (`==` / `!=`) between closures is not supported and produces a compile-time error.

---

## Higher-Order Functions

Functions can accept functions as arguments or return them as values.

```ry
fn mapList(xs: List<int>, f: fn(int) -> int) -> List<int>:
    result: List<int> = []
    for x in xs:
        result += [f(x)]
    return result

doubled = mapList([1, 2, 3], (x: int) => x * 2)
# [2, 4, 6]
```

---

## Generic Functions

Functions can have type parameters, enabling type-safe reuse without code duplication.

### Syntax

```ry
fn name<T, U>(param1: T, param2: U) -> T:
    # body using T, U as types
```

### Example

```ry
fn identity<T>(x: T) -> T:
    return x

# Explicit type argument
result = identity[int](42)      # 42
result = identity[str]("hello") # "hello"

# Type inference (type argument deduced from actual argument)
result = identity(42)           # T = int, result = 42
result = identity("hello")     # T = str, result = "hello"
```

### Multiple Type Parameters

```ry
fn pickFirst<T, U>(a: T, b: U) -> T:
    return a

result = pickFirst(1, "x")       # T = int, U = str, result = 1
result = pickFirst("hello", 42)  # T = str, U = int, result = "hello"
```

### Type Parameters Inside Container Types

Type parameters can appear inside generic container types (`List<T>`,
`Map<K, V>`, `Set<T>`), tuples `(T, T)`, and function types
`fn(T) -> T`. Inference walks the declared parameter type
structurally against the actual argument, so explicit type annotations
are not required when the shape is unambiguous.

```ry
fn firstOf<T>(xs: List<T>) -> T:
    return xs[0]

firstOf([1, 2, 3])            # T = int  → 1
firstOf(["hello", "world"])   # T = str  → "hello"
firstOf([[1, 2], [3, 4]])     # T = List<int>  → [1, 2]

fn mapLookup<K, V>(m: Map<K, V>, k: K) -> V:
    return m[k]

mapLookup({1: "a", 2: "b"}, 1)     # K = int, V = str → "a"
mapLookup({"x": 10, "y": 20}, "y") # K = str, V = int → 20

fn pairFirst<T>(p: (T, T)) -> T:
    return p.0

pairFirst((42, 7))      # T = int → 42
pairFirst(("a", "b"))   # T = str → "a"
```

A type parameter referenced across multiple parameter positions is
unified — both occurrences must resolve to the same concrete type:

```ry
fn applyList<T>(xs: List<T>, f: fn(T) -> T) -> T:
    return f(xs[0])

applyList([10, 20, 30], (x: int) => x + 1)  # T = int → 11
```

If inference cannot determine a type parameter (for example, from an
empty container literal), use the explicit `name[Type](args)` syntax:

```ry
firstOf[int]([])   # empty list: tell the compiler T = int explicitly
```

Conflicting inferences across arguments produce a clear compile error
naming the type parameter and function rather than an opaque type
mismatch:

```ry
fn same<T>(a: T, b: T) -> T:
    return a

same(1, "x")  # error: conflicting type inference for 'T' in call to 'same'
```

### Type Constraints (Bounds)

Type parameters can be constrained with record types using `: RecordName` syntax. The concrete type must be the bound type itself or a subtype of it.

```ry
record Animal:
    name: str
    legs: int

record Dog < Animal:
    breed: str

fn getName<T: Animal>(a: T) -> str:
    return a.name

getName(Dog("Rex", 4, "Lab"))  # OK — Dog is a subtype of Animal
getName(Animal("Cat", 4))      # OK — exact type match
```

Type aliases that resolve to a record type can be used either as the bound or as the concrete type argument; the compiler resolves aliases before checking the constraint.

```ry
type AnimalAlias = Animal

fn describe<T: AnimalAlias>(a: T) -> str:
    return a.name

describe(Dog("Rex", 4, "Lab"))  # OK — AnimalAlias resolves to Animal
```

Bounded and unbounded type parameters can be mixed:

```ry
fn pairName<T: Animal, U>(a: T, x: U) -> str:
    return a.name
```

### Overloading by Argument Type

Multiple generic functions with the same name may be declared as long as their parameter signatures differ in arity or in concrete argument types. At each call site the compiler picks the matching overload using a two-pass resolution:

1. **Pass 1 — exact match.** Every parameter must equal the inferred argument type. Type variables match any single concrete type.
2. **Pass 2 — widening fallback** (only when Pass 1 yields zero matches). Top-level numeric parameters accept the same widening conversions as `@native` dispatch: `u8 → int`, `u8 → float`, and `int → float`. Nested element positions inside `List<T>` / `Map<K, V>` / `Set<T>` / tuples / function types stay exact regardless of pass.

```ry
fn label<T>(kind: int, x: T) -> str:
    return "intKind"

fn label<T>(kind: str, x: T) -> str:
    return "strKind"

label(7, 42)         # "intKind"  — int parameter wins exact match
label("hi", 3.14)    # "strKind"  — str parameter wins exact match
```

```ry
fn first<T>(xs: List<T>) -> str:
    return "list"

fn first<T>(s: Set<T>) -> str:
    return "set"

first([1, 2, 3])     # "list"  — List<int> shape matches the first overload
first({1, 2, 3})     # "set"   — Set<int> shape matches the second overload
```

If exactly one template matches in a given pass, that overload is selected. If more than one template matches in the same pass the compiler reports an ambiguous-overload error naming the function. If no template matches in either pass and multiple templates are declared, a no-matching-overload error is reported.

Two templates whose parameter signatures normalize to the same shape after renaming type variables to positional `__T0`, `__T1`, ... are rejected at declaration time as duplicates:

```ry
fn id<T>(x: T) -> T:    # OK
    return x

fn id<U>(x: U) -> U:    # error: duplicate generic function declaration 'id'
    return x
```

The check is structural — both `<T>(x: T)` and `<U>(x: U)` normalize to `__T0`, so the compiler treats them as the same signature even though the type-variable spellings differ.

Out of scope for now: overloading non-generic functions, overloading record methods, and dispatch driven by return type.

### How It Works

Generic functions use **monomorphization**: a specialized version of the function is generated for each unique combination of type arguments. The same instantiation is cached and reused across multiple calls. When type constraints are present, they are validated at instantiation time.

For overloaded generic functions, the monomorphized symbol name includes a per-template fingerprint that normalizes type variables to positional `__T0`, `__T1`, ..., so two templates that resolve to the same surface type arguments (for example `first<T>(xs: List<T>)` and `first<T>(s: Set<T>)`, both instantiated with `T = int`) are compiled into distinct IR functions and cached independently.

---

## UFCS (Uniform Function Call Syntax)

`a.f(b)` can be used to call `f(a, b)`. Useful for method chaining.

### Syntax

```ry
# Normal call
f(a, b)

# UFCS call (equivalent)
a.f(b)
```

### Chaining

```ry
fn double(x: int) -> int:
    return x * 2

fn addOne(x: int) -> int:
    return x + 1

result = 5.double().addOne()   # double(5) -> 10, addOne(10) -> 11
```

### Mixing with Field Access

Field access (`.field`) and UFCS (`.method()`) use the same dot notation but are distinguished by the presence of arguments.

```ry
p = Point(3, 4)
length = p.x.toFloat()   # Field access + UFCS
```

---

## Operator Overloading

You can define operator behavior for user-defined types.

### Syntax

```ry
# Binary operator (2 parameters)
fn operator<op>(a: type, b: type) -> returnType:
    ...

# Unary operator (1 parameter)
fn operator<op>(a: type) -> returnType:
    ...
```

Symbolic operators (`+`, `-`, `*`, `==`, `+=`, etc.) must be written directly after `operator` with no whitespace: `operator+`, not `operator +`. Keyword operators (`in`, `as`, `and`, `or`, `not`) and bracket/call operators (`[]`, `[]=`, `()`) are written with the natural word/token boundary.

### Overloadable Operators

| Category | Operators |
|---|---|
| Arithmetic (binary) | `+` `-` `*` `/` `%` `**` `//` |
| Comparison (binary) | `==` `!=` `<` `<=` `>` `>=` |
| Bitwise (binary) | `&` `\|` `^` `<<` `>>` `>>>` |
| Logical (binary) | `and` `or` |
| Membership | `in` |
| Subscript | `[]` (read), `[]=` (write) |
| Call | `()` |
| Cast | `as` |
| Unary | `-` `~` `not` |
| Compound assignment | `+=` `-=` `*=` `/=` `%=` `//=` `**=` `&=` `\|=` `^=` `<<=` `>>=` |

### Return Type Constraints

Comparison, logical, and membership operators must return `bool`:

| Category | Operators | Required Return Type |
|---|---|---|
| Comparison | `==` `!=` `<` `<=` `>` `>=` | `bool` |
| Logical | `and` `or` `not` | `bool` |
| Membership | `in` | `bool` |
| Cast | `as` | Required (target type) |

```ry
# OK
fn operator==(a: Vec2, b: Vec2) -> bool:
    return a.x == b.x and a.y == b.y

# Error: comparison operator '==' must return 'bool', but returns 'int'
fn operator==(a: Vec2, b: Vec2) -> int:
    return 42
```

Arithmetic and bitwise operators have no return type constraint.

### Distinguishing Binary and Unary

Distinguished by the number of parameters.

```ry
record Vec2:
    x: float
    y: float

# Binary +
fn operator+(a: Vec2, b: Vec2) -> Vec2:
    return Vec2(a.x + b.x, a.y + b.y)

# Unary -
fn operator-(v: Vec2) -> Vec2:
    return Vec2(-v.x, -v.y)

# Comparison
fn operator==(a: Vec2, b: Vec2) -> bool:
    return a.x == b.x and a.y == b.y

v1 = Vec2(1.0, 2.0)
v2 = Vec2(3.0, 4.0)
v3 = v1 + v2    # Vec2(4.0, 6.0)
v4 = -v1        # Vec2(-1.0, -2.0)
```

---

## Checked/Saturating Arithmetic

> **See also**: [Builtins — Checked Arithmetic](builtins.md#checked-arithmetic) for the full API reference. This section provides a function-context summary and examples.

Built-in functions for explicit overflow control on integer types (`int`, `i8`..`i64`, `u8`..`u64`). Both arguments must be the same type.

| Function | Returns | Behavior |
|----------|---------|----------|
| `checkedAdd(a, b)` | `Result<T, Error>` | Returns `Err` on overflow |
| `checkedSub(a, b)` | `Result<T, Error>` | Returns `Err` on underflow |
| `checkedMul(a, b)` | `Result<T, Error>` | Returns `Err` on overflow |
| `saturatingAdd(a, b)` | `T` | Clamps to type min/max on overflow |
| `saturatingSub(a, b)` | `T` | Clamps to type min/max on underflow |
| `saturatingMul(a, b)` | `T` | Clamps to type min/max on overflow |
| `wrappingAdd(a, b)` | `T` | Explicit wrapping on overflow |
| `wrappingSub(a, b)` | `T` | Explicit wrapping on underflow |
| `wrappingMul(a, b)` | `T` | Explicit wrapping on overflow |

```ry
# int: trap-free checked arithmetic (default + on int traps on overflow)
r = checkedAdd(9223372036854775807, 1)
case r:
  Ok(v):
    print(v)
  Err(e):
    print("overflow!")   # prints "overflow!"

# Checked: returns Result, use match or ? to handle
r = checkedAdd(2147483647i32, 1i32)
case r:
  Ok(v):
    print(v)
  Err(e):
    print("overflow!")   # prints "overflow!"

# Saturating: clamps to bounds
v = saturatingAdd(2147483647i32, 100i32)
print(v as int)   # 2147483647

# Wrapping: self-documenting wrapping behavior
v = wrappingAdd(2147483647i32, 1i32)
print(v as int)   # -2147483648
```

> **Note**: These functions do not support floating-point types (`f32`). The default `+`, `-`, `*` operators on `int` trap on overflow; on low-level integers they use wrapping behavior (two's complement for signed, modular for unsigned).
