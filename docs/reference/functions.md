[English](functions.md) | [日本語](../ja/reference/functions.md) | [繁體中文](../zh/reference/functions.md)

# Function Reference

## Function Definition Syntax

```python
function function_name(param_name: type, ...) -> return_type:
    # body
    return value
```

- Parameter types are optional. When omitted, the parameter is treated as `any` type.
- A trailing comma after the last parameter is allowed: `function f(a: int, b: int,) -> int:`.
- Return type is optional. When omitted, the return type is **inferred from the body** (both named functions and lambdas). If the function has no `return` statement, the return type is inferred as `Unit`. Use `-> any` explicitly for functions that should accept any return type.
- The body is an indented block.
- Functions with an explicit return type (other than `Unit` or `any`) must have a `return` statement on all control flow paths. The compiler reports an error if any path is missing a return.
- Functions can have `require` (precondition) and `ensure` (postcondition) clauses. See [Design by Contract](contracts.md).

> **Naming convention**: Function names and parameter names must use snake_case (e.g., `add`, `get_value`, `map_list`). The compiler enforces this convention.

```python
function add(a: int, b: int) -> int:
    return a + b

function greet(name: str) -> Unit:
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

```python
function no_return(x: int) -> Unit:  # Return type Unit (explicit)
    print(x)

function get_value() -> int:     # Return type int
    return 42

function identity(x) -> any:    # Parameter type any (omitted)
    return x
```

### Type Omission and `any`

When a parameter type annotation is omitted, the parameter is treated as `any` — a dynamic type that accepts any primitive value at runtime. This is similar to Python's untyped parameters.

```python
# All parameters default to any
function add(a, b):
    return a + b

add(1, 2)              # 3 (int + int)
add("hello", " world") # "hello world" (str + str)
add(1, 2.0)            # 3.0 (int + float)
```

You can also use `any` explicitly in type annotations:

```python
function identity(x: any) -> any:
    return x
```

### Return Type Inference

When the return type is omitted, it is inferred from the `return` statements in the body:

```python
function double(x: int):     # return type inferred as int
    return x * 2

function greet(name: str):   # return type inferred as Unit (no return)
    print("Hello, " + name)
```

To explicitly allow any return type, use `-> any`:

```python
function flexible(x: any) -> any:
    return x    # can return int, float, str, etc.
```

---

## Nested Functions

Functions can be defined inside other functions. A nested function is only visible within its enclosing function's scope — it cannot be called from outside.

```python
function outer() -> int:
    function helper() -> int:
        return 42
    return helper()

outer()     # 42
# helper()  # error: undefined function
```

Same-named nested functions in sibling scopes do not collide:

```python
function foo() -> int:
    function helper() -> int:
        return 1
    return helper()

function bar() -> int:
    function helper() -> int:
        return 2
    return helper()

foo()   # 1
bar()   # 2
```

Nested functions can be used as values and passed to higher-order functions. Mutual recursion between nested functions in the same scope also works (the compiler forward-declares them).

### Closure Capture

Nested named functions can capture variables from enclosing scopes, just like lambdas. When a nested function references an outer variable, it becomes a closure:

```python
function make_adder(base: int) -> function(int) -> int:
    function add(x: int) -> int:
        return x + base
    return add

add10 = make_adder(10)
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

```python
function factorial(n: int) -> int:
    if n <= 1:
        return 1
    return n * factorial(n - 1)
```

### Mutual Recursion

Functions can call each other regardless of definition order. The compiler forward-declares functions with explicit return types before processing function bodies — this applies both at the top level and inside another function body (nested functions) — provided all referenced types are already known (primitive types are always available; record/enum types must be defined earlier in the file).

```python
function is_even(n: int) -> bool:
    if n == 0:
        return true
    return is_odd(n - 1)       # calls is_odd defined below

function is_odd(n: int) -> bool:
    if n == 0:
        return false
    return is_even(n - 1)      # calls is_even defined above
```

**Requirements for forward references:**

- The function must have an **explicit return type** annotation (`-> type`). Functions with inferred return types cannot be forward-referenced.
- The function must be defined at the **top level** or inside another function body. Forward references work within the same scope level.
- All parameter and return types must be resolvable at the point of forward declaration (e.g., record types must be defined before the functions that use them).

### Top-Level Variables and `@const` in Function Bodies

Top-level `let` bindings and `@const` declarations are visible from any top-level function — including nested functions and lambdas inside those functions — as long as the declaration appears **textually before** the referencing function in the same source file.

```python
@const
PI: float = 3.14

@const
MAX_RETRIES: int = 5

counter: int = 0

function area(radius: float) -> float:
    return PI * radius * radius            # reads top-level @const

function clamp_retries(n: int) -> int:
    if n > MAX_RETRIES:
        return MAX_RETRIES
    return n

function bump():
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

```python
function sum_to(n: int, acc: int) -> int:
    if n <= 0:
        return acc
    return sum_to(n - 1, acc + n)    # tail call → optimized

sum_to(1000000, 0)    # works without stack overflow
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

```python
function area(side: int) -> int:
    return side * side

function area(w: int, h: int) -> int:
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

```python
function process(x: int) -> str:
    return "int"

function process(x) -> str:          # x: any
    return "any"

process(42)       # "int" — exact match (int) beats any
process("hello")  # "any" — no exact match for str, falls back to any
```

```python
function double(x: float) -> float:
    return x * 2.0

double(5)         # OK — int is implicitly widened to float, returns 10.0
```

---

## Default Arguments

Parameters can have default values, allowing callers to omit trailing arguments.

### Syntax

```python
function connect(host: str, port: int = 8080, timeout: int = 30):
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

```python
# Error: ambiguous overload
function calc(x: int, y: int = 0) -> int:
    return x + y
function calc(x: int) -> int:      # conflicts with calc(int) from above
    return x * 2
```

### Limitations

- Default arguments are not supported in **generic functions** or **lambda expressions**.

---

## Unit Type Functions

Functions without a return value return `Unit`. The return type can be omitted (inferred as `Unit`) or explicitly specified with `-> Unit`.

```python
function log(msg: str) -> Unit:
    print(msg)
```

---

## Tasks And Async Functions

`Task<T>` is the built-in handle type for concurrent work. `async function` returns `Task<T>`, `await` extracts `T` inside another `async function`, and `block_on(task)` blocks from synchronous context until the task completes.

```python
async function add(a: int, b: int) -> int:
    return a + b

# From synchronous context, use block_on()
t: Task<int> = add(20, 22)
print(block_on(t))                  # 42
block_on(add(1, 2))                 # waits and discards the result

# Inside async function, use await
async function double_add(a: int, b: int) -> int:
    return (await add(a, b)) * 2
```

### Rules

- `async function name(...) -> T:` is declared with the awaited result type `T`.
- Calling an `async function` immediately returns `Task<T>`.
- `await expr` requires `expr` to be `Task<T>` and produces `T`.
- `await` can only be used inside an `async function`. Use `block_on(task)` from synchronous context.
- `block_on(task)` blocks the current thread until the task completes and returns the result.
- `async function ... -> Unit` is supported; `block_on(task)` is the primary way to wait when no value is produced.
- Tasks run on the runtime worker pool; they are not implemented as one OS thread per task.
- `async` lambdas and `async @native function` are not supported in v1.

---

## Lambda Functions

Anonymous functions can be defined inline.

### Syntax

```python
# Single expression (return type inferred from expression)
 (param_name: type, ...) => expression

# Parameter type can be omitted (defaults to any)
 (param_name, ...) => expression

# Multi-line block
(param_name: type, ...):
    # multiple statements
    return value

# With explicit return type (optional)
 (param_name: type, ...) -> return_type => expression
```

### Example

```python
double = (x: int) => x * 2
result = double(5)   # 10

add = (a: int, b: int) => a + b
sum = add(3, 4)      # 7

# Multi-line lambda
abs = (x: int):
    if x < 0:
        return -x
    return x
```

---

## Closures

Lambda functions **capture by value** the variables from the outer scope at the time of definition. The closure receives its own independent copy at capture time, and the captured variable cannot be reassigned inside the closure.

### Outer changes do not affect the closure

Because the closure holds a copy, reassigning the original variable after the closure is defined has no effect on the captured value:

```python
base = 10
add_base = (x: int) => x + base   # Captures base by value (copy of 10)

base = 99          # Does not affect the captured value
r = add_base(5)   # 15 (uses base = 10 from capture time)
```

### Captured variables are effectively final

Captured variables **cannot be reassigned** inside the closure. Attempting to do so produces a compile error:

```python
counter = 0
inc = ():
    counter += 1    # Compile error: cannot modify captured variable 'counter' inside closure

inc()
```

**Field assignment on captured records is allowed**, since it modifies the copy's internal state rather than reassigning the variable itself:

```python
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

```python
function(param_type1, param_type2, ...) -> return_type
```

### Example

```python
f: function(int) -> int = (x: int) => x * 2
g: function(int, int) -> int = (a: int, b: int) => a + b

function apply(func: function(int) -> int, x: int) -> int:
    return func(x)

result = apply(f, 5)   # 10
```

### String Representation

`print()`, `to_str()`, and f-string interpolation all produce `"<closure>"` for function values:

```python
f = (x: int) => x + 1
print(f)              # <closure>
s = to_str(f)         # "<closure>"
msg = f"fn={f}"       # "fn=<closure>"
```

> **Note**: Equality comparison (`==` / `!=`) between closures is not supported and produces a compile-time error.

---

## Higher-Order Functions

Functions can accept functions as arguments or return them as values.

```python
function map_list(xs: List<int>, f: function(int) -> int) -> List<int>:
    result: List<int> = []
    for x in xs:
        result += [f(x)]
    return result

doubled = map_list([1, 2, 3], (x: int) => x * 2)
# [2, 4, 6]
```

---

## Generic Functions

Functions can have type parameters, enabling type-safe reuse without code duplication.

### Syntax

```python
function name<T, U>(param1: T, param2: U) -> T:
    # body using T, U as types
```

### Example

```python
function identity<T>(x: T) -> T:
    return x

# Explicit type argument
result = identity[int](42)      # 42
result = identity[str]("hello") # "hello"

# Type inference (type argument deduced from actual argument)
result = identity(42)           # T = int, result = 42
result = identity("hello")     # T = str, result = "hello"
```

### Multiple Type Parameters

```python
function pick_first<T, U>(a: T, b: U) -> T:
    return a

result = pick_first(1, "x")       # T = int, U = str, result = 1
result = pick_first("hello", 42)  # T = str, U = int, result = "hello"
```

### Type Parameters Inside Container Types

Type parameters can appear inside generic container types (`List<T>`,
`Map<K, V>`, `Set<T>`), tuples `(T, T)`, and function types
`function(T) -> T`. Inference walks the declared parameter type
structurally against the actual argument, so explicit type annotations
are not required when the shape is unambiguous.

```python
function first_of<T>(xs: List<T>) -> T:
    return xs[0]

first_of([1, 2, 3])            # T = int  → 1
first_of(["hello", "world"])   # T = str  → "hello"
first_of([[1, 2], [3, 4]])     # T = List<int>  → [1, 2]

function map_lookup<K, V>(m: Map<K, V>, k: K) -> V:
    return m[k]

map_lookup({1: "a", 2: "b"}, 1)     # K = int, V = str → "a"
map_lookup({"x": 10, "y": 20}, "y") # K = str, V = int → 20

function pair_first<T>(p: (T, T)) -> T:
    return p.0

pair_first((42, 7))      # T = int → 42
pair_first(("a", "b"))   # T = str → "a"
```

A type parameter referenced across multiple parameter positions is
unified — both occurrences must resolve to the same concrete type:

```python
function apply_list<T>(xs: List<T>, f: function(T) -> T) -> T:
    return f(xs[0])

apply_list([10, 20, 30], (x: int) => x + 1)  # T = int → 11
```

If inference cannot determine a type parameter (for example, from an
empty container literal), use the explicit `name[Type](args)` syntax:

```python
first_of[int]([])   # empty list: tell the compiler T = int explicitly
```

Conflicting inferences across arguments produce a clear compile error
naming the type parameter and function rather than an opaque type
mismatch:

```python
function same<T>(a: T, b: T) -> T:
    return a

same(1, "x")  # error: conflicting type inference for 'T' in call to 'same'
```

### Type Constraints (Bounds)

Type parameters can be constrained with record types using `: RecordName` syntax. The concrete type must be the bound type itself or a subtype of it.

```python
record Animal:
    name: str
    legs: int

record Dog < Animal:
    breed: str

function get_name<T: Animal>(a: T) -> str:
    return a.name

get_name(Dog("Rex", 4, "Lab"))  # OK — Dog is a subtype of Animal
get_name(Animal("Cat", 4))      # OK — exact type match
```

Bounded and unbounded type parameters can be mixed:

```python
function pair_name<T: Animal, U>(a: T, x: U) -> str:
    return a.name
```

### How It Works

Generic functions use **monomorphization**: a specialized version of the function is generated for each unique combination of type arguments. The same instantiation is cached and reused across multiple calls. When type constraints are present, they are validated at instantiation time.

---

## UFCS (Uniform Function Call Syntax)

`a.f(b)` can be used to call `f(a, b)`. Useful for method chaining.

### Syntax

```python
# Normal call
f(a, b)

# UFCS call (equivalent)
a.f(b)
```

### Chaining

```python
function double(x: int) -> int:
    return x * 2

function add_one(x: int) -> int:
    return x + 1

result = 5.double().add_one()   # double(5) -> 10, add_one(10) -> 11
```

### Mixing with Field Access

Field access (`.field`) and UFCS (`.method()`) use the same dot notation but are distinguished by the presence of arguments.

```python
p = Point(3, 4)
length = p.x.to_float()   # Field access + UFCS
```

---

## Operator Overloading

You can define operator behavior for user-defined types.

### Syntax

```python
# Binary operator (2 parameters)
function operator<op>(a: type, b: type) -> return_type:
    ...

# Unary operator (1 parameter)
function operator<op>(a: type) -> return_type:
    ...
```

### Overloadable Operators

| Category | Operators |
|---|---|
| Arithmetic (binary) | `+` `-` `*` `/` `%` `**` `//` |
| Comparison (binary) | `==` `!=` `<` `<=` `>` `>=` |
| Bitwise (binary) | `&` `\|` `^` `<<` `>>` |
| Logical (binary) | `and` `or` |
| Membership | `in` |
| Subscript | `[]` (read), `[]=` (write) |
| Call | `()` |
| Cast | `as` |
| Unary | `-` `~` `not` |

### Return Type Constraints

Comparison, logical, and membership operators must return `bool`:

| Category | Operators | Required Return Type |
|---|---|---|
| Comparison | `==` `!=` `<` `<=` `>` `>=` | `bool` |
| Logical | `and` `or` `not` | `bool` |
| Membership | `in` | `bool` |
| Cast | `as` | Required (target type) |

```python
# OK
function operator==(a: Vec2, b: Vec2) -> bool:
    return a.x == b.x and a.y == b.y

# Error: comparison operator '==' must return 'bool', but returns 'int'
function operator==(a: Vec2, b: Vec2) -> int:
    return 42
```

Arithmetic and bitwise operators have no return type constraint.

### Distinguishing Binary and Unary

Distinguished by the number of parameters.

```python
record Vec2:
    x: float
    y: float

# Binary +
function operator+(a: Vec2, b: Vec2) -> Vec2:
    return Vec2(a.x + b.x, a.y + b.y)

# Unary -
function operator-(v: Vec2) -> Vec2:
    return Vec2(-v.x, -v.y)

# Comparison
function operator==(a: Vec2, b: Vec2) -> bool:
    return a.x == b.x and a.y == b.y

v1 = Vec2(1.0, 2.0)
v2 = Vec2(3.0, 4.0)
v3 = v1 + v2    # Vec2(4.0, 6.0)
v4 = -v1        # Vec2(-1.0, -2.0)
```

---

## Checked/Saturating Arithmetic

Built-in functions for explicit overflow control on low-level integer types (`i8`, `i16`, `i32`, `i64`, `u8`, `u16`, `u32`, `u64`). Both arguments must be the same type.

| Function | Returns | Behavior |
|----------|---------|----------|
| `checked_add(a, b)` | `Result<T, Error>` | Returns `Err` on overflow |
| `checked_sub(a, b)` | `Result<T, Error>` | Returns `Err` on underflow |
| `checked_mul(a, b)` | `Result<T, Error>` | Returns `Err` on overflow |
| `saturating_add(a, b)` | `T` | Clamps to type min/max on overflow |
| `saturating_sub(a, b)` | `T` | Clamps to type min/max on underflow |
| `saturating_mul(a, b)` | `T` | Clamps to type min/max on overflow |
| `wrapping_add(a, b)` | `T` | Explicit wrapping (same as `+`) |
| `wrapping_sub(a, b)` | `T` | Explicit wrapping (same as `-`) |
| `wrapping_mul(a, b)` | `T` | Explicit wrapping (same as `*`) |

```python
# Checked: returns Result, use match or ? to handle
r = checked_add(2147483647i32, 1i32)
case r:
  Ok(v):
    print(v)
  Err(e):
    print("overflow!")   # prints "overflow!"

# Saturating: clamps to bounds
v = saturating_add(2147483647i32, 100i32)
print(v as int)   # 2147483647

# Wrapping: self-documenting wrapping behavior
v = wrapping_add(2147483647i32, 1i32)
print(v as int)   # -2147483648
```

> **Note**: These functions do not support floating-point types (`f32`) or the high-level `int` type. The default `+`, `-`, `*` operators on low-level integers use wrapping behavior (two's complement for signed, modular for unsigned).
