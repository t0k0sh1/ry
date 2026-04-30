# Control Flow Reference

## if / else

### Statement Syntax

```ry
if condition:
    # then block
else:
    # else block (optional)
```

### Expression Forms

`if` can also be used as an expression that produces a value. Two forms are supported:

**Single-expression form** (`=>`):

```ry
x = if condition => trueValue else falseValue
```

Examples:

```ry
absVal = if x > 0 => x else -x
label = if score >= 90 => "A" else "B"
```

The `else` branch in the single-expression form takes a value directly (without `=>`). Both branches must produce the same type, and `else` is required.

**Colon form** (`:`):

```ry
x = if condition: trueValue else: falseValue
```

Inline and block branches can be mixed:

```ry
x = if condition: trueValue else:
    (computeOther())
```

Or both branches can use blocks:

```ry
x = if condition:
    (computeSomething())
else:
    (computeOther())
```

In the colon form, each branch may be either a same-line expression or an indented block. Indented branches must end with an expression statement (tail-expression semantics). The `else` branch is required, and both branches must produce the same type.

For multi-branch conditionals with values, use `case:` instead (see below).

### Condition Types

| Type | Falsy Value | Truthy Value |
|---|---|---|
| `bool` | `false` | `true` |
| `int` | `0` | non-zero |
| `float` | `0.0` | non-zero |

Only `bool`, integer, and `float` types may appear in a condition. `str`,
`List`, `Map`, `Set`, iterators, closures, records, `Option`, and `Result`
cannot be used directly as conditions. For collections and strings, write
the length check explicitly:

```ry
xs = [1, 2, 3]
# ✗ error: value of this type cannot be used as a boolean condition
# if xs:
#     print("non-empty")
# ✓ explicit length check
if len(xs) > 0:
    print("non-empty")
# ✓ equivalent using isEmpty
if not isEmpty(xs):
    print("non-empty")
```

For `Option` and `Result`, pattern-match the variants explicitly with
`case` instead of using them as conditions. These rules apply equally to
`while`, `case` arms, and the unary `not` operator.

### Example

```ry
x = 10

if x > 5:
    print("big")
else:
    print("small or equal")
```

### Scope Rules

- Each `if` / `else` block has its own independent block scope.
- Variables declared inside a block are not accessible outside the block.

```ry
if true:
    y = 42
# y is not accessible here
```

---

## while

### Syntax

```ry
while condition:
    # loop body
```

Repeats the loop body while the condition is `true`.

### Example

```ry
i = 0
while i < 5:
    print(i)
    i += 1
```

### Combining with break / continue

```ry
i = 0
while true:
    if i >= 3:
        break
    i += 1
```

---

## for

### Syntax

```ry
# List / set iteration
for x in iterableExpr:
    # x is assigned each element

# range (starting from 0)
for i in range(n):
    # i = 0, 1, ..., n-1

# range (with start and end)
for i in range(start, end):
    # i = start, start+1, ..., end-1

# range (with step)
for i in range(start, end, step):
    # i = start, start+step, start+2*step, ...
```

### String Iteration

A `for` loop over a `str` yields each **Unicode code point** as a single-character `str`. Multi-byte UTF-8 sequences (including CJK characters and emoji) are decoded correctly; bytes within a multi-byte character are never split.

This is **code-point** iteration, not **grapheme-cluster** iteration: user-perceived characters that span multiple code points — combining-mark sequences (e.g., base letter + U+0301) and ZWJ emoji sequences (e.g., family or skin-tone compositions) — are yielded as several iterations, one per code point. If you need grapheme-cluster-aware iteration, decompose the string with a future segmentation helper rather than relying on `for c in s:`.

```ry
for c in "hello":
    print(c)               # h, e, l, l, o

for c in "こんにちは":
    print(c)               # こ, ん, に, ち, は  (not individual bytes)

for c in "a🙂b":
    print(c)               # a, 🙂, b
```

The loop variable is typed as `str`, so you can pass it to other string functions:

```ry
for c in "abc":
    print(toUpper(c))     # A, B, C
```

Iterating an empty string runs the loop body zero times. `enumerate` and `zip` also accept `str` arguments and yield the same code-point units:

```ry
for i, c in enumerate("abc"):
    print(i, c)

for a, b in zip("abc", "xyz"):
    print(a + b)           # ax, by, cz
```

### Map Key-Value Iteration

```ry
for k, v in mapExpr:
    # k is the key, v is the value for each entry
```

### Tuple Destructuring

When iterating over a list or set of tuples, you can destructure into N variables matching the tuple's element count. Use `_` to discard a value.

```ry
xs = [10, 20, 30]

for i, x in enumerate(xs):
    print(f"{i}: {x}")    # 0: 10, 1: 20, 2: 30

for a, b in zip([1, 2], [10, 20]):
    print(a + b)          # 11, 22

for _, x in enumerate(xs):
    print(x)              # index discarded

# N-element destructuring (3+ variables)
triples = [(1, 2, 3), (4, 5, 6)]
for a, b, c in triples:
    print(a + b + c)      # 6, 15

for a, _, c in triples:
    print(a + c)          # 4, 10 (middle element discarded)

# Nested tuple destructuring also works
items = [("a", 1), ("b", 2), ("c", 3)]
for i, (k, v) in enumerate(items):
    print(f"{i}: {k}={v}")  # 0: a=1, 1: b=2, 2: c=3

# Sets of tuples destructure the same way (unordered traversal)
pairs: Set<(int, int)> = {(1, 2), (3, 4)}
for a, b in pairs:
    print(a + b)          # 3, 7 (order unspecified)
```

Statement-level destructuring assignment is also available and accepts both a bare and a parenthesized LHS. See [directives.md#const](directives.md) for the `@const` variant.

```ry
a, b = (10, 20)           # bare form
(c, d) = (30, 40)         # parenthesized form — same meaning
(_, e) = (50, 60)         # discard first component
```

### Range Operator (`..`)

The `..` operator creates an inclusive integer range. `1 .. 5` produces `[1, 2, 3, 4, 5]`.

```ry
for i in 1 .. 5:
    print(i)     # 1 2 3 4 5
```

### Example

```ry
xs = [10, 20, 30]
for x in xs:
    print(x)

s = {1, 2, 3}
for x in s:
    print(x)

for i in range(5):
    print(i)     # 0 1 2 3 4

for i in range(2, 6):
    print(i)     # 2 3 4 5

for i in range(0, 10, 2):
    print(i)     # 0 2 4 6 8

for i in range(10, 0, -3):
    print(i)     # 10 7 4 1

# Map iteration
m = {"a": 1, "b": 2}
for k, v in m:
    print(k)
    print(v)

# Range operator
for i in 1 .. 3:
    print(i)     # 1 2 3
```

### Mutation during Iteration

Mutating the iterable from inside the loop body is allowed and memory-safe.
The loop observes the collection as it was **at loop entry**; elements added
after the loop starts are not visited, and elements removed are still visited.

```ry
ys = [10, 20, 30]
for y in ys:
    append!(ys, y + 100)   # grows ys, but the loop still sees only 3 elements
# ys == [10, 20, 30, 110, 120, 130]
```

This applies to lists, sets, and maps:
- **`append!` / `add`**: new elements are not visited.
- **`remove`**: removed elements are still visited (snapshot was taken at entry).
- **Map insert / remove**: only the keys present at loop entry are iterated.

To explicitly iterate over a growing list, use a `while` loop that re-checks
the length each iteration:

```ry
i = 0
while i < len(xs):
    # xs[i] — observes elements appended after the loop starts
    i += 1
```

---

## async / await

`async fn` declares a function that runs concurrently. Calling an `async fn` returns `Task<T>`. Use `await` inside another `async fn` or `blockOn()` from synchronous context to wait for the result.

```ry
async fn add(a: int, b: int) -> int:
    return a + b

# From synchronous context, use blockOn()
t: Task<int> = add(20, 22)
print(blockOn(t))                  # 42
print(blockOn(add(1, 2)))          # 3

# Inside async fn, use await
async fn doubleAdd(a: int, b: int) -> int:
    result = await add(a, b)
    return result * 2
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

## `@parallel for`

`@parallel` can be attached only to counted `for` loops over `range(...)` or integer `..` ranges. The loop body runs in parallel chunks on the runtime worker pool.

```ry
@parallel
for i in range(8):
    print(i)
```

### Constraints

- Only `range(...)` and integer `..` loops are supported.
- Destructuring iteration is not supported.
- Assigning to outer mutable bindings is rejected.
- `break` and `continue` are rejected.
- Indexed assignment and field assignment inside the loop body are rejected in v1.
- Nested function definitions (`fn` statements) inside the body are not allowed.

Use `availableParallelism()` to inspect the runtime worker count.

---

## break

- Immediately exits the innermost loop (`while` or `for`).
- Using it outside a loop causes a compile error.

```ry
for i in range(10):
    if i == 5:
        break    # Exits when i == 5
    print(i)     # 0 1 2 3 4
```

### Error Example

```ry
# break outside a loop is a compile error
break   # Error: break outside loop
```

---

## continue

- Ends the current iteration of the innermost loop and skips to the next iteration.
- Using it outside a loop causes a compile error.

```ry
for i in range(5):
    if i == 2:
        continue   # Skip i == 2
    print(i)       # 0 1 3 4
```

---

## `...` (Ellipsis)

- A no-op statement that does nothing. Used as a placeholder for empty blocks.
- Can be used in any block: function body, `if`/`else`, `while`, `for`, `case` arm, etc.

```ry
fn notYet():
    ...

if true:
    ...
else:
    ...
```

---

## case

`case` unifies multi-branch conditional flow (formerly `when`) and pattern
matching (formerly `match`) into a single construct. Two forms are supported:

- `case:` — no subject, each arm is a condition expression (replaces `when:`)
- `case <expr>:` — with a subject, each arm is a pattern (replaces `match`)

Both forms support:
- block arms (`pattern:` followed by an indented body)
- expression arms (`pattern : valueExpression` on one line)

> **Note**: The `when` and `match` keywords were removed in favor of the
> unified `case` construct. Legacy Ry code using `when` / `match` must be
> migrated.

### case without subject

Use `case:` for multi-branch conditional flow without a subject value.

#### Syntax

```ry
case:
    condition:
        # body
    condition:
        # body
    _:
        # fallback body
```

#### Example

```ry
x = 0

case:
    x > 0:
        print("positive")
    x < 0:
        print("negative")
    _:
        print("zero")
```

The arms are evaluated from top to bottom and the first arm whose condition
is truthy is executed. The wildcard arm `_:` is optional for statements.

For the expression form of `case:`, see the Expression Forms section below.

---

## case with subject (pattern matching)

### Syntax

```ry
case expression:
    pattern:
        # body
    pattern if guardCondition:
        # guarded body
    _:
        # wildcard (matches anything)
```

### Pattern Types

| Pattern | Example | Description |
|----------|-----|------|
| Wildcard | `_` | Matches anything |
| Literal | `0`, `"hello"`, `true` | Equality comparison |
| Variable binding | `n` | Matches anything and binds to a variable |
| enum variant | `Color::Red` | Compares enum tag (simple enum) |
| ADT enum variant | `Shape::Circle(r)`, `Event::Click((0, y))` | Matches an enum variant with associated data; each position may be a variable, literal, wildcard, or tuple pattern |
| `Some(x)` | `Some(v)` | When Option has a value, binds the inner value |
| `None` | `None` | When Option has no value |
| `Ok(x)` | `Ok(v)` | When Result is Ok, binds the inner value |
| `Err(x)` | `Err(e)` | When Result is Err, binds the error value |
| Tuple pattern | `(a, b)`, `(1, n)` | Matches a tuple by element; binds, tests literals, or ignores (`_`) each position |
| Record pattern | `Point(a, b)`, `Point(0, y)` | Matches a record by positional fields; binds, tests literals, or ignores (`_`) each field |
| OR pattern | `1 \| 2 \| 3` | Matches if any alternative matches |

### Guard Clause

A guard condition can be specified in the form `pattern if condition:`. The arm is executed only when the pattern matches and the guard condition is true.

### OR Pattern

Multiple patterns can be combined with `|` to match any of them. Variable bindings (`n`, `Some(x)`, `Ok(v)`, `Err(e)`) are not allowed in OR patterns.

```ry
case x:
    1 | 2 | 3:
        print("small")
    _:
        print("other")

# Enum OR pattern
case color:
    Color::Red | Color::Blue:
        print("warm or cool")
    Color::Green:
        print("green")
```

### Exhaustiveness Checking

- enum types: Must cover all variants or include `_`. OR patterns count each alternative individually.
- Option types: Must cover both `Some` and `None` or include `_`.
- bool type: Must cover both `true` and `false` or include `_`.
- int / float / str literals: `_` is required.
- Guarded arms do not count toward exhaustiveness.

### Example

```ry
# enum pattern match
enum Color:
    Red
    Green
    Blue

case color:
    Color::Red:
        print("red")
    Color::Green:
        print("green")
    Color::Blue:
        print("blue")

# Option pattern match
x: Option<int> = Some(42)
case x:
    Some(v):
        print(v)
    None:
        print("nothing")

# Result pattern match
fn divide(a: int, b: int) -> Result<int, Error>:
    if b == 0:
        return Err(Error("division by zero"))
    return Ok(a // b)

case divide(10, 2):
    Ok(v):
        print(v)         # 5
    Err(e):
        print(e.message)

# Literal pattern match
case x:
    0:
        print("zero")
    1:
        print("one")
    _:
        print("other")

# Guard clause
case x:
    n if n > 0:
        print("positive")
    n if n < 0:
        print("negative")
    _:
        print("zero")
```

### ADT Enum Pattern Matching

When an enum variant carries associated data, use a binding pattern to extract the value(s).

```ry
enum Shape:
    Circle(float)
    Rectangle(float, float)
    Point

s = Shape::Circle(3.14)
case s:
    Shape::Circle(r):
        print(r)        # 3.14
    Shape::Rectangle(w, h):
        print(w)
        print(h)
    Shape::Point:
        print("point")
```

Multi-field variants bind each field to a separate name in declaration order.

Each binding position in a constructor pattern may be any pattern, not only a plain variable name. You can use:

- **A variable** (`r`, `x`, `y`) — binds the field value to that name.
- **A literal** (`42`, `0`) — tests that the field equals the literal; the arm is taken only if all fields match.
- **A wildcard** (`_`) — ignores the field value.
- **A tuple pattern** (`(x, y)`) — when a variant has multiple fields, a single tuple pattern whose element count equals the field count is unwrapped and matched field-by-field.

```ry
enum Event:
    Click(int, int)
    Key(str)

e = Event::Click(0, 0)

# Nested tuple literal — matches only when both fields are 0
case e:
    Event::Click((0, 0)):
        print("origin")
    _:
        print("other")

# Nested tuple variable binding — binds both fields
case e:
    Event::Click((x, y)):
        print(x)   # 0
        print(y)   # 0

# Mixed literal + variable — first field must be 0, second is bound
e2 = Event::Click(0, 7)
case e2:
    Event::Click((0, y)):
        print(y)   # 7

# Wildcard — ignore first field, bind second
case e2:
    Event::Click((_, y)):
        print(y)   # 7
```

### Tuple Pattern Matching

Tuple patterns destructure a tuple subject by element position. Each element may be a variable binding, a literal, or a wildcard (`_`). Nested patterns (e.g., `Some(v)` inside a tuple element) are also supported.

```ry
# Binding pattern — bind both elements
t = (10, 20)
case t:
    (x, y):
        print(x)   # 10
        print(y)   # 20

# Mixed literal + binding
point = (0, 99)
case point:
    (0, n):
        print(n)   # 99
    _:
        print("other")

# Wildcard
pair = (55, 77)
case pair:
    (_, second):
        print(second)  # 77

# Guard clause
case t:
    (a, b) if a > b:
        print("first bigger")
    (a, b):
        print("other")

# 1-tuple (trailing comma required)
single = (42,)
case single:
    (v,):
        print(v)   # 42

# Nested: Option inside a tuple
opt: Option<int> = Some(7)
pair2 = (opt, 0)
case pair2:
    (Some(v), _):
        print(v)   # 7
    (None, _):
        print("none")
```

**Exhaustiveness**: A tuple pattern where every element is a variable or `_` (irrefutable) is treated as exhaustive — no wildcard arm is required.

**Syntax rules**:

| Syntax | Meaning |
|--------|---------|
| `(a, b)` | 2-tuple pattern |
| `(v,)` | 1-tuple pattern — the trailing comma is required |
| `(p)` | Grouping — equivalent to just `p`; **not** a 1-tuple |
| `()` | Not supported (parse error) |

**Restrictions**: Variable bindings are not allowed inside OR patterns. `(1, x) | (2, y)` is rejected at parse time.

### Record Pattern Matching (Positional)

Record patterns destructure a record subject by positional field order. Each element may be a variable binding, a literal, or a wildcard (`_`). Nested patterns (including nested record patterns) are supported.

```ry
record Point:
    x: int
    y: int

p = Point(3, 4)

# Binding pattern — bind both fields positionally
case p:
    Point(a, b):
        print(a)   # 3
        print(b)   # 4

# Mixed literal + binding
case p:
    Point(0, y):
        print(y)   # only matches if x == 0
    _:
        print("other")

# Wildcard — ignore second field
case p:
    Point(x, _):
        print(x)   # 3

# Guard clause
case p:
    Point(a, b) if a > b:
        print("x bigger")
    Point(a, b):
        print("other")

# Nested: record inside a tuple
t = (p, 99)
case t:
    (Point(x, _), _):
        print(x)   # 3

# Nested: record inside another record
record Segment:
    start: Point
    endPt: Point

seg = Segment(Point(1, 2), Point(3, 4))
case seg:
    Segment(Point(x1, _), Point(x2, _)):
        print(x1)   # 1
        print(x2)   # 3
```

**Exhaustiveness**: Records have exactly one shape. A record pattern where every element is irrefutable (variable or `_`) is treated as exhaustive — no wildcard arm is required.

**Syntax rules**:

| Syntax | Meaning |
|--------|---------|
| `Point(a, b)` | Match a 2-field record; bind `a` and `b` |
| `Point(0, y)` | Match first field against literal 0, bind second to `y` |
| `Point(_, _)` | Match any record of type `Point`; bind nothing |
| `Point()` | Not supported (parse error — must have at least one element) |

**Restrictions**: Variable bindings are not allowed inside OR patterns. `Point(a, b) | Point(c, d)` is rejected at parse time.

**Arity check**: The number of pattern elements must exactly match the number of declared fields. A mismatch is reported at compile time.

### Expression Forms

Both `case:` and `case <expr>:` can be used as expressions by writing each arm as `patternOrCondition: valueExpression` on the same line. Each arm provides a single expression whose value becomes the result.

```ry
# case: expression (no subject)
label = case:
    x > 100 : "huge"
    x > 10  : "big"
    x > 0   : "small"
    _       : "non-positive"
```

Pattern-matching expression form:

#### Syntax

```ry
result = case expression:
    pattern : valueExpression
    pattern if guard : valueExpression
    _ : defaultValue
```

All patterns supported in `case:` statements are also supported in `case` expressions: literals, variable bindings, enums, ADT enums, `Some`/`None`, `Ok`/`Err`, tuple patterns, record patterns, OR patterns, guards, and wildcards.

`case` expressions must be exhaustive (same rules as `case:` statements).

#### Examples

```ry
# Option
value = case opt:
    Some(v) : v
    None    : 0

# Enum
label = case direction:
    Direction::North : "N"
    Direction::South : "S"
    Direction::East  : "E"
    Direction::West  : "W"

# Guard
grade = case score:
    n if n >= 90 : "A"
    n if n >= 80 : "B"
    _            : "F"

# OR pattern
kind = case x:
    1 | 2 | 3 : "small"
    _          : "large"

# ADT enum
area = case shape:
    Shape::Circle(r)  : 3.14 * r * r
    Shape::Rectangle(w, h) : w * h
    Shape::Point      : 0.0

# Tuple pattern
t = (3, 4)
sum = case t:
    (a, b) : a + b
    _ : 0
```

### Scope Rules

- Each `case` arm has its own block scope.
- Variables bound by variable binding patterns (`n`), `Some(x)`, `Ok(v)`, `Err(e)`, tuple patterns `(a, b)`, or record patterns `Point(a, b)` are only valid within that arm.

---

## Scope Rules

### Block Scope

- Each block of `if` / `else` / `while` / `for` / `case` has a block scope.
- Variables declared inside a block go out of scope when the block ends.

```ry
for i in range(3):
    tmp = i * 2
# tmp is not accessible here

if true:
    a = 1
# a is not accessible here
```

### Inner Scope Reassignment

- Assigning to a variable inside an inner scope modifies the outer variable (Python-style scoping).
- There is no shadowing — the inner assignment changes the same variable.

```ry
x = 10
if true:
    x = 99   # Modifies the outer x
    print(x)     # 99
print(x)         # 99
```
