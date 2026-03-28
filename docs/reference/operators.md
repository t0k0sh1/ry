[English](operators.md) | [日本語](../ja/reference/operators.md) | [繁體中文](../zh/reference/operators.md)

# Operator Reference

## Precedence Table

Lower numbers indicate higher precedence (evaluated first).

| Precedence | Operator | Description | Associativity |
|---|---|---|---|
| 0 | `?` `!!` | Error propagation (postfix) | Left |
| 1 | `()` | Grouping | -- |
| 2 | `+x` `-x` `~x` | Unary plus, unary minus, bitwise NOT | Right |
| 3 | `**` | Exponentiation | Right |
| 3.5 | `as` | Type cast | Left |
| 4 | `*` `/` `%` `//` | Multiplication, division, modulo, integer division | Left |
| 5 | `+` `-` | Addition, subtraction | Left |
| 6 | `<<` `>>` `>>>` | Bit shift | Left |
| 7 | `&` | Bitwise AND | Left |
| 8 | `^` | Bitwise XOR | Left |
| 9 | `\|` | Bitwise OR | Left |
| 10 | `==` `!=` `<` `<=` `>` `>=` `in` `not in` | Comparison, membership | Left |
| 11 | `not` | Logical NOT | Right |
| 12 | `and` | Logical AND | Left |
| 13 | `or` | Logical OR | Left |
| 13.5 | `??` | Null coalescing | Left |
| 14 | `?:` | Ternary conditional | Right |

## Arithmetic Operators

| Operator | Description | Example |
|---|---|---|
| `+` | Addition / string concatenation | `1 + 2` -> `3`, `"a" + "b"` -> `"ab"` |
| `-` | Subtraction | `5 - 3` -> `2` |
| `*` | Multiplication / string repetition | `4 * 3` -> `12`, `"ab" * 3` -> `"ababab"` |
| `/` | Division (always float) | `7 / 2` -> `3.5` |
| `//` | Floor division (toward -∞) | `7 // 2` -> `3`, `-7 // 2` -> `-4` |
| `%` | Modulo | `7 % 3` -> `1` |
| `**` | Exponentiation (always float) | `2 ** 10` -> `1024.0` |
| `-x` | Unary minus | `-5`, `-3.14` |
| `+x` | Unary plus | `+5` (no sign change) |

```python
a = 10 // 3    # 3 (int)
b = 10 / 3     # 3.3333... (float)
c = 2 ** 8     # 256.0 (float)
s = "foo" + "bar"  # "foobar"
```

## Comparison Operators

All return `bool`.

| Operator | Description |
|---|---|
| `==` | Equal |
| `!=` | Not equal |
| `<` | Less than |
| `<=` | Less than or equal |
| `>` | Greater than |
| `>=` | Greater than or equal |

- Can be used with numeric types (int / float) and bool.
- `str` values are compared lexicographically (byte order).
- Record types support `==` and `!=` with auto-generated field-by-field comparison (see [Struct Reference](structs.md#comparison--)).
- The `in` operator is used for membership checks on sets, lists, and maps (`x in s`).
- The `not in` operator is the negation of `in` (`x not in s`).
- For maps, `in` checks whether the key exists.

```python
x = 3 < 5       # true
y = "abc" < "abd"  # true (lexicographic)
s = {1, 2, 3}
z = 2 in s      # true
w = 4 not in s  # true
xs = [1, 2, 3]
a = 2 in xs     # true (list linear search)
m = {"a": 1}
b = "a" in m    # true (map key lookup)
```

## Logical Operators

| Operator | Description | Type |
|---|---|---|
| `and` | Logical AND | `bool` x `bool` -> `bool` |
| `or` | Logical OR | `bool` x `bool` -> `bool` |
| `not` | Logical NOT | `bool` -> `bool` |

```python
a = true and false   # false
b = true or false    # true
c = not true         # false
```

## Bitwise Operators

Only available for `int` type. Applying to `float` or `bool` causes a compile error.

| Operator | Description | Example |
|---|---|---|
| `&` | Bitwise AND | `0b1100 & 0b1010` -> `0b1000` |
| `\|` | Bitwise OR | `0b1100 \| 0b1010` -> `0b1110` |
| `^` | Bitwise XOR | `0b1100 ^ 0b1010` -> `0b0110` |
| `~` | Bitwise NOT (unary) | `~0` -> `-1` |
| `<<` | Left shift | `1 << 4` -> `16` |
| `>>` | Arithmetic right shift | `16 >> 2` -> `4` |
| `>>>` | Logical right shift | `-1 >>> 1` -> `9223372036854775807` |

```python
flags = 0b0001 | 0b0010   # 3
masked = flags & 0b0011   # 3
shifted = 1 << 8          # 256
```

## Error Propagation Operator (`?` / `!!`)

The postfix `?` operator unwraps a `Result` value. If the value is `Ok(v)`, it evaluates to `v`. If the value is `Err(e)`, the enclosing function immediately returns `Err(e)`.

The `!!` operator is an alias for `?` with identical semantics. Both can be used interchangeably.

The enclosing function must have a `Result` return type.

```python
fn safe_divide(a: int, b: int) -> Result<int, Error>:
    if b == 0:
        return Err(Error("division by zero"))
    return Ok(a // b)

fn compute(a: int, b: int, c: int) -> Result<int, Error>:
    x = safe_divide(a, b)?    # returns Err early if b == 0
    y = safe_divide(x, c)!!
    return Ok(y + 1)
```

This is equivalent to the following `match` pattern, but much more concise:

```python
fn compute(a: int, b: int, c: int) -> Result<int, Error>:
    match safe_divide(a, b):
        case Ok(x):
            match safe_divide(x, c):
                case Ok(y):
                    return Ok(y + 1)
                case Err(e):
                    return Err(e)
        case Err(e):
            return Err(e)
```

---

## Ternary Conditional Operator

```python
x = condition ? true_value : false_value
```

Evaluates `condition`. If truthy, returns `true_value`; otherwise returns `false_value`. Both branches must have the same type. Right-associative, so nested ternaries associate from right to left.

```python
x = 3 > 2 ? 10 : 20     # 10
s = false ? "yes" : "no" # "no"

# Nested (right-associative)
y = true ? (false ? 1 : 2) : 3   # 2
```

---

## Range Operator

The `..` operator creates an inclusive integer range.

```python
xs = 1 .. 5    # [1, 2, 3, 4, 5]

for i in 1 .. 3:
    print(i)       # 1 2 3
```

The result is a `List<int>` containing all integers from the left operand to the right operand (inclusive).

---

## Null Coalescing Operator (`??`)

```python
x = option_val ?? default_val
```

If `option_val` is `Some(v)`, returns `v`. Otherwise returns `default_val`. The right-hand operand must have the same type as the inner type of the Option.

```python
a: int? = Some(10)
b: int? = none

print(a ?? 0)    # 10
print(b ?? 0)    # 0
```

---

## Compound Assignment Operators

Shorthand for updating a variable. `x op= y` is equivalent to `x = x op y`.

| Operator | Equivalent Expression |
|---|---|
| `x += y` | `x = x + y` |
| `x -= y` | `x = x - y` |
| `x *= y` | `x = x * y` |
| `x /= y` | `x = x / y` |
| `x %= y` | `x = x % y` |
| `x //= y` | `x = x // y` |
| `x **= y` | `x = x ** y` |
| `x &= y` | `x = x & y` |
| `x \|= y` | `x = x \| y` |
| `x ^= y` | `x = x ^ y` |
| `x <<= y` | `x = x << y` |
| `x >>= y` | `x = x >> y` |

```python
x = 10
x += 5    # x = 15
x -= 3    # x = 12
x *= 2    # x = 24
x //= 3  # x = 8
x &= 6   # x = 0
```

## Increment / Decrement Operators

Postfix-only, statement-level operators for incrementing or decrementing a variable by 1. These are desugared to `x = x + 1` and `x = x - 1` respectively.

| Operator | Equivalent Expression |
|---|---|
| `x++` | `x = x + 1` |
| `x--` | `x = x - 1` |

```python
count = 0
count++       # count = 1
count++       # count = 2
count--       # count = 1

f = 1.5
f++           # f = 2.5 (int 1 is promoted to float)
```

> **Note**: `++` / `--` can only be used as statements, not as expressions.
> `@const` variables cannot be incremented/decremented (immutability is enforced).

---

## Type Rules for Operations

| Operation | Left Type | Right Type | Result Type |
|---|---|---|---|
| `+ - *` | int | int | int |
| `+ - *` | float | int / float | float |
| `+ - *` | int | float | float |
| `/` | any numeric | any numeric | float |
| `//` | int | int | int |
| `//` | float or int (one is float) | -- | float |
| `**` | any numeric | any numeric | float |
| `%` | int | int | int |
| `%` | float or int (one is float) | -- | float |
| `+` | str | str | str |
| `== != < <= > >=` | numeric / bool / str | same type | bool |
| `*` | str | int | str |
| `in` | any | Set<T> / List<T> / Map<K, V> | bool |
| `not in` | any | Set<T> / List<T> / Map<K, V> | bool |
| `& \| ^ ~ << >> >>>` | int | int | int |
| `and or not` | bool | bool | bool |

## Operator Overloading

You can define operator behavior for user-defined types.

### Syntax

```python
# Binary operator (2 parameters)
fn operator+(a: MyType, b: MyType) -> MyType:
    ...

# Unary operator (1 parameter)
fn operator-(a: MyType) -> MyType:
    ...
```

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

Comparison and logical operators must return `bool`:

| Category | Operators | Required Return Type |
|---|---|---|
| Comparison | `==` `!=` `<` `<=` `>` `>=` | `bool` |
| Logical | `and` `or` `not` | `bool` |
| Membership | `in` | `bool` |
| Cast | `as` | Required (target type) |

```python
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

```python
# Binary -
fn operator-(a: Vec2, b: Vec2) -> Vec2:
    return Vec2(a.x - b.x, a.y - b.y)

# Unary -
fn operator-(v: Vec2) -> Vec2:
    return Vec2(-v.x, -v.y)
```

### Compound Assignment Operator Overloading

Compound assignment operators (`+=`, `-=`, etc.) can be independently overloaded. This enables in-place optimization for large data structures.

```python
record Matrix:
    data: List
    rows: int
    cols: int

fn operator+=(a: Matrix, b: Matrix) -> Matrix:
    for i in range(len(a.data)):
        a.data[i] = a.data[i] + b.data[i]
    return a
```

#### Resolution Priority

When `x += y` is evaluated:

1. If `operator+=` is defined for the types → call it directly
2. If `operator+=` is not defined but `operator+` is → fall back to `x = x + y`
3. If neither is defined (for non-builtin types) → compile error

```python
record Vec2:
    x: float
    y: float

fn operator+=(a: Vec2, b: Vec2) -> Vec2:
    return Vec2(a.x + b.x, a.y + b.y)

v = Vec2(1.0, 2.0)
v += Vec2(3.0, 4.0)  # calls operator+= directly
# v.x == 4.0, v.y == 6.0
```

Compound assignment operators require exactly 2 parameters and have no return type constraint.

### Subscript Operator Overloading

The `[]` (read) and `[]=` (write) operators enable custom subscript behavior for user-defined types. Multi-index access (e.g., `m[row, col]`) is supported.

```python
record Grid:
    a: int
    b: int
    c: int
    d: int

# Read: requires 2+ parameters (object + indices)
fn operator[](g: Grid, row: int, col: int) -> int:
    if row == 0 and col == 0:
        return g.a
    if row == 0 and col == 1:
        return g.b
    if row == 1 and col == 0:
        return g.c
    return g.d

# Write: requires 3+ parameters (object + indices + value)
fn operator[]=(g: Grid, row: int, col: int, value: int):
    ...

g = Grid(1, 2, 3, 4)
print(g[0, 1])    # 2
g[1, 0] = 99
```

User-defined subscript operators are tried first; if no match is found, built-in subscript behavior (for lists, maps, and arrays) is used as a fallback.

### Membership Operator Overloading

The `in` operator can be overloaded to define custom membership tests. Must return `bool`.

```python
record Range:
    lo: int
    hi: int

fn operator in(value: int, r: Range) -> bool:
    return value >= r.lo and value < r.hi

r = Range(1, 10)
print(5 in r)       # true
print(15 not in r)  # true
```

User-defined `in` operators are tried first; if no match is found, built-in behavior (for sets, maps, and lists) is used as a fallback. `not in` is automatically supported when `in` is defined.

### Call Operator Overloading

The `()` operator enables records to behave as callable objects. Requires at least 2 parameters (object + arguments).

```python
record Adder:
    base: int

fn operator()(a: Adder, x: int) -> int:
    return a.base + x

add5 = Adder(5)
print(add5(10))    # 15
```

When a variable holding a record value is called like a function, the compiler tries `operator()` overloads first. If no match is found, other call resolution strategies (functions, constructors, lambdas) take precedence.

### Cast Operator Overloading

The `as` operator can be overloaded to define custom type conversions. Takes exactly 1 parameter (the source value) and must specify a return type (the target type). Dispatch matches by source type and return type.

```python
record Celsius:
    value: int

record Fahrenheit:
    value: int

fn operator as(c: Celsius) -> Fahrenheit:
    return Fahrenheit(c.value * 9 // 5 + 32)

c = Celsius(100)
f = c as Fahrenheit   # Fahrenheit(212)
```

User-defined `as` operators are tried first; if no match is found, built-in casts (int, float, bool, str, etc.) are used as a fallback.
