[English](operators.md) | [日本語](../ja/reference/operators.md) | [繁體中文](../zh/reference/operators.md)

# Operator Reference

## Precedence Table

Lower numbers indicate higher precedence (evaluated first).

| Precedence | Operator | Description | Associativity |
|---|---|---|---|
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

## Arithmetic Operators

| Operator | Description | Example |
|---|---|---|
| `+` | Addition / string concatenation | `1 + 2` -> `3`, `"a" + "b"` -> `"ab"` |
| `-` | Subtraction | `5 - 3` -> `2` |
| `*` | Multiplication / string repetition | `4 * 3` -> `12`, `"ab" * 3` -> `"ababab"` |
| `/` | Division (always float) | `7 / 2` -> `3.5` |
| `//` | Integer division (truncated) | `7 // 2` -> `3` |
| `%` | Modulo | `7 % 3` -> `1` |
| `**` | Exponentiation (always float) | `2 ** 10` -> `1024.0` |
| `-x` | Unary minus | `-5`, `-3.14` |
| `+x` | Unary plus | `+5` (no sign change) |

```python
let a = 10 // 3    # 3 (int)
let b = 10 / 3     # 3.3333... (float)
let c = 2 ** 8     # 256.0 (float)
let s = "foo" + "bar"  # "foobar"
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
- The `in` operator is used for membership checks on sets (`x in s`).
- The `not in` operator is the negation of `in` (`x not in s`).

```python
let x = 3 < 5       # true
let y = "abc" < "abd"  # true (lexicographic)
let s = {1, 2, 3}
let z = 2 in s      # true
let w = 4 not in s  # true
```

## Logical Operators

| Operator | Description | Type |
|---|---|---|
| `and` | Logical AND | `bool` x `bool` -> `bool` |
| `or` | Logical OR | `bool` x `bool` -> `bool` |
| `not` | Logical NOT | `bool` -> `bool` |

```python
let a = true and false   # false
let b = true or false    # true
let c = not true         # false
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
let flags = 0b0001 | 0b0010   # 3
let masked = flags & 0b0011   # 3
let shifted = 1 << 8          # 256
```

## Compound Assignment Operators

Shorthand for updating a variable. `x op= y` is equivalent to `x = x op y`.

| Operator | Equivalent Expression |
|---|---|
| `x += y` | `x = x + y` |
| `x -= y` | `x = x - y` |
| `x *= y` | `x = x * y` |
| `x /= y` | `x = x / y` |
| `x %= y` | `x = x % y` |

```python
let x = 10
x += 5    # x = 15
x -= 3    # x = 12
x *= 2    # x = 24
```

## Type Rules for Operations

| Operation | Left Type | Right Type | Result Type |
|---|---|---|---|
| `+ - *` | int | int | int |
| `+ - *` | float | int / float | float |
| `+ - *` | int | float | float |
| `/` | any numeric | any numeric | float |
| `//` | any numeric | any numeric | int |
| `**` | any numeric | any numeric | float |
| `%` | int | int | int |
| `%` | float or int (one is float) | -- | float |
| `+` | str | str | str |
| `== != < <= > >=` | numeric / bool / str | same type | bool |
| `*` | str | int | str |
| `in` | any | Set<T> | bool |
| `not in` | any | Set<T> | bool |
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
| Unary | `-` `~` `not` |

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
