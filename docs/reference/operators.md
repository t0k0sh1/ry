[English](operators.md) | [日本語](../ja/reference/operators.md) | [繁體中文](../zh/reference/operators.md)

# Operator Reference

## Precedence Table

Lower numbers indicate higher precedence (evaluated first).

| Precedence | Operator | Description | Associativity |
|---|---|---|---|
| 0 | `!!` | Error propagation (postfix) | Left |
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
| `//` | Integer division (truncated) | `7 // 2` -> `3` |
| `%` | Modulo | `7 % 3` -> `1` |
| `**` | Exponentiation (always float) | `2 ** 10` -> `1024.0` |
| `-x` | Unary minus | `-5`, `-3.14` |
| `+x` | Unary plus | `+5` (no sign change) |

```python
@const
a = 10 // 3    # 3 (int)
@const
b = 10 / 3     # 3.3333... (float)
@const
c = 2 ** 8     # 256.0 (float)
@const
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
- The `in` operator is used for membership checks on sets, lists, and maps (`x in s`).
- The `not in` operator is the negation of `in` (`x not in s`).
- For maps, `in` checks whether the key exists.

```python
@const
x = 3 < 5       # true
@const
y = "abc" < "abd"  # true (lexicographic)
@const
s = {1, 2, 3}
@const
z = 2 in s      # true
@const
w = 4 not in s  # true
@const
xs = [1, 2, 3]
@const
a = 2 in xs     # true (list linear search)
@const
m = {"a": 1}
@const
b = "a" in m    # true (map key lookup)
```

## Logical Operators

| Operator | Description | Type |
|---|---|---|
| `and` | Logical AND | `bool` x `bool` -> `bool` |
| `or` | Logical OR | `bool` x `bool` -> `bool` |
| `not` | Logical NOT | `bool` -> `bool` |

```python
@const
a = true and false   # false
@const
b = true or false    # true
@const
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
@const
flags = 0b0001 | 0b0010   # 3
@const
masked = flags & 0b0011   # 3
@const
shifted = 1 << 8          # 256
```

## Ternary Conditional Operator

```python
@const
x = condition ? true_value : false_value
```

Evaluates `condition`. If truthy, returns `true_value`; otherwise returns `false_value`. Both branches must have the same type. Right-associative, so nested ternaries associate from right to left.

```python
@const
x = 3 > 2 ? 10 : 20     # 10
@const
s = false ? "yes" : "no" # "no"

# Nested (right-associative)
@const
y = true ? (false ? 1 : 2) : 3   # 2
```

---

## Range Operator

The `..` operator creates an inclusive integer range.

```python
@const
xs = 1 .. 5    # [1, 2, 3, 4, 5]

for i in 1 .. 3:
    print(i)       # 1 2 3
```

The result is a `List<int>` containing all integers from the left operand to the right operand (inclusive).

---

## Null Coalescing Operator (`??`)

```python
@const
x = option_val ?? default_val
```

If `option_val` is `Some(v)`, returns `v`. Otherwise returns `default_val`. The right-hand operand must have the same type as the inner type of the Option.

```python
@const
a: int? = Some(10)
@const
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
| `//` | any numeric | any numeric | int |
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
