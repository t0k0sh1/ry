[English](03-operators.md) | [日本語](../ja/tutorial/03-operators.md) | [繁體中文](../zh/tutorial/03-operators.md)

# 03 - Operators

<- [02 - Variables and Types](02-variables-and-types.md) / Next -> [04 - Control Flow](04-control-flow.md)

---

## Arithmetic Operators

| Operator | Description | Example | Result |
|----------|-------------|---------|--------|
| `+` | Addition | `3 + 2` | `5` |
| `-` | Subtraction | `3 - 2` | `1` |
| `*` | Multiplication / string repetition | `3 * 2` | `6` |
| `/` | Division (always float) | `7 / 2` | `3.5` |
| `//` | Integer division (always int) | `7 // 2` | `3` |
| `%` | Modulo | `7 % 3` | `1` |
| `**` | Exponentiation (always float) | `2 ** 10` | `1024.0` |

```python
a = 10
b = 3

print(a + b)    # 13
print(a - b)    # 7
print(a * b)    # 30
print(a / b)    # 3.3333... (float)
print(a // b)   # 3 (int)
print(a % b)    # 1
print(2 ** 8)   # 256.0 (float)
```

---

## Comparison Operators

All comparison operators return a `bool` value.

| Operator | Description | Example |
|----------|-------------|---------|
| `==` | Equal to | `a == b` |
| `!=` | Not equal to | `a != b` |
| `<` | Less than | `a < b` |
| `<=` | Less than or equal to | `a <= b` |
| `>` | Greater than | `a > b` |
| `>=` | Greater than or equal to | `a >= b` |

```python
x = 5
y = 10

print(x == y)   # false
print(x != y)   # true
print(x < y)    # true
print(x <= y)   # true
print(x > y)    # false
print(x >= y)   # false
```

Comparison operators also work on strings (lexicographic comparison).

```python
print("abc" == "abc")   # true
print("abc" < "abd")    # true
print("b" > "a")        # true
```

---

## Logical Operators

| Operator | Description | Example |
|----------|-------------|---------|
| `and` | Logical AND | `a and b` |
| `or` | Logical OR | `a or b` |
| `not` | Logical NOT | `not a` |

```python
t = true
f = false

print(t and f)   # false
print(t or f)    # true
print(not t)     # false
print(not f)     # true
```

---

## Bitwise Operators

Bitwise operators can only be used with the `int` type.

| Operator | Description | Example |
|----------|-------------|---------|
| `&` | Bitwise AND | `5 & 3` -> `1` |
| `\|` | Bitwise OR | `5 \| 3` -> `7` |
| `^` | Bitwise XOR | `5 ^ 3` -> `6` |
| `~` | Bitwise NOT (unary) | `~5` -> `-6` |
| `<<` | Left shift | `1 << 3` -> `8` |
| `>>` | Right shift (arithmetic) | `8 >> 2` -> `2` |
| `>>>` | Logical right shift | `-1 >>> 1` -> `9223372036854775807` |

```python
a = 0b1010   # 10
b = 0b1100   # 12

print(a & b)    # 8  (0b1000)
print(a | b)    # 14 (0b1110)
print(a ^ b)    # 6  (0b0110)
print(~a)       # -11
print(1 << 4)   # 16
print(32 >> 2)  # 8
```

---

## Compound Assignment Operators

Shorthand notation for updating the value of a variable.

| Operator | Description | Equivalent Expression |
|----------|-------------|-----------------------|
| `+=` | Addition assignment | `x = x + n` |
| `-=` | Subtraction assignment | `x = x - n` |
| `*=` | Multiplication assignment | `x = x * n` |
| `/=` | Division assignment | `x = x / n` |
| `%=` | Modulo assignment | `x = x % n` |

```python
x = 10
x += 5    # x == 15
x -= 3    # x == 12
x *= 2    # x == 24
x /= 4    # x == 6.0 (becomes float)
```

---

## Increment / Decrement Operators

Shorthand for incrementing or decrementing a variable by 1.

| Operator | Description | Equivalent Expression |
|----------|-------------|-----------------------|
| `x++` | Increment by 1 | `x = x + 1` |
| `x--` | Decrement by 1 | `x = x - 1` |

```python
count = 0
count++       # count == 1
count++       # count == 2
count--       # count == 1
```

> **Note**: These are statement-only operators. They cannot be used inside expressions.

---

## Type Promotion Rules

The following describes the behavior when `int` and `float` are mixed in operations.

```python
# + - * produce float if either operand is float
print(1 + 2)      # 3 (int)
print(1 + 2.0)    # 3.0 (float)
print(1.0 + 2)    # 3.0 (float)

# / always produces float
print(4 / 2)      # 2.0 (float)

# // always produces int
print(7 // 2)     # 3 (int)
print(7.0 // 2)   # 3 (int)

# ** always produces float
print(2 ** 3)     # 8.0 (float)

# % produces int if both operands are int, float if either is float
print(7 % 3)      # 1 (int)
print(7.5 % 2)    # 1.5 (float)

# + concatenates strings if both operands are str
print("foo" + "bar")   # "foobar"

# * repeats a string if one operand is str and the other is int
print("ab" * 3)        # "ababab"
print(3 * "ab")        # "ababab"
```

---

## Membership Operators

| Operator | Description | Example |
|----------|-------------|---------|
| `in` | Membership test | `2 in {1, 2, 3}` -> `true` |
| `not in` | Negated membership test | `4 not in {1, 2, 3}` -> `true` |

```python
s = {1, 2, 3}
print(2 in s)        # true
print(4 not in s)    # true
```

---

<- [02 - Variables and Types](02-variables-and-types.md) / Next -> [04 - Control Flow](04-control-flow.md)
