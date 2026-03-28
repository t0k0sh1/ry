[English](contracts.md) | [日本語](../ja/reference/contracts.md) | [繁體中文](../zh/reference/contracts.md)

# Design by Contract (DbC)

Ry supports Eiffel-style Design by Contract with preconditions (`require`), postconditions (`ensure`), and struct invariants (`invariant`). Contract violations terminate the process with `exit(1)`.

---

## Preconditions (`require`)

Preconditions are checked at function entry. They specify what must be true for the function to be called correctly.

```python
fn deposit(amount: int, balance: int) -> int:
    require:
        amount > 0
        balance >= 0
    new_balance: int = balance + amount
    return new_balance
```

If any precondition fails, the program terminates with:
```
Contract violation: require failed in deposit()
```

---

## Postconditions (`ensure`)

Postconditions are checked before every `return`. They specify what the function guarantees about its result.

### Variable binding

`ensure` requires a variable name that binds the return value. This variable can be used in the postcondition expressions.

```python
fn abs(x: int) -> int:
    ensure v:
        v >= 0
    if x < 0:
        return -x
    return x
```

Since function arguments are immutable in Ry, you can reference them directly in `ensure` blocks to compare with entry values:

```python
fn increment(x: int) -> int:
    ensure v:
        v == x + 1
    return x + 1
```

### Tuple destructuring

For functions that return tuples, multiple variable names can be specified, separated by commas:

```python
fn divide(a: int, b: int) -> (int, int):
    ensure q, r:
        q >= 0
        r >= 0
    return (a // b, a % b)
```

The number of binding variables must match the number of tuple elements.

---

## Combined Example

```python
fn deposit(amount: int, balance: int) -> int:
    require:
        amount > 0
        balance >= 0
    ensure v:
        v >= 0
        v == balance + amount
    new_balance: int = balance + amount
    return new_balance
```

---

## Struct Invariants (`invariant`)

Invariants are conditions that must always hold for a struct instance. They are checked:
- After construction
- After every field assignment

```python
record BankAccount:
    balance: int
    min_balance: int
    invariant:
        balance >= min_balance
```

```python
a = BankAccount(100, 0)    # OK: 100 >= 0
a.balance = -1                  # Contract violation: invariant failed
```

---

## Rules

- `require` and `ensure` blocks are optional and appear before the function body.
- `require` must come before `ensure` when both are present.
- `ensure` requires a variable binding (e.g., `ensure v:`) to name the return value.
- For tuple returns, multiple bindings can be specified (e.g., `ensure q, r:`).
- `invariant` appears at the end of a `record` definition, after all field declarations.
- All contract violations terminate with `exit(1)` and print a diagnostic message.
