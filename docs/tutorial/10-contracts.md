[English](10-contracts.md) | [日本語](../ja/tutorial/10-contracts.md) | [繁體中文](../zh/tutorial/10-contracts.md)

# Design by Contract

[<- Prev: Packages](09-modules.md) | [Next: Testing ->](11-testing.md)

Ry supports Eiffel-style Design by Contract with preconditions (`require`), postconditions (`ensure`), and struct invariants (`invariant`). Contract violations terminate the program. For the full specification, see [Design by Contract Reference](../reference/contracts.md).

---

## Preconditions (`require`)

Use `require` to specify conditions that must be true when a function is called.

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

Use `ensure` to specify conditions that must be true when a function returns. The return value is bound to a user-chosen variable name.

```python
fn abs(x: int) -> int:
    ensure v:
        v >= 0
    if x < 0:
        return -x
    return x
```

Since function arguments are immutable in Ry, you can reference them directly in `ensure` blocks:

```python
fn increment(x: int) -> int:
    ensure v:
        v == x + 1
    return x + 1
```

For tuple returns, use multiple variable names separated by commas:

```python
fn divide(a: int, b: int) -> (int, int):
    ensure q, r:
        q >= 0
        r >= 0
    return (a // b, a % b)
```

---

## Combining `require` and `ensure`

Both can be used together. `require` must come before `ensure`.

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

Use `invariant` to specify conditions that must always hold for a struct. Invariants are checked after construction and after every field assignment.

```python
record BankAccount:
    balance: int
    min_balance: int
    invariant:
        balance >= min_balance
```

```python
a = BankAccount(100, 0)   # OK: 100 >= 0
# a.balance = -1              # Contract violation: invariant failed
```

---

## Rules

- `require` and `ensure` blocks are optional and appear before the function body.
- `require` must come before `ensure` when both are present.
- `ensure` requires a variable binding (e.g., `ensure v:`) to name the return value.
- For tuple returns, multiple bindings can be specified (e.g., `ensure q, r:`).
- `invariant` appears at the end of a `record` definition, after all field declarations.
- All contract violations terminate with `exit(1)`.

---

[<- Prev: Packages](09-modules.md) | [Next: Testing ->](11-testing.md)
