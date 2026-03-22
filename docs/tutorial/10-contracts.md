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

Use `ensure` to specify conditions that must be true when a function returns.

### The `result` Keyword

Inside an `ensure` block, `result` refers to the return value.

```python
fn abs(x: int) -> int:
    ensure:
        result >= 0
    if x < 0:
        return -x
    return x
```

### The `old()` Expression

`old(expr)` captures the value of an expression at function entry. This is useful for comparing pre- and post-states.

```python
fn increment(x: int) -> int:
    ensure:
        result == old(x) + 1
    return x + 1
```

---

## Combining `require` and `ensure`

Both can be used together. `require` must come before `ensure`.

```python
fn deposit(amount: int, balance: int) -> int:
    require:
        amount > 0
        balance >= 0
    ensure:
        result >= 0
        result == old(balance) + amount
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
- `result` and `old()` can only be used inside `ensure` blocks.
- `invariant` appears at the end of a `record` definition, after all field declarations.
- All contract violations terminate with `exit(1)`.

---

[<- Prev: Packages](09-modules.md) | [Next: Testing ->](11-testing.md)
