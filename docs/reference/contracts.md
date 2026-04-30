# Design by Contract (DbC)

Ry supports Eiffel-style Design by Contract with preconditions (`require`), postconditions (`ensure`), and record invariants (`invariant`). Contract violations terminate the process with `exit(1)`.

---

## Preconditions (`require`)

Preconditions are checked at function entry. They specify what must be true for the function to be called correctly.

```ry
fn deposit(amount: int, balance: int) -> int:
    require:
        amount > 0
        balance >= 0
    newBalance: int = balance + amount
    return newBalance
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

```ry
fn abs(x: int) -> int:
    ensure v:
        v >= 0
    if x < 0:
        return -x
    return x
```

Since function arguments are immutable in Ry, you can reference them directly in `ensure` blocks to compare with entry values:

```ry
fn increment(x: int) -> int:
    ensure v:
        v == x + 1
    return x + 1
```

### Tuple destructuring

For functions that return tuples, multiple variable names can be specified, separated by commas:

```ry
fn divide(a: int, b: int) -> (int, int):
    ensure q, r:
        q >= 0
        r >= 0
    return (a // b, a % b)
```

The number of binding variables must match the number of tuple elements.

---

## Combined Example

```ry
fn deposit(amount: int, balance: int) -> int:
    require:
        amount > 0
        balance >= 0
    ensure v:
        v >= 0
        v == balance + amount
    newBalance: int = balance + amount
    return newBalance
```

---

## Record Invariants (`invariant`)

Invariants are conditions that must always hold for a record instance. They are checked:
- After construction
- After every field assignment

```ry
record BankAccount:
    balance: int
    minBalance: int
    invariant:
        balance >= minBalance
```

```ry
a = BankAccount(100, 0)    # OK: 100 >= 0
a.balance = -1                  # Contract violation: invariant failed for BankAccount
```

---

## Rules

- `require` and `ensure` blocks are optional and appear before the function body.
- `require` must come before `ensure` when both are present.
- `ensure` can only be used on functions that return a non-Unit value; applying it to a Unit-return function is a parse error (`'ensure' requires a non-Unit return type`).
- `ensure` requires a variable binding (e.g., `ensure v:`) to name the return value.
- For tuple returns, multiple bindings can be specified (e.g., `ensure q, r:`).
- `invariant` appears at the end of a `record` definition, after all field declarations.
- All contract violations terminate with `exit(1)` and print a diagnostic message.

> **See also**: For error-as-value patterns using `Result<T, E>`, `Ok`, `Err`, and the `?` operator, see [Types — Result](types.md#result-type) and [Operators — `?`](operators.md).
