[English](08-error-handling.md) | [日本語](../ja/tutorial/08-error-handling.md) | [繁體中文](../zh/tutorial/08-error-handling.md)

# Error Handling

[<- Prev: Collections and Iterators](07-collections.md) | [Next: Packages ->](09-modules.md)

Ry provides three complementary strategies for dealing with errors and absent values: **Option** (a value may be missing), **Result** (an operation may fail), and **Design by Contract** (prevent invalid states at the boundary). This tutorial covers all three and when to use each.

---

## Option Type

`Option<T>` represents a value that may or may not exist. It has two variants: `Some(value)` and `None`.

```python
x: Option<int> = Some(42)
print(x)   # Some(42)

y: Option<int> = None
print(y)   # None
```

### Extracting the Value

Use `when` to safely extract the inner value and handle the `None` case. This uses the pattern matching you learned in [Control Flow](04-control-flow.md):

```python
when x:
    case Some(v):
        print(v)    # 42
    case None:
        print("nothing")
```

> **Why Option?** It makes the possibility of absence explicit in the type system. Instead of returning a "magic value" like `-1` or checking for `null`, the caller must handle the `None` case — the compiler ensures it.

### Where You Encounter Option

You have already seen `Option` in action: `iterator.next()` returns `Option<T>`, giving `Some(element)` for each element and `None` when the iterator is exhausted.

---

## Result Type

`Result<T, E>` is used for operations that may fail. Return `Ok(value)` for success and `Err(error)` for failure.

```python
fn divide(a: int, b: int) -> Result<int, str>:
    if b == 0:
        return Err("division by zero")
    return Ok(a // b)
```

### Handling Results with when

```python
r = divide(10, 0)
when r:
    case Ok(v):
        print(v)
    case Err(e):
        print(e)   # division by zero
```

### The `?` Operator (Error Propagation)

When calling a function that returns `Result` from another function that also returns `Result`, you can use `?` to propagate errors automatically. If the value is `Ok`, it is unwrapped; if `Err`, the function returns immediately with that error.

```python
fn safe_divide(a: int, b: int) -> Result<int, Error>:
    if b == 0:
        return Err(Error("division by zero"))
    return Ok(a // b)

fn divide_and_add(a: int, b: int) -> Result<int, Error>:
    v = safe_divide(a, b)?   # Returns Err early if b == 0
    return Ok(v + 1)
```

This is equivalent to writing:

```python
fn divide_and_add(a: int, b: int) -> Result<int, Error>:
    when safe_divide(a, b):
        case Ok(v):
            return Ok(v + 1)
        case Err(e):
            return Err(e)
```

The `?` operator removes the boilerplate, letting you chain multiple fallible operations cleanly:

```python
fn compute(a: int, b: int, c: int) -> Result<int, Error>:
    x = safe_divide(a, b)?
    y = safe_divide(x, c)?
    return Ok(y + 1)
```

> **Why Result?** It makes error handling explicit without exceptions. The type signature tells you exactly which functions can fail, and the `?` operator keeps the code concise.

> **Common mistake**: Using `?` in a function that does not return `Result` causes a compile error. The `?` operator can only be used in functions whose return type is `Result`.

---

## Design by Contract

Ry supports Eiffel-style Design by Contract with preconditions (`require`), postconditions (`ensure`), and record invariants (`invariant`). While Option and Result handle errors at runtime, contracts **prevent** invalid states from occurring in the first place.

For the full specification, see [Design by Contract Reference](../reference/contracts.md).

### Preconditions (`require`)

Use `require` to specify conditions that must be true when a function is called:

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

### Postconditions (`ensure`)

Use `ensure` to specify conditions that must be true when a function returns. The return value is bound to a user-chosen variable name:

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

For tuple returns, use multiple variable names:

```python
fn divide(a: int, b: int) -> (int, int):
    ensure q, r:
        q >= 0
        r >= 0
    return (a // b, a % b)
```

### Combining `require` and `ensure`

Both can be used together. `require` must come before `ensure`:

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

### Record Invariants (`invariant`)

Use `invariant` to specify conditions that must always hold for a record. Invariants are checked after construction and after every field assignment:

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

> **Why contracts?** They document and enforce assumptions directly in code. If a function requires `amount > 0`, the contract catches violations immediately at the call site — not deep inside the function body where the symptom appears.

### Contract Rules

- `require` and `ensure` blocks are optional and appear before the function body.
- `require` must come before `ensure` when both are present.
- `ensure` requires a variable binding (e.g., `ensure v:`) to name the return value.
- `invariant` appears at the end of a `record` definition, after all field declarations.
- All contract violations terminate with `exit(1)`.

---

## When to Use What

| Strategy | Use When | Example |
|----------|----------|---------|
| **Option** | A value may legitimately be absent | Looking up a key, `iterator.next()` |
| **Result** | An operation can fail with a meaningful error | File I/O, parsing, network calls |
| **Contract** | Invalid input should never happen (programmer error) | Negative deposit, null pointer |

**Rules of thumb:**
- Use **Result** for operations that can fail due to external factors (user input, file system, network).
- Use **Option** when "nothing" is a normal, expected outcome.
- Use **contracts** to catch programmer mistakes early — they are assertions, not error handling.

---

## Common Mistakes

1. **Ignoring a Result**: If you call a function returning `Result` and don't handle it, you lose the error information.
2. **Using `?` in a non-Result function**: The `?` operator requires the enclosing function to return `Result`.
3. **Confusing Option and Result**: `Option` has `Some`/`None`; `Result` has `Ok`/`Err`. They serve different purposes.

---

## Exercises

1. **Result with `?`**: Write a function `parse_and_double(s: str) -> Result<int, Error>` that parses a string to an integer using a helper function and doubles it. Use `?` for error propagation.

2. **Contracts**: Write a function `withdraw(amount: int, balance: int) -> int` with `require` that `amount > 0` and `amount <= balance`, and `ensure` that the result is non-negative.

3. **Option handling**: Write a function that takes a `List<int>` and returns the first even number as `Option<int>`, returning `None` if no even numbers exist.

---

[<- Prev: Collections and Iterators](07-collections.md) | [Next: Packages ->](09-modules.md)
