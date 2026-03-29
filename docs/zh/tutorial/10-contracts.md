[English](../../tutorial/10-contracts.md) | [日本語](../../ja/tutorial/10-contracts.md) | [简体中文](10-contracts.md)

# 契约式设计

[<- 上一篇：包](09-modules.md) | [下一篇：测试 ->](11-testing.md)

Ry 支持 Eiffel 风格的契约式设计，包含前置条件（`require`）、后置条件（`ensure`）以及 record 不变量（`invariant`）。当契约违反时，程序会终止。详细规格请参阅[契约式设计参考手册](../reference/contracts.md)。

---

## 前置条件（`require`）

使用 `require` 指定函数被调用时必须为真的条件。

```python
fn deposit(amount: int, balance: int) -> int:
    require:
        amount > 0
        balance >= 0
    new_balance: int = balance + amount
    return new_balance
```

当前置条件不满足时，程序会以下列消息终止：

```
Contract violation: require failed in deposit()
```

---

## 后置条件（`ensure`）

使用 `ensure` 指定函数返回时必须为真的条件。返回值会绑定到用户选择的变量名。

```python
fn abs(x: int) -> int:
    ensure v:
        v >= 0
    if x < 0:
        return -x
    return x
```

由于 Ry 的函数参数是不可变的，可以在 `ensure` 块中直接引用参数：

```python
fn increment(x: int) -> int:
    ensure v:
        v == x + 1
    return x + 1
```

对于返回元组的函数，可以用逗号分隔指定多个变量名：

```python
fn divide(a: int, b: int) -> (int, int):
    ensure q, r:
        q >= 0
        r >= 0
    return (a // b, a % b)
```

---

## 同时使用 `require` 和 `ensure`

两者可以同时使用。`require` 必须写在 `ensure` 之前。

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

## Record 不变量（`invariant`）

使用 `invariant` 指定 record 必须始终成立的条件。不变量会在建立后及每次字段赋值后检查。

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

## 规则

- `require` 和 `ensure` 块为可选项，写在函数主体之前。
- 同时使用时，`require` 必须写在 `ensure` 之前。
- `ensure` 需要变量绑定来命名返回值（例：`ensure v:`）。
- 对于元组返回值，可指定多个绑定变量（例：`ensure q, r:`）。
- `invariant` 写在 `record` 定义的末尾，在所有字段声明之后。
- 所有契约违反都会以 `exit(1)` 终止程序。

---

[<- 上一篇：包](09-modules.md) | [下一篇：测试 ->](11-testing.md)
