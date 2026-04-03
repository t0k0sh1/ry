[English](../../reference/contracts.md) | [日本語](../../ja/reference/contracts.md) | [繁體中文](contracts.md)

# 契约式设计 (Design by Contract)

Ry 支持 Eiffel 风格的契约式设计，包含前置条件（`require`）、后置条件（`ensure`）和结构体不变量（`invariant`）。契约违反时，程序将以 `exit(1)` 终止。

---

## 前置条件 (`require`)

前置条件在函数入口处检查。它们指定函数被正确调用所需满足的条件。

```python
function deposit(amount: int, balance: int) -> int:
    require:
        amount > 0
        balance >= 0
    new_balance: int = balance + amount
    return new_balance
```

若前置条件未满足，程序将输出以下消息并终止：
```
Contract violation: require failed in deposit()
```

---

## 后置条件 (`ensure`)

后置条件在每个 `return` 之前检查。它们指定函数对其返回值的保证。

### 变量绑定

`ensure` 需要一个变量名来绑定返回值。此变量可在后置条件表达式中使用。

```python
function abs(x: int) -> int:
    ensure v:
        v >= 0
    if x < 0:
        return -x
    return x
```

由于 Ry 的函数参数是不可变的，可以在 `ensure` 块中直接引用参数来与入口值比较：

```python
function increment(x: int) -> int:
    ensure v:
        v == x + 1
    return x + 1
```

### 元组解构

对于返回元组的函数，可以用逗号分隔指定多个变量名：

```python
function divide(a: int, b: int) -> (int, int):
    ensure q, r:
        q >= 0
        r >= 0
    return (a // b, a % b)
```

绑定变量的数量必须与元组元素数量一致。

---

## 组合示例

```python
function deposit(amount: int, balance: int) -> int:
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

## 结构体不变量 (`invariant`)

不变量是结构体实例必须始终满足的条件。在以下时机检查：
- 构造时
- 每次字段赋值后

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

## 规则

- `require` 和 `ensure` 块为可选，写在函数体之前。
- 同时使用时，`require` 必须在 `ensure` 之前。
- `ensure` 需要变量绑定来命名返回值（例：`ensure v:`）。
- 对于元组返回值，可指定多个绑定变量（例：`ensure q, r:`）。
- `invariant` 写在 `record` 定义的末尾，所有字段声明之后。
- 所有契约违反以 `exit(1)` 终止程序并输出诊断消息。
