[English](../../tutorial/08-error-handling.md) | [日本語](../../ja/tutorial/08-error-handling.md) | [简体中文](08-error-handling.md)

# 错误处理

[<- 上一篇：集合与迭代器](07-collections.md) | [下一篇：包 ->](09-modules.md)

Ry 提供三种互补的策略来处理错误和缺失值：**Option**（值可能缺失）、**Result**（操作可能失败）以及**契约式设计**（在边界处防止无效状态）。本教程涵盖这三种策略及其适用场景。

---

## Option 类型

`Option<T>` 表示一个可能存在也可能不存在的值。它有两个变体：`Some(value)` 和 `None`。

```python
x: Option<int> = Some(42)
print(x)   # Some(42)

y: Option<int> = None
print(y)   # None
```

### 提取值

使用 `case` 安全地提取内部值并处理 `None` 情况。这使用了你在[控制流](04-control-flow.md)中学到的模式匹配：

```python
case x:
    Some(v):
        print(v)    # 42
    None:
        print("nothing")
```

> **为什么使用 Option？** 它在类型系统中明确表示了值可能缺失的可能性。调用者必须处理 `None` 情况，而不是返回像 `-1` 这样的"魔术值"或检查 `null` —— 编译器会确保这一点。

### Option 的使用场景

你已经见过 `Option` 的实际使用：`iterator.next()` 返回 `Option<T>`，对每个元素给出 `Some(element)`，迭代器耗尽时给出 `None`。

---

## Result 类型

`Result<T, E>` 用于可能失败的操作。成功时返回 `Ok(value)`，失败时返回 `Err(error)`。

```python
function divide(a: int, b: int) -> Result<int, Error>:
    if b == 0:
        return Err(Error("division by zero"))
    return Ok(a // b)
```

### 使用 case 处理 Result

```python
r = divide(10, 0)
case r:
    Ok(v):
        print(v)
    Err(e):
        print(e.message)   # division by zero
```

### `?` 运算符（错误传播）

当从一个返回 `Result` 的函数中调用另一个也返回 `Result` 的函数时，可以使用 `?` 自动传播错误。如果值是 `Ok`，它会被解包；如果是 `Err`，函数会立即返回该错误。

```python
function safe_divide(a: int, b: int) -> Result<int, Error>:
    if b == 0:
        return Err(Error("division by zero"))
    return Ok(a // b)

function divide_and_add(a: int, b: int) -> Result<int, Error>:
    v = safe_divide(a, b)?   # 如果 b == 0 则提前返回 Err
    return Ok(v + 1)
```

这等价于：

```python
function divide_and_add(a: int, b: int) -> Result<int, Error>:
    case safe_divide(a, b):
        Ok(v):
            return Ok(v + 1)
        Err(e):
            return Err(e)
```

`?` 运算符消除了样板代码，让你可以简洁地链式调用多个可能失败的操作：

```python
function compute(a: int, b: int, c: int) -> Result<int, Error>:
    x = safe_divide(a, b)?
    y = safe_divide(x, c)?
    return Ok(y + 1)
```

### 使用 `and_then` 和 `map` 进行方法链

当你需要链接多个返回 `Result` 的操作但无法使用 `?`（例如，不在返回 `Result` 的函数内部）时，可以使用 `and_then` 和 `map` 来避免深层嵌套的 `case` 语句。

**`and_then`** —— 链接本身返回 `Result` 的操作：

```python
# 不需要嵌套 3 层 case：
result = safe_divide(100, 2)
    .and_then((v: int) => safe_divide(v, 5))
    .and_then((v: int) => safe_divide(v, 2))

case result:
    Ok(v):  print(v)      # 5
    Err(e): print(e.message)
```

**`map`** —— 转换 `Ok` 值而不改变 `Result` 包装：

```python
result = safe_divide(10, 2)
    .map((v: int) => v * 10)

case result:
    Ok(v):  print(v)      # 50
    Err(e): print(e.message)
```

两个方法都会在 `Err` 时短路 —— 如果链中的任何步骤失败，错误会传播而不执行后续的闭包。

你可以在单个链中混合使用 `and_then` 和 `map`：

```python
result = safe_divide(100, 10)
    .and_then((v: int) => safe_divide(v, 2))
    .map((v: int) => v + 100)
# Ok(105)
```

> **为什么使用 Result？** 它使错误处理变得显式，无需异常机制。类型签名准确告诉你哪些函数可能失败，而 `?` 运算符保持代码简洁。

> **常见错误**：在不返回 `Result` 的函数中使用 `?` 会导致编译错误。`?` 运算符只能在返回类型为 `Result` 的函数中使用。

---

## 契约式设计

Ry 支持 Eiffel 风格的契约式设计，包括前置条件（`require`）、后置条件（`ensure`）和 record 不变式（`invariant`）。Option 和 Result 在运行时处理错误，而契约从一开始就**防止**无效状态的出现。

完整规格请参阅[契约式设计参考手册](../reference/contracts.md)。

### 前置条件（`require`）

使用 `require` 指定函数被调用时必须满足的条件：

```python
function deposit(amount: int, balance: int) -> int:
    require:
        amount > 0
        balance >= 0
    new_balance: int = balance + amount
    return new_balance
```

如果任何前置条件失败，程序将终止并显示：

```text
Contract violation: require failed in deposit()
```

### 后置条件（`ensure`）

使用 `ensure` 指定函数返回时必须满足的条件。返回值绑定到用户选择的变量名：

```python
function abs(x: int) -> int:
    ensure v:
        v >= 0
    if x < 0:
        return -x
    return x
```

由于 Ry 中函数参数是不可变的，你可以在 `ensure` 块中直接引用它们：

```python
function increment(x: int) -> int:
    ensure v:
        v == x + 1
    return x + 1
```

对于元组返回值，使用多个变量名：

```python
function divide(a: int, b: int) -> (int, int):
    ensure q, r:
        q >= 0
        r >= 0
    return (a // b, a % b)
```

### 组合 `require` 和 `ensure`

两者可以同时使用。`require` 必须在 `ensure` 之前：

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

### Record 不变式（`invariant`）

使用 `invariant` 指定 record 必须始终满足的条件。不变式在构造后以及每次字段赋值后都会检查：

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

> **为什么使用契约？** 它们直接在代码中记录和强制执行假设。如果函数要求 `amount > 0`，契约会在调用点立即捕获违规 —— 而不是在函数体内部深处出现症状。

### 契约规则

- `require` 和 `ensure` 块是可选的，出现在函数体之前。
- 当两者同时存在时，`require` 必须在 `ensure` 之前。
- `ensure` 需要一个变量绑定（例如 `ensure v:`）来命名返回值。
- `invariant` 出现在 `record` 定义的末尾，在所有字段声明之后。
- 所有契约违规都会以 `exit(1)` 终止。

---

## 使用场景选择

| 策略 | 适用场景 | 示例 |
|------|---------|------|
| **Option** | 值可能合理地缺失 | 查找键、`iterator.next()` |
| **Result** | 操作可能因有意义的错误而失败 | 文件 I/O、解析、网络调用 |
| **契约** | 无效输入不应该发生（程序员错误） | 负数存款、越界索引 |

**经验法则：**
- 对于可能因外部因素（用户输入、文件系统、网络）失败的操作，使用 **Result**。
- 当"无值"是正常的、预期的结果时，使用 **Option**。
- 使用**契约**尽早捕获程序员错误 —— 它们是断言，不是错误处理。

---

## 常见错误

1. **忽略 Result**：如果调用返回 `Result` 的函数却不处理它，你会丢失错误信息。
2. **在非 Result 函数中使用 `?`**：`?` 运算符要求包含它的函数返回 `Result`。
3. **混淆 Option 和 Result**：`Option` 有 `Some`/`None`；`Result` 有 `Ok`/`Err`。它们用途不同。

---

## 练习

1. **Result 与 `?`**：编写一个函数 `parse_and_double(s: str) -> Result<int, Error>`，使用辅助函数将字符串解析为整数并将其翻倍。使用 `?` 进行错误传播。

2. **契约**：编写一个函数 `withdraw(amount: int, balance: int) -> int`，使用 `require` 确保 `amount > 0` 且 `amount <= balance`，并使用 `ensure` 确保结果为非负数。

3. **Option 处理**：编写一个函数，接受 `List<int>` 并返回第一个偶数作为 `Option<int>`，如果没有偶数则返回 `None`。

---

[<- 上一篇：集合与迭代器](07-collections.md) | [下一篇：包 ->](09-modules.md)
