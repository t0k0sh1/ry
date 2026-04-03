[English](../../tutorial/05-functions.md) | [日本語](../../ja/tutorial/05-functions.md) | [简体中文](05-functions.md)

# 函数

[<- 上一篇：控制流](04-control-flow.md) | [下一篇：Record 与枚举 ->](06-records.md)

---

## 基本函数定义

函数使用 `function` 关键字定义。参数类型以 `name: type` 格式指定。如果省略类型，默认为 `any`。返回类型在 `->` 之后指定。

```python
function add(a: int, b: int) -> int:
    return a + b
```

- 建议声明参数类型。如果省略，类型默认为 `any`。
- 返回类型在 `->` 之后指定。
- 使用 `return` 返回值。

---

## 函数调用

通过名称和参数调用已定义的函数。

```python
function multiply(x: int, y: int) -> int:
    return x * y

result = multiply(3, 4)
print(result)   # 12
```

---

## 递归函数

函数可以调用自身（递归）。

```python
function factorial(n: int) -> int:
    if n <= 1:
        return 1
    return n * factorial(n - 1)

print(factorial(5))   # 120
print(factorial(0))   # 1
```

---

## 函数重载

可以定义多个同名但参数数量或类型不同的函数。

```python
function add(a: int, b: int) -> int:
    return a + b

function add(a: float, b: float) -> float:
    return a + b

print(add(1, 2))       # 3
print(add(1.5, 2.5))   # 4
```

调用时会根据参数类型自动选择适当的函数。

> **注意**：定义参数类型相同但仅返回类型不同的函数会产生编译错误。

---

## 省略返回类型（Unit 类型）

不需要返回值的函数可以省略 `->`。此时函数返回 Unit 类型。

```python
function greet():
    print(42)

greet()   # 42
```

这是最简单的无参数、无返回值函数形式。

---

## 默认参数

参数可以有默认值。调用者省略这些参数时，将使用默认值。

```python
function greet(name: str, greeting: str = "Hello") -> str:
    return f"{greeting}, {name}"

print(greet("Alice"))             # Hello, Alice
print(greet("Bob", "Good morning"))  # Good morning, Bob
```

可以有多个默认参数：

```python
function connect(host: str, port: int = 8080, timeout: int = 30) -> str:
    return f"{host}:{port} (timeout={timeout})"

print(connect("localhost"))              # localhost:8080 (timeout=30)
print(connect("localhost", 3000))        # localhost:3000 (timeout=30)
print(connect("localhost", 3000, 10))    # localhost:3000 (timeout=10)
```

> **为什么使用默认参数？** 它们让你在常见情况下保持简洁的调用方式，同时在需要时允许自定义 —— 无需多个重载。

> **注意**：有默认值的参数必须放在无默认值的参数之后。

---

## Lambda 函数

Lambda 函数允许你将函数写成表达式。单表达式 lambda 使用 `(parameters) => expression` 形式，块 lambda 使用 `(parameters):` 后跟缩进块。两种情况下返回类型都会自动推断。

### 单表达式 Lambda

```python
double = (x: int) => x * 2
print(double(5))  # 10

add = (a: int, b: int) => a + b
print(add(3, 4))  # 7
```

### 无参数 Lambda

```python
answer = () => 42
print(answer())  # 42
```

### 多行 Lambda

在 `:` 之后换行并缩进，可以编写多条语句。

```python
abs = (x: int):
    if x < 0:
        return -x
    return x

print(abs(-5))  # 5
print(abs(3))   # 3
```

> **为什么使用 lambda？** 它们非常适合简短的一次性函数 —— 特别是作为 `filter` 和 `map` 等高阶函数的参数（见下文）。

---

## 闭包

Lambda 函数可以捕获定义它们的作用域中的变量。函数及其捕获的环境的组合称为**闭包**。

```python
offset = 10
add_offset = (x: int) => x + offset
print(add_offset(5))  # 15
```

闭包**按值**捕获变量 —— 创建闭包后修改原始变量不会影响闭包的副本。

```python
base = 10
f = (x: int) => x + base
base = 999
print(f(1))  # 11（仍然使用捕获的值 10）
```

这是双向的 —— 闭包内部的修改也不会影响外部变量：

```python
counter = 0
items = [1, 2, 3]
items.map((x: int):
    counter += x    # 只修改闭包的本地副本
    return x
)
print(counter)  # 0（外部变量不变）
```

> **为什么按值捕获？** 它确保了安全性和可预测性 —— 你总是可以只看当前作用域就能推断变量的值，无需担心闭包内部发生的修改。
> **为什么使用闭包？** 它们让你可以即时创建专用函数。例如，你可以从单个模板创建一系列加法函数。

---

## 高阶函数

可以定义接受其他函数作为参数的函数。函数类型写作 `function(parameter_types) -> return_type`。

```python
function apply(f: function(int) -> int, x: int) -> int:
    return f(x)

double = (x: int) => x * 2
print(apply(double, 3))                # 6
print(apply((n: int) => n + 1, 10))  # 11
```

### 函数作为值

命名函数也可以绑定到变量或作为参数传递 —— 它们的行为与 lambda 完全相同。

```python
function square(x: int) -> int:
    return x * x

# 将命名函数作为参数传递
print(apply(square, 4))  # 16

# 绑定到变量
sq = square
print(sq(5))  # 25
```

> **为什么使用高阶函数？** 它们让你将**做什么**与**怎么做**分离。同一个 `apply` 函数可以与任何变换一起使用，使代码更具复用性。你已经在[集合](07-collections.md)中看到过这种模式，如 `filter`、`map` 和 `reduce`。

---

## UFCS（统一函数调用语法）

使用 UFCS，你可以将 `f(a, b)` 写成 `a.f(b)`。第一个参数移到点号前面，实现方法链式调用风格。

```python
function add(a: int, b: int) -> int:
    return a + b

x = 1
print(x.add(2))   # add(x, 2) -> 3
```

### 链式调用

UFCS 在链接多个调用时特别出色 —— 从左到右阅读而不是从内到外：

```python
function double(n: int) -> int:
    return n * 2

# 链式（自然阅读："取 x，加 2，然后翻倍"）
print(x.add(2).double())   # 6

# 等价的嵌套调用（更难阅读）
print(double(add(x, 2)))   # 6
```

> **为什么使用 UFCS？** 它将深层嵌套的函数调用转变为可读的从左到右的管道。你已经在迭代器链中见过这种用法，如 `xs.iter().filter(...).map(...).to_list()`。

---

## 练习

1. **默认参数**：编写函数 `format_price(amount: int, currency: str = "USD", decimals: int = 2) -> str`，用于格式化价格。验证 `format_price(42)` 和 `format_price(42, "EUR")` 都能正常工作。

2. **高阶函数**：编写函数 `apply_twice(f: function(int) -> int, x: int) -> int`，将 `f` 应用于 `x` 两次（即 `f(f(x))`）。使用 `(x: int) => x + 1` 测试，验证 `apply_twice((x: int) => x + 1, 5)` 返回 `7`。

3. **UFCS 链式调用**：定义 `inc(n: int) -> int`（加 1）和 `triple(n: int) -> int`（乘以 3）。使用 UFCS 编写 `5.inc().triple()` 并验证结果为 `18`。

---

[<- 上一篇：控制流](04-control-flow.md) | [下一篇：Record 与枚举 ->](06-records.md)
