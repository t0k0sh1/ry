[English](../../tutorial/11-testing.md) | [日本語](../../ja/tutorial/11-testing.md) | [简体中文](11-testing.md)

# 测试

[<- 上一篇：并发](10-concurrency.md) | [下一篇：构建项目 ->](12-building-a-project.md)

Ry 内建了使用 `@describe`、`@it`、`expect` 的 RSpec 风格测试语法。详细规格请参阅[测试参考手册](../reference/testing.md)。

---

## 运行测试

```bash
ry test                       # 自动发现并运行所有 *.test.ry 文件
ry test tests/spec            # 递归运行指定目录下所有 *.test.ry 文件
ry test tests/my_test.test.ry # 运行特定的测试文件
ry test -p                    # 并行运行所有测试（-p 或 --parallel）
```

所有测试通过时，退出码为 `0`；若有任一测试失败，则为 `1`。

不带参数运行时，`ry test` 会搜索 `package.toml` 来找到项目根目录，然后递归地发现所有 `*.test.ry` 文件。

---

## 编写测试

将 `@it` 附加到命名函数上以宣告一个测试用例，并将一组相关测试包裹在带有 `@describe` 注解的函数中。

```python
@it("should add integers")
function test_add():
    expect(1 + 2).to_eq(3)

@describe("Calculator")
function calculator_tests():
    @it("should subtract integers")
    function test_sub():
        expect(5 - 3).to_eq(2)

    @it("should check booleans")
    function test_bool():
        expect(3 > 1).to_be_true()
```

- `@it` 接受一个描述字串。被装饰的函数成为测试主体
- `@describe` 会将其函数主体中定义的内部 `@it` 函数分组。可以巢状嵌套；输出按巢状深度比例缩排
- 直接在 `@describe` 函数主体中宣告的变数会作为**共享设置（shared setup）**，并被每个内部 `@it` 函数捕获

```python
@describe("shared setup")
function shared_setup_tests():
    base = 100
    offset = 5

    @it("should use base value")
    function test_base():
        expect(base).to_eq(100)

    @it("should use base and offset")
    function test_combined():
        expect(base + offset).to_eq(105)
```

- `expect`、`mock` 和 `verify` 仅可在 `ry test` 中使用（普通的 `ry` 执行会产生编译错误）

> **旧版 lambda 形式**：带 lambda 参数的 `describe("name", (): ...)` 和 `it("desc", (): ...)` 仍然可解析，但已**弃用**。新代码请优先使用作用于命名函数的指令形式。

---

## 匹配器

| 匹配器 | 说明 | 支持类型 |
|--------|------|---------|
| `to_eq(expected)` | 等值比较 | int, float, bool, str |
| `to_not_eq(expected)` | 不等值断言 | int, float, bool, str |
| `to_be_true()` | `true` 断言 | bool |
| `to_be_false()` | `false` 断言 | bool |
| `to_be_none()` | `None` 断言 | Option |
| `to_be_some()` | Option 为 `Some` 的断言 | Option |
| `to_be_ok()` | Result 为 `Ok` 的断言 | Result |
| `to_be_err()` | Result 为 `Err` 的断言 | Result |
| `to_contain(val)` | 容器包含值的断言 | List, Set, Map, str |
| `to_not_contain(val)` | 容器不包含值的断言 | List, Set, Map, str |
| `to_be_greater_than(v)` | `actual > v` 断言 | int, float |
| `to_be_less_than(v)` | `actual < v` 断言 | int, float |
| `to_be_greater_than_or_eq(v)` | `actual >= v` 断言 | int, float |
| `to_be_less_than_or_eq(v)` | `actual <= v` 断言 | int, float |
| `to_have_length(n)` | 长度等于 `n` 的断言 | List, Set, Map, str |
| `to_be_empty()` | 长度为 0 的断言 | List, Set, Map, str |
| `to_start_with(prefix)` | 字符串以前缀开头的断言 | str |
| `to_end_with(suffix)` | 字符串以后缀结尾的断言 | str |

### fail

`fail()` 立即将当前测试标记为失败。

```python
@it("should handle error")
function test_should_handle_error():
    case result:
        Ok(v):
            fail("expected error")
        Err(e):
            expect(e.message).to_eq("not found")
```

- `fail()` —— 以通用消息标记测试失败
- `fail(message)` —— 以自定义消息标记测试失败
- 仅在 `ry test` 模式下可用

---

## 输出格式

```
Calculator
  + should add integers
  + should subtract integers
  - should check booleans
    line 10: expected true, got false

2 passed, 1 failed
```

`+` 表示通过（绿色），`-` 表示失败（红色）。巢状的 `@describe` 群组会将其内部测试依巢状深度按比例缩排：

```text
outer group
  inner group
    + should pass deeply nested test
```

---

## 模拟（Mock）

### `mock(fn_name, replacement)`

在当前的 `@it` 块中将函数替换为模拟实现。`@it` 块结束时会自动恢复。
原函数的 `require` 和 `ensure` 契约仍然会对模拟调用执行。

```python
function fetch_data() -> str:
    return "real data"

@describe("mocking")
function mocking_tests():
    @it("should replace function")
    function test_replace():
        mock(fetch_data, () => "fake")
        expect(fetch_data()).to_eq("fake")

    @it("should auto-restore after it block")
    function test_restore():
        expect(fetch_data()).to_eq("real data")
```

### `verify(fn_name)`

返回模拟函数被调用的次数。

```python
@describe("verify")
function verify_tests():
    @it("should count calls")
    function test_count():
        mock(fetch_data, () => "fake")
        fetch_data()
        fetch_data()
        expect(verify(fetch_data)).to_eq(2)
```

---

## 参数化测试

结合 `@each` 与 `@it` 作用于命名函数上，以多组输入运行同一个测试：

```python
@each([(1, 2, 3), (0, 0, 0), (-1, 1, 0)])
@it("should add {0} + {1} = {2}")
function test_add_each(a: int, b: int, expected: int):
    expect(a + b).to_eq(expected)
```

每个元组成为一个独立的测试用例。描述中的 `{0}`、`{1}` 等会被替换为实际值。函数的参数列表必须与元组的元数匹配。

---

## 基于属性的测试

结合 `@property` 与 `@it` 作用于命名函数上，以随机生成的输入进行测试：

```python
@property(count=100)
@it("should verify addition is commutative")
function test_add_commutative(a: int, b: int):
    expect(a + b).to_eq(b + a)
```

测试以随机值运行 `count` 次。Ry 会从每个参数的类型注解推断生成器。失败时会显示反例。

---

## 使用契约进行测试

契约（来自[错误处理](08-error-handling.md)）与模拟协同工作：原函数的 `require` 和 `ensure` 契约**仍然会**对模拟调用执行。这意味着契约充当隐式测试断言。

```python
function deposit(amount: int, balance: int) -> int:
    require:
        amount > 0
    ensure v:
        v > balance
    return balance + amount

@describe("deposit")
function deposit_tests():
    @it("should enforce contracts even when mocked")
    function test_contract():
        mock(deposit, (amount: int, balance: int) => balance + amount)
        expect(deposit(10, 100)).to_eq(110)
        # deposit(-1, 100) 会以 "require failed" 终止
```

> **为什么这很重要**：你可以模拟实现细节，同时保留契约安全网。如果模拟违反了后置条件，测试会立即捕获。

---

## 限制

- 巢状只支援作用于命名函数的 `@describe`。旧版 lambda 形式 `describe("name", (): ...)` 无法巢状
- 不支援 `before_each` / `after_each` —— 请改用在 `@describe` 函数主体中宣告的共享设置变数
- 重载函数及 `@native` 函数无法模拟

---

## 练习

1. **基本测试**：为 `max(a: int, b: int) -> int` 函数编写 `describe` 块，覆盖相等值、正数和负数的情况。

2. **模拟**：编写 `fetch_temperature() -> int` 函数并返回一个值。在测试中模拟它返回固定值，使用 `verify` 检查它被调用了恰好一次。

3. **参数化测试**：使用 `@each` 测试 `is_even(n: int) -> bool` 函数，输入为 `[(2, true), (3, false), (0, true), (-4, true)]`。

---

[<- 上一篇：并发](10-concurrency.md) | [下一篇：构建项目 ->](12-building-a-project.md)
