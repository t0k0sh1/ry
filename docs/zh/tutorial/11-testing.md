[English](../../tutorial/11-testing.md) | [日本語](../../ja/tutorial/11-testing.md) | [简体中文](11-testing.md)

# 测试

[<- 上一篇：契约式设计](10-contracts.md)

Ry 内置了使用 `describe`、`it`、`expect` 的 RSpec 风格测试语法。详细规格请参阅[测试参考手册](../reference/testing.md)。

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

使用 `describe` 将相关测试分组，使用 `it` 定义各个测试用例。

```python
describe("Calculator", fn():
    it("adds integers", fn():
        expect(1 + 2).to_eq(3)

    )
    it("subtracts integers", fn():
        expect(5 - 3).to_eq(2)

    )
    it("checks booleans", fn():
        expect(3 > 1).to_be_true()
    )
)
```

- `describe` 和 `it` 接受描述字符串和 **lambda 参数** `fn():` 作为第二个参数
- `describe`、`it`、`expect`、`mock` 和 `verify` 仅可在 `ry test` 中使用（普通的 `ry` 执行会产生编译错误）

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
| `to_contain(val)` | 容器包含值的断言 | List, Set, str |

---

## 输出格式

```
Calculator
  + adds integers
  + subtracts integers
  - checks booleans
    line 10: expected true, got false

2 passed, 1 failed
```

`+` 表示通过（绿色），`-` 表示失败（红色）。

---

## 模拟（Mock）

### `mock(fn_name, replacement)`

在当前的 `it` 块中将函数替换为模拟实现。`it` 块结束时会自动恢复。

```python
fn fetch_data() -> str:
    return "real data"

describe("mocking", fn():
    it("replaces function", fn():
        mock(fetch_data, fn() => "fake")
        expect(fetch_data()).to_eq("fake")

    )
    it("auto-restores", fn():
        expect(fetch_data()).to_eq("real data")
    )
)
```

### `verify(fn_name)`

返回模拟函数被调用的次数。

```python
describe("verify", fn():
    it("counts calls", fn():
        mock(fetch_data, fn() => "fake")
        fetch_data()
        fetch_data()
        expect(verify(fetch_data)).to_eq(2)
    )
)
```

---

## 参数化测试

使用 `@each` 以多组输入运行同一个测试：

```python
@each([(1, 2, 3), (0, 0, 0), (-1, 1, 0)])
it("adds {0} + {1} = {2}", fn(a: int, b: int, expected: int):
    expect(a + b).to_eq(expected)
)
```

每个元组成为一个独立的测试用例。描述中的 `{0}`、`{1}` 等会被替换为实际值。

---

## 基于属性的测试

使用 `@property` 以随机生成的输入进行测试：

```python
@property(count=100)
it("addition is commutative", fn(a: int, b: int):
    expect(a + b).to_eq(b + a)
)
```

测试以随机值运行 `count` 次。失败时会显示反例。

---

## 限制

- 不支持 `describe` 的嵌套使用
- 不支持 `before_each` / `after_each`
- 重载函数及 `@native` 函数无法模拟

---

[<- 上一篇：契约式设计](10-contracts.md)
