[English](../../reference/testing.md) | [日本語](../../ja/reference/testing.md) | [繁體中文](testing.md)

# 测试功能

Ry 内建 RSpec 风格的测试语法。使用 `ry test` 子命令执行测试文件。

---

## 运行测试

```bash
ry test              # 自动发现并运行项目内所有 *.test.ry 文件
ry test tests/spec   # 递归运行指定目录下所有 *.test.ry 文件
ry test test_file.ry # 运行指定的测试文件
ry test -p           # 并行运行所有测试（-p 或 --parallel）
ry test -p tests/    # 并行运行指定目录的测试
ry test -w           # 监视模式：文件变更时自动重新运行测试（-w 或 --watch）
ry test -w -p        # 监视模式 + 并行运行
ry test -w tests/    # 监视指定目录
ry test --coverage   # 所有测试 + 行覆盖率摘要
ry test --cov        # --coverage 的简写
```

退出码为 0 表示所有测试通过，1 表示有测试失败。

### 自动发现模式

不带参数运行 `ry test` 时：

1. 搜索 `package.toml` 以找到项目根目录
2. 在项目根目录下递归发现所有 `*.test.ry` 文件（`.git`、`build`、`node_modules` 会被跳过）
3. 逐一运行并汇总结果

---

## 语法

### describe / it

```
describe("description", fn():
    it("test case name", fn():
        # test body
        expect(actual_value).to_eq(expected_value)
    )
)
```

- `describe` 和 `it` 接受描述字符串和 **lambda 参数** `fn():` 作为第二个参数
- `describe` 块内可以编写 `it` 块及其他语句（如变量声明等）
- 每个 `it` 块为独立的测试用例
- `describe` / `expect` 仅在 `ry test` 中可用（在普通的 `ry` 执行中会产生编译错误）

### 尾随块语法

任何函数调用（`describe`/`it`/`mock` 除外）都可以使用尾随块语法。在 `()` 后加上 `:` 会将缩进块作为无参数 lambda 传入最后的参数位置:

```
# 以下两者等价:
foo("arg"):
    bar()

foo("arg", fn():
    bar()
)
```

### expect / 匹配器

| 匹配器 | 说明 | 支持类型 |
|---|---|---|
| `to_eq(expected)` | 相等比较 | int, float, bool, str |
| `to_not_eq(expected)` | 不相等 | int, float, bool, str |
| `to_be_true()` | 为 `true` | bool |
| `to_be_false()` | 为 `false` | bool |
| `to_be_none()` | 为 `None` | Option |
| `to_be_some()` | Option 为 `Some` | Option |
| `to_be_ok()` | Result 为 `Ok` | Result |
| `to_be_err()` | Result 为 `Err` | Result |
| `to_contain(val)` | 容器包含值 | List, Set, Map, str |
| `to_not_contain(val)` | 容器不包含值 | List, Set, Map, str |
| `to_be_greater_than(v)` | `actual > v` | int, float |
| `to_be_less_than(v)` | `actual < v` | int, float |
| `to_be_greater_than_or_eq(v)` | `actual >= v` | int, float |
| `to_be_less_than_or_eq(v)` | `actual <= v` | int, float |
| `to_have_length(n)` | 长度为 `n` | List, Set, Map, str |
| `to_be_empty()` | 长度为 0 | List, Set, Map, str |
| `to_start_with(prefix)` | 字符串以 prefix 开头 | str |
| `to_end_with(suffix)` | 字符串以 suffix 结尾 | str |

### fail

立即将当前测试标记为失败。

```
it("should not reach here", fn():
    fail("unexpected error")
)
```

- `fail()` — 使用通用消息标记测试失败
- `fail(msg)` — 使用自定义消息标记测试失败
- `fail()` 之后执行继续进行（不会中止测试）
- 仅在 `ry test` 模式下可用

---

## 输出格式

```
Calculator
  + adds numbers
  + subtracts
  - fails test (red)
    line 10: expected 3, got 2

2 passed, 1 failed
```

- `+` 为成功（绿色），`-` 为失败（红色）
- 失败时显示行号与预期值/实际值

---

## 示例

```
describe("Arithmetic", fn():
    it("adds integers", fn():
        expect(1 + 2).to_eq(3)

    )
    it("compares strings", fn():
        expect("hello").to_eq("hello")

    )
    it("checks booleans", fn():
        expect(3 > 1).to_be_true()

    )
)
describe("Booleans", fn():
    it("false check", fn():
        expect(1 > 2).to_be_false()
    )
)
```

---

## 模拟（Mock）

### mock(fn_name, replacement)

在当前 `it` 块中将函数替换为模拟实现。`it` 块结束时模拟会自动清除。

```
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

- 第一个参数为函数名称（标识符，非字符串）
- 第二个参数为替换用 lambda
- 替换函数必须与原始函数具有相同的参数类型和返回类型
- `it` 块结束时模拟会自动恢复

### verify(fn_name)

返回模拟函数被调用的次数（`int`）。

```
describe("verify", fn():
    it("counts calls", fn():
        mock(fetch_data, fn() => "fake")
        fetch_data()
        fetch_data()
        expect(verify(fetch_data)).to_eq(2)
    )
)
```

### 限制

- 不支持重载函数的模拟
- 不支持使用捕获闭包进行模拟（仅支持纯 lambda）
- 不支持 `@native fn` 的模拟

---

## 参数化测试 (@each)

`@each` 可以用多组参数运行同一个测试。将元组列表附加到 `it` 块:

```
@each([
    (1, 2, 3),
    (0, 0, 0),
    (-1, 1, 0)
])
it("adds {0} + {1} = {2}", fn(a: int, b: int, expected: int):
    expect(a + b).to_eq(expected)
)
```

- 列表必须包含与 lambda 参数数量相同的元组
- 描述中的 `{0}`, `{1}`, ... 会被替换为参数值
- 每个元组生成一个独立的测试用例
- 支持的参数类型: `int`, `float`, `bool`, `str`

---

## 基于属性的测试 (@property)

`@property` 生成随机输入并多次运行测试:

```
@property(count=100)
it("addition is commutative", fn(a: int, b: int):
    expect(a + b).to_eq(b + a)
)
```

- `count=N` 指定随机试验次数（默认: 100）
- 失败时会显示反例（导致失败的输入值）
- 在第一次失败时停止测试
- 支持的参数类型: `int` ([-1000, 1000])、`float` ([-1000.0, 1000.0])、`bool`、`str` (随机 ASCII、0-20 字符)

---

## 测试覆盖率

使用 `--coverage`（或 `--cov`）标志来测量行覆盖率:

```bash
ry test --coverage                    # 所有测试 + 覆盖率摘要
ry test --cov tests/spec/math.test.ry # 单个文件
ry test --coverage tests/spec/        # 目录
```

### 输出

```
Test Coverage Summary:
  tests/spec/math.test.ry    100.0%  (74/74 lines)
  tests/spec/strings.test.ry  92.3%  (24/26 lines)
  -------------------------------------------------
  Total                        95.1%  (98/100 lines)
```

- 仅报告用户代码；标准库文件会被排除
- `--coverage` 与 `--parallel` 同时指定时，会退回为顺序执行

---

## 限制

- 不支持 `describe` 的嵌套
- 不支持 `before_each` / `after_each`
