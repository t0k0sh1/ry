[English](../../reference/gc.md) | [日本語](../../ja/reference/gc.md) | [简体中文](gc.md)

# GC 参考

`gc` 包提供对循环收集器的控制。循环收集器是与 ARC（自动引用计数）协同工作的安全网，用于检测和回收循环引用链。

## 概述

Ry 使用 ARC 进行内存管理，大多数释放操作是即时且确定性的。然而，ARC 本身无法回收循环引用（例如 A -> B -> A）。循环收集器使用类似 CPython 的试探性删除算法，定期查找并释放这类不可达的循环。

使用 `weak` 引用仍然是避免循环的推荐方式，但循环收集器可以捕获用户遗漏的情况。

## 导入

```python
from gc import collect, enable, disable, set_threshold
```

## 函数

| 函数 | 签名 | 说明 |
|----------|-----------|-------------|
| `collect` | `() -> int` | 执行一次完整的收集周期。返回收集的对象数量。 |
| `enable` | `() -> Unit` | 启用自动收集（默认已启用）。 |
| `disable` | `() -> Unit` | 禁用自动收集。适用于性能关键的代码段。 |
| `set_threshold` | `(n: int) -> Unit` | 设置自动收集的候选计数阈值（默认值：700）。 |

## 工作原理

1. **候选跟踪**：当 `arc_release` 减少对象的引用计数但计数仍大于零时，该对象会被添加到候选集合中（仅限于可能形成循环的类型）。
2. **试探性删除**：收集器暂时减少候选对象引用的所有对象的引用计数。如果某个对象的计数仅通过试探性删除就降为零，则说明它只在候选集合内可达——它是循环的一部分。
3. **复活检查**：仍可从候选集合外部访问的对象会恢复其引用计数。
4. **收集**：剩余的不可达对象被释放。

## 静态分析优化

编译器在编译时执行静态分析，以识别哪些类型可能形成引用循环（例如递归 ADT enum）。只有这些类型参与 GC 候选跟踪。非循环类型的 GC 开销为零。

## 示例

```python
from gc import collect, disable, enable

# Disable automatic collection for a performance-critical section
disable()

# ... performance-critical code ...

# Re-enable and force a collection
enable()
collected = collect()
print(f"Collected {collected} cyclic objects")
```
