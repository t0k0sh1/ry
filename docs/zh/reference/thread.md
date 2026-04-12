[English](../../reference/thread.md) | [日本語](../../ja/reference/thread.md) | [简体中文](thread.md)

# 线程参考

`thread` 包提供操作系统级别的线程原语，用于 CPU 密集型并行工作负载和细粒度的并发控制。

## 类型

| 类型 | 说明 |
|------|-------------|
| `Thread` | OS 线程的不透明句柄 |
| `Lock` | 互斥锁的不透明句柄 |
| `RWLock` | 读写锁的不透明句柄 |
| `Semaphore` | 计数信号量的不透明句柄 |
| `Barrier` | 线程屏障的不透明句柄 |
| `AtomicInt` | 原子 64 位整数的不透明句柄 |
| `AtomicBool` | 原子布尔值的不透明句柄 |

所有类型都是由 ARC（自动引用计数）管理的不透明指针。使用相应的 `*_new()` 函数创建实例。当不再被引用时，资源会自动清理——显式调用 `*_free()` 是可选的，但支持用于立即清理。

## 导入

```python
from thread import thread_spawn, thread_join, lock_new, lock_acquire, lock_release
```

## Thread

| 函数 | 签名 | 说明 |
|----------|-----------|-------------|
| `thread_spawn` | `(body: function() -> T) -> Thread` | 创建并启动一个执行 `body` 的新 OS 线程。捕获的变量按值复制。`T` 可以是 `Unit`、`int`、`float` 或 `bool`；请参阅下方的限制章节。 |
| `thread_join` | `(thread: Thread) -> Result<T, Error>` | 等待线程完成并以 `Ok(value)` 返回工作线程的值。对已 join 的 `Thread` 再次 join 会返回 `Err("thread already joined")`。 |

### 示例 — 副作用工作线程（`Unit`）

```python
from thread import thread_spawn, thread_join, atomic_int_new, atomic_int_load, atomic_int_add

counter = atomic_int_new(0)
t = thread_spawn(():
  atomic_int_add(counter, 1)
)
thread_join(t)
print(atomic_int_load(counter))  # 1
```

### 示例 — 返回值的工作线程（`int` / `float` / `bool`）

```python
from thread import thread_spawn, thread_join

t = thread_spawn(() => 42)
case thread_join(t):
  Ok(v):
    print(v)       # 42
  Err(e):
    print(e.message)

# 捕获也适用于返回值的工作线程：
x = 10
t2 = thread_spawn(() => x * x)
case thread_join(t2):
  Ok(v):
    print(v)       # 100
  Err(_):
    print("error")
```

> **注意：** 捕获的变量会被复制到线程的环境中。对于基本类型（int、float、bool、str），这会产生一个独立的副本。对于不透明句柄类型（Lock、AtomicInt 等），指针被复制，共享底层资源——这是同步原语的预期行为。

### 限制（MVP）

- **返回类型。** 工作线程可以返回 `Unit`、`int`、`float` 或 `bool`。ARC 管理的类型（`str`、`List`、`Map`、`Set`、记录）和 sum 类型（`Option`、`Result`、enum）尚不支持；传递此类工作线程会产生指向后续 issue 的代码生成错误。
- **Lambda 主体形式。** 仅表达式主体的 lambda（`() => <expr>`）可以返回值。块主体的 lambda 仍可用于 `Unit` 工作线程，但目前无法携带非 `Unit` 返回值。
- **变量引用工作线程。** `thread_spawn(my_fn)` 仍将 `my_fn` 视为 `Unit` 工作线程；目前读取其返回值需要使用内联 lambda 形式。
- **Panic。** 工作线程内的运行时 panic（例如除以零、数组越界、契约违反）会终止整个进程。它目前不会作为 `Err` 从 `thread_join` 返回 — 这需要单独重构 Ry 的 panic 机制，作为后续 issue 跟踪。

## Lock（互斥锁）

| 函数 | 签名 | 说明 |
|----------|-----------|-------------|
| `lock_new` | `() -> Lock` | 创建一个新的互斥锁。 |
| `lock_acquire` | `(lock: Lock) -> Result<Unit, Error>` | 获取锁。阻塞直到可用。 |
| `lock_release` | `(lock: Lock) -> Result<Unit, Error>` | 释放锁。 |
| `lock_free` | `(lock: Lock) -> Unit` | 立即释放锁。可选——ARC 会自动处理清理。 |

## RWLock（读写锁）

| 函数 | 签名 | 说明 |
|----------|-----------|-------------|
| `rwlock_new` | `() -> RWLock` | 创建一个新的读写锁。 |
| `rwlock_read_lock` | `(rwlock: RWLock) -> Result<Unit, Error>` | 获取共享读锁。允许多个读者同时持有。 |
| `rwlock_write_lock` | `(rwlock: RWLock) -> Result<Unit, Error>` | 获取独占写锁。阻塞直到所有读者和写者释放。 |
| `rwlock_unlock` | `(rwlock: RWLock) -> Result<Unit, Error>` | 释放锁（共享或独占）。 |
| `rwlock_free` | `(rwlock: RWLock) -> Unit` | 立即释放读写锁。可选——ARC 会自动处理清理。 |

## Semaphore

| 函数 | 签名 | 说明 |
|----------|-----------|-------------|
| `semaphore_new` | `(count: int) -> Result<Semaphore, Error>` | 创建具有给定初始计数的信号量。如果计数为负则返回 `Err`。 |
| `semaphore_acquire` | `(sem: Semaphore) -> Result<Unit, Error>` | 递减信号量。如果计数为零则阻塞。 |
| `semaphore_release` | `(sem: Semaphore) -> Result<Unit, Error>` | 递增信号量并唤醒一个等待的线程。 |
| `semaphore_free` | `(sem: Semaphore) -> Unit` | 立即释放信号量。可选——ARC 会自动处理清理。 |

## Barrier

| 函数 | 签名 | 说明 |
|----------|-----------|-------------|
| `barrier_new` | `(count: int) -> Result<Barrier, Error>` | 创建同步 `count` 个线程的屏障。如果计数不为正则返回 `Err`。 |
| `barrier_wait` | `(barrier: Barrier) -> Result<Unit, Error>` | 阻塞直到所有 `count` 个线程都调用了 `barrier_wait`。 |
| `barrier_free` | `(barrier: Barrier) -> Unit` | 立即释放屏障。可选——ARC 会自动处理清理。 |

## AtomicInt

| 函数 | 签名 | 说明 |
|----------|-----------|-------------|
| `atomic_int_new` | `(value: int) -> AtomicInt` | 创建具有给定初始值的原子整数。 |
| `atomic_int_load` | `(atomic: AtomicInt) -> int` | 原子地读取值。 |
| `atomic_int_store` | `(atomic: AtomicInt, value: int) -> Unit` | 原子地写入值。 |
| `atomic_int_add` | `(atomic: AtomicInt, delta: int) -> int` | 原子地加上 `delta` 并返回**之前**的值。 |
| `atomic_int_sub` | `(atomic: AtomicInt, delta: int) -> int` | 原子地减去 `delta` 并返回**之前**的值。 |
| `atomic_int_cas` | `(atomic: AtomicInt, expected: int, desired: int) -> bool` | 比较并交换：如果当前值等于 `expected`，则设置为 `desired` 并返回 `true`；否则返回 `false`。 |
| `atomic_int_free` | `(atomic: AtomicInt) -> Unit` | 立即释放原子整数。可选——ARC 会自动处理清理。 |

## AtomicBool

| 函数 | 签名 | 说明 |
|----------|-----------|-------------|
| `atomic_bool_new` | `(value: bool) -> AtomicBool` | 创建具有给定初始值的原子布尔值。 |
| `atomic_bool_load` | `(atomic: AtomicBool) -> bool` | 原子地读取值。 |
| `atomic_bool_store` | `(atomic: AtomicBool, value: bool) -> Unit` | 原子地写入值。 |
| `atomic_bool_free` | `(atomic: AtomicBool) -> Unit` | 立即释放原子布尔值。可选——ARC 会自动处理清理。 |

## 与 async/await 的比较

| 特性 | `async`/`await` | `thread` 包 |
|---------|----------------|-----------------|
| 执行模型 | 工作窃取线程池 | 专用 OS 线程 |
| 适用场景 | I/O 密集型任务、大量轻量级任务 | CPU 密集型任务、细粒度控制 |
| 同步方式 | 通过 `await` 自动同步 | 通过 Lock、Semaphore 等手动同步 |
| 开销 | 低（任务调度） | 较高（OS 线程创建） |
