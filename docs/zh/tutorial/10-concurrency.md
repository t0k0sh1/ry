[English](../../tutorial/10-concurrency.md) | [日本語](../../ja/tutorial/10-concurrency.md) | [简体中文](10-concurrency.md)

# 并发

[<- 上一篇：包](09-modules.md) | [下一篇：测试 ->](11-testing.md)

Ry 提供三种并发和并行执行模型：**async/await** 用于轻量级 I/O 密集型任务，**@parallel** 用于数据并行循环，**OS 线程**用于需要细粒度控制的 CPU 密集型工作负载。

---

## async/await

`Task<T>` 是并发工作的运行时句柄。使用 `async function` 声明返回任务的函数，在另一个 `async function` 中使用 `await` 等待任务完成，从同步上下文使用 `block_on(task)`。

### 定义异步函数

```python
async function add(a: int, b: int) -> int:
    return a + b
```

调用 `async function` 会立即返回 `Task<T>` —— 工作在后台开始：

```python
t: Task<int> = add(20, 22)
```

### 等待结果

从**同步**代码中，使用 `block_on()`：

```python
print(block_on(t))             # 42
print(block_on(add(1, 2)))     # 3
```

从**异步**代码中，使用 `await`：

```python
async function double_add(a: int, b: int) -> int:
    result = await add(a, b)
    return result * 2

print(block_on(double_add(3, 4)))   # 14
```

### 组合异步函数

你可以自然地链接异步操作：

```python
async function fetch_score() -> int:
    return 42

async function process() -> str:
    score = await fetch_score()
    return f"Score: {score * 2}"

print(block_on(process()))   # Score: 84
```

> **为什么使用 async/await？** 它让你编写看起来像顺序代码的并发代码。运行时在线程池上高效调度任务 —— 非常适合大部分时间都在等待的 I/O 密集型工作。

> **常见错误**：在非 async 函数中使用 `await` 会导致编译错误。从同步上下文调用时，请改用 `block_on()`。

---

## @parallel

`@parallel` 指令将计数 `for` 循环并行化到多个 CPU 核心：

```python
@parallel
for i in range(8):
    print(i)
```

### 限制

`@parallel for` 设计用于独立的迭代。它拒绝：
- **`break` 和 `continue`** —— 并行迭代之间没有有意义的顺序。
- **写入外部可变变量** —— 这会导致数据竞争。

仅支持计数循环（`range(...)` 或整数 `..` 范围）。

> **为什么使用 @parallel？** 这是并行化独立工作的最简单方式。无需锁、无需原子操作 —— 只需添加指令，运行时会处理分发。

---

## OS 线程

对于 CPU 密集型任务或需要细粒度同步时，使用 `thread` 模块创建 OS 线程。

```python
from thread import thread_spawn, thread_join, atomic_int_new, atomic_int_load, atomic_int_add
```

### 创建线程

`thread_spawn` 接受一个闭包并启动新的 OS 线程：

```python
counter = atomic_int_new(0)

t = thread_spawn(():
    atomic_int_add(counter, 1)
)

thread_join(t)
print(atomic_int_load(counter))   # 1
```

> **注意**：捕获的变量会被复制到线程中。对于原始类型（`int`、`str` 等），这会产生一个独立的副本。对于不透明句柄类型（`AtomicInt`、`Lock` 等），底层资源是共享的 —— 这正是同步所需要的。

### 使用原子操作共享状态

`AtomicInt` 提供无锁的线程安全整数操作：

```python
from thread import thread_spawn, thread_join, atomic_int_new, atomic_int_load, atomic_int_add

counter = atomic_int_new(0)

t1 = thread_spawn(():
    atomic_int_add(counter, 1)
)
t2 = thread_spawn(():
    atomic_int_add(counter, 1)
)

thread_join(t1)
thread_join(t2)
print(atomic_int_load(counter))   # 2
```

### 使用锁进行互斥

当你需要保护临界区（不止单个原子操作）时，使用 `Lock`：

```python
from thread import thread_spawn, thread_join, lock_new, lock_acquire, lock_release
from thread import atomic_int_new, atomic_int_load, atomic_int_add

lock = lock_new()
counter = atomic_int_new(0)

t = thread_spawn(():
    lock_acquire(lock)
    # 临界区：同一时间只有一个线程
    atomic_int_add(counter, 1)
    lock_release(lock)
)

lock_acquire(lock)
atomic_int_add(counter, 1)
lock_release(lock)

thread_join(t)
print(atomic_int_load(counter))   # 2
```

### 其他同步原语

`thread` 模块还提供：

| 原语 | 使用场景 |
|------|---------|
| `RWLock` | 多个读者或一个写者（读多写少的场景） |
| `Semaphore` | 限制对资源的并发访问 |
| `Barrier` | 等待 N 个线程到达同一点 |
| `AtomicBool` | 线程安全的布尔标志 |

完整 API 请参阅[线程参考手册](../reference/thread.md)。

---

## 网络（TCP 套接字）

Ry 透过 `net` 模组提供 TCP 套接字支援。网路操作返回 `Result` 类型（来自[错误处理](08-error-handling.md)），因为连接可能失败。

核心 TCP 原语：

| 函数 | 说明 |
|------|------|
| `bind(host, port)` | 分配一个监听器。返回 `Result<TcpListener, Error>` |
| `listen(listener, backlog)` | 开始接受连接。返回 `Result<Unit, Error>` |
| `accept(listener)` | 等待下一个连接。返回 `Result<TcpStream, Error>` |
| `connect(host, port)` | 打开一个出站流。返回 `Result<TcpStream, Error>` |
| `send(stream, bytes)` | 发送 `List<u8>`。返回 `Result<int, Error>` |
| `receive(stream, max)` | 读取最多 `max` 位元组。返回 `Result<List<u8>, Error>` |
| `close(handle)` | 释放 `TcpListener`、`TcpStream` 或 `TlsStream` |

以下是一个在 async 伺服器与同步客户端之间进行 echo 交换的范例：

```python
from net import bind, listen, accept, connect, listener_port
from io import to_bytes, bytes_to_str

async function echo_server(server: TcpListener) -> str:
    case accept(server):
        Ok(conn):
            case receive(conn, 4096):
                Ok(data):
                    case send(conn, data):
                        Ok(_):
                            ...
                        Err(e):
                            ...
                Err(e):
                    ...
            close(conn)
        Err(e):
            ...
    close(server)
    return "done"

function run_client(port: int) -> str:
    case connect("127.0.0.1", port):
        Ok(conn):
            case send(conn, to_bytes("hello")):
                Ok(_):
                    ...
                Err(e):
                    ...
            case receive(conn, 4096):
                Ok(resp):
                    case bytes_to_str(resp):
                        Ok(msg):
                            close(conn)
                            return msg
                        Err(e):
                            ...
                Err(e):
                    ...
            close(conn)
            return "fail"
        Err(e):
            return "fail"

case bind("127.0.0.1", 0):
    Ok(server):
        case listen(server, 1):
            Ok(_):
                port = listener_port(server)
                t = echo_server(server)
                print(run_client(port))   # hello
                block_on(t)
            Err(e):
                print(e.message)
    Err(e):
        print(e.message)
```

完整 TCP API 请参阅[网路参考手册](../reference/net.md)，其中包括 TLS（`tls_connect`）与每个流的超时设定。

---

## 选择合适的模型

| 特性 | `async`/`await` | `@parallel` | `thread` 模块 |
|------|----------------|-------------|--------------|
| 最适合 | I/O 密集型任务 | 数据并行循环 | CPU 密集型、细粒度控制 |
| 开销 | 低（任务调度） | 低（自动） | 较高（OS 线程创建） |
| 同步 | 通过 `await` 自动 | 不需要（独立迭代） | 手动（Lock、Semaphore 等） |
| 复杂度 | 中等 | 低 | 高 |

---

## 常见错误

1. **在 `async function` 外使用 `await`**：从同步上下文应使用 `block_on()` 代替。
2. **在 `@parallel` 中写入外部变量**：编译时会被拒绝以防止数据竞争。
3. **忘记 `thread_join`**：如果主程序在线程完成前退出，线程的工作可能会丢失。
4. **在线程间共享原始变量**：原始类型会被复制到闭包中。使用 `AtomicInt` 或 `Lock` 进行状态共享。

---

## 练习

1. **async/await**：编写两个 `async function` 函数 —— 一个返回名字，一个返回姓氏。编写第三个 `async function`，`await` 两者并返回全名。使用 `block_on()` 打印结果。

2. **线程与原子操作**：创建 5 个线程，每个线程向共享的 `AtomicInt` 加 10。在 join 所有线程后，验证计数器为 50。

---

[<- 上一篇：包](09-modules.md) | [下一篇：测试 ->](11-testing.md)
