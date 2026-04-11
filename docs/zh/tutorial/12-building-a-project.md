[English](../../tutorial/12-building-a-project.md) | [日本語](../../ja/tutorial/12-building-a-project.md) | [简体中文](12-building-a-project.md)

# 构建项目

[<- 上一篇：测试](11-testing.md)

在本教程中，你将构建一个**任务跟踪器** — 一个管理内存中待办事项列表的小应用程序。这个项目将把你学过的大部分语言特性串联起来：

- **Record 和 ADT enum**（数据建模）
- **集合和迭代器**（过滤和转换任务）
- **错误处理**（Result、Option、契约）
- **F 字符串和 UFCS**（可读的输出和链式调用）
- **带默认参数的函数**（灵活的 API）
- **模块**（跨文件组织代码）
- **测试**（验证行为）

---

## 项目设置

创建一个新项目：

```bash
ry new task-tracker
cd task-tracker
```

这会生成以下结构：

```
task-tracker/
  package.toml
  src/
    main.ry
```

---

## 步骤 1：定义数据模型

创建 `src/model.ry`，定义任务 Record 和状态 enum：

```python
enum Status:
    Todo
    InProgress
    Done

record Task:
    id: int
    title: str
    status: Status
    invariant:
        length(title) > 0

function create_task(id: int, title: str, status: Status = Status::Todo) -> Task:
    require:
        length(title) > 0
    return Task(id, title, status)
```

这里同时使用了多个特性：
- **ADT enum** 用于 `Status`（来自[Record 和 Enum](06-records.md)）
- **Record 不变条件** 确保标题永远不为空（来自[错误处理](08-error-handling.md)）
- **默认参数** 使 `status` 默认为 `Todo`（来自[函数](05-functions.md)）
- **契约**（`require`）作为构造时的安全检查（来自[错误处理](08-error-handling.md)）

---

## 步骤 2：任务操作

在 `src/model.ry` 中添加处理任务列表的函数：

```python
function add_task(tasks: List<Task>, title: str) -> List<Task>:
    id = length(tasks) + 1
    task = create_task(id, title)
    append(tasks, task)
    return tasks

function find_task(tasks: List<Task>, id: int) -> Option<Task>:
    for t in tasks:
        if t.id == id:
            return Some(t)
    return None

function complete_task(tasks: List<Task>, id: int) -> Result<List<Task>, Error>:
    case find_task(tasks, id):
        Some(t):
            t.status = Status::Done
            return Ok(tasks)
        None:
            return Err(Error(f"task {id} not found"))

function pending_tasks(tasks: List<Task>) -> List<Task>:
    return tasks
        .iter()
        .filter((t: Task) => t.status == Status::Todo)
        .to_list()
```

注意以下模式：
- **Option** 用于 `find_task` — 任务可能不存在（来自[错误处理](08-error-handling.md)）
- **Result** 用于 `complete_task` — 完成不存在的任务是一个错误
- **迭代器管道** 与 UFCS 链式调用在 `pending_tasks` 中使用（来自[集合](07-collections.md)和[函数](05-functions.md)）
- **F 字符串** 用于错误消息（来自[变量和类型](02-variables-and-types.md)）

---

## 步骤 3：显示

在 `src/model.ry` 中添加显示函数：

```python
function format_task(t: Task) -> str:
    marker = "[ ]"
    case t.status:
        Status::Todo:
            marker = "[ ]"
        Status::InProgress:
            marker = "[~]"
        Status::Done:
            marker = "[x]"
    return f"{marker} {t.id}. {t.title}"

function print_tasks(tasks: List<Task>):
    if is_empty(tasks):
        print("No tasks.")
        return
    for t in tasks:
        print(format_task(t))
```

---

## 步骤 4：主程序

编辑 `src/main.ry`：

```python
from model import create_task, add_task, complete_task, pending_tasks, print_tasks, Status, Task

tasks: List<Task> = []

# 添加一些任务
tasks = add_task(tasks, "Buy groceries")
tasks = add_task(tasks, "Write documentation")
tasks = add_task(tasks, "Review pull request")

print("All tasks:")
print_tasks(tasks)

# 完成一个任务
case complete_task(tasks, 1):
    Ok(updated):
        tasks = updated
    Err(e):
        print(f"Error: {e}")

print("\nPending tasks:")
print_tasks(pending_tasks(tasks))
```

运行程序：

```bash
ry src/main.ry
```

预期输出：

```
All tasks:
[ ] 1. Buy groceries
[ ] 2. Write documentation
[ ] 3. Review pull request

Pending tasks:
[ ] 2. Write documentation
[ ] 3. Review pull request
```

---

## 步骤 5：编写测试

创建 `tests/model.test.ry`：

```python
from model import create_task, add_task, find_task, complete_task, pending_tasks, Status, Task

describe("Task model", ():
    it("creates a task with default status", ():
        t = create_task(1, "Test task")
        expect(t.status == Status::Todo).to_be_true()
    )

    it("adds tasks to a list", ():
        tasks: List<Task> = []
        tasks = add_task(tasks, "First")
        tasks = add_task(tasks, "Second")
        expect(length(tasks)).to_eq(2)
    )

    it("finds a task by id", ():
        tasks: List<Task> = []
        tasks = add_task(tasks, "Target")
        case find_task(tasks, 1):
            Some(t):
                expect(t.title).to_eq("Target")
            None:
                fail("expected to find task")
    )

    it("returns None for missing task", ():
        tasks: List<Task> = []
        expect(find_task(tasks, 99)).to_be_none()
    )

    it("completes a task", ():
        tasks: List<Task> = []
        tasks = add_task(tasks, "Do it")
        case complete_task(tasks, 1):
            Ok(updated):
                remaining = pending_tasks(updated)
                expect(is_empty(remaining)).to_be_true()
            Err(e):
                fail("unexpected error")
    )

    it("returns error for invalid id", ():
        tasks: List<Task> = []
        case complete_task(tasks, 999):
            Ok(_):
                fail("expected error")
            Err(e):
                expect(e.message != "").to_be_true()
    )
)
```

运行测试：

```bash
ry test
```

---

## 下一步

以下是一些扩展此项目的方式：

- **添加截止日期** — 使用 `Option<str>` 添加可选的截止日期字段
- **持久化到 JSON** — 使用 `json` 模块将任务保存到文件或从文件加载
- **添加优先级** — 将 `Status` 扩展为带有优先级级别的 ADT
- **并发处理** — 使用 `@parallel` 并行处理一批任务

你现在已经拥有了扎实的 Ry 基础。请查阅[参考手册](../README.md)以了解完整的语言规格。

---

[<- 上一篇：测试](11-testing.md)
