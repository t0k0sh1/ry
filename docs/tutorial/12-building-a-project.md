[English](12-building-a-project.md) | [日本語](../ja/tutorial/12-building-a-project.md) | [繁體中文](../zh/tutorial/12-building-a-project.md)

# Building a Project

[<- Prev: Testing](11-testing.md)

In this tutorial, you will build a **task tracker** — a small application that manages an in-memory to-do list. This project ties together most of the language features you have learned:

- **Records and ADT enums** (data modeling)
- **Collections and iterators** (filtering and transforming tasks)
- **Error handling** (Result, Option, contracts)
- **F-strings and UFCS** (readable output and chaining)
- **Functions with default arguments** (flexible API)
- **Modules** (organizing code across files)
- **Testing** (verifying behavior)

---

## Project Setup

Create a new project:

```bash
ry new task-tracker
cd task-tracker
```

This gives you:

```
task-tracker/
  package.toml
  src/
    main.ry
```

---

## Step 1: Define the Data Model

Create `src/model.ry` with the task record and status enum:

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

This uses several features at once:
- **ADT enum** for `Status` (from [Records and Enums](06-records.md))
- **Record invariant** to ensure the title is never empty (from [Error Handling](08-error-handling.md))
- **Default argument** so `status` defaults to `Todo` (from [Functions](05-functions.md))
- **Contract** (`require`) as a safety net on construction (from [Error Handling](08-error-handling.md))

---

## Step 2: Task Operations

Add functions to `src/model.ry` for working with task lists:

```python
function add_task(tasks: List<Task>, title: str) -> List<Task>:
    id = length(tasks) + 1
    task = create_task(id, title)
    tasks.append(task)
    return tasks

function find_task(tasks: List<Task>, id: int) -> Option<Task>:
    for t in tasks:
        if t.id == id:
            return Some(t)
    return None

function complete_task(tasks: List<Task>, id: int) -> Result<List<Task>, Error>:
    when find_task(tasks, id):
        case Some(t):
            t.status = Status::Done
            return Ok(tasks)
        case None:
            return Err(Error(f"task {id} not found"))

function pending_tasks(tasks: List<Task>) -> List<Task>:
    return tasks
        .iter()
        .filter(function(t: Task) => t.status == Status::Todo)
        .to_list()
```

Notice the patterns:
- **Option** for `find_task` — a task may not exist (from [Error Handling](08-error-handling.md))
- **Result** for `complete_task` — completing a nonexistent task is an error
- **Iterator pipeline** with UFCS chaining in `pending_tasks` (from [Collections](07-collections.md) and [Functions](05-functions.md))
- **F-string** for the error message (from [Variables and Types](02-variables-and-types.md))

---

## Step 3: Display

Add a display function to `src/model.ry`:

```python
function format_task(t: Task) -> str:
    marker = "[ ]"
    when t.status:
        case Status::Todo:
            marker = "[ ]"
        case Status::InProgress:
            marker = "[~]"
        case Status::Done:
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

## Step 4: Main Program

Edit `src/main.ry`:

```python
from model import create_task, add_task, complete_task, pending_tasks, print_tasks, Status, Task

tasks: List<Task> = []

# Add some tasks
tasks = add_task(tasks, "Buy groceries")
tasks = add_task(tasks, "Write documentation")
tasks = add_task(tasks, "Review pull request")

print("All tasks:")
print_tasks(tasks)

# Complete a task
when complete_task(tasks, 1):
    case Ok(updated):
        tasks = updated
    case Err(e):
        print(f"Error: {e}")

print("\nPending tasks:")
print_tasks(pending_tasks(tasks))
```

Run it:

```bash
ry src/main.ry
```

Expected output:

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

## Step 5: Write Tests

Create `tests/model.test.ry`:

```python
from model import create_task, add_task, find_task, complete_task, pending_tasks, Status, Task

describe("Task model", function():
    it("creates a task with default status", function():
        t = create_task(1, "Test task")
        expect(t.status == Status::Todo).to_be_true()
    )

    it("adds tasks to a list", function():
        tasks: List<Task> = []
        tasks = add_task(tasks, "First")
        tasks = add_task(tasks, "Second")
        expect(length(tasks)).to_eq(2)
    )

    it("finds a task by id", function():
        tasks: List<Task> = []
        tasks = add_task(tasks, "Target")
        when find_task(tasks, 1):
            case Some(t):
                expect(t.title).to_eq("Target")
            case None:
                fail("expected to find task")
    )

    it("returns None for missing task", function():
        tasks: List<Task> = []
        expect(find_task(tasks, 99)).to_be_none()
    )

    it("completes a task", function():
        tasks: List<Task> = []
        tasks = add_task(tasks, "Do it")
        when complete_task(tasks, 1):
            case Ok(updated):
                remaining = pending_tasks(updated)
                expect(is_empty(remaining)).to_be_true()
            case Err(e):
                fail("unexpected error")
    )

    it("returns error for invalid id", function():
        tasks: List<Task> = []
        when complete_task(tasks, 999):
            case Ok(_):
                fail("expected error")
            case Err(e):
                expect(e.message != "").to_be_true()
    )
)
```

Run the tests:

```bash
ry test
```

---

## Next Steps

Here are some ways to extend this project:

- **Add due dates** using `Option<str>` for an optional due date field
- **Persist to JSON** using the `json` module to save/load tasks from a file
- **Add priorities** by extending `Status` into an ADT with priority levels
- **Concurrency** — process a batch of tasks in parallel using `@parallel`

You now have a solid foundation in Ry. Explore the [Reference](../README.md) documentation for the full language specification.

---

[<- Prev: Testing](11-testing.md)
