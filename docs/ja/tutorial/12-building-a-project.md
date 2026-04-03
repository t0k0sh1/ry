[English](../../tutorial/12-building-a-project.md) | [日本語](12-building-a-project.md) | [简体中文](../../zh/tutorial/12-building-a-project.md)

# プロジェクトを作る

[← 前: テスト](11-testing.md)

このチュートリアルでは、**タスクトラッカー** — インメモリの To-Do リストを管理する小さなアプリケーションを構築します。このプロジェクトでは、これまで学んだ言語機能のほとんどを活用します:

- **Record と ADT enum**（データモデリング）
- **コレクションとイテレータ**（タスクのフィルタリングと変換）
- **エラーハンドリング**（Result, Option, 契約）
- **F 文字列と UFCS**（読みやすい出力とメソッドチェーン）
- **デフォルト引数付き関数**（柔軟な API）
- **モジュール**（ファイル間のコード整理）
- **テスト**（動作の検証）

---

## プロジェクトのセットアップ

新しいプロジェクトを作成します:

```bash
ry new task-tracker
cd task-tracker
```

以下の構造が生成されます:

```
task-tracker/
  package.toml
  src/
    main.ry
```

---

## ステップ 1: データモデルの定義

`src/model.ry` を作成し、タスクの Record とステータスの enum を定義します:

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

ここでは複数の機能を同時に使っています:
- **ADT enum** による `Status`（[Record と Enum](06-records.md) より）
- **Record 不変条件** でタイトルが空でないことを保証（[エラーハンドリング](08-error-handling.md) より）
- **デフォルト引数** で `status` のデフォルトを `Todo` に設定（[関数](05-functions.md) より）
- **契約**（`require`）による構築時の安全チェック（[エラーハンドリング](08-error-handling.md) より）

---

## ステップ 2: タスク操作

タスクリストを操作する関数を `src/model.ry` に追加します:

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
    match find_task(tasks, id):
        case Some(t):
            t.status = Status::Done
            return Ok(tasks)
        case None:
            return Err(Error(f"task {id} not found"))

function pending_tasks(tasks: List<Task>) -> List<Task>:
    return tasks
        .iter()
        .filter((t: Task) => t.status == Status::Todo)
        .to_list()
```

使われているパターンに注目してください:
- **Option** を `find_task` で使用 — タスクが存在しない可能性がある（[エラーハンドリング](08-error-handling.md) より）
- **Result** を `complete_task` で使用 — 存在しないタスクの完了はエラー
- **イテレータパイプライン** と UFCS チェーンを `pending_tasks` で使用（[コレクション](07-collections.md) と [関数](05-functions.md) より）
- **F 文字列** をエラーメッセージで使用（[変数と型](02-variables-and-types.md) より）

---

## ステップ 3: 表示

表示関数を `src/model.ry` に追加します:

```python
function format_task(t: Task) -> str:
    marker = "[ ]"
    match t.status:
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

## ステップ 4: メインプログラム

`src/main.ry` を編集します:

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
match complete_task(tasks, 1):
    case Ok(updated):
        tasks = updated
    case Err(e):
        print(f"Error: {e}")

print("\nPending tasks:")
print_tasks(pending_tasks(tasks))
```

実行します:

```bash
ry src/main.ry
```

期待される出力:

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

## ステップ 5: テストの作成

`tests/model.test.ry` を作成します:

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
        match find_task(tasks, 1):
            case Some(t):
                expect(t.title).to_eq("Target")
            case None:
                fail("expected to find task")
    )

    it("returns None for missing task", ():
        tasks: List<Task> = []
        expect(find_task(tasks, 99)).to_be_none()
    )

    it("completes a task", ():
        tasks: List<Task> = []
        tasks = add_task(tasks, "Do it")
        match complete_task(tasks, 1):
            case Ok(updated):
                remaining = pending_tasks(updated)
                expect(is_empty(remaining)).to_be_true()
            case Err(e):
                fail("unexpected error")
    )

    it("returns error for invalid id", ():
        tasks: List<Task> = []
        match complete_task(tasks, 999):
            case Ok(_):
                fail("expected error")
            case Err(e):
                expect(e.message != "").to_be_true()
    )
)
```

テストを実行します:

```bash
ry test
```

---

## 次のステップ

このプロジェクトを拡張する方法をいくつか紹介します:

- **期日の追加** — `Option<str>` を使ってオプショナルな期日フィールドを追加
- **JSON への永続化** — `json` モジュールを使ってタスクをファイルに保存・読み込み
- **優先度の追加** — `Status` を優先度レベルを持つ ADT に拡張
- **並行処理** — `@parallel` を使ってタスクのバッチを並列処理

これで Ry の基礎は固まりました。言語の完全な仕様については[リファレンス](../README.md)ドキュメントをご覧ください。

---

[← 前: テスト](11-testing.md)
