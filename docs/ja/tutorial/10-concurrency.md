[English](../../tutorial/10-concurrency.md) | [日本語](10-concurrency.md) | [繁體中文](../../zh/tutorial/10-concurrency.md)

# 並行処理

[<- 前: パッケージ](09-modules.md) | [次: テスト ->](11-testing.md)

Ry には並行・並列実行のための3つのモデルがあります: 軽量な I/O バウンドタスク向けの **async/await**、データ並列ループ向けの **@parallel**、きめ細かい制御が必要な CPU バウンドワークロード向けの **OS スレッド**。

---

## async/await

`Task<T>` は並行処理のためのランタイムハンドルです。タスクを返す関数を宣言するには `async function` を使い、別の `async function` 内でタスクの完了を待つには `await` を、同期コンテキストからは `block_on(task)` を使います。

### 非同期関数の定義

```python
async function add(a: int, b: int) -> int:
    return a + b
```

`async function` を呼ぶと即座に `Task<T>` が返されます -- 処理はバックグラウンドで開始されます:

```python
t: Task<int> = add(20, 22)
```

### 結果の待機

**同期**コードからは `block_on()` を使います:

```python
print(block_on(t))             # 42
print(block_on(add(1, 2)))     # 3
```

**非同期**コードからは `await` を使います:

```python
async function double_add(a: int, b: int) -> int:
    result = await add(a, b)
    return result * 2

print(block_on(double_add(3, 4)))   # 14
```

### 非同期関数の合成

非同期操作を自然にチェーンできます:

```python
async function fetch_score() -> int:
    return 42

async function process() -> str:
    score = await fetch_score()
    return f"Score: {score * 2}"

print(block_on(process()))   # Score: 84
```

> **なぜ async/await なのか?** 逐次的なコードのように読める並行コードを書けます。ランタイムがスレッドプール上で効率的にタスクをスケジューリングします -- 待ち時間が大半を占める I/O バウンドな処理に最適です。

> **よくあるミス**: 非 async 関数内で `await` を使うとコンパイルエラーになります。同期コンテキストからは代わりに `block_on()` を使ってください。

---

## @parallel

`@parallel` ディレクティブはカウント付き `for` ループを複数の CPU コアにまたがって並列化します:

```python
@parallel
for i in range(8):
    print(i)
```

### 制約事項

`@parallel for` は独立した反復のために設計されています。以下は拒否されます:
- **`break` と `continue`** -- 並列反復間には意味のある順序がありません。
- **外側の可変変数への書き込み** -- データ競合を引き起こします。

カウント付きループ（`range(...)` または整数の `..` レンジ）のみがサポートされています。

> **なぜ @parallel なのか?** 独立した作業を並列化する最も簡単な方法です。ロックもアトミック操作も不要 -- ディレクティブを追加するだけで、ランタイムが分散を処理します。

---

## OS スレッド

CPU バウンドなタスクや、きめ細かい同期が必要な場合は、`thread` モジュールを使って OS スレッドを作成します。

```python
from thread import thread_spawn, thread_join, atomic_int_new, atomic_int_load, atomic_int_add
```

### スレッドの生成

`thread_spawn` はクロージャを受け取り、新しい OS スレッドを開始します:

```python
counter = atomic_int_new(0)

t = thread_spawn(():
    atomic_int_add(counter, 1)
)

thread_join(t)
print(atomic_int_load(counter))   # 1
```

> **注意**: キャプチャされた変数はスレッドにコピーされます。プリミティブ型（`int`、`str` など）の場合、独立したコピーが生成されます。不透明ハンドル型（`AtomicInt`、`Lock` など）の場合、基盤となるリソースが共有されます -- これは同期のためにまさに望まれる動作です。

### アトミック操作による共有状態

`AtomicInt` はロックなしでスレッドセーフな整数操作を提供します:

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

### ロックによる相互排他

クリティカルセクション（単一のアトミック操作以上のもの）を保護する必要がある場合は、`Lock` を使います:

```python
from thread import thread_spawn, thread_join, lock_new, lock_acquire, lock_release
from thread import atomic_int_new, atomic_int_load, atomic_int_add

lock = lock_new()
counter = atomic_int_new(0)

t = thread_spawn(():
    lock_acquire(lock)
    # クリティカルセクション: 一度に1つのスレッドのみ
    atomic_int_add(counter, 1)
    lock_release(lock)
)

lock_acquire(lock)
atomic_int_add(counter, 1)
lock_release(lock)

thread_join(t)
print(atomic_int_load(counter))   # 2
```

### その他の同期プリミティブ

`thread` モジュールは以下も提供しています:

| プリミティブ | 用途 |
|-------------|------|
| `RWLock` | 複数のリーダーまたは1つのライター（読み取りが多いワークロード向け） |
| `Semaphore` | リソースへの同時アクセスを制限 |
| `Barrier` | N 個のスレッドが同じ地点に到達するまで待機 |
| `AtomicBool` | スレッドセーフなブーリアンフラグ |

完全な API については [Thread リファレンス](../reference/thread.md)を参照してください。

---

## ネットワーク（TCP ソケット）

Ry は `net` モジュールを通じて TCP ソケットをサポートしています。接続は失敗する可能性があるため、ネットワーク操作は `Result` 型（[エラーハンドリング](08-error-handling.md)参照）を返します。

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
```

完全な TCP API については[ネットワークリファレンス](../reference/net.md)を参照してください。

---

## 適切なモデルの選択

| 特徴 | `async`/`await` | `@parallel` | `thread` モジュール |
|------|----------------|-------------|-------------------|
| 最適な用途 | I/O バウンドタスク | データ並列ループ | CPU バウンド、きめ細かい制御 |
| オーバーヘッド | 低（タスクスケジューリング） | 低（自動） | 高（OS スレッド生成） |
| 同期 | `await` による自動同期 | 不要（独立した反復） | 手動（Lock、Semaphore 等） |
| 複雑さ | 中 | 低 | 高 |

---

## よくあるミス

1. **`async function` の外で `await` を使う**: 同期コンテキストからは代わりに `block_on()` を使ってください。
2. **`@parallel` 内で外側の変数に書き込む**: データ競合を防ぐため、コンパイル時に拒否されます。
3. **`thread_join` を忘れる**: メインプログラムがスレッド完了前に終了すると、スレッドの処理が失われる可能性があります。
4. **スレッド間でプリミティブ変数を共有する**: プリミティブ型はクロージャにコピーされます。共有状態には `AtomicInt` や `Lock` を使ってください。

---

## 演習

1. **async/await**: 名前を返す `async function` と姓を返す `async function` の2つを書いてください。両方を `await` してフルネームを返す3つ目の `async function` を書き、`block_on()` で結果を表示してください。

2. **アトミック操作付きスレッド**: 共有の `AtomicInt` にそれぞれ 10 を加算する5つのスレッドを生成してください。すべてのスレッドを join した後、カウンターが 50 であることを確認してください。

---

[<- 前: パッケージ](09-modules.md) | [次: テスト ->](11-testing.md)
