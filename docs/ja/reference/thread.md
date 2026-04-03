[English](../../reference/thread.md) | [日本語](thread.md) | [简体中文](../../zh/reference/thread.md)

# スレッドリファレンス

`thread` パッケージは、CPU バウンドの並列ワークロードやきめ細かい並行制御のための OS レベルのスレッドプリミティブを提供します。

## 型

| 型 | 説明 |
|----|------|
| `Thread` | OS スレッドの不透明ハンドル |
| `Lock` | ミューテックス（相互排他ロック）の不透明ハンドル |
| `RWLock` | 読み書きロックの不透明ハンドル |
| `Semaphore` | カウンティングセマフォの不透明ハンドル |
| `Barrier` | スレッドバリアの不透明ハンドル |
| `AtomicInt` | アトミック 64 ビット整数の不透明ハンドル |
| `AtomicBool` | アトミック真偽値の不透明ハンドル |

すべての型は ARC（自動参照カウント）で管理される不透明ポインタです。インスタンスの作成には対応する `*_new()` 関数を使用します。リソースは参照がなくなると自動的にクリーンアップされます。即座にクリーンアップするための `*_free()` 呼び出しはオプションですがサポートされています。

## インポート

```python
from thread import thread_spawn, thread_join, lock_new, lock_acquire, lock_release
```

## Thread

| 関数 | シグネチャ | 説明 |
|------|-----------|------|
| `thread_spawn` | `(body: function() -> Unit) -> Thread` | `body` を実行する新しい OS スレッドを作成・開始する。キャプチャされた変数は値によりコピーされる。 |
| `thread_join` | `(thread: Thread) -> Result<Unit, Error>` | スレッドの終了を待つ。スレッドがエラーを発生させた場合は `Err` を返す。join 後にスレッドハンドルは消費される。 |

### 使用例

```python
from thread import thread_spawn, thread_join, atomic_int_new, atomic_int_load, atomic_int_add

counter = atomic_int_new(0)
t = thread_spawn(():
  atomic_int_add(counter, 1)
)
thread_join(t)
print(atomic_int_load(counter))  # 1
```

> **注意:** キャプチャされた変数はスレッドの環境にコピーされます。プリミティブ型（int、float、bool、str）の場合、独立したコピーが作成されます。不透明ハンドル型（Lock、AtomicInt など）の場合、ポインタがコピーされ基盤となるリソースを共有します。これは同期プリミティブの意図された動作です。

## Lock（ミューテックス）

| 関数 | シグネチャ | 説明 |
|------|-----------|------|
| `lock_new` | `() -> Lock` | 新しいミューテックスを作成する。 |
| `lock_acquire` | `(lock: Lock) -> Result<Unit, Error>` | ロックを取得する。利用可能になるまでブロックする。 |
| `lock_release` | `(lock: Lock) -> Result<Unit, Error>` | ロックを解放する。 |
| `lock_free` | `(lock: Lock) -> Unit` | ロックを即座に解放する。オプション — ARC が自動的にクリーンアップする。 |

## RWLock（読み書きロック）

| 関数 | シグネチャ | 説明 |
|------|-----------|------|
| `rwlock_new` | `() -> RWLock` | 新しい読み書きロックを作成する。 |
| `rwlock_read_lock` | `(rwlock: RWLock) -> Result<Unit, Error>` | 共有読み取りロックを取得する。複数の読み取りが許可される。 |
| `rwlock_write_lock` | `(rwlock: RWLock) -> Result<Unit, Error>` | 排他書き込みロックを取得する。すべての読み取り・書き込みが解放されるまでブロックする。 |
| `rwlock_unlock` | `(rwlock: RWLock) -> Result<Unit, Error>` | ロック（共有または排他）を解放する。 |
| `rwlock_free` | `(rwlock: RWLock) -> Unit` | 読み書きロックを即座に解放する。オプション — ARC が自動的にクリーンアップする。 |

## Semaphore

| 関数 | シグネチャ | 説明 |
|------|-----------|------|
| `semaphore_new` | `(count: int) -> Result<Semaphore, Error>` | 指定した初期カウントでセマフォを作成する。カウントが負の場合は `Err` を返す。 |
| `semaphore_acquire` | `(sem: Semaphore) -> Result<Unit, Error>` | セマフォをデクリメントする。カウントがゼロの場合はブロックする。 |
| `semaphore_release` | `(sem: Semaphore) -> Result<Unit, Error>` | セマフォをインクリメントし、待機中のスレッドを起こす。 |
| `semaphore_free` | `(sem: Semaphore) -> Unit` | セマフォを即座に解放する。オプション — ARC が自動的にクリーンアップする。 |

## Barrier

| 関数 | シグネチャ | 説明 |
|------|-----------|------|
| `barrier_new` | `(count: int) -> Result<Barrier, Error>` | `count` 個のスレッドを同期するバリアを作成する。カウントが正でない場合は `Err` を返す。 |
| `barrier_wait` | `(barrier: Barrier) -> Result<Unit, Error>` | すべての `count` 個のスレッドが `barrier_wait` を呼び出すまでブロックする。 |
| `barrier_free` | `(barrier: Barrier) -> Unit` | バリアを即座に解放する。オプション — ARC が自動的にクリーンアップする。 |

## AtomicInt

| 関数 | シグネチャ | 説明 |
|------|-----------|------|
| `atomic_int_new` | `(value: int) -> AtomicInt` | 指定した初期値でアトミック整数を作成する。 |
| `atomic_int_load` | `(atomic: AtomicInt) -> int` | 値をアトミックに読み取る。 |
| `atomic_int_store` | `(atomic: AtomicInt, value: int) -> Unit` | 値をアトミックに書き込む。 |
| `atomic_int_add` | `(atomic: AtomicInt, delta: int) -> int` | `delta` をアトミックに加算し、**変更前**の値を返す。 |
| `atomic_int_sub` | `(atomic: AtomicInt, delta: int) -> int` | `delta` をアトミックに減算し、**変更前**の値を返す。 |
| `atomic_int_cas` | `(atomic: AtomicInt, expected: int, desired: int) -> bool` | Compare-and-swap: 現在の値が `expected` と等しい場合、`desired` に設定して `true` を返す。そうでなければ `false` を返す。 |
| `atomic_int_free` | `(atomic: AtomicInt) -> Unit` | アトミック整数を即座に解放する。オプション — ARC が自動的にクリーンアップする。 |

## AtomicBool

| 関数 | シグネチャ | 説明 |
|------|-----------|------|
| `atomic_bool_new` | `(value: bool) -> AtomicBool` | 指定した初期値でアトミック真偽値を作成する。 |
| `atomic_bool_load` | `(atomic: AtomicBool) -> bool` | 値をアトミックに読み取る。 |
| `atomic_bool_store` | `(atomic: AtomicBool, value: bool) -> Unit` | 値をアトミックに書き込む。 |
| `atomic_bool_free` | `(atomic: AtomicBool) -> Unit` | アトミック真偽値を即座に解放する。オプション — ARC が自動的にクリーンアップする。 |

## async/await との比較

| 特徴 | `async`/`await` | `thread` パッケージ |
|------|----------------|-------------------|
| 実行モデル | ワークスティーリングスレッドプール | 専用 OS スレッド |
| 適した用途 | I/O バウンドタスク、多数の軽量タスク | CPU バウンドタスク、きめ細かい制御 |
| 同期 | `await` による自動同期 | Lock、Semaphore 等による手動同期 |
| オーバーヘッド | 低い（タスクスケジューリング） | 高い（OS スレッド作成） |
