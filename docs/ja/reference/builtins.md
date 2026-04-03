[English](../../reference/builtins.md) | [日本語](builtins.md) | [繁體中文](../../zh/reference/builtins.md)

# 組み込み関数リファレンス

## 関数一覧

### コア

| 関数 | 説明 |
|------|------|
| `print()` / `print(expr1, expr2, ...)` | 値を標準出力に表示（スペース区切り） |
| `length(value)` | リスト・マップ・セットの要素数、文字列の UTF-8 文字数を返す |
| `range(n)` / `range(start, end)` / `range(start, end, step)` | 整数のリストを生成 |
| `exit(code)` | 指定した終了コードでプロセスを終了 |
| `arguments()` | コマンドライン引数を `List<str>` として返す |
| `available_parallelism()` | ランタイムの worker 数を `int` で返す |
| `sleep(duration_ms)` | 指定ミリ秒間、実行を一時停止する |
| `env(key)` | 環境変数を `Option<str>` で返す |
| `env(key, default)` | 環境変数を返す。未設定なら `default` を返す |
| `send(stream, data)` | `TcpStream` または `TlsStream` を通じて `List<u8>` を送信し、`Result<int, Error>` を返す |
| `receive(stream, max)` | `TcpStream` または `TlsStream` から最大 `max` バイトを `Result<List<u8>, Error>` として受信 |
| `close(handle)` | `TcpStream`、`TlsStream`、または `TcpListener` を閉じる |
| `block_on(task)` | 現在のスレッドを `Task<T>` の完了までブロックし、結果を返す |
| `to_str(value)` | 値を文字列表現に変換する（`int`、`float`、`bool`、`str`、record、enum、タプル、`List`、`Map`、`Set`、`Result`、`Option`） |
| `fail()` / `fail(message)` | 現在のテストを失敗としてマークする（`ry test` モードでのみ使用可能） |

### Option

| 関数 | 説明 |
|------|------|
| `Some(expr)` | Option 型の値ありバリアントを構築 |

### Result / Error

| 関数 | 説明 |
|------|------|
| `Ok(value)` | `Result<T, Error>` の成功バリアントを構築 |
| `Err(error)` | `Result<T, Error>` のエラーバリアントを構築 |
| `Error(message)` | メッセージ付きの `Error` 値を作成 |
| `Error(message, code)` | メッセージとエラーコード付きの `Error` 値を作成 |
| `result.and_then(closure)` | `Ok` の場合、`closure`（`Result<U, E>` を返す）を呼び出す。`Err` の場合はエラーをそのまま伝播 |
| `result.map(closure)` | `Ok` の場合、`closure` を値に適用し結果を `Ok` でラップ。`Err` の場合はエラーをそのまま伝播 |

### チェック付き演算

| 関数 | 説明 |
|------|------|
| `checked_add(a, b)` | オーバーフローなしなら `Ok(a + b)`、そうでなければ `Err(Error("arithmetic overflow"))` |
| `checked_sub(a, b)` | オーバーフローなしなら `Ok(a - b)`、そうでなければ `Err(Error("arithmetic overflow"))` |
| `checked_mul(a, b)` | オーバーフローなしなら `Ok(a * b)`、そうでなければ `Err(Error("arithmetic overflow"))` |
| `saturating_add(a, b)` | `a + b` を返す。オーバーフロー時は `int` 範囲にクランプ |
| `saturating_sub(a, b)` | `a - b` を返す。オーバーフロー時は `int` 範囲にクランプ |
| `saturating_mul(a, b)` | `a * b` を返す。オーバーフロー時は `int` 範囲にクランプ |
| `wrapping_add(a, b)` | オーバーフロー時にラッピングする `a + b` を返す |
| `wrapping_sub(a, b)` | オーバーフロー時にラッピングする `a - b` を返す |
| `wrapping_mul(a, b)` | オーバーフロー時にラッピングする `a * b` を返す |

### コレクション操作

| 関数 | 説明 |
|------|------|
| `has_key(map, key)` | マップにキーが存在するかを返す |
| `add(set, value)` | セットに要素を追加（重複は無視） |
| `remove(set, value)` | セットから要素を削除 |
| `append(list, value)` / `append!(list, value)` | リストの末尾に要素を追加（ミューテーション操作） |
| `appended(list, value)` | 要素を末尾に追加した新しいリストを返す（非破壊） |
| `pop(list)` | リストの末尾の要素を削除して `Option<T>` として返す |
| `reverse(list)` | 逆順の新しいリストを返す（文字列にも対応） |
| `reverse!(list)` | リストをその場で逆順にする（ミューテーション操作） |
| `slice(list, start, end)` | start から end までの新しい部分リストを返す |
| `take(list, count)` | 先頭 count 要素の新しいリストを返す |
| `tap(list, function)` | 各要素に function を呼び出し副作用を実行し、元のリストを返す |
| `filter(list, pred)` | 述語を満たす要素だけの新しいリストを返す |
| `map(list, function)` | 各要素を変換した新しいリストを返す |
| `sort(list)` / `sort(list, comp)` | ソート済みの新しいリストを返す（デフォルト昇順） |
| `sort!(list)` / `sort!(list, comp)` | リストをその場でソートする（ミューテーション操作） |
| `insert(list, i, val)` | インデックス i に要素を挿入 |
| `remove_at(list, i)` | インデックス i の要素を削除して返す |
| `items(map)` | (キー, 値) タプルのリストを返す |
| `remove(map, key)` | 指定したキーのエントリを削除 |
| `get(map, key)` | キーの値を `Option<V>` として返す |
| `get(map, key, default)` | キーの値を返す（存在しない場合はデフォルト値） |
| `union(set, set)` | 2つのセットの和集合を返す |
| `intersection(set, set)` | 2つのセットの積集合を返す |
| `difference(set, set)` | 2つのセットの差集合を返す |
| `symmetric_difference(set, set)` | 2つのセットの対称差を返す |
| `is_subset(set, set)` | 最初のセットが2番目の部分集合かを返す |
| `is_superset(set, set)` | 最初のセットが2番目の上位集合かを返す |
| `first(list)` | 最初の要素を `Option<T>` として返す。空なら `None` |
| `last(list)` | 最後の要素を `Option<T>` として返す。空なら `None` |
| `remove(list, value)` | リストから値の最初の出現を削除 |
| `is_empty(list)` | リストが空かを返す |
| `distinct(list)` | 重複を排除した新しいリストを返す |
| `flatten(list)` | ネストされたリストをフラット化した新しいリストを返す |
| `reduce(list, fn)` | リデューサ関数を使ってリストを単一の値に畳み込む |
| `fold(list, init, fn)` | 初期アキュムレータ値を使ってリストを畳み込む |
| `any(list, pred)` | 述語にマッチする要素が1つでもあれば `true` を返す |
| `all(list, pred)` | すべての要素が述語にマッチすれば `true` を返す |
| `sum(list)` | 全要素の合計を返す |
| `min(list)` | 最小の要素を返す |
| `max(list)` | 最大の要素を返す |
| `enumerate(list)` | `(インデックス, 値)` タプルのリストを返す |
| `zip(list1, list2)` | 2つのリストの要素をペアにした `(a, b)` タプルのリストを返す |
| `keys(map)` | すべてのキーを `List<K>` として返す |
| `values(map)` | すべての値を `List<V>` として返す |
| `merge(map1, map2)` | 両方のマップのエントリを含む新しいマップを返す |

### イテレータ

| 関数 | 説明 |
|------|------|
| `iter(collection)` | List、Set、Map から遅延イテレータを作成 |
| `next(iter)` | 次の要素を `Option<T>` として返す。使い切った場合は `None` |
| `to_list(iter)` | イテレータの残りの要素をすべて `List<T>` に収集 |
| `filter(iter, pred)` | 述語にマッチする要素のみを返す遅延イテレータを返す |
| `map(iter, function)` | 各要素を変換する遅延イテレータを返す |
| `take(iter, count)` | 最大 count 要素を返す遅延イテレータを返す |

### [文字列操作](builtins-string.md)

| 関数 | 説明 |
|------|------|
| `contains(string, substring)` | 部分文字列が含まれるか |
| `starts_with(string, prefix)` | 接頭辞で始まるか |
| `ends_with(string, suffix)` | 接尾辞で終わるか |
| `find(string, substring)` | 部分文字列の文字位置（`Option<int>`） |
| `byte_len(string)` | 文字列のバイト長を返す |
| `substring(string, start, end)` | 部分文字列を取得 |
| `char_at(string, i)` | 指定位置の文字を取得 |
| `replace(string, old, new)` | 部分文字列を全置換 |
| `to_upper(string)` / `to_lower(string)` | 大文字・小文字変換 |
| `trim(string)` / `trim_start(string)` / `trim_end(string)` | 空白除去 |
| `repeat(string, count)` | 文字列を n 回繰り返す |
| `reverse(string)` | 文字列を逆順にする |
| `split(string, delimiter)` | 文字列を分割してリストを返す |
| `join(list, sep)` | リストの文字列をセパレータで結合 |
| `to_int(s)` / `to_float(s)` / `to_str(v)` | 型変換（`to_int` は `Result<int, Error>` を返す） |

-> 詳細は **[文字列操作関数リファレンス](builtins-string.md)** を参照

---

## print

**シグネチャ:** `print()` / `print(expr1, expr2, ...)`

1つ以上の値をスペース区切りで標準出力に表示します。末尾に改行が付きます。引数なしで呼び出すと改行のみを出力します。

| 型 | 出力形式 |
|----|---------|
| `int` | `%ld` |
| `float` | `%g` |
| `bool` | `true` / `false` |
| `str` | `%s` |
| `Result` (Ok) | `Ok(value)` |
| `Result` (Err) | `Err(value)` |
| `Option` (Some) | `Some(値)` |
| `Option` (None) | `None` |
| `list` | `[要素1, 要素2, ...]` |
| `map` | `{キー1: 値1, キー2: 値2, ...}` |
| `set` | `{要素1, 要素2, ...}` |
| `tuple` | `(要素1, 要素2, ...)` |
| `enum` | バリアント名（例: `Red`） |
| `record` | `RecordName(field: val, ...)` |

```python
print(42)          # 42
print(3.14)        # 3.14
print(true)        # true
print("hello")     # hello
print(Ok(42))      # Ok(42)
print(Err(Error("fail")))  # Err(Error: fail (code: 0))
print(Some(1))     # Some(1)
print(None)        # None
print([1, 2, 3])   # [1, 2, 3]
print({"a": 1})    # {a: 1}
print({1, 2, 3})   # {1, 2, 3}
print((1, "hello"))  # (1, hello)

# 複数引数（スペース区切り）
print(1, 2, 3)             # 1 2 3
print("hello", "world")   # hello world
print(1, "hello", true)   # 1 hello true
print()                    # （空行）
```

---

## Some

**シグネチャ:** `Some(expr) -> Option<T>`

Option 型の値ありバリアントを構築します。

```python
x: Option<int> = Some(42)
print(x)   # Some(42)
```

---

## length

**シグネチャ:** `length(x: List<T> | Map<K, V> | Set<T> | str) -> int`

リスト・マップ・セットの要素数、または文字列の UTF-8 文字数を返します。バイト長が必要な場合は `byte_len()` を使用してください。

```python
print(length([1, 2, 3]))         # 3
print(length({"a": 1, "b": 2})) # 2
print(length({1, 2, 3}))         # 3
print(length("hello"))           # 5
print(length("あいう"))           # 3 (UTF-8 文字数)
```

---

## has_key

**シグネチャ:** `has_key(m: Map<K, V>, key: K) -> bool`

マップに指定したキーが存在するかを返します。UFCS 記法も使用可能です。

```python
m = {"a": 1, "b": 2}
print(has_key(m, "a"))    # true
print(m.has_key("z"))     # false (UFCS)
```

---

## add

**シグネチャ:** `add(s: Set<T>, value: T)`

セットに要素を追加します。既に存在する要素を追加した場合は何もしません。UFCS 記法も使用可能です。

```python
s = {1, 2, 3}
s.add(4)          # UFCS
add(s, 5)         # 通常の呼び出し
s.add(1)          # 既に存在するため無視
print(length(s))     # 5
```

---

## remove

**シグネチャ:** `remove(s: Set<T>, value: T)`

セットから要素を削除します。UFCS 記法も使用可能です。

```python
s = {1, 2, 3}
s.remove(2)       # UFCS
print(2 in s)     # false
```

---

## range

**シグネチャ:** `range(n: int) -> List<int>` / `range(start: int, end: int) -> List<int>` / `range(start: int, end: int, step: int) -> List<int>`

整数のリストを生成します。

| 形式 | 生成される値 |
|------|------------|
| `range(n)` | `[0, 1, ..., n-1]` |
| `range(start, end)` | `[start, start+1, ..., end-1]` |
| `range(start, end, step)` | `[start, start+step, start+2*step, ...]` (`end` は含まない) |

- `step > 0` の場合、`start` から昇順に `end` に向かって生成します。
- `step < 0` の場合、`start` から降順に `end` に向かって生成します。
- `step == 0` の場合、ランタイムエラーになります。
- 範囲が空の場合（例: `range(0, 10, -1)`）、空リストを返します。

```python
print(range(3))           # [0, 1, 2]
print(range(2, 5))        # [2, 3, 4]
print(range(0, 10, 2))    # [0, 2, 4, 6, 8]
print(range(10, 0, -3))   # [10, 7, 4, 1]

for i in range(3):
    print(i)
# 0
# 1
# 2
```

---

## exit

**シグネチャ:** `exit(code: int)`

指定した終了コードでプロセスを即座に終了します。`exit()` 以降のコードは到達不能になります。

```python
exit(0)        # 正常終了
exit(1)        # エラー終了
```

---

## arguments

**シグネチャ:** `arguments() -> List<str>`

スクリプトに渡されたコマンドライン引数を文字列のリストとして返します。インタープリター名やスクリプトファイル名は含まれません -- スクリプトパスの後の引数のみです。

```python
# 実行: ry script.ry hello world
a = arguments()
print(length(a))    # 2
print(a[0])      # hello
print(a[1])      # world

for x in arguments():
    print(x)
```

---

## sleep

**シグネチャ:** `sleep(duration_ms: int) -> Unit`

指定されたミリ秒間、現在のスレッドの実行を一時停止します。`duration_ms` が 0 以下の場合は即座に返ります。

```python
sleep(1000)    # 1秒待機
sleep(0)       # 即座に返る
```

---

## env

**シグネチャ:** `env(key: str) -> Option<str>` / `env(key: str, default: str) -> str`

環境変数の値を返します。1引数の場合は `Option<str>`（設定済みなら `Some(value)`、未設定なら `None`）を返します。2引数の場合は値が未設定なら `default` を返します。

プロジェクトルート（`package.toml` が存在するディレクトリ）に `.env` ファイルがある場合、起動時に自動的に読み込まれます。既存の環境変数は `.env` の値で上書きされません。

> **セキュリティ上の注意:** `.env` ファイルには通常シークレット（API キー、データベースパスワード、トークン等）が含まれます。`.env` をバージョン管理にコミット**しないでください**（`.gitignore` 等に追加してください）。その内容は機密情報として扱ってください。

```python
# 1引数: Option<str> を返す
path = env("PATH")
match path:
    case Some(v):
        print(v)
    case None:
        print("PATH not set")

# 2引数: デフォルト値付き
port = env("PORT", "8080")
print(port)   # PORT 未設定なら "8080"
```

### `.env` ファイルの書式

```env
# コメントは # で始まる
DATABASE_URL=postgres://localhost/mydb
API_KEY="secret-key-123"
EMPTY_VALUE=
QUOTED='single quoted'
```

### 環境別 `.env` ファイル

`RY_ENV` が設定されている場合、Ry は以下の優先順位で環境別の `.env` ファイルを読み込みます:

- `.env.<環境名>` を最初に読み込む（例: `RY_ENV=dev` なら `.env.dev`）
- `.env` を次に読み込む（`.env.<環境名>` で設定済みの値は上書きされない）
- `RY_ENV=prod` の場合、`.env` ファイルは一切読み込まない（セキュリティ）
- `RY_ENV` が未設定の場合、`.env` のみ読み込む（後方互換）

環境モードの詳細は [RY_ENV](packages.md#ry_env) を参照してください。

---

## append

**シグネチャ:** `append(list: List<T>, value: T)`

リストの末尾に要素を追加します。これはミューテーション操作で、リストがその場で変更されます。UFCS 記法も使用可能です。

```python
xs = [1, 2]
xs.append(3)
print(xs)   # [1, 2, 3]
```

---

## pop

**シグネチャ:** `pop(list: List<T>) -> Option<T>`

リストの末尾の要素を削除して `Option<T>` として返します。リストが空の場合は `None` を返します。UFCS 記法も使用可能です。

```python
xs = [1, 2, 3]
v = xs.pop()
print(v)    # Some(3)
print(xs)   # [1, 2]
```

---

## reverse (list)

**シグネチャ:** `reverse(list: List<T>) -> List<T>`

要素を逆順にした新しいリストを返します。元のリストは変更されません。文字列に対しても使用できます（[文字列操作関数リファレンス](builtins-string.md)を参照）。UFCS 記法も使用可能です。

```python
xs = [1, 2, 3]
ys = reverse(xs)
print(ys)   # [3, 2, 1]
print(xs)   # [1, 2, 3]（変更なし）
```

---

## slice

**シグネチャ:** `slice(list: List<T>, start: int, end: int) -> List<T>`

`start`（含む）から `end`（含まない）までの新しい部分リストを返します。インデックスは有効範囲（`0` から `length(list)` まで）にクランプされます。UFCS 記法も使用可能です。

```python
xs = [1, 2, 3, 4, 5]
print(slice(xs, 1, 3))     # [2, 3]
print(slice(xs, 0, 100))   # [1, 2, 3, 4, 5]（クランプされる）
```

---

## take

**シグネチャ:** `take(list: List<T>, count: int) -> List<T>`

先頭 `count` 要素の新しいリストを返します。`count` がリストの長さを超える場合はリスト全体のコピーを返します。`count <= 0` の場合は空リストを返します。元のリストは変更されません。UFCS 記法も使用可能です。

```python
xs = [1, 2, 3, 4, 5]
ys = xs.take(3)
print(ys)   # [1, 2, 3]
print(xs.take(10))   # [1, 2, 3, 4, 5]（クランプされる）
print(xs.take(0))    # []
```

---

## tap

**シグネチャ:** `tap(list: List<T>, function: function(T) -> R) -> List<T>`

各要素に対して関数を呼び出し（戻り値は無視）、元のリストをそのまま返します。メソッドチェーン中のデバッグや副作用の挿入に有用です。UFCS 記法も使用可能です。

```python
xs = [1, 2, 3]
ys = xs.tap((x: int) => print(x)).map((x: int) => x * 2)
# 1, 2, 3 を出力し、ys = [2, 4, 6]
```

---

## filter

**シグネチャ:** `filter(list: List<T>, pred: function(T) -> bool) -> List<T>`

述語が `true` を返す要素のみを含む新しいリストを返します。元のリストは変更されません。UFCS 記法も使用可能です。

```python
xs = [1, 2, 3, 4, 5]
ys = xs.filter((x: int) => x > 3)
print(ys)   # [4, 5]
print(xs)   # [1, 2, 3, 4, 5]  （変更なし）
```

---

## map

**シグネチャ:** `map(list: List<T>, function: function(T) -> U) -> List<U>`

各要素を関数で変換した新しいリストを返します。出力の要素型は入力と異なっても構いません。元のリストは変更されません。UFCS 記法も使用可能です。

```python
xs = [1, 2, 3]
ys = xs.map((x: int) => x * 2)
print(ys)   # [2, 4, 6]
```

---

## sort

**シグネチャ:** `sort(list: List<T>) -> List<T>` / `sort(list: List<T>, comp: function(T, T) -> bool) -> List<T>`

ソート済みの新しいリストを返します。デフォルトは昇順です。カスタム比較関数を指定できます（第一引数が第二引数の前に来るべき場合に `true` を返す）。元のリストは変更されません。ソートは**安定**です（等しい要素の元の順序が保持されます）。UFCS 記法も使用可能です。

```python
xs = [3, 1, 2]
print(xs.sort())   # [1, 2, 3]

# 降順ソート
desc = xs.sort((a: int, b: int) => a > b)
print(desc)   # [3, 2, 1]
```

---

## sort!

**シグネチャ:** `sort!(list: List<T>)` / `sort!(list: List<T>, comp: function(T, T) -> bool)`

リストをその場でソートします。ソートアルゴリズムは `sort()` と同じですが、新しいリストを作成する代わりに元のリストを変更します。UFCS 記法も使用可能です。

```python
xs = [3, 1, 2]
xs.sort!()
print(xs)   # [1, 2, 3]
```

---

## reverse!

**シグネチャ:** `reverse!(list: List<T>)`

リストをその場で逆順にします。UFCS 記法も使用可能です。

```python
xs = [1, 2, 3]
xs.reverse!()
print(xs)   # [3, 2, 1]
```

---

## appended

**シグネチャ:** `appended(list: List<T>, value: T) -> List<T>`

要素を末尾に追加した新しいリストを返します。元のリストは変更されません。UFCS 記法も使用可能です。

```python
xs = [1, 2]
ys = xs.appended(3)
print(xs)   # [1, 2]（変更なし）
print(ys)   # [1, 2, 3]
```

---

## append!

**シグネチャ:** `append!(list: List<T>, value: T)`

`append()` のエイリアスです。リストの末尾に要素をその場で追加します。`!` 命名規約との一貫性のために提供されています。

---

## first

**シグネチャ:** `first(list: List<T>) -> Option<T>`

リストの最初の要素を `Option<T>` として返します。リストが空の場合は `None` を返します。

```python
print(first([10, 20, 30]))   # Some(10)
```

---

## last

**シグネチャ:** `last(list: List<T>) -> Option<T>`

リストの最後の要素を `Option<T>` として返します。リストが空の場合は `None` を返します。

```python
print(last([10, 20, 30]))   # Some(30)
```

---

## get (Map)

**シグネチャ:** `get(map: Map<K, V>, key: K) -> Option<V>` / `get(map: Map<K, V>, key: K, default: V) -> V`

2引数形式はキーの値を `Option<V>` として返します。3引数形式はキーの値またはデフォルト値を返します。

```python
m = {"a": 1, "b": 2}
print(get(m, "a"))       # Some(1)
print(get(m, "z"))       # None
print(get(m, "z", 0))   # 0
```

---

## iter

**シグネチャ:** `iter(collection: List<T> | Set<T>) -> Iterator<T>` / `iter(collection: Map<K, V>) -> Iterator<(K, V)>`

コレクションから遅延イテレータを作成します。イテレータはデータをコピーせず、元のコレクションを参照します。UFCS 記法も使用可能です。

- `List<T>` と `Set<T>` の場合、要素型は `T`。
- `Map<K, V>` の場合、要素型はタプル `(K, V)`。

```python
xs = [1, 2, 3]
it = xs.iter()           # Iterator<int>
ys = it.to_list()        # [1, 2, 3]

m = {"a": 1, "b": 2}
for k, v in m.iter():        # Iterator<(str, int)>
    print(k)
```

---

## next

**シグネチャ:** `next(iter: Iterator<T>) -> Option<T>`

イテレータから次の要素を `Option<T>` として返します。イテレータが使い切られた場合は `None` を返します。呼び出しごとにイテレータの内部状態が進みます。UFCS 記法も使用可能です。

```python
it = [10, 20].iter()
print(it.next())   # Some(10)
print(it.next())   # Some(20)
print(it.next())   # None
```

---

## to_list

**シグネチャ:** `to_list(iter: Iterator<T>) -> List<T>`

イテレータの残りの要素をすべて新しいリストに収集します。UFCS 記法も使用可能です。

```python
xs = [1, 2, 3, 4, 5]
ys = xs.iter().filter((x: int) => x > 2).to_list()
print(ys)   # [3, 4, 5]
```
