[English](../../reference/control-flow.md) | [日本語](control-flow.md) | [繁體中文](../../zh/reference/control-flow.md)

# 制御構文リファレンス

## if / elif / else

### 構文

```python
if 条件式:
    # then ブロック
elif 条件式:
    # elif ブロック（複数可）
else:
    # else ブロック（省略可）
```

### 条件式の型

| 型 | false になる値 | true になる値 |
|---|---|---|
| `bool` | `false` | `true` |
| `int` | `0` | 非 0 |

`float` や `str` は条件式に直接使用できない。

### 例

```python
x = 10

if x > 5:
    print("big")
elif x == 5:
    print("five")
else:
    print("small")
```

### スコープルール

- `if` / `elif` / `else` の各ブロックはそれぞれ独立したブロックスコープを持つ。
- ブロック内で宣言した変数はブロック外からアクセスできない。

```python
if true:
    y = 42
# y はここではアクセス不可
```

---

## while

### 構文

```python
while 条件式:
    # ループ本体
```

条件式が `true` の間、ループ本体を繰り返す。

### 例

```python
i = 0
while i < 5:
    print(i)
    i += 1
```

### break / continue との組み合わせ

```python
i = 0
while true:
    if i >= 3:
        break
    i += 1
```

---

## for

### 構文

```python
# リスト / セット走査
for x in iterable_expr:
    # x に各要素が代入される

# range（0 始まり）
for i in range(n):
    # i = 0, 1, ..., n-1

# range（開始・終了指定）
for i in range(start, end):
    # i = start, start+1, ..., end-1

# range（ステップ指定）
for i in range(start, end, step):
    # i = start, start+step, start+2*step, ...
```

### マップのキー・値走査

```python
for k, v in map_expr:
    # k はキー、v は各エントリの値
```

### タプル分解代入

タプルのリストを走査する際、タプルの要素数に合わせて N 個の変数に分解できます。`_` で値を破棄できます。

```python
xs = [10, 20, 30]

for i, x in enumerate(xs):
    print(f"{i}: {x}")    # 0: 10, 1: 20, 2: 30

for a, b in zip([1, 2], [10, 20]):
    print(a + b)          # 11, 22

for _, x in enumerate(xs):
    print(x)              # インデックスを破棄

# N要素の分解代入（3個以上の変数）
triples = [(1, 2, 3), (4, 5, 6)]
for a, b, c in triples:
    print(a + b + c)      # 6, 15

for a, _, c in triples:
    print(a + c)          # 4, 10（中間の要素を破棄）
```

### 範囲演算子（`..`）

`..` 演算子は両端を含む整数の範囲を生成します。`1 .. 5` は `[1, 2, 3, 4, 5]` を生成します。

```python
for i in 1 .. 5:
    print(i)     # 1 2 3 4 5
```

### 例

```python
xs = [10, 20, 30]
for x in xs:
    print(x)

s = {1, 2, 3}
for x in s:
    print(x)

for i in range(5):
    print(i)     # 0 1 2 3 4

for i in range(2, 6):
    print(i)     # 2 3 4 5

for i in range(0, 10, 2):
    print(i)     # 0 2 4 6 8

for i in range(10, 0, -3):
    print(i)     # 10 7 4 1

# マップの走査
m = {"a": 1, "b": 2}
for k, v in m:
    print(k)
    print(v)

# 範囲演算子
for i in 1 .. 3:
    print(i)     # 1 2 3
```

---

## spawn / await

`spawn` はユーザー定義関数またはラムダ呼び出しをランタイムの worker pool で開始し、`Task<T>` を返します。`await` と `join(task)` はどちらも task の完了を待機します。

```python
fn square(x: int) -> int:
    return x * x

t: Task<int> = spawn square(12)
print(await t)          # 144
u: Task<int> = spawn square(3)
print(join(u))          # 9, await の関数形式
```

### 制約

- `spawn` は関数呼び出し式にのみ使用できます。
- 呼び出し先はユーザー定義関数またはラムダに限られます。
- v1 の `spawn` は `Unit` 戻り値の呼び出しをサポートしません。
- task は OS スレッド 1 本ごとではなく、ランタイム上の軽量 job として実行されます。
- `await` は `Task<T>` にのみ使用できます。
- `await expr` は式としても文としても使えます。

## channels

`Channel<T>` は task 間で値を受け渡すための組み込みブロッキング通信プリミティブです。

```python
fn worker(ch: Channel<int>) -> int:
    send(ch, 42)
    close(ch)
    return 0

ch: Channel<int> = channel[int]()
t: Task<int> = spawn worker(ch)
for x in ch:
    print(x)
print(join(t))
```

### ルール

- `channel[T]()` は unbuffered channel を作成します。
- `channel[T](n)` は容量 `n` の buffered channel を作成します。
- `send(ch, value)` は受け入れられるまで待機します。
- `try_send(ch, value)` は送信を即時に試み、`bool` を返します。
- `recv(ch)` は strict な受信 API で、値が届くまで待機します。
- `recv_opt(ch)` は値が届くか、channel が close 済みかつ drained になるまで待機します。
- `try_recv(ch)` は受信を即時に試み、`Option<T>` または `Channel<Unit>` では `bool` を返します。
- `for x in ch:` は channel が close 済みかつ drained になるまで値を反復します。
- `close(ch)` は channel を閉じます。
- v1 では、閉じた channel への `send` と、空の closed channel からの `recv` はランタイムエラーです。
- `recv_opt(ch: Channel<T>) -> Option<T>` は受信した値を `Some(v)` として返し、channel が close 済みかつ drained なら `None` を返します。
- `recv_opt(ch: Channel<Unit>) -> bool` は `Unit` を受信したら `true`、close 済みかつ drained なら `false` を返します。
- `try_recv(ch: Channel<T>) -> Option<T>` は値が即時に取れれば `Some(v)`、そうでなければ `None` を返します。close 済みかつ drained な channel でも `None` です。
- `try_recv(ch: Channel<Unit>) -> bool` は `Unit` を即時に受信できれば `true`、そうでなければ `false` を返します。
- `for _ in ch:` を使うと `Channel<Unit>` を消費できます。

## select

`select` は複数の channel 操作を待ち合わせし、ready になった branch を 1 つだけ実行します。

```python
select:
    case let x = recv(inbox):
        print(x)
    case send(outbox, 42):
        print("sent")
    else:
        print("idle")
```

### Rules

- `select` は式ではなく文です。
- case は `case let name = recv(ch):`、`case let name = recv_opt(ch):`、`case send(ch, value):` のみです。
- 受信値を捨てる場合は `let _ = recv(ch)` を使います。
- `recv_opt` を使う case では `Option<T>`、`Channel<Unit>` の場合は `bool` が束縛されます。
- `else:` は省略可能で、書く場合は最後の branch に限られます。
- `timeout n:` は select 全体の待機上限をミリ秒で指定する最後の branch です。
- `else` と `timeout` は同時に指定できません。
- `else` が無い場合、いずれかの case が ready になるまで block します。
- v1 では `select` 内でも closed channel のエラーは変わりません。閉じた channel への `send` と、空の closed channel からの `recv` はランタイムエラーです。

---

## `@parallel for`

`@parallel` は `range(...)` または整数 `..` を使う counted `for` ループにだけ付与できます。ループ本体はランタイムのワーカープール上でチャンク単位に並列実行されます。

```python
@parallel
for i in range(8):
    print(i)
```

### 制約

- 対応するのは `range(...)` と整数 `..` のみです。
- 分解代入付きの反復は未対応です。
- 外側スコープのミュータブル変数への代入は拒否されます。
- `break` と `continue` は使えません。
- v1 ではループ本体内のインデックス代入とフィールド代入も拒否されます。

ランタイムの worker 数は `available_parallelism()` で取得できます。

---

## break

- 最も内側のループ（`while` または `for`）を即座に脱出する。
- ループの外で使用するとコンパイルエラー。

```python
for i in range(10):
    if i == 5:
        break    # i == 5 で脱出
    print(i)     # 0 1 2 3 4
```

### エラー例

```python
# ループ外での break はコンパイルエラー
break   # Error: break outside loop
```

---

## continue

- 最も内側のループの現在のイテレーションを終了し、次のイテレーションへスキップする。
- ループの外で使用するとコンパイルエラー。

```python
for i in range(5):
    if i == 2:
        continue   # i == 2 をスキップ
    print(i)       # 0 1 3 4
```

---

## `...`（Ellipsis）

- 何もしない文（no-op）。空ブロックのプレースホルダーとして使用する。
- 関数ボディ、`if`/`elif`/`else`、`while`、`for`、`match case` など任意のブロック内で使用可能。

```python
fn not_yet():
    ...

if true:
    ...
else:
    ...
```

---

## match

### 構文

```python
match 式:
    case パターン:
        # 本体
    case パターン if ガード条件:
        # ガード付き本体
    case _:
        # ワイルドカード（何にでもマッチ）
```

### パターンの種類

| パターン | 例 | 説明 |
|----------|-----|------|
| ワイルドカード | `_` | 何にでもマッチ |
| リテラル | `0`, `"hello"`, `true` | 値の等値比較 |
| 変数束縛 | `n` | 何にでもマッチし、変数に束縛 |
| enum バリアント | `Color::Red` | enum タグの比較（単純な enum） |
| ADT enum バリアント | `Shape::Circle(r)` | 関連データを持つ enum バリアントにマッチし、束縛する |
| `Some(x)` | `Some(v)` | Option が値ありの場合、中身を束縛 |
| `None` | `None` | Option が値なしの場合 |
| OR パターン | `1 \| 2 \| 3` | いずれかにマッチ |

### guard 節

`case パターン if 条件式:` の形式でガード条件を指定できる。パターンがマッチし、かつガード条件が真の場合にのみアームが実行される。

### OR パターン

複数のパターンを `|` で結合し、いずれかにマッチさせることができます。変数束縛（`n`、`Some(x)`、`Ok(v)`、`Err(e)`）は OR パターン内では使用できません。

```python
match x:
    case 1 | 2 | 3:
        print("small")
    case _:
        print("other")

# enum の OR パターン
match color:
    case Color::Red | Color::Blue:
        print("warm or cool")
    case Color::Green:
        print("green")
```

### 網羅性チェック

- enum 型: すべてのバリアントをカバーするか `_` が必要。OR パターンの各選択肢は個別にカウントされる。
- Option 型: `Some` と `None` の両方をカバーするか `_` が必要。
- bool 型: `true` と `false` の両方をカバーするか `_` が必要。
- int / float / str リテラル: `_` が必須。
- ガード付きアームは網羅性にカウントされない。

### 例

```python
# enum マッチ
enum Color:
    Red
    Green
    Blue

match color:
    case Color::Red:
        print("red")
    case Color::Green:
        print("green")
    case Color::Blue:
        print("blue")

# Option マッチ
x: Option<int> = Some(42)
match x:
    case Some(v):
        print(v)
    case None:
        print("nothing")

# リテラルマッチ
match x:
    case 0:
        print("zero")
    case 1:
        print("one")
    case _:
        print("other")

# guard 節
match x:
    case n if n > 0:
        print("positive")
    case n if n < 0:
        print("negative")
    case _:
        print("zero")
```

### ADT enum マッチ

enum バリアントが関連データを持つ場合、バインディングパターンを使って値を取り出します。

```python
enum Shape:
    Circle(float)
    Rectangle(float, float)
    Point

s = Shape::Circle(3.14)
match s:
    case Shape::Circle(r):
        print(r)        # 3.14
    case Shape::Rectangle(w, h):
        print(w)
        print(h)
    case Shape::Point:
        print("point")
```

複数フィールドを持つバリアントは、宣言順に各フィールドを別々の名前に束縛します。

### スコープルール

- 各 `case` アームはブロックスコープを持つ。
- 変数束縛パターン (`n`) や `Some(x)` で束縛された変数はそのアーム内でのみ有効。

---

## スコープルール

### ブロックスコープ

- `if` / `elif` / `else` / `while` / `for` / `match` の各ブロックはブロックスコープを持つ。
- ブロック内で宣言した変数はブロックの終了と同時にスコープから外れる。

```python
for i in range(3):
    tmp = i * 2
# tmp はここではアクセス不可

if true:
    a = 1
# a はここではアクセス不可
```

### 内側スコープでの再代入

- 内側のスコープで変数に代入すると、外側の変数が変更される（Python スタイルのスコーピング）。
- シャドーイングは行われず、内側の代入は同じ変数を変更する。

```python
x = 10
if true:
    x = 99   # 外側の x を変更
    print(x)     # 99
print(x)         # 99
```
