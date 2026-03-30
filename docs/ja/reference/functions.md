[English](../../reference/functions.md) | [日本語](functions.md) | [繁體中文](../../zh/reference/functions.md)

# 関数リファレンス

## 関数定義の構文

```python
fn 関数名(引数名: 型, ...) -> 戻り値型:
    # 本体
    return 値
```

- 引数の型は省略可能。省略時は `any` 型として扱われる。
- 戻り値型は省略可能。省略時は**ボディから推論**される（名前付き関数とラムダの両方）。`return` 文がなければ `Unit` に推論される。明示的に任意の戻り値型を許容するには `-> any` を指定する。
- 本体はインデントされたブロック。
- 明示的な戻り値型（`Unit` と `any` を除く）を持つ関数は、すべての制御フローパスで `return` 文が必要です。不足している場合はコンパイルエラーになります。
- 関数には `require`（事前条件）と `ensure`（事後条件）を定義できます。[契約による設計](contracts.md) を参照。

> **命名規則**: 関数名と引数名は snake_case（例: `add`、`get_value`、`map_list`）を使用する必要があります。コンパイラがこの規則を強制します。

```python
fn add(a: int, b: int) -> int:
    return a + b

fn greet(name: str) -> Unit:
    print("Hello, " + name)   # 戻り値型は Unit（明示的）
```

---

## 引数と戻り値の型

| 項目 | 説明 |
|---|---|
| 引数型 | 省略可能。`: 型` を省略すると `any` になる |
| 戻り値型 | 省略可能。省略時はボディから推論される（`return` 文がなければ `Unit`） |
| `Unit` | 値を返さない関数の戻り値型 |

> **注意**: 関数の引数は**不変**です。関数ボディ内で引数を再代入することはできません。これにより、事後条件チェックで引数のエントリ時の値を常に利用できます（[契約による設計](contracts.md) を参照）。

```python
fn no_return(x: int) -> Unit:  # 戻り値型 Unit（明示的）
    print(x)

fn get_value() -> int:     # 戻り値型 int
    return 42

fn identity(x) -> any:    # 引数型 any（省略）
    return x
```

### 型省略と `any`

引数の型アノテーションを省略すると、その引数は `any` として扱われます。`any` は実行時に任意のプリミティブ値を受け入れる動的型です。Python の型なし引数と同様の仕組みです。

```python
# すべての引数が any
fn add(a, b):
    return a + b

add(1, 2)              # 3（int + int）
add("hello", " world") # "hello world"（str + str）
add(1, 2.0)            # 3.0（int + float）
```

型アノテーションに `any` を明示的に書くこともできます:

```python
fn identity(x: any) -> any:
    return x
```

### 戻り値型の推論

戻り値型を省略すると、ボディ内の `return` 文から推論されます:

```python
fn double(x: int):     # 戻り値型は int に推論
    return x * 2

fn greet(name: str):   # 戻り値型は Unit に推論（return なし）
    print("Hello, " + name)
```

明示的に任意の戻り値型を許容するには `-> any` を指定します:

```python
fn flexible(x: any) -> any:
    return x    # int、float、str 等を返せる
```

---

## 再帰

関数は自身を呼び出せる。

```python
fn factorial(n: int) -> int:
    if n <= 1:
        return 1
    return n * factorial(n - 1)
```

### 末尾呼び出し最適化

コンパイラは、関数の最後の処理が自分自身の呼び出しである自己再帰末尾呼び出しを自動的に検出し、LLVM の `musttail` 最適化を適用します。これにより、末尾再帰関数は一定のスタック空間を使用することが保証され、深い再帰でのスタックオーバーフローを防ぎます。

```python
fn sum_to(n: int, acc: int) -> int:
    if n <= 0:
        return acc
    return sum_to(n - 1, acc + n)    # 末尾呼び出し → 最適化される

sum_to(1000000, 0)    # スタックオーバーフローなしで動作
```

**TCO の条件:**

- `return` 文で自分自身を直接呼び出している（`return f(args)`）
- 呼び出しの結果がそのまま返される（`return n * f(n-1)` は末尾呼び出しではない）
- 関数に `ensure`（事後条件）がない

相互再帰（A が B を呼び、B が A を呼ぶ）は現在最適化されません。

---

## オーバーロード

引数の数や型が異なる同名の関数を複数定義できる。

### ルール

- 引数の数または型が異なれば同名の関数を定義可能。
- 呼び出し時は引数の型と数に基づいて適切な関数が選択される。
- 戻り値型だけが異なるオーバーロードはできない。

```python
fn area(side: int) -> int:
    return side * side

fn area(w: int, h: int) -> int:
    return w * h

a = area(5)       # 25
b = area(3, 4)    # 12
```

### 解決優先順位

複数のオーバーロードが呼び出しに一致する場合、コンパイラは以下の優先順位（高い順）で最も具体的なものを選択します:

1. **完全型一致** -- 引数の型がパラメータの型に完全に一致
2. **暗黙の拡大変換** -- 安全な拡大変換（`u8` → `int`、`u8` → `float`、`int` → `float`）
3. **union 型一致** -- 引数の型が union パラメータ型のメンバー
4. **`any` 型一致** -- パラメータの型が `any`（任意の値を受け入れる）

完全一致が最も多いオーバーロードが選ばれます。2つ以上のオーバーロードが同じ具体性を持つ場合、コンパイラは曖昧性エラーを報告します。

低レベル数値型（`i8`、`i16`、`i32`、`i64`、`u8`～`u64`、`f32`）は暗黙の拡大変換に**参加しません** -- 明示的な `as` キャストが必要です。

```python
fn process(x: int) -> str:
    return "int"

fn process(x) -> str:          # x: any
    return "any"

process(42)       # "int" — 完全一致（int）が any に勝つ
process("hello")  # "any" — str の完全一致がないため any にフォールバック
```

```python
fn double(x: float) -> float:
    return x * 2.0

double(5)         # OK — int が暗黙的に float に拡大変換され、10.0 を返す
```

---

## デフォルト引数

パラメータにデフォルト値を指定でき、呼び出し時に末尾の引数を省略できる。

### 構文

```python
fn connect(host: str, port: int = 8080, timeout: int = 30):
    # ...

connect("localhost")                    # port=8080, timeout=30
connect("localhost", 3000)              # port=3000, timeout=30
connect("localhost", 3000, 10000)       # port=3000, timeout=10000
```

### ルール

- デフォルトパラメータは非デフォルトパラメータの後に配置する。
- デフォルト値を持つパラメータには明示的な型注釈が**必須**（例: `x: int = 10`; `x = 10` はコンパイルエラー）。
- デフォルト値はコンパイル時定数式（リテラルおよび `@const` 変数）に限定。
- デフォルト引数により曖昧なオーバーロード（arity 範囲の重複 + 型の一致）が生じる場合、コンパイルエラーとなる。

```python
# エラー: 曖昧なオーバーロード
fn calc(x: int, y: int = 0) -> int:
    return x + y
fn calc(x: int) -> int:      # 上の calc(int) と衝突
    return x * 2
```

### 制限事項

- **ジェネリック関数**および**ラムダ式**ではデフォルト引数は未サポート。

---

## Unit 型関数

戻り値のない関数は `Unit` を返す。戻り値型は省略可能（`Unit` に推論される）、または `-> Unit` で明示的に指定可能。

```python
fn log(msg: str) -> Unit:
    print(msg)
```

---

## Task と async 関数

`Task<T>` は並行実行の組み込みハンドル型です。`async fn` は `Task<T>` を返し、`await` は別の `async fn` 内で `T` を取り出します。`block_on(task)` は同期コンテキストからタスクの完了までブロックします。

```python
async fn add(a: int, b: int) -> int:
    return a + b

# 同期コンテキストからは block_on() を使用
t: Task<int> = add(20, 22)
print(block_on(t))                  # 42
block_on(add(1, 2))                 # 待機して結果を捨てる

# async fn 内では await を使用
async fn double_add(a: int, b: int) -> int:
    return (await add(a, b)) * 2
```

### ルール

- `async fn name(...) -> T:` の `T` は await 後の値型です。
- `async fn` の呼び出し結果は常に `Task<T>` です。
- `await expr` は `Task<T>` にのみ使用でき、結果は `T` です。
- `await` は `async fn` 内でのみ使用可能。同期コンテキストからは `block_on(task)` を使用します。
- `block_on(task)` は現在のスレッドをタスク完了までブロックし、結果を返します。
- `async fn ... -> Unit` をサポートします。値を返さない task の待機には `block_on(task)` を使うのが基本です。
- task はランタイムの worker pool 上で実行され、task ごとに OS スレッドを作る実装ではありません。
- `async` ラムダと `async @native fn` は v1 では未対応です。

---

## ラムダ関数

無名関数をその場で定義できる。

### 構文

```python
# 単一式（式の値が返る。戻り値型は推論）
fn(引数名: 型, ...) => 式

# 引数型の省略（any がデフォルト）
fn(引数名, ...) => 式

# 複数行ブロック
fn(引数名: 型, ...):
    # 複数の文
    return 値

# 戻り値型の明示（省略可能）
fn(引数名: 型, ...) -> 戻り値型 => 式
```

### 例

```python
double = fn(x: int) => x * 2
result = double(5)   # 10

add = fn(a: int, b: int) => a + b
sum = add(3, 4)      # 7

# 複数行ラムダ
abs = fn(x: int):
    if x < 0:
        return -x
    return x
```

---

## クロージャ

ラムダ関数は定義された時点の外側スコープの変数を**値でキャプチャ**する。

```python
base = 10
add_base = fn(x: int) => x + base   # base を値でキャプチャ

base = 99          # キャプチャ済みの値には影響しない
r = add_base(5)   # 15（キャプチャ時の base = 10 を使用）
```

### キャプチャルール

| 項目 | 内容 |
|---|---|
| キャプチャ方式 | 値キャプチャ（コピー） |
| キャプチャタイミング | ラムダ定義時 |
| 外側変数の変更の影響 | ない（コピーのため） |

---

## 関数型

関数を値として扱うための型。

### 構文

```python
fn(引数型1, 引数型2, ...) -> 戻り値型
```

### 例

```python
f: fn(int) -> int = fn(x: int) => x * 2
g: fn(int, int) -> int = fn(a: int, b: int) => a + b

fn apply(func: fn(int) -> int, x: int) -> int:
    return func(x)

result = apply(f, 5)   # 10
```

---

## 高階関数

関数を引数として受け取ったり、戻り値として返したりできる。

```python
fn map_list(xs: List<int>, f: fn(int) -> int) -> List<int>:
    result: List<int> = []
    for x in xs:
        result += [f(x)]
    return result

doubled = map_list([1, 2, 3], fn(x: int) => x * 2)
# [2, 4, 6]
```

---

## ジェネリック関数

関数は型パラメータを持つことができ、コード重複なしに型安全な再利用を実現します。

### 構文

```python
fn name<T, U>(param1: T, param2: U) -> T:
    # T、U を型として使用するボディ
```

### 例

```python
fn identity<T>(x: T) -> T:
    return x

# 明示的な型引数
result = identity[int](42)      # 42
result = identity[str]("hello") # "hello"

# 型推論（実引数から型引数を推論）
result = identity(42)           # T = int, result = 42
result = identity("hello")     # T = str, result = "hello"
```

### 複数の型パラメータ

```python
fn pick_first<T, U>(a: T, b: U) -> T:
    return a

result = pick_first(1, "x")       # T = int, U = str, result = 1
result = pick_first("hello", 42)  # T = str, U = int, result = "hello"
```

### 型制約（バウンド）

型パラメータには `: RecordName` 構文でレコード型の制約を付けることができます。具体型はバウンド型自体またはそのサブタイプでなければなりません。

```python
record Animal:
    name: str
    legs: int

record Dog < Animal:
    breed: str

fn get_name<T: Animal>(a: T) -> str:
    return a.name

get_name(Dog("Rex", 4, "Lab"))  # OK — Dog は Animal のサブタイプ
get_name(Animal("Cat", 4))      # OK — 完全な型一致
```

バウンド付きとバウンドなしの型パラメータを混在させることができます:

```python
fn pair_name<T: Animal, U>(a: T, x: U) -> str:
    return a.name
```

### 動作の仕組み

ジェネリック関数は**単相化**を使用します: 型引数の一意な組み合わせごとに特殊化されたバージョンの関数が生成されます。同じインスタンス化はキャッシュされ、複数の呼び出しで再利用されます。型制約がある場合、インスタンス化時に検証されます。

---

## UFCS（統一関数呼び出し構文）

`a.f(b)` の形式で `f(a, b)` を呼び出せる。メソッドチェーンに使いやすい。

### 構文

```python
# 通常呼び出し
f(a, b)

# UFCS 呼び出し（等価）
a.f(b)
```

### チェーン

```python
fn double(x: int) -> int:
    return x * 2

fn add_one(x: int) -> int:
    return x + 1

result = 5.double().add_one()   # double(5) → 10, add_one(10) → 11
```

### フィールドアクセスとの混在

フィールドアクセス（`.field`）と UFCS（`.method()`）は同じドット記法で書けるが、引数の有無で区別される。

```python
p = Point(3, 4)
length = p.x.to_float()   # フィールドアクセス + UFCS
```

---

## 演算子オーバーロード

ユーザー定義型に対して演算子の振る舞いを定義できる。

### 構文

```python
# 二項演算子（引数2個）
fn operator<op>(a: 型, b: 型) -> 戻り値型:
    ...

# 単項演算子（引数1個）
fn operator<op>(a: 型) -> 戻り値型:
    ...
```

### オーバーロード可能な演算子

| 種別 | 演算子 |
|---|---|
| 算術（二項） | `+` `-` `*` `/` `%` `**` `//` |
| 比較（二項） | `==` `!=` `<` `<=` `>` `>=` |
| ビット（二項） | `&` `\|` `^` `<<` `>>` |
| 論理（二項） | `and` `or` |
| 所属 | `in` |
| 添字 | `[]`（読み取り）、`[]=`（書き込み） |
| 呼び出し | `()` |
| キャスト | `as` |
| 単項 | `-` `~` `not` |

### 戻り値型の制約

比較演算子、論理演算子、所属演算子は `bool` を返す必要がある:

| 種別 | 演算子 | 必須戻り値型 |
|---|---|---|
| 比較 | `==` `!=` `<` `<=` `>` `>=` | `bool` |
| 論理 | `and` `or` `not` | `bool` |
| 所属 | `in` | `bool` |
| キャスト | `as` | 必須（ターゲット型） |

```python
# OK
fn operator==(a: Vec2, b: Vec2) -> bool:
    return a.x == b.x and a.y == b.y

# エラー: comparison operator '==' must return 'bool', but returns 'int'
fn operator==(a: Vec2, b: Vec2) -> int:
    return 42
```

算術演算子・ビット演算子には戻り値型の制約はない。

### 二項 / 単項の区別

引数の個数で区別する。

```python
record Vec2:
    x: float
    y: float

# 二項 +
fn operator+(a: Vec2, b: Vec2) -> Vec2:
    return Vec2(a.x + b.x, a.y + b.y)

# 単項 -
fn operator-(v: Vec2) -> Vec2:
    return Vec2(-v.x, -v.y)

# 比較
fn operator==(a: Vec2, b: Vec2) -> bool:
    return a.x == b.x and a.y == b.y

v1 = Vec2(1.0, 2.0)
v2 = Vec2(3.0, 4.0)
v3 = v1 + v2    # Vec2(4.0, 6.0)
v4 = -v1        # Vec2(-1.0, -2.0)
```

---

## チェック付き/飽和演算

低レベル整数型（`i8`、`i16`、`i32`、`i64`、`u8`、`u16`、`u32`、`u64`）向けの明示的なオーバーフロー制御用組み込み関数です。両方の引数は同じ型でなければなりません。

| 関数 | 戻り値 | 動作 |
|----------|---------|----------|
| `checked_add(a, b)` | `Result<T, Error>` | オーバーフロー時に `Err` を返す |
| `checked_sub(a, b)` | `Result<T, Error>` | アンダーフロー時に `Err` を返す |
| `checked_mul(a, b)` | `Result<T, Error>` | オーバーフロー時に `Err` を返す |
| `saturating_add(a, b)` | `T` | オーバーフロー時に型の最小/最大値にクランプ |
| `saturating_sub(a, b)` | `T` | アンダーフロー時に型の最小/最大値にクランプ |
| `saturating_mul(a, b)` | `T` | オーバーフロー時に型の最小/最大値にクランプ |
| `wrapping_add(a, b)` | `T` | 明示的なラッピング（`+` と同じ） |
| `wrapping_sub(a, b)` | `T` | 明示的なラッピング（`-` と同じ） |
| `wrapping_mul(a, b)` | `T` | 明示的なラッピング（`*` と同じ） |

```python
# チェック付き: Result を返す。when や ? でハンドリング
r = checked_add(2147483647i32, 1i32)
when r:
  case Ok(v):
    print(v)
  case Err(e):
    print("overflow!")   # "overflow!" が出力される

# 飽和: 境界値にクランプ
v = saturating_add(2147483647i32, 100i32)
print(v as int)   # 2147483647

# ラッピング: 自己文書化的なラッピング動作
v = wrapping_add(2147483647i32, 1i32)
print(v as int)   # -2147483648
```

> **注意**: これらの関数は浮動小数点型（`f32`）や高レベルの `int` 型をサポートしていません。低レベル整数のデフォルトの `+`、`-`、`*` 演算子はラッピング動作（符号付きは2の補数、符号なしはモジュラー）を使用します。
