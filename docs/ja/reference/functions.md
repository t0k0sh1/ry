[English](../../reference/functions.md) | [日本語](functions.md) | [繁體中文](../../zh/reference/functions.md)

# 関数リファレンス

## 関数定義の構文

```python
function function_name(param_name: type, ...) -> return_type:
    # 本体
    return value
```

- 引数の型は省略可能。省略時は `any` 型として扱われる。
- 戻り値型は省略可能。省略時は**ボディから推論**される（名前付き関数とラムダの両方）。`return` 文がなければ `Unit` に推論される。明示的に任意の戻り値型を許容するには `-> any` を指定する。
- 本体はインデントされたブロック。
- 明示的な戻り値型（`Unit` と `any` を除く）を持つ関数は、すべての制御フローパスで `return` 文が必要です。不足している場合はコンパイルエラーになります。
- 関数には `require`（事前条件）と `ensure`（事後条件）を定義できます。[契約による設計](contracts.md) を参照。

> **命名規則**: 関数名と引数名は snake_case（例: `add`、`get_value`、`map_list`）を使用する必要があります。コンパイラがこの規則を強制します。

```python
function add(a: int, b: int) -> int:
    return a + b

function greet(name: str) -> Unit:
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
function no_return(x: int) -> Unit:  # 戻り値型 Unit（明示的）
    print(x)

function get_value() -> int:     # 戻り値型 int
    return 42

function identity(x) -> any:    # 引数型 any（省略）
    return x
```

### 型省略と `any`

引数の型アノテーションを省略すると、その引数は `any` として扱われます。`any` は実行時に任意のプリミティブ値を受け入れる動的型です。Python の型なし引数と同様の仕組みです。

```python
# すべての引数が any
function add(a, b):
    return a + b

add(1, 2)              # 3（int + int）
add("hello", " world") # "hello world"（str + str）
add(1, 2.0)            # 3.0（int + float）
```

型アノテーションに `any` を明示的に書くこともできます:

```python
function identity(x: any) -> any:
    return x
```

### 戻り値型の推論

戻り値型を省略すると、ボディ内の `return` 文から推論されます:

```python
function double(x: int):     # 戻り値型は int に推論
    return x * 2

function greet(name: str):   # 戻り値型は Unit に推論（return なし）
    print("Hello, " + name)
```

明示的に任意の戻り値型を許容するには `-> any` を指定します:

```python
function flexible(x: any) -> any:
    return x    # int、float、str 等を返せる
```

---

## ネスト関数

関数は他の関数の内部に定義できます。ネスト関数は囲む関数のスコープ内からのみ可視で、外部からは呼び出せません。

```python
function outer() -> int:
    function helper() -> int:
        return 42
    return helper()

outer()     # 42
# helper()  # エラー: undefined function
```

兄弟スコープにある同名のネスト関数は衝突しません:

```python
function foo() -> int:
    function helper() -> int:
        return 1
    return helper()

function bar() -> int:
    function helper() -> int:
        return 2
    return helper()

foo()   # 1
bar()   # 2
```

ネスト関数は値として使用でき、高階関数に渡すこともできます。同じスコープ内のネスト関数間の相互再帰も動作します（コンパイラが前方宣言します）。

### クロージャキャプチャ

ネストされた名前付き関数は、ラムダと同様に囲むスコープから変数をキャプチャできます。ネスト関数が外側の変数を参照すると、それはクロージャになります:

```python
function make_adder(base: int) -> function(int) -> int:
    function add(x: int) -> int:
        return x + base
    return add

add10 = make_adder(10)
add10(5)   # 15
```

キャプチャルール:

- キャプチャは**値渡し**（ラムダと同じ）です。値はクロージャ生成時点でコピーされます。
- キャプチャされた変数はネスト関数のボディ内で**再代入できません**。
- ARC 管理の値（文字列、リスト等）は適切にリテイン／リリースされます。
- ネスト関数がキャプチャを持たない場合、単なる関数ポインタのままです（オーバーヘッドなし）。
- 多段階のキャプチャも動作します。深くネストされた関数は、囲むスコープの任意の変数を参照できます。

---

## 再帰

関数は自身を呼び出せる。

```python
function factorial(n: int) -> int:
    if n <= 1:
        return 1
    return n * factorial(n - 1)
```

### 相互再帰

関数は定義順に関係なく相互に呼び出すことができます。コンパイラは、参照される型がすべて既知（プリミティブ型は常に利用可能、record/enum 型はファイル内で先に定義されている必要がある）であることを条件に、明示的な戻り値型を持つトップレベル関数をボディ処理前に前方宣言します。

```python
function is_even(n: int) -> bool:
    if n == 0:
        return true
    return is_odd(n - 1)       # 下で定義された is_odd を呼び出す

function is_odd(n: int) -> bool:
    if n == 0:
        return false
    return is_even(n - 1)      # 上で定義された is_even を呼び出す
```

**前方参照の要件:**

- 関数は**明示的な戻り値型**アノテーション（`-> type`）を持つ必要があります。推論された戻り値型の関数は前方参照できません。
- 関数は**トップレベル**または別の関数ボディ内に定義できます。前方参照は同じスコープレベル内で動作します。
- すべてのパラメータ型と戻り値型は前方宣言の時点で解決可能でなければなりません（例: record 型はそれを使用する関数の前に定義されている必要があります）。

### トップレベル変数と関数ボディ内での `@const`

トップレベルの `let` 束縛と `@const` 宣言は、同じソースファイル内で宣言が参照する関数の**テキスト上前**に現れている限り、任意のトップレベル関数 -- およびそれらの関数内のネスト関数やラムダ -- から可視です。

```python
@const
PI: float = 3.14

@const
MAX_RETRIES: int = 5

counter: int = 0

function area(radius: float) -> float:
    return PI * radius * radius            # トップレベル @const を読む

function clamp_retries(n: int) -> int:
    if n > MAX_RETRIES:
        return MAX_RETRIES
    return n

function bump():
    counter = counter + 1                  # トップレベル可変 `let` に書く
```

**ルール:**

- **ソース順に厳密**。関数ボディは同じファイル内でそれより後に宣言されたトップレベル束縛を参照できません。束縛を関数の上に移動するか、遅延的に呼び出されるヘルパー関数でラップしてください。
- **`@const` は読み取り専用**。再代入やフィールドミューテーション（トップレベル `@const P: Point` に対する `P.x = 99`）はコンパイル時に拒否されます。
- **可変 `let` への書き込みはスルー**。関数内部からトップレベル可変変数に代入すると、実際にトップレベル束縛がミューテーションされます。同名のローカルが作成されるわけではありません。
- **ネストしたブロックはモジュールレベルではない**。トップレベルの `if`、`while`、`for` ブロック内の `let` はそのブロックにローカルで、関数からは可視ではありません。

**制限事項 (v0.0.8):**

- 並列 `for` ブロックはトップレベル可変変数に代入できません（データ競合の回避）。
- トップレベル `weak` 参照とリソース型の束縛（file / regex ハンドル）は、まだ関数ボディからアクセスできません。必要ならフォローアップ issue でこれらのユースケースを追跡してください。

### 末尾呼び出し最適化

コンパイラは、関数の最後の処理が自分自身の呼び出しである自己再帰末尾呼び出しを自動的に検出し、LLVM の `musttail` 最適化を適用します。これにより、末尾再帰関数は一定のスタック空間を使用することが保証され、深い再帰でのスタックオーバーフローを防ぎます。

```python
function sum_to(n: int, acc: int) -> int:
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
function area(side: int) -> int:
    return side * side

function area(w: int, h: int) -> int:
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
function process(x: int) -> str:
    return "int"

function process(x) -> str:          # x: any
    return "any"

process(42)       # "int" — 完全一致（int）が any に勝つ
process("hello")  # "any" — str の完全一致がないため any にフォールバック
```

```python
function double(x: float) -> float:
    return x * 2.0

double(5)         # OK — int が暗黙的に float に拡大変換され、10.0 を返す
```

---

## デフォルト引数

パラメータにデフォルト値を指定でき、呼び出し時に末尾の引数を省略できる。

### 構文

```python
function connect(host: str, port: int = 8080, timeout: int = 30):
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
function calc(x: int, y: int = 0) -> int:
    return x + y
function calc(x: int) -> int:      # 上の calc(int) と衝突
    return x * 2
```

### 制限事項

- **ジェネリック関数**および**ラムダ式**ではデフォルト引数は未サポート。

---

## Unit 型関数

戻り値のない関数は `Unit` を返す。戻り値型は省略可能（`Unit` に推論される）、または `-> Unit` で明示的に指定可能。

```python
function log(msg: str) -> Unit:
    print(msg)
```

---

## Task と async 関数

`Task<T>` は並行実行の組み込みハンドル型です。`async function` は `Task<T>` を返し、`await` は別の `async function` 内で `T` を取り出します。`block_on(task)` は同期コンテキストからタスクの完了までブロックします。

```python
async function add(a: int, b: int) -> int:
    return a + b

# 同期コンテキストからは block_on() を使用
t: Task<int> = add(20, 22)
print(block_on(t))                  # 42
block_on(add(1, 2))                 # 待機して結果を捨てる

# async function 内では await を使用
async function double_add(a: int, b: int) -> int:
    return (await add(a, b)) * 2
```

### ルール

- `async function name(...) -> T:` の `T` は await 後の値型です。
- `async function` の呼び出し結果は常に `Task<T>` です。
- `await expr` は `Task<T>` にのみ使用でき、結果は `T` です。
- `await` は `async function` 内でのみ使用可能。同期コンテキストからは `block_on(task)` を使用します。
- `block_on(task)` は現在のスレッドをタスク完了までブロックし、結果を返します。
- `async function ... -> Unit` をサポートします。値を返さない task の待機には `block_on(task)` を使うのが基本です。
- task はランタイムの worker pool 上で実行され、task ごとに OS スレッドを作る実装ではありません。
- `async` ラムダと `async @native function` は v1 では未対応です。

---

## ラムダ関数

無名関数をその場で定義できる。

### 構文

```python
# 単一式（式の値が返る。戻り値型は推論）
 (param_name: type, ...) => expression

# 引数型の省略（any がデフォルト）
 (param_name, ...) => expression

# 複数行ブロック
(param_name: type, ...):
    # 複数の文
    return value

# 戻り値型の明示（省略可能）
 (param_name: type, ...) -> return_type => expression
```

### 例

```python
double = (x: int) => x * 2
result = double(5)   # 10

add = (a: int, b: int) => a + b
sum = add(3, 4)      # 7

# 複数行ラムダ
abs = (x: int):
    if x < 0:
        return -x
    return x
```

---

## クロージャ

ラムダ関数は定義された時点の外側スコープの変数を**値でキャプチャ**します。クロージャはキャプチャ時点の独立したコピーを受け取り、キャプチャされた変数はクロージャ内で再代入できません。

### 外側の変更はクロージャに影響しない

クロージャはコピーを保持するため、クロージャ定義後に元の変数を再代入しても、キャプチャされた値には影響しません:

```python
base = 10
add_base = (x: int) => x + base   # base を値でキャプチャ（10 のコピー）

base = 99          # キャプチャ済みの値には影響しない
r = add_base(5)   # 15（キャプチャ時の base = 10 を使用）
```

### キャプチャされた変数は実質的に final

キャプチャされた変数はクロージャ内で**再代入できません**。再代入しようとするとコンパイルエラーになります:

```python
counter = 0
inc = ():
    counter += 1    # コンパイルエラー: cannot modify captured variable 'counter' inside closure

inc()
```

**キャプチャされた record へのフィールド代入は許可されています**。変数自体の再代入ではなく、コピーの内部状態を変更するためです:

```python
record Point:
    x: int
    y: int

p = Point(0, 0)
move = ():
    p.x = p.x + 1    # OK — キャプチャされたコピーのフィールドを変更
```

> **注意**: フィールドの変更はクロージャのコピーにのみ適用され、外側の変数には影響しません。

### キャプチャルール

| 項目 | 内容 |
|---|---|
| キャプチャ方式 | 値キャプチャ（コピー） |
| キャプチャタイミング | ラムダ定義時 |
| キャプチャ変数の再代入 | 不可（コンパイルエラー） |
| キャプチャされた record へのフィールド代入 | 可（コピーのみを変更） |
| 外側変数の変更の影響 | ない（クロージャは独自のコピーを保持） |

> **Python/JavaScript ユーザーへの注意**: JavaScript ではクロージャは変数を参照でキャプチャするため、キャプチャされた変数への変更はクロージャの外側にも反映されます。Python ではクロージャは外側の変数にアクセスでき、外側の名前の再バインド（例: `counter += x`）には `nonlocal` 宣言が必要です。Ry では、クロージャは常に値でキャプチャし、キャプチャされた変数は実質的に final です -- クロージャ内で再代入できません。これは意図的なもので、特に並行処理や高階関数のコンテキストにおいて安全性と予測可能性を確保します。

---

## 関数型

関数を値として扱うための型。

### 構文

```python
function(param_type1, param_type2, ...) -> return_type
```

### 例

```python
f: function(int) -> int = (x: int) => x * 2
g: function(int, int) -> int = (a: int, b: int) => a + b

function apply(func: function(int) -> int, x: int) -> int:
    return func(x)

result = apply(f, 5)   # 10
```

### 文字列表現

`print()`、`to_str()`、f-string の補間はいずれも関数値に対して `"<closure>"` を生成します:

```python
f = (x: int) => x + 1
print(f)              # <closure>
s = to_str(f)         # "<closure>"
msg = f"fn={f}"       # "fn=<closure>"
```

> **注意**: クロージャ間の等価比較（`==` / `!=`）はサポートされておらず、コンパイル時エラーになります。

---

## 高階関数

関数を引数として受け取ったり、戻り値として返したりできる。

```python
function map_list(xs: List<int>, f: function(int) -> int) -> List<int>:
    result: List<int> = []
    for x in xs:
        result += [f(x)]
    return result

doubled = map_list([1, 2, 3], (x: int) => x * 2)
# [2, 4, 6]
```

---

## ジェネリック関数

関数は型パラメータを持つことができ、コード重複なしに型安全な再利用を実現します。

### 構文

```python
function name<T, U>(param1: T, param2: U) -> T:
    # T、U を型として使用するボディ
```

### 例

```python
function identity<T>(x: T) -> T:
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
function pick_first<T, U>(a: T, b: U) -> T:
    return a

result = pick_first(1, "x")       # T = int, U = str, result = 1
result = pick_first("hello", 42)  # T = str, U = int, result = "hello"
```

### コンテナ型内の型パラメータ

型パラメータはジェネリックなコンテナ型（`List<T>`、
`Map<K, V>`、`Set<T>`）、タプル `(T, T)`、関数型
`function(T) -> T` の内部にも現れることができます。推論は宣言された
パラメータ型を実際の引数と構造的に突き合わせるため、形が曖昧でない限り
明示的な型注釈は必要ありません。

```python
function first_of<T>(xs: List<T>) -> T:
    return xs[0]

first_of([1, 2, 3])            # T = int  → 1
first_of(["hello", "world"])   # T = str  → "hello"
first_of([[1, 2], [3, 4]])     # T = List<int>  → [1, 2]

function map_lookup<K, V>(m: Map<K, V>, k: K) -> V:
    return m[k]

map_lookup({1: "a", 2: "b"}, 1)     # K = int, V = str → "a"
map_lookup({"x": 10, "y": 20}, "y") # K = str, V = int → 20

function pair_first<T>(p: (T, T)) -> T:
    return p.0

pair_first((42, 7))      # T = int → 42
pair_first(("a", "b"))   # T = str → "a"
```

複数のパラメータ位置から参照される型パラメータは統一されます --
両方の出現が同じ具体型に解決される必要があります:

```python
function apply_list<T>(xs: List<T>, f: function(T) -> T) -> T:
    return f(xs[0])

apply_list([10, 20, 30], (x: int) => x + 1)  # T = int → 11
```

推論が型パラメータを決定できない場合（例: 空のコンテナリテラル）、
明示的な `name[Type](args)` 構文を使ってください:

```python
first_of[int]([])   # 空リスト: コンパイラに T = int を明示的に伝える
```

引数間で矛盾する推論が発生した場合、曖昧な型不一致ではなく、型パラメータ
と関数名を明示したわかりやすいコンパイルエラーになります:

```python
function same<T>(a: T, b: T) -> T:
    return a

same(1, "x")  # error: conflicting type inference for 'T' in call to 'same'
```

### 型制約（バウンド）

型パラメータには `: RecordName` 構文でレコード型の制約を付けることができます。具体型はバウンド型自体またはそのサブタイプでなければなりません。

```python
record Animal:
    name: str
    legs: int

record Dog < Animal:
    breed: str

function get_name<T: Animal>(a: T) -> str:
    return a.name

get_name(Dog("Rex", 4, "Lab"))  # OK — Dog は Animal のサブタイプ
get_name(Animal("Cat", 4))      # OK — 完全な型一致
```

バウンド付きとバウンドなしの型パラメータを混在させることができます:

```python
function pair_name<T: Animal, U>(a: T, x: U) -> str:
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
function double(x: int) -> int:
    return x * 2

function add_one(x: int) -> int:
    return x + 1

result = 5.double().add_one()   # double(5) -> 10, add_one(10) -> 11
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
function operator<op>(a: type, b: type) -> return_type:
    ...

# 単項演算子（引数1個）
function operator<op>(a: type) -> return_type:
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
function operator==(a: Vec2, b: Vec2) -> bool:
    return a.x == b.x and a.y == b.y

# エラー: comparison operator '==' must return 'bool', but returns 'int'
function operator==(a: Vec2, b: Vec2) -> int:
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
function operator+(a: Vec2, b: Vec2) -> Vec2:
    return Vec2(a.x + b.x, a.y + b.y)

# 単項 -
function operator-(v: Vec2) -> Vec2:
    return Vec2(-v.x, -v.y)

# 比較
function operator==(a: Vec2, b: Vec2) -> bool:
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
# チェック付き: Result を返す。case や ? でハンドリング
r = checked_add(2147483647i32, 1i32)
case r:
  Ok(v):
    print(v)
  Err(e):
    print("overflow!")   # "overflow!" が出力される

# 飽和: 境界値にクランプ
v = saturating_add(2147483647i32, 100i32)
print(v as int)   # 2147483647

# ラッピング: 自己文書化的なラッピング動作
v = wrapping_add(2147483647i32, 1i32)
print(v as int)   # -2147483648
```

> **注意**: これらの関数は浮動小数点型（`f32`）や高レベルの `int` 型をサポートしていません。低レベル整数のデフォルトの `+`、`-`、`*` 演算子はラッピング動作（符号付きは2の補数、符号なしはモジュラー）を使用します。
