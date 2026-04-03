[English](../../tutorial/08-error-handling.md) | [日本語](08-error-handling.md) | [繁體中文](../../zh/tutorial/08-error-handling.md)

# エラーハンドリング

[<- 前: コレクションとイテレータ](07-collections.md) | [次: パッケージ ->](09-modules.md)

Ry にはエラーや値の不在に対処するための3つの補完的な戦略があります: **Option**（値が存在しない可能性がある）、**Result**（操作が失敗する可能性がある）、**契約による設計**（境界で不正な状態を防止する）。このチュートリアルでは3つすべてと、それぞれの使い分けを説明します。

---

## Option 型

`Option<T>` は値が存在するかどうかを表す型です。2つのバリアントがあります: `Some(value)` と `None`。

```python
x: Option<int> = Some(42)
print(x)   # Some(42)

y: Option<int> = None
print(y)   # None
```

### 値の取り出し

`match` を使って内部の値を安全に取り出し、`None` の場合を処理します。これは[制御構文](04-control-flow.md)で学んだパターンマッチングを使用します:

```python
match x:
    case Some(v):
        print(v)    # 42
    case None:
        print("nothing")
```

> **なぜ Option なのか?** 値の不在を型システムで明示的にします。`-1` のような「マジックナンバー」を返したり `null` をチェックしたりする代わりに、呼び出し側が `None` のケースを処理しなければなりません -- コンパイラがそれを保証します。

### Option に出会う場面

すでに `Option` の動作を見ています: `iterator.next()` は `Option<T>` を返し、各要素に対して `Some(element)` を、イテレータが使い尽くされると `None` を返します。

---

## Result 型

`Result<T, E>` は失敗する可能性のある操作に使用します。成功時は `Ok(value)`、失敗時は `Err(error)` を返します。

```python
function divide(a: int, b: int) -> Result<int, Error>:
    if b == 0:
        return Err(Error("division by zero"))
    return Ok(a // b)
```

### match による Result の処理

```python
r = divide(10, 0)
match r:
    case Ok(v):
        print(v)
    case Err(e):
        print(e.message)   # division by zero
```

### `?` 演算子（エラー伝播）

`Result` を返す関数から、同じく `Result` を返す別の関数を呼び出す際に、`?` を使ってエラーを自動的に伝播できます。値が `Ok` であればアンラップされ、`Err` であればその関数はそのエラーを即座に返します。

```python
function safe_divide(a: int, b: int) -> Result<int, Error>:
    if b == 0:
        return Err(Error("division by zero"))
    return Ok(a // b)

function divide_and_add(a: int, b: int) -> Result<int, Error>:
    v = safe_divide(a, b)?   # b == 0 なら Err を早期リターン
    return Ok(v + 1)
```

これは以下と等価です:

```python
function divide_and_add(a: int, b: int) -> Result<int, Error>:
    match safe_divide(a, b):
        case Ok(v):
            return Ok(v + 1)
        case Err(e):
            return Err(e)
```

`?` 演算子はボイラープレートを除去し、複数の失敗しうる操作を簡潔にチェーンできます:

```python
function compute(a: int, b: int, c: int) -> Result<int, Error>:
    x = safe_divide(a, b)?
    y = safe_divide(x, c)?
    return Ok(y + 1)
```

### `and_then` と `map` によるメソッドチェーン

複数の `Result` を返す操作をチェーンしたいが `?` を使えない場合（例: `Result` を返さない関数の中）、`and_then` と `map` を使ってネストの深い `match` 文を避けることができます。

**`and_then`** -- それ自体が `Result` を返す操作をチェーンします:

```python
# match を3段階にネストする代わりに:
result = safe_divide(100, 2)
    .and_then((v: int) => safe_divide(v, 5))
    .and_then((v: int) => safe_divide(v, 2))

match result:
    case Ok(v):  print(v)      # 5
    case Err(e): print(e.message)
```

**`map`** -- `Result` のラッパーを変えずに `Ok` の値を変換します:

```python
result = safe_divide(10, 2)
    .map((v: int) => v * 10)

match result:
    case Ok(v):  print(v)      # 50
    case Err(e): print(e.message)
```

どちらのメソッドも `Err` で短絡評価します -- チェーンのいずれかのステップが失敗すると、残りのクロージャを実行せずにエラーが伝播します。

`and_then` と `map` を1つのチェーンで混在させることもできます:

```python
result = safe_divide(100, 10)
    .and_then((v: int) => safe_divide(v, 2))
    .map((v: int) => v + 100)
# Ok(105)
```

> **なぜ Result なのか?** 例外を使わずにエラーハンドリングを明示的にします。型シグネチャがどの関数が失敗しうるかを正確に示し、`?` 演算子がコードを簡潔に保ちます。

> **よくあるミス**: `Result` を返さない関数で `?` を使うとコンパイルエラーになります。`?` 演算子は戻り値の型が `Result` の関数内でのみ使用できます。

---

## 契約による設計

Ry は Eiffel スタイルの契約による設計をサポートしており、事前条件（`require`）、事後条件（`ensure`）、record の不変条件（`invariant`）があります。Option と Result が実行時にエラーを処理するのに対し、契約は不正な状態の発生を**防止**します。

完全な仕様については[契約による設計リファレンス](../reference/contracts.md)を参照してください。

### 事前条件（`require`）

`require` を使って、関数が呼ばれる際に真でなければならない条件を指定します:

```python
function deposit(amount: int, balance: int) -> int:
    require:
        amount > 0
        balance >= 0
    new_balance: int = balance + amount
    return new_balance
```

事前条件が満たされない場合、プログラムは以下のメッセージで終了します:

```text
Contract violation: require failed in deposit()
```

### 事後条件（`ensure`）

`ensure` を使って、関数が返る際に真でなければならない条件を指定します。戻り値はユーザーが選択した変数名に束縛されます:

```python
function abs(x: int) -> int:
    ensure v:
        v >= 0
    if x < 0:
        return -x
    return x
```

Ry では関数の引数は不変なので、`ensure` ブロック内で直接参照できます:

```python
function increment(x: int) -> int:
    ensure v:
        v == x + 1
    return x + 1
```

タプルの戻り値には複数の変数名を使います:

```python
function divide(a: int, b: int) -> (int, int):
    ensure q, r:
        q >= 0
        r >= 0
    return (a // b, a % b)
```

### `require` と `ensure` の組み合わせ

両方を同時に使えます。`require` は `ensure` より前に書く必要があります:

```python
function deposit(amount: int, balance: int) -> int:
    require:
        amount > 0
        balance >= 0
    ensure v:
        v >= 0
        v == balance + amount
    new_balance: int = balance + amount
    return new_balance
```

### Record の不変条件（`invariant`）

`invariant` を使って、record に対して常に成り立つべき条件を指定します。不変条件はコンストラクション後とフィールド代入のたびにチェックされます:

```python
record BankAccount:
    balance: int
    min_balance: int
    invariant:
        balance >= min_balance
```

```python
a = BankAccount(100, 0)   # OK: 100 >= 0
# a.balance = -1              # Contract violation: invariant failed
```

> **なぜ契約なのか?** 仮定をコード内に直接文書化し強制します。関数が `amount > 0` を要求する場合、契約が呼び出し側で即座に違反を検出します -- 関数本体の奥深くで症状が現れるのではなく。

### 契約のルール

- `require` と `ensure` ブロックは任意で、関数本体の前に記述します。
- 両方使う場合、`require` は `ensure` より前に書く必要があります。
- `ensure` には変数束縛が必要です（例: `ensure v:`）。これにより戻り値に名前を付けます。
- `invariant` は `record` 定義の末尾、すべてのフィールド宣言の後に記述します。
- すべての契約違反は `exit(1)` で終了します。

---

## 使い分けの指針

| 戦略 | 使用場面 | 例 |
|------|---------|-----|
| **Option** | 値が正当に不在である場合 | キーの検索、`iterator.next()` |
| **Result** | 意味のあるエラーで操作が失敗しうる場合 | ファイル I/O、パース、ネットワーク呼び出し |
| **契約** | 不正な入力がそもそも発生してはいけない場合（プログラマのミス） | 負の入金額、範囲外インデックス |

**経験則:**
- 外部要因（ユーザー入力、ファイルシステム、ネットワーク）で失敗しうる操作には **Result** を使う。
- 「何もない」が正常で期待される結果の場合は **Option** を使う。
- プログラマのミスを早期に検出するには**契約**を使う -- これはアサーションであり、エラーハンドリングではない。

---

## よくあるミス

1. **Result を無視する**: `Result` を返す関数を呼んで処理しないと、エラー情報が失われます。
2. **Result を返さない関数で `?` を使う**: `?` 演算子は囲む関数が `Result` を返す必要があります。
3. **Option と Result を混同する**: `Option` には `Some`/`None`、`Result` には `Ok`/`Err` があります。用途が異なります。

---

## 演習

1. **Result と `?`**: ヘルパー関数を使って文字列を整数にパースし、2倍にする関数 `parse_and_double(s: str) -> Result<int, Error>` を書いてください。エラー伝播には `?` を使います。

2. **契約**: `amount > 0` かつ `amount <= balance` を `require` で、結果が非負であることを `ensure` で指定した関数 `withdraw(amount: int, balance: int) -> int` を書いてください。

3. **Option の処理**: `List<int>` を受け取り、最初の偶数を `Option<int>` として返す関数を書いてください。偶数が存在しない場合は `None` を返します。

---

[<- 前: コレクションとイテレータ](07-collections.md) | [次: パッケージ ->](09-modules.md)
