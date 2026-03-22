[English](../../reference/contracts.md) | [日本語](contracts.md) | [繁體中文](../../zh/reference/contracts.md)

# 契約による設計 (Design by Contract)

Ry は Eiffel スタイルの契約による設計をサポートしています。事前条件（`require`）、事後条件（`ensure`）、構造体の不変条件（`invariant`）を使用できます。契約違反時はプロセスが `exit(1)` で終了します。

---

## 事前条件 (`require`)

事前条件は関数の入口でチェックされます。関数が正しく呼び出されるために満たすべき条件を指定します。

```python
fn deposit(amount: int, balance: int) -> int:
    require:
        amount > 0
        balance >= 0
    new_balance: int = balance + amount
    return new_balance
```

事前条件が満たされない場合、以下のメッセージとともにプログラムが終了します：
```
Contract violation: require failed in deposit()
```

---

## 事後条件 (`ensure`)

事後条件はすべての `return` の直前にチェックされます。関数の戻り値について保証する条件を指定します。

### `result` キーワード

`ensure` ブロック内では、`result` は戻り値を参照します。

```python
fn abs(x: int) -> int:
    ensure:
        result >= 0
    if x < 0:
        return -x
    return x
```

### `old()` 式

`old(expr)` は関数本体の実行前の式の値をキャプチャします。変更前後の状態を比較するのに便利です。

```python
fn increment(x: int) -> int:
    ensure:
        result == old(x) + 1
    return x + 1
```

---

## 組み合わせ例

```python
fn deposit(amount: int, balance: int) -> int:
    require:
        amount > 0
        balance >= 0
    ensure:
        result >= 0
        result == old(balance) + amount
    new_balance: int = balance + amount
    return new_balance
```

---

## 構造体の不変条件 (`invariant`)

不変条件は構造体のインスタンスが常に満たすべき条件です。以下のタイミングでチェックされます：
- 構築時
- フィールド代入後

```python
record BankAccount:
    balance: int
    min_balance: int
    invariant:
        balance >= min_balance
```

```python
@const
a = BankAccount(100, 0)    # OK: 100 >= 0
a.balance = -1                  # Contract violation: invariant failed
```

---

## ルール

- `require` と `ensure` ブロックはオプションで、関数本体の前に記述します。
- 両方を使う場合、`require` は `ensure` の前に記述する必要があります。
- `result` と `old()` は `ensure` ブロック内でのみ使用できます。
- `invariant` は `record` 定義の末尾、全フィールド宣言の後に記述します。
- すべての契約違反は `exit(1)` でプログラムを終了し、診断メッセージを出力します。
