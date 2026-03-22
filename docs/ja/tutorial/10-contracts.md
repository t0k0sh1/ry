[English](../../tutorial/10-contracts.md) | [日本語](10-contracts.md) | [繁體中文](../../zh/tutorial/10-contracts.md)

# 契約による設計

[← 前: パッケージ](09-modules.md) | [次: テスト →](11-testing.md)

Ry は Eiffel スタイルの契約による設計をサポートしています。事前条件（`require`）、事後条件（`ensure`）、構造体不変条件（`invariant`）を使ってコードの正しさを保証します。契約違反が発生するとプログラムは終了します。詳細な仕様は[契約による設計リファレンス](../reference/contracts.md)を参照してください。

---

## 事前条件（`require`）

`require` を使って、関数が呼び出される際に真でなければならない条件を指定します。

```python
fn deposit(amount: int, balance: int) -> int:
    require:
        amount > 0
        balance >= 0
    new_balance: int = balance + amount
    return new_balance
```

事前条件が満たされない場合、以下のメッセージとともにプログラムが終了します。

```
Contract violation: require failed in deposit()
```

---

## 事後条件（`ensure`）

`ensure` を使って、関数が値を返す際に真でなければならない条件を指定します。

### `result` キーワード

`ensure` ブロック内では、`result` で戻り値を参照できます。

```python
fn abs(x: int) -> int:
    ensure:
        result >= 0
    if x < 0:
        return -x
    return x
```

### `old()` 式

`old(expr)` は関数開始時の式の値をキャプチャします。状態の変化前後を比較するのに便利です。

```python
fn increment(x: int) -> int:
    ensure:
        result == old(x) + 1
    return x + 1
```

---

## `require` と `ensure` の併用

両方を同時に使えます。`require` は `ensure` の前に記述する必要があります。

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

## 構造体不変条件（`invariant`）

`invariant` を使って、構造体に対して常に成り立つ必要がある条件を指定します。不変条件はコンストラクタの実行後とフィールドへの代入後にチェックされます。

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

---

## ルール

- `require` と `ensure` ブロックは任意で、関数本体の前に記述します。
- 両方を使う場合、`require` を `ensure` の前に記述する必要があります。
- `result` と `old()` は `ensure` ブロック内でのみ使用できます。
- `invariant` は `record` 定義の末尾、全フィールド宣言の後に記述します。
- すべての契約違反は `exit(1)` でプログラムを終了します。

---

[← 前: パッケージ](09-modules.md) | [次: テスト →](11-testing.md)
