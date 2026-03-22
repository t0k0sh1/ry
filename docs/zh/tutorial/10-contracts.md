[English](../../tutorial/10-contracts.md) | [日本語](../../ja/tutorial/10-contracts.md) | [繁體中文](10-contracts.md)

# 契約式設計

[← 前一篇：套件](09-modules.md) | [下一篇：測試 →](11-testing.md)

Ry 支援 Eiffel 風格的契約式設計，包含前置條件（`require`）、後置條件（`ensure`）以及結構體不變量（`invariant`）。當契約違反時，程式會終止。詳細規格請參閱[契約式設計參考手冊](../reference/contracts.md)。

---

## 前置條件（`require`）

使用 `require` 指定函式被呼叫時必須為真的條件。

```python
fn deposit(amount: int, balance: int) -> int:
    require:
        amount > 0
        balance >= 0
    new_balance: int = balance + amount
    return new_balance
```

當前置條件不滿足時，程式會以下列訊息終止：

```
Contract violation: require failed in deposit()
```

---

## 後置條件（`ensure`）

使用 `ensure` 指定函式回傳值時必須為真的條件。回傳值會綁定到使用者選擇的變數名。

```python
fn abs(x: int) -> int:
    ensure v:
        v >= 0
    if x < 0:
        return -x
    return x
```

由於 Ry 的函式參數是不可變的，可以在 `ensure` 區塊中直接引用參數：

```python
fn increment(x: int) -> int:
    ensure v:
        v == x + 1
    return x + 1
```

對於回傳元組的函式，可以用逗號分隔指定多個變數名：

```python
fn divide(a: int, b: int) -> (int, int):
    ensure q, r:
        q >= 0
        r >= 0
    return (a // b, a % b)
```

---

## 同時使用 `require` 和 `ensure`

兩者可以同時使用。`require` 必須寫在 `ensure` 之前。

```python
fn deposit(amount: int, balance: int) -> int:
    require:
        amount > 0
        balance >= 0
    ensure v:
        v >= 0
        v == balance + amount
    new_balance: int = balance + amount
    return new_balance
```

---

## 結構體不變量（`invariant`）

使用 `invariant` 指定結構體必須始終成立的條件。不變量會在建構後及每次欄位賦值後檢查。

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

## 規則

- `require` 和 `ensure` 區塊為選用項，寫在函式主體之前。
- 同時使用時，`require` 必須寫在 `ensure` 之前。
- `ensure` 需要變數綁定來命名回傳值（例：`ensure v:`）。
- 對於元組回傳值，可指定多個綁定變數（例：`ensure q, r:`）。
- `invariant` 寫在 `record` 定義的末尾，在所有欄位宣告之後。
- 所有契約違反都會以 `exit(1)` 終止程式。

---

[← 前一篇：套件](09-modules.md) | [下一篇：測試 →](11-testing.md)
