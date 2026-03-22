[English](../../reference/contracts.md) | [日本語](../../ja/reference/contracts.md) | [繁體中文](contracts.md)

# 契約式設計 (Design by Contract)

Ry 支援 Eiffel 風格的契約式設計，包含前置條件（`require`）、後置條件（`ensure`）和結構體不變量（`invariant`）。契約違反時，程式將以 `exit(1)` 終止。

---

## 前置條件 (`require`)

前置條件在函式進入時檢查。它們指定函式被正確呼叫所需滿足的條件。

```python
fn deposit(amount: int, balance: int) -> int:
    require:
        amount > 0
        balance >= 0
    new_balance: int = balance + amount
    return new_balance
```

若前置條件未滿足，程式將輸出以下訊息並終止：
```
Contract violation: require failed in deposit()
```

---

## 後置條件 (`ensure`)

後置條件在每個 `return` 之前檢查。它們指定函式對其回傳值的保證。

### 變數綁定

`ensure` 需要一個變數名來綁定回傳值。此變數可在後置條件表達式中使用。

```python
fn abs(x: int) -> int:
    ensure v:
        v >= 0
    if x < 0:
        return -x
    return x
```

由於 Ry 的函式參數是不可變的，可以在 `ensure` 區塊中直接引用參數來與進入時的值比較：

```python
fn increment(x: int) -> int:
    ensure v:
        v == x + 1
    return x + 1
```

### 元組解構

對於回傳元組的函式，可以用逗號分隔指定多個變數名：

```python
fn divide(a: int, b: int) -> (int, int):
    ensure q, r:
        q >= 0
        r >= 0
    return (a // b, a % b)
```

綁定變數的數量必須與元組元素數量一致。

---

## 組合範例

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

## 結構體不變量 (`invariant`)

不變量是結構體實例必須始終滿足的條件。在以下時機檢查：
- 建構時
- 每次欄位賦值後

```python
record BankAccount:
    balance: int
    min_balance: int
    invariant:
        balance >= min_balance
```

```python
a = BankAccount(100, 0)    # OK: 100 >= 0
a.balance = -1                  # Contract violation: invariant failed
```

---

## 規則

- `require` 和 `ensure` 區塊為選用，寫在函式本體之前。
- 同時使用時，`require` 必須在 `ensure` 之前。
- `ensure` 需要變數綁定來命名回傳值（例：`ensure v:`）。
- 對於元組回傳值，可指定多個綁定變數（例：`ensure q, r:`）。
- `invariant` 寫在 `record` 定義的末尾，所有欄位宣告之後。
- 所有契約違反以 `exit(1)` 終止程式並輸出診斷訊息。
