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

使用 `ensure` 指定函式回傳值時必須為真的條件。

### `result` 關鍵字

在 `ensure` 區塊內，`result` 代表回傳值。

```python
fn abs(x: int) -> int:
    ensure:
        result >= 0
    if x < 0:
        return -x
    return x
```

### `old()` 表達式

`old(expr)` 捕獲函式開始時表達式的值。這對於比較狀態變化前後非常有用。

```python
fn increment(x: int) -> int:
    ensure:
        result == old(x) + 1
    return x + 1
```

---

## 同時使用 `require` 和 `ensure`

兩者可以同時使用。`require` 必須寫在 `ensure` 之前。

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
@const
a = BankAccount(100, 0)   # OK: 100 >= 0
# a.balance = -1              # Contract violation: invariant failed
```

---

## 規則

- `require` 和 `ensure` 區塊為選用項，寫在函式主體之前。
- 同時使用時，`require` 必須寫在 `ensure` 之前。
- `result` 和 `old()` 只能在 `ensure` 區塊內使用。
- `invariant` 寫在 `record` 定義的末尾，在所有欄位宣告之後。
- 所有契約違反都會以 `exit(1)` 終止程式。

---

[← 前一篇：套件](09-modules.md) | [下一篇：測試 →](11-testing.md)
