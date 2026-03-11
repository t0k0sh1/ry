# 02 - 変数と型

← [01 - はじめに](01-getting-started.md) / 次 → [03 - 演算子](03-operators.md)

---

## let による変数宣言

`let` キーワードで変数を宣言します。型は右辺の値から自動的に推論されます。

```python
let x = 42        # int 型として推論
let y = 3.14      # float 型として推論
let flag = true   # bool 型として推論
let name = "Ry"   # str 型として推論
```

---

## const による定数宣言

`const` キーワードで定数を宣言します。宣言後に値を変更することはできません。

```python
const PI = 3.14159
const MAX = 100
```

---

## 型アノテーション

変数の型を明示的に指定できます。

```python
let x: int = 42
let rate: float = 0.5
let ok: bool = false
let msg: str = "hello"
```

型アノテーションと実際の値の型が一致しない場合はコンパイルエラーになります。

---

## 基本型

| 型 | 説明 | リテラル例 |
|----|------|-----------|
| `int` | 64ビット整数 | `0`, `42`, `-10` |
| `float` | 64ビット浮動小数点数 | `0.0`, `3.14`, `-1.5` |
| `bool` | 真偽値 | `true`, `false` |
| `str` | 文字列 | `"hello"`, `""` |

---

## 文字列操作

文字列に対してさまざまな操作が使えます。

```python
let a = "Hello"
let b = "World"

# 結合
let c = a + ", " + b   # "Hello, World"

# 比較（辞書順）
print(a == b)   # false
print(a != b)   # true
print(a < b)    # true（"H" < "W"）

# 長さ
print(len(a))   # 5

# 部分文字列チェック
let s = "Hello, World!"
print(contains(s, "World"))      # true
print(starts_with(s, "Hello"))   # true
print(ends_with(s, "!"))         # true
```

---

## エスケープシーケンス

文字列中で以下のエスケープシーケンスが使えます。

| シーケンス | 意味 |
|------------|------|
| `\n` | 改行 |
| `\t` | タブ |
| `\\` | バックスラッシュ |
| `\"` | ダブルクォート |
| `\0` | ヌル文字 |

```python
print("Hello\nWorld")   # 2行に分けて出力
print("A\tB")           # タブ区切り
print("say \"hi\"")     # ダブルクォートを含む文字列
```

---

## 再代入のルール

`let` で宣言した変数は再代入できます。ただし、以下の制限があります。

```python
let x = 10
x = 20        # OK: 同じ型への再代入
# x = "text" # エラー: 型を変更する再代入は禁止
```

`const` は再代入できません。

```python
const N = 5
# N = 6  # エラー: 定数への再代入は禁止
```

同名の変数を再宣言することもできません。

```python
let x = 1
# let x = 2  # エラー: 同名の再宣言は禁止
```

---

← [01 - はじめに](01-getting-started.md) / 次 → [03 - 演算子](03-operators.md)
