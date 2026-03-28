[English](../../tutorial/02-variables-and-types.md) | [日本語](02-variables-and-types.md) | [繁體中文](../../zh/tutorial/02-variables-and-types.md)

# 02 - 変数と型

← [01 - はじめに](01-getting-started.md) / 次 → [03 - 演算子](03-operators.md)

---

## 変数宣言

Ry では、シンプルな代入構文で変数を宣言します。デフォルトでは変数は可変です。

```python
x = 42        # int 型として推論
y = 3.14      # float 型として推論
flag = true   # bool 型として推論
name = "Ry"   # str 型として推論
```

---

## @const による不変変数（定数）宣言

`@const` ディレクティブで不変な変数（定数）を宣言します。宣言後に値を変更することはできません。

```python
@const
x = 42        # int 型として推論

@const
y = 3.14      # float 型として推論

@const
flag = true   # bool 型として推論

@const
name = "Ry"   # str 型として推論
```

---

## 型アノテーション

変数の型を明示的に指定できます。

```python
x: int = 42

rate: float = 0.5

ok: bool = false

msg: str = "hello"
```

型アノテーションと実際の値の型が一致しない場合はコンパイルエラーになります。

---

## 基本型

| 型 | 説明 | リテラル例 |
|----|------|-----------|
| `int` | 64ビット整数 | `0`, `42`, `-10` |
| `u8` | 符号なし8ビット整数（0-255） | `b: u8 = 42` |
| `float` | 64ビット浮動小数点数 | `0.0`, `3.14`, `-1.5` |
| `bool` | 真偽値 | `true`, `false` |
| `str` | 文字列 | `"hello"`, `""` |
| `[T; N]` | 固定長配列（低レベル型） | `buf: [i32; 4] = [1, 2, 3, 4]` |

---

## 文字列操作

文字列に対してさまざまな操作が使えます。

```python
a = "Hello"
b = "World"

# 結合
c = a + ", " + b   # "Hello, World"

# 比較（辞書順）
print(a == b)   # false
print(a != b)   # true
print(a < b)    # true（"H" < "W"）

# 長さ
print(length(a))   # 5

# 部分文字列チェック
s = "Hello, World!"
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
| `\r` | 復帰 |
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

`@const` なしで宣言した変数は再代入できます。ただし、以下の制限があります。

```python
x = 10
x = 20        # OK: 同じ型への再代入
# x = "text" # エラー: 型を変更する再代入は禁止
```

`@const` は再代入できません。

```python
@const
N = 5
# N = 6  # エラー: @const 変数への再代入は禁止
```

同名の変数を再宣言することもできません。

```python
x = 1
# 同一スコープでの同名の再宣言は禁止
```

---

## タプル分割代入

タプルを複数の変数に一度に展開できます。

```python
@const
a, b = (10, 20)
print(a)   # 10
print(b)   # 20
```

### ワイルドカード

`_` を使って特定の位置の値を無視できます。

```python
@const
x, _ = (1, 2)   # x のみが束縛される；2 は破棄
print(x)             # 1
```

### 可変変数での分割代入

`@const` を省略すると可変変数として宣言できます。

```python
a, b = (10, 20)
a = 99
print(a)   # 99
```

### ルール

- 左辺の変数の数はタプルの要素数と一致させる必要があります。
- 各変数は通常の `@const`/可変宣言と同じルールに従います。
- ネストされたタプルの分割代入はサポートされていません。

---

← [01 - はじめに](01-getting-started.md) / 次 → [03 - 演算子](03-operators.md)
