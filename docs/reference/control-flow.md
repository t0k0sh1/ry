# 制御構文リファレンス

## if / elif / else

### 構文

```python
if 条件式:
    # then ブロック
elif 条件式:
    # elif ブロック（複数可）
else:
    # else ブロック（省略可）
```

### 条件式の型

| 型 | false になる値 | true になる値 |
|---|---|---|
| `bool` | `false` | `true` |
| `int` | `0` | 非 0 |

`float` や `str` は条件式に直接使用できない。

### 例

```python
let x = 10

if x > 5:
    print("big")
elif x == 5:
    print("five")
else:
    print("small")
```

### スコープルール

- `if` / `elif` / `else` の各ブロックはそれぞれ独立したブロックスコープを持つ。
- ブロック内で宣言した変数はブロック外からアクセスできない。

```python
if true:
    let y = 42
# y はここではアクセス不可
```

---

## while

### 構文

```python
while 条件式:
    # ループ本体
```

条件式が `true` の間、ループ本体を繰り返す。

### 例

```python
let i = 0
while i < 5:
    print(i)
    i += 1
```

### break / continue との組み合わせ

```python
let i = 0
while true:
    if i >= 3:
        break
    i += 1
```

---

## for

### 構文

```python
# リスト / セット走査
for x in iterable_expr:
    # x に各要素が代入される

# range（0 始まり）
for i in range(n):
    # i = 0, 1, ..., n-1

# range（開始・終了指定）
for i in range(start, end):
    # i = start, start+1, ..., end-1
```

### 例

```python
let xs = [10, 20, 30]
for x in xs:
    print(x)

let s = {1, 2, 3}
for x in s:
    print(x)

for i in range(5):
    print(i)     # 0 1 2 3 4

for i in range(2, 6):
    print(i)     # 2 3 4 5
```

---

## break

- 最も内側のループ（`while` または `for`）を即座に脱出する。
- ループの外で使用するとコンパイルエラー。

```python
for i in range(10):
    if i == 5:
        break    # i == 5 で脱出
    print(i)     # 0 1 2 3 4
```

### エラー例

```python
# ループ外での break はコンパイルエラー
break   # Error: break outside loop
```

---

## continue

- 最も内側のループの現在のイテレーションを終了し、次のイテレーションへスキップする。
- ループの外で使用するとコンパイルエラー。

```python
for i in range(5):
    if i == 2:
        continue   # i == 2 をスキップ
    print(i)       # 0 1 3 4
```

---

## スコープルール

### ブロックスコープ

- `if` / `elif` / `else` / `while` / `for` の各ブロックはブロックスコープを持つ。
- ブロック内で宣言した変数はブロックの終了と同時にスコープから外れる。

```python
for i in range(3):
    let tmp = i * 2
# tmp はここではアクセス不可

if true:
    let a = 1
# a はここではアクセス不可
```

### シャドーイング

- 内側のスコープで外側と同名の変数を宣言すると、内側のスコープ内では内側の変数が参照される。
- 内側スコープを抜けると外側の変数に戻る。

```python
let x = 10
if true:
    let x = 99   # 外側の x をシャドーイング
    print(x)     # 99
print(x)         # 10
```
