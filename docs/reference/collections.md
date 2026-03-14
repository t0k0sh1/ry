[English](collections.md) | [日本語](../ja/reference/collections.md) | [繁體中文](../zh/reference/collections.md)

# Collection Reference (Tuple, List, Map, Set)

## Tuple

### Overview

A fixed-length combination of heterogeneous values. Implemented as a stack-allocated value type using LLVM literal StructType.

### Syntax

```python
let t = (1, 3.14)
let t: (int, float) = (1, 3.14)
```

### Type Annotation

```python
let pair: (int, str) = (42, "hello")
let triple: (int, float, bool) = (1, 2.0, true)
```

### Element Access

Access elements using numeric indices `.0`, `.1`, etc.

```python
let t = (10, 3.14)
print(t.0)   # 10
print(t.1)   # 3.14
```

### Function Return Values

```python
fn swap(a: int, b: int) -> (int, int):
    return (b, a)

let result = swap(1, 2)
print(result.0)   # 2
print(result.1)   # 1
```

### Constraints and Errors

| Constraint | Details |
|------|------|
| Out-of-range index | Compile error |
| Passing a tuple directly to `print` | Compile error (not supported by print) |

---

## List

### Overview

A variable-length sequence of elements of the same type. Allocated on the heap.

### Syntax

```python
let xs = [1, 2, 3]
let xs: List<int> = [1, 2, 3]
```

### Supported Element Types

`int`, `float`, `bool`, `str`

### Index Access

```python
let xs = [1, 2, 3]
print(xs[0])   # 1
print(xs[2])   # 3
```

### Index Assignment

```python
let xs = [1, 2, 3]
xs[0] = 99
print(xs[0])   # 99
```

### len

```python
let xs = [1, 2, 3]
print(len(xs))   # 3
```

### print

```python
let xs = [1, 2, 3]
print(xs)   # [1, 2, 3]
```

### for Iteration

```python
let xs = [10, 20, 30]
for x in xs:
    print(x)
# 10
# 20
# 30
```

### Constraints and Errors

| Constraint | Details |
|------|------|
| All elements must be the same type | Mixed types cause a compile error |
| Empty list `[]` | Compile error because type cannot be inferred |
| Out-of-range access | Runtime error (exit(1)) |

---

## Map

### Overview

A key-value mapping. Allocated on the heap.

### Syntax

```python
let m = {"a": 1, "b": 2}
let m: Map<str, int> = {"a": 1, "b": 2}
```

### Key Access

```python
let m = {"a": 1, "b": 2}
print(m["a"])   # 1
```

### Insert and Update

```python
let m = {"a": 1}
m["b"] = 2     # Insert new entry
m["a"] = 99    # Update existing entry
```

### len

```python
let m = {"a": 1, "b": 2, "c": 3}
print(len(m))   # 3
```

### print

```python
let m = {"a": 1, "b": 2}
print(m)   # {a: 1, b: 2}
```

### has_key

```python
let m = {"a": 1, "b": 2}
print(m.has_key("a"))   # true
print(m.has_key("z"))   # false
```

### Constraints and Errors

| Constraint | Details |
|------|------|
| All keys must be the same type | Mixed key types cause a compile error |
| All values must be the same type | Mixed value types cause a compile error |
| Empty map | Type annotation is required (e.g., `let m: Map<str, int> = {"a": 1}`) |
| Accessing a non-existent key | Runtime error (exit(1)) |
| Key lookup | Linear scan |
| Capacity overflow | Automatically doubles in size |

---

## Set

### Overview

A collection that holds elements of the same type without duplicates. Allocated on the heap.

### Syntax

```python
let s = {1, 2, 3}
let s: Set<int> = {1, 2, 3}
```

### Supported Element Types

`int`, `float`, `bool`, `str`

### in Operator (Membership Check)

```python
let s = {1, 2, 3}
print(2 in s)   # true
print(5 in s)   # false
```

### len

```python
let s = {1, 2, 3}
print(len(s))   # 3
```

### print

```python
let s = {1, 2, 3}
print(s)   # {1, 2, 3}
```

### add (Add Element)

Duplicate elements are ignored when added.

```python
let s = {1, 2, 3}
s.add(4)         # Add
s.add(1)         # Ignored because it already exists
print(len(s))    # 4
```

### remove (Remove Element)

```python
let s = {1, 2, 3}
s.remove(2)
print(2 in s)   # false
```

### for Iteration

```python
let s = {10, 20, 30}
for x in s:
    print(x)
```

### Empty Set

An empty set requires a type annotation.

```python
let s: Set<int> = {}
```

### Function Parameters

```python
fn has_value(s: Set<int>, v: int) -> bool:
    return v in s
```

### Constraints and Errors

| Constraint | Details |
|------|------|
| All elements must be the same type | Mixed types cause a compile error |
| Empty set `{}` | Type annotation is required |
| Element lookup | Linear scan |
| Capacity overflow | Automatically doubles in size |
