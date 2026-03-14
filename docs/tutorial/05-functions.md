[English](05-functions.md) | [日本語](../ja/tutorial/05-functions.md) | [繁體中文](../zh/tutorial/05-functions.md)

# Functions

[<- Prev: Control Flow](04-control-flow.md) | [Next: Structs ->](06-structs.md)

---

## Basic Function Definition

Functions are defined with the `fn` keyword. Parameter type declarations are required and use the `name: type` format. The return type is specified after `->`.

```python
fn add(a: int, b: int) -> int:
    return a + b
```

- Parameter type declarations are required.
- The return type is specified after `->`.
- Use `return` to return a value.

---

## Calling Functions

Call a defined function by its name with arguments.

```python
fn multiply(x: int, y: int) -> int:
    return x * y

let result = multiply(3, 4)
print(result)   # 12
```

---

## Recursive Functions

Functions can call themselves (recursion).

```python
fn factorial(n: int) -> int:
    if n <= 1:
        return 1
    return n * factorial(n - 1)

print(factorial(5))   # 120
print(factorial(0))   # 1
```

---

## Function Overloading

You can define multiple functions with the same name but different parameter counts or types.

```python
fn add(a: int, b: int) -> int:
    return a + b

fn add(a: float, b: float) -> float:
    return a + b

print(add(1, 2))       # 3
print(add(1.5, 2.5))   # 4
```

The appropriate function is automatically selected based on the argument types at the call site.

> **Note**: Defining functions with identical parameter types but different return types causes a compile error.

---

## Omitting the Return Type (Unit Type)

If a function does not need to return a value, you can omit `->`. In this case, the function returns the Unit type.

```python
fn greet():
    print(42)

greet()   # 42
```

This is the simplest form of a function with no parameters and no return value.

---

[<- Prev: Control Flow](04-control-flow.md) | [Next: Structs ->](06-structs.md)
