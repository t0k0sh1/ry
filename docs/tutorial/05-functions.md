[English](05-functions.md) | [日本語](../ja/tutorial/05-functions.md) | [繁體中文](../zh/tutorial/05-functions.md)

# Functions

[<- Prev: Control Flow](04-control-flow.md) | [Next: Records and Enums ->](06-records.md)

---

## Basic Function Definition

Functions are defined with the `function` keyword. Parameter types use the `name: type` format. If a type is omitted, it defaults to `any`. The return type is specified after `->`.

```python
function add(a: int, b: int) -> int:
    return a + b
```

- Parameter type declarations are recommended. If omitted, the type defaults to `any`.
- The return type is specified after `->`.
- Use `return` to return a value.

---

## Calling Functions

Call a defined function by its name with arguments.

```python
function multiply(x: int, y: int) -> int:
    return x * y

result = multiply(3, 4)
print(result)   # 12
```

---

## Recursive Functions

Functions can call themselves (recursion).

```python
function factorial(n: int) -> int:
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
function add(a: int, b: int) -> int:
    return a + b

function add(a: float, b: float) -> float:
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
function greet():
    print(42)

greet()   # 42
```

This is the simplest form of a function with no parameters and no return value.

---

## Default Arguments

Parameters can have default values. When a caller omits those arguments, the default is used.

```python
function greet(name: str, greeting: str = "Hello") -> str:
    return f"{greeting}, {name}"

print(greet("Alice"))             # Hello, Alice
print(greet("Bob", "Good morning"))  # Good morning, Bob
```

You can have multiple default parameters:

```python
function connect(host: str, port: int = 8080, timeout: int = 30) -> str:
    return f"{host}:{port} (timeout={timeout})"

print(connect("localhost"))              # localhost:8080 (timeout=30)
print(connect("localhost", 3000))        # localhost:3000 (timeout=30)
print(connect("localhost", 3000, 10))    # localhost:3000 (timeout=10)
```

> **Why default arguments?** They let you keep a simple call site for common cases while allowing customization when needed — no need for multiple overloads.

> **Note**: Parameters with defaults must come after parameters without defaults.

---

## Lambda Functions

Lambda functions let you write functions as expressions. Single-expression lambdas use the form `(parameters) => expression`, while block lambdas use `(parameters):` followed by an indented block. In both cases, the return type is automatically inferred.

### Single-Expression Lambda

```python
double = (x: int) => x * 2
print(double(5))  # 10

add = (a: int, b: int) => a + b
print(add(3, 4))  # 7
```

### No-Parameter Lambda

```python
answer = () => 42
print(answer())  # 42
```

### Multi-Line Lambda

You can write multiple statements by adding a newline after `:` and indenting.

```python
abs = (x: int):
    if x < 0:
        return -x
    return x

print(abs(-5))  # 5
print(abs(3))   # 3
```

> **Why lambdas?** They are ideal for short, throwaway functions — especially as arguments to higher-order functions like `filter` and `map` (see below).

---

## Closures

Lambda functions can capture variables from the scope in which they are defined. This combination of a function and its captured environment is called a **closure**.

```python
offset = 10
add_offset = (x: int) => x + offset
print(add_offset(5))  # 15
```

Closures capture variables **by value** — changing the original variable after creating the closure does not affect the closure's copy.

```python
base = 10
f = (x: int) => x + base
base = 999
print(f(1))  # 11 (still uses the captured value 10)
```

This works in both directions — mutations inside the closure do not affect the outer variable either:

```python
counter = 0
items = [1, 2, 3]
items.map((x: int):
    counter += x    # Only modifies the closure's local copy
    return x
)
print(counter)  # 0 (outer variable unchanged)
```

> **Why capture by value?** It ensures safety and predictability — you can always reason about a variable's value by looking at the current scope alone, without worrying about mutations happening inside closures.
> **Why closures?** They let you create specialized functions on the fly. For example, you can create a family of adder functions from a single template.

---

## Higher-Order Functions

You can define functions that take other functions as arguments. Function types are written as `function(parameter_types) -> return_type`.

```python
function apply(f: function(int) -> int, x: int) -> int:
    return f(x)

double = (x: int) => x * 2
print(apply(double, 3))                # 6
print(apply((n: int) => n + 1, 10))  # 11
```

### Functions as Values

Named functions can also be bound to variables or passed as arguments — they behave just like lambdas.

```python
function square(x: int) -> int:
    return x * x

# Pass a named function as an argument
print(apply(square, 4))  # 16

# Bind to a variable
sq = square
print(sq(5))  # 25
```

> **Why higher-order functions?** They let you separate **what** to do from **how** to do it. The same `apply` function works with any transformation, making code more reusable. You have already seen this pattern in [Collections](07-collections.md) with `filter`, `map`, and `reduce`.

---

## UFCS (Uniform Function Call Syntax)

With UFCS, you can write `f(a, b)` as `a.f(b)`. The first argument is moved before the dot, enabling a method-chaining style.

```python
function add(a: int, b: int) -> int:
    return a + b

x = 1
print(x.add(2))   # add(x, 2) -> 3
```

### Chained Calls

UFCS really shines when you chain multiple calls — it reads left-to-right instead of inside-out:

```python
function double(n: int) -> int:
    return n * 2

# Chained (reads naturally: "take x, add 2, then double")
print(x.add(2).double())   # 6

# Equivalent nested calls (harder to read)
print(double(add(x, 2)))   # 6
```

> **Why UFCS?** It turns deeply nested function calls into readable left-to-right pipelines. You have already seen this with iterator chains like `xs.iter().filter(...).map(...).to_list()`.

---

## Exercises

1. **Default arguments**: Write a function `format_price(amount: int, currency: str = "USD", decimals: int = 2) -> str` that formats a price. Verify that `format_price(42)` and `format_price(42, "EUR")` both work.

2. **Higher-order function**: Write a function `apply_twice(f: function(int) -> int, x: int) -> int` that applies `f` to `x` twice (i.e., `f(f(x))`). Test it with `(x: int) => x + 1` and verify that `apply_twice((x: int) => x + 1, 5)` returns `7`.

3. **UFCS chaining**: Define `inc(n: int) -> int` (adds 1) and `triple(n: int) -> int` (multiplies by 3). Use UFCS to write `5.inc().triple()` and verify the result is `18`.

---

[<- Prev: Control Flow](04-control-flow.md) | [Next: Records and Enums ->](06-records.md)
