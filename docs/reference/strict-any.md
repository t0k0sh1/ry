# Strict-any semantics

Strict-any is the **default** set of compile-time rules governing the `any` type as of v0.0.30 (#2322). Each rule is identified by a short kebab-case id (`[strict-any/<rule>]`) in the diagnostic, so users can grep their output and follow-up rules slot into the same diagnostic shape.

The `strict-any/` prefix is retained as the rule namespace even though the semantics are no longer opt-in — there is no compatibility flag or environment variable to restore the previous permissive behaviour. The recovery path is `asType[T]` / `isType[T]` (#2315) plus the canonical `case` narrowing pattern.

## Rule catalog

### `any-arithmetic`

Direct arithmetic and ordering comparisons on a value whose static type is `any` are rejected:

- Binary operators `+`, `-`, `*`, `/`, `%`, `//`, `**`
- Unary `-` (unary `+` is an identity no-op and stays allowed)
- Ordering comparisons `<`, `<=`, `>`, `>=`

Equality (`==`, `!=`) is intentionally permitted: `__ry_any_eq` returns 0 on type mismatch (safe), whereas ordering would trap at runtime on heterogeneous operands.

Example rejection:

```ry
a: any = 1
b: any = 2
print(a + b)
# error[strict-any/any-arithmetic]: direct '+' on 'any' is not permitted ...
print(a < b)
# error[strict-any/any-arithmetic]: direct '<' on 'any' is not permitted ...
```

To fix:

```ry
a: any = 1
b: any = 2

# Option 1: annotate the operands.
ai: int = a   # rejected — see any-implicit-unwrap below
# Use asType[T] instead:
case asType[int](a):
    Ok(ai):
        case asType[int](b):
            Ok(bi): print(ai + bi)
            Err(_): print("b not int")
    Err(_): print("a not int")
```

### `any-implicit-unwrap`

Implicit conversion of an `any`-typed value to a concrete type — the Path 9 sites enumerated in [`docs/architecture/implicit-any-paths.md`](../architecture/implicit-any-paths.md) — is rejected. As of #2379 the rule covers seven structurally similar hazard categories:

- Variable declaration: `n: int = v` where `v: any` (Path 9a).
- Named function-call argument: `f(v)` (and the default-value branch) where the parameter is concrete (Path 9b).
- Lambda-call argument: `g(v)` where `g: (int) -> int` (Path 9c).
- `Ok(v)` / `Err(v)` / `Some(v)` flowing into a `Result<T, E>` / `Option<T>` slot whose payload type is concrete (Path 9d).
- Reassignment of a previously-declared typed variable (local or module-global): `x: int = 1; x = v` where `v: any` (Path 9e, #2379). Also covers `Result` slot widening on reassignment — `r: Result<int, str> = Ok(0); r = produce()` where `produce()` returns `Result<any, str>` (or `Result<int, any>`) — because the per-slot `any → concrete` coercion inside `coerceResultType` is the same hazard class.
- Returning `any` from a typed function or lambda: `fn f() -> int: return v` where `v: any` (Path 9f, #2379).
- Mutating a typed collection with an `any` value: `append!`, `appended`, `insert`, `add` (Set), `remove` (Set), `m[k] = v` (Map index assign), and `xs[i] = v` (List index assign) — all rejected when the receiver has a concrete element type and the value is `any` (Path 9g, #2379).

Example rejection:

```ry
v: any = 1
n: int = v
# error[strict-any/any-implicit-unwrap]: assigning 'any' to variable 'n' of type 'int' performs an implicit runtime unwrap; use a checked cast such as 'asType[int](...)' or 'case' narrowing for safety
```

To fix, use `asType[T]` (#2315) with `case` narrowing:

```ry
v: any = 1
case asType[int](v):
    Ok(n): print(n)
    Err(_): print("not int")
```

The rule does not affect explicit `any` boundaries (`v: any = ...`, `from ry.json import load`, FFI `@extern` returns) — those remain valid. Read-only `any → concrete` paths whose surface is not insertion or assignment — the `in` / `not in` membership query and the `get(list, idx, default)` fallback — also remain valid; the runtime unwrap they perform is internal to the operator rather than a slot-bound coercion the user introduced.

## Migration cookbook

The canonical recovery for both rules is `case asType[T](v)` narrowing. `asType[T]` returns `Result<T, Error>` and never traps — Err carries a message of the form `asType[T]: expected T` for diagnostics. For scalar payloads (`int`, `float`, `str`, `bool`), records, record subtype projections, and `Option<T>`, `asType[T]` reuses the same `tryUnwrapFromAny` path that previously powered the implicit unwrap — only the call surface changed.

The nested narrow-each-operand fix for `any-arithmetic` and the per-site fix for `any-implicit-unwrap` are shown in [`docs/reference/types.md` § Arithmetic and Ordering Operations](types.md#arithmetic-and-ordering-operations) and § Passing any to Typed Functions.

For JSON / JSON5 readers and other dynamic-data boundaries, prefer `load[Map<str, any>](...)` / `load[List<any>](...)` and narrow each leaf with `asType[T]` (or use `Map.getPath` / dot-sugar to descend before narrowing).

### Roundtrip recovery from a native-typed `any` source

In addition to JSON-shape sources, `asType[T]` recovers values that were wrapped from a native-typed source via `v: any = ...`. The runtime descriptor stored on the boxed value (for `Result` / enum) or the typed-collection side table (for `List<T>` / `Map<str, V>` / `Set<T>`) gates the unwrap, so a mismatched source returns `Err` rather than mis-reading the payload.

```ry
# Typed collections — works for both JSON-shape and native-typed sources.
xs: List<int> = [1, 2, 3]
v: any = xs
case asType[List<int>](v):
    Ok(out): use(out[0])
    Err(_): ...

# Result<T, E> — descriptor-gated; mismatched Result types return Err.
r: Result<int, str> = Ok(42)
v: any = r
case asType[Result<int, str>](v):
    Ok(out):
        case out:
            Ok(n): use(n)
            Err(_): ...
    Err(_): ...

# Simple or ADT enum.
v: any = Color::Red
case asType[Color](v):
    Ok(c): use(c)
    Err(_): ...

v: any = Shape::Rect(3, 4)
case asType[Shape](v):
    Ok(s):
        case s:
            Shape::Rect(w, h): use(w, h)
            ...
    Err(_): ...
```

Coverage matrix:

| Target shape | JSON-shape source (`List<any>` etc) | Native-typed source |
|---|---|---|
| `List<T>` / `Map<str, V>` (T/V ≠ any) | iterates and narrows each element | passthrough with ARC retain when the registered name matches |
| `Set<T>` (T ≠ any) | not supported (Err) | passthrough when registered name matches |
| `Result<T, E>` | not applicable | descriptor-gated unwrap |
| `Option<T>` | `Unit`→`None`, scalar→`Some` | descriptor-gated unwrap |
| Simple / ADT enum | not applicable | descriptor-gated unwrap |

A mismatched source returns `Err` with a prefixed `asType[T]: ...` message.

## v0.0.30 issue ladder

The migration to default strict-any landed across a sequence of issues; the table below records the final state.

| Issue | Contribution |
|---|---|
| #2315 | Adds `asType[T]` / `isType[T]` checked-cast and type-test builtins. |
| #2316 | Promotes direct arithmetic and ordering on `any` to deprecation warnings before #2322 raised them to errors. |
| #2317 | Adds broad-`any`-usage lint warnings (Pattern 1–4) — still emitted for the patterns not covered by strict-any. |
| #2318 | Redefines `any` as an explicit type-erased boundary in the type reference. |
| #2319 | Lands the strict-any framework (`[strict-any/<rule>]` tag, `any-arithmetic` rule) as an opt-in mode. |
| #2320 | Documents the 10 implicit-`any` creation paths in `docs/architecture/implicit-any-paths.md`. |
| #2321 | Adds the `any-implicit-unwrap` rule covering Path 9a–9d. |
| #2323 | Adds Pattern 3 lint warning for unannotated lambda parameters. |
| #2322 | Promotes strict-any to the compiler default; removes the `--strict-any` / `RY_STRICT_ANY` opt-in. |
| #2378 | Extends `asType[T]` to recover native-typed collections, `Result<T, E>`, and simple / ADT enums so every shape rejected by `any-implicit-unwrap` has a canonical `case asType[T](v)` recovery. |
