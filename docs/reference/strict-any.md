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

Implicit conversion of an `any`-typed value to a concrete type — the four Path 9 sites enumerated in [`docs/architecture/implicit-any-paths.md`](../architecture/implicit-any-paths.md) — is rejected:

- Variable declaration: `n: int = v` where `v: any` (Path 9a).
- Named function-call argument: `f(v)` (and the default-value branch) where the parameter is concrete (Path 9b).
- Lambda-call argument: `g(v)` where `g: (int) -> int` (Path 9c).
- `Ok(v)` / `Err(v)` / `Some(v)` flowing into a `Result<T, E>` / `Option<T>` slot whose payload type is concrete (Path 9d).

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

The rule does not affect explicit `any` boundaries (`v: any = ...`, `from ry.json import load`, FFI `@extern` returns) — those remain valid. Reassignment to a previously-declared variable, `return v` from a typed function, and collection mutation paths are structurally similar hazards but are out of scope for the rule and tracked separately.

## Migration cookbook

The canonical recovery for both rules is `case asType[T](v)` narrowing. `asType[T]` returns `Result<T, Error>` and never traps — Err carries a message of the form `asType[T]: expected T` for diagnostics. For scalar payloads (`int`, `float`, `str`, `bool`), records, record subtype projections, and `Option<T>`, `asType[T]` reuses the same `tryUnwrapFromAny` path that previously powered the implicit unwrap — only the call surface changed.

The nested narrow-each-operand fix for `any-arithmetic` and the per-site fix for `any-implicit-unwrap` are shown in [`docs/reference/types.md` § Arithmetic and Ordering Operations](types.md#arithmetic-and-ordering-operations) and § Passing any to Typed Functions.

For JSON / JSON5 readers and other dynamic-data boundaries, prefer `load[Map<str, any>](...)` / `load[List<any>](...)` and narrow each leaf with `asType[T]` (or use `Map.getPath` / dot-sugar to descend before narrowing).

### Recovery shapes that lack a canonical migration on v0.0.30

`asType[T]` does not yet cover every shape that the implicit unwrap previously handled. The following patterns have no canonical replacement and must keep their values statically typed end-to-end (or be reshaped to descend through `Map<str, any>` / `List<any>` and narrow each leaf):

- **Native-source typed collection roundtrip** — `xs: List<int> = anyVal` where `anyVal` was wrapped from a previously-typed `List<int>` source. `asType[List<int>](anyVal)` accepts only JSON-shape (`List<any>` / `Map<str, any>` / `Set<any>`) sources today.
- **`Result<T, E>` recovery** — `r: Result<int, str> = anyVal` and `asType[Result<int, str>](anyVal)` both fail (`asType` returns Err with "Result/enum target not yet supported"). Case-based pattern matching directly on the runtime value isn't reachable either because the implicit unwrap site is the only way to introduce the typed `r` binding.
- **Enum recovery (simple or ADT)** — `c: Color = anyVal` and `asType[Color](anyVal)` both fail with the same "target not yet supported" message.

`asType[Option<T>]` already works on v0.0.30 and is the canonical recovery for `Option<T>` payloads.

Extending `asType[T]` to cover the cases above is tracked as a follow-up.

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
