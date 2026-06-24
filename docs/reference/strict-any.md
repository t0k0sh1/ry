# Strict-any mode

`strict-any` is an opt-in compiler mode that enables stricter semantics for the `any` type ahead of those semantics becoming the default. It exists to give existing code a controlled migration window. Each rule the mode enforces is identified by a short kebab-case id (`[strict-any/<rule>]`) in the diagnostic, so users can grep their output and follow-up issues can extend the rule catalog without changing the diagnostic shape.

## Enabling strict-any

Two equivalent entry points:

```bash
# Environment variable — inherited by subprocesses spawned by `ry test`.
RY_STRICT_ANY=1 ry run app.ry

# CLI flag — implemented as a thin setenv of the same variable, so the
# subprocess inheritance behaviour is identical. Place the flag BEFORE
# the subcommand; `parseGlobalFlags` (src/cli/cli.cpp) consumes it during
# the pre-subcommand pass and the `test` runner's per-file subprocesses
# inherit the env var automatically.
ry --strict-any run app.ry
ry --strict-any test path/to/spec.test.ry
```

Strict-any is **off by default**. Setting it currently activates the rules listed in the next section. Adding the flag never changes the meaning of a successful compile — it only converts previously-accepted patterns into compile-time errors.

## Rule catalog

### `any-arithmetic`

Direct arithmetic on a value whose static type is `any` is rejected in strict-any mode. The rule covers:

- Binary operators `+`, `-`, `*`, `/`, `%`, `//`, `**`
- Unary `-` (unary `+` is an identity no-op and stays allowed)

Comparisons (`==`, `!=`, `<`, `<=`, `>`, `>=`) are still permitted because they always yield a concrete `bool` and do not rely on guessing the operand's runtime type.

Example rejection:

```ry
a: any = 1
b: any = 2
print(a + b)
# error[strict-any/any-arithmetic]: direct '+' on 'any' is not permitted ...
```

To fix:

```ry
a: any = 1
b: any = 2

# Option 1: annotate the operands.
ai: int = a
bi: int = b
print(ai + bi)

# Option 2: use asType[T] (#2315) to recover concrete values from any.
case asType[int](a):
    Ok(ai):
        case asType[int](b):
            Ok(bi): print(ai + bi)
            Err(_): print("b not int")
    Err(_): print("a not int")
```

### `any-implicit-unwrap`

Implicit conversion of an `any`-typed value to a concrete type — performed today via a runtime tag check in `unwrapFromAny` — is rejected in strict-any mode. The rule covers the four Path 9 sites enumerated in [`docs/architecture/implicit-any-paths.md`](../architecture/implicit-any-paths.md):

- Variable declaration: `n: int = v` where `v: any`.
- Named function-call argument: `f(v)` (and the default-value branch when the omitted default is `any`-typed).
- Lambda-call argument: `g(v)` where `g: (int) -> int`.
- `Ok(v)` / `Err(v)` / `Some(v)` flowing into a `Result<T, E>` / `Option<T>` slot whose payload type is concrete.

Compat mode keeps running such code but emits a warning at the same sites (with the existing Pattern 4 message template); strict mode promotes those warnings into a `[strict-any/any-implicit-unwrap]` hard error.

Example rejection:

```ry
v: any = 1
n: int = v
# error[strict-any/any-implicit-unwrap]: assigning 'any' to variable 'n' of type 'int' performs an implicit runtime unwrap; use a checked cast such as 'asType[int](...)' or 'case' narrowing for safety
```

To fix, use `asType[T]` (#2315) with `case` narrowing — the same recovery pattern shown in `any-arithmetic` above:

```ry
v: any = 1
case asType[int](v):
    Ok(n): print(n)
    Err(_): print("not int")
```

The rule does not affect explicit `any` boundaries (`v: any = ...`, `from ry.json import load`, FFI `@extern` returns) — those remain valid in both modes. Reassignment to a previously-declared variable, `return v` from a typed function, and collection mutation paths are structurally similar hazards but are out of scope for this rule and tracked separately.

## Relationship to upcoming changes

Strict-any is the framework that several v0.0.30 follow-up issues plug into:

| Issue | Adds to strict-any |
|---|---|
| #2316 | Restrict additional operators on `any` (bitwise, shifts) |
| #2317 | Warn-on-broad-`any`-usage lint rule |
| #2321 | Reject implicit unwrap from `any` to concrete type at assignment and call sites |
| #2323 | Reject implicit `any` for unannotated parameters |
| #2322 | Flip strict-any to the compiler default |

Until #2322 lands, strict-any remains opt-in. Once #2320 / #2324 have migrated the stdlib examples and projects off broad `any` use, the default flip becomes safe.

## Why an opt-in flag

Strict-any cannot ship as the default in v0.0.30 because existing `any`-heavy patterns — most notably dynamic-data flows through `from ry.json import load`, `from ry.json5 import load`, and typed-collection reads — are pervasive in user projects today. The flag gives those patterns a deprecation window: the new semantics are testable per-file and per-project, the diagnostic catalog grows incrementally, and the flip in #2322 becomes a mechanical change rather than a compatibility break.
