### Changed

- **Breaking change**: strict-any semantics are now the compiler default. Code that previously emitted the deprecation warnings introduced by #2316 (direct arithmetic on `any`) and #2321 (implicit `any` → concrete unwrap on the four Path 9 sites) now fails to compile with the `[strict-any/<rule>]` diagnostic tag. Ordering comparisons (`<`, `<=`, `>`, `>=`) on `any` were promoted to the `any-arithmetic` rule alongside arithmetic operators (previously warned only). Equality (`==`, `!=`) and explicit `any` boundaries (`v: any = ...`, FFI returns, `from ry.json import load`) remain valid.

  Common fixes use `asType[T]` / `isType[T]` (#2315) with `case` narrowing:

  - Direct arithmetic / ordering on `any`: narrow each operand first.

    ```ry
    case asType[int](a):
        Ok(ai):
            case asType[int](b):
                Ok(bi): use(ai + bi)
                Err(_): ...
        Err(_): ...
    ```

  - Implicit unwrap (`n: int = anyVal` and the Path 9b–9d analogues): replace with `case asType[T](anyVal): Ok(n): ...`.

  - Unannotated function / lambda parameters: add an explicit type annotation, or `: any` if type-erasure is intentional. The Pattern 3 lint (#2317, #2323) still emits the lead-indicator warning; the function body's arithmetic / unwrap on the implicit-any parameter is what now refuses to compile.

  Some recovery shapes that previously worked via implicit unwrap initially lacked a canonical replacement on v0.0.30: reading back a *typed* collection from a native-sourced `any`, recovering a `Result<T, E>`, or recovering a simple / ADT enum value. #2378 closes that gap inside the same v0.0.30 milestone by extending `asType[T]` to those targets, so every shape rejected by `any-implicit-unwrap` now has a canonical `case asType[T](v)` recovery. `asType[Option<T>]` already worked at the time #2322 landed. See [Strict-any mode reference](docs/reference/strict-any.md) for the full migration cookbook. (#2322)

### Removed

- The `--strict-any` CLI flag and `RY_STRICT_ANY` environment variable were removed — strict semantics are now the default, so the opt-in entry points serve no purpose. Passing `--strict-any` is reported as an unknown option; `RY_STRICT_ANY` is silently ignored. (#2322)
