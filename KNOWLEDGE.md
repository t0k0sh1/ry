# KNOWLEDGE

未分類知見の暫定バッファ。新たな教訓のうち既存 `.claude/rules/` / `.claude/skills/` のどれにも該当 entry を持たないものをここに蓄積し、安定後に rules または skills に昇格させる。

蓄積・参照・昇格・外部参照ポリシーの詳細は `/knowledge-md-management` 参照。

<!-- Entry format:
### <短く具体的な heading>

**Source**: <PR / issue / commit など出典>
**Tags**: <空白区切りキーワード>
**Rule**: <教訓本文>
-->

### Named arguments for builtins: generic parse, builtin-only dispatch

**Source**: #747 (2026-04-14, migrated from `codegen-stdlib-dispatcher.md` 2026-05-10)
**Tags**: codegen, builtins, named-args, parser, print

**Rule**: Ry has generic named-arg parsing (`std::vector<NamedArg> named_args` on `CallExpr`/`CallStmt`) but codegen-time restricts use to builtins. Non-builtin calls with named args emit a codegen error at `src/codegen_call_user.cpp:504` (`if (!s.named_args.empty() && builtins_.find(s.callee) == builtins_.end())`). The `=` token is never valid inside an expression (equality uses `==`), so the lookahead in `parseArgList()` is unambiguous.

**Why this design (option 3)**: Of three options considered — (1) full language-wide named args, (2) parser-side special-case for `print`, (3) generic parse + builtin-only codegen — option 3 was chosen to keep parser AST-agnostic without a full type-checker rewrite.

**How to apply**: When adding the next named-arg builtin, only `emitPrint()` in `src/codegen_call_io.cpp` and the `builtins_` map in `src/codegen.cpp` need updating. To lift the builtin-only restriction later, remove the check at `codegen_call_user.cpp:504` and add type-checker support.

### #1156: split match subject type into enum-only vs broad source name

**Source**: PR #1156 fix for `codegen_match.cpp` subjectEnumType wrong channel (migrated from `codegen-stdlib-dispatcher.md` 2026-05-10 — that file's `paths:` did not match `src/codegen_match.cpp`)
**Tags**: pattern, match, ARC, codegen, Option, Result, tuple, subjectEnumType

**Rule**: `emitPatternTest`, `emitPatternBindings`, and `checkMatchExhaustiveness` take **two** source-name parameters:

- `subjectEnumName` — narrow channel (`ValueMetadata::enum_value_type`). Only set for enum/ADT subjects (from `resolveEnumType()`). Used by `EnumPattern`/`EnumConstructorPattern` generic-instantiated lookup, by `Some`/`Ok`/`Err` binding `extractGenericTypeArg`, and by `VariablePattern` binding's `enum_value_type` write (still guarded by `enum_types_.count(resolveTypeAlias(name))`).
- `subjectSourceTypeName` — broad channel (`resolveSubjectSourceTypeName()`). Reconstructs `Option<T>` / `Result<T, E>` / `(T1, T2, ...)` from the LLVM subject type when no enum annotation exists. Used by `TuplePattern` / `RecordPattern` structural verification and by `emitPatternBindingArc` Path 2a from `VariablePattern` only.

**Reconstruction lossiness**: `reverseResolveTypeName(ptrTy_)` returns `"str"`; unknown structs return `"any"`. So `Option<List<int>>` reconstructs as `"Option<str>"`. The reconstructed string is therefore **only** safe for structural checks ("is this a tuple struct?" / "does this match record name X?"). Do NOT re-feed it into `extractGenericTypeArg` for ARC payload classification — `Option<List<int>>` would be misclassified as `Option<str>` (wrong header offset: str uses -24, List uses -16), causing heap corruption under ASan.

**Defense-in-depth on TuplePattern**: the test arm rejects via `!sTy || !isTupleStructType(sTy)` regardless of any source name. This fires even when both names are empty — closing the path where Option's `{i1, T}` 2-element struct silently passed the 2-arity check and crashed with ICmp type mismatch (original #1156 crash vector).

**Why split rather than unify**: overloading the previous single `subjectEnumType` parameter with non-enum names would force every consumer to add an `enum_types_.count(name)` guard. The split signature makes "enum-only" vs "broader subject type" a compile-time-enforced distinction.

**Follow-up**: Add a lossless `source_type_name` field to `ValueMetadata` so `Option<List<int>>` reconstruction is accurate, enabling ARC Path 2a for nested generics (currently handled by Path 2b heuristic via `propagateMeta`). Pre-existing defect in `emitListSlice`: ARC retain omitted for reference-typed elements (#1204) — tracked separately.

### Cross-check operator-level specs before flagging per-file code examples as drift

**Source**: #1118 PR 4 docs audit (migrated from `docs-reference-conventions.md` 2026-05-10 — procedural audit lesson, code-side guard exists at `src/codegen_expr.cpp:1941`)
**Tags**: documentation, audit, drift, operators

**Rule**: When a per-file doc example uses a language operator (`?`, `!!`, `case`, `@` directives, etc.), do NOT judge it solely by analogy with how that operator appears in fn bodies. Always: (1) Check `docs/reference/operators.md` for the full spec (including top-level usage rules); (2) Cross-check `src/codegen_expr.cpp` or the relevant codegen for the operator's desugar behavior in different contexts (function body vs. top level vs. lambda). For `?` operator specifically, the top-level desugar constraint is `src/codegen_expr.cpp:1941` (`Err` type must be `Error` exactly; codegen emits `'?' at top level: Result err type must be Error`).

**Why this matters**: During PR 4 audit of `docs/reference/base64.md`, a top-level `?` example was initially flagged as drift, but `docs/reference/operators.md:165-177` documents it as legal. The false alarm arose because only per-package docs were consulted, not the canonical operator spec.
