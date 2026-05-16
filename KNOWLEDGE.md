# KNOWLEDGE

未分類知見の暫定バッファ。新たな教訓のうち既存 `.claude/rules/` / `.claude/skills/` のどれにも該当 entry を持たないものをここに蓄積し、安定後に rules または skills に昇格させる。

蓄積・参照・昇格・外部参照ポリシーの詳細は `/knowledge-md-management` 参照。

<!-- Entry format:
### <短く具体的な heading>

**Source**: <PR / issue / commit など出典>
**Tags**: <空白区切りキーワード>
**Rule**: <教訓本文>
-->

### `tests/spec/<name>/` directories collide with stdlib module names

**Source**: #1687 (2026-05-16 implementation)
**Tags**: testing, module-loader, stdlib, tests-spec, layout, collision, gotcha

**Context**: While adding new `tests/spec/testing/directive_*.test.ry` files for the `@skip` / `@only` / `@todo` directives, every test in `tests/` started failing with `'it' not found in module 'testing'`. The cause was the directory name `tests/spec/testing/` itself — `ry test`'s module loader treats every directory on the resolution path as a candidate package root, and the local `tests/spec/testing/` (which has no `it` symbol) shadows `share/std/testing/`. The collision is silent at build time and surfaces only as runtime import errors during test discovery.

**Rule**: Do not create a subdirectory under `tests/spec/` whose name matches a stdlib module (e.g. `testing`, `math`, `path`, `filesystem`, `crypto`, `io`, `json`, `regex`, `thread`, `time`, `http`, `str`, `list`, `map`, `set`, `option`, `result`). Place per-file tests at the top level of `tests/spec/` instead (`tests/spec/directive_skip.test.ry`, not `tests/spec/testing/directive_skip.test.ry`). Existing subdirectories such as `tests/spec/braced_import/`, `tests/spec/combinatorial/`, `tests/spec/concurrency/` are safe because their names do not match any stdlib module.

**How to apply**:
- When adding a new `.test.ry` file under a new subdirectory, grep `share/std/` first: `ls share/std/ | sort | uniq` — pick a directory name that does not appear in the list.
- If a stdlib module is renamed in the future, audit `tests/spec/` for newly-colliding directories at the same time.

### Pre-scan AST walkers must traverse every `ExprPtr` slot, or document the limitation inline

**Source**: #1765 (2026-05-16 implementation)
**Tags**: codegen, pre-scan, ast-walker, mock, spy, slot-coverage, blind-spot

**Context**: `collectTestTargetsFromStmt` (`src/codegen.cpp`) was introduced for the test-mode pre-scan that populates `mocked_functions_` / `spied_functions_`. The first version walked `LambdaExpr.body` only inside `CallStmt.args`, missing identical lambdas stored in `AssignStmt.value`, `ReturnStmt.value`, `CallExpr.args` (nested at any depth), `IfStmt.condition`, and other ExprPtr slots. The bug surfaces only when the gated callsite is codegen'd BEFORE the lambda body runs — typically inside a module-level helper fn declared above the `@describe` block. `emitMockCall` populates the same set at codegen time, masking the bug whenever the callsite happens to follow the mock-setup statement in emission order.

**Rule**: A pre-scan AST walker that needs to find anonymous nested constructs (lambda bodies, embedded directives, etc.) must traverse **every** `ExprPtr` and `StmtNode` slot reachable from each variant — not just the slots that happen to host the construct on the day the walker was written. Concretely: implement an `expr` visitor over all `ExprNode` alternatives in tandem with the stmt visitor, and route every stmt's ExprPtr field (`AssignStmt.value`, `ReturnStmt.value`, `IfStmt.branch.condition`, `WhileStmt.condition`, `ForStmt.iterable`, `ExpectStmt.{actual,expected,extra_args}`, etc.) through it. The expr visitor descends into compound nodes (`CallExpr.args`, `BinaryExpr.{lhs,rhs}`, `IfBlockExpr.{then_body,else_body}` — the only Expr that contains StmtNodes — and so on) so an anonymous LambdaExpr buried any number of levels deep is reached.

**Why**: Slot coverage gaps are silent at compile time and only surface as runtime correctness bugs in narrow code paths. The fix is mechanical (mirror the AST variant list), the test is unstable to write (depends on emission order to manifest), and the recurrence cost is high if a future pre-scan extension forgets the same slots.

**How to apply**:
- When adding a new pre-scan walker (`stmtHasOnlyDirective` analogues, future test-mode discovery passes, lint passes), implement the full ExprNode + StmtNode visitor pair up-front. If a slot is intentionally excluded (e.g. `RecordStmt.invariants` is dead for this walker's purpose), add an inline comment naming the slot and stating the reason — silent omission is the bug.
- When adding a new AST variant (new `ExprNode` or `StmtNode` alternative), grep for existing walkers in `src/codegen.cpp` and update them to include the new variant. Walkers that should `if constexpr (...)` over every variant include `collectTestTargetsFromStmt`, `collectTestTargetsFromExpr` (#1765), and `stmtHasOnlyDirective`.
- The `LambdaExpr` and `IfBlockExpr` alternatives are the only ExprNode variants that carry `std::vector<StmtNode>` — both must hand off to the stmt-walker (`collectTestTargetsFromStmts`) explicitly. Forgetting one of them re-introduces the same class of bug.


