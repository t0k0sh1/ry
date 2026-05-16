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


