### Added

- Added `editor/tree-sitter/test/corpus/` with the Phase 2 hand-curated
  `tree-sitter test` corpus (53 cases across 8 files: imports, functions,
  literals, expressions, control flow, case match, lambdas, decorators).
  Each case pairs a Ry snippet with its expected S-expression so that
  grammar edits which silently change parse-tree shape are caught — a
  capability the Phase 1 ERROR/MISSING smoke-check (`check.sh`, #1617)
  cannot provide. Run with `tree-sitter test` from
  `editor/tree-sitter/`. Coverage scope is limited to grammar surface
  area that already parses cleanly today; gaps listed in
  `expected-fail.txt` are intentionally excluded so the harness stays
  green and shape regressions are unambiguous. (#1633)
