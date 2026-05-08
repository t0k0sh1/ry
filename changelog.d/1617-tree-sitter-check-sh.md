### Added

- Added `editor/tree-sitter/check.sh` and `editor/tree-sitter/expected-fail.txt`
  as a Phase 1 corpus smoke-check for the in-tree tree-sitter grammar.
  `check.sh` runs `tree-sitter parse` against every
  `tests/spec/**/*.test.ry` and treats any `ERROR` / `MISSING` node as
  a regression unless the file is listed in `expected-fail.txt` — the
  single place where tolerated divergence is recorded. Files that move
  out of the gap list automatically surface a `WARN: ... now passes`
  on the next run so the entry can be retired in the same PR. The
  initial `expected-fail.txt` clusters the 38 currently failing fixtures
  into six named buckets (tuple member access, generic syntax variants,
  lambda-block bodies, numeric literal forms, async / decorator /
  operator-overload declarations, and other surface gaps).
  `pre-commit-checklist` §3.6.5 now invokes `./check.sh --no-build`
  alongside the existing `build.sh` + `install.sh --no-build` gate.
  Phase 2 (hand-curated `tree-sitter test` corpus with S-expression
  assertions) remains tracked in #1633. (#1617)
