### Added

- `case` statement and expression unify `when` (conditional branching) and `match` (pattern matching) into a single construct (#799). Two forms are supported: `case:` for multi-branch conditionals without a subject (replaces `when:`) and `case <expr>:` for pattern matching with a subject (replaces `match`). Both forms support a block body (`:`) and a single-expression body (`=>`). Use `_` as the wildcard/default arm instead of `else`.
- `if` expression syntax for two-branch conditional values (#798). Supports both a single-expression form (`if cond => true_value else false_value`) and a block form (`if cond: body else: body`) with tail-expression semantics. For multi-branch expressions, use `case:` instead.

### Removed

- **Breaking**: The `when` and `match` keywords have been removed (#800). Legacy code using these keywords must migrate to `case`. Migration table:

  | Before | After |
  |---|---|
  | `when:` | `case:` |
  | `match value:` with `case pattern:` arms | `case value:` with bare `pattern:` arms |
  | `else:` / `else =>` inside `when` arms | `_:` / `_ =>` |
