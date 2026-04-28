### Changed

- User-defined directives applied to a target outside their declared `target=[...]` list now silently no-op instead of triggering undefined behavior. The compile succeeds, no diagnostic is emitted, and the directive's argument validation is also skipped. Built-in directives are unaffected. Note that for-loop and function-call use sites still reject all user-defined directives at the parser level (tracked separately in #1427). (#1425)
