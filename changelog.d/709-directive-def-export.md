### Added

- `DirectiveDefStmt` (e.g. `@directive(target="function", stage="compile") fn name(params)`) is now exportable from packages. Both wildcard (`from pkg`) and named (`from pkg import name`) imports include directive definitions, with the same `_`-prefix privacy rules as functions and types. (#709)
