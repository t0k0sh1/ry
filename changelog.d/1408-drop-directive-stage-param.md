### Removed

- Removed the `stage` parameter from user-defined `@directive(...)` declarations. `@directive(target=[...]) fn name(...)` is now the canonical form. `@directive(target=[...], stage="compile")` is rejected as `unknown argument 'stage'` (hard error, no deprecation window). The `stage` knob conveyed no useful information today (only `"compile"` was accepted) and was reserved for a Tier 2 design (#1400) that has been declined. (#1408)
