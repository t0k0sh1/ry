### Added

- Nested patterns are now supported inside ADT enum constructor pattern arms (#990).
  Each binding position may be a variable, a literal, a wildcard, or a tuple pattern.
  A single tuple pattern whose arity matches the variant's field count is unwrapped
  and matched field-by-field, so `Event::Click((0, 0))`, `Event::Click((x, y))`,
  `Event::Click((_, y))`, and `Wrapper::Val(42)` all work as expected. Plain variable
  bindings (`Shape::Circle(r)`) continue to work unchanged.
