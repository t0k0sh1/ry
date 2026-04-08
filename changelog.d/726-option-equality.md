### Fixed

- Option equality (`==` / `!=`) now correctly compares inner values when both operands are `Some`, instead of comparing only the `has_value` flag (#726)
