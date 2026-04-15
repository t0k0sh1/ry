### Changed

- ADT enum `==` / `!=` now compares the variant payload in addition to the tag.
  Previously two values with the same tag but different payload were incorrectly treated
  as equal (e.g. `Circle(1.0) == Circle(2.0)` returned `true`). (#959)
