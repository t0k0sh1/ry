### Fixed

- `reduce` with a lambda that omits parameter type annotations now
  returns the correct result. Previously, on `List<int>` (and other
  primitive lists) the accumulator seed was stored as a narrow value
  into a 16-byte `any` slot, leaving the payload uninitialized and
  producing garbage values like `14.0` instead of `15` (#1020).
