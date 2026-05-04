### Added

- `digits(n: int) -> List<int>` and `digits(n: int, base: int) -> List<int>`
  to the `math` module. Decomposes a non-negative integer into its digits
  low-first (least-significant digit at index 0), matching Ruby's
  `Integer#digits` (`digits(1234) == [4, 3, 2, 1]`, `digits(255, 16) == [15, 15]`,
  `digits(0) == [0]`). Default base is 10. Composes with `sum` for digit-sum
  in one expression: `sum(digits(1234)) == 10`. Aborts with a runtime error
  on negative `n` or `base < 2`. (#1578)
