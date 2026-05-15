### Fixed

- Calling `<mod>.fn(...)` or accessing `<mod>.field` where `<mod>` is the
  name of a registered stdlib module (e.g. `math`, `json`, `path`) but
  was not introduced via `import <mod>` now produces an actionable error:
  `module 'math' is not imported (add 'import math' at the top of the
  file)`. Previously the qualified call fell through to UFCS conversion
  (`math.sqrt(4.0)` → `sqrt(math, 4.0)`) and codegen surfaced a
  misleading `undefined function: sqrt (hint: forward references...)`
  diagnostic that pointed users away from the root cause. The check
  fires at codegen-dispatch time so a local variable that happens to
  share a stdlib name (e.g. `path: str = "/tmp"; path.basename()`)
  shadows the package and the existing diagnostic path is preserved.
  Bare unqualified calls (`sqrt(4.0)` without `from math import sqrt`)
  are out of scope and continue to use the forward-reference hint.
  (#1746)
