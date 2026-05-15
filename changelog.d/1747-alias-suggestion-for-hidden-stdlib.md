### Fixed

- When a stdlib module is imported under an alias (e.g. `import math as
  m`), the canonical name is hidden per the Python-style contract
  (`docs/reference/modules.md`). Writing bare `math.sqrt(...)` /
  `math.PI` after such an import now produces a targeted suggestion:
  `'math' is not defined. Did you mean 'm' (aliased from 'math')?`.
  Previously the diagnostic from #1746 fired with the generic
  `module 'math' is not imported (add 'import math' ...)` message,
  which was misleading because the user had already imported the
  module — just under a different name. The unaliased case
  (`math.sqrt(4.0)` with no `import math` at all) keeps the original
  hint unchanged. (#1747)
