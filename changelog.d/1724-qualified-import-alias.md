### Added

- Extended qualified import (#1723) with the alias clause:
  `import <module> as <local>` registers `<local>` as the effective
  module name, so `import math as m` makes `m.sqrt(2.0)` and `m.PI`
  work. The alias **replaces** the original name (Python-style): bare
  `math.sqrt(2.0)` after `import math as m` is no longer routed to the
  qualified-call path. The alias must be camelCase, and two imports
  whose effective names collide (e.g. `import math as m` followed by
  `import path as m`) are a parse error. The original module name is
  preserved internally so user-defined-module rejection diagnostics
  still cite the actual file (`from mymod import greet`, not
  `from m import greet`). (#1724)
