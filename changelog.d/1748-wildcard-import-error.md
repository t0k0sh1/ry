### Fixed

- `from x import *` and other wildcard import positions now produce a
  clear, actionable diagnostic instead of the misleading
  `expected function name after 'import'` message. The new error reads
  `selective import does not support wildcards ('from x import *');
  use 'from x import a, b' or 'from x import {a, b}' instead` and
  fires uniformly across all four wildcard positions:
  `from x import *`, `from x import {*}`, `from x import a, *`, and
  `from x import {a, *}`. Wildcard import remains intentionally
  unsupported; whether to add it is tracked separately. (#1748)
