### Removed

- **Breaking change**: legacy な stdlib import 形式 (`from math import …` / `from std.math import …` / `from std import …` / `import math` 等、13 個の `ry.*` 公開モジュールに対する flat / `std.*` 形式) を hard error に昇格した (#2350 の deprecation warning から変更)。canonical な `ry.*` 形式 (`from ry.math import …` / `import ry.math` / `from ry.lang import …`) のみが受理される。user-defined module (`math.ry` 等、stdlib 名と同名のローカルファイル) は従来どおり referrer dir で先に解決されるため (`from_stdlib=false`)、影響を受けない。`tests/spec/` (216 ファイル) / `examples/` (8 ファイル) / `docs/reference/` (19 ファイル) 配下の全 legacy 形式を canonical 形式へ移行済み。 (#2351)
