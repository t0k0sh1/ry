### Fixed

- ADT enum variant payload fields with collection (`List`/`Map`/`Set`), nested enum, `Option`, or `Result` types now format correctly via `print` / `to_str` instead of rendering as an empty string, raw tag integer, or wrongly-nested value. Self-referential ADTs such as `enum Tree: Node(int, List<Tree>)` now print faithfully (#1238).
