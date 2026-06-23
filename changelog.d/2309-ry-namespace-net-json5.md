### Changed

- `ry.*` 予約 namespace の公開 allowlist に `ry.net` と `ry.json5` を追加し、13 モジュールに拡張。`from ry.net import bind` / `import ry.net` / `from ry.json5 import load, stringify` / `import ry.json5` が canonical な書き方として解決するようになる。bare 形式 (`from net import …` / `from json5 import …`) は compatibility alias として引き続き動作。`ry.bogus` 等を reject する際の `available: …` リストも 13 件表示に追従する。 (#2309)
