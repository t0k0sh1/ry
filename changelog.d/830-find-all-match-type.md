### Changed

- `find_all` and `regex_find_all` now return `List<Match>` instead of `List<str>`. Each `Match` record has a `full: str` field (the matched text) and a `groups: List<str>` field (captured groups, in order). Patterns without capture groups return an empty `groups` list. (#830)
