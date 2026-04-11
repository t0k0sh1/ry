### Fixed

- Corrected `` `match value:` `` references to `` `case value:` `` in the pattern matching tutorial — the actual keyword is `case` (#889)
- Rewrote the networking example in the concurrency tutorial so the server/client snippets match runnable `net` test code (#889)
- Replaced outdated "struct" phrasing in `README.md` and `docs/README.md` with "record" to match the Ry keyword (#889)
- Updated the install one-liner in `README.md` to the current release version (#889)
- Added the `@describe` / `@it` directive-based test style to the testing tutorial and to the directives reference, so the new preferred syntax is actually documented (#889)
- Expanded the `README.md` feature list to mention pattern matching, the built-in testing framework, union types, GC (`std.gc`), and the `?` error propagation operator (#889)
- Expanded the `README.md` directives line beyond `@deprecated` to include the other common directives (#889)
- Added an explicit "In-Place Mutating Variants" section to the collections reference covering `append!`, `sort!`, `reverse!`, and the non-mutating `appended` counterpart (#889)

### Changed

- `remove_at(values: List<int>, index: int)` in `share/std/list.ry` is now declared to return `int` instead of `Unit`, matching both the runtime implementation and the existing `collections.test.ry` expectations (#889)
