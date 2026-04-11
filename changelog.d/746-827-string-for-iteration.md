### Added

- `for c in s:` now iterates a string character by character, yielding each UTF-8 code point as a single-character `str`. `enumerate(s)` and `zip(s, t)` also accept `str` arguments with the same semantics. (#746, #827)
