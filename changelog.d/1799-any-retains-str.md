### Fixed

- Fixed a use-after-free when storing a dynamically-allocated `str`
  (from `+` concatenation, `toString`, runtime construction, etc.) in
  an `any` value. Wrapping a `str` in `any` now retains the underlying
  `StringHeader` so the inner buffer outlives the source binding;
  unwrapping back to `str`, copying `any` to `any`, and reassigning
  `any` to a different `str` all emit symmetric retain/release calls.
  Literal-backed strings remain unaffected (they are marked
  `ARC_IMMORTAL` and the retain/release become no-ops). Previously,
  `let s = "a" + "b"; let a: any = s; s = "..."` left `a` pointing at
  freed heap; this is now sound. Closes the str half of the broader
  `any` ARC integration started in #1697. (#1799)
