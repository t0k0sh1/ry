### Changed

- Self-referential enum diagnostic now also suggests `Task<T>` and `Channel<T>` as valid indirection wrappers, aligning the recommendation with the existing checker's acceptance. The message previously only mentioned `List`/`Map`/`Set`, even though pointer-backed `Task<T>` and `Channel<T>` are equally valid indirections. (#1351)
