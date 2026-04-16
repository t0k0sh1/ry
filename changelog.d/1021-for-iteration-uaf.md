### Fixed

- Fixed use-after-free when mutating a list, set, or map during `for` iteration.
  The loop now snapshots the iterable at entry via an ARC retain; mutations through
  the source alias inside the loop body trigger copy-on-write and do not affect the
  iteration — appended elements are not visited, and removed elements are still
  visited (#1021).
