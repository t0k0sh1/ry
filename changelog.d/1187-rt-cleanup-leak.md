### Fixed

- Eliminate intermittent SIGABRT/SIGBUS in `ry test -p` triggered by
  `tests/spec/combinatorial/collection_element.test.ry` during JIT
  teardown by cancelling the ResourceTracker scope_exit before leaking
  the LLJIT (#1187)
