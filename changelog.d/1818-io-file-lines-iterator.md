### Added

- `io.lines(f: File) -> Iterator<str>` lazy line iterator. Pair with
  `for line in lines(f) { ... }` to process large files without loading
  them into memory. The iterator retains the underlying `File` for its
  lifetime and shares the read position with subsequent `readLine` /
  `lines` calls. After `close(f)`, iteration terminates at the next
  step rather than raising (Python-compatible). Closes the `#1700`
  series of streaming-IO requests (`#1816` File handles, `#1817`
  `using` statement, `#1818` `lines()` + `Iterator<T>`). (#1818)
