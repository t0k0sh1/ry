### Changed

- **Breaking:** Changed the signature of the stdin `readLine()` builtin
  in `share/std/io/io.ry` from `() -> str` to
  `() -> Result<Option<str>, Error>`, mirroring the File-handle variant
  introduced in #1816. Previously `readLine()` returned `""` for both
  EOF and an empty input line, so callers could not distinguish "stdin
  closed" from "user pressed Enter on an empty line". The new shape
  returns `Ok(Some(line))` on success (trailing newline removed),
  `Ok(None)` at EOF, and `Err(e)` on I/O failure. Migration:

  ```ry
  # before
  from io import readLine
  name = readLine()
  print(f"Hello, {name}!")

  # after
  from io import readLine
  case readLine():
      Ok(opt):
          case opt:
              Some(name): print(f"Hello, {name}!")
              None: print("(EOF)")
      Err(e): print(e.message)
  ```

  The `input()` builtin is unchanged and still returns a bare `str`,
  so short scripts that do not need EOF distinction can continue using
  it; whether to give `input()` the same EOF distinction is tracked
  separately in #1868. (#1850)
