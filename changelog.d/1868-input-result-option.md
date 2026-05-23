### Changed

- **Breaking:** Changed the signature of the `input()` builtin from
  `() -> str` / `(prompt: str) -> str` to
  `() -> Result<Option<str>, Error>` /
  `(prompt: str) -> Result<Option<str>, Error>`, mirroring the
  stdin `readLine()` change in #1850. Previously `input()` returned
  `""` for EOF, an empty input line, and I/O errors alike, so callers
  could not tell them apart. The new shape returns `Ok(Some(line))`
  on a successful read (trailing newline removed), `Ok(None)` at
  EOF, and `Err(e)` on I/O failure. Migration:

  ```ry
  # before
  name = input("Name? ")
  print(f"Hello, {name}!")

  # after
  case input("Name? "):
      Ok(opt):
          case opt:
              Some(name): print(f"Hello, {name}!")
              None: print("(EOF)")
      Err(e): print(e.message)
  ```

  The `input()` builtin and stdin `readLine()` now share the same
  semantics; pick whichever fits the call site (no `import` for
  `input()`, explicit `from io import readLine` for the other). (#1868)
