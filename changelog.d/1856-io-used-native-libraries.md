### Fixed

- Fixed JIT unresolved symbol errors (`Symbols not found: [ ___ry_io_file_open ]`
  / `[ ___ry_write_text ]`) when a program imports only a subset of the
  `io` module that triggers a custom-emitter or inline codegen path —
  e.g. `from io import open` (without `close`), `from io import readAll`,
  `from io import lines`, `from io import writeText` (path-string
  overload). The custom emitters in `dispatchIO` (`emitFileOpen`,
  `emitFileReadAll`, `emitFileReadLine`, `emitFileLines`,
  `emitFileWriteText`) and the inline `writeText(path, content)` branch
  all bypass `emitTableDrivenNativeCall`, so the `sig.library`-driven
  `used_native_libraries_.insert("io")` never ran and the JIT failed to
  load `libry_io.dylib`. Only programs that also imported `close`
  happened to work, because `close` registers the library explicitly in
  `codegen_call.cpp`. `dispatchIO` now registers the `io` library once
  at the top so every dispatch path resolves correctly. (#1856)
