### Removed

- The bare CLI invocations `ry <file.ry>`, `ry --`, and `ry` (no args, runs entry from `package.toml`) are removed. Use `ry run <file.ry>`, `ry run --`, and `ry run` instead. The `ry run` subcommand also now honors the global `--emit-llvm-ir` flag (e.g. `ry --emit-llvm-ir run <file.ry>` replaces the old `ry --emit-llvm-ir <file.ry>` invocation). (#1735)
