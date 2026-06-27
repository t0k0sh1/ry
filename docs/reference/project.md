# Project Management

## CLI Overview

```bash
echo '<code>' | ry -c               # Run code from stdin
ry test [options] [<file> | <dir>]  # Run tests
ry init                             # Initialize a project
ry new <project-name>               # Create a new project
ry run [<name>|<file.ry>|-- [args]] # Run a project script or Ry source file
ry fmt [options] [<file> | <dir>]   # Format source files
ry docs [options]                   # Generate static HTML API documentation
ry self-update [options]            # Update ry itself
```

### Global Options

| Option | Description |
|---|---|
| `-c` | Read and execute code from stdin |
| `-h`, `--help` | Show help |
| `-v`, `--version` | Show version |
| `--env=<env>` | Set environment. Valid values: `prod`/`production`, `dev`/`development`, `internal`, `test`, `staging`. Overrides the `RY_ENV` environment variable. See [Module Reference — RY_ENV](modules.md#ry_env) for details. |

### Stdin Execution

Use the `-c` flag to read and execute code from stdin:

```bash
echo 'print("hello")' | ry -c
```

---

## `ry init` - Project Initialization

Initializes the current directory as a Ry project.

```bash
ry init
```

### Generated Files and Directories

```
my-project/
  package.toml          # Project configuration file
  src/
    main.ry        # Entry point (sample code)
```

### Behavior

1. Exits with an error if `package.toml` already exists
2. Creates the `src/` directory (if it doesn't exist)
3. Generates `package.toml` (`name` is set to the current directory name with hyphens normalized to underscores)
4. Generates `src/main.ry` (skipped if it already exists)

---

## `ry new` - Create a New Project

Creates a new directory and initializes it as a Ry project.

```bash
ry new my-project
```

### Generated Files and Directories

```
my-project/
  package.toml          # Project configuration file
  src/
    main.ry        # Entry point (sample code)
```

### Behavior

1. Exits with an error if no project name is given
2. Exits with an error if the directory already exists
3. Creates the `<project-name>/` directory
4. Creates the `src/` directory inside it
5. Generates `package.toml` (`name` is set to the given project name with hyphens silently normalized to underscores, e.g. `my-project` → `my_project`)
6. Generates `src/main.ry`

---

## `ry run` - Run Project Scripts or Ry Source Files

`ry run` is a dual-purpose subcommand: it runs scripts defined in the `[scripts]` section of `package.toml`, **and** it runs Ry source files directly.

```bash
ry run                      # List all available scripts
ry run build                # Run the "build" script from [scripts]
ry run main.ry              # Run main.ry (resolved via [paths])
ry run src/main.ry          # Run a Ry file given by relative or absolute path
ry run main                 # Try [scripts].main, fall back to main.ry (via [paths])
ry run -- arg1 arg2         # Run the project entry point with arguments
ry run main.ry foo bar      # Run main.ry with positional arguments foo, bar
```

### Argument resolution

For `ry run <name> [args...]`, the first positional argument is resolved in this order:

1. **`<name>` is `--`** — runs the project entry point (`package.toml` `entry`) and passes the remaining arguments to `args()`. `ry run` with no arguments instead lists the available `[scripts]`.
2. **`<name>` ends with `.ry`** — treated as a Ry file. If `<name>` contains a directory component or already exists, runs it directly; otherwise resolves it as a bare filename through `[paths]` (see "Bare filename resolution" below).
3. **`<name>` matches a `[scripts]` key** — runs the script via the system shell (`std::system()`). The exit code is propagated.
4. **`<name>` has no `.ry` extension and no script match** — falls back to resolving `<name>.ry` through `[paths]`.
5. **Nothing matches** — exits with status 1, prints `Error: no such file: <name>.ry` with the searched paths, and lists the available scripts (if any) to help disambiguation.

Scripts always take precedence over bare-name file resolution when both exist for the same name. To bypass scripts and run a file unambiguously, append `.ry` (e.g. `ry run build.ry` runs `build.ry` even when `[scripts].build` is defined).

### Bare filename resolution

When the first argument to `ry run` (or `ry test`) is a **single path component** whose name ends with `.ry` (for example `main.ry`) and no file with that name exists relative to the current directory, the nearest `package.toml` project is searched in order: **first** the project root directory (the directory containing `package.toml`), **then** each directory listed under `[paths]` (keys sorted alphabetically; keys starting with `_` are reserved and ignored, except `_dev_stdlib` which is handled separately). The **first** existing regular file wins (for example `foo.ry` next to `package.toml` is chosen over `src/foo.ry` when both exist). If none match, the file is reported as missing and the searched paths are listed. Tokens without a `.ry` suffix are not resolved this way.

If the argument is a **path with more than one component** (for example `src/foo.ry` or `./foo.ry`) and that path does not exist, `ry run` reports **no such file** instead of falling through to the script-name path.

### Behavior

1. Searches for `package.toml` from the current directory upward. Fails with status 1 if none is found.
2. Without arguments, lists all available scripts and their commands.
3. With a script-name match, runs the shell command and propagates the exit code.
4. With a file match, JIT-compiles and runs the Ry source. Positional arguments after the file (or after `--`) are available to the script via `args()`.

### Notes

- Listing scripts (`ry run` with no arguments) and running a script do not require LLVM initialization (fast startup).
- Running a Ry source file initializes LLVM lazily on demand.
- Commands are executed in the current working directory
- Shell features (pipes, redirects, etc.) are supported since commands run through the shell

---

## `ry fmt` - Code Formatter

Formats `.ry` source files with consistent 2-space indentation and canonical style.

```bash
ry fmt                     # Format all .ry files in the project (requires package.toml)
ry fmt src/main.ry         # Format a single file
ry fmt src/                # Format all .ry files in a directory (recursive)
ry fmt --check             # Check if files are formatted (exit 1 if not)
ry fmt --check src/        # Check specific directory
```

### Formatting Rules

- 2-space indentation per block level
- Spaces around binary operators (`a + b`, not `a+b`)
- Space after comma (`f(a, b)`, not `f(a,b)`)
- Blank line between top-level definitions (functions, records, enums)
- Comments are preserved

### Behavior

1. Reads the source file, parses it into an AST, and re-emits with canonical formatting
2. Writes the formatted result back to the file (in-place)
3. With `--check`, only reports unformatted files and exits with code 1 if any are found (useful for CI)
4. Skips `.git/`, `build/`, and `node_modules/` directories during recursive formatting

### Notes

- Does not require LLVM initialization (fast startup)
- Compound assignment operators (`+=`, `-=`, etc.) are represented in their desugared form (`x = x + expr`) after formatting, because the parser desugars them during parsing
- Hex (`0xFF`) and binary (`0b1010`) number literals are converted to decimal notation

---

## `ry test` - Run Tests

Discovers and runs test files (`*.test.ry`). See [Testing](testing.md) for full test syntax documentation.

```bash
ry test                        # Auto-discover and run all *.test.ry files
ry test tests/spec             # Run all tests under a directory
ry test test_file.ry           # Run a specific test file
ry test -p                     # Run tests in parallel (default workers = CPU count - 1, minimum 1)
ry test -p 8                   # Run tests in parallel with 8 workers
ry test -w                     # Watch mode: re-run on file change
ry test --coverage             # Collect line coverage information
```

### Options

| Option | Description |
|---|---|
| `-p [N]`, `--parallel [N]` | Run tests in parallel; optional positive integer N selects worker count (default: hardware concurrency - 1, minimum 1). `--parallel=N` is also accepted. |
| `-w`, `--watch` | Watch for changes and re-run |
| `--coverage`, `--cov` | Collect coverage information |
| `--outline` | Print the describe/it structure without running tests |
| `--trace` | Emit structured internal trace as JSON Lines to stderr |
| `--trace-out=PATH` | Write trace output to `PATH` (use `-` for stderr) |
| `-h`, `--help` | Show help |

### Behavior

1. Without arguments, searches for `package.toml` to find the project root and recursively discovers `*.test.ry` files (skipping `.git`, `build`, `node_modules`)
2. Exit code is 0 if all tests passed, 1 if any failed
3. `--coverage` with `--parallel` falls back to sequential execution

---

## `ry self-update` - Self Update

Updates ry itself to the latest version. Downloads a binary from GitHub Releases and replaces the current executable.

```bash
ry self-update              # Update to the latest stable version
ry self-update v0.0.1       # Update to a specified version
```

### Behavior

1. Displays the current version
2. Resolves the target version based on arguments
   - No arguments: Latest stable release from GitHub (`/releases/latest`)
   - Version specified: Release with the specified tag
3. If the current version is the same, exits with `"Already up to date."`
4. Downloads the binary and replaces the current executable

### Downgrade Limits

`v0.0.29` and earlier shipped an `install_native_libs` filter that does not install `libemit` / `liblower` / `libLLVM` / `libzstd`. After downgrading to those versions, a subsequent forward `ry self-update` would not lay down the libs the new binary needs and the binary would fail to start. To prevent that lock-out:

- `ry self-update v0.0.29` (or any earlier version) exits with an error and leaves the existing binary untouched.
- `ry self-update v0.0.30` and later proceed normally.
- If a downgrade to a pre-v0.0.30 release is unavoidable, set `RY_ALLOW_LEGACY_DOWNGRADE=1` to opt in. A warning is printed and the update proceeds; use `ry-rescue` to recover if the binary stops starting after a later forward update.

### Security

Release archives are verified in two steps:

1. **Authenticity**: The `checksums.txt.sig` file is verified against the embedded Ed25519 public key.
   - If the signature file is **missing**, the update is aborted unless `RY_SKIP_SIGNATURE=1` is set.
   - If the signature file is **present but invalid**, the update is aborted regardless of `RY_SKIP_SIGNATURE`.
2. **Integrity**: The archive's SHA-256 hash is compared against `checksums.txt`.

To bypass the failure caused by a missing signature file (not recommended), set `RY_SKIP_SIGNATURE=1`. This does **not** bypass verification when a signature file is present but invalid.

### Notes

- Requires `curl` and `tar` commands
- If replacing the binary fails due to insufficient permissions, a message suggesting `sudo` is displayed (sudo is not invoked automatically)
- Downloads are performed to a temporary directory first; however, if the cross-filesystem `cp` fallback is interrupted, the destination binary may be left in a partial state

---

## `ry docs` - Static HTML API Documentation Generator

Generates static HTML and a machine-readable JSON manifest from `@doc` directives attached to declarations under `[paths].src`. The output is written to `docs/api/` by default and can be served from any static host (including GitHub Pages publishing from `/docs`).

```bash
ry docs                          # Generate HTML + docs.json into docs/api/
ry docs --out web/api            # Write to a custom directory
ry docs --include-private        # Include declarations without @public
ry docs --clean                  # Remove files listed in .ry-docs-manifest
```

### Options

| Option | Description |
|---|---|
| `--out <path>` | Output directory (default: `docs/api`). Relative paths are resolved against the project root. |
| `--format <fmt>` | Output format. Only `html` is accepted in this version; any other value exits non-zero. |
| `--emit-json` | Accepted for forward compatibility. `docs.json` is always written alongside the HTML. |
| `--include-private` | Include declarations that do not carry `@public`. By default only `@public` declarations are documented. |
| `--clean` | Delete files listed in `.ry-docs-manifest` under the output directory. Refuses to run when the manifest is missing. |

### Output Layout

```text
docs/api/
├── index.html             # module index page
├── modules/
│   └── <module>.html      # one page per source file
├── docs.json              # machine-readable manifest
└── .ry-docs-manifest      # generated-file list for safe regeneration / --clean
```

Module names are derived from the source file path relative to `[paths].src`, with `/` replaced by `.` (for example `src/foo/bar.ry` becomes module `foo.bar`).

### `docs.json`

```json
{
  "ry_version": "0.0.30",
  "modules": [
    {
      "name": "main",
      "path": "src/main.ry",
      "symbols": [
        {
          "kind": "fn",
          "name": "greet",
          "signature": "fn greet() -> str",
          "doc": "Greets the world.",
          "is_public": true,
          "source": "src/main.ry:6"
        }
      ]
    }
  ]
}
```

`kind` is one of `fn`, `record`, `field`, `enum`, `typeAlias`, or `const`. `path` and `source` are project-relative. Record fields are emitted alongside their parent record with the qualified name `<record>.<field>`. Enum variants are not currently emitted because the `@doc` directive cannot be attached to them.

### Safety

`ry docs` never overwrites hand-written files. Before writing each output file, the generator checks whether the destination already exists and is recorded in the on-disk `.ry-docs-manifest`. Untracked files trigger a refusal:

```text
Error: refusing to overwrite untracked file '<path>' — was it hand-written? Use --out to pick a different directory.
```

`--clean` mirrors this guarantee in the opposite direction: it removes only the entries listed in the manifest (and empty parent directories left behind), so hand-edited files placed next to generated output are preserved. Running `--clean` without a manifest is an error.

Regenerating after a source file is removed also purges the old per-module page: any tracked file not in the new generation plan is deleted.

### Determinism

Output is deterministic across runs given the same source tree:

- Modules are listed in alphabetical order.
- Symbols within a module are sorted by `(line number, name)`.
- All paths in `docs.json` and HTML are project-relative.
- No timestamps are written into any file.

`ry docs --out a && ry docs --out b && diff -r a b` produces no output for a stable source tree.

### Markdown in `@doc`

`@doc` Markdown payloads are emitted as escaped text in the current version. Block-string payloads (`"""..."""`) preserve line breaks via `<pre>`, single-line payloads (`"..."`) render inside `<p>`. The compiler does not interpret Markdown beyond HTML escaping; richer rendering may be added in a later version without changing CLI behavior.

---

## `package.toml` Configuration File

Describes project metadata and path settings in TOML format.

```toml
[project]
name = "my-project"
version = "0.1.0"
entry = "src/main.ry"

[paths]
src = "src"

[scripts]
build = "cmake --preset default && cmake --build build"
test = "./build/ry_tests"
clean = "rm -rf build"
```

> **macOS**: the `[scripts]` example uses the Linux/CI build form. On macOS, substitute `cmake --preset rust-emit` for `cmake --preset default` and `build-rust/` for `build/` (so `test = "./build-rust/ry_tests"`) — post-Rust-cutover preset split (`AGENTS.md` § "Build & Test").

### `[project]` Section

| Key | Description |
|------|------|
| `name` | Project name (directory name at initialization) |
| `version` | Version string |
| `entry` | Source file serving as the entry point |

### `[paths]` Section

| Key | Description |
|------|------|
| `src` | Source code directory |
| (other keys) | Additional project-relative directories. Values must not be absolute and must not contain `..`. Together with `src`, these directories are used to resolve **bare filenames** for `ry run <file>` and `ry test <file>` (see **Bare filename resolution** above). |
| `_dev_stdlib` | Optional; development override for the standard library location (see tooling docs). Not used for bare-filename resolution. |

### `[scripts]` Section

Defines named scripts that can be executed with `ry run <name>`. Each key is a script name and the value is a shell command string.

| Key | Description |
|------|------|
| `<name>` | Shell command to execute (run with `ry run <name>`) |

### TOML Subset Specification

`package.toml` supports the following TOML subset.

- Section headers: `[section]`
- Key-value pairs: `key = "value"` (string values only)
- Comments: From `#` to end of line
- Blank lines are ignored
