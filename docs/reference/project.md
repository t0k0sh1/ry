[English](project.md) | [日本語](../ja/reference/project.md) | [繁體中文](../zh/reference/project.md)

# Project Management

## `ry init` - Project Initialization

Initializes the current directory as a Ry project.

```bash
ry init
```

### Generated Files and Directories

```
my-project/
  ry.toml          # Project configuration file
  src/
    main.ry        # Entry point (sample code)
```

### Behavior

1. Exits with an error if `ry.toml` already exists
2. Creates the `src/` directory (if it doesn't exist)
3. Generates `ry.toml` (`name` is set to the current directory name)
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
  ry.toml          # Project configuration file
  src/
    main.ry        # Entry point (sample code)
```

### Behavior

1. Exits with an error if no project name is given
2. Exits with an error if the directory already exists
3. Creates the `<project-name>/` directory
4. Creates the `src/` directory inside it
5. Generates `ry.toml` (`name` is set to the given project name)
6. Generates `src/main.ry`

---

## `ry fmt` - Code Formatter

Formats `.ry` source files with consistent 2-space indentation and canonical style.

```bash
ry fmt                     # Format all .ry files in the project (requires ry.toml)
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

## `ry self-update` - Self Update

Updates ry itself to the latest version. Downloads a binary from GitHub Releases and replaces the current executable.

```bash
ry self-update              # Update to the latest stable version
ry self-update --nightly    # Update to the latest nightly pre-release
ry self-update v0.0.1       # Update to a specified version
```

### Behavior

1. Displays the current version
2. Resolves the target version based on arguments
   - No arguments: Latest stable release from GitHub (`/releases/latest`)
   - `--nightly`: Latest pre-release
   - Version specified: Release with the specified tag
3. If the current version is the same, exits with `"Already up to date."`
4. Downloads the binary and replaces the current executable

### Notes

- Requires `curl` and `tar` commands
- If replacing the binary fails due to insufficient permissions, a message suggesting `sudo` is displayed (sudo is not invoked automatically)
- Downloads are performed to a temporary directory first; however, if the cross-filesystem `cp` fallback is interrupted, the destination binary may be left in a partial state

---

## `ry.toml` Configuration File

Describes project metadata and path settings in TOML format.

```toml
[project]
name = "my-project"
version = "0.1.0"
entry = "src/main.ry"

[paths]
src = "src"
```

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

### TOML Subset Specification

`ry.toml` supports the following TOML subset.

- Section headers: `[section]`
- Key-value pairs: `key = "value"` (string values only)
- Comments: From `#` to end of line
- Blank lines are ignored
