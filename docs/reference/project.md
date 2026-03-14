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
  test/            # Test code directory
```

### Behavior

1. Exits with an error if `ry.toml` already exists
2. Creates the `src/` directory (if it doesn't exist)
3. Creates the `test/` directory (if it doesn't exist)
4. Generates `ry.toml` (`name` is set to the current directory name)
5. Generates `src/main.ry` (skipped if it already exists)

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
test = "test"
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
| `test` | Test code directory |

### TOML Subset Specification

`ry.toml` supports the following TOML subset.

- Section headers: `[section]`
- Key-value pairs: `key = "value"` (string values only)
- Comments: From `#` to end of line
- Blank lines are ignored
