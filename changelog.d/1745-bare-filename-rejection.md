### Changed

- `ry foo.ry` (a bare filename with no path separator) is now rejected with
  an actionable error when `foo.ry` exists in the current working directory:
  `Error: ambiguous script path 'foo.ry'. Use './foo.ry' or an absolute
  path.` Previously the bare form silently bypassed referrer-directory
  resolution, causing scripts that used relative imports
  (`from .sub import ...`) to fail with `relative import requires a
  referrer directory`. Use `./foo.ry` or an absolute path instead; bare
  filenames that do not exist in the current directory are still resolved
  through `package.toml` `[paths]` as before. (#1745)
