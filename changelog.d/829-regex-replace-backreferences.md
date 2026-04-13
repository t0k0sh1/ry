### Added

- `regex.replace` and `regex_replace` now support capture group backreferences in the replacement string: `$1`–`$9` expand to the corresponding captured groups, `$0` expands to the entire match, `$$` produces a literal `$`, and `${N}` handles multi-digit group indices (#829)
