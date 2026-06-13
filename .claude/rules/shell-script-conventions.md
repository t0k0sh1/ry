---
paths:
  - "scripts/**/*.sh"
  - "docker/*.sh"
  - ".claude/skills/**/run-*.sh"
---

# Shell Script Conventions

- Under `set -u`, expand optional arrays as `"${arr[@]+"${arr[@]}"}"`.
- Under `set -e`, put fallible command-substitution assignments inside an `if` condition.
- Under `pipefail`, avoid `printf ... | grep -q`; use a here-string or `[[ =~ ]]` so early `grep` exit cannot turn the upstream writer's SIGPIPE into failure.
- Do not pipe build output through `tail` unless `pipefail` is active and the build command's status is preserved.
