---
name: ry-playground
description: Try Ry language code snippets for ad-hoc behavior verification. Uses heredoc with ./build/ry -c. No file creation allowed. Not for self-tests.
metadata:
  short-description: Try Ry code snippets in a sandbox
---

# Ry Playground

Execute Ry language snippets via heredoc to verify behavior. This is for ad-hoc verification only, not for self-tests (`ry test`).

## Absolute Rules

1. **No file creation** — never create `.ry` files or temp files anywhere (project or external). No `Write` tool, no `echo > file`, no `cat > file`, no `tee`, no redirection to disk
2. **Heredoc only** — all Ry code MUST be passed via heredoc to `./build/ry -c`
3. **Project-local execution** — always run `./build/ry` from the project root. Never use a system-installed `ry`
4. **No `~/.ry` reference** — any operation that touches `~/.ry/` stdlib is incorrect. `./build/ry` resolves to the project-local `share/std/` automatically via `package.toml`
5. **Not for self-tests** — do not use this as a substitute for `./build/ry test`

## Execution Pattern

Always use single-quoted delimiter `'EOF'` to prevent shell variable expansion:

```bash
./build/ry -c <<'EOF'
<ry code>
EOF
```

With trace (for internal behavior analysis):

```bash
./build/ry --trace -c <<'EOF'
<ry code>
EOF
```

## Steps

1. Identify the Ry code to verify from the user's request
2. Compose appropriate Ry code snippets if needed
3. Run via Bash with `./build/ry -c <<'EOF' ... EOF`
4. Report the output
5. If an error occurs, analyze and explain the cause
6. For multiple independent snippets, run Bash calls in parallel
7. If the build may be stale, run `cmake --build build` first
