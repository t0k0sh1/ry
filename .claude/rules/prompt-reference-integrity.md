# Prompt / Instruction Reference Integrity

- Keep inline-code repo paths in `.claude/**/*.md`, `AGENTS.md`, and `CLAUDE.md` resolvable.
- Keep inline-code slash commands backed by `.claude/skills/<name>/SKILL.md` unless registered as built-ins in `scripts/check-prompt-refs.sh`.
- After editing prompt files, run `.claude/skills/pre-commit-checklist/run-prompt-refs-lint.sh`.
- Ensure each skill's `allowed-tools` covers commands prescribed by its body.
- After editing agent frontmatter, validate YAML separately; prompt-reference lint does not validate YAML syntax.
- Fenced code blocks and plain prose are not scanned by the prompt-reference lint; use them for intentional historical or placeholder references.
