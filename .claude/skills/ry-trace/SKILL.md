---
name: ry-trace
description: Analyze Ry internal behavior using --trace and --trace-out. Use when investigating 内部挙動 / trace / --trace / --trace-out / JSON Lines trace output / import 解決 / JIT 実行 / compiler pipeline flow / function call tracing / branch selection.
allowed-tools: Bash
---

# Ry Trace

Use `--trace` to observe Ry's internal execution pipeline. Trace output is a JSON Lines stream (machine-readable), not a human log.

> **Source of truth**: AGENTS.md §"内部挙動の解析に trace を使う" was migrated to this skill in #1498.

## Rules

- Prefer `./build/ry --trace` for investigating internal behavior, compilation flow, import resolution, JIT execution, function calls, or branch selection.
- Treat trace output as a JSON Lines machine-readable stream, not a human log.
- To see both program stdout and trace separately, use `--trace-out=<path>` to redirect trace to a file.
- For test analysis: `./build/ry test --trace ...`.
- Use trace selectively — only when behavior is unclear or evidence is needed. Output is verbose.
- When trace-derived facts inform a Plan or investigation summary, label them explicitly as "trace で確認した事実".

## Example Commands

```bash
./build/ry --trace app/main.ry
./build/ry --trace-out=/tmp/ry-trace.jsonl app/main.ry
./build/ry test --trace tests/spec
echo 'print(1)' | ./build/ry --trace -c
```
