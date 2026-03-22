# Repository Guidelines

## Project Structure

Core implementation lives in `src/`, public headers in `include/ry/`, and the standard library in `lib/std/`. C++ unit tests are under `tests/`, while Ry language self-tests live in `tests/spec/`. User-facing documentation is in `docs/`, with generated PDFs committed alongside the source docs.

## Build and Test

Use the LLVM-based CMake build already documented in this repo:

```bash
cmake -B build -DLLVM_DIR=/usr/local/llvm/lib/cmake/llvm
cmake --build build
./build/ry_tests
./build/ry test
./build/ry test tests/spec/<file>.test.ry
```

Before finishing a task, run the full verification flow: rebuild, run `./build/ry_tests`, then run `./build/ry test`.

## Development Workflow

Start from the target issue or task description, then make an explicit implementation plan before editing code. Follow TDD where practical:

- Existing behavior changes: ensure a test exposes the gap first, then update code and tests together.
- New features: add the new failing test first, implement, then refactor.

Keep scope limited to implementation, tests, self-verification, and required docs updates. Do not perform `git add`, `git commit`, `git push`, or PR creation unless the user explicitly asks.

Repo-local Codex skills live under `.codex/skills/`. In this repository, use them for branch creation and branch / PR maintenance workflows, not for autonomous commit or push operations.

## Branch and Change Rules

Do not commit directly on `main` or release branches matching `v*.*.*`; create a feature branch first. Recent history uses concise Conventional Commit-style subjects such as `fix: ...`, `docs: ...`, and `chore: ...`; follow that style for proposed commit messages.

If you discover out-of-scope problems during the task, report them separately instead of folding them into the current change.

## Documentation Checks

If behavior, syntax, built-ins, or user-visible workflows change, review `docs/reference/`, `docs/tutorial/`, `docs/README.md`, and `README.md` for needed updates. If no docs change is needed, state why.

If you modify files under `docs/`, regenerate PDFs with:

```bash
./docs/generate-pdf.sh
```

Treat PDF generation warnings, including missing glyph warnings, as issues to resolve before considering the task complete.
