### Fixed

- `fuzz_io_open` libFuzzer harness は `docker/entrypoint.sh` の case dispatch に登録されておらず、`./docker/run.sh fuzz fuzz_io_open ...` が `error: unknown command 'fuzz_io_open'` で exit 1 していた。CMake target (`add_ry_fuzz_target(fuzz_io_open ...)`) と corpus (`tests/fuzz/corpus/fuzz_io_open/`) は既存だったため Docker 経由の実行手段だけが欠落していた状態。entrypoint.sh の case パターンと error message、`docker/run.sh` の usage コメント、`docker/README.md` / `.claude/skills/linux-docker-dev/SKILL.md` の libFuzzer quickstart 例コマンドを 4 harness 対応に揃えた。(#1976)

### Changed

- `/pre-commit-checklist` §3.6 を 4 harness 対応に拡張。skip-detection grep が `src/runtime/native/io.cpp` / `include/ry/runtime/native/io.hpp` を含むようになり、`io.cpp` を変更する PR は §3.6 を自動的に検証対象に含む。`run-fuzz.sh` は 4 target (`fuzz_parser` / `fuzz_json` / `fuzz_utf8` / `fuzz_io_open`) を 60 s ずつ実行する (合計 ~4 分、従来は ~3 分)。Change-type matrix の row label と Fuzzer mapping、`.claude/agents/fuzzer-runner.md` の TARGETS list / REPORT FORMAT 例も併せて更新した。(#1976)
- `run-fuzz.sh` および §3.6 の wording で libFuzzer の `-rss_limit_mb` を 512 MB から 2048 MB に引き上げ。実測ピーク RSS は `fuzz_parser` 514 MB / `fuzz_json` 597 MB / `fuzz_utf8` 429 MB / `fuzz_io_open` 536 MB と、いずれの harness も libFuzzer の coverage tracking overhead (~275k inline 8-bit counters + PC table) で 400-600 MB に達する。512 MB cap では `fuzz_parser` で startup OOM を引き起こし、`fuzz_json` / `fuzz_io_open` も borderline だった (parser 固有のバグではなく、全 harness 共通の corpus + coverage 構造的 overhead)。2048 MB に引き上げて 4 harness 全てが安定して完走する。(#1976)
