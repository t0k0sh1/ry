# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.1.0/).

## [Unreleased]

## [0.0.33] - 2026-06-27

### Fixed

- Repair installs that upgraded from a pre-companion updater without installing `ry-self-update`, and refresh `ry-self-update` from `ry-rescue` (#2459).

## [0.0.32] - 2026-06-27

### Added

- `ry-self-update` standalone native updater binary. `ry self-update` now acts as a healthy-install forwarder that `execv`s the sibling updater with inherited stdin / stdout / stderr, while broken main-binary installs can run `ry-self-update` directly without loading `libLLVM`, `libemit`, `liblower`, `libry_*`, or `libzstd`. Release packaging, `install.sh`, `scripts/verify-bundle.sh`, rollback snapshots, smoke tests, and docs now install and verify `ry-self-update` next to `ry` and `ry-rescue`; `ry-self-update` also refreshes itself during updates using a sibling temp file plus `rename()` so Linux does not hit `ETXTBSY` when replacing the running updater. (#2459)


## [0.0.31] - 2026-06-27

### Added

- `ry-rescue` standalone POSIX shell 復旧スクリプトを release tarball / `install.sh` / `ry self-update` で配布。`#2005` の cutover 以降 `ry` は shared libLLVM をリンクしているが、pre-`#2005` 版の `install_native_libs` filter は `libLLVM` / `libemit` / `liblower` / `libzstd` を install しないため、ダウングレード後の forward update では `~/.ry/lib/libLLVM.{dylib,so.*}` 不在で `ry` 自体が dyld error で起動不能になり `ry self-update` 経由の自己復旧ができなかった。`scripts/rescue.sh` は libLLVM をリンクせず curl / tar / shasum + POSIX shell built-ins のみで完結し、latest stable release を fetch して再 install する。SHA-256 checksum verification は mandatory、Ed25519 signature verification は capability-probed OpenSSL 3.x (macOS LibreSSL 3.3.x は明示的に skip + warning) が見つかった時のみ有効化する。Orphan cleanup として既存 `~/.ry/lib/` の `libLLVM*` / `libemit*` / `liblower*` / `libry_*` / `libzstd*` glob のみを除去し `lib/std` や non-glob のユーザーファイルには触れない。`scripts/bundle-dist.sh` が `dist/ry-rescue` (mode 0755) を生成、`scripts/verify-bundle.sh` が存在 + executable + `sh -n` を検証、`install.sh` は `$INSTALL_DIR/ry-rescue` に install、`ry self-update` の `install_rescue_script` が新バイナリ隣接の `ry-rescue` を更新する。`rescue.sh` の `RY_RELEASE_PUBKEY_HEX` と `src/cli/self_update.cpp` の `SIGNING_PUBLIC_KEY` は同一鍵を持つ運用が `.claude/rules/distribution-packaging.md` で固定済み。 (#2455)
- `ry self-update` に backup / smoke-test / 自動 rollback の atomic update を追加。アップデート前に `~/.ry/.backup/` へ旧 binary + stdlib tree + bundled native libs (libemit / liblower / libry_* / libLLVM / libzstd) + 隣接 `ry-rescue` を snapshot し、全 install 完了後に新 binary を `--version` で smoke test する。child を timeout 30s で WNOHANG + drain pipe で wait し、exit ≠ 0 / signal / timeout / stdout に `ry ` banner 不在のいずれかなら rollback path に入って backup から復元、復元後の binary を再 smoke test して通れば旧バージョンに戻ったことを報告して exit 1、再 smoke test も失敗するなら半壊状態を明示して `ry-rescue` (#2455) を案内して exit 1。並行実行ガードとして `~/.ry/.update.lock` を `O_CREAT|O_EXCL` で取得し、live holder には `BusyOtherProcess` で refuse、stale PID (`kill(pid,0) == ESRCH`) は reclaim する。lock は早期 return すべてを覆う RAII guard で解放。smoke test 通過時と rollback 自体が成功して再 smoke test も通った時のみ backup を cleanup し、restore が失敗するか rollback 後の再 smoke test も失敗した場合は backup を `~/.ry/.backup/` に残して inspection に利用できるようにする。 (#2456)

### Changed

- `ry self-update v0.0.29` (および v0.0.29 以下のすべてのバージョン) を refuse するようガードを追加。これらのバージョンの `install_native_libs` filter は `libemit` / `liblower` / `libLLVM` / `libzstd` を install しないため、ダウングレード後の forward `ry self-update` で新 binary が必要とする lib が install されず、binary が起動不能になる (libLLVM 不在による dyld error 報告と同型の症状)。`resolve_update_target` で `requested_tag` の major/minor/patch を semver parser (先頭 `v` / leading zero / pre-release suffix / 4-segment suffix を許容、`-` / `+` を先頭 digit guard で reject) に通し、`<= v0.0.29` ならエラー終了する。緊急回避用に `RY_ALLOW_LEGACY_DOWNGRADE=1` (sticker-shock warning 付きで通る) と `RY_UPDATE_REPO != t0k0sh1/ry` (fork CI 用に guard skip) の escape hatch を用意。`v0.0.30` 以降は従来通り、`ry self-update` 引数なしも常に最新 stable を引くので影響なし。 (#2457)

## [0.0.30] - 2026-06-26

### Added

- `getPath(map, path)` and `setPath(map, path, value)` for jq-style nested access into `Map<str, any>` values (typically loaded via `json.load[Map<str, any>]`). Path is a dot-separated string literal. For `getPath`, numeric segments resolve to `List<any>` int indexes or `Map<str, any>` str keys via runtime tag dispatch, and missing intermediate segments return `None`. For `setPath`, every intermediate segment must resolve to a `Map<str, any>` — non-`Map` intermediates or missing intermediates raise a runtime error; the leaf is inserted when absent and updated when present. Dot syntax sugar — `cfg.server.host` on `Map<str, any>` — desugars to chained `Option<any>` lookups, returning `None` on the first miss; combine with the trailing `?` operator (`let host: str = cfg.server.host?`) to unwrap or propagate `None` out of the enclosing fn. Top-level `setPath` receivers are COW-checked; intermediate `Map`s mutate in place. (#1701)
- `scripts/export-run-logs.sh` wraps `ry test`, `ry test --trace`, and `ry_tests` invocations and emits one JSON Lines record per command into `.ry-eval/runs/<run-id>/run.jsonl` alongside raw stdout / stderr / trace / gtest-json artifacts under `artifacts/`. Each `command` record carries the full argv, exit code, wall-clock duration, RFC3339 timestamps, repo-relative paths to the artifact files, an `stdout_byte_count`, and (for `ry test`) a parsed `{passed, failed, skipped, todo}` summary; a single `run_meta` record at the top of each run captures `host_os` (`uname -s` only), `git_sha` / `git_branch` / `git_dirty`, `ry_build_dir`, and `ry --version`. The script auto-detects the build directory (`build-rust/` then `build/`, overridable via `RY_BUILD_DIR`), expands directory targets per-file so `--trace` and per-file exit codes stay clean, and reports a captured-vs-attempted summary so an individual command's non-zero exit is recorded as *data* rather than aborting the script. The schema and privacy expectations — JSONL metadata is scrubbed of hostname / username / absolute paths, but the raw artifact files are passed through verbatim and may still contain locally-identifying content — are documented in `docs/architecture/jsonl-run-logs.md`. Output root is git-ignored under `.ry-eval/`. (#1731)
- Reserved `ry` namespace and explicit `ry.lang` prelude module. The official standard library is now addressable under the canonical `ry.*` path: `ry.lang` exposes the implicit prelude symbols (`map`, `filter`, `sum`, `int`, `float`, `str`, `len`, `range`, `print`, ...), and each submodule is reachable as `ry.<module>` (`ry.math`, `ry.io`, `ry.path`, `ry.filesystem`, `ry.json`, `ry.http`, `ry.thread`, `ry.regex`, `ry.testing`, `ry.base64`). The compiler injects `from ry.lang` as the implicit prelude, replacing the implicit `from std` injection while preserving identical loaded symbols. Both `from ry.<mod> import x` and `import ry.<mod>` are supported; the qualified form binds the bare last segment (`import ry.math` exposes `math.sqrt(...)`), with `as <alias>` for collision avoidance. The legacy `from std import` / `from std.<mod> import` / flat `from <mod> import` spellings continue to work as compatibility aliases — they resolve through the same physical stdlib under `share/std/`. A user-defined top-level `ry/` directory or `ry.ry` file is ignored (the loader resolves `ry.*` only against bundled stdlib search paths) and triggers a one-time stderr warning advising the user to rename it; a future release will promote this warning to a hard error. (#1769)
- `@doc("...")` built-in directive for attaching Markdown documentation strings to declarations. Accepts a single string argument — single-line (`"..."`) or triple-quoted block string (`"""..."""`) — and applies to `fn` / `async fn`, `record`, record fields, `enum`, `type` aliases, and `@directive fn` declarations. The compiler does not parse Markdown; `@doc` preserves the body as metadata for future tooling. Empty strings (`@doc("")`) are accepted; `@doc` is rejected on `for` loops, function-call statements, and enum variants. Tree-sitter highlights `@doc` payloads with the `@string.documentation` capture (falling back to `@string` when the editor lacks the predicate). (#1844)
- `ry docs` subcommand: a static HTML documentation generator that walks the `.ry` files under `[paths].src`, extracts `@public` declarations together with their `@doc` payloads, and writes a per-module HTML page set plus a machine-readable `docs.json` manifest. Default output is `docs/api/`; the source location, signature, visibility, and Markdown payload of each symbol are preserved with `data-kind` / `data-name` / `data-visibility` attributes so the pages remain useful to both humans and tooling. Output is deterministic across runs (modules and symbols sorted, no timestamps written). Supported flags: `--out <path>`, `--format html`, `--emit-json` (no-op; `docs.json` is always emitted), `--include-private`, and `--clean`. A `.ry-docs-manifest` file tracks generated outputs so the generator refuses to overwrite hand-written files and `--clean` removes only what it previously created. Regeneration after a source file is removed also purges the stale per-module page. (#1845)
- New `json5` stdlib module with the same 8-function surface as `json` (`load[T]` / `dump` / `stringify` / `stringifySafe`, each with two arities) that accepts the [JSON5 spec](https://json5.org) extensions on input: line and block comments, trailing commas, single-quoted strings, multi-line strings via `\<LF>` line continuation, unquoted ASCII identifier object keys, hex integer literals (`0xFF`), leading and trailing decimal points (`.5`, `5.`), `Infinity` / `-Infinity` / `NaN` literals, and explicit positive sign (`+5`). `json5.stringify` emits strict-JSON-compatible output (lossless round-trip with `json.load`) except for non-finite floats, which emit JSON5 bare tokens (`"NaN"`, `"Infinity"`, `"-Infinity"`). `json5.stringifySafe(NaN)` returns `Ok("NaN")` instead of `Err`. `json` (RFC 8259 strict) is unchanged. (#1855)
- `fuzz_json5` libFuzzer harness (`tests/fuzz/fuzz_json5.cpp`, seed corpus under `tests/fuzz/corpus/json5/`) mirroring `fuzz_json`. The target compiles under the `fuzz` preset but the docker entrypoint allowlist update (`docker/entrypoint.sh`, `docker/run.sh`) only takes effect after the next `/ci-image-workflow` rebuild; until then `run-fuzz.sh` still runs the existing 4 targets and pre-commit-checklist treats `fuzz_json5` as a deferred check. (#1855)
- `NativeCallDescriptor` C++ struct (`include/ry/native_call_descriptor.hpp`) を新規導入し、`docs/architecture/native-call-boundary.md` follow-up #1 (#2231 子計画、tracking #2299) の foundation を実装。v1 では `library_name` のみを保持し、後続の consumer PR が必要に応じて field を追加する YAGNI 方針 (`docs/architecture/native-call-boundary.md` §"NativeCallDescriptor" の他フィールドは consumer 移行 PR で初出時に追加)。
- `inferLibraryName(directiveTag, declaringModule)` pure function (`src/native_call_descriptor.cpp`) で library inference rule (a)/(b) を実装。rule (a) は `@native("<lib>")` の明示タグを優先、rule (b) は bare `@native` でも declaring module が `knownNativeLibs()` (CMakeLists.txt:386 `RY_NATIVE_LIBS` の C++ side mirror、12 entries) に含まれていれば自動推論する。Pattern B carve-out (`int(s)` / `float(s)` / `input()` / `close()`) は依然として hand-written なので rule (b) の attach は inert。
- `CodeGen::native_call_descriptors_` storage + `getNativeCallDescriptors()` getter を追加 (`include/ry/codegen.hpp`)。`@native` 宣言処理 (`src/codegen_fn.cpp:640+`) で `NativeFnSignature` と同じ dedup ルールで descriptor を構築・保存する。key 形式は `native_fn_sigs_` と同一 (`ry::util::nativeSigKey(pkg, name)`)、`fn_name`/`module_name` は key suffix/prefix と `native_fn_sigs_` lookup で復元可能なので descriptor 側には持たない。
- `tests/test_native_call_descriptor.cpp` 新規追加。pure-function inference (5 ケース) + end-to-end descriptor storage (rule (a) のみ、1 ケース) + `knownNativeLibs()` の local literal 整合 guard (1 ケース、CMake との cross-file 整合は hand-maintained の制限を test 内コメントで明記)。rule (b) end-to-end は `runSource` harness が `SourceLocation::file_id` を `share/std/<M>/<M>.ry` に偽造できないため本 PR では skip し、後続 consumer PR の spec test で間接 verify する方針。
- foundation のみ。任意の `dispatchXXX` (io/net/http/thread/path/json/json5/base64) は無変更で、`emitTableDrivenNativeCall` / `emitGenericNativeCall` 経路も書き換えていない。
- 本 PR の foundation 着地と先行 PR #2306 / #2332 の組み合わせで #2299 を close する。後続の per-module descriptor 移行は本 issue の範囲外として個別 issue で追跡する。 (#2299)
- `tests/scripts/test-export-run-logs.sh` — `scripts/export-run-logs.sh` の JSONL スキーマ契約 (`docs/architecture/jsonl-run-logs.md`) を 1 ケース fixture で検証する pure shell + `jq` ハーネス。`run_meta` / `command` の両レコードに `schema_version: "1"`、必須キー、`summary` ↔ `exit_code` 整合を assert する。あわせて `scripts/export-run-logs.sh` の `schema_version: "1"` リテラル重複を `SCHEMA_VERSION` 変数 1 箇所に抽出して 2 ブロック間の drift を機械的に排除。`/pre-commit-checklist` から `.claude/skills/pre-commit-checklist/run-export-run-logs-tests.sh` 経由で実行可能。(#2300)
- `json5` の unquoted object key を ASCII subset から ECMAScript `IdentifierName` 全体に拡張した (`share/std/json5/json5.cpp`)。`unicode-ident` ベースの `XID_Start` / `XID_Continue` に加えて `$` / `_` / ZWNJ (`U+200C`) / ZWJ (`U+200D`) と `\uHHHH` escape (BMP / surrogate pair) を受理する。これにより JavaScript の `JSON5.parse`、Python の `pyjson5`、Rust `json5` crate との cross-language 相互運用ギャップが解消され、日本語 / 中国語 / 韓国語 / ギリシャ語 / アラビア・インド数字 / combining mark を含むキーがそのまま渡せるようになる。RFC 3629 strict UTF-8 codepoint decoder / encoder を json5.cpp 内に追加し、ASCII 連続部は escape を含まない区間として bulk copy するため ASCII-only キーの FFI 越境は発生しない。
- `crates/xid/` を新規 Rust cdylib として追加 (`unicode-ident` の薄いラッパ)。`__ry_xid_start(u32)` / `__ry_xid_continue(u32)` の 2 シンボルを export し、`emit` クレートと同じパターンで `ry` / `ry_tests` にリンクして dlopen 済み `libry_json5` からプロセス内で resolve させる (inter-library link dependency なし)。
- 拒否ケース (Unicode digit start / punctuation start / 不正な escape / `true` 等の予約語キーワードに Unicode サフィックスを連結したもの) は明示的なパースエラーとして返る。`parse_bool` / `parse_null` / `parse_number` の hex literal trailing guard も ECMAScript IdentifierContinue を意識した境界判定に揃えた。(#2314)
- `any` 型から具体型への明示的なチェック付きキャスト API を追加。`asType<T>(value: any) -> Result<T, Error>` は実行時の tag/descriptor 検査の結果 `Ok(value)` または `Err(Error{message: "asType[T]: ..."})` を返し、暗黙的 unwrap (`let x: T = anyVal`) のように `_Exit(1)` でプロセス終了しない。`isType<T>(value: any) -> bool` は実行時 tag テストヘルパ。両者とも primitive (`int` / `float` (Int 自動昇格) / `bool` / `str`)、record (descriptor walk によるサブタイプ判定)、`Option<T>`、`List<any>` / `Map<str, any>` / `Set<any>`、typed `List<T>` / `Map<str, V>` を受け付ける。`Result<T, E>` / 通常 enum / `Set<T>` (T ≠ any) / `Map<NonStr, _>` は "target not yet supported" Err を返す。実装は既存 `CodeGen::tryUnwrapFromAny()` を再利用し、record/Option については native-tagged source (any が record/enum を直接保持しているケース) に対して descriptor 検証後に panic 版 `unwrapFromAny` / `unwrapEnumFromAny` を呼ぶ 2-shape dispatch を `tryUnwrapFromAny` 内に追加。JSON-shape source (Map / Unit) は既存 helper にフォールバック。`tryUnwrapFromAny` 系統のエラーメッセージプレフィックスは `callerLabel` パラメータで設定可能 (デフォルト `"load"`、`asType` 経由では `"asType"`)。 (#2315)
- The strict-any rule framework that produces `[strict-any/<rule>]` diagnostics for unsafe `any` patterns, and the first rule `any-arithmetic`: direct binary `+`/`-`/`*`/`/`/`%`/`//`/`**` and unary `-` on an `any`-typed operand is rejected with a hint to annotate the operand type or use `asType[T](...)` to recover a concrete value first. The opt-in entry points (`--strict-any` CLI flag, `RY_STRICT_ANY` env var) were removed in the same release once #2322 promoted the rule set to the default. (#2319)
- `asType[T]` now recovers values that were wrapped from a native-typed source — `List<T>` / `Map<str, V>` / `Set<T>` (T/V ≠ any), `Result<T, E>`, simple enums, and ADT enums (including payload-carrying variants). Combined with the existing scalar, record, `Option<T>`, and JSON-shape coverage, every shape that the legacy implicit `any → T` unwrap previously handled now has a canonical `case asType[T](v)` recovery. Source / target mismatches (different `Result` parameters, unrelated enum types, a `List<int>` source requested as `List<str>`) return `Err` rather than mis-reading the payload. (#2378)
- `testing` モジュールに 3 つの bool-returning matcher intrinsic を追加: `calledWith(name, args...)` (記録された呼び出しのうち少なくとも 1 件が一致すれば `true`)、`calledTimes(name, n)` (呼び出し回数が `n` と等しければ `true`)、`lastCalledWith(name, args...)` (最後の呼び出しが一致すれば `true`)。既存の `verifyCalledWith` (int 返却) と同じコンパイル時バリデーション (関数が存在する / mock or spy 済みである / 引数の arity と型が一致する) と lifecycle (it-end auto-clear、`mockClear` / `mockReset` / `mockResetAll` で reset) を共有する。`lastCalledWith` のバックエンドとして `__ry_mock_last_call_matches` runtime helper を新規追加。引数記録 IR は `emitCalledMatcherImpl` ヘルパに共通化し、runtime call と結果変換のみが mode 別に分岐する。
- 新規 spec test `tests/spec/called_with.test.ry` を追加 (30 ケース、int / float / bool / str / List / Set / Map / record / tuple / fn / zero-arg + overload sig 形式 + `mockReset` / `mockResetAll` の相互作用をカバー)。`tests/spec/spy.test.ry` / `tests/spec/mock.test.ry` の既存挙動は behavior-preserving refactor のため退行なし。
- スコープ外として明示的にドロップした項目: `spy.calls` / `mock.calls` プロパティアクセサ。Ry には function value へのプロパティアクセス機構がなく、内部記録バッファは opaque な kind-tagged int でできているため、`List<tuple<paramTypes>>` として export するには overload ごとの typed deserializer が必要になる。引数レベルのアサーションは本 PR の 3 matcher で完全にカバーできるため、現時点では露出しない。(#2396)
- `Map.getPath` / `Map.setPath` now accept negative-index path segments on `List<any>` arms: `-1` resolves to the last element, `-2` to the next-last, down to `-len` (the first). Out-of-range negatives return `None` for `getPath` and raise a runtime error for `setPath`. Dotted-key escape is also supported via backslash: `"a\.b"` reaches the literal Map key `"a.b"`, and `"a\\b"` reaches `"a"`. `setPath` additionally walks through `List<any>` intermediates (with the same negative-wrap rules), enabling shapes such as `setPath(m, "users.-1.name", "Alice")`. Trailing backslashes and unrecognised escape sequences in the path are compile-time errors. Leaf writes into a List index remain out of scope: the final segment of the `setPath` path is still always a Map-key write. (#2398)
- Ry 文字列リテラルに `\u{HHHH}` Unicode escape を実装 (`src/lexer/lexer.cpp`)。1 〜 6 桁の hex を `{...}` で囲み、対応する Unicode scalar value を UTF-8 として decode する (例: `"\u{1F600}"` → 😀 = `😀`)。regular string (~672)、block string (~582)、f-string (~829) の 3 つの escape switch 全てから共通の `decodeUnicodeEscape` + `appendUtf8` ヘルパを呼ぶ実装で乖離を防止。バリデーションは `0x10FFFF` を上限、surrogate range `0xD800..0xDFFF`、`\u{}` 空、`\u41` (`{`欠落)、`\u{41` (`}`欠落)、`\u{ZZZ}` (非 hex)、`\u{1234567}` (7 桁以上) を全て構造化エラーに変換する。raw string (`r"..."`) は escape 非処理の既存契約を維持し `\u{...}` を literal の 10 byte として保持する。
- Ry 文字列リテラルに `\xNN` hex escape を実装 (`src/lexer/lexer.cpp`)。`\xNN` は厳密に 2 桁の hex digit を要求し、対応する単一バイト (0x00 〜 0xFF) を文字列に追加する (例: `"A"` → `"A"`, `"�"` → 単一バイト `0xFF`)。`\u{HHHH}` が UTF-8 encode 済みの Unicode code point を生成するのと異なり、`\xNN` は生のバイト 1 つを生成するため、`�` 〜 `�` 単独では valid UTF-8 にならない点も既存の `

### Changed

- Stacking the same directive twice on one declaration is now rejected — e.g. `@public @public fn ...` or `@deprecated @deprecated fn ...` raise `duplicate directive '@<name>' on the same declaration`. The rule applies uniformly to every builtin and user-defined directive. (#1844)
- `@native("base64")` calls now route through descriptor-driven generic dispatch instead of the hand-written `base64_table`. Behavior change for misdeclared natives: the `@native` declaration is the source of truth for argument types — declaring `@native("base64") fn encodeBytes(input: List<int>) -> str` no longer routes through the hidden `requireListU8Arg` table override and would silently mis-call the runtime; declare `List<u8>` to match the actual ABI. Stdlib registration adds an optional `snake_case_symbols=true` flag (via `RY_REGISTER_STDLIB_PACKAGE_NAMING`) for modules whose C symbols predate Ry's camelCase convention. (#2285)
- `share/std/manifest.json` から `version` フィールドを削除。コード上一切消費されない装飾メタデータで、`src/cli/self_update.cpp` の `install_stdlib` は読み込み直後に捨て、書き戻し時は呼出元から渡された `new_version` を上書きするのみだった。`StdlibManifest` 構造体と `write_manifest()` シグネチャを単純化し、`install_stdlib()` の `new_version` パラメータも併せて削除。`files` 配列は引き続き hand-maintained で、`/preparing-for-release` skill に追加した新 verify task が release ごとに on-disk stdlib との突合をゲートする。(#2296)
- `dispatchIO` (`src/codegen_call_io.cpp`) に sig-presence gate `isIoImported` を導入し、`cg.used_native_libraries_.insert("io")` を per-emit `markIo` lambda で wrap して dispatch 成功時のみ insert するように変更。`dispatchJson` / `dispatchJson5` (`.claude/rules/codegen-stdlib-dispatcher.md` #1855) と同じ思想だが、hybrid 形式 (prefix scan over `cg.getNativeFnSigs()` for `"io::"` keys + bare-name presence check for io の 14 fn 名) で実装した。Pass 1 (prefix scan) は canonical な `from io import ...` / `@native("io") fn ...` の user code をカバーし、Pass 2 (bare-name list) は C++ test harness パターン (`runSource` が ModuleLoader を skip するため inline test source が `@native fn writeText(...)` を bare で宣言し bare key `"writeText"` として登録) をカバーする。
- per-emit pattern (PR #2332 CodeRabbit review 対応) により、`from io import readText` 等で io を import しているが実 callsite は base64 のみ — のようなケースでも `required_libraries` が `{"base64"}` に保たれる。gate が broad に passing する dispatcher 設計と組み合わせて demand-driven loading を正確に維持する。
- 結果として、io を import しないプログラム (例: base64-only) で `libry_io.dylib` の余分な JIT load が発生しなくなり、`GetRequiredLibrariesOnlyIncludesCalledFunctions` (`tests/test_codegen_directive.cpp`) の expected set が transitional な `{"base64", "io"}` から `{"base64"}` に縮退。これは issue #2299 の close criterion 2 (`ASSERT_EQ(libs.size(), 1u)`) を満たす。
- 14 fn 名 list の drift behavior は **loud, not silent**: real user code は常に Pass 1 を通るため、Pass 2 の name list 漏れは C++ test の `no matching overload for @native fn '<name>'` で即座に検出される (json/json5 の enumeration とは異なり silent breakage は発生しない)。
- close criterion 1 (全 native dispatcher の descriptor 駆動化、custom-emitter fall-through library insertion の根絶) は v0.0.31+ で各 native module の Rust 化と並行して進める計画であり、本変更では tracking issue #2299 は open のまま継続する。 (#2299)
- `GetRequiredLibrariesOnlyIncludesCalledFunctions` (`tests/test_codegen_directive.cpp`) のアサーションを presence/absence チェック (`EXPECT_TRUE(libs.count("base64"))` + `EXPECT_FALSE(libs.count("path"))`) から、実測値ベースの bounded exact-set assertion (`EXPECT_EQ(libs, {"base64", "io"})`) に強化。PR #2285 の base64 descriptor 移行で sibling dispatcher が dispatcher loop に fall-through し、`dispatchIO` の unconditional top-insert が `"io"` を required-libraries 集合に押し込む transitional 状態を、固定された期待集合として記録する。新規 dispatcher の追加・既存 dispatcher の insertion 挙動変更が起きると EXPECT_EQ が fail するため、silent regression を防ぐ regression guard として機能する。issue #2299 の完了条件 (`ASSERT_EQ(libs.size(), 1u)`) は全モジュールの descriptor 移行完了時 (v0.0.31+) に達成する想定で、本変更ではトラッカー issue を open のままとする。 (#2299)
- `ry.*` 予約 namespace の公開 allowlist に `ry.net` と `ry.json5` を追加し、13 モジュールに拡張。`from ry.net import bind` / `import ry.net` / `from ry.json5 import load, stringify` / `import ry.json5` が canonical な書き方として解決するようになる。bare 形式 (`from net import …` / `from json5 import …`) は compatibility alias として引き続き動作。`ry.bogus` 等を reject する際の `available: …` リストも 13 件表示に追従する。 (#2309)
- **Breaking change**: strict-any semantics are now the compiler default. Code that previously emitted the deprecation warnings introduced by #2316 (direct arithmetic on `any`) and #2321 (implicit `any` → concrete unwrap on the four Path 9 sites) now fails to compile with the `[strict-any/<rule>]` diagnostic tag. Ordering comparisons (`<`, `<=`, `>`, `>=`) on `any` were promoted to the `any-arithmetic` rule alongside arithmetic operators (previously warned only). Equality (`==`, `!=`) and explicit `any` boundaries (`v: any = ...`, FFI returns, `from ry.json import load`) remain valid.
  Common fixes use `asType[T]` / `isType[T]` (#2315) with `case` narrowing:
  - Direct arithmetic / ordering on `any`: narrow each operand first.
    ```ry
    case asType[int](a):
        Ok(ai):
            case asType[int](b):
                Ok(bi): use(ai + bi)
                Err(_): ...
        Err(_): ...
    ```
  - Implicit unwrap (`n: int = anyVal` and the Path 9b–9d analogues): replace with `case asType[T](anyVal): Ok(n): ...`.
  - Unannotated function / lambda parameters: add an explicit type annotation, or `: any` if type-erasure is intentional. The Pattern 3 lint (#2317, #2323) still emits the lead-indicator warning; the function body's arithmetic / unwrap on the implicit-any parameter is what now refuses to compile.
  Some recovery shapes that previously worked via implicit unwrap initially lacked a canonical replacement on v0.0.30: reading back a *typed* collection from a native-sourced `any`, recovering a `Result<T, E>`, or recovering a simple / ADT enum value. #2378 closes that gap inside the same v0.0.30 milestone by extending `asType[T]` to those targets, so every shape rejected by `any-implicit-unwrap` now has a canonical `case asType[T](v)` recovery. `asType[Option<T>]` already worked at the time #2322 landed. See [Strict-any mode reference](docs/reference/strict-any.md) for the full migration cookbook. (#2322)
- Descriptor-driven dispatch pilot extended from base64 (#2285) to path. `NativeCallDescriptor` now carries `return_wrapping`, `out_param_type_name`, `error_channel`, and `require_list_u8_arg` populated at `@native` declaration time in `src/codegen_fn.cpp`, eliminating dispatch-time inference in `emitGenericNativeCall`. `dispatchPath` joined `dispatchBase64` as a bare `return nullptr;` stub; the legacy `path_table` and its `emitTableDrivenNativeCall` invocation are retired, with `path::join`'s arity-suffix runtime symbol convention (`__ry_path_join2/3/4`) handled at consume time via multi-overload detection on the matched sig key. IR is byte-identical for every Pattern C consumer (base64 / path / filesystem / gc) per the `.claude/rules/codegen-llvm-ir-conventions.md` #2026 byte-exact rule; verified with `--emit-llvm-ir` probes covering each dispatch shape (Direct / ResultPtr / ResultStatus / ResultOutParam / BoolFromI64 / require_list_u8_arg / arity-suffix). (#2337)
- `dispatchIO` (`src/codegen_call_io.cpp`) の `io_table[]` 10 entry を retire し、`@native("io")` 宣言経由の descriptor 駆動 dispatch (`emitGenericNativeCall`) に統合した。`RY_REGISTER_STDLIB_PACKAGE_NAMING` に `snake_case=true` flag を追加して、Ry の camelCase fn 名 (`readText` / `writeText` / 他) と runtime の snake_case symbol (`__ry_io_read_text` / `__ry_io_write_text` / 他) を pilot の base64 と同じ規約で連結する。File-coupled overload (`open` / `readAll(File)` / `readLine` variants / `writeText(File, s)` / `lines`) は依然として custom emitter で dispatch — 個別 issue (`exported_symbol` descriptor field / overload group id) で migration。
- `include/ry/native_call_descriptor.hpp` の `NativeCallDescriptor` に `resource_kind` field (int、default `ResourceKindRegistry::NONE`) を追加。`src/codegen_native_call_descriptor.cpp` に `inferResourceKind` pure function を追加し、`Result<T, Error>` 戻り型の T を `ResourceKindRegistry::lookupByTypeName` で逆引きする。`src/codegen_fn.cpp:703-743` の descriptor 構築 loop で populate、`src/codegen_call_native.cpp:704-710` の `ReturnWrapping::ResultPtr` case で `addResourceKind(result, desc.resource_kind)` を自動付与。`io::open` の descriptor が `rk_file` を carry する初の populate ケース (consume path は `dispatchIO` 内の `emitFileOpen` custom branch が catch するため当面 unused だが、Installment 2-b の net/http resource returners で descriptor 駆動 consume が実体化する)。
- `src/runtime/native/io.cpp` の `setLastError` を dual-write 化 — io 専用 thread-local buffer `io_last_error_buf` (`__ry_io_get_last_error` から読み出し可) と shared buffer (`__ry_set_last_error` 経由) の双方に書き込む。前者で descriptor が derive する per-module channel `__ry_io_get_last_error` を有効化し、後者で cross-module caller (`__ry_json_load_file` / `__ry_json5_load_file` 等) との後方互換を保持。`DEFINE_LAST_ERROR(io)` macro ではなく手書きで定義した (macro は dual-write を表現できない)。
- io runtime symbol を `__ry_io_<snake_case(fn)>` 規約に統一: `__ry_read_all` → `__ry_io_read_all`、`__ry_read_text` → `__ry_io_read_text`、`__ry_write_text` → `__ry_io_write_text`、`__ry_append_text` → `__ry_io_append_text`、`__ry_file_exists` → `__ry_io_exists`、`__ry_delete_file` → `__ry_io_delete_file`、`__ry_read_bytes` → `__ry_io_read_bytes`、`__ry_write_bytes` → `__ry_io_write_bytes`、`__ry_bytes_to_str` → `__ry_io_bytes_to_str`、`__ry_str_to_bytes` → `__ry_io_to_bytes`。`__ry_io_file_*` (File handle API) と `__ry_io_input_prompt` は既に correct prefix で touch せず。
- C++ test harness で io @native を inline 宣言していた箇所 (`tests/test_codegen_io.cpp`、`tests/test_codegen_type_safety.cpp`、`tests/test_codegen_net.cpp`、`tests/test_codegen_http.cpp`) を bare `@native` から `@native("io")` に変更 — `runSource` が ModuleLoader を skip するため rule (b) module-keyed inference が走らず、descriptor 駆動 dispatch の `native_lib_index_` lookup で library 名が必要。
- `isIoImported` gate と `markIo` lambda は Group C (File-coupled custom emitter) 用に残置。完全 retire は将来の installment で。filesystem dispatcher は元から存在せず (`emitGenericNativeCall` 経由で descriptor 駆動済み)、本 change で regression なし — `DescriptorStorage_ResultOutParamWithType` (`tests/test_native_call_descriptor.cpp`) と `tests/spec/filesystem.test.ry` が引き続き pass。 (#2338)
- `dispatchNet` (`src/codegen_call_io.cpp`) の `net_table[]` 10 entry を retire し、`bind` / `connect` / `tlsConnect` を `@native("net")` 宣言経由の descriptor 駆動 dispatch (`emitGenericNativeCall`) に統合した。handle-coupled overload (`listen` 2-arg / `accept` / `listenerPort` / `shutdown` / `setTimeout` family) は typed-handle check が必要なため #2338 の io File-coupled overload と同様に custom emitter で dispatch を継続。`RY_REGISTER_STDLIB_PACKAGE_FULL` で `snake_case=true` を追加して `tlsConnect` → `__ry_net_tls_connect` を導出 (priority 50 は維持)。
- `dispatchHttp` (`src/codegen_call_io.cpp`) の `http_table[]` 19 entry を retire し、`response` を descriptor 駆動 dispatch に統合した。残り 18 entry (`listen` × 3 overload / `method` / `path` / `body` / `bodyBytes` / `header` / `query` / `cookie` / `formField` / `formFile` / `queryAll` / `cookies` / `formFields` / `httpGet` / `httpPost` / `httpRequest` / `status` / `httpClientResponseFree`) は typed-handle check / NUL check / 制御フロー合成 (`emitHttpListen` ~232 行) のいずれかが必要で descriptor に乗らないため inline custom branch を維持。
- `include/ry/stdlib_registry.hpp` の `ResourceKindRegistry::Info` に `errorChannelLibrary` field (`const char*`、default は `library`) を追加。`registerKind` に optional 第 5 引数を追加し、resource が独自 error channel を持つ場合 (TlsStream のみ — `__ry_tls_get_last_error`) を declarative に表現可能にした。`src/codegen_fn.cpp:702-747` の descriptor 構築 loop で `inferResourceKind` 直後に `errorChannelLibrary != effectivePackage` のときに `desc.error_channel` を override する経路を追加。Installment 2-b 唯一の override 適用箇所は `tlsConnect`: `@native("net")` 宣言の package-derived default (`__ry_net_get_last_error`) を `__ry_tls_get_last_error` に上書き。
- `src/codegen_call_native.cpp:710` 周辺の `ReturnWrapping::ResultPtr` case で `addResourceKind` 直後に resource_kind-driven library linkage を追加。`Info::library` を `used_native_libraries_` に insert することで、`@native("net")` 宣言だが TLS 実装が http library に存在する `tlsConnect` で http library を自動 link。`matchedPackage` の既存 insert (line 564) と idempotent に共存する。これは resource_kind 由来の linkage であり、issue out-of-scope の `used_native_libraries_` 完全 consolidation とは独立。
- net runtime symbol を descriptor convention (`__ry_<lib>_<snake>(callee)`) に合わせて rename: `__ry_bind` → `__ry_net_bind` (`src/runtime/native/net.cpp:38`)、`__ry_connect` → `__ry_net_connect` (`src/runtime/native/net.cpp:192`)。`__ry_tls_connect` → `__ry_net_tls_connect` (`src/runtime/native/http/tls.cpp:161` — symbol 名は net convention に従うが実装は依然として http library 内、resource_kind-driven library linkage が http link を保証)。http runtime symbol は `__ry_http_response_create` → `__ry_http_response` (`src/runtime/native/http/http.cpp:585`) のみ rename。custom emitter で残置する symbol (`__ry_listen` / `__ry_accept` / `__ry_listener_port` / `__ry_tcp_listener_shutdown` 等) は #2338 の io `__ry_io_file_open` 前例と同様に非 convention で維持。
- `emitHttpListen` 内の `__ry_bind` 参照 (`src/codegen_call_io.cpp:838`) を `__ry_net_bind` に rename 反映。`__ry_listen` / `__ry_listener_port` / `__ry_accept` 参照は rename 対象外の symbol を呼ぶため touch せず。
- C++ test harness で net + http @native を inline 宣言していた箇所 (`tests/test_codegen_net.cpp`、`tests/test_codegen_http.cpp`) を bare `@native` から `@native("net")` / `@native("http")` に変更 — descriptor 駆動 dispatch の `native_lib_index_` lookup と symbol derivation で library 名が必要。`sleep` は thread (Installment 3-c 対象、table-driven 継続) のため bare のまま。
- `tests/test_runtime_http.cpp` で `__ry_http_response_create` を `__ry_http_response` に rename 反映 (forward decl 1 箇所 + 直呼び 6 箇所)。
- `tests/test_native_call_descriptor.cpp` に Installment 2-b の coverage を追加: `tlsConnect` の error_channel override 検証、`connect` (TcpStream) と `response` (HttpResponse) の package-default 適用検証、`Info::errorChannelLibrary` 自体の registration 値検証。
- `isNetImported` / `isHttpImported` gate と `markNet` / `markHttp` lambda を導入 (dispatchIO 前例と同形式)。custom branch 経由の `used_native_libraries_.insert` 経路は維持し、`emitGenericNativeCall` フォールスルー経路は line 564 (matchedPackage) と line 720 (resource_kind library) で自動 insert される。
- `setTimeout` / `setReceiveTimeout` / `setSendTimeout` の TcpStream + TlsStream overload — `emitNetTimeout` が runtime symbol を type で切替えるため descriptor 駆動化には引数 type-based overload resolution が必要 (Installment 3 carve-out 候補)。
- `emitHttpListen` (~232 行制御フロー合成) — bind → listen → accept loop + handler call + sendResponse の連鎖は declarative descriptor に乗らない。
- HTTP NUL check (`__ry_http_str_has_nul`) pattern — descriptor の pre-call check 機構が未整備。
- `used_native_libraries_` の完全 consolidation — 残る手動 insert (`emitNetTimeout`、`emitHttpListen`) は別 issue で。
- `src/runtime/core/hash.cpp` の http 複製問題 (G4 anomaly) — 別 issue。
- thread / json / json5 / math の descriptor 化 — Installment 3-x。 (#2339)
- Type-driven `@native` dispatches that always required hand-written customEmitters — `math::abs` / `log` / `pow` / `floor` / `ceil` / `round` / `digits`, `json::stringify` / `stringifySafe` (and the `json5` variants), and `thread::threadSpawn` / `threadJoin` — are now compiler builtins (Pattern B) instead of `math_table` / `json_table` / `json5_table` / `thread_table` entries. The user-visible consequence is that those eleven names are now reserved: declaring a user `fn abs(...)` etc. raises `cannot declare function 'X': name is reserved for a built-in function` at parse time. Function behaviour, argument shapes, error channels, `Result` wrapping, and `mock` / `spy` interception (including `mock("pow(int, int)", ...)`) are unchanged. (#2340)
- **Breaking change**: the `any-implicit-unwrap` rule (#2321 / #2322) now also rejects three structurally similar hazards that were previously carved out of Path 9 ("tracked separately" in [`docs/reference/strict-any.md`](docs/reference/strict-any.md)). The seven sub-cases now covered by `[strict-any/any-implicit-unwrap]` are:
  - Variable declaration `n: int = v` where `v: any` (Path 9a, unchanged).
  - Named-fn call argument `f(v)`, including the default-value branch (Path 9b, unchanged).
  - Lambda-call argument `g(v)` (Path 9c, unchanged).
  - `Ok(v)` / `Err(v)` / `Some(v)` flowing into a typed `Result` / `Option` slot (Path 9d, unchanged).
  - **New**: reassignment of a previously-declared typed variable — both function-local (`x: int = 1; x = v`) and module-global write-through. Also covers `Result` slot widening on reassignment (`r: Result<int, str> = Ok(0); r = produce()` where `produce()` returns `Result<any, str>` or `Result<int, any>`), which previously slipped through `coerceResultType` (Path 9e).
  - **New**: returning an `any` value from a typed function or lambda (`fn f() -> int: return v`, expr-body lambda `() -> int => v`) (Path 9f).
  - **New**: mutating a typed collection with an `any` value — covers `append!` / `appended` / `insert` on `List<T>`, `add` / `remove` on `Set<T>`, `m[k] = v` on `Map<K, V>`, and `xs[i] = v` on `List<T>` (Path 9g).
  Recovery is the same as the existing rule: `case asType[T](v): Ok(x): ... Err(_): ...` before the boundary. Read-only `any → concrete` paths whose surface is not a slot-bound assignment — the `in` / `not in` membership operator and the `get(list, idx, default)` fallback value — continue to unwrap silently and are out of scope for this rule. Explicit `any` boundaries (`v: any = ...`, `from ry.json import load`, FFI `@extern` returns) remain valid. (#2379)
- All handle-coupled / NUL-checked / file-coupled `@native` entries in `io` / `net` / `http` now route through descriptor-driven generic dispatch instead of per-overload custom emitters. `NativeCallDescriptor` gains `handle_param_index` (inferred), `handle_resource_kind`, `exported_symbol`, `nul_checks`, and `iterator_elem_type_name` fields; three new return wrappings — `OptionFromNullablePtr`, `ResultOutParamOption`, `IteratorFromHandle` — cover the patterns the pre-2-c custom emitters owned. Per-overload symbol / NUL-check / iterator metadata lives in the `kOverrides` table in `src/codegen_native_call_descriptor.cpp`. Overload resolution in `emitGenericNativeCall` now disambiguates by `resource_kind` so `body(HttpRequest)` and `body(HttpClientResponse)` (identical `(ptr) -> ptr` LLVM signatures) route to their distinct runtime symbols (`__ry_http_body` vs `__ry_http_client_body`). One behavior change: `net::listen(TcpListener, int)` failures now report the runtime's `strerror(errno)` message via the `__ry_net_get_last_error` channel (now populated by `__ry_listen`'s new `setLastError` call) instead of the static "listen failed" string the pre-2-c custom emitter baked into IR. SSA value-name hints drift for migrated calls (e.g. `%open` instead of `%file_open_ptr`, `%httpGet` instead of `%http_get_result`) — cosmetic only; instruction shape, callee names, and metadata propagation are preserved. (#2381)
- `scripts/export-run-logs.sh` の `RUN_NONCE` 生成を microsecond ベース (`jq -nr 'now * 1000000 | floor'`) から `/dev/urandom` 由来の decimal uint64 に置き換え、生成ロジックを sourceable helper `scripts/lib/run-nonce.sh` (`gen_run_nonce`) に切り出し。これにより同一 wall-clock 秒内で複数の invocation が並走しても `run-id` (`YYYYMMDD-HHMMSS-<short-sha>-<nonce>`) が衝突しなくなる。`tests/scripts/test-export-run-logs.sh` に同一 shell プロセス内で `gen_run_nonce` を 50 回呼んで全値が distinct であることを assert する回帰チェックを追加 (PID stable な `$$` ベース実装への regression を検知)。`docs/architecture/jsonl-run-logs.md` を新しい nonce 仕様にあわせて更新。(#2402)
- release: bumped the `release.yml` Linux container pin from `ry-ci-glibc-old:llvm-21-rev12` to the latest published immutable revision `llvm-21-rev14`, so the v0.0.30 release builds on the current pre-baked image. (#2450)

### Deprecated

- `any` 値への直接算術演算子 (`+`, `-`, `*`, `/`, `//`, `%`, `**`, 単項 `-`) と順序比較演算子 (`<`, `<=`, `>`, `>=`) を deprecated とし、コンパイル時に stderr へ one-time 警告を発するようにした。`==` / `!=` と `print` / `str` / f-string 補間は retained-and-documented として警告対象外 (`__ry_any_eq` は型不一致で `false` を返すのみで trap しないため安全、文字列化は migration boundary として保持)。移行は #2315 で追加した `asType[T](v: any) -> Result<T, Error>` を使い narrow してから操作する。dedup は演算子単位で 1 回 (`+` を 5 回使っても警告は 1 行のみ)。動作は変わらず、警告のみで rejection は行わない (`tests/spec/any.test.ry` の既存テストは引き続き exit code 0 で pass する; 出力に warning が混ざるのは deprecation 期間中の意図された動作)。strict mode への昇格 (toggle 導入 + 既定の reject 化) は #2322 予定。compound assignment (`x += 5` where `x: any`) は `emitBinaryOp` 経由で同じ gate を通るためカバー済み。 (#2316)
- `any` から具体型への暗黙的 unwrap を deprecated とし、新しい strict-any ルール `any-implicit-unwrap` で段階的に廃止する。対象は `docs/architecture/implicit-any-paths.md` の Path 9 の 4 サブケース: 変数宣言 (`n: int = v`)、名前付き関数呼び出し引数 (`f(v)` の暗黙引数およびデフォルト値引数)、ラムダ呼び出し引数、`Ok(v)` / `Err(v)` / `Some(v)` の typed Result / Option スロットへの値。compat mode では Pattern 4 と同形式の警告を発し (`shouldEmitAnyLintAt` ゲートで stdlib / cross-package import は抑止)、`--strict-any` / `RY_STRICT_ANY=1` 下では `[strict-any/any-implicit-unwrap]` ハードエラーとして reject する。修復には #2315 で追加した `asType[T](v: any) -> Result<T, Error>` を `case` narrowing と組み合わせて使う。既存の round-trip 動作は compat mode で維持されており、`tests/spec/any.test.ry` の Path 9 関連 describe ブロック (`:207-272`) は引き続き exit code 0 で pass する。strict mode の既定化は #2322 予定。 (#2321)
- `@directive def` 宣言のパラメータについて、型注釈を省略すると Pattern 3 警告 (`warning: parameter '<name>' of @directive '<dir>' has no type annotation and defaults to 'any'; ...`) を新たに emit するようにした。named function (#2317) / lambda (#2323) と同じ implicit-any hazard が directive 定義にも存在しており、PR #2369 が明示的にスコープ外宣言していたものを本 issue で補完する。共有ヘルパー `emitImplicitAnyParamWarning` を再利用しているため、warning wording は三形式 (named fn / lambda / `@directive def`) で完全一致する。`shouldEmitAnyLintAt` ゲートも共通のため、stdlib / cross-package import は引き続き抑止される (`share/std/core/directive.ry` / `share/std/testing/testing.ry` の既存 `@directive def` 宣言はすべて annotated 済みで影響なし)。compat / strict mode の挙動は #2369 と同じく warning-only (新規 reject rule は導入しない)。回避策: 明示的な `: any` (intentional type-erasure) または concrete type annotation を追加。詳細は `docs/reference/functions.md` の "Type Omission and `any`" 節と `tests/spec/any_directive_param.test.ry` を参照。 (#2380)

### Removed

- The `--strict-any` CLI flag and `RY_STRICT_ANY` environment variable were removed — strict semantics are now the default, so the opt-in entry points serve no purpose. Passing `--strict-any` is reported as an unknown option; `RY_STRICT_ANY` is silently ignored. (#2322)
- **Breaking change**: legacy な stdlib import 形式 (`from math import …` / `from std.math import …` / `from std import …` / `import math` 等、13 個の `ry.*` 公開モジュールに対する flat / `std.*` 形式) を hard error に昇格した (#2350 の deprecation warning から変更)。canonical な `ry.*` 形式 (`from ry.math import …` / `import ry.math` / `from ry.lang import …`) のみが受理される。user-defined module (`math.ry` 等、stdlib 名と同名のローカルファイル) は従来どおり referrer dir で先に解決されるため (`from_stdlib=false`)、影響を受けない。`tests/spec/` (216 ファイル) / `examples/` (8 ファイル) / `docs/reference/` (19 ファイル) 配下の全 legacy 形式を canonical 形式へ移行済み。 (#2351)

### Fixed

- `ry fmt` no longer mistakes `#`-prefixed lines inside triple-quoted block strings (e.g. Markdown headings in a `@doc` body) for source comments. The comment extractor now tracks `"""..."""` state across line boundaries so block-string content survives round-trip formatting unchanged. (#1844)
- `dispatchJson` (`src/codegen_call_json.cpp`) now gates on whether any `json::*` symbol is registered before claiming `load<T>(...)` calls. The dispatcher chain (`src/codegen_call_dispatch.cpp:197`) iterates every registered stdlib dispatcher, and the previous unconditional `load<T>` interceptor would have routed `from json5 import load; load[T](...)` calls to the strict `json` parser. (#1855)
- `import ry` および `from ry import X` の bare 形式が予約 namespace ガードを bypass し、project-local の `ry/` ディレクトリ / `ry.ry` ファイルが silently shadow する問題を修正。bare 形式は無効として reject し、`ry.<module>` の使用を促すヒントを返す。あわせて `ry.*` 経路は #1769 で documented public とされた 11 module (`ry.lang`, `ry.math`, `ry.io`, `ry.path`, `ry.filesystem`, `ry.json`, `ry.http`, `ry.thread`, `ry.regex`, `ry.testing`, `ry.base64`) のみ受理する allowlist を導入。internal modules (`ry.builtins`, `ry.gc`, `ry.core`, `ry.runtime_internal` 等) は許可リストヒント付きで reject される。bare な互換エイリアス (`from net import`, `from json5 import`) は変更なし。`ry.*` 配下のエラーメッセージは loader 内部のスラッシュ表記ではなくユーザが書いたドット表記で返す。 (#2297)
- `parseTypeConstraint` (`src/codegen_type.cpp`) が単一整数リテラル型 / 範囲型の上下境界 / 整数リテラル union のメンバーで `int64_t` を超える値を受け取った際、`std::stoll` の uncaught `std::out_of_range` でプロセス全体が abort (`libc++abi: ... stoll: out of range`, exit 134) する問題を修正。`src/parser/parser_decl.cpp:951-955` の固定長配列サイズで既に確立されている `std::strtoll` + `errno == ERANGE` + end-pointer + empty-string check のパターンを移植した file-local テンプレート helper `parseInt64Bound` を導入し、4 箇所 (`std::stoll`) の呼び出しを差し替えた。失敗時は `codegenError` 経由で通常のコンパイル診断 (`integer literal out of range in type constraint: <value>` / `range low bound out of range ...` / `range high bound out of range ...`) を返す。 (#2307)
- `CodeGen::emitExprVariant(FieldAccessExpr)` (`src/codegen_expr_literal.cpp`) が `unsigned long` を超える数値タプルフィールドインデックス (例: `t.999999999999999999999999999999999999999999`) を受け取った際、`std::stoul` の uncaught `std::out_of_range` でプロセス全体が abort (`libc++abi: ... stoul: out of range`, exit 134) する問題を修正。`src/parser/parser_decl.cpp:951-955` の固定長配列サイズで既に確立されている `std::strtoul` + `errno == ERANGE` + end-pointer check のパターンを移植し、parse 失敗時は既存の `tuple index <field> out of range` 診断にルーティングする (ユーザー可視診断は不変)。 (#2308)
- `case <subject>:` (`Parser::parseCaseStatementWithSubject`, `Parser::parseCaseExprWithSubject`) が、subject 式に multiline UFCS chain を含む場合 (`case xs
    .len():`) に `expected indented block` で reject される問題を修正。chain が body の literal `Indent` token を absorb した残余を、`Parser::parseBlock` が `#2136` で導入した `chain_pending_dedents_` 経由で扱うのと同じ accounting を、`case` body opening でも行うよう、shared helper `Parser::consumeBlockOpening` (`src/parser/parser.cpp`, declared in `include/ry/parser/parser.hpp`) に共通化。`parseBlock` および 4 つの `case` body opening (with-subject / no-subject の statement / expression form, `src/parser/parser_decl.cpp` + `src/parser/parser_expr.cpp`) をすべて helper 経由に経路化し、`while` / `if` の body opening と挙動を完全に揃える (深さ 2 以上の chain も含む)。あわせて `Parser::parseCaseExprArmBody` (`src/parser/parser_expr.cpp:159`) の block-arm opening も同一 helper 経由とし、case-expression の arm guard に multiline UFCS chain を含み arm body が block 形式の場合に `expected indented block after ':' in case expression arm` で reject される latent な同種バグを修正 (statement form の arm body は `parseBlockOrInline` → `parseBlock` を経由しており影響なし)。診断メッセージは `parseBlock` と同じ `expected indented block` に統一される。 (#2311)
- `Map<str, V>` への動的キー挿入後の retain/release が ArcHeader (-16) で行われ、StringHeader (-24) と offset が食い違う bug を修正。`for k in keys: m[k] = v` / `k = keys[i]; m[k] = v` / `m[rec.strField] = v` の三経路すべてに影響し、Linux/default-emit (release build) では JIT optimiser が固定アドレスへの UB store を残すため、3 件目あたりの挿入で `private constant` global の weak_count に書き込んで SIGSEGV していた。macOS/rust-emit では同じ UB が malloc 配置や Mach-O の rodata 扱いで偶発的に許容されていただけで、`RY_NO_OPT=1` でも観測可能。`retainArcValue` 経路を str 認識ベースで分岐させ、`for` ループ束縛・暗黙束縛・record field の三経路すべてが StringHeader (-24) へ正しく合流する。(#2375)
- tree-sitter grammar (`editor/tree-sitter/grammar.js`) で以下 2 件の不整合を解消し、対応する corpus entry を追加して editor (Neovim 等) の syntax highlight / indent 体験を修正:
  - `directive_def_declaration` ルールが body 必須に書かれていたため、`share/std/core/directive.ry` 等の body-less な `@directive(target=[...])
fn name(...)` 形が ERROR ノードを発生させていた問題を修正。`@directive(...)` 末尾の NEWLINE を rule に明示し、後続 `function_declaration` の body-less 形 (既存 `choice(function_body, _newline)`) を再利用する。`test/corpus/decorators.txt` に 3 ケース (single target / multi-target / `@public` 前置) を追加して shape を lock。
  - `qualified_import_statement` ルールの `module` field が単発 `IDENT` 想定だったため、`import ry.math` / `import ry.math as m` の dotted module path 形が ERROR ノードを発生させていた問題を修正。`field('module', $.module_path)` に変更し dotted form を許容。既存 corpus 3 entry および `queries/highlights.scm` の `@module` キャプチャを追従更新。
- あわせて `editor/tree-sitter/expected-fail.txt` の housekeeping:
  - `#1618` 由来 3 entry (`arc_set_map_tuple_2226.test.ry` / `tuple_nested_generic_2264.test.ry` / `nested_fn_loop_capture.test.ry`) の triage を完了し、各 bucket の既存 grammar gap (tuple member access `.0`/`.1` / top-level `@const NAME: T = value`) と一致する旨を comment で明示。
  - 既に clean parse する `tests/spec/implicit_widening.test.ry` を expected-fail から削除。
- `./editor/tree-sitter/check.sh --verbose` で smoke `pass=169 skip=49 warn=0 fail=0` / `tree-sitter test` で corpus 119/119 pass を確認。 (#2382)
- `scripts/export-run-logs.sh` のディレクトリ展開で symlink された `.test.ry` が `find ... -type f` に拾われず黙って除外される問題を修正。`find -L` でリンクを辿るように変更し、リテラルファイル経路の `[[ -f ... ]]` 判定 (symlink 既追従) と挙動を一致させた。`tests/scripts/test-export-run-logs.sh` にディレクトリ配下の symlink `.test.ry` を回帰検出する sub-test を追加。(#2403)
- The 5-argument form of `http.listen` (with a `portCallback: fn(int) -> Unit`) now works when `portCallback` captures variables. Previously the callback was invoked through a raw `void(i64)` call that treated the closure value as a bare function pointer, crashing at runtime for any capturing callback — including the documented pattern of binding to port `0` and storing the OS-assigned port through a captured handle. The callback is now dispatched through the standard closure-call path, so every closure form runs correctly. (#2421)
- A single-expression lambda with an explicit `-> Unit` return type whose body is a Unit-returning call (e.g. `(p: int) -> Unit => store(p)`) no longer crashes the compiler. The single-expression codegen path returned the body's value unconditionally, but a Unit-returning call produces no value, so codegen dereferenced a null value and segfaulted; it now emits `ret void` for a Unit-typed body and rejects a non-Unit single-expression body as a type error (matching `return <expr>` in a Unit function). (#2421)
- `CodeGen::resolveType` (`src/codegen_type.cpp:139`) が `T[N]` の `N` パースに `std::stoull` を裸で使っており、overflow / 非10進数 / 空文字を受け取った場合に `std::out_of_range` / `std::invalid_argument` が uncaught でコンパイラ全体を `libc++abi` 経由で abort (exit 134) させうる残存 hit site を、`src/parser/parser_decl.cpp:951-955` で確立済みの `strtoull` + `errno == ERANGE` + end-pointer + 空文字ガード パターンに置換した。失敗時は parser 経路と同一文言 `invalid or out-of-range array size in array type T[N]: <value>` を `codegenError` 経由で返すため、ユーザー向け診断は変わらない。これは PR #2341 (closes #2308) の本文で「`codegen_type.cpp:133` remains a separately tracked open hit site」とされていたまま、追跡先の `.claude/rules/parser-conventions.md` が PR #2343 で削除されて 6 か月以上放置されていた防御的ハードニング案件。surface 構文 `buf: u8[999999...]` は parser が既に同じ文言で reject するため通常のユーザー経路からは到達しないが、`resolveType` は型エイリアス展開 / ジェネリック実体化 / レジストリ参照など複数の内部呼び出し元から再到達するため、`std::sto*` 例外が compiler abort に化ける窓は塞ぐ価値がある。
- あわせて `rg 'std::sto' src/` で残存 hit site をスイープし、`codegen_call_collection.cpp:77` (`tryParseSegmentInt` の `try`/`catch` 形式) と `codegen_expr_literal.cpp:298` (`strtoul` 化済、#2308) が既に保護済みであることを確認。`src/` での `std::sto*` の未保護呼び出しは 0 件 (`using namespace std` も `src/` で 0 件)。
- 失われた `parser-conventions.md` の代替として、`.claude/rules/codegen-numeric-parse-exception-safety.md` を `src/**/*.cpp` / `src/**/*.hpp` / `include/ry/**/*.hpp` スコープで新規追加し、`std::sto*` 禁止 / `strto*` + errno + end-pointer + 空文字ガードの 3 点セット / 既存 hit site への参照 (`parser_decl.cpp:951-955` / `parseInt64Bound` / `codegen_expr_literal.cpp:298`) を記述した。既存の `build-warning-flags.md` は CMake 系パスにスコープされており codegen 編集時に surface しないため、issue scope 項目 4 の「最も近い既存ルールに統合」案ではなく新規 path-scoped ルールとした。 (#2422)
- `ry fmt` が `import ry.<mod>` 形式の qualified import を `import <mod>` (bare 形式) に書き換える破壊書換を行い、#2351 で hard error 化された legacy syntax を生成する不具合を修正。`Formatter::formatQualifiedImport` が `module_name` (dotted path の最終セグメント、例: `math`) のみを emit して `ry.` namespace prefix を脱落させていたのが原因。AST 側には dotted path の生の slash 表記 (`ry/math`) が `import_path` として保持されているため、これが非空の場合は slash を dot に変換して `ry.math` を再構成し、空のときのみ `module_name` への従来 fallback を行うよう修正。`tests/test_formatter.cpp` に `import ry.math` / `import ry.math as m` / `import ry.json5` / `from ry.net import open as net_open` の 4 ケースを round-trip 退行テストとして追加した。(#2423)
- `Formatter::formatEnum` (`src/formatter_stmt.cpp:233`) が `enum` variant の明示的 discriminant (`Ok = 200` / `NotFound = -1` 等) を出力時に完全に脱落させ、ABI / wire format / debug 出力の意味論を破壊する書換を行っていたバグを修正。AST 側は `EnumVariant::explicit_value` (`include/ry/ast/ast.hpp:323`) として `std::optional<int64_t>` を以前から保持していたが、formatter 実装が variant 名と associated-data 型しか emit せず、explicit value 句を一切参照していなかったのが原因。負値・ゼロ・`int64_t` 境界値 (`INT64_MIN` / `INT64_MAX`) を含めて round-trip 保存されるよう `" = " + std::to_string(*variant.explicit_value)` を末尾に追加し、`tests/test_formatter.cpp` の `EnumExplicitDiscriminantValues` で 4 ケース (正値 / 負値+ゼロ / i64 境界 / idempotent 再 format) を退行検出として固定した。
- あわせて同 formatter が `enum Variant(name: type, ...)` 形式の named associated-data field 名も silently 脱落させ unnamed 形式 (`Variant(type, ...)`) に書き換えていた側方欠陥も発見したため、parser が両形式を `field_names` / `field_types` のパラレル配列で AST に保持している事実 (`src/parser/parser_decl.cpp:729-744`) を踏まえて `field_names` 非空時のみ `name:` + 空白を前置する形で同時修正した。`RecordEnumDef` テストに named-fields ケースを追加し退行を固定。 (#2424)
- `Formatter` が CallExpr の引数として現れる multi-line inline lambda (例: `t = threadSpawn(():
  body
)`) を round-trip できず、フォーマット結果が再 parse 不能になり `ry fmt` が `tests/spec/thread.test.ry` 等のファイルを silently skip する不具合を修正。原因は `formatExpr` の `LambdaExpr` ブランチ (`src/formatter.cpp:560`) が body を `out_` に直接 emit しつつ空文字列を返していたため、外側の `CallExpr` ブランチが `"callee(" + "" + ")"` を後追いで emit し、body が assignment 行の途中に splice されていたこと。block-form を生成するヘルパー (`Formatter::emitCallTrailingLambda` / `Formatter::callExprNeedsBlockForm` / `Formatter::findTrailingMultiLineLambda`) を追加し、`formatAssign` / `formatReturn` / `formatIndexAssign` / `formatFieldAssign` / `formatTupleDestruct` / `formatCall` の値位置で CallExpr を検出した時点で multi-line lambda を含むケースは `callee(args, ():
  body
)` 形式に直接 emit するよう変更。`emitCallTrailingLambda` は positional 引数と named 引数 (`named=():..`) を統合した単一シーケンスとして走査するため、trailing block が positional 末尾でも named 末尾でも同様に検出される。trailing chain (`outer(inner(():..))`) は close paren を stack して再帰し、non-trailing 位置の multi-line lambda (`call(():
  body
, 3)`) も継続インデントを正しく emit する。non-trailing 位置の `(): # cmt` 行 inline コメントは block-form 内で `emitInlineComment` を経由するため round-trip で保持される。`tests/test_formatter.cpp` に 14 件の round-trip 退行テストを追加 (Assign / Return / IndexAssign / FieldAssign / TupleDestruct / BareCallStmt / Nested / WithLeadingArgs / WithParamsAndReturnType / InNestedBlock / NonTrailingPositionAsStmt / NonTrailingPositionInAssign / **NamedArgRoundTrip** / **NonTrailingPreservesInlineComment**)。
- 既知の限界: (a) non-trailing 位置の **ネストした** call が multi-line lambda を含むケース (例: `call(outer(inner(():..)), other)`) は round-trip 可能な canonical 形式が存在しないため `formatExpr` の splice 経路に fallthrough し、round-trip verifier が "formatted output failed to re-parse" として skip する (silent corruption ではない)。(b) close paren 行の inline コメント (`)  # cmt`) は AST が close paren の source line を保持していないため preserve していない。これは pre-existing な formatter 全体の制約 (本 PR 以前の `formatCall` trailing-lambda 経路も同じ drop) であり、close-paren line tracking の追加もしくは source scan 機構が必要で本 issue の round-trip スコープ外。本 issue 内で確認した現実コード (`tests/spec/thread.test.ry` 含む) ではどちらの形も出現しない。 (#2425)
- `f(args)[T]("arg")` 等の chained call (callee が単一 IDENT でない `<expr>(args)`) を parser が silent split せず明示的な parse error で reject するよう変更。従来は `r = make(42)[int]("inner")` を 2 文 `r = make(42)[int]` + `("inner")` に silent 分解し、`ry fmt` 後に改行で分断されたコードが出力されて「fmt が壊した」と誤認される原因となっていた。エラーメッセージは関連 issue #809 (chained call サポートは `not_planned`) への参照と、中間変数への束縛 (`tmp = f(args)[T]` → `tmp(args)`) という workaround を含む。`Ident[T](args)` (`identity[int](42)` 等) や `f(...)?[T]` / `f(...)?.method()` は引き続き正常動作する。 (#2426)
- `ry fmt` が `\u{HHHH}` Unicode escape を含む合法な Ry プログラムを `unknown escape sequence '\u'` で reject していたバグを修正 (#2427)。PR #2373 (closes #2326) の "Notes on known-fmt issues" で fmt 単独の欠陥として宣言されていたが、原因は fmt と `ry run` が共有する lexer が `\u` 自体を escape として認識していなかったことで (fmt 専用 string parser は存在せず `Formatter::escapeString` の default arm に decode 済み UTF-8 byte を素通しさせるだけ)、lexer 側に escape を実装したことで自動的に fmt も close criteria を満たすようになった。fmt は decode 済み literal UTF-8 (`"😀"`) を出力するため、二度目の format pass は固定点となり (`Formatter::verifyFormatting` で idempotency 検証) round-trip も保持される。 (#2427)
- `ry test --trace <dir>` (multi-file) と `ry test --trace <file>.test.ry` (single-file JIT 経路) の双方で、trace JSONL に `session.start` のみが出力され対応する `session.end` が抑止される問題を修正。multi-file 経路では `warnAndDisableMultiFileFlags` が `session.start` 発火後に `ry::configureTrace(false, "")` を呼び recorder を破棄するため destructor の `traceEnabled()` gate で `session.end` が落ちていた (PR #2416 で side finding として記録されていたが対応 issue 未作成のまま v0.0.30 を迎えた)。single-file 経路は `finalizeAfterPossibleJit()` の `_exit()` shortcut が `SessionTraceGuard` の destructor をスキップするため同じ症状。`session.start` の発火状況を `TraceRecorder` 内で追跡する `ry::flushSessionEnd()` API を追加し、destructor と `_exit()` 直前の両方で冪等に呼び出す。multi-file 経路の `configureTrace(false, "")` は削除 (parent プロセスは `session.start` 以降 trace イベントを発火しないため安全)。`scripts/export-run-logs` 等の JSONL 消費側で `session.start`/`session.end` のペアリングを前提とした集計が正しく動作するようになる。(#2429)
- 文字列リテラル中の `\xNN` hex escape が `unknown escape sequence '\x'` で reject されていたバグを修正 (#2440)。`docs/reference/builtins-string.md:20` は `\xNN` が標準 escape 集合に含まれると明記していたが、PR #2427 (`\u{HHHH}`) と同様に lexer の 3 つの escape switch のいずれにも `case 'x':` が存在しなかった。`ry fmt` も同じ lexer を共有するため自動的に `\xNN` を受理するようになり、decode 済み literal byte を出力する (例: `"A"` → `"A"`)。`�` のように非 UTF-8 single byte に decode される escape は fmt 出力後に source file が非 UTF-8 になるが、format 自体は exit 0 で idempotent (`\u{FF}` が valid な 2-byte UTF-8 に decode されるのと対照的)。
- 非 ASCII 文字 (`α` / `ℕ` / `π` など) を含むソースでの lexer 診断が、UTF-8 シーケンスの先頭 1 バイトのみを Token 値として返し、ターミナルで U+FFFD (`�`) としてレンダリングされる問題を修正。`Lexer::readToken` の default fallback で UTF-8 シーケンスを完全に decode し、Token 値に code point 全体を格納するよう変更したことで、`unexpected token '<X>'` 診断にソーススニペットとキャレット付きで該当 code point が表示されるようになった。`checkNoTrailingIdentStart` のメッセージも `invalid character '<X>' after numeric literal` の形で文字を含むよう改善した (predicate は ASCII alpha/`_` のまま据え置き — 非 ASCII follow-on は default fallback 経由で parser の `DiagnosticError` に流すことでソーススニペット + キャレットを維持するため)。`decodeUnicodeEscape` (`"\u{α}"` 等) と `decodeHexEscape` (`"\xα"` 等) の `invalid hex digit '<X>'` メッセージも同様の UTF-8-aware レンダリングに変更。不正な UTF-8 lead byte (`�` 単独 / truncated `�` 等) は `\xHH` の hex escape 表記で deterministic にエラー化される。 (#2442)
- macOS release tarballs: `scripts/bundle-dist.sh` now rewrites the build-tree absolute `LC_LOAD_DYLIB` of process-linked `libry_*` cdylibs in `ry` to `@rpath/<basename>` and sets a matching `@rpath` self-id on every bundled `libry_*.dylib`. `libry_xid` (added by #2314 for json5 Unicode unquoted keys; process-linked at startup since it has no `@native("xid")` and is called directly from `src/runtime/native/json5.cpp`) carried cargo's absolute build-tree path in `ry`'s load commands, and the script previously rewrote only `libLLVM` / `libemit` / `liblower`. `scripts/verify-bundle.sh`'s `ry has no absolute LLVM/zstd/build-tree refs (forbidden token present)` check therefore failed the first macOS v0.0.30 build job. The new loop is generic (`otool -L "$RY" | awk '/libry_[A-Za-z0-9_]+\.dylib/{print $1}'`), so any future process-linked `libry_*` cdylib is covered without a sync-point edit; the new rule is captured in `.claude/rules/distribution-packaging.md`. (#2314)

## [0.0.29] - 2026-06-21

### Added

- `ry run` is now a dual-purpose subcommand: in addition to listing and running `[scripts]` entries from `package.toml`, it can execute Ry source files directly. `ry run <file.ry>` and `ry run <path/to/file.ry>` run a Ry file with the same semantics as `ry <file.ry>` (bare names resolve via `[paths]`, paths with directory components are read as-is). `ry run <name>` without a `.ry` extension first checks `[scripts]` and falls back to resolving `<name>.ry` via `[paths]`; when both forms exist for the same name, the script takes precedence (append `.ry` to bypass it). `ry run -- args...` runs the project entry point with arguments. `ry run <file.ry> arg1 arg2` passes positional arguments to the file (available through `args()`). Listing scripts and running a script still skip LLVM initialization for fast startup; running a Ry file initializes LLVM lazily on demand. Existing `ry <file>` / `ry --` / `ry` (no args) invocations are unchanged in this release; their deprecation follows in #1735. (#1734)
- `@beforeAll`, `@beforeEach`, `@afterEach`, and `@afterAll` may now be declared at file top level (outside any `@describe` block). File-level hooks wrap every `@it` in the file, including those inside `@describe`, and cascade with describe-level hooks: file `@beforeEach` fires **before** describe `@beforeEach`, and file `@afterEach` fires **after** describe `@afterEach`. File `@beforeAll` runs once before the first test anchor (top-level `@it` or `@describe`) and file `@afterAll` once after the last. `@timeout`-triggered tests still skip every `@afterEach` layer (file and describe) for the same `siglongjmp` reason that already applied to describe-level `@afterEach`. At most one of each hook kind is allowed per file at top level, mirroring the per-describe limit; combining file-level `@beforeEach` / `@afterEach` with `@each` or `@property` `@it` is rejected for the same reason combining describe-level hooks with them is. (#1780)
- Triple-quoted block string literals `"""..."""` for multiline text such as Markdown-flavored documentation. The lexer normalizes incidental leading/trailing newlines, strips the baseline indentation matching the closing delimiter, preserves intentional blank lines, and decodes the same escape sequences as regular strings (`\n`, `\t`, `\r`, `\\`, `\"`, `\0`). Block strings evaluate to the same `str` runtime value as the equivalent regular string, so they interoperate transparently with all string APIs. The formatter preserves the triple-quoted form on round-trip; regular `"..."` strings keep their single-quoted form. (#1843)

### Changed

- `self-update` internals cleaned up to remove the nightly-era abstractions that survived the v0.0.14 (`#1372`) `--nightly` removal: `detail::resolve_update_target`'s signature changed from `(const std::string &mode, ...)` to `(const std::optional<std::string> &tag, ...)` (the `"stable"` sentinel and a version-tag string no longer share one parameter), `cmd_self_update`'s argument parsing now builds a `std::optional<std::string>` up front and collapses the duplicated `v` prefix normalization + `is_valid_tag` validation in the caller (the callee-side duplicates were removed), and `detail::extract_json_string`'s unreachable bool / null value branches were dropped since the only production callers (`tag_name` in `cli/self_update.cpp` and `version` in `project/paths.cpp`) read string values. The documented user-facing flows are unchanged — `ry self-update` still picks the latest stable release and `ry self-update <version>` still installs a specific tag. One previously undocumented usage shifts: `ry self-update stable` used to be the same as the bare `ry self-update` because `"stable"` was the sentinel; it now goes through the specific-version path and reports `Version vstable not found.` Use the no-argument form for the stable release. (#1535)
- `@afterEach` (both describe-level and file-level) now runs even when `@timeout(N)` fires for the body of an `@it`. The codegen emits two independent `sigsetjmp` landing pads — one for the body phase (file `@beforeEach` → describe `@beforeEach` → test body) and one for the `@afterEach` phase (describe `@afterEach` → file `@afterEach`) — each with its own fresh `N`-ms `setitimer` budget. A body timeout `siglongjmp`s out of the body, lands in the `@afterEach` phase, and cleanup proceeds against possibly partially set-up state (`@afterEach` should be written to tolerate this — e.g. nil-guard handles before closing them). If `@afterEach` itself blows its `N`-ms budget, a secondary failure line `@afterEach (timeout after Nms)` is printed alongside the body outcome and the test runner moves on, so a hung cleanup never blocks subsequent tests. Worst-case wall-clock per timed test is now `2N` ms. The ARC release-skipping leak documented for body timeouts applies symmetrically to objects allocated inside an `@afterEach` body that itself times out — leaks, not use-after-free. (#1781)
- `json.stringify` and `json.stringifySafe` now accept a `sortKeys: bool` named argument (default `false`). When `sortKeys=true`, `Map<str, any>` entries — including nested ones — are emitted in byte-lexicographic key order, equivalent to the removed `stringifySorted` / `stringifySortedSafe` functions. The named argument composes with the existing optional `indent` positional, e.g. `stringify(m, sortKeys=true)`, `stringify(m, 2, sortKeys=true)`, `stringifySafe(m, sortKeys=true)`. (#1890)
- `ry test -p` / `ry test --parallel` now accepts an optional positive worker count. Bare `-p` / `--parallel` defaults to `std::thread::hardware_concurrency()`; `-p N`, `--parallel N`, and `--parallel=N` select N workers. The effective count is capped only by the number of discovered test files. The hard-coded four-worker cap that previously suppressed the LLVM/JIT teardown SIGABRT family (#1187 / #1895 / #2172) has been removed; the crash family remains parallelism-dependent — reduce `-p N` or drop `-p` as the first triage step under intermittent SIGABRT. Invalid worker-count inputs (zero, negative, malformed, missing value) are rejected with a clear diagnostic. (#2177)
- `ry test -p` / `ry test --parallel` の暗黙デフォルトワーカー数を `std::thread::hardware_concurrency()` から `hardware_concurrency() - 1`(最低 1)に変更し、1 コアをユーザ操作や他プロセス用に空ける挙動に統一した。`hardware_concurrency()` が `0`(取得失敗)あるいは `1` を返した場合はどちらも 1 ワーカーになる。明示指定の `-p N` / `--parallel N` / `--parallel=N`、およびシーケンシャル `ry test` の挙動は変えない。並列実行開始時に `Running M test files with K workers...` を stderr に表示し、終了サマリーの `(K workers)` 表示と合わせて開始・終了の両端から並列数を確認できる。`.github/workflows/ci.yml` の bare `-p` 呼び出しは新しいデフォルトをそのまま継承する。(#2216)

### Removed

- The bare CLI invocations `ry <file.ry>`, `ry --`, and `ry` (no args, runs entry from `package.toml`) are removed. Use `ry run <file.ry>`, `ry run --`, and `ry run` instead. The `ry run` subcommand also now honors the global `--emit-llvm-ir` flag (e.g. `ry --emit-llvm-ir run <file.ry>` replaces the old `ry --emit-llvm-ir <file.ry>` invocation). (#1735)
- **Breaking change**: `json.stringifySorted` and `json.stringifySortedSafe` are removed. Migrate `stringifySorted(v)` → `stringify(v, sortKeys=true)` and `stringifySortedSafe(v)` → `stringifySafe(v, sortKeys=true)`; the optional `indent` positional argument still precedes `sortKeys` (`stringifySorted(v, 2)` → `stringify(v, 2, sortKeys=true)`). (#1890)

### Fixed

- `ry test --outline` で directory / auto-discovery 経由でも各 `.test.ry` の outline が出力されるよう、子 subprocess の argv に `--outline` を forward した。fan-out の親は outline モード時にサマリ行と progress 行を suppress するので、出力は per-file outline 内容のみ(stdout+stderr は subprocess pipe で merge されるため pre-#2234 sequential 経路の分離ストリームと byte-identical ではないが内容は等価)。#2234 が同 path で warn + disable していたものを復活させた形。`--coverage` (cross-process 集計が必要) と `--trace` (shared file の clobbering) は引き続き single-file のみで multi-file warn + disable のまま。 (#2236)
- `wrapInAny` の str retain 分岐を metadata-gated にし、`filter` / `slice` / `map` などのループ内で container element の fresh load を str と誤判定して StringHeader offset `-24` で retain するヒープ破壊を解消した。 `#1799` で導入された `isStringValue(val)` (`ptrTy_ && !isNonStrPointer` の否定証拠ベース判定) は、metadata を持たない `ptrTy_` 値を str と肯定するため、Map / List / Set header の data ptr に対して `-24` で `strong_count` を inc し、隣接 allocation の末尾 8 bytes (しばしば兄弟 Map の `keys` buffer の str ptr) を破壊して `runtime error: map key not found` を引き起こしていた (KNOWLEDGE.md L49 `## map key not found` (#1888) の root cause)。本修正は `wrapInAny` の str 分岐に肯定証拠ゲート (`arc_str_owned_values_` / `arc_str_managed_vars_` membership、`GlobalVariable`、`meta->str_elem`) を追加して container element の fresh load を str dispatch から除外する。 (#2246)
- Higher-order combinators (`filter` / `map` / `reduce` / `fold` / `any` / `all` / `tap`) now stamp container element metadata (`list_elem_type_name`) onto each freshly loaded loop element before the callback dispatch, and `get(Map, key)` / `get(Map, key, default)` propagate `map_value_type_name` (and `map_value_fn_type_info`) onto the loaded value and through the merge PHI. Without this, the metadata-less `ptrTy_` element caused `coerceCallArgs` → `wrapInAny` to tag the value as `Str=3` instead of the collection tag (silently breaking `any == any` for collection-typed callback params) and made nested-Map lookups like `case get(m, "o"): Some(inner): inner["i"]` fail at codegen with "str does not support index access". Closes the upstream gap that `#2246`'s local `wrapInAny` retain gate was patching. (#2247)
- `iter()` over nested containers (`List<Map<...>>` / `Set<Map<...>>` / `Map<K, List<...>>` / `List<List<...>>` etc.) now propagates the source-level element type name through the IteratorHeader so that downstream `for elem in iter(xs):` / `toList(iter(xs))` / `filter(iter(xs), p)` / `take(iter(xs), n)` consumers resolve the element's collection metadata. Without this, the loop variable's static type fell back to `str` after the next-fn boundary, causing `len(m)` to read offset-0 of a StringHeader (returning 0 instead of the Map size — a silent wrong result) and `m["a"]` / `inner[0]` / `v[0]` to fail at codegen with `str does not support index access`. The fix introduces a new `ValueMetadata::iterator_elem_type_name` channel stamped by `emitIteratorHeaderAlloc` (with `propagateMeta` extended to copy it so `it = iter(xs); for m in it:` works), read by the for-in iterator branch in `codegen_stmt_loop.cpp` and by `toList`. Map iter encodes `(K, V)` so the tuple destructure `for k, v in iter(m):` distributes per-binding metadata via `splitTupleSig`. Companion to `#2247` (PR `#2253`); identity-lambda `map(iter(xs), m => m)` chained access is tracked as a follow-up because it requires lambda return-type inference rather than additional metadata propagation. (#2261)
- Named nested functions declared inside a top-level `for` or `while` body now correctly capture loop induction variables and body-local variables, matching the spec in `docs/reference/functions.md` ("Nested named functions can capture variables from enclosing scopes, just like lambdas") and matching how lambdas already behave in the same position. Previously such named fns failed compilation with "undefined variable" because the capture-analysis gate was reached only when `fn_nesting_depth_ > 0`, so a fn declared in a loop body at the top level was treated as a true top-level function and its body lost access to the loop's scope. Module-globals (#817) continue to flow through the `__ry_modvar_<name>` trampoline rather than being captured by value, so #817 write-through semantics are preserved for the named-fn path. (#2263)
- `fn` および lambda の return type に `weak T` を書くことを parse 時に reject するようになった。これまでは `fn make() -> weak str:` のような宣言が通り、呼出側で型推論が `str` に倒れて weak ref semantics（`case` での auto-upgrade、`None` 化）が消失していた。根本原因は単なる metadata 伝搬問題ではなく lifetime 問題で、`return weak xs` の `xs` (唯一の strong owner) が return 直前に die するため、呼出側に届く weak ref は構造的に dangling になる。ry には borrow-checking がなく param/struct-field-source の sound pattern を区別できないため、宣言を一律 reject する方針を採用。ローカル `w: weak T = weak src` の正規パターンは引き続き使える。型 alias 経由（`type W = weak str; fn make() -> W:`）と wrapped return type (`List<weak T>` 等) は parser-only check のため検出されないが、使用時の動作は undefined であることを docs に明記。 (#2266)
- Tuple field metadata is now propagated through every producer path, not just `List<(K, V)>` direct indexing (the only path #1664 had covered). `r.1["a"]` on a function-returned tuple, `xs[0].1["k"]` on a type-alias-expanded `List<TupleAlias>` element, `m[k].1["a"]` on a `Map<K, Tuple>` value, `for k, v in m: v.1[...]` on a `Map<K, Tuple>` iter, and `xs.filter(...)[i].1[...]` on a higher-order combinator over `List<Tuple>` all used to fail at codegen with `"str does not support index access"`. `propagateTypeMeta` now has a tuple-sig branch (with an `empty()` guard so wrapper branches `Result<(K, V), E>` / `Option<(K, V)>` / `(K, V)?` keep their outer-name identity through recursion), and the higher-order combinator element guards (`filter` / `map` / `reduce` / `fold` / `any` / `all` / `tap`) accept tuple `StructType` elements in addition to pointer-backed collection elements. (#2273)

## [0.0.28] - 2026-06-14

### Added

- `List<T>` now supports `get(list, index) -> Option<T>` and `get(list, index, default) -> T` overloads, symmetric to the existing `Map<K, V>` `get`. Semantics mirror `list[index]?`: negative indices wrap around, out-of-range (after wrap) returns `None` / the default. Both direct-call (`get(xs, i)`) and UFCS (`xs.get(i)`) forms are supported. (#2116)

### Changed

- `emit` crate: migrated the five string byte-operations (`toUpper` / `toLower` / `trim` / `trimStart` / `trimEnd`, plus the shared `emitIsWhitespace` helper) in `src/codegen_call_string.cpp` from inline `IRBuilder<>::Create*` to eleven new fine-grained scalar/memory primitive boundary ops in `crates/emit` — `ry_emit_alloca` / `_load` / `_store` / `_gep` / `_icmp` / `_and` / `_or` / `_add` / `_sub` / `_select` / `_const_int` (engine in `crates/emit/src/primitive.rs`, `#[no_mangle]` shells in `crates/emit/src/abi/primitive.rs`, CodeGen-side wrappers in `include/ry/codegen.hpp`). The five functions now carry zero `builder_.Create*`. This records the previously-unrecorded architecture decision **[C] = (ii) "boundary move"**: the emission layer owns every `IRBuilder<>::Create*`, superseding the (i) primitive C++ carve-out for the migration direction (recorded additively in `docs/architecture/codegen-layering-plan.md` §"Explicit non-inclusion" and `docs/architecture/llvm-ir-emission-boundary.md`). Installment #1 — the five string ops only; the full-codebase sweep and the migration of shared helpers (`emitStringByteLen`, etc.) are future work. Pure refactor — the emitted LLVM IR is byte-identical (verified by an ASLR-normalized `--emit-llvm-ir` diff over a probe exercising all five ops, each op's markers coverage-gated in the baseline first), the `api.h` C surface adds only the new ops + a `RyICmpPred` enum (`scripts/check-llvm-emit-abi-header.sh` / `check-emit-abi-no-ir.sh` clean), and the new boundary input-validation guards (`ctx`-NULL, NULL type handle, out-of-range icmp predicate, NULL-resolving operand) are locked in `tests/test_emit_abi_guards.cpp`. (#2072)
- `emit` crate: migrated the numeric reduce builtins `sum` / `min` / `max` from inline `builder_.Create*` IR in `src/codegen_call_higher_order.cpp` into the `emit` crate — the ARC-independent "safest batch" (#2092, following the #2072 string-op pilot). Four `#[no_mangle]` externs in `crates/emit/src/abi/reduce.rs` are resolve / intern shells over the IR-emission bodies, which became `impl EmitCtx` methods in the abi-independent core-role `crates/emit/src/reduce.rs`. The op set splits by shape: the **list forms** (`ry_emit_reduce_sum_list`, `ry_emit_reduce_minmax_list_loop`) emit a whole accumulate loop; the **variadic forms** (`ry_emit_reduce_sum_step`, `ry_emit_reduce_minmax_step`) expose one fold step the C++ loop drives per argument — a single "fold the pre-evaluated array" op would bunch the operand loads ahead of the arithmetic and break byte-exactness, so the per-step shape keeps each load interleaved with its add / compare-select exactly as the inline C++ emitted it. `minmax_list_loop` is deliberately partial: the empty-list guard + `emitRuntimeError` stay on the C++ side because `emitRuntimeError` builds an ARC string global (`cachedGlobalString`), which is out of scope for this ARC-free batch; the C++ side keeps `loadListHeader` + the empty-check, positions the builder at `mm.ok`, and the Rust op emits only the seed + loop. Pure refactor — the emitted LLVM IR is byte-identical (verified by an ASLR-normalized `--emit-llvm-ir` diff over a coverage-gated probe set exercising every shape: sum list i64/f64/i8, sum variadic i64/f64/i8, min/max list i64/f64 with the empty-list error block, min/max variadic i64/f64), the `api.h` C surface gains only the four new declarations (`scripts/check-llvm-emit-abi-header.sh` / `scripts/check-emit-abi-no-ir.sh` clean), and `ry_tests` (incl. 12 new `tests/test_emit_abi_guards.cpp` boundary-input cases) + `ry test -p` stay green. (#2092)
- `emit` crate: migrated the malloc+memcpy copy-generation of five collection ops that build a fresh List buffer from a source — `emitCollOp_appended` / `emitCollOp_take_impl` (`src/codegen_call_collection.cpp`), `keys` / `values` (`src/codegen_call.cpp`), and `emitListConcat` (`src/codegen_expr.cpp`) — from inline `builder_.Create*` IR into the `emit` crate (#2093, following the #2092 reduce shape). Three `#[no_mangle]` externs in `crates/emit/src/abi/collection.rs` (`ry_emit_list_copy_full` / `_appended` / `_concat`) are resolve / intern shells over the IR-emission bodies, which became `impl EmitCtx` methods in the abi-independent core-role `crates/emit/src/collection.rs`. No `lowered::` op struct is introduced — the call sites already hold the C++-loaded length/data Values, so the C++-side `emission::emitListCopyFull` / `emitListAppendedCopy` / `emitListConcatCopy` wrappers just intern → call → resolve. By design the header load (`loadListHeader` / `loadMapHeader`) stays C++-side: it sidesteps the interleaved-vs-grouped header-load order trap and keeps the length / clamped-count live for the C++-side ARC retain loop + `storeListHeaderFields`. The boundary is the size-mul + malloc + memcpy chain only; `emitArcAllocCollectionHeader` (emitted just before the boundary call), the per-element ARC retain loop, the appended-element tail store, and metadata propagation all stay C++-side, so no instruction reorder is needed. `ry_emit_list_copy_full` shares `keys` / `values` / `take` behind a `RyListCopyKind` selector that picks each call site's SSA name pair (`keys_ds`/`keys_nd` · `vals_ds`/`vals_nd` · `tk_dsize`/`tk_data`) so the migration stays byte-identical rather than consolidating to one renamed buffer; `ry_emit_list_concat` is the only entry needing `elem_ty` (for the mid-buffer `cat_rhs_dst` GEP). Pure refactor — the emitted LLVM IR is byte-identical (verified by an ASLR-normalized `--emit-llvm-ir` diff over a coverage-gated probe running all five ops on runtime-built ARC-managed-element collections — `List<List<int>>`, `Map<str, List<int>>` — each op's marker grep-confirmed in the baseline first), the `api.h` C surface gains only the three new declarations + a `RyListCopyKind` enum (`scripts/check-llvm-emit-abi-header.sh` / `scripts/check-emit-abi-no-ir.sh` clean), and `ry_tests` (incl. 8 new `tests/test_emit_abi_guards.cpp` boundary-input cases) + `ry test -p` stay green. (#2093)
- `emit` crate: migrated the **Option / branching-return** batch of eight collection and string ops — `first` / `last` (`src/codegen_call.cpp`), `emitCollOp_pop` and `emitCollOp_get` 2-arg + 3-arg (`src/codegen_call_collection.cpp`), `emitStrOp_find` (`src/codegen_call_string.cpp`), and the **List try-index** `xs[i]?` + **Map try-index** `m[k]?` arms (`src/codegen_expr_literal.cpp`) — onto the existing `ry_emit_*` primitive vocabulary so the scaffold of each op carries **zero `builder_.Create*`** (#2094, following the #2101 style — option (ii), primitive sequence — rather than the #2093 coarse-grained method). All eight ops share the same shape — success/failure condition compute → empty/OOB/found basic blocks → `buildSomeValue` / `buildNoneValue` (or default value) → merge BB + `createPhi` — so the migration is a uniform 1:1 textual replacement (`CreateICmpEQ/SGE/SLT` → `emitICmpEQ/SGE/SLT`, `CreateStructGEP` / `CreateGEP` / `CreateLoad` / `CreateStore` / `CreateSub` / `CreateOr` / `CreateZExt` → `emitStructGEP` / `emitGEP` / `emitLoad` / `emitStore` / `emitSub` / `emitOr` / `emitZExt`, `CreateCall(runtimeFn, …)` → `emitRuntimeCallDirect(...)`, `ConstantInt::get(...)` → `emitConstInt(...)`). **No new `ry_emit_*` boundary surface, no new `impl EmitCtx` method per op, no new `abi` shell** — the 14 primitives the eight ops need are all already exported by prior batches (#1967 OptionWrap, #1971 ControlFlow, #2072 scalar primitives, #2098 RuntimeCallDirect, #2101 `ry_emit_zext`). The only new C++-side wrapper is `emitICmpSGE` in `src/codegen.cpp` over the existing `ry_emit_icmp` + `RY_ICMP_SGE` (mirroring #2101's `emitICmpNE` pattern — same op-code dispatch over the same boundary entry). `pop`'s destructive `CreateStore(lastIdx, lf.lenPtr)` crosses too (the `lenPtr` value still comes from `loadListHeader` C++-side); the Map vals field-3 GEP stays a literal `3` on the C++ side because `tests/test_header_layout.cpp` already pins it cross-language against `crates/emit/src/core.rs:header_fields(HeaderKind::Map)`. **Carve-outs**: `loadListHeader` / `loadMapHeader` (header-load capability), `emitNegativeIndexWrap` (index-wrap capability, list try-index), `emitStringByteLen` (StringHeader-load capability, `find`), `emitComparisonOp` (compare capability, reached via `emitMapKeyLookup`'s linear-scan path), and `emitCowCheck` (CoW pre-emission capability, `pop`) stay above the migrated scaffold — each is a separate capability and a follow-on. Pure refactor — the emitted LLVM IR is byte-for-byte identical (verified by an ASLR-normalized `--emit-llvm-ir` diff over per-op probes that exercise both the Some and None arm of each op with non-constant operands so `ICmp` / `GEP` / `Select` do not fold to `ConstantInt`, with each op's markers — `first.empty` / `pop_last_idx` / `find_byte_off` / `find_char_idx` / `trylist.oob` / `tryidx_ext` / `get2_vals_field` / `get_val_ptr` / `trymap_val_ptr` — grep-confirmed in the baseline first), `scripts/check-emit-abi-no-ir.sh` / `scripts/check-llvm-emit-abi-header.sh` / `cargo fmt --check` / `cargo clippy -- -D warnings` clean, and `ry_tests` (2631) + `ry test -p` (204 files) stay green. (#2094)
- The collection linear-generation / linear-operation ops `listRemove` /
  `distinct` / `flatten` / list `reverse` / `items` / `enumerate` / `zip`
  now emit their LLVM IR from the Rust `crates/emit/` cdylib via the new
  `ry_emit_list_remove` / `_list_distinct` / `_list_flatten` /
  `_list_reverse` / `_map_items` / `_list_enumerate` / `_list_zip`
  boundary entry points. List header GEP positions (LEN/CAP/DATA) and
  Map header positions (LEN/CAP/KEYS/VALS/BUCKET_COUNT/BUCKETS) are
  centralized in `core::header_fields` as named constants and pinned by
  `tests/test_header_layout.cpp` against C++'s canonical struct so a
  constant-value drift fails the build. Pure refactor: the generated
  IR is byte-identical after ASLR normalization, no behavioral change.
  The per-element ARC retain callbacks for enumerate / zip / items
  cross the boundary via the new `RyRetainFn` trampoline + stack-struct
  pattern (mirroring `ry_emit_result_branch`'s re-entrant closure
  discipline from #2069). (#2095)
- `emit` crate: migrated the three string-build ops (`str + str` concat in `emitArithmeticOp`'s `+` string branch, `emitStringRepeat` for `str * int`, `emitStrOp_join` for `List<str>.join(sep)`) plus the shared `emitStringByteLen` helper from inline `IRBuilder<>::Create*` to the existing `ry_emit_*` primitive vocabulary, extending the surface with two new fine-grained primitives — `ry_emit_mul` (`LLVMBuildMul`) and `ry_emit_sdiv` (`LLVMBuildSDiv`) — added in the same #2072 4-layer pattern (engine in `crates/emit/src/primitive.rs`, `#[no_mangle]` shells in `crates/emit/src/abi/primitive.rs`, C declaration in `include/ry/llvm_emit/api.h`, CodeGen-side wrappers `emitMul` / `emitSDiv` in `include/ry/codegen.hpp` / `src/codegen.cpp`). The four functions now carry zero `builder_.Create*`. `emitStringByteLen` was previously listed as a future-work carve-out at `docs/architecture/llvm-ir-emission-boundary.md` L116-128 (the #2072 "Stage 2-C complete" note); #2096 closes it in the same batch as the three op functions to complete the string-build cluster's `builder_.Create*` = 0 AC. Pure refactor — the emitted LLVM IR is byte-identical (verified by three new ASLR-normalized `--emit-llvm-ir` parity goldens at `tests/filecheck/str_concat_parity.ry` / `str_repeat_parity.ry` / `str_join_parity.ry`, each pinning the overflow-guard markers `cat_ovf` / `would_overflow` / `safe_sep_count` to address the [D] coverage-gate gap left over from #2072 — `emitRuntimeError` calls `_Exit(1)` and cannot be exercised from a runtime spec). The new boundary input-validation guards (`ctx`-NULL for `ry_emit_mul` / `ry_emit_sdiv`, shape-shared with `ry_emit_add` / `ry_emit_sub` for resolve / name guards) are locked in `tests/test_emit_abi_guards.cpp`. `scripts/check-emit-abi-no-ir.sh` stays clean; `ry_tests` (2672) and `ry test -p` (3392 / 205 spec files) stay green; ASan+UBSan and TSan pass with zero findings. The `InterpolatedStringExpr` half of concat stays C++-side because it routes through `valueToString` (`tostring`), gated on the value-metadata-crossing + variadic-runtime-call capabilities settled by #2100 — a follow-on sweep migrates it once a generic-typed `tostring` boundary lands. (#2096)
- `emit` crate: the shared `emitCheckedFPToInt` helper (`src/codegen_call_user.cpp`) — the BB scaffold + range check + runtime-error exit + FPToSI / FPToUI sequence that every `int(<float>)` / `<float> as <int>` cast emits, shared by the 9 int-target cast cases in `src/codegen_expr_cast.cpp` and the int branch of `coerceToLowLevelType` (10 callsites total) — now crosses the boundary as a **coarse op** (`ry_emit_checked_fp_to_int`), following the `ry_emit_bounds_check` (#1996) pilot pattern. The op shape ("BB scaffold + range check + runtime-error exit + happy-path cast") is identical to `bounds_check`, so #2097 settles it as a single boundary entry rather than a generic-primitive batch; the 10 callsites are *implicitly migrated* the moment the C++ wrapper switches to the boundary call. **No new generic primitives.** The C++ wrapper computes the message string (typeName + siteLabel), the `fptoi_err_counter_++`-stamped global-name hint, the bit width, and the signedness; the engine emits everything from FPExt(f32→f64) through the unordered FCmpULT/UGE range check, CondBr to fresh failBB / okBB, runtime-error exit in failBB, and FPToSI / FPToUI on the original value in okBB. **`emitRuntimeError` shape ≠ `bounds_error` shape (the load-bearing trap)**: C++ `cachedGlobalString` → `buildArcGlobal` (`src/codegen.cpp`) builds a StringHeader-prefixed `{ i64 ARC_IMMORTAL, i64 0, i64 byte_len, [N+1 x i8] data }` global with a `.arc` suffix and passes a ConstantExpr in-bounds GEP `[0, 3, 0]` into the data payload to fprintf, while `crates/emit/src/bounds.rs::bounds_error` builds a plain `[N+1 x i8]` global via `get_or_create_msg_global`; the two also order stderr/stdout loads differently (C++ `emitRuntimeError` interleaves stdout's load AFTER fprintf, Rust `bounds_error` loads them adjacently before). Reusing `bounds_error` for an `emitRuntimeError`-shaped exit would silently break bit-exact parity on both axes, so #2097 introduces a dedicated `get_or_create_arc_msg_global` (`crates/emit/src/core.rs`) mirroring `buildArcGlobal` byte-for-byte, plus a `cast.rs`-local `runtime_error_with_value_arg` reproducing the C++ instruction order. Dedup caches stay independent (`bounds_msg_cache` for plain, `arc_msg_cache` for str-handle); both key on message bytes. The lesson is captured in `.claude/rules/codegen-llvm-ir-conventions.md` so any future `emitRuntimeError`-flavored migration does not re-discover it. **Variadic fprintf carve-out**: the fprintf call uses an inline variadic `FunctionType` (`isVarArg=1`) built with `LLVMBuildCall2`, NOT routed through `ry_emit_runtime_call` — same shape as `bounds_error`'s fprintf, the #2100 carve-out. Scope (settle ≠ unlock): `emitCheckedFPToInt` only. The sibling `emitRuntimeError` callers in `src/codegen_call_user.cpp` (`emitIntZeroDivGuard` / `emitIntDivOverflowGuard`) and the `?` operator / saturating-arithmetic exits stay C++-side, available to the follow-on sweep under the same discipline; the integer-only / float-only / `f64 → f32` carve-out cases in `codegen_expr_cast.cpp` (~40 straight `sext` / `zext` / `trunc` / `sitofp` / `fptrunc` 1:1s) are intentionally C++. Recorded additively in `docs/architecture/llvm-ir-emission-boundary.md`. Pure refactor — the emitted LLVM IR is byte-identical (verified by an ASLR-normalized `--emit-llvm-ir` diff over a probe exercising signed (`cast_i_lo` / `cast_i_hi` / `cast_i_invalid` / `cast_i.fail` / `cast_i.ok` / `fptosi`) + unsigned (`cast_u32_lo` / `cast_u32_hi` / `fptoui`) + f32 source (`cast_i_f64ext`) classes using non-constant operands), pinned in `tests/filecheck/cast_fp_to_int.ry`. Boundary input-validation guards (`ctx`-NULL, NULL-resolving val, `target_width <= 0`) are locked in `tests/test_emit_abi_guards.cpp`. (#2097)
- `emit` crate: the LLVM IR emission ABI (`crates/emit`) acquired the function-**creation** capability — until now every `ry_emit_*` op emitted instructions / basic blocks *inside* a function the C++ side already had open (`ry_emit_create_basic_block` took an explicit parent handle), with `llvm::Function::Create` itself confined to C++ (33 sites: iterator-next / ADT-tostring / closure / ARC & GC destructors / thunks). Five new generic, reusable boundary primitives close that gap: `ry_emit_create_function` (the `Function::Create` equivalent — `LLVMAddFunction` + linkage, returning a raw `RyFunctionRef`; + a `RyLinkage` enum), `ry_emit_get_param`, `ry_emit_struct_gep` (compile-time field-index GEP, distinct from the runtime-index `ry_emit_gep`), `ry_emit_call_indirect` (call through a loaded fn-pointer value, distinct from the name-keyed `ry_emit_runtime_call`), and `ry_emit_ret` (engine methods in the new `crates/emit/src/function.rs` + `primitive.rs` / `control_flow.rs`, `#[no_mangle]` shells in `crates/emit/src/abi/function.rs` + `abi/primitive.rs` / `abi/control_flow.rs`, CodeGen-side wrappers `emitCreateFunction` / `emitGetParam` / `emitStructGEP` / `emitCallIndirect` / `emitRet` in `include/ry/codegen.hpp`). This is the architecture-decision pilot **[A] = (ii) "boundary move" with generic primitives** (over a coarse per-op `ry_emit_iter_take_next`, which would leak Ry-level iterator semantics into emission and need 30+ bespoke engine functions): a *capability* decision orthogonal to #2072's *style* decision, recorded additively in `docs/architecture/codegen-layering-plan.md` and `docs/architecture/llvm-ir-emission-boundary.md`. Proven by moving the `take` iterator's `next`-function construction (`src/codegen_call_iterator.cpp`) across the boundary so its body carries zero `builder_.Create*`. Settle ≠ unlock — the capability only; the iterator header-alloc `ValueMetadata` crossing (#2100), closure / `FnTypeInfo` crossing (#2099), and the remaining 32 `Function::Create` sites are follow-on work. Pure refactor — the emitted LLVM IR is byte-identical (verified by an ASLR-normalized `--emit-llvm-ir` diff over a `take(iter([…]), n)` probe, the next-fn markers coverage-gated in the baseline first), the `api.h` C surface adds only the new ops + a `RyLinkage` enum (`scripts/check-llvm-emit-abi-header.sh` / `check-emit-abi-no-ir.sh` clean), and the boundary input-validation guards (`ctx`-NULL, NULL type / fn handle, unknown linkage, NULL-resolving callee, NULL arg array) are locked in `tests/test_emit_abi_guards.cpp`. (#2098)
- `emit` crate: the closure / `FnTypeInfo` call now crosses the LLVM IR emission boundary (`crates/emit`) through the existing generic primitives — **without** a `FnTypeInfo` descriptor. This is the architecture-decision pilot **[A2] = (ii) "boundary move"**, the *surface complement* of #2098 (same capability — the emission layer building and calling functions — a different surface): the settle question was whether a closure must cross via a coarse `RyFnCallDesc` (calling convention + capture flag) feeding a per-op `ry_emit_iter_filter_next` / `map_next`, or whether the call is expressible in the generic primitives. The latter wins, for the same reason #2098 rejected `ry_emit_iter_take_next`: `emitLambdaCall`'s three calling-convention layouts — uniform `{thunk, env}`, plain (no-capture), captured `{fn_ptr, caps…}` — are each just `emitStructGEP` + `emitLoad` + `emitCallIndirect` (the loaded fn-pointer call from #2098), so the `FnTypeInfo` is consumed entirely C++-side to *select* which primitives to emit and never crosses the boundary; a `RyFnCallDesc` would leak Ry closure semantics into emission for zero capability gain. One new generic primitive — `ry_emit_extract_value` (`LLVMBuildExtractValue`, an in-register aggregate read, distinct from `ry_emit_struct_gep`'s pointer-addressed field GEP; engine method `build_extract_value` in `crates/emit/src/primitive.rs`, `#[no_mangle]` shell in `crates/emit/src/abi/primitive.rs`, CodeGen-side wrapper `emitExtractValue` in `include/ry/codegen.hpp`) — covers the `Option` destructure in the `filter` / `map` next-fn (`buildSomeValue` / `buildNoneValue` already route through `ry_emit_option_wrap_*`). Proven by migrating `emitLambdaCall` (all three layouts, both void and non-void returns) **and** the `filter` / `map` iterator `next`-function bodies (`src/codegen_call_iterator.cpp`) across the boundary so both carry zero `builder_.Create*`; `coerceCallArgs` (the `wrapInAny` / `unwrapFromAny` / subtype-coercion argument prep) stays lowering-side as a *semantic* decision. Settle ≠ unlock — the outer iterator state-setup (`malloc` + the closure-state stores + header alloc, the `ValueMetadata` crossing of #2100) stays C++ at #2098's exact line, and the remaining `emitLambdaCall` next-fn migrations and the dense `list` / `set` / `map` iterators are follow-on work. Recorded additively in `docs/architecture/codegen-layering-plan.md` and `docs/architecture/llvm-ir-emission-boundary.md`. Pure refactor — the emitted LLVM IR is byte-identical (verified by an ASLR-normalized `--emit-llvm-ir` diff over a probe forcing every layout — uniform / plain / captured, both void and non-void — plus a 2-argument lambda, iterator `filter` / `map` over int and ptr elements, and List higher-order call sites covering the shared-helper blast radius, each marker coverage-gated in the baseline first), the `api.h` C surface adds only `ry_emit_extract_value` (`scripts/check-llvm-emit-abi-header.sh` / `check-emit-abi-no-ir.sh` clean), and the boundary input-validation guards (`ctx`-NULL, NULL-resolving aggregate) are locked in `tests/test_emit_abi_guards.cpp`. (#2099)
- `emit` crate: `ValueMetadata` type-meta crossing and `switch` construction are now settled as architecture-decision pilot **[B] = (ii) "boundary move"**, proven by migrating `str(List<enum>)`'s enum variant-name lowering (`src/codegen_tostring.cpp`) so the enum-switch branch and the List-loop scaffold carry zero `builder_.Create*`. The settle had two orthogonal capabilities: (1) whether the per-element type metadata (`enum_value_type` etc., keyed by `llvm::Value*` in `value_metadata_`, which cannot cross the boundary) must be carried across via a descriptor, and (2) whether `CreateSwitch` + `addCase` must be abi'd. **Both resolve to (ii)**: the type meta does **not** cross — the migrated List-loop element load round-trips the boundary (`intern → ry_emit_load → resolve`), and the returned SSA value still serves as a `value_metadata_` key C++-side where `propagateElemMeta` re-stamps the enum type, so `getMeta` and the recursive `valueToString` stay C++ (the same "type-metadata copy stays on the codegen side" rule as `ry_emit_list_slice`); and the switch crosses through new **generic** primitives, not a coarse `ry_emit_tostring_list` descriptor carrying the case set. Three new generic primitives: `ry_emit_create_switch` (`LLVMBuildSwitch`, returns the opaque `RySwitchRef` like `create_basic_block`'s `RyBasicBlockRef` — a switch is mutated post-creation so it crosses as a handle, never interned) + `ry_emit_switch_add_case` (`LLVMAddCase`) for the variant-name switch, and `ry_emit_array_gep` (a two-index `{i64 0, idx}` GEP into the `[N x ptr]` name-array global, distinct from the single-index `ry_emit_gep` and the field-index `ry_emit_struct_gep`). The enum's variant values / name-array index are driven by the C++-side `EnumInfo` and never cross the boundary; the `FnTypeInfo` discipline of #2099 generalizes — descriptors are unnecessary. **Carve-out**: the sprint-buffer mechanism (the shared `emitSprintBegin` / `emitSprintEnd` plus the **variadic** `__ry_sprint_printf` call) stays C++ — `ry_emit_runtime_call` is non-variadic by contract, and varargs crossing is an orthogonal capability shared by every tostring path, deferred to a follow-on. Scope (settle ≠ unlock): single-level flat non-ADT enums only — deep ADT tostring (`getOrCreateADTToStringFn` via `Function::Create`) is #2098's function-creation domain, the non-explicit-value direct-GEP enum branch is a separate sub-capability left untouched, and Map / Set tostring + the other metadata-driven switch sites are the follow-on sweep using the same discipline. Recorded additively in `docs/architecture/codegen-layering-plan.md` and `docs/architecture/llvm-ir-emission-boundary.md`. Pure refactor — the emitted LLVM IR is byte-identical (verified by an ASLR-normalized `--emit-llvm-ir` diff over a `str(List<HttpStatus>)` probe with the switch path — `switch i64` / `vts.enum.merge` / per-variant case BBs — coverage-gated in the baseline first, since a non-explicit-value enum would fall to the direct-GEP branch and never exercise the switch), the `api.h` C surface adds only the three new entries (`scripts/check-llvm-emit-abi-header.sh` clean; `scripts/check-emit-abi-no-ir.sh` gains `LLVMAddCase` to its forbidden set and stays green — the `switch_add_case` engine method lives in `core`, not the abi shell), and the boundary input-validation guards (`ctx`-NULL, NULL `default_bb` / `array_ty`, NULL-resolving operand) are locked in `tests/test_emit_abi_guards.cpp`. (#2100)
- `emit` crate: hash-table **lookup** is now settled as architecture-decision pilot **[C] = (ii) "boundary move"**, proven by migrating the set `isSubset` representative op (`emitSubsetCheck` → `emitSetElementLookup` → `emitHashTableLookup`) so the lookup scaffold + the `__ry_ht_find_*` call carry zero `builder_.Create*`. The settle question was whether the hash-table lookup must cross via a coarse `RyHashTableDesc` (header_ty / key_ty / key_type_name / elem_size / layout_kind) op (option (i)) or is expressible in the existing generic primitives (option (ii)). **(ii) is chosen**, on the same axis as #2098 / #2099 / #2100: the unified `emitHashTableLookup` is `struct_gep` ×4 + `load` ×4 (bucket-count / buckets / len / keys header reads) + `sub` (bucket mask), plus the **fixed-arity** 5-operand `__ry_ht_find_str` / `_f64` / `_i64` call routed through the existing non-variadic `ry_emit_runtime_call` (`emitRuntimeCallDirect`) — no descriptor. `keyTy` stays C++-side selecting *which* `__ry_ht_find_*` symbol and key-arg type to emit, and never crosses. The composite-element linear-scan path (`Set<record>` / `Set<List<…>>` / `Set<any>`) is the same generic-primitive scaffold; its element comparison (`emitComparisonOp`) stays C++-side — the same "type-metadata / dispatch stays on the codegen side" rule as #2100 — and only the fixed-arity `__ry_any_eq` call crosses. **One new generic primitive**: `ry_emit_zext` (`LLVMBuildZExt`, the conditional i1→i64 key widening in `emitHashTableLookup` for `Set<bool>` / `Map<bool, V>`) — `LLVMBuildZExt` was already used internally in `crates/emit` (`bounds.rs` / `any.rs`), so the boundary entry is a #2072 scalar-vocabulary completion, not a new capability (a `CodeGen`-side `emitICmpNE` wrapper over the existing `ry_emit_icmp` was also added, no new boundary entry). **Carve-out**: the element comparison (`emitComparisonOp` dispatch) and `propagateTypeMeta` re-stamp stay C++ — type meta does not cross (the #2100 precedent) — and `loadSetHeader` / `loadMapHeader`'s header read stays unmigrated as a follow-on (header-load capability, like `loadListHeader` in #2093). Scope (settle ≠ unlock): hash + composite-element **lookup** only (`isSubset` / `isSuperset`, plus the shared-helper blast radius — set literal / `add` / `remove` / `in` / `contains` / `union` / `intersection` / `difference` and `Map` `get` / `has_key` / index, whose lookup portion is now boundary-emitted). It does **not** unlock insert+rehash (`emitBucketInsertAndRehashCheck`'s rehash-BB generation / `coerceHashKey`), `emitMapKeyLookup`'s linear-scan scaffold, or the set/map op bodies on top of lookup; the pending collection-hash work is folded into this decision (no separate issue), but the representative implementation reaches only the lookup. Recorded additively in `docs/architecture/codegen-layering-plan.md` and `docs/architecture/llvm-ir-emission-boundary.md`. Pure refactor — the emitted LLVM IR is byte-identical (verified by an ASLR-normalized `--emit-llvm-ir` diff over a probe exercising both the hash path — `Set<int>` / `Set<str>` / `Set<float>` / `Set<bool>` `isSubset`, `isSuperset`, `Map<str, V>` index, markers `ht_lookup_idx` / `key_ext` — and the linear-scan path — `Set<any>` / `Set<record>`, markers `slin.any.eq` / `slin_cp` / `slin_cand` — both coverage-gated in the baseline first), the `api.h` C surface adds only `ry_emit_zext` (`scripts/check-llvm-emit-abi-header.sh` / `scripts/check-emit-abi-no-ir.sh` clean), and the boundary input-validation guards (`ctx`-NULL, NULL `dest_ty`, NULL-resolving val) are locked in `tests/test_emit_abi_guards.cpp`. (#2101)
- `emit` crate: LLVM intrinsic emission (declaration acquisition + call + `{T, i1}` aggregate decomposition) is now settled as architecture-decision pilot **[D] = (ii) "boundary move"**, proven by migrating the `emitIntOverflowCheck` representative op (`src/codegen_arith.cpp:130-172`) so the non-constant path carries zero `builder_.Create*` and zero `Intrinsic::getOrInsertDeclaration`. The settle question was whether the intrinsic call must cross via a coarse `RyOverflowIntrinsicDesc` (intrinsic kind + signed/unsigned + operand type + `overflow_err_counter_` label scheme) op (option (i)) or is expressible in generic primitives (option (ii)). **(ii) is chosen**, on the same axis as #2098 / #2099 / #2100 / #2101: a `RyOverflowIntrinsicDesc` would leak Ry arithmetic semantics (signed/unsigned × add/sub/mul × panic/Result/saturating) into emission for zero capability gain. Within (ii), the further sub-decision was a *single* `ry_emit_intrinsic_call` (acquires declaration + emits call in one engine call) vs. a *2-step* `ry_emit_get_intrinsic_decl` + reuse-`ry_emit_call_indirect` split — **the single primitive is chosen**: the 2-step split would force the C++ caller to call `getFunctionType()` on the returned `RyFunctionRef` to feed `ry_emit_call_indirect`'s `fn_ty` parameter, residual `llvm::Function` manipulation on the codegen side, exactly the thing this pilot exists to move across. The single primitive derives the FunctionType engine-side (`LLVMIntrinsicGetType`) so nothing about the returned `llvm::Function*` leaks back to C++ — complete boundary ownership. **One new generic primitive**: `ry_emit_intrinsic_call` (the engine method `build_intrinsic_call` makes three LLVM C-API calls — `LLVMGetIntrinsicDeclaration` + `LLVMIntrinsicGetType` + `LLVMBuildCall2` — so it lives in `crates/emit/src/function.rs` next to `call_indirect`, not `primitive.rs`, whose doc contracts 1:1 `LLVMBuild*` wrappers). Aggregate `{T, i1}` extraction reuses the existing `ry_emit_extract_value` (#2099). **Carve-out**: the constant-fold path (`llvm::APInt::{sadd,ssub,smul,umul}_ov` compile-time evaluation, lines 134–153) stays C++ — APInt operations do not lower to IR. `intrinsic_id` is `llvm::Intrinsic::ID` cast to `uint32_t`; the `llvm-sys` `force-dynamic` shared-libLLVM design makes the numeric value identical across the process so no cross-version drift arises (the api.h header lint + `check-emit-abi-no-ir.sh` stay clean). Scope (settle ≠ unlock): `emitIntOverflowCheck` only — its 10 call sites (signed × 5 in `codegen_expr.cpp`, unsigned × 5 in `codegen_call_higher_order.cpp` collection-alloc) are *implicitly migrated* because they share the helper body. It does **not** unlock the sibling functions `emitCheckedArithmetic` (its own inline `getOrInsertDeclaration + CreateCall + 2 × ExtractValue` for `checkedAdd/Sub/Mul`), `emitSaturatingArithmetic` mul path (same inline shape for `saturatingMul`), or the `*_sat` scalar paths (`sadd_sat` / `usub_sat` etc.) — those migrate in the follow-on sweep using this discipline. Recorded additively in `docs/architecture/codegen-layering-plan.md` and `docs/architecture/llvm-ir-emission-boundary.md`. Pure refactor — the emitted LLVM IR is byte-identical (verified by an ASLR-normalized `--emit-llvm-ir` diff over a probe exercising both the signed path — `sadd_with_overflow` / `ssub_with_overflow` / `smul_with_overflow` markers — and the unsigned path — `umul_with_overflow` collection-alloc with `map_data_size_ov` / `filter_data_size_ov` markers — using non-constant operands so the constant-fold path does not silently swallow the probe and produce a false-pass empty diff), and the boundary input-validation guards (`ctx`-NULL, NULL overload array with count > 0, per-element NULL type, NULL arg array with count > 0, NULL-resolving arg) are locked in `tests/test_emit_abi_guards.cpp`. (#2102)
- `emit` crate: reorganised the flat module layout into a layered tree — `context.rs` for layer-neutral state / handle wrappers / enum selectors / layout constants / pure-data `header_fields`; `primitive/**.rs` for LLVM 1:1 emission with no Ry semantics (type constructors, name builders, module-symbol lookup, plain string globals, libc emitters, inline runtime-error, scalar / memory ops, function creation, indirect call, intrinsic call, control flow, generic runtime calls); `composite/**.rs` for Ry-semantic emission with layout / ABI knowledge (ARC, bounds, checked FP→int, Option / Result, Any wrap/unwrap, collection mutations, CoW ensure-unique, reduce, shared header struct construction). The dependency direction `abi → composite → primitive → context` is enforced by two new lint scripts: `scripts/check-emit-composite-no-primitive.sh` (forbids `primitive → composite` and `context → {abi, primitive, composite}` import paths) and `scripts/check-emit-llvm-ir-gen-concentration.sh` (freezes the current set of composite files calling `llvm_sys::core` directly as an explicit ALLOWLIST and blocks new direct callers — issue AC #5 / #7's "残存箇所と許可理由を明示し、新規追加を防止する gate" requirement). The `abi` layer's content is unchanged; only its import paths are rewritten from `crate::core::*` to `crate::context::*`. `core.rs` is dissolved into the three sub-layers. The public C boundary (`include/ry/llvm_emit/api.h`) is unchanged. Pure architectural refactor — emitted LLVM IR is byte-identical (verified by an ASLR-normalized `--emit-llvm-ir` diff over a multi-coverage probe), `ry_tests` and `ry test -p` stay green, and `scripts/check-llvm-emit-abi-header.sh` / `check-emit-abi-no-ir.sh` continue to pass. Architecture documentation updated in `docs/architecture/codegen-layering-plan.md` (new "Composite and primitive emission sub-layers" section + transitional carve-out table), `docs/architecture/llvm-ir-emission-boundary.md` (new "Composite / primitive sub-layer split" section), `docs/architecture/codegen-terminology.md` (composite / primitive emission entries in the Layers table), and `.claude/rules/codegen-llvm-ir-conventions.md` (path references updated to the new sub-layer locations). (#2109)

### Fixed

- `ry fmt` now preserves regex literals (`/pattern/`) instead of replacing them with `/* unknown expr */`. The formatter's expression dispatch was missing a `RegexExpr` branch and silently fell through to the unknown-expression placeholder, so any source containing a regex literal formatted to a file that no longer ran. The fix emits the pattern verbatim (the lexer already preserves regex backslashes such as `\d` / `\w` / `\/` byte for byte) and reverses the lexer's only lossy translation by re-encoding embedded NUL bytes as `
- `ry fmt` now preserves a parseable spelling when the postfix `?` (Option / Result propagation) follows an expression that already ends in `?` — most commonly the Option-returning safe-index form `xs[i]? ?` and nested `safeGet(...)? ?` chains. The formatter previously concatenated to `xs[i]??`, which the lexer fuses greedily into the `??` (null-coalescing) token, causing `ry fmt --check` to fail with `formatted output failed to re-parse` and skip the file. The fix inserts a space between the two `?` tokens in `formatExprInner`'s `ErrorPropagateExpr` branch whenever the inner formatted text ends with `?`. (#2114)
- Parser: multiline UFCS chains with continuation lines starting with `.` (e.g. `xs
    .iter()
    .toList()`) — the canonical iterator-pipeline idiom documented in `docs/reference/collections.md` — now parse correctly. Previously `parsePostfixContinuation` (`src/parser/parser_expr.cpp`) exited on the first `Newline` and the chain was rejected with "unexpected token '.'". The fix speculatively skips `Newline`/`Indent`/`Dedent` looking for a continuation `.` (saved via `lex_.saveState()` and rolled back if no `.` follows) and tracks chain-internal `Indent`s in a counter so the matching trailing `Dedent`s are drained before returning — without that drain, the surrounding `parseBlock`/`parseProgram` would see a stray `Dedent` and either terminate the block prematurely (inside a fn body) or hit "unexpected token" (at top level). The continuation relaxation applies to `.IDENT` only — `.INTEGER` (tuple index) is not supported on continuation lines because the lexer (`lexer.cpp:452-458`) tokenizes `.<digit>` after `Newline` as a Float literal; `[` / `?` on continuation lines are excluded because they would conflict with a fresh list literal / unary statement. The companion tree-sitter grammar update is deferred to a follow-up issue (adding `optional($._newline)` to `field_access` conflicts with the statement-terminator `_newline` elsewhere and would require a more sophisticated GLR-state structure than is in scope for #2115). (#2115)
- List index access (`get(list, idx[, default])`, `xs[i]`, `xs[i]?`) now rejects non-`int` indices at compile time. Previously a `bool` index was silently zero-extended to `i64` (so `xs.get(true)` returned `Some(xs[1])` instead of erroring), and a `float` index would reach `emitNegativeIndexWrap` / `lowerBoundsCheck` / GEP with the wrong LLVM type — either misbehaving or failing LLVM verify. The fix mirrors the existing `insert() index must be int` / `removeAt() index must be int` pattern: a single `codegenError("get() index must be int")` guard at the top of the `emitCollOp_get` List branch (`src/codegen_call_collection.cpp`) covers both 2-arg and 3-arg overloads, and a single `codegenError("list index must be int")` guard at the top of the `IndexExpr` List branch (`src/codegen_expr_literal.cpp`) covers both `xs[i]` and `xs[i]?`. The function signatures in `share/std/list.ry` are unchanged (`index: int`). Map key type checking (`emitMapKeyLookup`) was already strict and is out of scope. (#2124)
- `get(list, index, default)` and `get(map, key, default)` now evaluate the `default` expression only when the index is out-of-bounds or the key is not found. Previously the default expression was always evaluated, so passing a function call as the default would invoke it (and run its side effects) even on the in-bounds / key-found path. (#2132)
- Top-level `fn get(...)` is now rejected at compile time with `name is reserved for a built-in function`. Previously the collection `get` builtin (#2116) silently shadowed any user-defined `fn get(xs: List<int>, i: int) -> int` (or any 2- / 3-arg signature whose first argument was a `List` or `Map`), turning the user body into dead code with no diagnostic. The single-int form used to compile via fall-through; reservation makes the rejection uniform across all signatures, matching the existing `iter` / `pop` precedent per the `kReservedBuiltinFunctionNames` empirical-maintenance rule. (#2133)
- Parser: multiline UFCS chains in sub-expression positions (`if` / `while` condition, `return` expression, assignment / `let` RHS, call argument) no longer corrupt the surrounding block. Previously the chain absorbed an `Indent` to allow `.method()` continuation on the next line, but when the chain ended on a non-Newline/Dedent token (`>`, `:`, `)`, `,`) the tail Dedent drain in `parsePostfixContinuation` silently discarded its unmatched count — `parseBlock` then saw no Indent for the body (`if xs
    .count() > 0:
    print(1)` → `expected indented block`) or a stray Dedent surfaced at the statement / program boundary. The fix promotes the per-call `chainIndents` counter to a Parser member (`chain_pending_dedents_`), keeps the residue on stall, and accommodates it at `parseBlock` (drain stream Dedents then treat any remaining count as an implicit Indent), `parseProgram`, and block body loops. The four speculative parse sites that may transitively invoke `parsePostfixContinuation` (lambda dispatch, case-arm tail-vs-stmt, generic `Ident[T](args)` call, generic `Ident<T>::Variant(args)` enum) save and restore the member on their lexer-restore paths so a chain absorbed inside a speculative body cannot leak across the fallback. (#2136)
- Parser: a `#` comment-only line inserted between hops of a multiline UFCS chain (e.g. `xs
    # skip empty
    .iter()
    .toList()`) no longer breaks the chain with `unexpected token`. The lexer now suppresses the trailing `Newline` of comment-only lines so the chain's drain loop sees the same token stream as the comment-free form; blank-line separators are still rejected as before. Follow-up to #2121 / #2136. (#2137)
- Parser: a multiline `.method()` chain immediately after a statement-position `CallStmt` (e.g. `xs.sort()
    .iter()
    .toList()` or `foo()
    .iter()`) no longer fails with `unexpected token ''`. Both the 1-hop UFCS path (`ident.method(args)`) and the direct-call path (`ident(args)`) in `src/parser/parser.cpp`'s statement dispatch now route through `parsePostfixContinuation` and fall back to a `CallStmt` (preserving the trailing-block fast path) only when no chain follows. The expression-position chain support added in #2115 / #2121 already covered statements whose leader was a primary expression (list literal, bare ident); this closes the asymmetry for the call-statement leader. Follow-up to #2121. (#2138)
- `emit` crate: `ry_emit_get_param` now rejects out-of-range `idx` (`idx >= LLVMCountParams(fn_handle)`) at the boundary, returning the sentinel `0` instead of forwarding to `LLVMGetParam`'s raw-pointer-arithmetic `arg_begin()[index]` (UB on OOB). The range guard lives in the core layer (`crates/emit/src/primitive/function.rs::EmitCtx::get_param`, now returning `Option<ValueRef>`) because `scripts/check-emit-abi-no-ir.sh` (#2069) forbids `llvm_sys::core` references in `abi/**`; the abi shell propagates the `None` as the existing sentinel-0 contract. Currently unreachable from `CodeGen::emitGetParam` (every caller passes `idx=0` on a single-arg function), but locked in with a direct `tests/test_emit_abi_guards.cpp::GetParamOobIdxReturnsZero` regression so a future multi-arg function-creation path cannot accidentally re-introduce the UB hole. The other handle-NULL guards on the same extern are unchanged. (#2141)
- `copy(src, dst)` now returns `Err` on macOS when the source is a directory or other non-regular file, matching the Linux behavior. Previously the macOS path called `copyfile(... COPYFILE_ALL)` without a pre-check, which silently created an empty destination directory and returned `Ok` for a directory source — a latent data-loss risk if the caller subsequently removed the source. Symbolic links continue to be followed and are accepted when their target is a regular file. (#2164)

## [0.0.27] - 2026-06-10

### Added

- `min`, `max`, and `sum` now accept a variadic scalar form in addition
  to the existing single-list form: `min(3, 5)`, `max(1, 2, 3)`,
  `sum(1.0, 2.0, 3.0)` (previously only the list form such as
  `min([3, 5])` worked, and `min(3, 5)` failed with
  `min() takes exactly 1 argument`). The variadic form takes two or more
  arguments (unbounded), all of the same type; `min`/`max` accept
  `int`/`float` and `sum` accepts `int`/`float`/`u8`, matching the
  element types each already supported for the list form. (#1886)
- `case` **expressions** now accept indented-block arms, not only single-line `pattern : value` arms — closing the asymmetry where only "case expression × indented block" was rejected (`unexpected token '
'`) while the other three case-form / arm-notation combinations parsed. A block arm runs its intermediate statements and yields its **tail expression** as the arm's value (Rust/Scala `match`-style), so an arm that needs a local computation can be written directly:
  ```ry
  r = case x:
      1:
          tmp = x + 10
          tmp * 2          # tail expression — the arm's value
      _ : 0
  ```
  Covers both the subject form (`case x:`) and the no-subject condition form (`case:`), including the latter's `_:` else arm. The tail line is parsed as an expression, so an identifier-starting tail (`tmp * 2`) or a UFCS / method-call tail is accepted without parentheses — unlike `if`-expression block branches, which still require parenthesizing such a tail. A block whose final line produces no value (e.g. an assignment) is rejected at parse time with `case arm block must end with an expression`. Inline and block arms may be mixed within one `case`; the formatter canonicalizes a block arm with no intermediate statements back to the inline `pattern : value` form. (#1891)
- `scripts/check-prompt-refs.sh` — a reference-integrity lint for the prompt /
  instruction definition files (`.claude/**/*.md`, `AGENTS.md`, `CLAUDE.md`),
  wired into the CI `lint` job. It fails on (a) inline-code paths that do not
  exist on disk, (b) `/<name>` slash-command references with no matching
  `.claude/skills/<name>/SKILL.md`, and (c) `KNOWLEDGE.md` section references
  whose heading is not present verbatim in `KNOWLEDGE.md`. Detection is
  inline-code-span only — fenced blocks, plain prose, and `<...>` placeholders
  are the escape hatch. A local mirror
  (`.claude/skills/pre-commit-checklist/run-prompt-refs-lint.sh`,
  `/pre-commit-checklist` §3.5.7) and a path-scoped rule
  (`.claude/rules/prompt-reference-integrity.md`) accompany it. (#2029)

### Changed

- **Breaking:** Type conversions are now spelled `int`, `float`, and `str` — the same as the type names. `int(s)` / `float(s)` parse a string and return `Result<_, Error>`; `str(v)` renders any value to a string and stays overridable for records via `fn str(v: MyRecord) -> str`. The type-vs-call ambiguity is resolved by parse position, so `x: int = int("1") ?? 0` is legal; `int` and `float` are reserved built-in names. Passing a non-`str` to `int` / `float` is a compile error that points at the `as` cast (`3.14 as int`, `n as str`). The previous `toInt` / `toFloat` / `toStr` helpers — and the `parseInt` / `parseFloat` aliases added in #1772 — are removed with no deprecation period. (#1773)
- `emit` crate refactor: extended the #2057 abi/ffi/core layering to the `control_flow` (`create_basic_block` / `branch_cond` / `branch_uncond` / `create_phi`), `option` (`option_wrap_some` / `_none`), and `lifecycle` (`ctx_create` / `_destroy` / `_set_function` / `intern` / `resolve`) ops, and added the Rust-native `BasicBlockRef` / `FunctionRef` / `TypeRef` handle newtypes to `core`. Each migrated op's `#[no_mangle]` externs now live in a per-op child module under `crates/emit/src/abi/` (resolve / translate / intern shells), while the IR-emission bodies became `impl EmitCtx` methods in abi-independent core-role modules; the already-merged arc pilot's externs relocated there too. `lifecycle` (which emits no IR) collapsed into a `core::EmitCtx::new` constructor and `src/lifecycle.rs` was removed. Pure refactor — the emitted LLVM IR is byte-identical (verified by an `--emit-llvm-ir` diff over an `if`-expr / `while` / `Some`/`None` / CoW probe, ASLR-normalized), the `api.h` C surface is unchanged (`scripts/check-llvm-emit-abi-header.sh` clean), and the `#[no_mangle]` boundary symbols stay exported. (#2059)
- `emit` crate refactor: migrated `collection` (`collection_append` / `_insert` / `_remove_at` / `list_slice`) to the #2057 abi/ffi/core layering. The four `#[no_mangle]` externs became resolve / intern shells in `crates/emit/src/abi/collection.rs`, while the IR-emission bodies became `impl EmitCtx` methods in the abi-independent core-role `collection.rs`; `insert` / `remove_at` now call the `negative_index_wrap` / `bounds_error` engine methods directly instead of round-tripping through the abi externs. `list_slice` returns a new `core::SliceParts { count, new_data }` aggregate (the crate's first multi-value engine return) that the shell splits into its two `*mut RyValueId` out-params. As `collection` was the last in-crate caller of the `bounds` externs, the `pub use bounds::*` re-export in `abi.rs` was dropped. Pure refactor — the emitted LLVM IR is byte-identical (verified by an `--emit-llvm-ir` diff over an `append!` / `insert` / `removeAt` / `slice` probe, ASLR-normalized, with all 15 filecheck fixtures unchanged), the `api.h` C surface is unchanged (`scripts/check-llvm-emit-abi-header.sh` clean), and the `#[no_mangle]` boundary symbols stay exported. (#2061)
- `emit` crate refactor: migrated `any` (`any_wrap` / `_unwrap` / `_try_unwrap`) to the #2057 abi/ffi/core layering — the largest and final op, completing the series. The three `#[no_mangle]` externs became resolve / map / intern shells in `crates/emit/src/abi/any.rs` that translate each C descriptor (`RyAnyWrapDesc` / `RyAnyUnwrapDesc` / `RyAnyTryUnwrapDesc`) into a Rust-native struct and map the `c_int` kind into a dedicated `AnyWrapKind` / `AnyUnwrapKind` / `AnyTryUnwrapKind` enum (in `core`), while the IR-emission bodies became `impl EmitCtx` methods in the abi-independent core-role `any.rs`. Because `any`'s guards are branch-nested (unlike `cow`'s flat validation), the engine methods return `Option<ValueRef>` and the shell interns `None` to the 0 sentinel, keeping the branch-specific guards in place so the relocation stays byte-identical even on error paths. As `any` was the last in-crate `use crate::abi` consumer, the transitional `cstr_bytes` re-export in `abi.rs` was dropped; no module outside the `abi/` children now references `crate::abi`. Pure refactor — the emitted LLVM IR is byte-identical (verified by an `--emit-llvm-ir` diff over a probe exercising all eight `any` kind-paths plus both the ARC and str retain guards, each marker coverage-gated before the ASLR-normalized diff, with all 15 filecheck fixtures unchanged), the `api.h` C surface is unchanged (`scripts/check-llvm-emit-abi-header.sh` clean), and the `#[no_mangle]` boundary symbols stay exported. (#2063)
- `emit` crate hardening: the internal list/map/set/arc header structs the cdylib rebuilds (`crates/emit/src/cow.rs` `cow_ensure_unique`, `crates/emit/src/core.rs` `arc_header_type`) to mirror CodeGen's `listHeaderTy_` / `mapHeaderTy_` / `setHeaderTy_` / `arcHeaderTy_` (`src/codegen.cpp`) are now single-sourced through `core::header_fields` and guarded mechanically instead of by a `// Field order MUST stay in sync` comment. A C++ field-order change the Rust mirror failed to follow previously compiled, passed `tests/test_abi_layout.cpp` (which pins only the boundary descriptors, not the internal headers), passed the filecheck goldens (which don't emit the CoW Map/Set paths), and only corrupted Copy-on-Write deep-copy at runtime. The guard is a cross-language parity test (`tests/test_header_layout.cpp` asserts the canonical `CodeGen` types against a test-only `ry_emit_test_header_layout` extern — field count + per-index type + ABI size — plus a permanent negative test that the comparator rejects a permuted layout) backed by same-type-swap (`len↔cap` / `keys↔vals` / `strong↔weak`) behavioral coverage in `tests/spec/cow.test.ry`, since those all-8-byte swaps are invisible to any layout check. Pure refactor — the emitted LLVM IR is byte-identical (`build_header_struct` produces the same literal struct as the previous inline `LLVMStructTypeInContext` calls), the `api.h` C surface and `RyCowEnsureUniqueDesc` are unchanged, and the test-only extern reads only the pure-data field-kind table so `crates/emit/src/abi/` stays clear of the `scripts/check-emit-abi-no-ir.sh` gate. (#2071)
- `emit` crate cleanup: removed the unused `EmitCtx::function` internal state and its entire write path. The cached `function` field — written by `ry_emit_ctx_create`'s 4th argument and the `ry_emit_ctx_set_function` setter — was never read once the BB-creating emission ops migrated to deriving their parent function from `ctx->builder->GetInsertBlock()->getParent()` (the builder-derived-parent rule in `.claude/rules/codegen-llvm-ir-conventions.md`, #1968 / #1996). The field is gone from `crates/emit/src/core.rs`'s `EmitCtx` / `EmitCtx::new`; the `ry_emit_ctx_set_function` boundary entry (`crates/emit/src/abi/lifecycle.rs` + the `include/ry/llvm_emit/api.h` declaration) and `ry_emit_ctx_create`'s `function` parameter were removed; and the ~13 `ry_emit_ctx_set_function(cg.emit_ctx_, …)` call sites across the `src/codegen_emission_*.cpp` emission TUs plus the now-stale "Precondition: ry_emit_ctx_set_function must be called" comments in `include/ry/codegen/lowered_*.hpp` and `include/ry/llvm_emit/api.h` were swept. **Full removal rather than a documented no-op was chosen** because the `api.h` surface is an internal FFI boundary — `ry` and the `libemit` cdylib are always built and linked together in one corrosion build, there is no external consumer, and no test pins the signatures — so there is no ABI-compatibility constraint, and keeping a do-nothing setter would have left exactly the misleading contract this cleanup removes. `RyFunctionRef` / `asRyFunction` survive: `ry_emit_create_basic_block(ctx, name, fn)` still takes an explicit parent-function handle. Pure refactor — the removed setter wrote only the never-read `EmitCtx::function` field, so eliminating its writes is a dead store that cannot affect emitted IR. Confirmed byte-identical by an ASLR-normalized `--emit-llvm-ir` diff (baseline vs after) over the 15 filecheck goldens plus a probe exercising the arc-retain, CoW-ensure-unique, collection append-grow / insert + removeAt (memmove) / slice, bounds-check, Result-construction, and any wrap + unwrap emission paths; the two remaining setter-removal sites (the `ry_emit_result_branch` three-BB merge-phi and `ry_emit_any_try_unwrap`) are the identical one-line deletion and are covered at runtime by `ry_tests` + `ry test -p`. `scripts/check-llvm-emit-abi-header.sh` and `scripts/check-emit-abi-no-ir.sh` stay clean, and `ry_tests` (2577 cases, including `tests/test_emit_abi_guards.cpp`) + `ry test -p` (204 files) remain green. (#2083)
- Bump the Linux release container image pin (`ry-ci-glibc-old`) from `llvm-21-rev7` to `llvm-21-rev12` so byte-reproducible release builds track the latest published CI base image, matching what mainline CI already runs against via the mutable `:llvm-21` pointer. (#2088)

### Fixed

- Fixed a pre-existing use-after-free in `if`-expression block branches: a branch that bound an ARC-managed value (`List` / `Map` / `Set` / `str` / `Option` payload) to a local and returned it as the parenthesized tail (e.g. `if c:` … `items = [...]` … `(items)`) released that binding when the branch scope closed, so the result came back freed/empty. The block-tail value is now retained before scope cleanup — the same escape-retain that makes the new `case`-expression block arms above sound. (#1891)
- Swept the #1827 parser/lexer path drift through the prompt definitions:
  ~20 stale inline-code paths (the old flat `src/parser*.cpp` /
  `include/ry/parser.hpp` forms updated to their current subdirectory
  locations under `src/parser/`, `src/lexer/`, `include/ry/parser/`,
  `include/ry/diagnostic/`, and `editor/tree-sitter/src/`), a dead
  slash-command link in `.claude/agents/sanitizer-runner.md`, and the
  English/Japanese `KNOWLEDGE.md` "sanitizer issues" section-name mismatch in
  `AGENTS.md` that made a grep for the cited section silently miss. Also
  corrected three references to now-closed issues (two of which were closed
  not-planned, not fixed) and a drifted `CHANGELOG.md` line-number citation in
  `AGENTS.md`. (#2029)
- A corrosion crate rename (`ry_codegen` → `emit`, #2040) leaves the old
  cdylib (`libry_codegen.{so,dylib}`) as an orphan in a non-clean build
  tree: a non-destructive `cmake` reconfigure self-heals `build.ninja` to
  `libemit` but does not garbage-collect the stale output, and
  `bundle-dist.sh`'s `libry_*` glob would then ship the dead cdylib in the
  release tarball. The packaging path now guards against this structurally
  using a zero-drift discriminator — the emission cdylib is the only native
  lib that links `libLLVM`, while stdlib `libry_*` libs do not:
  `scripts/verify-bundle.sh` (the pre-`tar` release gate) now FAILs if any
  bundled native lib other than `libemit` links `libLLVM`,
  `scripts/bundle-dist.sh` skips such orphans when copying (with a warning),
  and `.claude/skills/pre-commit-checklist/run-tests.sh` removes them from
  the host build tree after each build so a manual `--clean` is no longer
  required to clear the orphan. `ADDITIONAL_CLEAN_FILES` was intentionally
  not used: it is keyed to the current target name and cannot retroactively
  GC a renamed-away output. (#2041)
- `emit` crate hardening: unified the input-validation contract across every `crates/emit/src/abi/` boundary entry point. Previously only `runtime_call` / `build_error_from_runtime` / `get_runtime_fn` / `result_branch` (#2028) converted malformed input to a sentinel; `control_flow`, `collection`, `bounds`, `option`, and part of `lifecycle` passed unvetted values straight to the `core` engine, which forwards them to the LLVM C API — so a NULL ctx, a NULL FFI array (`create_phi`'s incoming buffers, raw-deref'd via `*ptr.add(i)`), a NULL out-param (`list_slice`'s `*out_count` / `*out_new_data`), or an id that resolves to a NULL handle (`arc_retain` / `arc_release`'s header) reached undefined behavior immediately. Every production entry point now shares one contract via three shared helpers in `crates/emit/src/abi.rs` — `checked_cx` (ctx + context / module / builder NULL), `resolve_value` (id → non-NULL handle, subsuming `any.rs`'s former `opt_value_id`), and `ffi_slice` (the `(ptr, count)` borrow) — converting malformed input to a sentinel (`0` / NULL) or a no-op before any LLVM call; the re-entrant `result_branch` keeps its inline guards, and the IR-free `ctx_set_function` / `intern` / `resolve` guard `ctx.is_null()` only. Additive hardening — happy-path emitted IR is unchanged (`scripts/check-emit-abi-no-ir.sh` stays green and the filecheck goldens are byte-identical), and the reachable guard branches (per-entry ctx-NULL plus the `create_phi` NULL-array, `list_slice` NULL-out-param, and `arc` NULL-header cases) are locked in `tests/test_emit_abi_guards.cpp`. (#2080)

## [0.0.26] - 2026-06-03

### Added

- Mixed-type literals for `Map<K, any>`, `List<any>`, and `Set<any>`
  variable declarations and reassignments. Previously
  `m: Map<str, any> = {"a": 1, "b": "two", "c": true}` failed at
  codegen with `map values must all have the same type` because the
  `MapExpr` / `ListExpr` / `SetExpr` emitters strictly required
  identical LLVM types across elements and the annotation-driven
  `wrapInAny` auto-wrap (which works for `x: any = 1`) was never
  reached. A `LiteralAnyHintGuard` RAII helper, installed at the
  three assignment-system call sites (var-decl, function-local
  reassignment, module-global reassignment in `src/codegen_stmt.cpp`),
  signals to the literal emitters that each element should be
  individually wrapped via `wrapInAny` and that the strict
  same-type gate should be skipped. Element-type metadata
  (`list_elem_type_name` / `set_elem_type_name` /
  `map_value_type_name`) is stamped as `"any"` on the literal header
  so downstream destructor dispatch picks the right release path.
  `Map<any, V>` (any-typed keys) is intentionally out of scope because
  the rehash dispatch (`__ry_ht_rehash_i64` / `_f64` / `_str`) has no
  16-byte struct variant; mixed-key annotations continue to be
  rejected at the strict same-type check. `Map<str, int> = {...}` and
  other concrete-element annotations continue to enforce strict type
  equality. (#1884)
- Tree-sitter grammar (`editor/tree-sitter/grammar.js`) now parses the
  `[T]` generic call syntax: `load[int]("42")`,
  `mapHas[str, int](m, "a")`, `load[Map<str, any>]("{}")`, and
  arbitrarily nested type arguments such as
  `load[Map<str, List<Foo>>](text)`. The `call_expression` rule gains
  an optional `type_arguments` field that consumes the existing `_type`
  rule, so the `function` / `arguments` field names and the highlights
  query bindings are preserved. Corpus regressions covering the four
  shape categories are added to
  `editor/tree-sitter/test/corpus/expressions.txt`, and
  `tests/spec/json.test.ry` and
  `tests/spec/collection_element_metadata.test.ry` are dropped from
  `editor/tree-sitter/expected-fail.txt` now that they parse cleanly.
  This closes the editor-tooling parity gap introduced when the C++
  parser added the syntax in #1887; runtime and typecheck behavior is
  unchanged. (#1906)

### Changed

- Pure type-name utility helpers (`splitGenericTypeName`, `trimTypeNameSpaces`, `isListTypeName`, `isMapTypeName`, `isSetTypeName`, `isWeakTypeName`, `isFunctionTypeName`, `isLowLevelTypeName`, `deriveRuntimeFnName`, `nativeSigKey`) moved from `CodeGen` static members to a new layer-independent `ry::util` namespace under `include/ry/util/type_name.hpp`. Internal refactor with no behavior change. Added `docs/architecture/{compiler-layers,llvm-ir-emission-boundary,runtime-abi-boundary}.md` to document the compiler layer dependency direction, the candidate LLVM IR emission shared-library boundary, and the runtime ABI surface as preparation for incremental Rust migration. (#1820)
- Added `docs/architecture/layer-graduation-workflow.md` and `docs/architecture/codegen-layering-plan.md` to define when a compiler/runtime layer is graduate-ready (criteria + per-layer document template) and to record the codegen 2-layer split working hypothesis (Ry semantic lowering vs LLVM IR emission, lowered IR vocabulary, bounds-check pilot). `docs/architecture/compiler-layers.md` is updated to forward-reference the planned split. Documentation-only; no behavior change. Preparation for #1949 (LLVM IR emission shared library) and #1950 (Rust reimplementation). (#1824)
- Reorganized lexer, parser, AST, diagnostic, and source-manager source files
  into role-specific subdirectories (`src/lexer/`, `src/parser/`, `src/ast/`,
  `src/diagnostic/`, `src/source_manager/`) and the matching `include/ry/`
  layout. No behavior change — file rename plus `#include` path updates only.
  Stage 1 of the v0.0.26 C++ tree reorganization tracked under the #1819
  umbrella.
- Reorganized module/import resolution, semantic analysis, project/environment,
  CLI, application entry, JIT, trace, and coverage source files into
  role-specific subdirectories (`src/module/`, `src/sema/`, `src/project/`,
  `src/cli/`, `src/app/`, `src/jit/`, `src/trace/`, `src/coverage/`) and the
  matching `include/ry/` layout. `main.cpp` was placed under `src/app/` to keep
  the production binary entry separate from the CLI library layer. No behavior
  change — file rename plus `#include` and CMake path updates only. Stage 2 of
  the v0.0.26 C++ tree reorganization tracked under the #1819 umbrella.
- Added `ry_llvm_emit` as a `SHARED` CMake target — the scaffolding for the LLVM IR emission shared-library boundary (#1820 / #1824 design, #1950 Rust target). The `extern "C"` ABI lives in `include/ry/llvm_emit/api.h` (opaque `RyValueId` handles, `RyEmitCtx`, `RyEmitCallbacks` slot). Three category-3 helpers cross the ABI in this PR: `CodeGen::getRuntimeFn`, `CodeGen::buildErrorFromRuntime`, and the `BoundsCheck` pilot emission (#1961). The remaining two helpers (`wrapPtrAsResult`, `wrapStatusAsResult`) and the `emitResultBranch` core stay as `CodeGen` methods — they pull in pointer-identity-sensitive caches (`result_types_`) and metadata helpers (`propagateMeta`, `tryRetainArcSource`) and will migrate together with the `ResultWrap` lowered op in a successor PR. Internal refactor with no behavior change; all C++ and Ry tests pass against the ABI-routed implementations. (#1949)
- Codegen-layering Stage 2-B (#1964) completes the category-3 helper migration begun in #1949 / #1963. `wrapPtrAsResult`, `wrapStatusAsResult`, and `emitResultBranch` now cross the `libry_llvm_emit` ABI via a new `ry_emit_result_branch` entry point and a `ResultBranch` lowered op (`include/ry/codegen/lowered_result_branch.hpp`); `CodeGen::emitResultBranch` survives only as a thin shim that bridges `llvm::function_ref<>` callers to the C-fnptr ABI via a trampoline. The `RyEmitCallbacks` slot is removed entirely — `emit_negative_index_wrap` / `emit_bounds_error` are promoted to proper ABI functions (`ry_emit_negative_index_wrap` / `ry_emit_bounds_error`) that are self-contained inside the shared library. `getResultType`'s `StructType` cache stays on the CodeGen side because its reverse map is consumed by ARC release and Any wrapping; `resTy` crosses the ABI as `void*`, mirroring the existing `errorTy_` shape from Stage 2-A. (#1964)
- Codegen-layering Stage 2-C (#1967) starts the post-`RyEmitCallbacks` op migration begun in #1949 / #1963 / #1964. `CodeGen::buildSomeValue` and `CodeGen::buildNoneValue` now cross the `libry_llvm_emit` ABI via two new entry points (`ry_emit_option_wrap_some` / `ry_emit_option_wrap_none`) and an `OptionWrap` lowered op (`include/ry/codegen/lowered_option_wrap.hpp`); they survive only as thin shims that forward to `codegen::lowering::lowerOptionWrap` + `codegen::emission::emitOptionWrap`. Unlike BoundsCheck (#1961) and ResultBranch (#1964), the OptionWrap emission helper creates no basic blocks and therefore does not call `ry_emit_ctx_set_function`. The Ry-semantic side effects of the `Some` arm (`propagateMeta` + `tryRetainArcSource`, #999 ARC retain contract) stay on the CodeGen-side shim, keeping the emission TU as a pure `intern → ABI → resolve` transit. `getOptionType`'s `StructType` cache stays in CodeGen for the same reason as `getResultType` in #1964 (its reverse map `reverse_option_types_` is consumed by ARC release / Any wrapping); `optTy` crosses the ABI as `void*`, mirroring `resTy` / `errorTy_`. (#1967)
- Codegen-layering Stage 2-C begins with the ARC retain/release op migration (#1968). `CodeGen::emitArcRetain` / `emitArcRelease` now cross the `libry_llvm_emit` ABI via new `ry_emit_arc_retain` / `ry_emit_arc_release` entry points and a paired `lowered::ArcRetainOp` / `lowered::ArcReleaseOp` (`include/ry/codegen/lowered_arc.hpp`); both `CodeGen` methods survive only as thin shims. The `@parallel for` SeqCst contract is preserved across the ABI via the new `RyArcAtomic` enum (`RY_ARC_NONATOMIC` / `RY_ARC_ATOMIC`). The destructor crosses as a `void *` C function pointer rather than an `llvm::FunctionCallee`, and the GC visit function is similarly opaque. ARC ops are the highest-fanout lowered op surface (55+ call sites across record field retain, tuple element CoW, lambda capture release, `@parallel for` thunk teardown, weak upgrade, and tagged-union release), so migrating them first establishes the ABI shape that the remaining Stage 2-C ops (`RuntimeCall`, `AnyWrap`, `CollectionMutate`, `CowEnsureUnique`, etc.) will reuse. `used_native_libraries_.insert("gc")` migrates from inline in `emitArcRelease` to the emission shim because the runtime dependency on `libry_gc` is driven entirely by the emitted `__ry_gc_track` / `__ry_gc_untrack` calls. The `%ArcHeader = type { i64, i64 }` named struct declaration moves to an anonymous struct created locally inside the ABI implementation; under opaque pointers the emitted IR is otherwise bit-exact, preserving every `arc.retain` / `arc.retain.done` / `arc.release.body` / `arc.release` / `arc.free` / `arc.skip_free` / `arc.gc_track` / `arc.done` basic-block label that existing FileCheck goldens depend on. (#1968)
- Codegen-layering Stage 2-C continues with RuntimeCall (#1969). All `__ry_*` runtime symbol resolutions now cross the `libry_llvm_emit` ABI via a new `ry_emit_runtime_call` entry point and a paired `lowered::RuntimeCallOp` (`include/ry/codegen/lowered_runtime_call.hpp`). The bulk migration consolidates ~130 scattered `mod_->getOrInsertFunction("__ry_…", fnTy) + builder_.CreateCall(...)` pairs across 17 codegen translation units (largest: `codegen_test.cpp` at 46 sites, `codegen_call_io.cpp` at 39 sites, `codegen_call_string.cpp` at 12 sites) into uniform `getRuntimeFn("__ry_…", retTy, {argTys})` calls. `CodeGen::getRuntimeFn` itself is unchanged from #1949 (it was already an ABI-aware thin wrapper over `ry_emit_get_runtime_fn`); the consolidation is purely a call-site uniformization that completes the "no direct `getOrInsertFunction("__ry_…")`" objective of issue #1969. The new `codegen::lowering::lowerRuntimeCall` and `codegen::emission::emitRuntimeCall` helpers are provided for future call-site migrations that want to route a full call (callee resolution + `CreateCall`) through the ABI in a single shot — current call sites continue to construct the call locally via `getRuntimeFn` + `builder_.CreateCall` for a minimal-churn migration. Two variadic call sites in `codegen_call_user.cpp` (`__ry_print_printf`, `__ry_sprint_printf`, both `isVarArg=true`) are intentionally left out of scope because `getRuntimeFn` fixes `isVarArg=false`; a variadic-aware ABI variant or extension is deferred. (#1969)
- Codegen-layering Stage 2-C continues with the CoW (Copy-on-Write) uniqueness-check op migration (#1970). `CodeGen::emitCowCheckSlot` now crosses the `libry_llvm_emit` ABI via the new `ry_emit_cow_ensure_unique` entry point and a paired `lowered::CowEnsureUniqueOp` (`include/ry/codegen/lowered_cow.hpp`); the CodeGen method survives only as a ~60-line shim. The shim collects per-kind element / key / value sizes from `DataLayout`, walks `arc_str_managed_vars_` and `arc_field_record_vars_` to set the retain flags, and runs `propagateMeta` / `propagateMetaWide` on the result. `CodeGen::emitCowCheck` (the `arc_backed_vars_` guard wrapper) is untouched.
  The op takes a single `RyCowEnsureUniqueDesc *` descriptor pointer carrying 12 primitive / opaque fields: `data_ptr_id`, `slot_ptr_id`, `kind`, `atomic`, `elem_size`, `key_size`, `val_size`, `do_elem_retain`, `elem_is_str`, `do_key_retain`, `key_is_str`, and `destructor_callee`. The single-descriptor shape keeps the C ABI extension surface narrow even for ops with many kind-dependent parameters.
  CoW is the most structurally complex op migrated so far. A single ABI call expands into atomic strong-count load, ICmp, CondBr, per-kind (List / Map / Set) deep copy with malloc + memcpy of the data buffer, optional per-element retain loop, optional per-key retain loop (Map), `arc_release` of the old data pointer, slot store, and a PHI joining the no-clone and clone branches. The List / Map / Set header struct shapes are reproduced as anonymous `StructType::get(*ctx, {...})` calls inside the ABI implementation, mirroring the named struct shapes still created in `src/codegen.cpp` (a sync comment in `src/llvm_emit/impl.cpp` flags the dependency).
  `ry_emit_cow_ensure_unique` is the first ABI helper that composes other ABI helpers. It invokes `ry_emit_arc_retain` (per element / per key in the retain loops) and `ry_emit_arc_release` (for the old data pointer before the slot store) directly inside the shared library. This validates that the ABI surface is composable, not leaf-only.
  The three CodeGen-private helpers `emitCowDeepCopyList` / `emitCowDeepCopyMap` / `emitCowDeepCopySet` had no callers outside `emitCowCheckSlot` and are removed entirely. `emitCowRetainArcElements` stays on the CodeGen side because it has 8 non-CoW external call sites (`append!`, `insert!`, `concat`, `merge`, `slice`, …).
  The atomic strong-count load uses `AtomicOrdering::Acquire` when `desc->atomic == RY_ARC_ATOMIC`, preserving the pairing with `atomicrmw SeqCst` updates in `ry_emit_arc_retain` / `ry_emit_arc_release` so TSan stays clean on the `@parallel for` ARC path. The pre-migration `arc_owned_values_.insert(newDataPtr)` side effect after the clone branch is dropped — the set is rebuilt on every subsequent `emitArcGetDataPtr` call so the insert was redundant. The existing destructor-touching spec tests (List / Map / Set with str / nested-collection elements, `Result<List<int>, str>` Err binding, `@parallel for` worker ARC retain/release loops) confirm equivalence. (#1970)
- Codegen-layering Stage 2-C continues with the list-mutation op migration (#1971). `CodeGen::emitCollOp_append` / `emitCollOp_insert` / `emitCollOp_remove_at` / `emitListSlice` now cross the `libry_llvm_emit` ABI via four new entry points (`ry_emit_collection_append` / `_insert` / `_remove_at` / `_list_slice`) paired with `lowered::CollectionAppendOp` / `CollectionInsertOp` / `CollectionRemoveAtOp` / `ListSliceOp` (`include/ry/codegen/lowered_collection_mutate.hpp`); the four `CodeGen` methods survive only as thin shims that retain `emitCowCheck` / value coercion / ARC retain decision / `emitArcAllocCollectionHeader` / `setTypeMeta` / `propagateMeta` side effects. Four separate ABI entries (rather than a single dispatched op) are used because the op shapes are non-uniform — append/insert/removeAt are in-place mutations while slice produces a fresh header — and the per-variant split mirrors the `ArcRetain` / `ArcRelease` precedent (#1968). The slice ABI uses `RyValueId *out_count, RyValueId *out_new_data` out-parameters to avoid struct-by-value across the ABI boundary. Header struct pointer and element struct pointer cross as `void *` (typed only on the codegen side as `llvm::StructType *` / `llvm::Type *`), `RyValueId` handles cross as `uint32_t`, and `elem_size` crosses as `uint64_t` so element-type metadata stays on the codegen side; the ARC retain loop for owned slice elements is hoisted into the emission shim because it depends on `list_elem_type_name` which the ABI cannot see. The reused ABI helpers (`ry_emit_bounds_error`, `ry_emit_negative_index_wrap`) keep insert's negative-index wrap + bounds-error behavior byte-for-byte identical to the pre-migration emission. Basic-block labels (`app.grow` / `app.store`; `ins.err` / `ins.ok` / `ins.grow` / `ins.move`; `rmat.err` / `rmat.ok`) are preserved bit-exact across the migration; slice remains branchless and adds no new BBs. FileCheck goldens that exercise these ops keep passing without modification. (#1971)
- Codegen-layering Stage 2-C continues with the `Any` wrap/unwrap op migration (#1972). `CodeGen::wrapInAny` / `unwrapFromAny` / `tryUnwrapFromAny` now cross the `libry_llvm_emit` ABI via three new entry points (`ry_emit_any_wrap` / `ry_emit_any_unwrap` / `ry_emit_any_try_unwrap`) paired with `lowered::AnyWrapOp` / `AnyUnwrapOp` / `AnyTryUnwrapOp` (`include/ry/codegen/lowered_any.hpp`); the three top-level `CodeGen` methods survive only as thin shims that retain Ry-semantic side effects on the CodeGen side: type-name resolution (`findEnumLikeTypeNameForBoxing` / `findRecordInfoForType` / `findRecordTypeName` / `buildTypeNameFromMeta`), descriptor lookup (`getOrCreateRecordDescriptor` / `getOrCreateEnumDescriptor`), layout-type construction (`recordBoxLayoutType` / `enumBoxLayoutType`), tag computation (`getAnyTypeTagForValue` / `getAnyTypeTag`), typed-collection registration via `__ry_any_register_typed_coll`, field-wise ARC retain via `emitRecordArcFieldsRetain` / `emitEnumBoxArcFieldsRetain` gated by the `!isa<CallInst> && !isa<InvokeInst>` reassignment guard, generic-substitution rejection, sub-helper dispatch (`unwrapEnumFromAny` / `tryUnwrapRecordFromAny` / `tryUnwrapListFromAny` / `tryUnwrapMapFromAny` / `tryUnwrapOptionFromAny`), and the post-unwrap `emitRecordArcFieldsRetain` on the returned record value. Three top-level entries (rather than a single dispatched op) match the public CodeGen surface and let each ABI helper keep its own `RyAnyWrapKind` / `RyAnyUnwrapKind` / `RyAnyTryUnwrapKind` enum (NonBox/RecordBox/EnumBox; Standard/F64Promote/Record; Standard/F64Promote). Sub-emission helpers stay CodeGen-private and do not cross the ABI surface (Path 1 narrower design): the typed `Record` / `List<T>` / `Map<str, V>` / `Option` / simple-enum arms of `tryUnwrapFromAny` depend on per-record reconstruction / `reverse_option_types_` / per-Map<str, V> rebuilding that the ABI cannot see. Per the codegen-llvm-ir-conventions rule the ABI helpers derive the parent function via `ctx->builder->GetInsertBlock()->getParent()` (not `ctx->function`), so the helpers stay safe under ARC / lambda / thunk / destructor / iterator-next retargeting. Basic-block labels (`any.bool.zext` / `any.rec.tag_ok` / `any.rec.tag_err` / `any.rec.desc_check` / `any.rec.desc_err` / `any.float` / `any.check_int` / `any.int2float` / `any.mismatch` / `any.merge` / `any.match` / `tryany.fp.tmp` / `tryany.fp.data` / `tryany.fp.fval` / `tryany.fp.ival` / `tryany.fp.i2f` / `tryany.fp.is_float` / `tryany.fp.is_int` / `tryany.fp.is_accept` / `tryany.fp.is_err` / `tryany.fp.val` / `res.ok` / `res.err` / `res.merge` / `tryany.tag` / `tryany.tag.eq` / `tryany.is_err` / `tryany.tmp` / `tryany.data` / `tryany.val`) are preserved bit-exact across the migration, as are all `load[<TargetType>]: …` error-message prefixes used by the typed sub-helper arms. (#1972)
- Completed Stage 2-C of the codegen layering plan: the LLVM IR emission
  ABI (`include/ry/llvm_emit/api.h`) no longer exposes any LLVM-owned
  types or transitional `void *` parameters in its public signatures.
  Category 1 (LLVM context handles: `RyModuleHandle` / `RyBuilderHandle`
  / `RyContextHandle` / `RyFunctionHandle`) and category 2 (primitive
  type accessors: `RyTypeRef` / `RyFuncTypeRef`) cross the ABI as
  opaque pointer typedefs. The 22 existing ABI entries (Stage 2-A / 2-B
  helpers + OptionWrap / ARC / RuntimeCall / CollectionMutate /
  CowEnsureUnique / AnyWrap / AnyUnwrap / AnyTryUnwrap) had their
  `void *` type parameters swept to typed handles. ControlFlow
  primitive ops (`ry_emit_create_basic_block` / `ry_emit_branch_cond` /
  `ry_emit_branch_uncond` / `ry_emit_create_phi`) cross the ABI; every
  `IRBuilder<>::Create{CondBr,Br,PHI}` / `BasicBlock::Create` call in
  `src/codegen_*.cpp` now goes through the `CodeGen::createBB` /
  `emitBranchCond` / `emitBranchUncond` / `createPhi` wrappers. A
  header-level lint script (`scripts/check-llvm-emit-abi-header.sh`,
  wired into the `lint` CI job) enforces the AC by failing on any
  `llvm::*` or non-carve-out `void *` token in the ABI surface.
  Primitive arithmetic / lexical scope / module-level symbol
  declarations remain outside the ABI per
  `docs/architecture/codegen-layering-plan.md` §"Explicit non-inclusion"
  — those are not part of #1973's AC. (#1973)
- `/pre-commit-checklist` §3.6 を 4 harness 対応に拡張。skip-detection grep が `src/runtime/native/io.cpp` / `include/ry/runtime/native/io.hpp` を含むようになり、`io.cpp` を変更する PR は §3.6 を自動的に検証対象に含む。`run-fuzz.sh` は 4 target (`fuzz_parser` / `fuzz_json` / `fuzz_utf8` / `fuzz_io_open`) を 60 s ずつ実行する (合計 ~4 分、従来は ~3 分)。Change-type matrix の row label と Fuzzer mapping、`.claude/agents/fuzzer-runner.md` の TARGETS list / REPORT FORMAT 例も併せて更新した。(#1976)
- `run-fuzz.sh` および §3.6 の wording で libFuzzer の `-rss_limit_mb` を 512 MB から 2048 MB に引き上げ。実測ピーク RSS は `fuzz_parser` 514 MB / `fuzz_json` 597 MB / `fuzz_utf8` 429 MB / `fuzz_io_open` 536 MB と、いずれの harness も libFuzzer の coverage tracking overhead (~275k inline 8-bit counters + PC table) で 400-600 MB に達する。512 MB cap では `fuzz_parser` で startup OOM を引き起こし、`fuzz_json` / `fuzz_io_open` も borderline だった (parser 固有のバグではなく、全 harness 共通の corpus + coverage 構造的 overhead)。2048 MB に引き上げて 4 harness 全てが安定して完走する。(#1976)
- AGENTS.md に「起票判断における選択肢提示の禁止」サブセクションを追加し、Claude Code がユーザーに提示する選択肢 (`AskUserQuestion` の options、テキストの選択肢列挙等) に「別 issue に起票する」を含めることを MUST 禁止した (#1981)。これに合わせて `/triage-side-finding` の Q4 を「3 択 (即時修正 / 別 issue 起票 / ユーザー確認)」から **Claude Code 自律判断 2 分岐 (即時修正 / 起票許可を求める)** に書き換え、Issue Creation Steps Step 1 の escalate 節も 3 択列挙 (「file an independent issue, expand the PR's scope, branch off, etc.」) を「1 つの推奨案を提示」に置換した。`/git-create-issue` Step 1 (preview 6 項目 → 明示許可待ち) は変更なし — 本ルールと既に整合している。意図: #1851 の自律誘導失敗の再発防止 / `/git-create-issue` permission gate との二重化排除 / ユーザーが判断材料なしで即決を求められる問題の排除。(#1981)
- AGENTS.md に新規セクション `## 禁止用語: flake / flaky` を追加し、Claude Code の説明・出力で `flake` / `flaky` の語をあらゆる言語 (日本語訳・カタカナ・他言語同義語 `unstable` / `intermittent` の言い換え用法を含む) で使用すること、および CI 失敗・テスト失敗の理由・結論として用いることを MUST 禁止した。代替表現として (a) 発生条件の明示、(b) `KNOWLEDGE.md` 既存 entry への明示リンク (issue 番号 + 行番号)、(c) root cause 未特定の場合「発生条件未特定。再現条件の調査が未完了」と明記し安易な再実行提案を禁止する、のいずれかを必須化した。歴史的記述 (`KNOWLEDGE.md` L261 #1895 / `CHANGELOG.md` L598 等) は検索性維持のため変更しないが、これらを引用・参照して `flake` を結論に再導入する行為も禁止した。新ルール導入時点で違反していた active prompt material 2 箇所 (`.claude/skills/triage-side-finding/SKILL.md` Q1 説明、`.claude/agents/fuzzer-runner.md` REPRO: FAILED 分岐) も同時に新ルール準拠表現に書き換えた。意図: CI #2578 で Claude Code が安易に「flake (re-run)」と結論した事故の再発防止 / root cause analysis 放棄の構造的排除。(#1990)
- codegen: the LLVM IR emission shared library (`ry_llvm_emit`) is now built unconditionally from the Rust crate (`crates/ry_llvm_emit/`). Because `ry` now always links the shared `libLLVM`, building from source outside the Docker CI image requires a shared `libLLVM` in the LLVM prefix and a Rust 1.83+ toolchain on `PATH`. On macOS use `cmake --preset rust-emit` (Homebrew `llvm@21` ships `libLLVM.dylib`); the static-only `/usr/local/llvm` no longer satisfies `--preset default`. The `ry-ci` Docker image bakes in both the shared libLLVM and Rust, so container builds need no extra setup. (#1993)
- Internal: added a compile-time ABI struct-layout verification mechanism between the C++ and Rust sides of the LLVM IR emission shared-library boundary. `tests/test_abi_layout.cpp` (C++ `static_assert`) and `crates/ry_llvm_emit/src/lib.rs` (`const _: () = assert!(...)`) pin the `sizeof` / `alignof` / per-field offset of the four `Ry*Desc` descriptor structs and the `sizeof` / `alignof` of the opaque handle typedefs declared in `include/ry/llvm_emit/api.h`. Both sides assert against the same constants, so incidental layout drift (field reorder, padding, type-width change) on either side breaks the build. CI's `lint` job runs `cargo check -p ry_llvm_emit` so the Rust assertions are exercised on every PR. This is a safety net for the in-progress Rust reimplementation of `ry_llvm_emit` (#1950 / #1993); the chosen method and the canonical layout table are documented in `docs/architecture/llvm-ir-emission-boundary.md`. (#1995)
- codegen: ported all 28 LLVM IR emission ABI function bodies from the C++ implementation (`src/llvm_emit/impl.cpp`) to Rust (`crates/ry_llvm_emit/`) via the LLVM C API, producing byte-for-byte equivalent IR under `RY_LLVM_EMIT_IMPL_RUST=ON` (verified against the FileCheck goldens and the full Ry/C++ test suites). The C++ implementation and the `RY_LLVM_EMIT_IMPL_RUST` flag remain for now (removed in a follow-up sub-issue). ON builds require a shared libLLVM in the LLVM prefix so the host and the Rust cdylib share one LLVM instance — use `cmake --preset rust-emit` (Homebrew `llvm@21`); the default OFF (C++) build via `--preset default` is unchanged. (#1997)
- release: distribution tarballs now bundle the shared `libLLVM` (plus its macOS chain dependency `libzstd`) and the Rust cdylib `libry_llvm_emit` next to `ry`, with install names / rpaths rewritten to `@loader_path` / `$ORIGIN`, so the release binary is self-contained after the #1999 Rust-emit cutover made a shared `libLLVM` mandatory at runtime. Without this, the first tag push after cutover would publish a binary that fails to start on systems without an installed LLVM (`dyld: Library not loaded` on macOS / `libLLVM.so: not found` on Linux). `install.sh` and `ry self-update` install the bundled libraries alongside `libry_*`. Tarballs grow by roughly the size of libLLVM (~100 MB class), and each now ships `LICENSE-LLVM.txt` (Apache-2.0 with LLVM exceptions) per redistribution requirements. (#2005)
- release: bumped the `release.yml` Linux container pin from `ry-ci-glibc-old:llvm-21-rev5` to the latest published immutable revision `llvm-21-rev7`, so the v0.0.26 release builds on the current pre-baked image. (#2017)

### Removed

- codegen: removed the C++ LLVM IR emission implementation (`src/llvm_emit/impl.cpp`) and the `RY_LLVM_EMIT_IMPL_RUST` CMake option — the Rust cdylib is now the only implementation. The locked `extern "C"` ABI surface (`include/ry/llvm_emit/api.h`, `cast_helpers.hpp`) is unchanged. (#1993)

### Fixed

- `from <mod> import <Type>` for C++-registered resource types
  (`File` / `TcpListener` / `TcpStream` / `TlsStream` / `HttpRequest` /
  `HttpResponse` / `HttpClientResponse` / `Thread` / `Lock` / `RWLock` /
  `Semaphore` / `Barrier` / `AtomicInt` / `AtomicBool`) and the
  builtin `regex.Match` record now succeeds, restoring symmetry with
  `@native fn` imports. Previously `extractDefinitions` only scanned
  `.ry` AST top-level declarations and rejected names registered via
  `ResourceKindRegistry` or `CodeGen`'s constructor, surfacing a
  misleading "typo? deprecated?" diagnostic. `module_loader.cpp` now
  bypasses the rejection for those names when the import path matches
  the registered `library` (gated by `from_stdlib=true` so local
  `<mod>.ry` shadows continue to enforce the .ry-source name set).
  Alias support (`from io import File as MyFile`) is also wired:
  `TypeAlias` validation in `emitImportAliasStmt` accepts an `orig`
  resolved via `ResourceKindRegistry`, and `registerResourceByTypeName`
  prefixes `resolveTypeAlias` so an aliased type name still receives
  resource-kind metadata for ARC cleanup. Concurrently fixed a
  pre-existing hardcoded `if (resolved == "File")` compare in
  `emitPatternBindingArc` (`src/codegen_match.cpp:795`) by normalising
  through `resolveTypeAlias` first, preventing handle leaks when a
  `Result<MyFile, Error>` is destructured via `Ok(f)`. (#1888)
- Top-level user `fn` declarations that collide with stdlib built-in function names (e.g. `sum`, `min`, `max`, `len`, `range`, `print`, `enumerate`, `zip`, `map`, `filter`, `Ok`, `Err`, `Some`, `None`) are now rejected at compile time with a clear diagnostic instead of silently being shadowed by the built-in. Generic-fn templates and `from <module> import <name> as <reserved>` aliases are checked the same way. Nested `fn`s, `@native` declarations, qualified-import module members, and type-aware overrides like `fn toStr(p: MyRecord)` remain accepted. (#1889)
- Nested tuple/record field access with chained numeric indices
  (`nested.0.0`, `pair.1.0`, `((1,2),(3,4)).0.1`) failed to parse with
  `expected field name or index after '.'`. The lexer greedily absorbed
  `.0` after an integer literal as a fraction part, so `t.0.0` lexed as
  `[Ident, Dot, Float("0.0")]` instead of `[Ident, Dot, Number("0"),
  Dot, Number("0")]`. Suppress fraction absorption when the integer
  literal directly follows a `Dot` token (`src/lexer.cpp` —
  `prev_kind_ != TokenKind::Dot` check, symmetric to the existing
  leading-dot float disambiguation). Update `docs/grammar.ebnf` to
  reflect that `INTEGER` is accepted in field-access position alongside
  `IDENT`. Non-regression for `1.5`, `(1.5)`, `a + 1.5`, `.5`, and
  `5.double()` is verified by lexer unit tests
  (`DotAfterDotSuppressesFractionAbsorption`). (#1892)
- `json.load[T]()` now correctly rejects `List`, `Map`, `Set`, closure, and other non-`str` pointer-typed arguments at compile time with `load[T]() requires a str or File argument`. Previously the non-`File` branch only checked for LLVM opaque pointer type, which let any reference value through and reached `__ry_json_parse_to_any` as if it were JSON text (invalid read under ASan/UBSan). The new guard uses `isStringValue` so the non-`File` branch is symmetric with the `isFile` branch. (#1941)
- The release-path `weak_count == 0` decision in `ry_emit_arc_release` now uses an atomic load (`AtomicOrdering::Acquire` in `@parallel for` atomic mode, `NotAtomic` otherwise) instead of a plain `load`. The previous non-atomic read raced with `atomicrmw Monotonic` updates emitted by `weak_retain` / `weak_rel` in `src/codegen_arc.cpp`, which would be flagged as a data race under TSan when an atomic-mode object's last strong reference is dropped concurrently with another thread's weak retain / release. This was pre-existing behavior inherited verbatim from `CodeGen::emitArcRelease`; the migration to `libry_llvm_emit` was the natural opportunity to harden it. (#1968)
- `ry_emit_result_branch` now derives the parent function for newly-created basic blocks from `ctx->builder->GetInsertBlock()->getParent()` instead of the cached `ctx->function`. This preemptive fix mirrors the `ry_emit_arc_retain` / `ry_emit_arc_release` correction in #1968 and forestalls the same cross-function reference hazard: result-branch helpers may be invoked inside destructor / lambda / thunk bodies where `cg.fn_` (the source of `ctx->function`) tracks the outer function while the IRBuilder has been retargeted to the nested function. Using `ctx->function` would have placed new BBs in the wrong function and produced verifier-rejected cross-function references. Pre-existing call sites had not triggered the hazard because they happened to update `cg.fn_` before invoking the helper, but the RuntimeCall migration (#1969) opens up more callers and the safer pattern is applied uniformly. (#1969)
- `fuzz_io_open` libFuzzer harness は `docker/entrypoint.sh` の case dispatch に登録されておらず、`./docker/run.sh fuzz fuzz_io_open ...` が `error: unknown command 'fuzz_io_open'` で exit 1 していた。CMake target (`add_ry_fuzz_target(fuzz_io_open ...)`) と corpus (`tests/fuzz/corpus/fuzz_io_open/`) は既存だったため Docker 経由の実行手段だけが欠落していた状態。entrypoint.sh の case パターンと error message、`docker/run.sh` の usage コメント、`docker/README.md` / `.claude/skills/linux-docker-dev/SKILL.md` の libFuzzer quickstart 例コマンドを 4 harness 対応に揃えた。(#1976)
- Tree-sitter highlight query (`editor/tree-sitter/queries/highlights.scm`)
  now applies `(#set! "priority" 105)` to the `decorator` pattern so that
  the decorator's identifier (e.g. `my_dec` in `@my_dec`) is highlighted
  as `@attribute` instead of being overridden by the generic
  `(identifier) @variable` fallback. Both patterns previously matched at
  default priority 100, and tree-sitter's last-match-wins tie-breaker
  promoted `@variable` — which was semantically wrong (a decorator name
  is not a variable) and produced no color in colorschemes that leave
  `@variable` unstyled. The fix is `highlights.scm`-only; `ry.so` does
  not need to be rebuilt. (#1988)

## [0.0.25] - 2026-05-26

### Added

- Extended the `any` type to hold `List`, `Map`, and `Set` collections
  in addition to the existing primitive types. `RyAnyTag` gains
  `List=5`, `Map=6`, and `Set=7`; the 16-byte struct layout is
  preserved by storing the collection header pointer in `data[8]`.
  Wrap-in-`any` now emits an ARC retain on the collection, and the
  enclosing variable's scope-end cleanup emits a tag-dispatched
  release. Implicit unwrap (`let xs: List<int> = anyVal`) succeeds
  whenever the dynamic tag matches the target collection kind, trusting
  the static type annotation for element-type narrowing. `any == any`
  on two collection-holding values does best-effort deep equality
  (length + 8-byte-slot byte-equal data buffer) for `List` and pointer
  identity for `Map` / `Set`; `to_string` returns opaque markers
  (`<List>`, `<Map>`, `<Set>`) since element-type metadata is erased on
  wrap. Order comparisons and arithmetic on collection-holding `any`
  values continue to surface the existing "operator X not supported"
  runtime error. Record / enum / function-pointer / resource types
  remain unsupported and are tracked as follow-ups. (#1697)
- `m[k]?` and `xs[i]?` postfix syntax for safe collection access.
  Applied directly to a Map or List index expression, the trailing `?`
  changes the semantics from "abort on miss" to "produce an `Option`":
  `m["a"]?` returns `Some(v): Option<V>` when the key is present and
  `None` otherwise; `xs[i]?` returns `Some(v): Option<T>` when the
  (possibly negative-wrapped) index is in range and `None` otherwise.
  This is a postfix syntax rather than sugar for `get(m, k)` — it
  parses as `IndexExpr` with a new `try_mode` flag and flows through
  the same codegen path on both Map and List. The negative-index wrap
  established by `xs[-1]` is preserved (so `xs[-1]?` on a non-empty
  list always returns `Some(last)`); only the post-wrap out-of-range
  case yields `None`. Write-form `m[k]? = v` (including `m[k]?.x = v`
  and `mm[k]?[k2] = v`), `?` on fixed-length arrays, on `str`, on
  range slice `xs[a..b]?`, and on `any`-typed nested access are
  rejected at compile time. The lexer's greedy tokenization of `??`
  means `m["k"]?? default` (no space) still parses as `m["k"]` +
  `?? default` — write `m["k"]? ?? default` (with a space) for the
  Option-returning form coalesced with a default value. (#1699)
- Extended the `any` type to hold user-defined `record` values.
  `RyAnyTag` gains `Record=8`; the 16-byte struct layout is preserved
  by heap-boxing the record so `any.data[8]` holds a pointer to a box
  laid out as `[ ArcHeader (16B) ][ descriptor ptr (8B) ][ record
  struct ]`. Each record type emits a singleton
  `__ry_record_desc_<typename>` global carrying the destructor,
  equality function, and type name, so the dynamic type survives
  erasure across function boundaries — release / equality / `toStr`
  all dispatch through the descriptor word inside the box rather than
  the (possibly stale) static type name at the call site. Wrap-in-`any`
  emits an ARC retain on the box (and field-wise retains for ARC fields
  when the source is an existing record alias), and the enclosing
  variable's scope-end cleanup releases the box through a descriptor
  trampoline. `any == any` on two record-holding values does field-wise
  deep equality when the descriptor pointers match (different record
  types always compare unequal); `toStr` emits a `<TypeName>` marker
  (e.g. `<Point>`) using the descriptor's type name. Implicit unwrap
  is gated by a descriptor-pointer-equality check against the expected
  type's descriptor global, so only **exact-type unwrap**
  (`let q: Point = anyVal` where `anyVal` holds a `Point`) is
  permitted; cross-type unwrap to a parent record traps at runtime and
  is tracked as a follow-up. The typed-path subtype coercion
  (`fn f(p: Parent): ...; f(child)`) is unchanged. Function-pointer
  and `enum` types remain unsupported. (#1797)
- Extended the `any` type to hold `enum` values — organic `enum`
  declarations (with or without payloads) plus the built-in
  `Option<T>` / `Result<T, E>` types. `RyAnyTag` gains `Enum=9`; the
  16-byte struct layout is preserved by heap-boxing the enum so
  `any.data[8]` holds a pointer to a box laid out as
  `[ ArcHeader (16B) ][ descriptor ptr (8B) ][ enum payload ]`. The
  payload is the enum's native representation (`i64` discriminant for
  simple enums; the ADT discriminated-union struct for ADT / `Option<T>`
  / `Result<T, E>`). Each enum type emits a singleton
  `__ry_enum_desc_<typename>` global carrying the destructor (which
  switches on the discriminant and releases the active variant's ARC
  fields), the equality function (variant-wise deep compare), and the
  type name — including the full generic parameterization, so
  `Option<int>` is distinct from `Option<str>` and `Result<List<int>,
  str>` is distinct from `Result<int, str>`. Even simple enums (no
  payload) flow through the new `Enum` tag rather than the prior
  `Int=0` shortcut, so the source-level enum identity survives the
  round-trip and `let c: Color = anyVal` only accepts an `any` that
  actually carries a `Color`. Wrap-in-`any` emits an ARC retain on the
  box (and field-wise retains for ARC fields in the active variant
  when the source is an existing enum alias); the enclosing variable's
  scope-end cleanup releases the box through a descriptor trampoline.
  `any == any` on two enum-holding values matches descriptor pointers
  first, then dispatches through the descriptor's equality function;
  enums of different types always compare unequal. `toStr` /
  f-string interpolation emits a `<TypeName>` marker (e.g. `<Color>`,
  `<Option<int>>`, `<Result<int, str>>`). Implicit unwrap is gated by
  descriptor-pointer equality, so only exact-type unwrap is permitted
  — enums do not participate in record-style subtype unwrap chains.
  Function-pointer and resource types (`TcpListener`, `TcpStream`,
  etc.) remain unsupported. (#1798)
- Extended `any` record unwrap to admit subtype projection. Given
  `record Dog < Animal: ...`, `let a: Animal = anyHoldingDog` now
  succeeds and reads the Animal-prefix fields from the boxed `Dog`,
  rather than trapping as in v0.0.24. `RyRecordDescriptor` gains a
  fourth pointer `parent_desc` that links each record's descriptor to
  its parent's descriptor (or `null` for root records); the unwrap site
  walks this chain at runtime via a new
  `__ry_record_is_subtype_desc(actual, expected)` helper instead of
  doing a single descriptor-pointer equality check, so the actual
  dynamic type inside `any` is matched against the expected type's
  entire ancestor chain. Multi-level inheritance
  (`GuideDog < Dog < Animal`) and cross-function boundaries
  (`fn make() -> any: return Dog(...)` then `let a: Animal = make()`)
  both work because the descriptor stored in the box is the authoritative
  dynamic-type record. Parent-prefix ARC fields (e.g. `Animal.name: str`)
  are retained when projecting and released independently at scope end;
  Child-only fields keep being released through the box destructor, so
  no leak or double-free occurs. Unwrapping `any` to an unrelated record
  type (e.g. a `Point` held in `any` to an `Animal` slot) still traps at
  runtime. The typed-path subtype coercion
  (`fn f(p: Parent); f(child)`) is unchanged. (#1802)
- File handle API for the `io` module: `open(path, mode)`,
  `readAll(f)`, `readLine(f)`, `writeText(f, s)`, and `close(f)`.
  `open` returns `Result<File, Error>`; valid modes are `"r"`, `"w"`,
  and `"a"`. `readLine` returns `Result<Option<str>, Error>` — `Ok(None)`
  signals EOF cleanly. `File` is an opaque ARC resource handle: the file
  is closed automatically when the last reference drops; calling `close`
  explicitly allows earlier release and is idempotent. Path and mode
  arguments are checked for embedded NUL bytes at the runtime boundary.
  (#1816)
- `using` statement for scope-based resource release. `using f =
  open(path, "r"): ...` binds `f` to the block body and calls `close(f)`
  automatically on every exit path: normal block end, `return`, `?`
  propagation, and `break` / `continue`. When the initializer itself
  propagates an error via `?`, no binding is established and no `close`
  is invoked. Nested `using` releases resources in reverse order of
  acquisition. The current scope is `io.File`; passing any other type
  produces the compile error `using requires an io.File value`. Panic /
  uncaught-runtime-error paths are tracked separately. (#1817)
- `io.lines(f: File) -> Iterator<str>` lazy line iterator. Pair with
  `for line in lines(f) { ... }` to process large files without loading
  them into memory. The iterator retains the underlying `File` for its
  lifetime and shares the read position with subsequent `readLine` /
  `lines` calls. After `close(f)`, iteration terminates at the next
  step rather than raising (Python-compatible). Closes the `#1700`
  series of streaming-IO requests (`#1816` File handles, `#1817`
  `using` statement, `#1818` `lines()` + `Iterator<T>`). (#1818)
- `io.open(path, mode)` now accepts `"rb"` (binary read) and `"wb"`
  (binary write) in addition to the existing `"r"` / `"w"` / `"a"`
  text modes. The internal `fopen_nofollow` helper already mapped
  `"rb"` / `"wb"` to `O_RDONLY` / `O_WRONLY | O_CREAT | O_TRUNC`; only
  the strcmp guard at the entry of `__ry_io_file_open` was rejecting
  them. The invalid-mode error message now reads `(must be "r", "w",
  "a", "rb", or "wb")` to reflect the extended set. This is a
  prerequisite for the future `readBytes(f: File)` / `writeBytes(f:
  File, bytes)` overloads (#1816 follow-up). Append-binary `"ab"`
  remains unsupported and is tracked separately in #1862. (#1848)
- Extended `json.loadAs[T](text)` and `json.loadAs[T](File)` to support
  user-defined records (flat and nested), typed collections of records
  (`List<Record>`, `Map<str, Record>`), and `Option<T>`. Each field of
  the parsed JSON object is looked up by name and recursively coerced
  into the declared type; missing fields, wrong-typed fields, and
  unsupported source shapes return `Err(Error{message})` with a
  `loadAs[T]: ...` prefix that locates the failure (e.g.
  `loadAs[Outer]: field 'inner': loadAs[Inner]: field 'age' missing`).
  `Option<T>` accepts JSON `null` as `Ok(None)` and any non-null shape
  as `Ok(Some(_))` (recursively coerced); a primitive source for
  `Option<Record>` errors with `loadAs[Option<X>]: expected null or
  loadAs[X]: expected JSON object`. Previously these targets crashed
  with `_Exit(1)` via the panic-version `unwrapFromAny`; the new
  `tryUnwrapFromAny` sibling routes Result-based propagation
  end-to-end. (#1852)
- Added three additive companion functions to `json.stringify`:
  - `stringifySafe(value: any) -> Result<str, Error>` and
    `stringifySafe(value: any, indent: int) -> Result<str, Error>` —
    same encoding as `stringify`, but inputs that would otherwise
    panic (non-finite floats, typed collections wrapped as `any`,
    `Set` / record / enum tags) return `Err(Error{message})` so
    callers can recover.
  - `stringifySorted(value: any) -> str` and
    `stringifySorted(value: any, indent: int) -> str` — emits
    `Map<str, any>` keys (including nested ones) in
    byte-lexicographic order so output is reproducible across runs
    that build the same logical map via different insertion
    sequences. Panic semantics match `stringify`.
  - `stringifySortedSafe(value: any) -> Result<str, Error>` and
    `stringifySortedSafe(value: any, indent: int) -> Result<str, Error>` —
    sorted-key output combined with the `Err`-on-unsupported-input
    behavior of `stringifySafe`.
  The existing `stringify` API is unchanged: signature, insertion-order
  iteration, and panic-on-unsupported-input behavior are all preserved
  for backwards compatibility. (#1853)
- `json.load(f: File) -> Result<any, Error>`,
  `json.dump(f: File, value: any) -> Result<Unit, Error>` /
  `json.dump(f: File, value: any, indent: int) -> Result<Unit, Error>`,
  and `json.loadAs[T](f: File) -> Result<T, Error>` File-handle
  overloads. `load(f)` fuses `io.readAll(f)? → load(text)?` and
  `dump(f, value [, indent])` fuses
  `stringify(value [, indent]) → io.writeText(f, ...)?` so callers can
  avoid the intermediate `str` buffer. `loadAs[T](f)` reuses the
  existing `any → T` coerce path (same supported `T` set as the str
  overload: `int` / `float` / `str` / `bool` / `List<any>` /
  `Map<str, any>`). Io errors (closed handle, write failure, etc.) are
  propagated as `Err(Error{message})` alongside the existing parse-error
  channel. Argument order follows the `io` module convention (File
  first), and the `dump` overloads accept `indent < 0` as a fall-through
  to compact form, matching `stringify(value, indent)`. (#1854)
- `io.open(path, mode)` now accepts `"ab"` (append binary), completing
  the binary-mode trio alongside `"rb"` / `"wb"` (added in #1848) and
  restoring parity with the text-mode triple `"r"` / `"w"` / `"a"`.
  `"ab"` maps to `O_WRONLY | O_CREAT | O_APPEND` (same POSIX flags as
  `"a"`); writes always go to end-of-file, the file is created if
  missing. The invalid-mode error message now reads `(must be "r",
  "w", "a", "rb", "wb", or "ab")`. (#1862)
- Generic user-defined functions can now be overloaded by argument
  type. Multiple `fn name<T>(...)` templates with the same name are
  allowed as long as their parameter signatures differ in arity or in
  concrete argument types, and the compiler picks the matching template
  at each call site via a two-pass resolution mirroring `@native`
  dispatch: Pass 1 requires exact type match, Pass 2 (only when Pass 1
  yields zero matches) accepts the widening conversions `u8 → int`,
  `u8 → float`, and `int → float` at top-level parameter positions.
  Nested element positions (`List<T>` / `Map<K, V>` / `Set<T>` / tuples
  / function types) stay exact regardless of pass. Ambiguous matches in
  either pass and no-match across the overload set produce dedicated
  diagnostics naming the function. Templates whose parameter signatures
  normalize identically after rewriting type variables to positional
  `__T0`, `__T1`, ... are rejected at declaration time as duplicates,
  catching alpha-equivalent redeclarations such as `fn id<T>(x: T)` /
  `fn id<U>(x: U)` that previously caused silent shadowing. Single-
  declaration code is unchanged. (#1874)

### Changed

- **BREAKING**: Redesigned the `json` module around the `any` type. The
  opaque `JsonValue` handle and its 13 low-level accessors (`parse`,
  `get`, `at`, `toStr`, `toInt`, `toFloat`, `toBool`, `kind`, `len`,
  `keys`, `stringify(JsonValue, ...)`, `jsonFree`) are removed without a
  deprecation period. The new API consists of four entry points:
  - `load(text: str) -> Result<any, Error>` parses JSON into a tag-typed
    `any` payload (`Null` / `Bool` / `Int` / `Float` / `Str` /
    `List<any>` / `Map<str, any>`).
  - `loadAs[T](text: str) -> Result<T, Error>` is a generic wrapper that
    parses and then coerces to `T` via the existing
    `let v: T = anyVal` slot-coercion path. Supported `T` in this
    release: `int` / `float` / `str` / `bool`, plus the element-erased
    forms `List<any>` and `Map<str, any>` (the parser materializes
    container payloads as `any`; element-typed collections such as
    `List<int>` / `Map<str, str>` are rejected by the generic
    substitution guard in `unwrapFromAny` and must be unwrapped
    element-by-element from `List<any>` / `Map<str, any>`). `T = record`,
    `T = Set<...>`, `T = Option<...>`, and `T = Result<...>` are not
    supported in this release and surface as a runtime type-mismatch
    `Err` from the coerce step.
  - `stringify(value: any) -> str` produces compact JSON.
  - `stringify(value: any, indent: int) -> str` pretty-prints with the
    given indent width (`indent < 0` falls back to compact form).
  Lifetime of the parsed payload is now driven by codegen's standard ARC
  machinery — the `jsonFree` early-return discipline is no longer
  required. Map iteration order for `stringify` is the underlying map's
  insertion order. Tags that JSON cannot represent (`Set`, `Record`,
  `Enum`, and `Map` keyed by non-`str`) panic from `stringify` since the
  return type is `-> str` and offers no `Result` channel. File-handle
  overloads (`load(f: File)` / `dump(value, f: File)`) are intentionally
  out of scope for this PR and will land alongside `io.File`. (#1698)
- **Breaking:** Changed the signature of the stdin `readLine()` builtin
  in `share/std/io/io.ry` from `() -> str` to
  `() -> Result<Option<str>, Error>`, mirroring the File-handle variant
  introduced in #1816. Previously `readLine()` returned `""` for both
  EOF and an empty input line, so callers could not distinguish "stdin
  closed" from "user pressed Enter on an empty line". The new shape
  returns `Ok(Some(line))` on success (trailing newline removed),
  `Ok(None)` at EOF, and `Err(e)` on I/O failure. Migration:
  ```ry
  # before
  from io import readLine
  name = readLine()
  print(f"Hello, {name}!")
  # after
  from io import readLine
  case readLine():
      Ok(opt):
          case opt:
              Some(name): print(f"Hello, {name}!")
              None: print("(EOF)")
      Err(e): print(e.message)
  ```
  The `input()` builtin is unchanged and still returns a bare `str`,
  so short scripts that do not need EOF distinction can continue using
  it; whether to give `input()` the same EOF distinction is tracked
  separately in #1868. (#1850)
- `json.load` / `json.loadAs[T]` parse errors now report position as
  `at line <L>, column <C> (offset <O>)` instead of the previous
  `at position <N>` byte-offset-only form. `<L>` and `<C>` are 1-based
  and `<C>` counts UTF-8 codepoints (matching Python's `JSONDecodeError`
  and typical editor column numbers); the original byte offset is
  preserved in parentheses for precision. Affects 11 position-bearing
  error sites in `runtime_json.cpp` (`expected '<c>'`, `unexpected
  character`, `unescaped control character in string`, `invalid number`,
  `leading zeros not allowed`, `number out of range`, `integer overflow`,
  `invalid literal`, `expected string key`). Position-less messages
  (`unexpected end of input`, `unterminated string`, `json: maximum
  nesting depth exceeded`, etc.) are unchanged. (#1851)
- **Breaking:** Changed the signature of the `input()` builtin from
  `() -> str` / `(prompt: str) -> str` to
  `() -> Result<Option<str>, Error>` /
  `(prompt: str) -> Result<Option<str>, Error>`, mirroring the
  stdin `readLine()` change in #1850. Previously `input()` returned
  `""` for EOF, an empty input line, and I/O errors alike, so callers
  could not tell them apart. The new shape returns `Ok(Some(line))`
  on a successful read (trailing newline removed), `Ok(None)` at
  EOF, and `Err(e)` on I/O failure. Migration:
  ```ry
  # before
  name = input("Name? ")
  print(f"Hello, {name}!")
  # after
  case input("Name? "):
      Ok(opt):
          case opt:
              Some(name): print(f"Hello, {name}!")
              None: print("(EOF)")
      Err(e): print(e.message)
  ```
  The `input()` builtin and stdin `readLine()` now share the same
  semantics; pick whichever fits the call site (no `import` for
  `input()`, explicit `from io import readLine` for the other). (#1868)
- `docker/run.sh` now bind-mounts each source directory and config file
  individually (`src/`, `include/`, `tests/`, `share/`, `CMakeLists.txt`,
  `CMakePresets.json`, `package.toml`, `.clang-tidy`,
  `.cppcheck-suppressions`) instead of bind-mounting the entire project
  root. The per-preset build directory (`build-docker/`,
  `build-asan-docker/`, etc.) is still mounted into its container
  counterpart, but host macOS native build dirs (`build/`, `build-asan/`,
  `build-fuzz/`) are no longer visible inside the container. This closes
  the cross-OS contamination path where macOS Mach-O binaries leaked
  through the outer `PROJECT_DIR:/workspace` mount alongside the inner
  Docker build dir and caused `clang-tidy` to fail when
  `compile_commands.json` listed `/Users/...` host paths. The
  `./docker/run.sh <preset> <args>` invocation interface is unchanged;
  adding a new top-level source or config file the build reads now
  requires updating `docker/run.sh` `MOUNT_ARGS` (and the matching
  `entrypoint.sh` guard) in the same PR. (#1876)
- `docker/entrypoint.sh` validates the container state at startup and
  fails fast on three contamination patterns: required source/config
  mounts are missing (exit 70, signals a `docker/run.sh` mount-list
  drift), a `BUILD_DIR/ry` or `BUILD_DIR/ry_tests` binary that is not
  ELF (exit 71, signals a macOS Mach-O leak into the container build
  dir), or `BUILD_DIR/compile_commands.json` listing `/Users/...`
  directories (exit 72, the symptom that previously broke `clang-tidy`).
  Each failure message names the host-side build dir to `rm -rf` for
  recovery, via the `RY_HOST_BUILD_DIR` environment variable that
  `docker/run.sh` now exports into the container. (#1876)
- **Breaking** — `json.load` is now typed-only. The pre-#1887
  non-generic overloads `load(text: str) -> Result<any, Error>` and
  `load(f: File) -> Result<any, Error>` have been removed because they
  exposed no safe accessor into the resulting `any`: callers had to
  reach the payload via an unchecked `xs: List<T> = v` cast that #1883
  later rejected at compile time. The remaining `loadAs[T]` API
  (#1852) was renamed to `load[T]`, consolidating the typed-deserialize
  path under a single name. Every JSON parse now picks an explicit
  type argument; `load[any]` is intentionally not supported and falls
  through `tryUnwrapFromAny`'s `non-record struct target not yet
  supported` rejection (use `load[Map<str, any>]` / `load[List<any>]`
  for the JSON-shape-typed equivalents). The error-message prefix
  produced by the coerce path flipped from `loadAs[...]: ...` to
  `load[...]: ...` to stay in sync with the API name. A direct call
  to `load(text)` without a type argument now emits a compile-time
  diagnostic that lists concrete-`T` alternatives instead of silently
  resolving to "undefined function: load". Migration:
  ```ry
  # Before (#1852-era)
  case loadAs[Map<str, int>](text):
    Ok(m): ...
    Err(e): ...
  # After (#1887)
  case load[Map<str, int>](text):
    Ok(m): ...
    Err(e): ...
  # Before (pre-#1852 untyped path — already discouraged after #1883)
  case load(text):
    Ok(v):
      m: Map<str, any> = v
      ...
  # After (#1887)
  case load[Map<str, any>](text):
    Ok(m): ...
    Err(e): ...
  ```
  (#1887)

### Fixed

- Fixed a use-after-free when storing a dynamically-allocated `str`
  (from `+` concatenation, `toString`, runtime construction, etc.) in
  an `any` value. Wrapping a `str` in `any` now retains the underlying
  `StringHeader` so the inner buffer outlives the source binding;
  unwrapping back to `str`, copying `any` to `any`, and reassigning
  `any` to a different `str` all emit symmetric retain/release calls.
  Literal-backed strings remain unaffected (they are marked
  `ARC_IMMORTAL` and the retain/release become no-ops). Previously,
  `let s = "a" + "b"; let a: any = s; s = "..."` left `a` pointing at
  freed heap; this is now sound. Closes the str half of the broader
  `any` ARC integration started in #1697. (#1799)
- `coerceResultType` now handles `Result<any, X>` → `Result<E, X>`
  coercion when the destination Ok or Err slot is an enum type (simple
  enum, `Option<T>`, `Result<T, E>`, or ADT). Previously the per-slot
  `unwrapFromAny` call passed an empty `targetTypeName`, and the
  `canAnyHoldType` gate rejected enum struct destinations entirely, so
  the coercion fell back to "type error: variable '...' cannot be
  reassigned to a different type" or silently took the wrong runtime
  branch. `coerceResultType` gains an optional `dstResTypeName`
  argument (defaulting to `""` for backward compatibility); the five
  call sites — `emitVarDecl`, function-local reassignment,
  module-global reassignment, and the two `mockReturnValueOnce`
  emitters — thread the destination's source-level Result type name so
  the Ok / Err slot can dispatch through the descriptor-driven enum
  unwrap path. Cross-type mismatches continue to trap at runtime via
  the existing `any enum type mismatch` diagnostic from
  `unwrapEnumFromAny`. The `canAnyHoldType` gate remains restricted to
  primitives, preserving the parallel-branch design from #1797 /
  #1798. (#1808)
- `json.stringify(any)` no longer reads out-of-bounds memory when `any`
  holds a typed (non-`any`-element) collection (`List<int>`,
  `Map<str, int>`, etc.). `wrapInAny` previously preserved the original
  collection header pointer untouched, so passing the resulting `any`
  to `stringify` walked the 8-byte-stride inner buffer as
  `RyAny[16]` — undefined behavior that could segfault. The runtime
  now records the source-level type name at `wrapInAny` time in a
  register-only side-table (`std::unordered_map<void *, std::string>`
  guarded by `std::mutex`, with an `std::atomic<size_t>` fast-path
  counter; entries are overwritten on re-register via
  `insert_or_assign`), and `stringify_any`'s List / Map arms look up
  the inner data pointer before walking the buffer. On hit the runtime
  exits deterministically with
  `stringify: any holds typed collection 'List<int>' — use List<any> /
  Map<str, any> / Set<any> instead` and `exit(1)`. ABI is unchanged;
  the happy path for `List<any>` / `Map<str, any>` pays nothing because
  the wrap arm only registers when at least one element type differs
  from `any`. (#1811)
- `emitRuntimeError` (the codegen helper that emits an `fprintf`
  diagnostic and aborts the program from JIT'd code paths such as
  `unwrapEnumFromAny`'s tag / descriptor mismatch trap) now calls
  `_Exit(1)` (C11) instead of `exit(1)`. `exit(1)` runs the libc
  atexit chain, which on Linux glibc invokes LLVM `ManagedStatic`
  destructors (`PassRegistry::~PassRegistry`, etc.) on a heap still
  referenced by the live JIT module — producing intermittent
  `free(): invalid pointer` SIGABRT before the `EXPECT_EXIT`-expected
  `ExitedWithCode(1)` is observed. The new
  `CodeGenTest.CoerceResultOkNestedResultMismatchTrapsArcPayload`
  death test exercises the same trap path with an ARC-bearing inner
  enum payload as additional regression coverage. ASan and the macOS
  libSystem malloc both masked the issue; the abort was only visible
  on the default Linux build. The C++ runtime helper used for normal
  program exit (`finalizeAfterPossibleJit` → `_exit(rc)`) already
  bypasses the same atexit chain, so this change closes the
  remaining JIT-triggered abort hole without altering user-visible
  `exit()` semantics. (#1838)
- Unified the JIT trap-path exit across the C++ runtime so it matches
  the codegen-emitted trap path from #1838. A new shared helper
  `ry_runtime_trap_exit()` in `include/ry/runtime_alloc.hpp` is now used
  by all 16 `exit(1)` sites that previously lived in
  `src/runtime_any.cpp`, `src/runtime_utf8.cpp`, `src/runtime_json.cpp`,
  and `src/runtime_regex.cpp`. The helper calls `fflush(stdout)` +
  `fflush(stderr)` then `std::_Exit(1)` — bypassing the `atexit` chain
  (and the LLVM `ManagedStatic` destructors that ran on the still-live
  JIT heap, causing `free(): invalid pointer` SIGABRT before the
  expected `ExitedWithCode(1)` was observed). `CodeGen::emitRuntimeError`
  in `src/codegen_call_user.cpp` was retrofitted to emit matching
  `fflush(stdout)` / `fflush(stderr)` IR calls immediately before the
  existing `_Exit(1)` IR call, so panic messages and any preceding
  `print` output survive even when stdio buffering was line-buffered
  to a pipe. A new CI `lint` step ("Check for banned direct exit()
  calls in runtime") blocks regressions by rejecting any direct
  `exit(...)` / `std::exit(...)` in `src/runtime_*.cpp`; allowed forms
  (`_Exit`, `_exit`, `quick_exit`, and the codegen helpers
  `getStdlibExit` / `getStdlibImmediateExit`) are not affected.
  (#1840)
- `io.open(path, mode)` now surfaces the detailed runtime error message
  on failure instead of the static string `"open failed"`. Concretely,
  `Err(e).message` now carries `"open: cannot open '<path>' in mode
  '<mode>'"` / `"open: invalid mode '<mode>'"` / `"open: path contains
  an embedded NUL byte"` / `"open: mode contains an embedded NUL byte"`
  (the strings set by `setLastError` in `__ry_io_file_open`). Previously
  `emitFileOpen` used `emitPtrToResult(..., "open failed", rk_file)`
  which embedded a static error string and discarded the runtime
  message. The fix switches to `wrapPtrAsResult(ptr)` (default
  `errFnName = "__ry_get_last_error"`) + explicit
  `addResourceKind(res, rk_file)`, matching the pattern already used by
  `emitFileReadAll` and other `io` functions. (#1847)
- Generic user-defined function dispatch on the **explicit type-args**
  path (`f[T1, T2, ...](args)`) now resolves the correct overload by
  substituting the type arguments into each candidate template's
  parameter signature and comparing the substituted signature against
  the call-site argument types. Previously this path hard-coded
  template index 0, silently routing every explicit-`[T]` call to the
  first declared overload regardless of arg types — for example
  `loadAs[int](file)` would route through the `loadAs[T](text: str)`
  body and fail with a confused type mismatch. The inferred-type-args
  path (`f(args)`) was already correct in #1874; this fix closes the
  remaining gap on the explicit path. Single-template programs are
  unaffected (the legacy templateIndex=0 fast path is preserved).
  (#1854)
- Fixed JIT unresolved symbol errors (`Symbols not found: [ ___ry_io_file_open ]`
  / `[ ___ry_write_text ]`) when a program imports only a subset of the
  `io` module that triggers a custom-emitter or inline codegen path —
  e.g. `from io import open` (without `close`), `from io import readAll`,
  `from io import lines`, `from io import writeText` (path-string
  overload). The custom emitters in `dispatchIO` (`emitFileOpen`,
  `emitFileReadAll`, `emitFileReadLine`, `emitFileLines`,
  `emitFileWriteText`) and the inline `writeText(path, content)` branch
  all bypass `emitTableDrivenNativeCall`, so the `sig.library`-driven
  `used_native_libraries_.insert("io")` never ran and the JIT failed to
  load `libry_io.dylib`. Only programs that also imported `close`
  happened to work, because `close` registers the library explicitly in
  `codegen_call.cpp`. `dispatchIO` now registers the `io` library once
  at the top so every dispatch path resolves correctly. (#1856)
- `net.accept()` and `net.tlsConnect()` now surface the detailed
  runtime error message on failure instead of the static strings
  `"accept failed"` / `"TLS connection failed"`. Concretely,
  `Err(e).message` now carries strings such as `"accept: timed out
  waiting for connection"`, `"accept: listener shut down"`,
  `"tlsConnect: cannot connect to host:port: <strerror>"`,
  `"tlsConnect: TLS handshake failed: <openssl>"`, and `"tlsConnect:
  certificate verification failed: <reason>"`. Previously `emitNetAccept`
  and `emitNetConnect` (TLS branch) used `emitPtrToResult(..., "...
  failed", rk_*)` which embedded a static error string and discarded
  the runtime message. The fix adds `setLastError` calls to
  `__ry_accept` (`runtime_net.cpp`) and `tls_handshake` /
  `__ry_tls_connect{,_resolved}` (`runtime_tls.cpp`, gated by a new
  `DEFINE_LAST_ERROR(tls)` thread-local channel exposed via
  `__ry_tls_get_last_error`), and switches both codegen sites to
  `wrapPtrAsResult(ptr, "__ry_<mod>_get_last_error")` +
  `addResourceKind(res, rk_<kind>)` — matching the pattern already used
  by `emitNetBind` / non-TLS `emitNetConnect` / `emitFileOpen`
  (#1847). (#1858)
- `json.load` / `json.loadAs[T]` now report `at line <L>, column <C>
  (offset <O>)` for the 16 parse-error sites in `runtime_json.cpp` that
  were left position-less by #1851 — `unexpected end of input`,
  `unterminated string`, `unterminated string escape`, `incomplete
  unicode escape`, `invalid hex digit in unicode escape`, `unpaired
  high surrogate in unicode escape`, `invalid low surrogate in unicode
  escape`, `unpaired low surrogate in unicode escape`, `invalid escape
  character '\X'`, `invalid number: expected digit after decimal point`,
  `invalid number: expected digit in exponent`, `json: maximum nesting
  depth exceeded`, `array too large`, `unterminated array`, `object too
  large`, `unterminated object`. Number errors point at the start of
  the offending number; unterminated containers point at the opening
  `"` / `[` / `{`; nesting-depth and escape errors point at the failing
  token. All `json.load` parse-error messages now carry the position
  suffix consistently. (#1882)
- The compiler now rejects assignments from an `any` whose source type
  is unknown into a typed collection (`List<T>` / `Map<K, V>` /
  `Set<T>` where `T` / `V` is not `any`). The motivating hazard was
  the pre-#1887 non-generic `json.load(text)` overload that returned
  `Result<any, Error>`: previously
  ```ry
  # Reproducible before #1883 against the pre-#1887 `load(text)` API.
  # Post-#1887 the non-generic overload is gone, but the same guard
  # still fires for any other Result<any, _>-returning source.
  case load(text):
    Ok(v):
      xs: List<str> = v   # compiled cleanly, crashed at runtime
  ```
  compiled without a diagnostic, then either segfaulted (`List<str>` /
  `List<float>` — the 8-byte typed stride walked off the end of the
  16-byte `RyAny` payload) or silently produced garbage (`List<int>`
  read the `RyAny` tag bytes as the payload). The same trap applied to
  `Map<_, T>` and `Set<T>`. `emitVarDecl` now uses the
  `source_type_name` metadata stamped by `registerAnyManagedVar` to
  distinguish the legitimate roundtrip
  `xs: List<int> = ...; a: any = xs; ys: List<int> = a` (allowed —
  stamped) from the `case Ok(v):` extraction whose binding has no
  collection element metadata (rejected — empty source name). The
  diagnostic suggests `load[T]` or per-element `case`, which were
  already the safe alternatives. `List<any>` / `Map<str, any>` /
  `Set<any>` annotations remain unconditional (the payload stride
  matches the destination). Round-trips whose source type is itself
  `any` (e.g. through a function returning `any`) are treated as
  ambiguous and deferred to `unwrapFromAny`'s runtime tag check,
  preserving shipped behavior. The same hazard in the reassignment
  path (`xs = v` after `xs: List<str>` is already declared) and across
  function argument / return boundaries with concrete-mismatched
  element strides is not yet covered and will be addressed in a
  follow-up. (#1883)
- Parser: `Ident<...>(args)` in expression position (e.g.
  `loadAs<int>("1")`) is now rejected with a dedicated diagnostic that
  directs users to the canonical `[T]` generic-call syntax
  (`f[int](x)`), instead of silently misparsing the form as a chain of
  comparison operators and surfacing a misleading
  `undefined variable: <name>` error. `Foo<T>::Variant` enum
  constructors and plain comparison chains (`a < b > c`) are
  unaffected. The runtime `loadAs[T]: ...` error-message prefix and
  the `tests/spec/json.test.ry` describe/it names are unified with the
  `[T]` user-facing notation. (#1885)
- Linux CI flake (~5-10 %) where `tests/spec/collection_meta_propagation.test.ry`
  printed `25 passed, 0 failed` and then SIGABRT'd inside `~FnStmt()`
  with glibc tcache assertions (`malloc(): invalid next->prev_inuse` /
  `corrupted size vs. prev_size`). The parsed AST (`Program prog` in
  `runRySource`) was destructed via stack unwind after JIT execution
  had already disturbed the heap — `~Program()` walks
  `vector<StmtNode>` → `unique_ptr<FnStmt>` → `~FnStmt()` over lambda
  body / capture chains, triggering glibc 2.40's tcache integrity
  check on freed chunks the JIT had touched. The fix extends the
  existing LLJIT / CodeGen teardown suppression in
  `src/jit_runner.cpp` with a sixth step: `new Program(std::move(prog))`
  inside the existing `#if defined(__linux__) || defined(__APPLE__)`
  block, so the AST is intentionally leaked alongside the LLJIT and
  CodeGen instances. The OS reclaims memory on process exit. Same
  #1187 family workaround; root cause in LLVM ORC / JITLink heap
  patterns is still unidentified upstream. macOS Docker did not
  reproduce locally (50/50 then 200/200 PASS), so the fix is validated
  via the mechanistic argument that `~FnStmt()` no longer runs after
  JIT teardown, with CI statistics as the post-merge oracle. (#1895)

## [0.0.24] - 2026-05-18

### Added

- Added `mockReturnValueOnce(name, value)` to the testing framework for
  Jest-compatible per-call queued mock returns. Each call to
  `mockReturnValueOnce` enqueues `value` for the named function; the next
  call to that function dequeues and returns the head of the queue.
  When the queue empties, calls fall back to the function set via
  `mock(name, replacement)` (if any), then to the original implementation
  — matching Jest's fallback chain. All return types are supported
  (primitives, `str`, `List` / `Map` / `Set`, records, `Result`,
  `Option` including bare `None`). The first argument must be a string
  literal naming the function; overloaded functions, unknown names,
  Unit-returning functions, and value type mismatches are rejected at
  compile time. `mockClear(name)` preserves the queue (only resets the
  call counter), while `mockReset(name)` and `mockResetAll()` discard
  the queue. Queues are auto-cleared at the end of each `it` block.
  Note: `verify(name)` counts only queue-served and default-mock-served
  calls; calls that fall through to the original implementation are not
  counted (matching `mockReset` semantics). (#1681)
- Added per-overload mocking, spying, and verification for overloaded
  functions across the testing framework. The mock registry is now
  keyed by canonical signature `"name(T1, T2)"` instead of bare name,
  so each overload has an independent slot. `mock` /
  `mockReturnValueOnce` / `spy` / `verify` / `verifyCalledWith` /
  `mockClear` / `mockReset` all accept the signature-form string
  (e.g. `mock("add(int, int)", ...)`, `verify("digits(int, int)")`).
  Custom-emitter `@native` overloads — including the math overload
  set (`abs`, `floor`, `ceil`, `round`, `log`, `pow`, `digits`) —
  are now mockable / spy-able via the same signature form; argument
  recording for `verifyCalledWith` on those natives is not supported
  in v1 (count-based `verify` works). Whitespace inside the signature
  is normalized; type aliases are resolved automatically. (#1682)
- Added `spy(name)` to the testing framework for recording calls
  without replacing the implementation. Unlike `mock(name, replacement)`
  which fully replaces the function body, `spy` keeps the original
  implementation running and only adds call-count and argument-recording
  instrumentation around it. The argument is the function's name as a
  string literal; overloaded functions and non-existent names are
  rejected at compile time. `verify(name)` and
  `verifyCalledWith(name, args...)` work uniformly on spied functions
  (same internal call-recording registry as `mock`), and
  `mockClear(name)` / `mockReset(name)` / `mockResetAll()` apply
  identically. A function may be both mocked and spied across different
  `it` blocks; when both are active in the same block, `mock` takes
  precedence and the real implementation is bypassed. Spy registrations
  are automatically cleared at the end of each `it` block. (#1683)
- Added `mockClear(name)`, `mockReset(name)`, and `mockResetAll()` to
  the testing framework for partial mock state reset within an `it`
  block (Jest / Vitest compatible). `mockClear` resets the call count
  while keeping the mock active; `mockReset` removes a single mock and
  restores the original implementation; `mockResetAll` removes every
  mock currently registered, equivalent to the automatic cleanup that
  runs at the end of each `it` block but explicit and usable
  mid-block. All three accept the function name as a string (same
  convention as `verify`) and are no-ops when the name is not
  currently mocked. (#1684)
- Added IEEE 754 special-value matchers `toBeNaN()`, `toBeInfinity()`,
  and `toBeFinite()` to the testing framework. Because `NaN == NaN` is
  false in IEEE 754, `expect(0.0/0.0).toEq(NAN)` always failed and
  tests had to rely on indirect idioms such as
  `expect(x == x).toBeFalse()`. The new matchers express the intent
  directly: `expect(0.0/0.0).toBeNaN()`,
  `expect(1.0/0.0).toBeInfinity()` (matches both `+∞` and `-∞`), and
  `expect(3.14).toBeFinite()`. All three accept `float` only and emit
  a `codegenError` for other types. Complements stdlib `math.isNan` /
  `math.isInf` (assertion vs. conditional branch). (#1685)
- Added `@beforeEach` / `@afterEach` / `@beforeAll` / `@afterAll`
  lifecycle hook directives for the testing framework. Each hook is
  declared on a parameterless, return-typeless function inside a
  `@describe` block and runs at the corresponding point in the
  describe's lifecycle: `@beforeAll` once before the first `@it`,
  `@beforeEach` before every `@it`, `@afterEach` after every `@it`
  that completes normally, and `@afterAll` once after the last `@it`.
  Hook bodies are inlined into the describe scope rather than emitted
  as standalone functions, so they may freely read and reassign
  describe-scope variables (`@it` bodies, by contrast, capture those
  variables read-only). `@describe` bodies execute once, so
  `@beforeEach` mutations accumulate across tests — write an explicit
  reset if per-test isolation is required. Constraints: at most one
  hook of each kind per describe; lifecycle hooks cannot coexist with
  `@it` / `@describe` / `@timeout` / `@skip` / `@only` / `@todo` /
  `@each` / `@property` on the same function; hooks declared outside
  a `@describe` are rejected; and hook bodies cannot contain
  top-level declarations (`fn` / `record` / `enum` / type alias /
  directive / `import`) because re-emission per `@it` would
  duplicate-register them. Known limitation: a test fired by
  `@timeout` unwinds via `siglongjmp` past the inlined `@afterEach`
  body, so cleanup runs only on normal completion. (#1686)
- Added `@skip`, `@only`, and `@todo` testing directives for
  individual test selection within a file. `@skip @it("...")` skips
  the test entirely and counts it as `skipped`. `@only @it("...")`
  causes every non-`@only` test in the same file to be implicitly
  skipped — useful for focused TDD on a single failing case.
  `@todo @it("...")` is a placeholder that never emits a body (so
  the function may reference undefined identifiers and still
  compile) and counts as `todo`. All three directives compose with
  `@each` and `@property` and are rejected on `@describe` in this
  release (MVP scope; tracked for future expansion). The test
  summary now always prints the 4-item form
  `N passed, M failed, K skipped, T todo`; only `failed` influences
  the exit code. Outline mode (`ry test --outline`) renders the
  directive as a suffix, e.g. `it foo (@skip)`,
  `it foo (@only @each)`. Mutual combinations
  (`@skip @only`, `@skip @todo`, `@only @todo`) are codegen errors.
  (#1687)
- Added `@timeout(ms)` testing directive for per-test millisecond-precision
  timeouts. `@timeout(N) @it("...")` aborts the test if its body runs
  longer than `N` milliseconds, marks it as `failed` with a
  "(timeout after Nms)" suffix, and continues execution with the next
  test. This replaces the previous "alarm + `_exit(124)`" behavior for
  affected tests, which terminated the entire test process and lost all
  subsequent test results. The `ms` argument must be a **positive integer
  literal**; zero, negative, non-literal, or non-integer arguments are
  rejected at compile time. Combining `@timeout` with `@each` or
  `@property` is a compile error in this release (MVP scope). The timer
  is delivered via `setitimer(ITIMER_REAL, ms)` and `siglongjmp` from the
  signal handler — the test runner stays single-threaded and TSan-safe.
  Known limitation: on timeout, ARC release is skipped for objects
  allocated inside the test body (leaks are reclaimed at process exit
  but may be flagged by leak detectors). (#1688)
- Added ergonomic matchers `toBeBetween(min, max)` and
  `toBeOneOf(list)` to the testing framework. Both express common
  assertion patterns that previously required verbose combinations:
  `expect(x).toBeBetween(1, 10)` replaces
  `expect(x).toBeGreaterThanOrEq(1)` plus
  `expect(x).toBeLessThanOrEq(10)`, and
  `expect(status).toBeOneOf([200, 201, 204])` replaces the
  argument-order-reversed `expect([200, 201, 204]).toContain(status)`.
  `toBeBetween` is inclusive on both bounds and accepts `int` /
  `float` operands (mixed int/float is allowed); `toBeOneOf` accepts a
  `List` whose element type matches the actual value (`int`, `float`,
  `str`, or `bool`). Both emit `codegenError` for type or shape
  mismatches. (#1689)
- Added Troubleshooting, Recipes, and Best Practices sections to
  `docs/reference/testing.md` covering common errors (missing
  `from testing import`, `verify` returning 0, `toEq` vs `toBeCloseTo`
  for floats, `@afterEach` skipped on `@timeout`,
  `@each` / `@property` + `@timeout` compile error), worked patterns for
  `mockReturnValueOnce` / `spy` / `toBeCloseTo` / `@property` /
  overloaded mock, and conventions to prevent footguns
  (`@only` in committed code, mock scope, `verify` paired with
  behavioral assertion, `@beforeAll` weight, `should ...` form). (#1783)
- Added a `Feature interactions` section to
  `docs/reference/testing.md` documenting how v0.0.24 testing features
  combine: `@beforeAll` / `@afterAll` with `@each` / `@property`
  (parameterized-aware lifecycle), `mock` / `spy` installed from
  `@beforeEach` (fresh per-`it` state via auto-restore), mutually
  exclusive combinations (`@beforeEach` / `@afterEach` with `@each` /
  `@property`, `@timeout` with `@each` / `@property`) with verbatim
  compile-error messages, and nested-`@describe` lifecycle
  (hooks are describe-local, not inherited). Adds
  `tests/spec/feature_combinations.test.ry` covering the four supported
  combinations, plus verbatim error text in
  `docs/reference/directives.md` for the `@timeout` mutual exclusion.
  (#1784)
- Added two follow-up Recipes to `docs/reference/testing.md`:
  "Per-test mock setup with `@beforeEach`" (reusing the
  `mockInBeforeEach` fixture from
  `tests/spec/feature_combinations.test.ry`) and
  "Setup patterns for `@each` parameterized tests" (backed by new
  `tests/spec/parameterized_lifecycle.test.ry`, covering both
  inline per-iteration setup and `@beforeAll` hoist workarounds for
  the `@each` + `@beforeEach` compile-error case). Completes the
  recipes deferred from #1783. (#1788)

### Changed

- Bare-name semantics for the testing API on overloaded functions are
  defined as follows (no change for single-overload functions):
  `mock(n, repl)` auto-dispatches when the replacement lambda's
  signature uniquely matches one overload, otherwise errors with the
  candidate list; `mockReturnValueOnce(n, v)` errors (return-value
  alone cannot disambiguate); `spy(n)` registers spy for **all**
  overloads aggregately; `verify(n)` returns the **sum** of call
  counts across all overloads; `verifyCalledWith(n, ...)` dispatches
  to the arity-matching overload or errors when ambiguous;
  `mockClear(n)` / `mockReset(n)` clear / remove every overload. As
  a consequence, if existing code calls `verify("foo")` and `foo`
  later gains a second overload, the return value silently becomes
  the aggregate count — switch to `verify("foo(int)")` to preserve
  per-overload counting through such a change. (#1682)

### Fixed

- Fixed the formatter dropping extra arguments on `expect` matchers
  with more than one argument. Previously `expect(x).toBeCloseTo(1.0,
  4)` was reformatted as `expect(x).toBeCloseTo(1.0)`, silently
  discarding the `decimals` argument; the same gap would have affected
  the new `toBeBetween(min, max)`. The formatter now emits every
  argument in `ExpectStmt.extra_args` alongside the primary
  `expected`. (#1689)
- Fixed undefined behavior in `expect(x).toContain(y)` and
  `expect(x).toNotContain(y)` where pointer-typed list/set elements
  were unconditionally compared with `strcmp`. Under opaque pointers
  `elemTy == ptrTy_` matches not only `List<str>` / `Set<str>` but
  also `List<List<T>>` / `List<Map<K, V>>` / `List<Set<T>>` /
  `List<fn>` / `Set<List<T>>` / `Set<Map<K, V>>` / `Set<fn>`, so the
  previous code read the bytes of a collection / closure header as a
  C string — UB that could silently report two distinct length-N
  lists as "equal" because their headers begin with the same length
  prefix. These shapes are now rejected at compile time with a clear
  diagnostic (`list element type must be int, float, str, or bool` /
  `set element type must be int, float, str, or bool`), mirroring the
  positive-allowlist guard previously applied to `toBeOneOf`
  (#1689) and `emitListRemove`. (#1763)
- `mock()` / `spy()` pre-scan now walks lambda bodies inside every
  `ExprPtr` slot, not just `CallStmt.args`. Targets defined inside a
  lambda stored in `AssignStmt.value`, `ReturnStmt.value`,
  `CallExpr.args` (nested at any depth), `IfStmt.condition`, or any
  other AST position are now detected, so the mock dispatch gate fires
  for callsites compiled before the lambda runs. (#1765)
- Fixed a JIT crash / use-after-free when calling a higher-order
  function whose return value is an `fn(...) -> T` typed value loaded
  from a parameter (e.g. `fn pick(f: fn() -> Unit) -> fn() -> Unit:
  return f` invoked inline and then called via the bound local).
  Fn-typed parameter allocas are not registered in `arc_managed_vars_`
  because callers own the uniform-closure wrap temp via
  `releaseUniformClosureTemps`. Returning such a value out of the
  callee made the caller's post-call release free the storage while
  the caller still held the returned handle. A new
  `retainFnTypedParamForReturn` helper, called from `emitStmt(ReturnStmt)`,
  retains the value when the source alloca's metadata flags it as a
  uniform-closure fn-typed parameter; non-return load sites
  (pass-through fn args, two-level nesting) are unaffected. (#1770)

## [0.0.23] - 2026-05-15

### Added

- Added symbol alias support in selective import (`from m import foo as bar`).
  The parser now accepts an optional `as <ident>` after each imported name,
  the formatter emits it round-trip, and self-alias (`foo as foo`) is
  normalized away (#1721).
  In this release only `@const` aliases are functional end-to-end; alias
  requests for `fn` / `record` / `enum` / `type alias` parse and reach the
  module loader but are rejected with a clear diagnostic pointing at
  follow-up #1725, which will extend codegen-side name resolution to make
  the remaining kinds work. (#1721)
- Added braced selective import syntax: `from x import { a, b }` and
  `from x import { a as b, c }`. Both single-line and multi-line forms
  are accepted, with an optional trailing comma. The new form parses to
  the same `ImportStmt` AST as the existing `from x import a, b` form,
  so semantics (including #1721 symbol aliases) are unchanged. Empty
  braces (`from x import {}`) are rejected with
  `expected import name after '{'`.
  The tree-sitter grammar accepts braced single-line imports; brace-
  internal newline suppression for the multi-line form is tracked in
  #1727 alongside the same gap for list / map / set literals. (#1722)
- Added qualified import syntax: `import <module>` binds the module
  itself, and members are accessed via `<module>.<name>` (e.g.
  `import math; math.sqrt(2.0)`, `math.PI`). Qualified and selective
  imports compose — `import math` and `from math import PI` may both
  appear in the same file. Qualified import is the recommended way to
  resolve name collisions between modules: `from str import contains`
  alongside `import list` lets the importer use `contains(...)` for the
  string version and `list.append(...)` for the list version without
  ambiguity. v0.0.23 supports qualified import for standard library
  modules only; user-defined modules continue to use
  `from <mod> import ...`. Constraints: single-identifier modules only
  (`import a.b` is rejected, use `from a.b import ...`); the
  `import <mod> as <local>` alias form is parsed but rejected with a
  pointer to the follow-up issue
  [#1724](https://github.com/t0k0sh1/ry/issues/1724); duplicate
  `import` of the same module in a file and local bindings that shadow
  an imported module name are both parse errors. (#1723)
- Extended qualified import (#1723) with the alias clause:
  `import <module> as <local>` registers `<local>` as the effective
  module name, so `import math as m` makes `m.sqrt(2.0)` and `m.PI`
  work. The alias **replaces** the original name (Python-style): bare
  `math.sqrt(2.0)` after `import math as m` is no longer routed to the
  qualified-call path. The alias must be camelCase, and two imports
  whose effective names collide (e.g. `import math as m` followed by
  `import path as m`) are a parse error. The original module name is
  preserved internally so user-defined-module rejection diagnostics
  still cite the actual file (`from mymod import greet`, not
  `from m import greet`). (#1724)
- Extended `from m import foo as bar` symbol alias to `fn`, `record`,
  `enum`, and `type alias` kinds. The module loader now generates an
  `ImportAliasStmt` AST node per non-`@const` alias, and codegen
  registers the alias under the existing function / record / enum /
  type-alias tables so every call site, type annotation, constructor,
  enum variant access, and ADT pattern match resolves transparently
  through the alias name (#1725).
- Generic-fn and generic-enum aliases are explicitly rejected at codegen
  with a "not yet supported" diagnostic; aliases for non-`@const`
  mutable globals and `@directive` definitions remain rejected by the
  module loader (#1725).
- Extended qualified import (#1723 / #1724) to user-defined modules: after
  `import usermod`, the qualified forms `usermod.foo()`, `usermod.PI`, and
  `usermod.MyRecord(...)` resolve through a per-module namespace bucket on
  `CodeGen`, replacing the previous "throwaway Program" carve-out that
  outright rejected qualified calls into user-defined modules. The
  selective form (`from usermod import foo`) continues to share the same
  loader cache, so mixing both in one file (`import usermod` followed by
  `from usermod import foo`) reuses the AST without re-parsing. Bare-name
  leak isolation is preserved: `import usermod` alone never exposes
  `foo()` as a top-level identifier. (#1730)

### Changed

- Generic functions, `enum` declarations, and `type` aliases inside a
  user-defined module reached through qualified import are now rejected
  at codegen with an actionable diagnostic suggesting
  `from <module> import ...`. These constructs route through flat tables
  (`generic_fn_templates_` / `enum_types_` / `type_aliases_`) that the
  per-module namespace bucket cannot intercept; surfacing the limitation
  early avoids silent bare-name leaks. (#1730)
- CI `scan-build` job now analyses only the `ry` target on pull requests
  for faster feedback (~76 TU instead of the default `all` target,
  which previously also dragged in `ry_tests`, `ry_<pkg>` native shared
  libraries, and fuzz harnesses); the full all-target scan is retained
  for `push` to `main` so mainline keeps the wider coverage. Both
  invocations now pass `--parallel`. (#1738)
- CodeQL Advanced workflow's c-cpp matrix Build step now compiles only
  the `ry` target on pull requests for faster feedback (~76 TU instead
  of the default target, which previously also dragged in `ry_tests`,
  `ry_<pkg>` native shared libraries, and fuzz harnesses); the full
  default-target build is retained for `push` to `main` and
  `workflow_dispatch` so the Code Scanning dashboard and the release
  `codeql-gate` keep the wider coverage. The `cmake --build` invocation
  now also passes `--parallel`. (#1740)
- CI `clang-tidy` job's `Build` step now compiles only the `ry` target
  on pull requests for faster feedback (~76 TU instead of the default
  `all` target, which previously also dragged in `ry_tests`,
  `ry_<pkg>` native shared libraries, and fuzz harnesses); the full
  all-target build is retained for `push` to `main`. The `cmake --build`
  invocation now also passes `--parallel`, and the `Run clang-tidy`
  step now parallelises per-TU via `xargs -0 -n 1 -P "$(nproc)"`
  instead of running clang-tidy sequentially (`-n 1` is required —
  otherwise xargs batches every `.cpp` path into a single clang-tidy
  invocation and the `-P` flag does nothing). The PR `--target ry` narrows only
  the build step — clang-tidy still analyses every `src/*.cpp`
  (90 files) in both event modes. (#1741)
- `ry foo.ry` (a bare filename with no path separator) is now rejected with
  an actionable error when `foo.ry` exists in the current working directory:
  `Error: ambiguous script path 'foo.ry'. Use './foo.ry' or an absolute
  path.` Previously the bare form silently bypassed referrer-directory
  resolution, causing scripts that used relative imports
  (`from .sub import ...`) to fail with `relative import requires a
  referrer directory`. Use `./foo.ry` or an absolute path instead; bare
  filenames that do not exist in the current directory are still resolved
  through `package.toml` `[paths]` as before. (#1745)
- `pre-commit-checklist` §3.6 (libFuzzer) reframes the crash-handling
  policy to match §3.5 (TSan): hard-to-reproduce crashes (sanitizer /
  fuzz output that does not reliably reproduce locally) are now fixed
  in the current PR to capture the reproduction window, instead of
  being deferred to a separate issue. This aligns with the new
  `/triage-side-finding` Q1 short-circuit (formerly `/scope-out-issue`)
  and removes the prior asymmetry where TSan races required immediate
  fixes but libFuzzer crashes were routed to follow-up issues. Crash
  inputs are still saved to `tests/fuzz/regressions/<name>/` and
  `tests/fuzz/corpus/<name>/` regardless of fix timing. (#1752)

### Fixed

- Renamed the TU-local `struct ry::Parser` in `src/runtime_json.cpp` to
  `JsonParser` to remove a latent ODR collision with the public
  `class ry::Parser` declared in `include/ry/parser.hpp`. The collision
  was benign while both implicit destructors were trivially equivalent,
  but became a crash (`AddressSanitizer: unknown-crash` inside
  `__ry_json_parse`) on Linux libstdc++ once `ry::Parser` grew a
  non-trivial member as part of the qualified-import work in this PR.
  (#1723)
- tree-sitter grammar now accepts newlines inside brace-delimited
  expressions: multi-line `list_literal` (`[
  1,
  2,
]`),
  `map_literal` (`{
  "a": 1,
}`), `set_literal` (`{
  1,
  2,
}`),
  and braced selective import (`from std.io import {
  print,
}`)
  no longer produce `(ERROR (UNEXPECTED '
'))`. The fix is contained to
  `editor/tree-sitter/grammar.js` via a new `bracedSep1` helper that
  absorbs the external `_newline` token around list separators and at
  the brace boundaries; `_indent` / `_dedent` are intentionally not
  absorbed so the scanner's indent stack stays clean. This mirrors the
  C++ parser's `skipStructuralTokens` (`src/parser.cpp:352`) and the
  Phase 2 corpus gains four `#1727` cases under `imports` / `literals`
  including a function-body nesting case that exercises indent-stack
  health. Out of scope (still produce ERROR for multi-line forms):
  `tuple_literal` / `_parenthesized` / `argument_list` / `parameter_list`
  / `case_*` arm bodies. (#1727)
- Calling `<mod>.fn(...)` or accessing `<mod>.field` where `<mod>` is the
  name of a registered stdlib module (e.g. `math`, `json`, `path`) but
  was not introduced via `import <mod>` now produces an actionable error:
  `module 'math' is not imported (add 'import math' at the top of the
  file)`. Previously the qualified call fell through to UFCS conversion
  (`math.sqrt(4.0)` → `sqrt(math, 4.0)`) and codegen surfaced a
  misleading `undefined function: sqrt (hint: forward references...)`
  diagnostic that pointed users away from the root cause. The check
  fires at codegen-dispatch time so a local variable that happens to
  share a stdlib name (e.g. `path: str = "/tmp"; path.basename()`)
  shadows the package and the existing diagnostic path is preserved.
  Bare unqualified calls (`sqrt(4.0)` without `from math import sqrt`)
  are out of scope and continue to use the forward-reference hint.
  (#1746)
- When a stdlib module is imported under an alias (e.g. `import math as
  m`), the canonical name is hidden per the Python-style contract
  (`docs/reference/modules.md`). Writing bare `math.sqrt(...)` /
  `math.PI` after such an import now produces a targeted suggestion:
  `'math' is not defined. Did you mean 'm' (aliased from 'math')?`.
  Previously the diagnostic from #1746 fired with the generic
  `module 'math' is not imported (add 'import math' ...)` message,
  which was misleading because the user had already imported the
  module — just under a different name. The unaliased case
  (`math.sqrt(4.0)` with no `import math` at all) keeps the original
  hint unchanged. (#1747)
- `from x import *` and other wildcard import positions now produce a
  clear, actionable diagnostic instead of the misleading
  `expected function name after 'import'` message. The new error reads
  `selective import does not support wildcards ('from x import *');
  use 'from x import a, b' or 'from x import {a, b}' instead` and
  fires uniformly across all four wildcard positions:
  `from x import *`, `from x import {*}`, `from x import a, *`, and
  `from x import {a, *}`. Wildcard import remains intentionally
  unsupported; whether to add it is tracked separately. (#1748)
- `ry test` / `ry run` / `ry -c` on Linux and macOS now bypass the C++
  static-destructor chain via `_exit(rc)` after a successful JIT run
  (gated on a `jitWasInitialized()` flag set inside
  `src/jit_runner.cpp` immediately after `LLJITBuilder().create()`
  succeeds). The existing triple-stage leak (`rtCleanup.release()`,
  `(void)jit.release()`, `(void)cg.release()`) already suppresses the
  `~LLJIT()` / `~CodeGen()` frames of the #1187 / #1657 LLVM ORC
  teardown family, but residual LLVM `ManagedStatic` / `llvm_shutdown`
  state run from `atexit` handlers intermittently aborted inside glibc
  `_int_malloc` heap consolidation (exit 134) after the test result
  had already been printed. Non-JIT exits (help printing, formatter,
  parse-time errors before any LLJIT instance is created) still run
  normal C++ teardown. (#1749)
- The `scan-build` CI job no longer fails when GitHub's artifact API
  returns a transient HTML error page instead of JSON during
  `actions/upload-artifact@v4` retry exhaustion. The
  `Upload scan-build report` step now carries `continue-on-error: true`,
  mirroring the warn-only posture of the sibling `Build + Analyze` step
  — both are best-effort because the scan-build findings backlog is
  still being triaged, and an artifact-upload transient should not
  fail-close the job. (#1750)

## [0.0.22] - 2026-05-10

### Added

- Imported the tree-sitter grammar from the standalone `tree-sitter-ry`
  repository into this repository under a new editor-agnostic layout:
  `docs/grammar.ebnf` is now the canonical grammar specification (single
  source of truth) and `editor/tree-sitter/` holds the tree-sitter
  implementation (`grammar.js`, `src/scanner.c`, `queries/highlights.scm`,
  `tree-sitter.json`, `build.sh`, `install.sh`). Generated artifacts
  (`parser.c`, `grammar.json`, `node-types.json`, runtime headers,
  `bindings/`) are reproducible via `editor/tree-sitter/build.sh` and are
  excluded from version control. (#1614)
- Added `editor/tree-sitter/check.sh` and `editor/tree-sitter/expected-fail.txt`
  as a Phase 1 corpus smoke-check for the in-tree tree-sitter grammar.
  `check.sh` runs `tree-sitter parse` against every
  `tests/spec/**/*.test.ry` and treats any `ERROR` / `MISSING` node as
  a regression unless the file is listed in `expected-fail.txt` — the
  single place where tolerated divergence is recorded. Files that move
  out of the gap list automatically surface a `WARN: ... now passes`
  on the next run so the entry can be retired in the same PR. The
  initial `expected-fail.txt` clusters the 41 currently failing fixtures
  into six named buckets (tuple member access, generic syntax variants,
  lambda-block bodies, numeric literal forms, async / decorator /
  operator-overload declarations, and other surface gaps).
  `pre-commit-checklist` §3.6.5 now invokes `./check.sh --no-build`
  alongside the existing `build.sh` + `install.sh --no-build` gate.
  Phase 2 (hand-curated `tree-sitter test` corpus with S-expression
  assertions) remains tracked in #1633. (#1617)
- Added `editor/tree-sitter/queries/indents.scm` with the
  nvim-treesitter rewrite capture vocabulary (`@indent.begin` /
  `@indent.branch`) so Neovim 0.12+ users get tree-sitter-driven
  auto-indent / auto-dedent for `.ry` files: `<CR>` after `fn foo():`
  / `if cond:` bumps +1 indent, `else` / `else if` on its own line
  dedents to the parent `if`, and `]` / `}` / `)` on its own line
  returns to the opener's column. Multi-element tuples, list / map /
  set literals, and call / index argument lists are also handled.
  Known limitation: a parenthesized single expression spanning multiple
  lines (e.g. `s = (
  1
  + 2
)`) is parsed via the hidden
  `_parenthesized` grammar rule, which tree-sitter inlines into the
  parent and cannot be matched by capture queries — contents are not
  bumped and the closing `)` does not auto-dedent.
  `editor/tree-sitter/install.sh` now also deploys `indents.scm` to
  `$XDG_CONFIG_HOME/nvim/queries/ry/indents.scm`. Enable per-buffer with
  `vim.bo.indentexpr = "v:lua.require'nvim-treesitter'.indentexpr()"`.
  (#1620)
- Added `editor/tree-sitter/test/corpus/` with the Phase 2 hand-curated
  `tree-sitter test` corpus (53 cases across 8 files: imports, functions,
  literals, expressions, control flow, case match, lambdas, decorators).
  Each case pairs a Ry snippet with its expected S-expression so that
  grammar edits which silently change parse-tree shape are caught — a
  capability the Phase 1 ERROR/MISSING smoke-check (`check.sh`, #1617)
  cannot provide. Run with `tree-sitter test` from
  `editor/tree-sitter/`. Coverage scope is limited to grammar surface
  area that already parses cleanly today; gaps listed in
  `expected-fail.txt` are intentionally excluded so the harness stays
  green and shape regressions are unambiguous. (#1633)
- `expect(actual).toBeCloseTo(expected)` /
  `expect(actual).toBeCloseTo(expected, decimals)` — new test matcher
  for float approximate equality, modeled on Jest's `toBeCloseTo`.
  Asserts `|actual - expected| < 0.5 * 10^-decimals`, which makes
  `expect(0.1 + 0.2).toBeCloseTo(0.3)` pass even though strict
  `toEq` (FCmpOEQ) does not. `decimals` defaults to `2` and must be
  a non-negative integer literal in `[0, 15]` (the upper bound is
  tied to practical `f64` precision; larger values no longer
  provide meaningful decimal-place guarantees because adjacent
  representable doubles differ by more than `0.5 * 10^-decimals`).
  Both
  `actual` and `expected` accept `int` or `float`, and mixed
  combinations (e.g. `expect(1).toBeCloseTo(1.0)`) are promoted to
  `f64` before comparison. Non-numeric operands and non-literal
  `decimals` are rejected at compile time. (#1675)
- `expect(actual: str).toMatch(pattern: str)` — new test matcher for
  regex string matching, modeled on Jest's `toMatch`. Internally calls
  `__ry_regex_is_match` (unanchored search), so `expect("hello world")
  .toMatch("world")` and `expect("v1.2.3").toMatch("^v\d+\.\d+\.\d+$")`
  both pass. Both `actual` and `pattern` must be `str`; passing any
  other type to `actual` (e.g. `expect(42).toMatch(...)`) is rejected
  at compile time. An invalid regex pattern (e.g. `toMatch("(")`) is
  surfaced as a runtime panic with the regex engine's error message,
  matching the behavior of other `regex` stdlib calls. (#1676)
- `verifyCalledWith(name: str, args...) -> int` testing intrinsic.
  Returns the number of recorded mock calls whose arguments exactly
  match `args...`, complementing `verify()` which counts all calls
  regardless of arguments. The function name must be a string literal
  so that the compiler can validate the remaining argument types
  against the original function's signature. v1 supports `int`,
  `float`, `bool`, and `str` arguments; `List<T>` / `Map<K, V>` /
  `Set<T>` / record / tuple / function-typed arguments are rejected
  at compile time and tracked for follow-up. Requires
  `from testing import verifyCalledWith`. (#1677)
- Capture-based closures can now be used as the replacement passed to
  `mock(target, replacement)`. The closure may read or mutate
  variables from the enclosing scope, which is the canonical pattern
  for recording call history (e.g. appending arguments to a captured
  `List<int>`). The captured environment is retained when the mock is
  registered and released automatically when the `it` block ends.
  (#1678)
- `verifyCalledWith(name, args...)` now accepts `List<T>` arguments
  where `T ∈ {int, float, bool, str}`. The recorded call's list is
  deep-snapshotted at call time and compared element-wise against the
  verify-side snapshot, so `verifyCalledWith("f", [1, 2, 3])` matches
  only calls where `f` was invoked with a list of identical length and
  values. `str` elements are compared NUL-safely via length+`memcmp`.
  Mismatched arity or element types (e.g. passing `List<str>` against
  a `List<int>` parameter, or a scalar against a `List<T>` parameter)
  are rejected at compile time. Internally this introduces a snapshot
  ABI (kind tag 6 = list) reserved for future `Set<T>`, `Map<K, V>`,
  record, tuple, and function-value extensions of `verifyCalledWith`.
  (#1703)
- `verifyCalledWith(name, args...)` now accepts `Set<T>` arguments
  where `T ∈ {int, float, bool, str}`. The recorded call's set is
  deep-snapshotted at call time and compared **unordered** against the
  verify-side snapshot, so `verifyCalledWith("f", {1, 2, 3})` matches
  any call where `f` was invoked with a set of the same length and the
  same elements regardless of insertion order or hash-bucket layout
  (e.g. `{3, 2, 1}` and `{1, 2, 3}` are equivalent). `str` elements are
  compared NUL-safely via length+`memcmp`. Mismatched arity, container
  kind (e.g. `Set<int>` against a `List<int>` parameter, or a scalar
  against a `Set<T>` parameter), or element types (e.g. `Set<int>`
  against a `Set<str>` parameter) are rejected at compile time. This
  reuses the snapshot ABI introduced in #1703 (kind tag 7 = set;
  storage layout shared with `List<T>`, only the comparison semantics
  differ). (#1704)
- `verifyCalledWith(name, args...)` now accepts `Map<K, V>` arguments
  where `K, V ∈ {int, float, bool, str}`. The recorded call's map is
  deep-snapshotted at call time (independent copies of every key and
  value, with `str` slots ARC-retained) and compared **unordered** by
  key→value pair against the verify-side snapshot, so
  `verifyCalledWith("f", {"a": 1, "b": 2})` matches any call where `f`
  was invoked with a map having the same key set and the same value at
  each key, regardless of insertion order or hash-bucket layout (e.g.
  `{"b": 2, "a": 1}` and `{"a": 1, "b": 2}` are equivalent). `str` keys
  and `str` values are compared NUL-safely via length+`memcmp`.
  Mismatched arity, container kind (e.g. `Map<str, int>` against a
  `List<int>` / `Set<int>` / scalar parameter), key types, or value
  types are rejected at compile time. This reuses the snapshot ABI
  introduced in #1703 / #1704 (kind tag 8 = map; parallel keys and
  values arrays mirror the existing `MockListSnapshot` / `MockSetSnapshot`
  layouts). (#1705)
- `verifyCalledWith(name, args...)` now accepts **record** and **tuple**
  arguments whose fields / elements are all in `{int, float, bool, str}`.
  - Records are compared by **declared type name** plus field-by-field
    equality. Two records with structurally identical fields but
    different declared names (e.g. `Point(1, 2)` vs `Vec(1, 2)`) do not
    match and are rejected at compile time when the parameter type is
    fixed.
  - Tuples are compared by **arity** plus element-by-element equality.
    Tuples with different arity do not match and are rejected at
    compile time.
  - Each field / element is per-slot deep-snapshotted at call time
    (independent copies; `str` slots ARC-retained) and compared
    byte-exactly for `int` / `float` / `bool` and via length+`memcmp`
    for `str` (NUL-safe). LLVM struct padding is sidestepped by
    serializing each slot to an i64 value array plus an i8 kind array
    instead of memcmp'ing the raw struct.
  - Records or tuples whose fields / elements include nested
    collections (`List<T>` / `Set<T>` / `Map<K, V>`), nested records,
    nested tuples, or function values are rejected at compile time and
    are tracked for v0.0.x follow-up. (#1706)
- `verifyCalledWith(name, args...)` now accepts **function-typed**
  arguments (`fn(...) -> R` parameters and capture closures), compared
  by **pointer equality** on the underlying `{thunk_ptr, env_ptr}` pair
  extracted from the uniform closure struct.
  - The same closure value passed twice matches (e.g. `let g = lambda;
    f(g); f(g); verifyCalledWith("f", g)` returns `2`); two
    independently-constructed but structurally identical lambdas do
    not match.
  - Bare `@public fn` references and `let g = f` aliases share the same
    cached forwarding thunk, so passing `f` and `g` interchangeably
    matches as expected.
  - Capture closures with different captured environments (e.g.
    `makeAdder(5)` vs `makeAdder(6)`) are distinguished by the
    per-instance `env_ptr` even though they share a single cached
    capturing thunk.
  - The fn-snapshot side-table holds `{thunk_ptr, env_ptr}` pairs as
    plain copies — closure environments are not ARC-retained because
    the issue's contract is pointer equality only; the caller scope
    keeps the underlying closure alive for the duration of the test
    block. (#1707)
- Allowed `from testing import expect / mock / verify / fail / it /
  describe` by introducing a compiler-intrinsic allow-list in
  `ModuleLoader` and permitting the `expect` keyword (the only
  intrinsic that lexes as a reserved token, used elsewhere by the
  matcher statement form) at the import-name position in the parser.
  Wildcard `from testing` is also recognized as importing all six
  intrinsics. Names imported this way are exposed via
  `ModuleLoader::importedTestingIntrinsics()` for the forthcoming
  codegen-side enforcement (#713 / #715 / #716). Non-intrinsic names
  still fail with the existing `'<name>' not found in module
  'testing'` diagnostic. (#712)

### Changed

- Aligned `include/ry/codegen.hpp` and
  `include/ry/module_loader.hpp` testing-intrinsic header comments
  with the current allow-list (`expect` / `mock` / `fail`). The
  pre-existing 6-name listings (`expect` / `mock` / `verify` /
  `fail` / `it` / `describe`) had drifted from the actual
  enforcement set after #721 (`it` / `describe` → general
  user-directive resolution) and #722 (`verify` → Ry function).
  Documentation-only change; no behavior or ABI impact. (#1674)
- `verifyCalledWith(name, args...)` now enforces **exact fn signature
  match** for function-typed arguments (`fn(...) -> R` parameters
  introduced in #1707). Mismatched parameter count, parameter types, or
  return type are rejected at compile time with a diagnostic that
  includes both the recorded parameter signature and the verify-side
  value's signature (e.g. `verifyCalledWith: argument 1 of 'takesFn' is
  declared as fn(int) -> int but expected value has type fn(str) -> int`).
  Previously the signature was opaque to `verifyCalledWith`, so passing
  a fn value with a different signature compiled but always returned
  `0` (closure pair identity could never be equal across signatures) —
  silently masking test bugs. v1 requires exact match; variance and
  subtyping are not supported. (#1715)
- **Breaking**: `expect` / `mock` / `verify` / `fail` now require an
  explicit `from testing import <name>` declaration in the test file.
  Previously, codegen tracked which testing intrinsics were imported
  (#713) but did not enforce the import; any `*.test.ry` file run via
  `ry test` could call these intrinsics without declaring them. The
  compiler now rejects unimported usage with `'<name>' requires
  'from testing import <name>'` at codegen time, after the existing
  test-mode check so non-test-mode usage still wins the more useful
  "only allowed in test mode" diagnostic. All 171 in-tree spec files
  were migrated to declare these imports under #714, so the suite
  remains green; downstream test files that omitted the imports must
  add them. `it` / `describe` enforcement is tracked separately under
  #716. (#715)
- **Breaking**: `@it("...")` and `@describe("...")` directives now
  require an explicit `from testing import it, describe` (or the
  subset used) declaration in the test file. Codegen rejects
  unimported usage with `'@it' requires 'from testing import it'`
  or `'@describe' requires 'from testing import describe'` after
  the existing test-mode check, so non-test-mode usage still wins
  the more useful "only allowed in test mode" diagnostic. This
  completes the enforcement story started in #715 (which covered
  `expect` / `mock` / `verify` / `fail`). All `tests/spec/*.test.ry`
  files already declare these imports after the #714 migration,
  so the Ry self-test suite remains green; downstream test files
  that omitted the imports must add them. (#716)
- `docs/reference/directives.md` testing-related code examples now
  declare an explicit `from testing import ...` line at the top of
  each `@each` / `@property` / `@it` / `@describe` block, matching
  the codegen enforcement introduced in #715 (`expect` / `mock` /
  `verify` / `fail`) and #716 (`@it` / `@describe`). Previously the
  prose stated the imports were required but the example bodies
  omitted them, so the concrete examples (Basic / Composed / Shared
  setup / Nested) would have been rejected by codegen for missing
  imports. Each block lists only the names it actually uses
  (per-block tailored, including non-codegen-enforced names like
  `each` / `property` for pedagogical consistency), matching the
  convention already in `docs/reference/testing.md`. The "Syntax:"
  templates use placeholder bodies (`# test body`, `# assertions`)
  that do not parse on their own; converting those templates to
  runnable examples is tracked in #1629. (#717)
- `fail()` is now implemented as a Ry function in
  `share/std/testing/testing.ry` that delegates to a new
  `@native("testing")` runtime call (`_reportFail`) backed by a new
  `libry_testing.dylib` shared library. The compiler still
  special-cases the `fail` callee to inject the call-site line
  number as the first argument (the `__LINE__` intrinsic from #705
  was closed as `NOT_PLANNED`, so a hybrid approach keeps
  line-number injection in codegen), but the function body itself
  runs as ordinary Ry code. User-facing behavior is unchanged:
  `fail()` and `fail("message")` still report the call-site line
  number and message exactly as before, and the
  `'fail' requires 'from testing import fail'` import-gate from
  #715 still fires for unimported usages. (#718)
- `@it` and `@describe` directive declarations are now resolved entirely
  through the general user-directive import mechanism. `share/std/testing/testing.ry`
  has carried `@directive(target=["function"])` declarations for both since #710,
  and #716 added a parallel set-based check (`testing_intrinsics_imported_`) that
  produced `'@it' requires 'from testing import it'` / `'@describe' requires
  'from testing import describe'` before the directive-resolution path ran. That
  bespoke check has been removed: usage without the import is now rejected by the
  same `unknown directive '@<name>'` path that handles every other unimported
  user-defined directive. The intrinsic enforcement set now tracks only `expect`,
  `mock`, `verify`, `fail`. Existing test files that already declare
  `from testing import it, describe` (or use a wildcard `from testing`) are
  unaffected; the only behavioural change is the diagnostic wording for the
  unimported case, which now reads `unknown directive '@it'` /
  `unknown directive '@describe'`. (#721)
- `verify()` is now an ordinary `@public fn verify(name: str) -> int`
  in `share/std/testing/testing.ry` that delegates to a new
  `@native("testing")` runtime call (`_mockGetCallCount`). The
  compiler-level special cases for `verify` were removed: the
  string-coercion sugar in the parser, the dispatch arm in
  `codegen_call_dispatch.cpp`, and the `verify` entry in
  `module_loader.cpp`'s testing-intrinsic allow-list are all gone.
  `verify` now flows through the ordinary import + user-fn
  resolution machinery — the same path used by `fail` since #718.
  (#722)

### Removed

- The bare-identifier form `verify(fnName)` is no longer accepted —
  the argument must be a string literal or `str`-typed expression
  (e.g. `verify("fnName")`). All in-tree call sites already used the
  string form, so no spec migration was required, but external users
  who relied on the identifier form must quote the function name.
  (#722)
- Compile-time validation that the function name passed to `verify`
  refers to a real function has been removed alongside the dispatch
  special case. `verify("nonexistent")` now compiles cleanly and
  returns `0` at runtime — the same value `verify` returns for any
  function that has not been mocked / called. (#722)

### Fixed

- `editor/tree-sitter/grammar.js` now produces complete named nodes for
  partially-typed block-introducing statements, so `indents.scm`
  `@indent.begin` captures fire during live editing in Neovim. Before this
  change, typing `fn foo():` and pressing `<CR>` left the cursor at column
  0 because the parser wrapped the incomplete statement in `(ERROR)`,
  dropping the `function_body` field that the indent capture matches.
  After this change, the body of `function_body`, `if_statement`,
  `while_statement`, `for_statement`, `case_match_statement`, and
  `case_cond_statement` is wrapped in `optional(...)` so the prefix
  `fn foo():` / `if cond:` / `while x:` / `for x in xs:` / `case c:` /
  `case:` is a valid full sentence of the grammar — the parser commits to
  the surrounding statement node as soon as it sees the `:` and the
  capture's field predicate is satisfied. The trailing `else` clause of
  `if_statement` additionally allows its `:` to be missing, so a bare
  `else` typed on its own line is absorbed into the surrounding
  `if_statement` and the existing
  `(if_statement "else" @indent.branch)` capture dedents to the parent
  `if`'s column. The relaxation introduces a precedence ambiguity with
  `else if` (continue the chain vs. end the statement and start a new
  top-level `if`), resolved by wrapping `if_statement` in
  `prec.right(...)`. The Ry compiler continues to enforce non-empty
  bodies at compile time; the relaxation is editor-side only and
  intentionally diverges from `docs/grammar.ebnf` (canonical EBNF spec).
  `case_arm` / `case_cond_arm` were not relaxed because the next-arm
  condition can begin with `(`, which would create a parser ambiguity
  with an inline body; the outer `case_*_statement` relaxation is
  sufficient for the primary live-editing scenario. See
  `editor/tree-sitter/README.md` §"Live-editing tolerance" for the full
  table. (#1623)
- `docs/reference/directives.md` testing-related "Syntax:" templates
  (`@each`, `@property`, `@it`, `@describe`) no longer use placeholder
  bodies (`# test body`, `# assertions`), placeholder type names
  (`param1: type`), placeholder argument tuples (`(arg1, arg2, ...)`),
  or undefined function references (`makeInputs()`) that the parser
  rejected. All five affected blocks are now runnable examples with
  concrete types, values, and `expect(...)` bodies, matching the
  convention already established in `docs/reference/testing.md`.
  Each updated block also adds `expect` to its `from testing import`
  line. The `@each` w/ function-call block now defines a small
  `fn makeInputs() -> List<(int, int)>` helper inline so the
  function-call-as-argument lesson stands on its own. Companion to
  #717, which addressed the codegen-import side of the same drift.
  (#1629)
- `Err(e):` bindings on `Result<ARC-type, str>` (e.g.
  `Result<List<int>, str>`, `Result<Map<str, int>, str>`,
  `Result<Set<int>, str>`) now preserve `str` typing on `e`. Previously,
  metadata from the Ok side (collection element-type) leaked through the
  bulk `propagateMeta(subjectAlloca, varAlloca)` call in
  `emitPatternBindings`, making `e` look like a collection: `"prefix: " + e`
  failed compilation with `operator '+' not supported between str and
  non-str types`, and `f"prefix: {e}"` typechecked but crashed at runtime
  with a SIGSEGV when the `Err` arm executed (the str-pointer payload was
  dispatched through the list `valueToString` path). The fix introduces a
  lossless `source_type_name` field on `ValueMetadata`, stamped by
  `propagateTypeMeta` at the `Result<...>` / `Option<...>` / `T?` branch
  entries, and routes the Ok/Err/Some pattern arms through
  `propagateTypeMeta(innerSig, varAlloca)` instead of bulk
  `propagateMeta`. Each binding now receives only the metadata that
  corresponds to its actual type. `Result<int, str>`, `Result<int, int>`,
  and the Error-typed `Err(e)` paths are unaffected. (#1638)
- `case` expressions and statements with `Result<T, E>` or `Option<T>`
  subjects no longer leak ARC headers across iterations. The struct
  alloca that materializes the subject value (`{i1, T, E}` for `Result`,
  `{i1, T}` for `Option`) is now registered with the new
  `arc_tagged_union_vars_` side-table so that scope cleanup releases
  the active payload slot at scope exit. Previously, the
  construction-time retain emitted by `buildOkValue` / `buildErrValue`
  / `buildSomeValue` had no balancing release on the subject alloca,
  so each `case` evaluation leaked one ARC header per ARC-managed
  active slot. The release dispatches on the runtime tag and only
  touches ARC-managed slots, so `Result<int, int>` and `Option<int>`
  remain zero-cost. (#1640)
- ARC release IR is now correctly emitted on every exit path of a scope
  that has both an early-exit (`return` / `break` / `continue` / `?`)
  and a natural-exit path. Previously, `CodeGen::emitScopeCleanupToDepth`
  emitted the release IR and then erased the alloca from the relevant
  ARC side-table (`arc_managed_vars_`, `weak_managed_vars_`,
  `arc_field_record_vars_`, `arc_tagged_union_vars_`); when an early-exit
  cleanup ran first, the natural-exit `popScope()` found no entry and
  emitted no IR on the fall-through path, leaking one ARC header per
  iteration in loops with conditional early returns. The side-table
  erase responsibility has moved into `popScope` so each runtime path
  through the scope releases exactly once. The bug pre-dated #1640 and
  affected every ARC side-table, not just the new tagged-union one.
  (#1642)
- `emitListConcat` (the LLVM IR codegen for the list `+` operator)
  now calls `propagateMeta(lhs, newHeader)` after `setTypeMeta`, so
  element-type metadata such as `map_key_type_name` /
  `map_value_type_name` propagates to the concatenated result. Before
  this fix, an inferred binding like `ys = a + b` where
  `a, b: List<Map<str, int>>` lost the Map-element metadata and was
  treated as `List<str>`, causing subsequent `ys[i]["k"]` access to
  fail at codegen with `str does not support index access`. This
  brings `emitListConcat` in line with `emitListSlice` and
  `emitMapMergeCore`, both of which already pair `setTypeMeta` with
  `propagateMeta` per the existing rule. (#1648)
- Seven additional same-element-type collection helpers that previously
  called `setTypeMeta(TypeMeta::ListElem|SetElem, …)` without the
  matching `propagateMeta(src, newHeader)` now propagate source-level
  metadata correctly: `filter` and `emitSortCore` in
  `codegen_call_higher_order.cpp`, `emitStrOp_reverse` (List branch)
  in `codegen_call_string.cpp`, and the four set operations
  `emitSetUnionCore` / `emitSetOp_intersection` /
  `emitSetOp_difference` / `emitSetOp_symmetric_difference` in
  `codegen_call_set_ops.cpp`. Before this fix, source-level metadata
  such as `list_elem_type_name`, `map_value_type_name`,
  `set_elem_fn_type_info`, `nested_list_elem`, and `resource_kinds`
  was silently dropped on the output collection — for example
  `filter(xs, p)` where `xs: List<Map<str, int>>` lost the inner
  `Map<str, int>` metadata, so a subsequent `ys[0]["k"]` access
  failed at codegen with `str does not support index access`. The
  redundant manual `set_elem_type_name` copy at each set-op site is
  also removed because `propagateMeta` already copies that field.
  This completes the codegen sweep started in #1648 (`emitListConcat`)
  and brings every same-element-type collection helper in line with
  `emitListSlice` / `emitMapMergeCore`. (#1651)
- `keys(map)` and `values(map)` now propagate the source map's key /
  value type metadata onto the returned `List`. Before this fix,
  `emitBuiltinKeys` and `emitBuiltinValues` (`src/codegen_call.cpp`)
  stamped only the LLVM `TypeMeta::ListElem` slot via `setTypeMeta`,
  leaving `list_elem_type_name` and the derived `nested_list_elem` /
  `list_elem_fn_type_info` empty. The result list's elements were
  therefore dispatched as `str` by downstream operations, so
  `len(values(m)[0])` returned `0` (reading the List header's
  `weak_count` as `byte_len`) and `keys(m)[0][0]` /
  `values(m)[0]["k"]` raised `str does not support index access` at
  codegen for nested-collection key / value types like
  `Map<List<int>, str>` or `Map<str, List<int>>`. The fix snapshots
  `map_key_type_name` / `map_value_type_name` from the source map's
  metadata, calls `propagateTypeMeta("List<…>", newHeader)` after
  `setTypeMeta` to populate every derived slot, and pairs the element
  buffer `memcpy` with `emitCowRetainArcElements` when the element
  type is ARC-managed (#1204 / #1242 — required because the newly
  propagated `list_elem_type_name` flips the result's destructor to
  recurse into the inner ARC elements). The analogous bug in
  `items(map)` (`src/codegen_call_collection.cpp`) is tracked
  separately as #1659 because its tuple element type requires a
  different fix shape. (#1655)
- TSan no longer SEGVs on macOS during `~CodeGen()` teardown after
  combinatorial spec tests that nest `@describe` / `@it` (e.g.
  `tests/spec/combinatorial/collection_element_option_iterate.test.ry`).
  Previously `~CodeGen()` walked
  `functions_ → vector<OverloadEntry> → ~OverloadEntry() → unique_ptr<unordered_map<size_t, FnTypeInfo>>::reset()`
  on a heap whose state had already been disturbed by LLVM ORC JIT
  teardown, and intermittently called `free()` on a garbage pointer
  (e.g. `0x4800000001135036`). ASan + UBSan were both clean on the
  same binary and test, confirming this was the same #1187 family
  ORC teardown heap corruption — TSan exposed the disturbed-heap
  sequel that the existing `(void)jit.release()` + `rtCleanup.release()`
  block did not cover. `runRySource` (`src/jit_runner.cpp`) now
  heap-allocates `CodeGen` via `std::make_unique<CodeGen>(...)` and
  leaks it via `(void)cg.release()` alongside the existing LLJIT
  releases under `#if defined(__linux__) || defined(__APPLE__)`. The
  process exits immediately after `runRySource` returns, so the leak
  is bounded by process lifetime. This is still a workaround — the
  upstream LLVM ORC / JITLink heap corruption pattern that propagates
  into the codegen heap is unidentified — but it suppresses the
  `~CodeGen()` / `~OverloadEntry()` SEGV reliably under TSan, ASan,
  UBSan, and default builds. (#1657)
- `items(map)` now stamps the source map's key/value type names onto
  the returned `List<(K, V)>` as `list_elem_type_name = "(K, V)"`.
  Before this fix, `emitCollOp_items`
  (`src/codegen_call_collection.cpp`) stamped only the LLVM tuple
  `TypeMeta::ListElem` slot via `setTypeMeta`, leaving
  `list_elem_type_name` empty. The for-loop destructure
  `for k, v in items(m):` relies on `splitTupleSig` reading that
  name to split the tuple into per-component metadata; without it,
  K/V components fell back to `str` and operations like `v[0]` on
  `Map<str, List<int>>` raised `str does not support index access`
  at codegen. The fix snapshots `map_key_type_name` /
  `map_value_type_name` before any `getOrCreateMeta` call (per the
  #858 name-snapshot-before-rehash discipline) and writes
  `list_elem_type_name = "(K, V)"` after `setTypeMeta`, mirroring
  the format used by `enumerate` and `zip`. Unlike the sibling fix
  for `keys()` / `values()` (#1655), no `emitCowRetainArcElements`
  is needed because the destructor for `List<(K, V)>` does not
  recurse into tuple fields — `fieldTypeIsArcManaged` returns false
  for tuple-syntax `list_elem_type_name`, so adding a retain would
  leak. (#1659)
- Numeric tuple-field access (`xs[0].1`) and chained subscripts
  (`xs[0].1[0]`) on `List<(K, V)>` results now carry per-component
  metadata through the extraction. Before this fix, the IndexExpr List
  path called `propagateTypeMeta(elemTypeName, elem)` with
  `elemTypeName = "(K, V)"`, but `propagateTypeMeta` is single-value by
  design (per `.claude/rules/codegen-type-and-metadata.md` —
  *"propagateTypeMeta is single-value; callers decompose tuples"*),
  so the tuple components received no metadata. The downstream
  FieldAccessExpr numeric-index arm then emitted a bare
  `CreateExtractValue` that fell through to the `str` dispatch, so
  `print(enumerate(xs)[0].1[0])` (for `xs: List<List<int>>`) raised
  `str does not support index access` at codegen — the same shape the
  for-loop destructure path already handled correctly via
  `splitTupleSig` (`src/codegen_stmt_loop.cpp`). The fix stamps the
  tuple sig onto the loaded element via the
  `ValueMetadata::source_type_name` channel (the same lossless slot
  used for `Result<T, E>` / `Option<T>` since #1638) at the IndexExpr
  List path in `src/codegen_expr_literal.cpp`, then decomposes
  per-component via `splitTupleSig` and propagates the matching
  component's name onto the extracted field at the FieldAccessExpr
  numeric-index arm in the same file. `enumerate(xs)` and
  `zip(xs, ys)` work end-to-end because their codegen sites already
  stamp `list_elem_type_name = "(int, T)"` / `"(T, U)"`. `items(m)`
  also works end-to-end now that #1659 stamps `list_elem_type_name =
  "(K, V)"` on its result — together the two fixes make direct
  field access (`its[0].1[0]`) carry per-component metadata through
  the chain. (#1664)
- `items()`, `enumerate()`, and `zip()` now correctly retain ARC-managed
  tuple components when constructing their `List<(K, V)>` results, and
  the collection destructor now releases inner ARC components for
  tuple-element lists. Previously both halves were missing simultaneously,
  so a rebind of the source container (e.g.
  `m: Map<str, List<int>> = {"a": [1,2,3]}; its = items(m); m = {"z": [99]}`)
  freed the inner `List<int>` while `its` still held the raw pointer,
  producing a use-after-free on the next read. The fix lands the retain
  and release sides symmetrically (parallel to #1242's whole-collection
  rebind fix). The same retain symmetry is also applied to `slice`,
  `take`, `appended`, and `concat` on tuple-element lists, since these
  inherit the new tuple-aware destructor via `propagateMeta`. (#1667)
- `xs[i] = (a, b)` slot overwrite on `List<(K, V)>` now retains the
  ARC-managed components of the new tuple and releases the components
  of the evicted tuple. Previously the IndexAssignStmt path was the
  remaining symmetry gap from #1667: the destructor recursed into
  inner tuple components, but slot overwrite did neither retain nor
  release, leaking the evicted tuple's inner ARC values on every
  reassignment. The fix mirrors #1667's per-component dispatch by
  source-level type name (str at offset −24, List/Map/Set at −16,
  nested tuples recurse), gates on a non-empty `list_elem_type_name`
  with tuple shape `"(...)"` (preserving pre-fix behavior for
  literal-built lists whose tuple sig is empty — same blind spot as
  `List<str>` literals), and orders retain-before-release so
  self-assignment `e[i] = e[i]` is safe. (#1670)
- Without `from testing import verify`, calling `verify(...)` now
  fails with the standard `undefined function: verify` diagnostic
  instead of the bespoke `'verify' requires 'from testing import
  verify'` message. The behavior is unchanged for legitimate users
  (the import is still required), and the diagnostic is now
  consistent with every other unimported function. (#722)

## [0.0.21] - 2026-05-06

### Changed

- Migrated 167 of 170 `tests/spec/*.test.ry` files from the deprecated
  lambda form `describe("...", ():` / `it("...", ():` to the canonical
  `@describe("...")` / `@it("...")` named-function form. Without this
  migration, `./build/ry test -p` emitted 340 deprecation warnings
  (newly visible after the warning-flush fix in #1424). Three files —
  `numeric_literal_suffix.test.ry`, `numeric_underscore_separator.test.ry`,
  and `operator_overload.test.ry` — could not be migrated because the
  tests they contain expose a separate parser/codegen bug: `f64` literal
  suffix and locally-declared `record` types fail to resolve inside any
  named-function body (including module-level `fn`), while resolving
  correctly inside a lambda body. These three files were retained in
  the lambda form and tracked for migration under #1601; with #1601
  shipped in this release, those files were migrated, and the lambda
  parser/codegen path removal is captured under #1602. (#1599)

### Removed

- Removed the deprecated lambda call form of `describe("...", ():)` and
  `it("...", ():)` from the parser and codegen. After #1599 (stdlib
  migration) and #1601 (deferred-file migration), all `tests/spec/*.test.ry`
  files use the canonical `@describe("...") fn name():` / `@it("...") fn name():`
  named-function form, so the lambda form is no longer reachable from
  any in-tree source. The trailing-block carve-out for `describe` / `it`
  in the parser and the dedicated lambda-form codegen helpers
  (`extractLambdaArg`, `emitDescribeCall`, `emitItCall`, the lambda
  branches of `emitEachItCall` / `emitPropertyItCall`) were deleted
  along with the `warned_call_deprecations_` warning-dedup state.
  Source that still uses the lambda form now fails compilation with
  `undefined function: describe` / `undefined function: it`. (#1602)

### Fixed

- `@deprecated` warnings now reach stderr when running a file via `./build/ry`. Previously, the compiler collected `warning: 'X' is deprecated` messages in `CodeGen::warnings_` but no production code path called `getWarnings()`, so users saw nothing despite `docs/reference/directives.md` documenting the behavior. The flush is performed in `runRySource()` right after `compile()` succeeds, before both the `--emit-llvm-ir` early return and JIT setup, so warnings always reach stderr regardless of how compilation continues. CLI-side deduplication keeps repeated call sites of the same deprecated symbol from emitting the same warning multiple times. (#1424)
- Overloaded function calls with no matching argument types now produce a canonical diagnostic at the frontend instead of leaking through to the LLVM IR verifier. Previously `for x in range(1..n): ...` (where `1..n` has type List of int, not int) crashed with an LLVM "IR verify error: Both operands to a binary operator are not of the same type!"; it now reports a no-matching-overload error for `range` followed by the candidate signatures (one-, two-, and three-argument forms returning a list of int) and the actual call types (the supplied List of int argument). The same canonical format is now used across all three dispatch paths: the `range`, `len`, and `enumerate` inline custom emitters; `@native` table-driven calls (e.g. `pow` with mismatched int and float arguments); and user-defined function overload resolution (including ambiguous calls). (#1577)
- `math.abs(INT_MIN)` now traps with `runtime error: integer overflow` and exits with status 1 instead of silently returning `INT_MIN`. The post-condition `abs(x) >= 0` is preserved by detecting the unrepresentable result before negation. (#1591, #1592)
- `INT_MIN // -1` and `INT_MIN % -1` now trap with `runtime error: integer overflow` and exit with status 1 instead of returning poison from LLVM's `sdiv` / `srem`. The new check matches the existing trap behavior of `+` / `-` / `*` / unary `-`. (#1591, #1592)
- `json.parse()` now enforces a hard cap of 256 on array/object nesting depth and returns `Err(Error{message: "json: maximum nesting depth exceeded"})` for inputs that exceed it. Previously, deeply nested input (depth ~90,000+ on macOS with the default 8 MB stack) would exhaust the C stack and abort the process with SIGABRT (exit 132). The depth counter is shared between `parse_array` and `parse_object`, so mixed array/object nesting also triggers the cap. **Behavior change**: inputs that previously parsed successfully at extreme depths (e.g. depth 80,000 in the issue's pre-fix table) will now return `Err`; well-formed real-world JSON is unaffected since 256 levels of nesting comfortably exceeds typical use. (#1593)
- `base64.decode()` and the related `decodeUrlSafe` / `decodeBytes` / `decodeBytesUrlSafe` functions now reject inputs with malformed padding per RFC 4648 §3.2 strict mode, returning `Err(Error{message: "invalid base64: ..."})` instead of silently succeeding. Previously, `decode("====")` returned `Ok("")`, and trailing-padding overflows like `decode("TWFu=")` / `decode("TWFu==")` returned `Ok("Man")`. After the fix, any input that contains `=` padding must have a length that is a multiple of 4 and contain at most 2 trailing `=` characters; otherwise the call returns `Err`. The validation is shared with the URL-safe variants — `decodeUrlSafe("====")` / `decodeBytesUrlSafe("TWFu=")` are also rejected. URL-safe canonical no-padding inputs (e.g. `"SGVsbG8sIFdvcmxkIQ"`) remain accepted as before. **Behavior change**: any caller that relied on the silent acceptance of pure-padding (`"="`, `"=="`, `"==="`, `"===="`) or trailing-padding-overflow inputs now needs to handle `Err`. (#1594)
- `f64` literal suffix now resolves correctly inside any named-function
  body. Previously, `x = 3.14f64` or `1.5_0f64` inside a `fn`/`@it`/`@describe`
  body (including module-level `fn`) raised `unknown type: f64`, even
  though the same code worked at module top level and inside lambda
  bodies. The root cause was a missing `f64` entry in `resolveType`'s
  primitive type table; the `FloatExpr` lambda return-type inference
  pre-pass then fell through to the `unknown type` error before any
  body statement could emit. (#1601)
- Locally-declared `record` types are now resolvable as `as`-cast
  targets inside named-function bodies. Previously, declaring a
  `record` inside a `fn` body and then using `value as <RecordName>`
  raised `unknown type: <RecordName>` because the lambda return-type
  inference pre-pass ran before the body emit loop registered the
  record into `record_types_`. The pre-pass now uses a permissive
  type lookup with a safe fallback; the strict fatal lookup at body
  emit time is unchanged, so genuinely unknown cast targets are still
  diagnosed. (#1601)
- Migrated the three deferred test files from #1599
  (`numeric_literal_suffix.test.ry`, `numeric_underscore_separator.test.ry`,
  `operator_overload.test.ry`) from the deprecated lambda form
  `describe("...", ():` / `it("...", ():` to the canonical
  `@describe("...")` / `@it("...")` named-function form. (#1601)

## [0.0.20] - 2026-05-05

### Added

- List destructuring assignment. `a, b = some_list` (and the parenthesized
  form `(a, b) = some_list`) now unpacks a `List<T>` whose runtime length
  matches the number of positions on the left, where each `_` wildcard
  still counts as a position (so `_, b = some_list` requires two RHS
  elements). The `_` wildcard, `@const` prefix, and function-return values
  work the same as for tuple destructuring. A length mismatch aborts with
  `runtime error: list destructuring expected N elements but got M`,
  matching Python's semantics. The motivating idiom `a, b = split(s, " ")`
  now works without an intermediate temporary. (#1567)
- `@native` stdlib functions imported with `from <module> import <name>` can now be
  used as **first-class function values**: bound to variables (`let f = toInt`),
  passed to higher-order functions (`xs.map(toInt)`), and forwarded through
  user-defined `fn(...) -> R`-typed parameters. Internally, the codegen
  materializes a single internal LLVM thunk per name (cached) that forwards
  through the existing native dispatch chain, so both bare `@native` and
  `@native("libname")` declarations work identically. Materialization rules:
  (a) names with **multiple overloads** (e.g. `toStr` over `int`/`float`/`bool`,
  most `math` custom-emitter natives like `abs`/`pow`/`round`/`log`) are
  rejected with `ambiguous reference to @native function 'X': multiple overloads
  exist; wrap in a lambda to select one`; (b) names with **default arguments**
  (e.g. `startsWith(haystack, needle, ignoreCase=false)`) materialize at
  full arity — the resulting binding requires every parameter; the
  default-omission shortcut is only available on the original direct call.
  User-defined `fn` declarations continue to take precedence on name conflict
  (the new path activates only when the user-fn lookup misses). (#1569)
- `sequence(values: List<Result<T, E>>) -> Result<List<T>, E>` and
  `sequence(values: List<Option<T>>) -> Option<List<T>>` for folding
  a list of `Result`/`Option` into a single `Result`/`Option` of list,
  short-circuiting on the first `Err`/`None`. Empty list returns
  `Ok([])` / `Some([])`. UFCS form `xs.sequence()` is also supported.
  (#1570)
- `count(string: str, substring: str, ignoreCase: bool = false) -> int` to the
  `str` module. Returns the number of non-overlapping occurrences of
  `substring` in `string`, matching Python / Go semantics
  (`"aaaa".count("aa") == 2`). Empty `substring` returns `byteLen + 1` (gap
  count); `ignoreCase` performs ASCII-only folding; arguments may contain
  embedded NUL bytes. (#1571)
- Single-parameter lambdas may now omit the parentheses when the parameter
  has no type annotation and the body is a single expression: `xs.filter(s => s == "1")`.
  Multi-arg, type-annotated, and block-bodied lambdas keep their existing paren-required
  syntax. (#1572)
- `digits(n: int) -> List<int>` and `digits(n: int, base: int) -> List<int>`
  to the `math` module. Decomposes a non-negative integer into its digits
  low-first (least-significant digit at index 0), matching Ruby's
  `Integer#digits` (`digits(1234) == [4, 3, 2, 1]`, `digits(255, 16) == [15, 15]`,
  `digits(0) == [0]`). Default base is 10. Composes with `sum` for digit-sum
  in one expression: `sum(digits(1234)) == 10`. Aborts with a runtime error
  on negative `n` or `base < 2`. (#1578)

### Removed

- `!!` operator. Use `?` instead — the two operators were identical aliases since
  introduction, and removing one eliminates a stylistic split that added review
  cost without benefit. Sources using `!!` will fail to parse; replace each `!!`
  with `?`. (#1568)

### Fixed

- ARC retain was missing on str elements when destructuring an unannotated `List<str>` produced by `split()`: `parts = split("a b", " "); a, b = parts; parts = split("c d", " ")` would release the original list (and its str elements) on the source rebind while `a` / `b` still held raw pointers into the freed strings — UAF only avoided in tests because the str-aware destructor was suppressed by a #1266 carve-out, leaving the strings leaked rather than dangling. The root cause was a counter asymmetry between `__ry_arc_alloc_counted` (which incremented `g_arc_live_count` by +1) and `makeString` / `makeStringUninit` / `freeStringSlot` (which were no-ops on the counter). With the counter symmetric for every dynamic str allocation, the `split()` emitter (`src/codegen_call_string.cpp`) now safely stamps `list_elem_type_name = "str"` on its result so `resolveCollectionDestructor` dispatches to the str-aware variant — and `tryRetainArcSource` Case 4 now emits the missing element retain on `a, b = parts` for the untyped form. The `xs: List<str> = ...` annotation branch in `src/codegen_stmt.cpp` was extended to stamp the same metadata, so typed and untyped destructuring share one path. The pre-existing `#1266` carve-out in `.claude/rules/codegen-arc-cow.md` is narrowed to historical context, since "no stamp without retain" no longer applies once allocate / free are counter-symmetric. New regression coverage: `tests/filecheck/arc_retain_list_destruct_str_elem_untyped.ry` (IR-level, asserts the `getelementptr i8, ptr %destruct_elem_*, i64 -24` retain block on each destructured element) and a new spec case in `tests/spec/arc_split_chars.test.ry` ("split() untyped — destructure + rebind does not leak"). `tests/spec/arc_split_chars.test.ry` and `tests/spec/arc_list_destructure.test.ry` (case 3) had their `arcLiveCount` deltas re-baselined and their commentary rewritten to reflect the symmetric regime; `tests/spec/str_arc.test.ry` two `ExprStmt` release tests were re-baselined for the same reason. (#1576)
- The intermediate buffer of a chained string concatenation `"x" + "y" + "z"` was never released, leaking one `StringHeader` allocation per inner concat. `emitArithmeticOp`'s str+str branch (`src/codegen_expr.cpp`) allocated a result buffer with `__ry_string_make_uninit` and inserted it into `arc_str_owned_values_`, but never released the lhs/rhs operands when those were themselves freshly-produced concat buffers. For `BinaryExpr("+", BinaryExpr("+", "x", "y"), "z")`, the inner `"xy"` buf survived past its only use as the outer concat's lhs because `emitStmt(ExprStmt)` only releases the outermost SSA value. The fix releases lhs/rhs at the concat site itself — after the second `memcpy` and before `arc_str_owned_values_.insert(buf)` — for any operand that was tracked in `arc_str_owned_values_`. Because the release is local to the concat emitter, it works in every enclosing context (bare `ExprStmt`, let-binding, return value, function arg, nested binary). Verified via `runtime_internal.arcLiveCount()` delta assertions in `tests/spec/str_arc.test.ry` (chained 3-arg / 4-arg / let-binding now net to the correct counter delta) and a new IR golden `tests/filecheck/str_concat_chain_release.ry` that asserts the inner `__ry_string_make_uninit` buf is followed by an `arc.release.body` block before the outer buf is stored. Combined with the #1576 ARC counter symmetrization, chained concat in `ExprStmt` position now nets to delta `0`. (#1583)

## [0.0.19] - 2026-05-04

### Added

- `release.yml` now runs a `codeql-gate` preflight job that confirms the `codeql.yml` workflow run for the exact `github.sha` finished with `conclusion=success` before the `release` job is allowed to publish artifacts. Previously, `release.yml` (`build` → `release`) ran independently of `codeql.yml`, so a tag push that arrived close to a main push could publish a GitHub Release before the corresponding CodeQL analysis had even completed — observed at v0.0.18 (#1539 follow-up), where the GitHub Release was published 2026-05-03T03:55:21Z while the CodeQL run for `e3431e86` was still `in_progress`. The new gate uses `gh api` to query `actions/workflows/codeql.yml/runs?head_sha=<SHA>&event=push`, deliberately filtering by `event=push` so only mainline analysis state counts (PR-triggered and `workflow_dispatch`-triggered CodeQL runs do not satisfy the gate). The polling logic is two-phase: Phase 1 waits up to `INITIAL_WAIT_SECONDS` (default 120s) for a CodeQL run to appear for the SHA — needed because main-push and tag-push arrival order is not guaranteed and the CodeQL run may not be enqueued yet when `release.yml` starts — and Phase 2 then polls up to `POLL_TIMEOUT_SECONDS` (default 30 minutes) at `POLL_INTERVAL_SECONDS` (default 30s) intervals until the run reaches `status=completed`. The gate fails closed: `failure` / `cancelled` / `timed_out` / `action_required` / Phase 1 timeout / Phase 2 timeout all exit non-zero and prevent `release` from running. To intentionally bypass the gate (CodeQL outage, broken analysis), `release.yml` now also accepts a `workflow_dispatch` boolean input `skip_codeql_gate` (default `false`); the gate job's `if:` skips itself when the input is `true`, and the `release` job's `if:` accepts either `success` or `skipped` from the gate (with `always() && needs.build.result == 'success'` so a build failure still blocks release). All three timing parameters are exposed as job-level `env:` so the polling script body can be exercised locally with shortened timeouts. `.claude/skills/preparing-for-release/SKILL.md` is updated with a new "Note: CodeQL gate is enforced by `release.yml`" section after Step 3 explaining the operational consequences (no manual pre-tag CodeQL check needed; `event=push` filter; escape hatch via `workflow_dispatch`), and the Release prep / Release issue templates each carry a short `## Note` paragraph pointing at the gate. End-to-end verification of the gate against a real tag push will only be observable at the next stable release; pre-merge checks were limited to YAML/bash syntax validation and local execution of the polling script with shortened timeouts against a known-success SHA (`e3431e86`) and a synthetic nonexistent SHA. (#1542)
- Registered `@public` as a compiler built-in directive in `src/directive_meta.cpp`'s `builtinDirectiveRegistry()` so `@public fn`, `@public name = value`, and `@public record` declarations parse and codegen cleanly without any visibility effect (the annotation is inert at this stage). The signature carries `T::Function | T::Record | T::Statement` for `allowed_targets`, takes zero positional arguments (`min_pos=0`, `max_pos=0`), and has no custom validator — matching the bootstrap pattern of `@native` (compiler builtin, not declared in `share/std/core/directive.ry`). This unblocks #1546 (stdlib `@public` annotation) by accepting the syntax at parser/registry level; the visibility effect itself (default = package-internal, `@public` = universe) is implemented in #1544, and the `docs/reference/directives.md` reference update belongs to #1547. Out of scope: enum and type-alias targets — `EnumStmt` and `TypeAliasStmt` lack a `directives` field and the parser explicitly rejects directives on them at `src/parser.cpp:446-449,547-548`; stdlib has zero such declarations today, so #1546 is not blocked. Regression tests: `DirectiveTest.{PublicDirectiveRegistered,PublicDirectiveOnFnCodegenPass,PublicDirectiveOnLetCodegenPass,PublicDirectiveOnRecordCodegenPass,PublicDirectiveRejectsArgs}` in `tests/test_codegen_directive.cpp` (the last one is the direct `max_positional=0` rejection-branch regression per `.claude/rules/tests-rejection-tdd.md`); `ParserTest.{PublicOnFn,PublicOnLet,PublicOnRecord}` in `tests/test_parser.cpp`. (#1545)

### Changed

- **Breaking:** Flipped the default symbol visibility from universe to **package-internal**. Definitions are now visible only within the same package by default; the new `@public` directive (registered as a compiler builtin in #1545 and applied to stdlib in #1546) makes a definition visible across package boundaries. A **package** here is the directory tree rooted at the nearest ancestor `package.toml`; files without any `package.toml` ancestor share a single anonymous package, so ad-hoc scripts and the REPL `-c` input continue to see each other but cannot reach non-`@public` symbols inside any rooted package (including stdlib internals). The `@public` annotations added in #1546 cover every public-facing stdlib symbol, so existing user code that imports stdlib via `from math import sqrt`, `from io import readText`, etc. continues to work; the import error message for the new failure mode is `cannot import 'X' from module 'Y': symbol is not @public` (`src/module_loader.cpp`), distinct from the existing `'X' not found in module 'Y'` so a missing-vs-private mistake is diagnosable. (#1544)
- **Breaking:** Removed the legacy underscore-prefix visibility convention. The `_` leading underscore on identifiers no longer marks a definition as private — it is now a stylistic naming convention with no compiler-enforced meaning. Wildcard imports (`from m`) no longer filter `_`-prefixed names, named imports of `_`-prefixed symbols (`from m import _x`) no longer error, and directory modules no longer skip `_*.ry` files. All visibility decisions go through `@public` and the package boundary; the `_` prefix is purely cosmetic. The pre-existing parse-time check that allowed `_camelCase` identifiers is unchanged — only the import-side filtering and file-skip logic was removed. The `isPrivateName()` helper in `src/module_loader.cpp` is gone, and `extractDefinitions()` was refactored into a `collect` + `filter` pair so cross-package access errors can distinguish "not found" from "not @public". (#1544)
- Extended `EnumStmt` and `TypeAliasStmt` AST nodes with a `directives` field so `@public` can be applied uniformly to every export-bearing definition kind (`fn`, `record`, `enum`, `type` alias, `let`, `@directive`). Previously these two node kinds had no directives slot and were always treated as public; the parser now attaches directives parsed before `enum` / `type` declarations, the formatter round-trips them, and `isPublicDefinition()` in `src/module_loader.cpp` consults the field. New cross-package visibility tests in `tests/test_codegen_stmt.cpp` and parser positive tests in `tests/test_parser.cpp` lock the behavior. This closes the AST gap that was originally tracked separately as #1559 — folded into #1544 because the visibility-default flip is incomplete without it. (#1544)
- Added `share/std/package.toml` so the standard library has its own package root. Without this file, every stdlib `.ry` file would have walked up past the install prefix, found no `package.toml`, and shared the anonymous package with the user's code — making the stdlib's package-internal symbols silently importable. The manifest is minimal (`[project] name = "std"`, no `entry`); a round-trip test in `tests/test_project_config.cpp` locks the empty-`entry` parse + serialize behavior so a future tightening of `[project]` requirements does not break stdlib silently. The file is installed into `$RY_HOME/share/std/package.toml` by the existing `share/std/` install rule (no `CMakeLists.txt` change required). (#1544)
- Annotated every public-facing symbol in `share/std/` with `@public` to prepare for the v0.0.19 default-visibility flip from universe to package-internal (#1543's case A-X). Coverage: **256** `@public` annotations across 19 stdlib `.ry` files (`base64`, `builtins`, `convert`, `filesystem`, `gc`, `higher_order`, `http`, `io`, `json`, `list`, `map`, `math`, `net`, `path`, `regex`, `set`, `str`, `testing`, `thread`). All `@native` functions, the four math-module constants (`PI`, `E`, `INF`, `NAN`), the four `testing.ry` directive declarations (`it`, `describe`, `each`, `property`), and the resource-cleanup helpers (`httpClientResponseFree`, `jsonFree`, `lockFree`, `rwlockFree`, `semaphoreFree`, `barrierFree`, `atomicIntFree`, `atomicBoolFree`) are marked `@public`. Two modules are intentionally left without `@public`, keeping their symbols package-internal: `share/std/core/directive.ry` (re-exported via `builtins.ry` within the same stdlib package) and `share/std/runtime_internal/runtime_internal.ry` (test-instrumentation only, not public API). The annotations are inert at this point — `@public` was registered as a compiler builtin in #1545 (parser/registry-level acceptance only) and the visibility effect itself lands in #1544 when the default flips. Existing tests, golden files, and ABI surface are unchanged. (#1546)
- Relaxed the `@directive can only be combined with @public` parser constraint (`src/parser.cpp`) so `@public` may be combined with `@directive(target=...)` declarations. Previously the parser rejected any directive list containing `@directive` plus a second directive (the original wording: "@directive cannot be combined with other directives"); the relaxation is a strict whitelist — only `@public` is admitted alongside `@directive`, and combinations with any other directive (e.g. `@inline @directive`, `@native @directive`, `@deprecated @directive`) continue to be rejected with the new wording "@directive can only be combined with @public". `DirectiveDefStmt` was extended with a `directives: std::vector<Directive>` field that captures auxiliary directives (today: `@public`) attached to the declaration; the `@directive` entry itself is excluded from that field because its contents are already represented by the dedicated `name` / `targets` / `params` fields on the AST node. The relaxation is required so `share/std/testing/testing.ry`'s four `@directive fn` declarations (`it`, `describe`, `each`, `property`) can also be marked `@public`; without it, `@public + @directive` would parse-error and #1546's stdlib annotation would be incomplete. The validation hook for the new field is intentionally deferred to #1544 — at this point the field is collected but not consumed by `validateDirectives`, matching the inert design of `@public` until the default flips. Regression tests: `DirectiveDefParserTest.{AcceptsPublicBeforeDirective,AcceptsPublicAfterDirective,AcceptsBareDirectiveHasEmptyDirectives,RejectsCombiningDirectiveWithInlineBefore,RejectsCombiningDirectiveWithInlineAfter}` in `tests/test_parser.cpp` (the pre-existing `RejectsCombiningDirectiveWithDeprecated{Before,After}` continue to guard the non-`@public` rejection branch) plus `FormatterTest.DirectiveDefRoundTripWithPublic` in `tests/test_formatter.cpp` for the formatter roundtrip. (#1546)
- Removed 6 dangling references to a previously existing `KNOWLEDGE.md` from `.claude/rules/` (4 files: `tests-arc-leak-pattern.md`, `codegen-pattern-and-match.md`, `codegen-stdlib-dispatcher.md`, `codegen-arc-cow.md`). All references were broken pointers — the surrounding sentences are self-contained, so only the citation fragments were removed. Brings the rules tree into alignment with `/knowledge-md-management` REQ-3 (no individual-entry references to KNOWLEDGE.md from `AGENTS.md` / `.claude/rules/` / `.claude/skills/` / `.claude/agents/`). Also updated the skill's own self-reference (`.claude/skills/knowledge-md-management/SKILL.md`) to mark the cleanup as completed. Knowledge-base only — no code, ABI, or build behavior changes. (#1550)

### Removed

- **Breaking:** Removed the `_*.ry`-file skipping convention from directory module loading. Files like `share/std/list/_helpers.ry` (none currently exist in the tree) were previously skipped during `loadModuleDir()`; now every `.ry` file in a directory module is loaded. Test files (`*.test.ry`) continue to be skipped as before — the `.test.ry` suffix is a separate, retained convention. (#1544)

### Fixed

- Stripped the lone blank line between the leading header comment block and the first declaration in `share/std/core/directive.ry` and `share/std/runtime_internal/runtime_internal.ry` so the two files match what `ry fmt` emits. Pre-existing drift carried since #1490 and surfaced during #1546's stdlib `@public` self-verification — both files were intentionally left without `@public` (kept package-internal) and so were not touched there, yet they still failed `ry fmt --check` on `main`. The other 19 stdlib `.ry` files already follow the "header comment, then declaration with no separating blank line" convention; this aligns the last two outliers with that de-facto rule. The formatter behavior itself is unchanged — fixing the source is the cheaper of the two directions sketched in #1554, and no formatter idempotency rule needed to change. No code, ABI, or build behavior changes; `ry fmt --check share/std/**/*.ry` now exits 0 across all 21 stdlib files. (#1554)
- Cross-package and same-package selective wrapper patterns (REQ-B3 from #1543) now work as documented: a `@public` facade can transitively call non-`@public` helpers in the same package, and external importers can `from foo import bas` without `bas`'s body failing with `undefined function: helper`. Previously the visibility filter in `extractDefinitions` (`src/module_loader.cpp`) doubled as a code-availability filter — non-`@public` definitions were dropped from the importer's program even when only the `@public` facade was named, so the helper could not be linked at JIT codegen. The filter is now decoupled into two layers: code availability (always copy every exportable definition into `dest` for codegen) and name visibility (the `from foo import <name>` validation still rejects non-`@public` names cross-package, satisfying REQ-A1). The four `*WildcardExcludesNonPublic` test cases for functions, `@directive` defs, enums, and type aliases were renamed to `*WildcardIncludesAllForCodegen` and flipped to assert the new semantics; seven new wrapper-pattern tests were added (cross-pkg selective in/out same file, cross-pkg wildcard, same-pkg selective, same-pkg wildcard, REQ-A1 regression, two importers of the same module). **Known limitations**: (a) two cross-package modules with the same internal helper name now collide at codegen with `duplicate function`; (b) an importer-local function colliding with an imported package-internal helper similarly collides — same-package selective imports surface this where they used to silently drop the helper. Workarounds: rename the colliding helper or restructure to use a single canonical module. `docs/guide/visibility.md` was rewritten to describe the working pattern and the collision limitations; `docs/reference/directives.md` cross-package wildcard example was updated to match. (#1560)

## [0.0.18] - 2026-05-03

### Added

- New `.claude/skills/test-design-techniques/` skill cataloguing five general-purpose deductive test design techniques (equivalence partitioning, boundary value analysis, state transition testing, decision table testing, pairwise / all-pairs) with definitions and ry-specific application examples (parser numeric literals, `int64` overflow boundaries, type-inference scope transitions, operator overload resolution decision tables, native-dispatch pairwise axes). The new skill is the **deductive** complement to the existing `/test-checklist` (which is **inductive** — ry-specific recurring omissions P1–P8); `/tdd-cycle` and `/test-checklist` both gain a one-line cross-reference pointing at `/test-design-techniques` so the テスト作成 step starts with deductive enumeration and follows up with inductive verification. Knowledge-base only — no code, ABI, or build behavior changes. (#1497)
- New `.claude/agents/devils-advocate.md` sub-agent providing structured contrarian analysis (steel-man reconstruction, multi-angle attack across assumptions / failure modes / hidden costs / alternatives / reversibility / scale / stakeholders / precedent / second-order effects / selection bias, severity-ranked objections, constructive synthesis) for stress-testing design proposals and technical plans. The agent has its own persistent memory at `.claude/agent-memory/devils-advocate/` and is invokable via the `Agent` tool with `subagent_type: "devils-advocate"`. Bundled into the same PR as the `/test-design-techniques` skill addition because it is a complementary internal-tooling change (one PR, one mainline integration). (#1497)
- Added the "計画の抽象度（WHAT/HOW 分離）" principle to `AGENTS.md` §"Plan モードのルール" as a single bullet (one line, keeping AGENTS.md under the 170-line policy from #1498) and introduced a new `.claude/skills/plan-rubric/SKILL.md` skill that evaluates plans on four pass/fail axes (抽象度 WHAT/HOW, スコープ, テスト可能性, 依存表明) before `ExitPlanMode`. The skill includes ry-specific OK/NG examples (stdlib モジュール追加 with `__ry_crypto_sha256` signature anti-pattern, codegen エラー報告 with `emitArithmeticOp` line-number anti-pattern) for the WHAT/HOW axis, defines how to invoke `/test-design-techniques` from a Plan (technique names are listed at Plan-stage as WHAT; concrete cases are expanded later in the Red step as HOW), and documents when to invoke the `devils-advocate` agent (via `Agent` tool with `subagent_type: devils-advocate`) at ExitPlanMode for architectural plans, with skip criteria for single-file bug fixes. The skill is read-only — it inspects plans rather than editing them. Knowledge-base only — no code, ABI, or build behavior changes. (#1501)

### Changed

- Reduced `AGENTS.md` from 255 lines / 20,594 bytes to 164 lines / 12,618 bytes by delegating four duplicated sections to existing rules and a new skill, and compressing additional verbose prose. Sections delegated: compiler warning flags → `.claude/rules/build-warning-flags.md` (zero-warnings policy and `-Werror` status added); LLVM IR golden-test conventions → `.claude/rules/codegen-llvm-ir-conventions.md` (`--emit-llvm-ir` pipeline, manual run command, CI warn-only note added to existing entry); runtime memory-safety table → `.claude/rules/runtime-memory-safety.md` (forbidden-function table, `oom_abort(n)`, NULL-check rule, CI lint note added as new entry); internal behavior trace usage → new `.claude/skills/ry-trace/SKILL.md` with auto-fire description (trigger keywords: `trace`, `--trace`, `--trace-out`, `JSON Lines`, `内部挙動`, `import 解決`, `JIT 実行`). Additionally compressed in-place: terminology preamble, CI container image description, knowledge-base meta section, ASan/UBSan blockquote, Plan-mode prerequisites, repo build & stdlib resolution section, PR-review learning section, workflow overview, issue-driven development section. The heading `## FileCheck IR Golden Tests` was renamed to `## IR ゴールデンテスト` to satisfy the issue's grep-based acceptance criterion (`grep -nE "checked_malloc|FileCheck|-Wall -Wextra"` returns 0 hits). (#1498)
- Simplified `.claude/skills/tdd-cycle/SKILL.md` from 39 lines to 25 lines in line with issue #1500: the "新機能追加時" 3-step enumeration is replaced with a single sentence delegating to general TDD (Red-Green-Refactor) and noting "each test case runs its own internal cycle; do not mark complete after the happy path"; the "既存コードの変更時" 6-step ry-specific flow is preserved verbatim; `## Cross-reference` is compressed from 4 bullets to 2 (テスト観点 / 完了前) while keeping `/test-design-techniques`, `/test-checklist`, and `/pre-commit-checklist` cross-references; the intro sentence, source-of-truth note, and `## Context` section are dropped. A new top-level bullet `**TDD サイクルの分割禁止**` is added to `AGENTS.md §"Plan モードのルール"` prohibiting Red/Green/Refactor split into separate Plan tasks (each case runs its cycle internally). The 2-row `/tdd-cycle` invocation table at `.claude/skills/test-checklist/SKILL.md` is collapsed into a single prose line to follow tdd-cycle's Step renumbering. Knowledge-base only — no code, ABI, or build behavior changes. (#1500)
- Added a `.claude/agents/<name>.md` bullet to the `AGENTS.md` ナレッジベース section so contributors and Claude Code learn, on first read, that subagent definitions live alongside `.claude/rules/` and `.claude/skills/` and which directory holds them. The new bullet documents that agents launch via the `Agent` tool's `subagent_type: <name>` parameter as an **独立コンテキスト**, contrasting with skills which execute in the same context, and notes that `/<name>` slash commands do not address agents (skill ではなく agent のため) — wording that mirrors the existing explanation already in `.claude/skills/plan-rubric/SKILL.md` line 148. The use case (Plan / 設計 / 実装 の批評で artifact だけを別コンテキストに渡したいケース) and the only currently-defined agent (`.claude/agents/devils-advocate.md`) are also called out so the section is accurate today rather than describing a hypothetical fleet. The section heading was extended from `## ナレッジベース (.claude/rules/ + .claude/skills/)` to `## ナレッジベース (.claude/rules/ + .claude/skills/ + .claude/agents/)` for consistency with the new bullet. The "読む" / "書く" / "いつ書く" trailing bullets are intentionally left scoped to `.claude/rules/` + `.claude/skills/` because they describe the lessons-learned workflow (entry format `### <heading>` + `**Source**:` + `**Tags**:` + `**Rule**:`) that does not apply to agent definitions; the agents directory holds subagent specs, not accumulated lessons. The parent #1496 acceptance criterion ("rename `devils-advocate.md` to `critic.md` or create a new `critic.md`") is intentionally not pursued: per the issue-author's call during the Plan-mode AskUserQuestion, the file stays named `devils-advocate.md` and `.claude/skills/plan-rubric/SKILL.md`'s nine `devils-advocate` references are kept as-is. Knowledge-base only — no code, ABI, or build behavior changes. (#1515)
- Refreshed the Cppcheck section of `.claude/rules/build-warning-flags.md` to match the post-#1505 CI environment, where the lint job uses the source-built cppcheck 2.16.0 bundled in the `ghcr.io/<owner>/ry-ci:llvm-21` image (`docker/ci.Dockerfile:163-186`) rather than the Ubuntu 24.04 apt package (cppcheck 2.13). Removed the stale "Gotcha: Cppcheck 2.13 ... does NOT support `#` comment lines in `--suppressions-list` files" paragraph, since the constraint no longer applies under 2.16 (`#` comment support was added in 2.14) — keeping it would also have contradicted the section's own Rule that suppressions must carry a rationale comment. Added a short paragraph after the Rule documenting that inline `// cppcheck-suppress <id>` comments are also supported (CI already invokes `cppcheck --inline-suppr`) and explaining when to prefer them over `.cppcheck-suppressions` entries. Added `#`-prefixed rationale comments to the two existing entries in `.cppcheck-suppressions` (`unknownMacro` global and `syntaxError:src/test_runtime.cpp`) so the file complies with the Rule. Existing inline `// cppcheck-suppress` annotations in `src/runtime_http*.cpp` and `src/runtime_sort.cpp` are left untouched: the Rule covers newly added suppressions, and re-annotating historical ones is out of scope. (#1519)

### Fixed

- Pinned `release.yml`'s Linux container reference from the mutable `ghcr.io/<owner>/ry-ci-glibc-old:llvm-21` pointer to the immutable per-build tag `:llvm-21-rev3` so that re-running the release workflow on an older `vX.Y.Z` source tag pulls the same image bits as the original release. Previously, `build-ci-image.yml`'s `manifest` job overwrote `:llvm-21` on every rebuild, which meant a re-run for an older release would silently pick up today's image and produce bit-different binaries — invalidating downstream `sha256sums.txt`, signatures, and any distro-packager checksum mirrors. The immutable `:llvm-<MAJOR>-rev<N>` tag is published in parallel by the same `manifest` job and is monotonically increasing (never reused), so static-pinning it inside `release.yml` makes the workflow file itself a record of which image rev was used at release time. Other workflows (`ci.yml`, `codeql.yml`, dev `docker/Dockerfile`) continue to use the mutable `:llvm-21` pointer because their artifacts never leave the runner and they should auto-track the latest image. The new convention is documented as a rule in `.claude/rules/ci-workflows.md` ("Release container must pin to immutable `:llvm-<MAJOR>-rev<N>` tag") and a procedure in `.claude/skills/ci-image-workflow/SKILL.md` ("How to update release.yml's pin"), and `.claude/skills/preparing-for-release/SKILL.md` Step 4 now includes a Release-prep task that verifies the pin is fresh against the latest published rev (using the public GHCR token endpoint rather than `gh api .../packages/...` which would require a `read:packages` PAT scope). Knowledge-base + workflow YAML only — no code, ABI, or test behavior changes; E2E verification is only possible at the next real release because `release.yml` runs on `tag` push only. (#1508)
- `ry self-update --nightly` now returns an explicit "no longer supported" error instead of the confusing `Error: Version v--nightly not found.` message. The `--nightly` mode was removed in v0.0.14 (#1372) when self-update was simplified to always target the latest stable release, but `--nightly` still slipped through `is_valid_tag()`'s lenient regex (`^v?[0-9A-Za-z._-]+$`) and was sent to the GitHub release-search path, yielding a misleading error. `cmd_self_update()` (`src/self_update.cpp`) now intercepts the exact string `--nightly` between the `--help` check and the `is_valid_tag()` check, prints `Error: --nightly is no longer supported. self-update always targets the latest stable release.` followed by `(removed in v0.0.14 — see #1372)` to stderr, and returns exit code 1 — directing users at the historical removal PR rather than implying the tag is missing. Match scope is exact-string only: `--nightly=value` and other variants still fall through to `is_valid_tag()` as before, so this is a pure addition of one rejection branch with no behavior change to any other input. Regression test: `HelpOption.SelfUpdateNightlyRejected` in `tests/test_help.cpp` (uses the existing `runRy` fork/exec helper to assert non-zero exit and that stderr contains both `--nightly is no longer supported` and `v0.0.14`). The internal `mode` abstraction's residual nightly-aware branches are out of scope for this CLI UX fix and tracked separately. (#1533)

## [0.0.17] - 2026-05-01

### Added

- New `docs/reference/glossary.md` defining the canonical terminology used across the Ry reference: **Module** (`xxx` in `from xxx import ...` — either a single `.ry` file or a directory of `.ry` files), **Package** (an external library managed by the `ry` command via the planned `ry add` / `ry remove` subcommands — **not yet implemented as of v0.0.17**), **`package.toml`** (the project manifest, named by analogy with Rust's `Cargo.toml` even though it describes a project rather than a package in the strict sense), and **stdlib (`std`)** (the auto-imported standard library — `math`, `io`, `path`, `filesystem`, etc.). Indexed at the top of `docs/README.md` so subsequent reference pages can defer to it. This glossary is the basis for follow-up terminology cleanups in v0.0.17. (#1480)

### Changed

- Renamed `docs/reference/packages.md` to `docs/reference/modules.md` and rewrote the page under v0.0.17's module/package terminology (introduced by #1480's glossary). The page is now titled "Module Reference" and uses "module" exclusively for `from ... import ...` units; the word "package" is reserved for the future `ry add` external-library feature. Section headers updated: "Package Resolution" → "Module Resolution", "Directory Packages" → "Directory Modules", "Sub-packages" → "Sub-modules", "Single File Package" / "Directory Package" → "Single File Module" / "Directory Module"; the `__ry_<package>_<symbol>` placeholder in the Native Function Naming Convention section was renamed to `__ry_<module>_<symbol>`; in-prose import-syntax placeholders were also updated (`from pkg` / `from .pkg` → `from mymodule` / `from .submodule`) so no `pkg` (an abbreviation derivative of "package") survives in the page. Anchor-bearing headers ("Standard Library (`std`)" and "RY_ENV") preserved verbatim so existing inbound links keep resolving. Cross-page link targets updated in `docs/README.md`, `docs/reference/glossary.md`, `docs/reference/project.md`, `docs/reference/directives.md`, `docs/reference/builtins.md`, `docs/reference/naming.md`, and a doc-link comment in `tests/test_parser.cpp:1870-1871` (also rephrased "Package-private" → "Module-private" in that comment for consistency with the new `naming.md` prose); `glossary.md`'s pre-existing-names note was simplified after `packages.md` ceased to exist (only `package.toml` remains as a stability-preserved name); `naming.md` prose changed "package-private names" to "module-private names"; `directives.md`'s `@native` paragraph changed "stdlib packages" / "legacy packages" to "stdlib modules" / "legacy modules" for cross-page consistency. The C ABI symbols themselves (`__ry_base64_encode`, `__ry_filesystem_listDir`, etc.) are unchanged — this is a documentation-terminology rename only. (#1481)
- Swept residual "package" terminology in the root `README.md` and `docs/reference/*.md` so that every stdlib unit imported via `from ... import ...` is consistently called a **module**, completing the v0.0.17 module/package terminology rollout begun by #1480 (glossary) and #1481 (`packages.md` → `modules.md`). `README.md`: feature-list bullet "Packages — Directory-based packages, auto-imported `std` library" → "Modules — Directory-based modules, auto-imported `std` library", and the sample-code comment `# Package import` → `# Module import`. `docs/reference/directives.md`: user-defined-directives section opener "Packages can declare their own compile-time directives" → "Modules can declare their own compile-time directives"; "private to the declaring package" → "private to the declaring module" in the export/import paragraph; and the in-prose import-syntax placeholders `pkg/mod.ry` / `from pkg import directiveName` / `from mypkg import logged, cached` → `mymodule/mod.ry` / `from mymodule import directiveName` / `from mymodule import logged, cached` so no `pkg` (a derivative spelling of "package") survives in the page (matching the placeholder convention #1481 established in `modules.md`). `docs/reference/{math,json,gc,thread,filesystem}.md`: package → module in the opening prose introducing each stdlib unit (e.g. "The `math` package provides..." → "The `math` module provides..."); `math.md` also corrects "Unlike the `std` package" → "Unlike the `std` module"; `json.md` corrects the second mention "the package uses an opaque pointer type" → "the module uses..."; `filesystem.md` updates both "the `filesystem` package" and "the `io` package" in the same sentence. `docs/reference/thread.md`: also updates the "thread package" cell in the async/await comparison table to "thread module". `docs/reference/builtins.md`: `input` description "explicitly scoping I/O through the `io` package" → "...the `io` module". `docs/reference/naming.md`: approved-abbreviations table column "Filesystem package" → "Filesystem module" for the `mkdir` / `mkdirAll` rows. Intentionally preserved: the manifest filename `package.toml` and every reference to it across `docs/reference/{project,testing,modules,builtins,glossary}.md` and `docs/README.md` (kept by analogy with Rust's `Cargo.toml` per the glossary's stability note); the `## Package` and ``## `package.toml` `` entries in `docs/reference/glossary.md` (canonical definitions of the v0.0.17 terminology); and the "Module, Package, `package.toml`, stdlib" enumeration in `docs/README.md`'s reference index (it advertises the four glossary entries, all of which still exist). Package management functionality (`ry add` / `ry remove`) remains unimplemented and is reserved for a future release. (#1482)
- **Breaking:** Aligned internal compiler error messages and the import-resolve trace event with the v0.0.17 module/package terminology (introduced by #1480's glossary). User-facing import errors now say "module" wherever they previously said "package": `package not found: X` → `module not found: X` (`src/module_loader.cpp:245,272`), `'name' not found in package 'X'` → `'name' not found in module 'X'` (`:108,375`), `cannot import private symbol 'X' from package 'Y'` → `... from module 'Y'` (`:81,370`), `invalid package path (path traversal): X` / `invalid package path (absolute): X` → `invalid module path (path traversal): X` / `invalid module path (absolute): X` (`:166,168`); parser-side `expected package name after 'from'` → `expected module name after 'from'` (`src/parser.cpp:393`) and `hyphens '-' are not allowed in package names; use underscores '_' instead` → `hyphens '-' are not allowed in module names; use underscores '_' instead` (`:353`). The `import.resolve.error` trace event's `detail` field value changed from `"package not found"` to `"module not found"` — this is the observable breaking change for tooling that consumes `--trace-out` JSON Lines and matches on the `detail` string. Internal API rename: the `ModuleLoader::loadPackageDir(abs_dir_path)` member function is renamed to `loadModuleDir(abs_dir_path)` (`include/ry/module_loader.hpp:56`, `src/module_loader.cpp:310,384`); only the in-process header API is affected — there is no external C ABI surface for this method. Header / implementation comments referring to "package/file", "package_path" cache key, "Resolve a package path", "from a package directory", "Directory (package)", and "Exclude test files from package loading" are likewise updated to "module" wording. Six regression tests lock in the new wording: `ParserTest.{ImportHyphenError,ImportRelativeHyphenError,ImportFromMissingModuleNameError}` in `tests/test_parser.cpp` and `ImportTest.{ModuleNotFoundErrorMentionsModule,NameNotFoundErrorMentionsModule,PrivateImportErrorMentionsModule}` in `tests/test_codegen_stmt.cpp` — each new test asserts both the presence of the new "module" wording and the absence of the old "package" wording. The `package_path` parameter name in `ModuleLoader::resolve` is intentionally preserved (the issue scoped wording-only edits to comments containing `package_path`, not the parameter itself); the pre-existing internal asymmetry between the `module_path` trace key and the `package_path` parameter it reads from is retained for this PR. The manifest filename `package.toml` is similarly preserved per #1480's stability note. (#1483)
- Refreshed the internal knowledge base (`.claude/skills/`, `.claude/rules/`, `AGENTS.md`) to v0.0.17's module/package terminology, completing the rollout begun by #1480/#1481/#1482/#1483. The `.claude/skills/stdlib-package-add/` skill is renamed to `.claude/skills/stdlib-module-add/` (with `git mv` to preserve history) and its `SKILL.md` rewrites the procedure prose to "stdlib module" (placeholders also flipped: `<pkg>` → `<mod>`, `@native("pkg")` → `@native("mod")`); the path-scoped rule `.claude/rules/stdlib-package-additions.md` is renamed to `.claude/rules/stdlib-module-additions.md` and its prose / placeholders updated identically. Cross-references inside the skill body and the rule body that pointed to the old filenames are repointed; four sibling rule files (`tests-arc-leak-pattern.md`, `tests-cpp-conventions.md`, `codegen-stdlib-dispatcher.md`, `runtime-memory-safety.md`) and `build-warning-flags.md`'s self-registration paragraph have their "stdlib package(s)" / "stdlib-package" prose flipped to the module spelling. A second-pass `[Pp]kg` derivative grep (per the `docs-reference-conventions.md` "lexical derivatives" methodology) caught additional `<pkg>` placeholders inside `tests-cpp-conventions.md` (`share/std/<pkg>/<pkg>.ry`, `__ry_<pkg>_<oldName>`, `runSource("...@native(\"<pkg>\")...")`, `from pkg import _name`) and `codegen-stdlib-dispatcher.md` (`share/std/<pkg>/<pkg>.ry`, `libry_<pkg>.dylib`, `used_native_libraries_.insert("<pkg>")`, `@native("pkg")` cross-reference) — all flipped to `<mod>` / `mod` for consistency with the renamed skill body. Two further skills picked up incidentally from the `[Pp]ackage` grep: `commands-environment-gotchas/SKILL.md` updates its quoted module-loader error from `'encode_bytes' not found in package 'base64'` to `'encodeBytes' not found in module 'base64'` so the example matches the diagnostic emitted by `src/module_loader.cpp:108,375` after #1483 (and the camelCase function spelling adopted in #1415); `git-branch-naming/SKILL.md` updates the `feat/add-crypto-stdlib` example caption from "new standard library package" to "new standard library module". `AGENTS.md` gains a top-level terminology note pointing at `docs/reference/glossary.md` so contributors land on the canonical v0.0.17 definitions on first read. Intentionally preserved: legacy C++ identifiers (`effectivePackage`, `deriveNativePackage`, `RY_REGISTER_STDLIB_PACKAGE`), ABI symbols (`__ry_<symbol>` prefix family), apt/OS package mentions in CI prose (`build-warning-flags.md:114` "Ubuntu 24.04 package", `ci-workflows.md` "LLVM packages", `static-analysis-tools/SKILL.md:66` "apt パッケージ", `llvm-mirror-workflow/SKILL.md`'s `extra-packages` action input), and the `package`/`pkg` example tokens inside `docs-reference-conventions.md`'s terminology-sweep methodology rule (those are the rule's own worked example, not stale prose). The manifest filename `package.toml` is also kept verbatim per the glossary's stability note. This is a documentation/knowledge-base change only; no code or build behavior is affected. (#1484)
- Final v0.0.17 terminology sweep: replaced remaining incorrectly-used "package" (in the sense of "module") references across stdlib `.ry` source comments, test descriptions, GoogleTest names, and C++ source/header prose comments with "module", completing the cleanup begun by #1480/#1482/#1483/#1484. Edits cover (A-1) `share/std/filesystem/filesystem.ry` and `share/std/runtime_internal/runtime_internal.ry` Ry comments; (A-2) `tests/spec/relative_import/relative_import.test.ry` `it()` description and 6 GoogleTest names in `tests/test_codegen_stmt.cpp` (`DirectoryPackage{ImportAll,SelectiveImport,SkipsUnderscoreFiles,FallbackToFile}` → `DirectoryModule*`) and `tests/test_parser.cpp` (`RelativeImportDot{Subpackage,NestedSubpackage}` → `*Submodule`); (A-3) C++ source comments in `src/codegen_call_runtime_internal.cpp`, `src/runtime_arc_counter.cpp`, and `src/codegen_fn.cpp`; (A-4) C++ header/source prose comments in `include/ry/stdlib_registry.hpp`, `include/ry/codegen.hpp`, `include/ry/codegen_native_dispatch.hpp`, `src/codegen_call_dispatch.cpp`, and `src/codegen_call_native.cpp`; (A-5) `effectivePackage` description prose in `src/codegen_fn.cpp:578` rewritten to "use library name as the effective module"; (B-3) `src/project_config.cpp:195` "in package name" → "in project name" (the `[project] name` field is what is being normalized, so the original prose was a `package` term misuse); (B-6) `src/codegen_call_native.cpp:597` placeholder comment `__ry_<package>_<fn_name>` → `__ry_<module>_<fn_name>`. Intentionally preserved (legacy ABI / source-stability identifiers per the glossary): `effectivePackage` / `deriveNativePackage` C++ variables, `RY_REGISTER_STDLIB_PACKAGE` macro, `StdlibPackageEntry` struct, `sig.package` field, the `__ry_<symbol>` C symbol prefix family, and the `package.toml` manifest filename. Test identifiers in `tests/test_builtin_stdlib_registry.cpp` and `tests/test_codegen_directive.cpp` are kept as-is because they directly assert on the legacy `StdlibPackageEntry::package_name` and `sig.package` fields. Parameter names like `package_path` (`include/ry/module_loader.hpp:53`) and `package` (`include/ry/codegen.hpp`) are also kept; renaming them is tracked separately to keep this PR's scope minimal. The historical "per-package docs were consulted" prose in `.claude/rules/docs-reference-conventions.md:264` is retained as-is because it documents a v0.0.17 pre-glossary lesson in its original wording. This is a documentation/knowledge-base / test-name change only; no code, ABI, or build behavior is affected. (#1490)

## [0.0.16] - 2026-04-30

### Added

- Reference documentation for Ry's v0.0.16 naming conventions (`docs/reference/naming.md`): camelCase for functions/variables/fields, PascalCase for records/enums/type aliases, acronym first-letter-only rule, approved abbreviations table, and verbose-by-intent rationale for `toInt`/`toStr`. (#1410)

### Changed

- **Breaking:** Renamed three built-in functions to align with the v0.0.16 naming conventions: `length(...)` → `len(...)` (all overloads — `str`, `List`, `Map`, `Set`, and the JSON value form), `arguments()` → `args()`, and `available_parallelism()` → `availableParallelism()`. The old names are removed entirely; there is no alias or deprecation period. `print`, `input`, `range`, `zip`, `exit`, `sleep`, `env`, and `enumerate` keep their existing names (`enumerate` cannot be shortened because `enum` is a reserved keyword). (#1411)
- **Breaking:** Renamed string-family stdlib functions to align with the v0.0.16 naming conventions. `share/std/str`: `starts_with` → `startsWith`, `ends_with` → `endsWith`, `byte_len` → `byteLen`, `substring` → `substr` (only this one shortens, per the approved abbreviation table), `char_at` → `charAt`, `to_upper` → `toUpper`, `to_lower` → `toLower`, `trim_start` → `trimStart`, `trim_end` → `trimEnd`. `contains`, `find`, `replace`, `trim`, `repeat`, `reverse`, `split`, `join` are already single-word and unchanged. `share/std/convert`: `to_int` → `toInt`, `to_str` → `toStr`, `to_float` → `toFloat` (the `to`-prefix is intentional, not abbreviated to `int` / `str` / `float`, to avoid colliding with the type-name spellings). `share/std/json`: `to_str` / `to_int` / `to_float` / `to_bool` → `toStr` / `toInt` / `toFloat` / `toBool`, and `json_free` → `jsonFree`. `share/std/regex`: `regex_match` / `regex_search` / `regex_replace` / `regex_split` / `regex_find_all` → `regexMatch` / `regexSearch` / `regexReplace` / `regexSplit` / `regexFindAll`, and `is_match` / `find_all` → `isMatch` / `findAll`. The old names are removed entirely; there is no alias or deprecation period. (#1412)
- **Breaking:** Renamed collection-family stdlib functions to align with the v0.0.16 naming conventions. `share/std/list`: `remove_at` → `removeAt`, `flatten` → `flat` (per the approved abbreviation table), `is_empty` → `isEmpty`. `share/std/map`: `has_key` → `hasKey`. `share/std/set`: `symmetric_difference` → `symmetricDifference`, `is_subset` → `isSubset`, `is_superset` → `isSuperset`. `share/std/higher_order` requires no renames. The old names are removed entirely; there is no alias or deprecation period. (#1413)
- **Breaking:** Renamed `math.Inf` → `math.INF` and `math.NaN` → `math.NAN` so the only mathematical constants exported from `share/std/math` use SCREAMING_SNAKE_CASE consistently with `math.PI` and `math.E`. The previous PascalCase spellings were the only stdlib constants that did not follow the camelCase-or-SCREAMING_SNAKE_CASE rule. The old names are removed entirely; there is no alias or deprecation period. (#1470)
- **Breaking:** Renamed path-family and filesystem-family stdlib functions to align with the v0.0.16 naming conventions. `share/std/path`: `extension` → `ext` (per the approved abbreviation table), `is_absolute` → `isAbsolute`. `share/std/filesystem`: `make_dir` → `mkdir` (POSIX-aligned short form), `make_dir_all` → `mkdirAll`, `glob_files` → `glob` (the suffix is redundant given the package), `list_dir` → `listDir`, `remove_all` → `removeAll`, `file_size` → `fileSize`, `is_file` → `isFile`, `is_dir` → `isDir`, `is_symlink` → `isSymlink`, `read_link` → `readLink`. `walk`, `copy`, `move`, `remove`, `chmod`, and `symlink` are already single-word and unchanged. The old names are removed entirely; there is no alias or deprecation period. (#1414)
- **Breaking:** Renamed I/O-network family stdlib functions to align with the v0.0.16 naming conventions. `share/std/io`: `read_line` → `readLine`, `read_all` → `readAll`, `read_text` → `readText`, `write_text` → `writeText`, `append_text` → `appendText`, `delete_file` → `deleteFile`, `read_bytes` → `readBytes`, `write_bytes` → `writeBytes`, `to_bytes` → `toBytes`, `bytes_to_str` → `bytesToStr`. `share/std/net`: `listener_port` → `listenerPort`, `tls_connect` → `tlsConnect`, `set_timeout` → `setTimeout`, `set_receive_timeout` → `setReceiveTimeout`, `set_send_timeout` → `setSendTimeout`. `share/std/http`: `body_bytes` → `bodyBytes`, `query_all` → `queryAll`, `form_field` → `formField`, `form_fields` → `formFields`, `form_file` → `formFile`, `http_get` → `httpGet`, `http_post` → `httpPost`, `http_request` → `httpRequest`, `http_client_response_free` → `httpClientResponseFree`. `share/std/base64`: `encode_url_safe` → `encodeUrlSafe`, `decode_url_safe` → `decodeUrlSafe`, `encode_bytes` → `encodeBytes`, `encode_bytes_url_safe` → `encodeBytesUrlSafe`, `decode_bytes` → `decodeBytes`, `decode_bytes_url_safe` → `decodeBytesUrlSafe`. The old names are removed entirely; there is no alias or deprecation period. (#1415)
- **Breaking:** Renamed thread/atomic/math/gc/runtime_internal stdlib functions and codegen-only builtins to align with the v0.0.16 naming conventions. `share/std/thread`: `thread_spawn` → `threadSpawn`, `thread_join` → `threadJoin`, `lock_new` → `lockNew`, `lock_acquire` → `lockAcquire`, `lock_release` → `lockRelease`, `lock_free` → `lockFree`, `rwlock_new` → `rwlockNew`, `rwlock_read_lock` → `rwlockReadLock`, `rwlock_write_lock` → `rwlockWriteLock`, `rwlock_unlock` → `rwlockUnlock`, `rwlock_free` → `rwlockFree`, `semaphore_new` → `semaphoreNew`, `semaphore_acquire` → `semaphoreAcquire`, `semaphore_release` → `semaphoreRelease`, `semaphore_free` → `semaphoreFree`, `barrier_new` → `barrierNew`, `barrier_wait` → `barrierWait`, `barrier_free` → `barrierFree`, `atomic_int_new` → `atomicIntNew`, `atomic_int_load` → `atomicIntLoad`, `atomic_int_store` → `atomicIntStore`, `atomic_int_add` → `atomicIntAdd`, `atomic_int_sub` → `atomicIntSub`, `atomic_int_cas` → `atomicIntCas`, `atomic_int_free` → `atomicIntFree`, `atomic_bool_new` → `atomicBoolNew`, `atomic_bool_load` → `atomicBoolLoad`, `atomic_bool_store` → `atomicBoolStore`, `atomic_bool_free` → `atomicBoolFree`. `share/std/math`: `is_nan` → `isNan`, `is_inf` → `isInf`. `share/std/gc`: `set_threshold` → `setThreshold`. `share/std/runtime_internal`: `arc_live_count` → `arcLiveCount`. Codegen-only builtins: `type_of` → `typeOf`, `block_on` → `blockOn`, `and_then` → `andThen`, `to_list` → `toList`, `checked_add`/`checked_sub`/`checked_mul` → `checkedAdd`/`checkedSub`/`checkedMul`, `saturating_add`/`saturating_sub`/`saturating_mul` → `saturatingAdd`/`saturatingSub`/`saturatingMul`, `wrapping_add`/`wrapping_sub`/`wrapping_mul` → `wrappingAdd`/`wrappingSub`/`wrappingMul`. Runtime symbols (`__ry_*`) follow the existing package-specific ABI convention: most packages mirror the Ry identifier spelling, while legacy `base64`/`string` symbols remain snake_case; `__ry_gc_set_threshold` was updated to `__ry_gc_setThreshold` under generic native dispatch. The old names are removed entirely; there is no alias or deprecation period. (#1416)
- Audited all `record` and `enum` declarations repo-wide for compliance with the v0.0.16 acronym rule (`HttpClient` not `HTTPClient`). Of 146 declarations, 6 test-only types had a multi-letter capital prefix and were corrected: `TAPoint` → `TaPoint`, `TAName` → `TaName`, `NTRecA`/`B`/`C` → `NtRecA`/`B`/`C` (`tests/spec/type_advanced.test.ry`), and `FStrExplicit` → `FstrExplicit` (`tests/spec/type_of.test.ry`). Additionally, 8 type aliases in `tests/spec/type_advanced.test.ry` were renamed for visual consistency with the renamed records: `TAVal` → `TaVal`, `TAShape` → `TaShape`, `NTAInner`/`Outer` → `NtaInner`/`Outer`, `NTRInner`/`Outer` → `NtrInner`/`Outer`, `NCycA`/`B` → `NcycA`/`B`. Inner-scope short placeholder aliases (`NA1`–`NC4` inside `it()` bodies) were left as-is — they are test-local and produce no visual mismatch with the renamed records. (#1417)
- **Breaking:** Flipped the parser's user-identifier casing enforcement from `snake_case` to `camelCase`, completing the v0.0.16 naming convention switch. User-defined `fn` and `@directive` names now require camelCase via `isMutationFnName` (which still permits the trailing `!` mutation suffix, e.g. `clear!`, `appendInPlace!`); `@native fn` declarations continue to permit `SCREAMING_SNAKE_CASE` for FFI-style names. Function parameters, record fields, enum ADT variant fields, `for`-loop variables, and `@directive` parameters are validated with `isCamelCase` (extended to accept an optional leading `_` for package-private names). PascalCase enforcement on records, enums, enum variants, and type aliases is unchanged. The migration moved 292 user-defined `fn` declarations across `tests/spec/` and the embedded Ry source inside `tests/*.cpp` to camelCase; existing parser fixtures that locked the old `EXPECT_THROW` were flipped or paired with positive `EXPECT_EQ(prog.size(), 1u)` tests so the new spec is anchored under the same input. There is no alias layer — old snake_case identifiers in user code are now hard rejects. (#1443)
- **Breaking:** Renamed all 18 `expect()` test DSL matchers from `snake_case` to `camelCase`, completing the v0.0.16 naming convention migration for the testing layer (parent #1409, parser flip #1443): `to_eq` → `toEq`, `to_not_eq` → `toNotEq`, `to_contain` → `toContain`, `to_not_contain` → `toNotContain`, `to_be_greater_than` → `toBeGreaterThan`, `to_be_less_than` → `toBeLessThan`, `to_be_greater_than_or_eq` → `toBeGreaterThanOrEq`, `to_be_less_than_or_eq` → `toBeLessThanOrEq`, `to_have_length` → `toHaveLen` (also applies the `length` → `len` rule from #1411), `to_start_with` → `toStartWith`, `to_end_with` → `toEndWith`, `to_be_true` → `toBeTrue`, `to_be_false` → `toBeFalse`, `to_be_none` → `toBeNone`, `to_be_some` → `toBeSome`, `to_be_ok` → `toBeOk`, `to_be_err` → `toBeErr`, `to_be_empty` → `toBeEmpty`. The parser whitelists in `src/parser.cpp`, the dispatch chain and runtime error wording in `src/codegen_test.cpp`, the `ExpectStmt::matcher` field comment in `include/ry/ast.hpp`, every `*.test.ry` call site under `tests/spec/`, every embedded Ry literal inside `tests/*.cpp`, and the matcher table plus examples in `docs/reference/testing.md` (and adjacent reference pages) were all migrated together. Per the #1409 "no back-compat shim" stance there are no aliases — old `snake_case` matcher names are now hard rejects from the parser. (#1448)
- **Breaking:** Lambda parameters now also require `camelCase`, completing the v0.0.16 naming-convention sweep that #1443 began. `parseParenLambdaExpr` validates each parameter name with `isCamelCase` (the same rule used at five sites in `parser_decl.cpp` for `fn` parameters, record fields, etc., which permits a leading `_` for package-private names) and rejects snake_case with the existing wording `parameter name 'X' must be camelCase`. The validation is deferred until just past the closing `)` so that the speculative `parseParenLambdaExpr → tuple` fallback in `parsePrimary` does not swallow the diagnostic; a new `lambda_committed_` flag re-throws past the speculative `try { ... } catch (...)` once `'->' / '=>' / ':'` is observed. Tests `LambdaParamRejectsSnakeCase`, `LambdaParamAcceptsCamelCase`, and `LambdaParamAcceptsUnderscorePrefix` lock the rule. No source migration was required — the existing `tests/spec`, `share/std`, and `examples` lambdas were already camelCase or single-letter. (#1449)

### Fixed

- Parser now enforces camelCase on tuple-destructure LHS identifiers in both the parenthesized form (`(a, b) = expr`) and the bare form (`a, b = expr`), aligning with the v0.0.16 naming conventions enforced elsewhere by #1443. The `_` placeholder remains accepted at any position. (#1450)
- Parser now enforces camelCase on module-global typed declarations of the form `name: Type = value` (the keywordless implicit-binding form), closing the last gap left by #1443 / #1449 / #1450. SCREAMING_SNAKE_CASE remains accepted on `@native` and `@const` declarations. (#1470)
- Migration notes in `docs/reference/naming.md` now also document the camelCase enforcement extensions added by #1449 (lambda parameters), #1450 (tuple-destructure LHS identifiers in both `(a, b) = expr` and bare `a, b = expr` forms), and #1470 (module-global typed declarations of the form `name: Type = value`). Previously the section listed only #1443's seven categories, leaving readers to infer that the follow-up gaps were not enforced. (#1474)
- Migrated remaining snake_case identifiers in `docs/reference/*.md` example code to camelCase, completing the v0.0.16 documentation switch started in #1456. Touched 14 files: `directives.md` (`old_value` → `oldValue`, `test_handle`, `make_inputs`, `test_property`, `test_name`, `test_add`, `test_add_each`, `test_commutative`, `group_name`, `test_nested`, `arithmetic_tests`, `test_sub`, `test_mul`, `shared_setup_tests`, `test_base`, `test_combined`, `test_deep`, `hot_path`, `medium_path`, `cold_error_handler`, `old_api`, `target_fn`, `target_fn2`, `slow_fn`, `fast_fn`, `other_fn`, `directive_name`, plus the `__ry_<libname>_<fn_name>` placeholder reworded to `__ry_<libname>_<symbol>` with prose clarifying that most packages mirror the Ry name verbatim while legacy `base64`/`string` packages keep snake_case C symbols); `testing.md`, `operators.md`, `http.md` (`port_holder` → `portHolder`, `file_info` → `fileInfo`), `collections.md` (`has_value` — kept ARC `strong_count` prose), `control-flow.md`, `packages.md` (`helper_fn`, `public_api`; `_helper` package-private convention preserved), `contracts.md` (`new_balance`, `min_balance`), `errors.md` (`max_int`), `functions.md` (`add_base`; `return_type` and `param_type1/2` syntax placeholders), `json.md` (`name_val`), `net.md` (`echo_server`), `thread.md` (`my_fn`), `types.md` (`light_spd`, `fn_val`, `max_u64`, `dynamic_value`). External or non-identifier strings preserved: `before_each`/`after_each` (referenced as a feature name not implemented), `strong_count`/`weak_count`/`byte_len` (C `StringHeader` field names), `content_type`/`session_id`/`filename` (HTTP form / cookie keys), filesystem paths, C++ binary names, and the `naming.md` historical prose contrasting old snake_case with new camelCase. (#1444)
- Aligned snake_case parameter / record-field names in `docs/reference/{builtins,builtins-string,directives,http}.md` with the camelCase identifiers actually declared in `share/std/`, completing a follow-up to the #1443 user-identifier camelCase migration. `builtins.md`: `duration_ms` → `durationMs`. `builtins-string.md`: `ignore_case` → `ignoreCase` (across `contains` / `startsWith` / `endsWith`). `directives.md`: the `@deprecated` record-field example switched from `old_setting` / `new_setting` to `oldSetting` / `newSetting` (the parser's camelCase enforcement on record fields previously rejected the example). `http.md`: `max_requests` → `maxRequests` and `port_callback` → `portCallback` in both signature tables and prose. (#1454)
- Renamed snake_case fn names in `docs/reference/{directives,http}.md` to camelCase, matching the parser's camelCase enforcement on `fn` declarations (split out from #1454 so the two PRs map 1:1 to scope). `directives.md`: the `@deprecated` on functions example switched from `old_function` to `oldFunction` (the prose comment `# warning: 'old_function' is deprecated` was updated in step). `http.md`: `on_port` → `onPort`, and `start_server` → `startServer` across both the "Non-blocking Server with `async fn`" and "Server with Request Limit" example blocks. Variable names like `port_holder` are parser-allowed and intentionally left as-is. Other snake_case fn declarations that remain in `docs/reference/` are tracked separately. (#1456)
- Migrated residual snake_case Ry identifiers in `.claude/rules/` and `.claude/skills/` knowledge-base examples to camelCase, closing the gap left by #1444 (`docs/reference/`) and #1451 (`tests/spec/`). Touched 10 files: `tests-spec-conventions.md` (`got_none` → `gotNone`, `is_dir` → `isDir`, `remove_all` → `removeAll`), `stdlib-package-additions.md` (`ignore_case` → `ignoreCase` parameter on the `@native fn startsWith` example, plus `find_all` → `findAll` Ry-API references), `test-checklist/SKILL.md` (`byte_len` → `byteLen`, `str_index` → `strIndex`, `try_parse` → `tryParse`, `bad_str_op` → `badStrOp`, `to_match` → `toMatch`, plus the runtime error-message expectation rewritten to match the actual diagnostic emitted by `codegen_expr_literal.cpp:638` — `"str does not support index access; use charAt(s, i) instead"`), `codegen-arc-cow.md` (`weak_ref`/`closure_arg` → `weakRef`/`closureArg` in inline-prose Ry syntax examples; the NUL-safety operations list — `byte_len` → `byteLen`, `starts_with`/`ends_with` → `startsWith`/`endsWith`, `char_at` → `charAt`, `is_empty` → `isEmpty`, `to_upper`/`to_lower`/`trim_start`/`trim_end` → `toUpper`/`toLower`/`trimStart`/`trimEnd`, the `regex_*` family and UFCS variants → `regexMatch`/`regexSearch`/`regexReplace`/`regexSplit`/`regexFindAll` and `isMatch`/`findAll`, `json.to_str` → `json.toStr`, `io.write_text`/`io.append_text`/`io.write_bytes` → `io.writeText`/`io.appendText`/`io.writeBytes`; the str-temporary example `"foo".to_upper()` → `"foo".toUpper()`; the diagnostic text quoted at line 467 also updated to the actual `charAt` form), `codegen-type-and-metadata.md` (`fn first_of` → `fn firstOf` to match `tests/spec/generic_infer_container.test.ry`; `to_str([["a","b"],["c"]])` → `toStr([...])`; the `http_get`/`http_post`/`http_request` HTTP-stdlib references → `httpGet`/`httpPost`/`httpRequest`; `mk_thread()` → `mkThread()`; `make_result_fn()` → `makeResultFn()` placeholder), `docs-reference-conventions.md` (`remove_at` → `removeAt` in stdlib declaration / test-assertion examples; the C++ symbol `emitCollOp_remove_at` was preserved as it remains the actual function name in `src/codegen_call_collection.cpp`), `parser-conventions.md` (`to_str` → `toStr`, `to_float` → `toFloat` in prose referencing the canonical category name and the runtime converter), `regex-engine.md` (`find_all` → `findAll`, `is_match` → `isMatch`, `regex_match` → `regexMatch`, plus the prefixed-form placeholder `regex_*` → `regex*` to match the modern API shape), `codegen-fn-and-generic.md` (`takes_float(4)` → `takesFloat(4)` placeholder), and `codegen-stdlib-dispatcher.md` (tag list `bytes_to_str, write_bytes` → `bytesToStr, writeBytes`). Intentionally left in snake_case: the `runtime_internal` package directory (`share/std/runtime_internal/`), `__ry_*` C runtime symbols (clarified in #1463), C++ field/parameter names (`byte_len` in StringHeader layout, `list_elem_type_name`, `record_types_`, `thread_result`, etc.), the `is_absolute` "old name" example in `tests-cpp-conventions.md` that illustrates rename blind-spots, the `<fn_name>`/`fn_val`/`add_base`/etc. "before" placeholders in the `docs-reference-conventions.md` migration-pattern entry, the `my_x`/`my_a`/etc. snake_case examples in `parser-conventions.md` that demonstrate `LambdaParamRejectsSnakeCase` rejections, and the `byte_len` prose mirroring the `__ry_str_cmp` C++ comment in `codegen_expr.cpp:1017`. (#1464)
- Migrated 7 residual snake_case variable bindings in `tests/spec/` to camelCase, closing the gap left by #1450 / #1451. The bindings used the implicit `name: type = value` form inside `it(...)` blocks (no `let`/`var` keyword), which is why the previous sweep missed them. Touched 5 files: `tests/spec/nul_safety_http_client.test.ry` (`no_headers` → `noHeaders`, ×3 sites), `tests/spec/nul_safety_http.test.ry` (`no_headers` → `noHeaders`), `tests/spec/concurrency_stress.test.ry` (`outer_log` → `outerLog`), `tests/spec/option_branch_merge_none.test.ry` (`inner_cond` → `innerCond`), and `tests/spec/list_concat_arc.test.ry` (`empty_a` → `emptyA`). The `nul_safety_http*` `describe(...)` label strings were intentionally left in snake_case to match the corresponding test file names. Applied via `sed`-based bulk substitution rather than per-site edits to make the rewrite exhaustive. (#1466)
- Replaced residual `length()` / `substring()` Ry-API mentions in prose with `len()` / `substr()` across `docs/` and `.claude/rules/` + `.claude/skills/`, closing the gap left by #1444 / #1456 / #1459 / #1464 (which only covered code blocks and code examples, not surrounding prose that cited the old names as the "current API"). Touched 7 files: `docs/README.md` (builtins description column), `docs/reference/packages.md` (two `Built-in functions` enumerations), `.claude/rules/codegen-arc-cow.md` (NUL-safety operations list — `length` → `len` and `substring` → `substr`), `.claude/rules/codegen-stdlib-dispatcher.md` (bare-builtins enumeration), `.claude/rules/codegen-type-and-metadata.md` (two `result.length()` references in lambda-return rule entries), `.claude/rules/tests-spec-conventions.md` (NUL-truncation example using `substring`), and `.claude/skills/test-checklist/SKILL.md` (multibyte-divergence checklist row). Intentionally preserved: the rename-history table and prose in `docs/reference/naming.md` (#1411 historical contrast), `CHANGELOG.md` past-version entries, English-prose uses of "length" / "substring" as nouns (`byte length`, `fixed-length array`, `substring check`, function parameter names like `find(string, substring)`), C++/C symbol references (`emitStrOp_substring`, `__ry_utf8_substring`, `**Tags**: substring`), and the historical bug citation `length(xs)` in `codegen-fn-and-generic.md:440` describing pre-#1411 dispatch-misroute behavior. (#1469)

## [0.0.15] - 2026-04-28

### Added

- User-defined `@directive` declarations imported via `from <pkg> import <name>` now register in a per-program directive table and are accepted by directive validation, alongside the built-in registry. Defining a directive whose name collides with a built-in, or registering the same directive name twice in one program, is rejected. Unknown named arguments on a user directive are also rejected. (#710)
- Reference documentation for the user-defined `@directive(target=...)` declaration syntax in `docs/reference/directives.md`, including `target` parameter values, parameter mapping rules, and the bootstrap rule for `@directive` and `@native`. Each existing built-in directive section is also labeled with its definition origin. (#1392)
- `DirectiveDefStmt` (e.g. `@directive(target="function") fn name(params)`) is now exportable from packages. Both wildcard (`from pkg`) and named (`from pkg import name`) imports include directive definitions, with the same `_`-prefix privacy rules as functions and types. (#709)

### Changed

- `@it` and `@describe` are now stdlib-package directives provided by `share/std/testing/testing.ry`. Test files that use them must add an explicit `from testing import it, describe` (or `from testing`) at the top. The directives are no longer in the C++ built-in directive registry. (#710)
- Migrated 6 built-in directives from the C++ registry to stdlib `.ry` declarations. `@inline`, `@parallel`, `@const`, and `@deprecated` are now declared in `share/std/core/directive.ry` and remain implicitly available via the `share/std/builtins.ry` re-export. `@each` and `@property` are now declared in `share/std/testing/testing.ry` and require an explicit `from testing import each, property` (or the subset used) — consistent with `@it` / `@describe`. Only `@directive` and `@native` remain as compiler built-ins (the bootstrap pair). (#1390)
- Defaulted parameters of user-defined `@directive` declarations may now be passed positionally in declaration order, in addition to the existing named-argument and omitted (default-value) forms. For example, given `fn logged(label: str = "info")`, all of `@logged("warn")`, `@logged(label="warn")`, and `@logged()` are now accepted. Previously the positional form was rejected with "accepts at most 0 positional argument(s)". Built-in directives (`@native`, etc.) are unaffected. (#1402)
- User-defined directives applied to a target outside their declared `target=[...]` list now silently no-op instead of triggering undefined behavior. The compile succeeds, no diagnostic is emitted, and the directive's argument validation is also skipped. Built-in directives are unaffected. Note that for-loop and function-call use sites still reject all user-defined directives at the parser level (tracked separately in #1427). (#1425)
- The parser now accepts user-defined directives on `for` statements and function-call statements. Previously every user-defined directive at those two sites was rejected at parse time, masking the codegen-level silent-no-op behavior introduced in #1425. The compiler built-in directive `@native` is still rejected at both sites; applying `@parallel` more than once on the same `for` loop is also still rejected. (#1427)

### Removed

- Removed the `stage` parameter from user-defined `@directive(...)` declarations. `@directive(target=[...]) fn name(...)` is now the canonical form. `@directive(target=[...], stage="compile")` is rejected as `unknown argument 'stage'` (hard error, no deprecation window). The `stage` knob conveyed no useful information today (only `"compile"` was accepted) and was reserved for a Tier 2 design (#1400) that has been declined. (#1408)

### Fixed

- User-defined `@directive` declarations now accept required parameters in named-argument form. Previously `@mydir(description="hi")` for `fn mydir(description: str)` was rejected with "unknown named argument"; now both `@mydir("hi")` and `@mydir(description="hi")` are accepted. Mixed positional+named for the same parameter is rejected as a duplicate, and missing required parameters produce a clearer error. (#1397)
- Removed dead language-switcher lines from 25 docs pages (`docs/README.md` and 24 `docs/reference/*.md` pages). Both the three-language `[English] | [日本語] | [繁體中文]` pattern (21 files) and the residual English-only self-link `[English](self.md)` (4 files) pointed to non-existent `docs/ja/` and `docs/zh/` trees. (#1398)
- Aligned directive terminology in a parser code comment (`src/parser_decl.cpp` `parseDirectiveDefStatement`), in the `@directive` definition section of `.claude/rules/parser-conventions.md`, and in the `README.md` / `docs/README.md` overview lines — now all consistently use "directive(s)" / "compile-time instructions" rather than "annotation(s)" / "decorating" / "compile-time metadata", matching the canonical definition in `docs/reference/directives.md`. (#1422)

## [0.0.14] - 2026-04-26

### Changed

- Self-referential enum diagnostic now also suggests `Task<T>` and `Channel<T>` as valid indirection wrappers, aligning the recommendation with the existing checker's acceptance. The message previously only mentioned `List`/`Map`/`Set`, even though pointer-backed `Task<T>` and `Channel<T>` are equally valid indirections. (#1351)
- `release.yml` now deletes the matching `vX.Y.Z-nightly` prerelease (and its tag) after a stable `vX.Y.Z` release is published, preventing `ry self-update` from pinning users to a stale nightly that predates the stable release. (#1365)
- Heavy CI analysis (`clang-tidy`, `scan-build`, `asan`, `tsan`) now runs on every pull request instead of only on `v*.*.*` branch pushes. CodeQL also runs per PR plus on push to `main`, replacing the previous daily cron. The redundant `ci-scheduled.yml` workflow has been removed. (#1367)
- Release workflow now triggers on tag push (`v*.*.*`) instead of `workflow_dispatch` only. Pushing a semver tag from `main` builds, tests, and publishes a GitHub Release in one shot. (#1369)

### Removed

- `VERSION` file removed. CI derives the version from `${GITHUB_REF_NAME#v}`; local builds default to `0.0.0`. (#1369)
- `ry self-update --nightly` flag and the implicit nightly default (when the running version had a prerelease suffix, `self-update` with no arguments previously targeted the latest prerelease). `self-update` now always targets the latest stable release unless an explicit version tag is given. The nightly build workflow (`dev-release.yml`) has been retired as part of this change. (#1372)

### Fixed

- Lambda return-type inference now correctly narrows `@native` overloads that differ only in ptr-backed argument types (`str` vs `List` vs `Map` vs `Set`). Previously `f = () => length(xs)` failed with "ambiguous @native call in lambda return-type inference". Captured collection variables also retain their source-level element/key/value type metadata so the body dispatches to the correct runtime overload. (#1349)
- `for a, b in setOfTuples:` no longer fails with "for loop destructuring requires a list of tuples". The multi-variable for-loop binding path now handles `Set<(T, U)>` alongside maps and lists of tuples, and source-level element type names on `Set<T>` annotations are propagated for non-primitive inner types (collections, records, enums, tuples). (#1350)
- `List<str>` and `Set<str>` literals now correctly retain locally-constructed str elements, preventing dangling pointers when source variables go out of scope. Mirrors the `Map<str, str>` literal fix from #1353. (#1354)

## [0.0.13] - 2026-04-24

### Added

- Parenthesized tuple destructuring assignment `(a, b) = expr` and
  `@const (a, b) = expr` (#1189). Mirrors the existing bare form
  `a, b = expr` and matches what the formatter has been emitting.
- `input()` / `input(prompt)` builtin — reads one line from standard input as the stdin counterpart of `print()`. Returns `""` on EOF with the trailing newline stripped. Available without `import`, mirroring Python's `input()` (#1261)
- Introduced LLVM FileCheck-based golden IR tests for codegen regressions (`tests/filecheck/`) (#897)
- Added `ry --emit-llvm-ir` flag to emit unoptimized LLVM IR to stdout without running the program (#897)

### Changed

- `x: float = 10` (int → float widening) and `x: int = 3.14` (float → int truncation toward zero) are now accepted without an explicit `as` cast. The same coercion applies to record field compound assign (`r.n **= 2`) and collection-element compound assign (`xs[0] **= 2`, `m["k"] **= 2`). Low-level numeric types (`i64`, `f32`, etc.) still require exact type match, and narrowing is still rejected at function arg / return / if-expr branch sites (#1192).
- Function return values now support implicit `int` ↔ `float` coercion, matching
  the behavior at variable declaration and reassignment sites. `-> float`
  functions accept `int` return values (widening), and `-> int` functions
  accept `float` return values (truncation toward zero). Low-level numeric
  types (`i64`, `f32`, etc.) still require explicit `as` casts. (#1195)
- `is_match(text, /pattern/)` now performs **partial (unanchored) search** — it returns `true` if the pattern matches anywhere in the text, consistent with its name and with `search()` / `regex_search()`. Previously it performed a full-string match. To require a full-string match, anchor the pattern explicitly with `^` and `$` (e.g. `/^[a-z]+$/`). The legacy string-pattern `regex_match(text, pattern)` is unchanged and still requires a full-string match (#1197).
- Self-referential enum fields such as
  `enum Tree: Leaf(int), Node(int, Tree, Tree)` and their generic
  counterparts `enum LList<T>: Cons(T, LList<T>)` now emit a helpful
  diagnostic pointing to wrapper types (`List<...>`, `Map<K, ...>`,
  `Set<...>`) at declaration time instead of the cryptic
  `unknown type: Tree` / `unknown type: T`. Compiling a generic enum
  name without type arguments in a signature (e.g. `opt: MyOpt`)
  likewise produces a clear error asking for `MyOpt<T>` (#1203).
- `reduce(list, fn)` now returns `Option<T>` (previously `T`) and returns `None`
  for an empty list instead of raising a runtime error. Unwrap with `?? default`
  or pattern match, e.g. `(reduce(xs, fn)) ?? 0`. `fold(list, init, fn)` is
  unchanged and remains the preferred function when you have a seed value.
  (#1209)
- Function types are written `fn(T1, ...) -> R` only; `function(...)` is no longer accepted as a type or declaration keyword.
- `type_of` / `to_str` category for function-typed values is reported as `"fn"` (was `"function"`).
- Trace `symbol_define` entries use kind `"fn"` for user-defined functions (was `"function"`).

### Removed

- The `function` keyword is removed; use `fn` for all function definitions and `async fn` for async definitions (#1343).

### Fixed

- Restored `HeaderFilterRegex` in `.clang-tidy` to `^include/ry/.*\.hpp$`, removing the unintentional `src/` inclusion added defensively in #950 (#1150)
- `None()` and bare `none` in `if`/`case` branch-merge positions now correctly
  adopt the sibling arm's `Option<T>` inner type instead of defaulting to
  `Option<i8>` or `Option<i64>` (#1154)
- Generic type constraint checks (`<T: RecordName>`) no longer reject
  type aliases that resolve to a record type. Both the bound and the
  concrete type argument are now resolved through the alias table
  before the subtype check, while error messages continue to report
  the user-written names. (#1155)
- `case <subject>: (a, b)` where the subject is `Option<T>` or `Result<T, E>`
  no longer silently destructures the LLVM struct layout as a tuple.
  Previously the TuplePattern arm's source-name-based guard was skipped when
  the subject had no enum annotation, allowing `{i1, T}` to pass arity
  validation and producing wrong IR or an `ICmp` type-mismatch crash.
  The pattern test now rejects these subjects structurally via
  `isTupleStructType`, independent of any source-level type name. (#1156)
- `coerceResultType` no longer silently drops the active payload when a
  function-returned `Result` is bound to a variable with a different `Result`
  annotation. Such mismatches are now rejected at compile time with an explicit
  type-error message (#1157)
- Fix f-string interpolation of enums with explicit discriminant values (`enum E { A = 5 }`) no longer misreads `byte_len` via a non-StringHeader pointer, which could truncate output or trigger UB on the unreachable default branch (#1159)
- `None()` and bare `none` in lambda variable call arguments now adopt the callee parameter's `Option<T>` inner type, so `g(None())` compiles where `g: (o: Option<str>) -> Option<str>`. Previously required a typed-variable workaround. (#1179)
- `lst[a..b]` (list range-indexing) no longer crashes at codegen with `ICmp`
  type mismatch between `ptr` and `i64`. The indexing path now detects a
  `RangeExpr` as the first index, negative-wraps each bound against the list
  length, and routes to the shared slice helper. Semantics match
  `slice(lst, a, b + 1)` (inclusive, out-of-bounds clamped, negatives wrap).
  (#1184)
- `contains(map, key)` and `m.contains(key)` now correctly perform map key lookup instead of always returning `false` (#1185)
- `None()` / `none` passed as a positional field value in a record/struct
  constructor now correctly inherits the field's `Option<T>` inner type,
  matching the behavior already available in `let` annotations, if/case
  branches, and lambda call arguments (#1186).
- Eliminate intermittent SIGABRT/SIGBUS in `ry test -p` triggered by
  `tests/spec/combinatorial/collection_element.test.ry` during JIT
  teardown by cancelling the ResourceTracker scope_exit before leaking
  the LLJIT (#1187)
- Formatter no longer emits a stray colon and space (`": "`) between the
  pattern and `=` in `TupleDestructStmt` output, which previously broke
  formatter → parser round-tripping for `@const` variants (#1189).
- `x: int = 2 ** 3`, `x: int = 10 / 2`, and `x **= n` / `x /= n` (where `x: int`) now compile successfully. `**` and `/` still return `float`, but high-level `int` and `float` variables implicitly accept cross-type values at declaration, reassignment, and compound assignment (#1192).
- `@native` stdlib functions (`math.sqrt`, `math.sin`, `math.cos`, `math.tan`, `math.asin`, `math.acos`, `math.atan`, `math.atan2`, `math.hypot`, `math.exp`, `math.log2`, `math.log10`, and other table-driven natives) now accept `int` arguments with implicit `int → float` widening, matching user-defined function overload resolution. Exact-match precedence is preserved: `pow(2, 3)` still dispatches to the `(int, int) -> int` overload (#1193)
- `slice(lst, start, end)` now resolves negative `start` / `end` as offsets from the end of the list (`length + idx`), consistent with Python-style indexing, subscript access, and the `lst[a..b]` range-index operator (#1184). Over-negative inputs are silently clamped to `0`. (#1198)
- `substring(s, start, end)` now resolves negative `start` / `end` as offsets from the end of the string (`length + idx`), consistent with Python-style indexing and matching `char_at()`, `slice()`, and `lst[-1]` subscript access. Over-negative inputs are silently clamped to `0`. (#1199)
- Generic enums can now be used as function parameter types, return
  types, and let-binding type annotations. Both fully-qualified forms
  (`MyOpt<int>`) and type-parameter-referencing forms (`MyOpt<T>` inside
  a generic function `fn<T>`) resolve correctly (#1203).
- `slice(lst, a, b)` / `lst[a..b]` now correctly retains ARC-managed
  reference-typed elements (`List<str>`, `List<List<T>>`, `List<Map<K,V>>`,
  closures), preventing use-after-free when the source list is dropped (#1204)
- Fix `lst[a..b]` and `slice(lst, a, b)` losing nested element metadata
  (`list_elem_type_name`, `list_elem_fn_type_info`, `nested_list_elem`,
  `map_value_type_name`) when slicing collections such as
  `List<List<int>>`, `List<Map<str, int>>`, or `List<function>`.
  Second-level access on the resulting slice (e.g.
  `slice(xs, 0, 1)[0][0]`) now works correctly. (#1205)
- Calling `reduce(list, init, fn)` with 3 arguments (Python/JS style) now
  reports a targeted compile error suggesting `fold(list, init, fn)` instead of
  the generic "takes exactly 2 arguments" message. (#1209)
- Reject `function operator +(...)` with whitespace between `operator` and a symbolic operator. Only the canonical `function operator+(...)` (no space) is now accepted for symbolic operators. Keyword operators (`in`, `as`, `and`, `or`, `not`) and bracket/call operators (`[]`, `[]=`, `()`) are unaffected. (#1210)
- `!!` error-propagation operator now works in expression position immediately after an identifier (e.g. `Ok(r!!)`, `Some(v!! + 1)`), matching the documented equivalence with `?`. The lexer previously consumed the trailing `!` as part of the identifier (to support mutating method names like `sort!`), so `r!!` tokenized as `r!` + `!` and failed to parse (#1211)
- `math` custom emitters (`floor` / `ceil` / `round` / `log` / `pow` mixed-type) now accept `int` arguments via implicit `int → float` widening, completing the fix started in #1193 for table-driven `@native` dispatch. Exact-match precedence is preserved: `pow(2, 3)` still returns int `8`, while `pow(2.0, 3)` and `pow(2, 3.0)` now return float `8.0` instead of erroring (#1230)
- `as int` / `as i64` / `as i32` / `as i16` / `as i8` / `as u8` /
  `as u16` / `as u32` / `as u64` casts and the implicit `float → int`
  coercions (`x: int = 1.0 / 0.0`, compound assignments such as `x /= 0`
  where `x: int`) now raise a runtime error and exit with status 1 when
  the source value is `NaN`, `±inf`, or outside the target integer's
  representable range. Previously these silently produced LLVM poison
  (undefined behavior) via `fptosi` / `fptoui`. (#1232)
- `floor()`, `ceil()`, `round()`, and `trunc()` now correctly accept
  `-9.223372036854776e+18` (exactly `INT64_MIN`) as input. The previous
  `fabs(x) >= 2^63` overflow guard incorrectly rejected this value. (#1232)
- `take(lst, n)` now ARC-retains reference-typed elements, preventing
  use-after-free when the source list is released (same defect class
  as #1204 for `emitListSlice`). (#1235)
- `List + List` concatenation now ARC-retains reference-typed elements,
  preventing use-after-free when either source list is released (same
  defect class as #1204 for `emitListSlice` and #1235 for `take()`). (#1236)
- ADT enum variant payload fields with collection (`List`/`Map`/`Set`), nested enum, `Option`, or `Result` types now format correctly via `print` / `to_str` instead of rendering as an empty string, raw tag integer, or wrongly-nested value. Self-referential ADTs such as `enum Tree: Node(int, List<Tree>)` now print faithfully (#1238).
- Fix `appended(lst, elem)` losing nested element metadata
  (`list_elem_type_name`, `list_elem_fn_type_info`, `nested_list_elem`,
  `map_value_type_name`) when appending to collections such as
  `List<List<int>>`, `List<Map<str, int>>`, or `List<function>`.
  Second-level access on the resulting list (e.g.
  `appended(xs, [5, 6])[0][0]`) now works correctly. (#1239)
- Fix `take(lst, n)` losing nested element metadata
  (`list_elem_type_name`, `list_elem_fn_type_info`, `nested_list_elem`,
  `map_value_type_name`) when taking the prefix of collections such as
  `List<List<int>>`, `List<Map<str, int>>`, or `List<function>`.
  Second-level access on the resulting list (e.g.
  `take(xs, 2)[0][0]`) now works correctly. (#1240)
- Fix `distinct(lst)` losing nested element metadata
  (`list_elem_type_name`, `list_elem_fn_type_info`, `nested_list_elem`,
  `map_value_type_name`) when deduplicating collections such as
  `List<Map<str, int>>` or `List<function>`.
  Second-level access on the resulting list (e.g.
  `distinct(xs)[0]["a"]`) now works correctly. (#1241)
- Whole-list reassignment (`xs = [...]`) now releases ARC-managed inner elements, preventing the ~3 ARC headers per iteration leak observed when rebinding `List<List<T>>`, `List<Map<K,V>>`, `List<Set<T>>`, `Map<K, List<V>>`, etc. inside a loop. Applies to List/Map/Set element types; str elements remain on the existing path. (#1242)
- `appended(list, elem)`, `insert(list, i, elem)`, and `merge(map1, map2)` now retain ARC-managed collection elements they duplicate from source containers, matching the retain-on-store discipline already used by `slice` / `take`. Without these retains, the destructor fix above would have introduced UAFs when a source container was rebound or went out of scope. (#1242)
- Parser no longer aborts on overflow or non-decimal integers in array type
  `T[N]`. `parseTypeNameSingle` now uses `strtoull` + `errno` instead of
  `std::stoull`, so inputs such as `T[99999999999999999999...]`, `T[0xFF]`,
  or `T[1_000]` are rejected with a structured diagnostic instead of crashing
  via uncaught `std::out_of_range` / `std::invalid_argument`. Discovered by
  `fuzz_parser`. (#1259)
- `distinct()` now emits a compile error for lists of non-string pointer
  elements such as `List<Map<K, V>>`, `List<function(...) -> R>`, and
  `List<Set<T>>`. Previously the guard only rejected `List<List<T>>` and
  silently fell through to a `strcmp` on non-C-string pointers, which is
  undefined behaviour. (#1262)
- ARC retain now fires for container element loads (`xs = ys[i]`,
  `v = m["k"]`, function return, call-site argument passing) for nested
  ARC containers and `List<str>` / `Map<K,str>` borrows. Previously
  missed in `AssignStmt`, `return`, caller-side argument passing, match
  binding, type coercion, and lambda capture — every caller of
  `tryRetainArcSource`. Prerequisite for the `#1242` destructor fix that
  makes nested collection headers reclaimable. (#1266)
- `remove()` on a list now emits a compile error for lists of non-string
  pointer elements such as `List<List<T>>`, `List<Map<K, V>>`, `List<Set<T>>`,
  and `List<function(...) -> R>`. Previously the guard only rejected
  `List<List<T>>` and silently fell through to a `strcmp` on non-C-string
  pointers, which is undefined behaviour. (#1268)
- The `in` / `not in` operator on a list now emits a compile error for lists
  of non-string pointer elements such as `List<List<T>>`, `List<Map<K, V>>`,
  `List<Set<T>>`, and `List<function(...) -> R>`. Previously there was no
  guard at all and the linear-search loop fell through to `strcmp` on
  non-C-string pointers (Map/Set/closure/list headers), which is undefined
  behaviour. Mirrors the `distinct()` (#1262) and `remove()` (#1268) guards.
  (#1269)
- `floor(x)`, `ceil(x)`, and `round(x)` with a single `int` argument now
  short-circuit and return the input unchanged. Previously the value was
  widened to `f64` and passed through `floor`/`ceil`/`round`, losing
  precision for magnitudes above `2^53`. The 2-argument form and the
  widening precedence rules from #1193/#1230 are unaffected (#1346).
- `for x in s:` on a `Set<T>` now reads element-type metadata from
  `set_elem_type_name` instead of `list_elem_type_name`. Previously
  iterating a `Set<str>` silently fell through to the `list_elem` path and
  misread the loaded element, producing wrong values at the use site
  (#1346).
- `m[k] = v` on an empty-then-inserted `Map<str, str>` now retains the str
  key and value at SetItem time. Previously the retain was gated on
  `mapKeyArcKind != CollectionKind::Str` / `mapValArcKind != CollectionKind::Str`
  (a stale leftover from the #1266 destructor-only carve-out), leaving
  both slots as weak references. When the local source strings went out of
  scope, the map's slots became dangling pointers and subsequent lookups
  surfaced as "map key not found". The Map/List/Set literal-construction
  variants have a different root cause and are tracked separately in
  #1347 (#1346).
- `m: Map<str, str> = {k: v}` (non-empty Map literal with str keys/values)
  now retains each str handle at literal-construction time and stamps
  `map_key_type_name = "str"` / `map_value_type_name = "str"` on the
  returned header so the destructor dispatches to the str-releasing
  variant. Previously the retain gate relied on
  `inferCollectionTypeName(val)`, which returns `""` for plain str values
  and short-circuits at `Empty`, so the map held borrowed references to
  locally-constructed strings. When the source strings went out of scope
  the map's slots became dangling pointers, reproducing the #1346
  "map key not found" symptom through the literal path. `retainArcValue`
  routes through `tryRetainArcSource` Case 2b (no-op for fresh `+1`
  `makeString` values) and Case 1 (emits retain for `LoadInst` from a
  bound variable alloca), preserving `#1266` counter symmetry. List/Set
  literal variants are deferred to v0.0.14+ because the #1266
  destructor-only carve-out for them has a different resolution path
  (#1347).
- Inline if-expression (`if cond: then-expr else: else-expr`) now accepts a
  newline between the then-branch expression and `else:`. Previously the
  parser rejected `if x > 0: x\nelse: -x` because the trailing Newline
  after the inline then-branch was treated as a statement terminator,
  causing `parseIfExpression` to fail on the missing `else` at the current
  token (#1346).

## [0.0.12] - 2026-04-18

### Added

- `in` and `not in` operators now support substring check when the right operand is a `str`.
  `"world" in "hello world"` evaluates to `true`; empty-needle `"" in s` evaluates to `true`
  to match Python and the existing `contains` semantics. (#1032)
- `base64.encode_bytes(List<u8>) -> str` and `base64.encode_bytes_url_safe(List<u8>) -> str` for encoding raw binary byte lists to base64 without going through `str` (#1130)
- `base64.decode_bytes(str) -> Result<List<u8>, Error>` and `base64.decode_bytes_url_safe(str) -> Result<List<u8>, Error>` for decoding base64 directly to raw bytes, preserving embedded NUL bytes and non-UTF-8 sequences (#1130)
- `to_eq` and `to_not_eq` test matchers now support `List`, `Set`, `Map`, `Option`, `Result`, record, tuple, and union types in addition to the previously supported `int`, `float`, `bool`, and `str` (#737)
- Tuple destructuring patterns in `case` statements and expressions (#834). Supports binding patterns `(a, b)`, literal patterns `(1, 2)`, mixed `(1, n)`, wildcard `(_, n)`, 1-tuples `(v,)`, guard clauses `(a, b) if a > b`, and nested patterns such as `(Some(v), _)`. A fully irrefutable tuple pattern (all elements are variables or `_`) is treated as exhaustive.
- `runtime_internal.arc_live_count() -> int` — test-only introspection function that returns the running balance of ARC header allocations minus frees. Enables delta-based leak assertions in Ry spec tests without relying on LSan (#859)
- `Map + Map` (merge, rhs-wins on key collision) and `Set + Set` (union) are now supported via `+` and `+=` operators, parallel to existing `List + List` concatenation (#866)
- `tests/spec/concurrency_stress.test.ry`: stress tests for `@parallel for` with Map/Set captures (CoW semantics), GC collect() during parallel execution, nested `@parallel for`, many `thread_spawn` workers sharing a str capture, and Lock high-contention (4 threads × 2000 iterations) (#872)
- `tests/test_runtime_arc_contention_stress.cpp`: C++ GoogleTest suite exercising concurrent atomic `retain`/`release` on a single ARC header (16 threads × 10,000 iterations); part of the required `build-tsan/ry_tests` gate (#872)
- `tests/test_runtime_lock_stress.cpp`: C++ GoogleTest suite for `__ry_lock_acquire`/`release` under high contention (8 threads × 10,000 iterations, sequential reacquire, and independent-lock baselines) (#872)
- Integrated Clang Static Analyzer (`scan-build`) into CI `scan-build` job (#898)
- `Set<T>` `==` and `!=` now support complex element types: records, tuples, and nested collections (`Set<Point>`, `Set<List<int>>`, `Set<Map<str, int>>`, `Set<Set<int>>`) (#958)
- `Map<K, V>` `==` and `!=` now support complex key types: records, tuples, and nested collections (`Map<Point, int>`, `Map<(int, int), str>`, `Map<List<int>, str>`, etc.). Non-primitive keys use an O(n·m) structural linear-scan lookup; primitive keys continue using the existing hash-based path unchanged (#961)
- Positional record destructuring patterns in `case` arms: `case Point(a, b):` binds record fields by declaration order (#989)
- Nested patterns are now supported inside ADT enum constructor pattern arms (#990).
  Each binding position may be a variable, a literal, a wildcard, or a tuple pattern.
  A single tuple pattern whose arity matches the variant's field count is unwrapped
  and matched field-by-field, so `Event::Click((0, 0))`, `Event::Click((x, y))`,
  `Event::Click((_, y))`, and `Wrapper::Val(42)` all work as expected. Plain variable
  bindings (`Shape::Circle(r)`) continue to work unchanged.

### Changed

- `str` now stores an explicit byte length (`StringHeader` layout: `strong_count`, `weak_count`, `byte_len` prefix before the character data). The operations `byte_len`, `length`, `==`, `!=`, `<`, `>`, `+`, `*`, and Map/Set key lookup are fully NUL-safe; strings containing embedded NUL bytes (`\0`) are no longer silently truncated. (#1022)
- Indexing a `str` value with `[]` now emits a clear diagnostic pointing to `char_at(s, i)`, instead of the misleading "cannot determine list element type" message (#1026)
- Writing an octal literal (`0o...`) now produces a targeted compile error
  explaining that octal literals are not supported and suggesting `0x...`
  (hex) or `0b...` (binary) instead. Previously it produced the generic
  `invalid character after numeric literal` diagnostic. (#1027)
- `checked_add`, `checked_sub`, `checked_mul`, `saturating_add`, `saturating_sub`, `saturating_mul`, `wrapping_add`, `wrapping_sub`, `wrapping_mul` now accept the high-level `int` type in addition to low-level integer types (`i8`..`i64`, `u8`..`u64`) (#1028)
- `bool` operands are now rejected at compile time for arithmetic operators
  (`+`, `-`, `*`, `/`, `//`, `%`, `**`, unary `-`) and bitwise operators
  (`&`, `|`, `^`, `<<`, `>>`, unary `~`). Previously, `bool` was silently promoted
  to `int`. Use `bool as int` for explicit conversion. This also aligns the bitwise
  implementation with the documentation (#1030).
- `str` values are now fully ARC-managed (#1046). Dynamic strings created by `+` concatenation, `repeat`, f-string interpolation, and runtime functions are automatically freed when their last reference goes out of scope, eliminating string leaks. `List<str>`, `Map<K, str>`, and `Set<str>` also release string payloads when the collection is freed.
- `path.join`, `path.basename`, `path.dirname`, `path.extension` now return `Result<str, Error>` instead of `str`; callers receive a typed error if any argument contains an embedded NUL byte (#1054)
- `filesystem.is_file`, `filesystem.is_dir`, `filesystem.is_symlink` now return `Result<bool, Error>` instead of `bool`; callers receive a typed error if the path contains an embedded NUL byte (#1054)
- `http.listen` handler type is now `function(HttpRequest) -> Result<HttpResponse, Error>`; the listen loop synthesises a 500 response when the handler returns `Err` (#1054)
- `http.header(req, key)`, `http.query(req, key)`, `http.cookie(req, name)`, `http.form_field(req, name)`, `http.form_file(req, name)` now return `Result<Option<…>, Error>` instead of `Option<…>`; callers receive a typed error if the key/name contains an embedded NUL byte (#1054)
- `http.response(status, headers, body)` now returns `Result<HttpResponse, Error>` instead of `HttpResponse`; callers receive a typed error if any header key or value contains an embedded NUL byte (#1054)
- `http.header(resp, key)` (client response accessor) now returns `Result<Option<str>, Error>` instead of `Option<str>` (#1054)
- Unified ARC header offset dispatch for str: added `CapturedArcKind::Str` variant and `emitArcHeaderForAlloca` helper to prevent closure capture retain/release from using the wrong header offset (−16 instead of −24) for str values (#1105).
- `@it` and `@describe` functions with a return type annotation now produce a compile error instead of silently ignoring the annotation (#1122)
- `List<T>` and `Map<K, V>` `==` / `!=` now support complex element/value types: records, tuples, and nested collections (`List<List<T>>`, `List<Map<K,V>>`, `Map<str, List<T>>`, `Map<str, Map<K,V>>`, etc.) (#736).
- Internal codegen now uses `record` terminology throughout (`RecordInfo`, `record_types_`, `emitRecordConstructor`, `emitRecordComparison`, `findRecordTypeName`, `createRecordVisitFunction`, `recordToString`, `recordHasArcFields`, `arc_field_record_vars_`) to align with the `record` keyword used at the language surface (#816)
- User-visible error messages updated from "struct type" to "record type" (e.g., "unknown record type", "field access on non-record type") (#816)
- `ConcurrencySpecSuite` (in-process `@parallel for` / async spec suite) is now enabled under ASan builds; the `DISABLED_` guard added in commit `fb010ea` was removed after #630's atomic-ARC fix resolved the root cause (non-atomic ARC ops racing with ASan shadow-memory interceptors) (#872)
- Expanded clang-tidy `HeaderFilterRegex` to include `src/` implementation headers (#950)
- `union == / !=` now supports collection (`List`, `Map`, `Set`), record, ADT enum, and nested union variants in addition to primitives. Function-typed variants remain unsupported. (#960)

### Removed

- Removed `docs/tutorial/` directory and related references from `docs/README.md`, `AGENTS.md`, and top-level `README.md` (#968)

### Fixed

- `Err([...])` and similar Err-constructor expressions can now be coerced to a
  `Result<Ok, Collection>` type annotation at variable declaration and reassignment
  sites (e.g., `a: Result<int, List<int>> = Err([1, 2, 3])`).  Previously this
  emitted a type error because the inferred struct layout differed from the
  annotation layout (#1001).
- Pattern-matching an `Err(binding)` arm now correctly propagates collection
  element-type metadata to the bound variable, enabling index access and
  collection operations on the Err payload without a "cannot determine list
  element type" error.
- `T?` shorthand return type now propagates collection metadata identically to
  `Option<T>` — `xs.length()`, index access, and equality now work correctly for
  functions declared as `-> List<T>?` / `-> Map<K,V>?` / `-> Set<T>?` (#1003)
- `to_bytes`, `read_bytes`, `tcp_receive`, `tls_receive`, HTTP `body_bytes` が返す `List<u8>` を変数に代入すると macOS で `malloc: *** error for object ...: pointer being freed was not allocated` がクラッシュしていた問題を修正 (#1007)
- `__ry_split_chars` (used by `split(str, "")`) now allocates its returned `ListHeader`
  with `arc_alloc` so that ARC retain/release in `emitVarDecl` reads a valid counter
  prefix. Previously the `checked_malloc` allocation placed malloc metadata at
  `header_ptr - 16`, which could be corrupted by retain and crash on scope-exit
  release with `pointer being freed was not allocated` on non-ASan macOS builds.
  Same bug class as #1007. (#1010)
- HTTP リクエストの `query_all`, `cookies_all`, `form_fields`, `form_file` が返す `Map<str, str>` を変数に代入すると macOS で `malloc: *** error for object ...: pointer being freed was not allocated` がクラッシュしていた問題を修正 (#1011)
- Fix refcount imbalance when pattern-matching `Some(...)` on a value declared with the `T?` shorthand (e.g., `str?`, `List<int>?`). `extractGenericTypeArg` now recognises the `T?` suffix form as equivalent to `Option<T>`, ensuring the typed ARC retain path (Path 2a) is selected instead of the heuristic fallback (#1015).
- Fix use-after-free when pattern-binding `str` or bare function pointer
  fields of tuples / records / enum variants (#1016)
- `reduce` with a lambda that omits parameter type annotations now
  returns the correct result. Previously, on `List<int>` (and other
  primitive lists) the accumulator seed was stored as a narrow value
  into a 16-byte `any` slot, leaving the payload uninitialized and
  producing garbage values like `14.0` instead of `15` (#1020).
- Fixed use-after-free when mutating a list, set, or map during `for` iteration.
  The loop now snapshots the iterable at entry via an ARC retain; mutations through
  the source alias inside the loop body trigger copy-on-write and do not affect the
  iteration — appended elements are not visited, and removed elements are still
  visited (#1021).
- `bytes_to_str` now preserves embedded NUL bytes instead of rejecting them. (#1022)
- `weak str` upgrade no longer returns `None` instead of `Some` when the strong reference is alive; codegen now uses the correct `STRING_HEADER_SIZE` (24) offset to reach `strong_count` instead of the collection `ARC_HEADER_SIZE` (16). (#1022)
- `int / 0` now follows IEEE 754 and returns `inf` (or `-inf` for negative
  dividends; `nan` for `0 / 0`), consistent with `10.0 / 0` and `10 / 0.0`
  which already returned `inf`. The `/` operator is documented as always
  returning `float`, so integer operands are promoted before division and
  IEEE 754 semantics apply. This reverts the integer-specific runtime-error
  guard added in #754; `//` (floor division) and `%` (modulo) retain
  integer semantics and still raise a runtime error on a zero divisor for
  integer operands (#1023).
- Lambda return-type inference now correctly unifies `Ok(T)` and `Err(Error)` branches in an if-expression body, so unannotated lambdas like `(x: int) => if x > 10 => Ok(x * 2) else Err(Error("too small"))` compile without a spurious "all branches must have the same type" error (#1024)
- `-9223372036854775808` (INT64_MIN) is now accepted as a bare integer
  literal. Previously it required the `i64` suffix or a workaround
  such as `-9223372036854775807 - 1`. A standalone
  `9223372036854775808` (without the unary minus) remains rejected,
  and `-9223372036854775809` is rejected at compile time (#1025).
- `Map<K, any>`, `List<any>`, and `Set<any>` now accept direct assignment of concrete values (`str`, `int`, `float`, `bool`). Previously, assignments like `m["name"] = "Alice"` or `xs.append!(42)` would fail with a type mismatch error even though the `any` type is documented to support implicit conversion. The fix applies the canonical widening pattern to six collection element-write sites: `Map` index-assign, `List` index-assign, `List.append!`, `List.appended`, `List.insert`, and `Set.add`. The symmetric unwrap direction (`any` → concrete) is also supported at all six sites, and `Set<any>` element comparison uses the `__ry_any_eq` runtime function. (#1029)
- `print` and `to_str` on `float` now use the shortest round-trip decimal representation (minimum digits to reconstruct the exact `double` value), matching Python 3, Rust, Go, and JavaScript. Imprecise arithmetic like `0.1 + 0.2` now prints as `"0.30000000000000004"` instead of `"0.3"`, accurately reflecting the stored value. Exact literals such as `3.14`, `3.0`, and `2.5` are unchanged (#1031)
- For-loop UAF guard now fires for `FieldAccessExpr` iterables
  (e.g. `for x in obj.items: append!(obj.items, ...)`), not only bare
  variable references (#1041).
- Lambda return-type inference now unifies `Some(T)` and `None()` branches in
  if-expr, matching the `Ok`/`Err` behavior added in #1024. Previously
  `(x: int) => if cond => Some(x) else None()` failed with `undefined function: None`,
  and even `(x: int) => Some(x)` alone failed with a return-type mismatch (#1043)
- `contains`, `starts_with`, `ends_with`, and `find` now honour embedded NUL bytes instead of truncating at the first `\0` (#1047).
- `replace` now honours embedded NUL bytes in the haystack, needle, and replacement instead of truncating at the first `\0` (#1048).
- `substring`, `char_at`, `reverse`, `split("", "")`, `for c in str:`, and `enumerate(str)` now honour embedded NUL bytes instead of truncating at the first `\0` (#1049).
- String operations `to_upper`, `to_lower`, `trim`, `trim_start`, `trim_end` now formally preserve embedded NUL bytes (#1050)
- `split` with non-empty delimiter now preserves embedded NUL bytes in the subject and delimiter; the inline `strstr`/`strlen` codegen path was replaced by `__ry_str_split` in `runtime_string.cpp` using `memmem` (#1051)
- `join` and `repeat` / `*` string operations formally NUL-safe (#1051)
- Regex operations `regex_match`, `regex_search`, `regex_replace`, `regex_split`, `regex_find_all` (and UFCS variants `is_match`, `search`, `replace`, `split`, `find_all`) now preserve embedded NUL bytes in subject, pattern, and replacement; the public ABI was extended to carry explicit byte lengths for all string arguments (#1052)
- `json.parse` now accepts `\u0000` in string values and object keys (previously rejected with an error) (#1053)
- `json.stringify` now emits `\u0000` for embedded NUL bytes instead of truncating the string (#1053)
- `json.to_str`, `json.get`, and `json.keys` now correctly handle strings and keys containing embedded NUL bytes (#1053)
- HTTP client body truncated at first embedded NUL byte: `Content-Length` was computed with `strlen(body)`; now uses `stringByteLen(body)` for binary-safe payloads (#1054)
- HTTP request URL silently truncated at embedded NUL: `http_get`, `http_post`, and `http_request` now reject URLs containing embedded NUL bytes with a typed `Err` (#1054)
- HTTP `http_request` method silently truncated at embedded NUL: now rejected with a typed `Err` (#1054)
- HTTP header build used `std::string::operator+=` on Ry handles, truncating values at the first NUL; replaced with byte-length-correct `append(data, byte_len)` (#1054)
- DNS hostname lookup (`net.bind`, `net.connect`, `net.tls_connect`) silently truncated hosts containing embedded NUL bytes; now rejected with a typed `Err` (#1054)
- `path.join`, `path.basename`, `path.dirname`, `path.extension`, `path.resolve` silently truncated paths at embedded NUL bytes; now rejected with a typed `Err` (#1054)
- `filesystem` functions silently truncated paths at embedded NUL bytes; now rejected with a typed `Err` (#1054)
- `bytes_to_str()` and `write_bytes()` now reject non-`u8` list arguments at compile time instead of silently producing garbage output. Plain integer list literals like `[97, 0, 98]` use 64-bit element layout incompatible with the byte-list runtime; passing them previously caused corrupted output. Use `[97u8, 0u8, 98u8]` (explicit `u8` literals) or `to_bytes("...")` instead (#1055).
- `weak <alias>` where the alias resolves to `str` now uses the correct `StringHeader`
  offset instead of the `ArcHeader` offset. Without this fix, weak upgrade of a str-alias
  weak ref could load the wrong `strong_count` and crash or return wrong results (#1060)
- `fold()` now accepts untyped lambdas (e.g. `fold(xs, 0, (a, b) => a + b)`), matching the fix already applied to `reduce()` in #1038 (#1061)
- Lambda with explicit return type annotation (e.g. `(a, b) -> int => a + b`)
  now correctly coerces `any`-typed body expressions to the declared return
  type. Previously this failed at compile time when lambda parameters were
  untyped (which default to `any`), blocking the common
  `reduce(xs, (a, b) -> int => a + b)` pattern. Fix applies to both
  expression-body and block-body lambdas, and to `return` statements in
  regular functions. (#1062)
- The `in` and `not in` operators now accept concrete values on `Set<any>`, `List<any>`, and `Map<any, V>` containers, and support testing `any`-typed values against collections with concrete element types. Previously, expressions like `"x" in s` on a `Set<any>` failed with a compile-time type-mismatch error despite the write side (`add`, `append!`, index-assign) already accepting the same widening since #1029. The three check sites in `src/codegen_expr.cpp` (Set, Map, and List membership branches) now apply the canonical 3-branch any-widening pattern. The List branch additionally gained an `isAnyType` case in its inline comparison loop that invokes `__ry_any_eq` with scratch allocas hoisted outside the loop, mirroring `emitSetElementLookup` and `emitMapKeyLookup`. The symmetric unwrap direction (`any` value tested against a concrete container) is also supported. (#1065)
- `is_empty` on strings now honours embedded NUL bytes instead of returning `true` for strings that begin with `\0`. The check now reads `byte_len` from the StringHeader (via `emitStringByteLen`) instead of comparing only the first byte (#1069).
- Regex literal `\0` escape now produces a NUL byte in the pattern, matching string literal behavior (`/a\0b/` now correctly matches `"a\0b"`) (#1076)
- `bs: List<u8> = [97, 0, 98]` now compiles correctly; the `List<u8>` annotation propagates `u8` to each integer literal element so the list has 8-bit element stride and passes the `bytes_to_str` / `write_bytes` compile-time type gate (#1079)
- Reassignment to a `List<u8>` (or other `List<T>` with low-level integer element type) variable now propagates the element suffix so `bytes_to_str`, `write_bytes`, and TLS/TCP byte-list consumers accept the list, matching the declaration-time behavior from #1079 (#1085)
- Parallel test runner (`ry test -p`) now prints the failing file path and exit code for any non-zero worker, eliminating silent failure-count increments that were unattributable to a specific file. (#1088)
- Test runtime flushes stdout at every `it` boundary and after the summary, so output is preserved even when a worker exits abnormally. (#1088)
- Fixed an intermittent `~40%` failure rate in `ry test -p` on macOS caused by a crash in `~LLJIT()` during JIT teardown. Extended the existing Linux `(void)jit.release()` workaround to also apply on macOS. (#1088, #742)
- For-loops over captured collections (`VariableExpr` / `FieldAccessExpr` iterables) inside `thread_spawn` closures no longer crash the JIT optimizer (`LowerExpectIntrinsicPass`). The thread thunk now releases ARC-managed locals before its `ret void`, matching the parallel-for thunk pattern (#1090).
- `for x in xs[i]:` now snapshots the indexed collection via ARC retain, preventing
  use-after-free when the same slot is mutated (`append!`/`add`/`xs[i][k] = v`) inside
  the loop body. Extends the guard from #1021 (`VariableExpr`) and #1041 (`FieldAccessExpr`)
  to `IndexExpr` iterables. (#1091)
- `for a, b in xs[0]:` where `xs: List<List<(int, int)>>` now correctly types the second destructured variable `b` as `int` instead of reading raw bytes (#1094)
- `for x in outer[0][0]:` where `outer: List<List<List<int>>>` now correctly iterates all elements instead of running 0 times (#1095)
- `None()` call-form is now recognised as a None literal in let-decl, local
  variable reassignment, and module-global reassignment contexts, matching the
  behaviour of bareword `None` and `none`. Previously `x: Option<int> = None()`
  and `x = None()` (on an already-declared `Option<T>` variable) produced a
  type-mismatch compile error (#1099).
- `List<u8>` / `List<i8>` compound assignment (`bs += [99]`) no longer raises "list concatenation requires matching element types"; element suffix propagation now covers compound-op branches for both local variables and module-global write-through (#1102)
- Closure construction and destructor were corrupting `StringHeader.byte_len` when a str value was captured, by retaining/releasing at the wrong ARC header offset. Fixed by dispatching through `CapturedArcKind::Str` in `codegen_lambda.cpp` and `codegen_arc_cow.cpp` (#1105).
- Bare-expression str temporaries (e.g., `"foo".to_upper()` used as a statement) were leaked because `emitStmt(ExprStmt)` only checked `arc_owned_values_` and missed `arc_str_owned_values_` (#1105).
- Fixed memory leak when overwriting a slot in `List<List<str>>`, `Map<K, List<str>>`, or a record field of a nested collection type containing `str` elements. The overwritten inner collection's `str` handles are now released correctly (#1108).
- Result-returning lambda with unannotated parameter no longer loses its `Ok` payload when flowing into a typed `Result<T, E>` binding (#1111)
- Unannotated lambda body with 3+ branches constructing `Err(Error(...))` now compiles without "all branches must have the same type" error (#1111)
- Option branch-type merge in unannotated lambda if-expressions now prefers concrete types over `anyTy_` placeholders, matching the Result merge logic. Also propagates the `anyTy_` unwrap pattern from `Ok` to `Some` so concrete-vs-any branches produce matching `Option<T>` structs (#1115).
- `Err(x)` with an unannotated lambda parameter no longer causes a branch-type mismatch when the enclosing function's Result Err slot is a primitive type (`int`, `float`, `bool`, `str`) (#1116)
- `reverse!()` on a string now produces a clear diagnostic instead of a misleading
  "requires a list" internal error (#1124)
- Rejected embedded NUL bytes in path arguments of `io.read_text`, `io.write_text`,
  `io.append_text`, `io.delete_file`, `io.read_bytes`, and `io.write_bytes`; each
  now returns `Err(Error{ message: "<fn>: argument contains an embedded NUL byte" })`
  instead of silently truncating the C string and operating on an unintended file.
  `io.exists` returns `false` for such paths (no error channel available). Brings
  `io` to parity with the existing guards in `filesystem` and `path` (#1128).
- `base64.encode`, `base64.decode`, `base64.encode_url_safe`, `base64.decode_url_safe` no longer silently truncate input at embedded NUL bytes. `encode` / `encode_url_safe` now correctly process the full binary payload (binary-safe). `decode` / `decode_url_safe` now return `Err("invalid base64 character at position N")` for inputs containing NUL (since NUL is not a valid base64 character), instead of silently succeeding on the prefix before the NUL (#1129).
- `io.write_text` and `io.append_text` silently truncated content at the first
  embedded NUL byte because they used `fputs(content, f)`. They now use
  `fwrite(content, 1, stringByteLen(content), f)` for binary-transparent writes,
  matching the already-safe `io.write_bytes` path. `fclose` return code is still
  checked so buffered-write errors surface as `Err` (#1133).
- thread: align `thread_spawn` / `thread_join` `@native` declarations with their runtime behaviour (supports `int` / `float` / `bool` workers in addition to `Unit`) by using `any` as the declaration-level placeholder (#1135)
- `List<Set<T>>` and `List<Map<K,V>>` equality no longer silently falls back to pointer comparison, which produced incorrect results (#736).
- Clearer compile-time error for `Set<T>` equality with non-primitive element types, with reference to tracking issue (#736).
- ADT enum `==` / `!=` now compares the variant payload in addition to the tag.
  Previously two values with the same tag but different payload were incorrectly treated
  as equal (e.g. `Circle(1.0) == Circle(2.0)` returned `true`). (#959)
- Nested-collection equality (`Set<List<T>>`, `Set<Map<K,V>>`, `Set<Set<T>>`) now
  returns correct results regardless of insertion order (#963)
- `Set.contains(elem)`, `elem in set`, `set.add(elem)`, and `set.remove(elem)` now
  use structural equality when the element type is a nested collection, instead of
  incorrectly treating the element pointer as a C string (#963)
- `Option<List<T>>`, `Option<Map<K, V>>`, and `Option<Set<T>>` equality no
  longer returns a false-positive `true` when inner collections share a byte
  prefix; inner values are now compared element-wise. (#982)
- `Result<Collection, E>` and `Result<_, Collection>` equality now performs element-wise comparison of the inner collection instead of raw `strcmp` on collection header bytes (#985).
- ARC retain missing for fields extracted in pattern binding arms — `Some(xs)`, `Ok(xs)`, `Err(msg)`, record, enum-constructor, tuple, and variable patterns now correctly retain ARC-managed bindings, preventing use-after-free and refcount underflow under ASan (#997)
- `ListHeader` objects returned from runtime string-list builders (`makeStringList`, `makeMatchList`) are now allocated with `arc_alloc` so that Ry's ARC retain/release machinery can safely manage their lifetime (#997)
- `IOListHeader` objects returned from IO/network runtime functions (`receive`, `read_bytes`, `str_to_bytes`, TLS receive, HTTP body bytes) are now allocated with `arc_alloc`, fixing use-after-free when Ry's ARC retain/release accesses `header_ptr - 16` on pattern-bound byte-list values (#997)
- Fix use-after-free when a function returns `Result` or `Option` wrapping a collection (List, Map, Set) — covers direct parameters (`Ok(v)`) and record/tuple field access (`Ok(rec.field)`) — the inner value is now retained before scope cleanup releases local variables (#999)
- `emitStrGetDataPtr` now registers the recovered str handle in `arc_str_owned_values_` (STRING_HEADER_SIZE=24 offset) instead of `arc_owned_values_` (ARC_HEADER_SIZE=16 offset); using the wrong set caused incorrect header arithmetic on any subsequent retain/release of a str pointer recovered from a StringHeader (PR #1148 review)
- Map CoW clone now retains str keys independently of value retention; `elementTypeIsArcManaged` only checked `map_value_type_name`, so `Map<str, V>` CoW clones dropped key refcounts to zero after releasing the old header — use-after-free (PR #1148 review)
- `emitMapKeyLookup` now correctly routes StructType keys through the linear-scan path when `map_key_type_name` metadata is absent; previously an empty key name with an LLVM StructType fell through to `emitHashTableLookup` which has no hash function for structs (PR #1148 review)
- `Set<any>.remove(elem)` now applies the same 3-way any-widening (concrete → any wrap / any → concrete unwrap) as `Set<any>.add(elem)`, eliminating the compile-time type-mismatch error for concrete-typed arguments (PR #1148 review)
- `http.listen()` handler return-type validation now rejects pointer types whose type name is not `HttpResponse`; previously any opaque pointer type passed the check in the LLVM opaque-pointer model (PR #1148 review)
- `http.listen()` now registers `"net"` and `"http"` in `used_native_libraries_` so the JIT linker resolves `__ry_bind`, `__ry_listen`, and the HTTP runtime symbols; previously the function compiled but crashed at JIT link time (PR #1148 review)
- `http.listen()` now closes the TCP listener on `__ry_listen` failure before returning the error, preventing a file-descriptor leak (PR #1148 review)
- `str * n` with `n ≤ 0` now returns a heap-allocated empty StringHeader instead of a global constant; the PHI that merges the empty and repeat branches is registered in `arc_str_owned_values_`, so the global constant was previously released on scope exit — undefined behaviour (PR #1148 review)
- Record ARC reassignment now retains the incoming value for `InsertValueInst` chains (e.g. `r2 = { r.field, new_val }`), not only for `LoadInst` and `ExtractValueInst`; the missing retain caused use-after-free when an `InsertValueInst` aggregate was stored into an ARC-field record variable (PR #1148 review)
- `int` and `float` `to_str` / `value_to_string` no longer leak the allocated StringHeader; the buffer is now registered in `arc_str_owned_values_` so it is released on scope exit (PR #1148 review)
- `base64.decode_bytes` and `base64.decode_bytes_url_safe` now guard against a null `input` pointer before calling `stringByteLen`; a null input now returns an empty `List<u8>` instead of dereferencing at a negative offset (PR #1148 review)
- Fixed `is_empty([])` example in `docs/reference/collections.md` to use a type-annotated variable declaration (`empty: List<int> = []`); bare `[]` requires type inference context that is not always available (PR #1148 review)

## [0.0.11] - 2026-04-14

### Added

- `print()` now supports `end` and `sep` named parameters to control line ending and separator (#747)
- `Option.map()` combinator: transform the inner value of an `Option` with a function, returning `Some(f(x))` for `Some(x)` and `None` for `None` (#804)
- `regex.replace` and `regex_replace` now support capture group backreferences in the replacement string: `$1`–`$9` expand to the corresponding captured groups, `$0` expands to the entire match, `$$` produces a literal `$`, and `${N}` handles multi-digit group indices (#829)
- Trailing commas are now allowed in list, map, and set literals, function call arguments, function and lambda parameters, enum variant field lists, generic type parameters, generic type arguments, function type parameters, and enum constructor patterns (#832)
- Clang-Tidy static analysis with `bugprone-*`, `performance-*`, `cert-*` checks (#893)

### Changed

- `find_all` and `regex_find_all` now return `List<Match>` instead of `List<str>`. Each `Match` record has a `full: str` field (the matched text) and a `groups: List<str>` field (captured groups, in order). Patterns without capture groups return an empty `groups` list. (#830)
- CI now uses a mirrored LLVM 21.1.8 toolchain from GitHub Releases instead of fetching from apt.llvm.org on every run (#892)
- Integrated clang-tidy provisioning into `setup-llvm` action; CI no longer installs clang-tidy via a separate apt step (#934)
- Resolved all 85 existing clang-tidy warnings across `src/` and `include/ry/`; clang-tidy is now a hard CI gate with `WarningsAsErrors: '*'` (#935)

## [0.0.10] - 2026-04-12

### Changed

- CI: ccache now only saves on `main` and `v*` branch pushes, preventing redundant cache accumulation on PR runs (#926)

## [0.0.9] - 2026-04-12

### Changed

- Documented that `share/std/math/math.ry` intentionally uses bare `@native` (no argument) because math functions have no separate shared library (#907)
- `include/ry/codegen.hpp` uniform closure comment now documents all three struct fields: `{thunk_ptr, env_ptr, env_dtor_ptr}` (#907)
- `include/ry/ry_layout.hpp` `ARC_HEADER_SIZE` derived from `sizeof(int64_t) * 2` instead of hardcoded `16` (#907)
- `AGENTS.md` wip-label timing unified to post-merge rule; constant registration guidance now cites header path (#907)
- `.cursorrules` stdlib CMake step now distinguishes `ry_lib` (codegen) from `add_ry_native_lib` (runtime) targets (#907)
- `.claude/skills/git-commit/SKILL.md` `allowed-tools` now includes `git diff` and `git log` (#907)
- `.claude/skills/git-resolve-conflicts/SKILL.md` (+ `.codex` mirror) verifies `headRefName` before merging base branch (#907)
- `.claude/skills/git-branch-naming/SKILL.md` removes `style` branch type; branch creation is now conditional (#907)
- `.claude/skills/git-fix-pr-reviews/SKILL.md` generalizes nitpick marker detection and parameterizes reviewer handle (#907)
- `.codex/skills/git-search-issues/SKILL.md` adds deterministic precedence rule for ambiguous inputs (#907)
- `.codex/skills/git-triage-issue/SKILL.md` scopes autonomous action to Cases 1-2 only (#907)
- `.claude/skills/git-merge-pr/SKILL.md` (+ `.codex` mirror) uses dynamic default branch detection instead of hardcoded `main` (#907)
- `.codex/skills/git-switch-branch/SKILL.md` handles local branches without upstream (#907)
- `.github/workflows/release.yml` native-lib glob uses `nullglob` + array check for diagnosable errors (#907)

### Fixed

- `install.sh` now fails with a clear error when the release archive does not contain a standard library at `share/std` or `lib/std`, instead of silently installing a broken `ry` that crashes at runtime (PR #901 review)
- `arc_alloc` now guards `ARC_HEADER_SIZE + data_size` against integer overflow via `__builtin_add_overflow`, preventing an undersized heap allocation followed by out-of-bounds writes if `data_size` is near `SIZE_MAX` (PR #901 review)
- ADT example in control-flow reference used `Shape::Rect` instead of `Shape::Rectangle` (#907)
- Concurrency tutorial incorrectly implied `send`/`receive`/`close` are `net` module exports; clarified they are language builtins (#907)
- `to_str` signature in builtins-string reference now matches the supported-types table (`any` instead of a restricted union) (#907)
- `docs/zh/reference/operators.md` described `else =>` as required but examples used `_ =>`; unified to `_ =>` (#907)
- `docs/tutorial/11-testing.md` `test_should_handle_error` example had uninitialized `result` variable (#907)
- `docs/reference/functions.md` mutual recursion description now correctly states forward-declaration applies to nested functions too (#907)
- Typo "overloads case a call" corrected to "overloads match a call" in `docs/reference/functions.md` (#907)
- Traditional Chinese leftovers (`字串`/`巢状`/`缩排`) normalized to Simplified Chinese in `docs/zh/tutorial/11-testing.md` (#907)
- "match statements/expressions" terminology in `docs/zh/reference/control-flow.md` updated to `case` (#907)
- Code fences for `@each`/`@property` in testing reference now include `ry` language identifier (markdownlint MD040) (#907)
- Heading "Handling Results with match" corrected to "with case" in `docs/tutorial/08-error-handling.md` (#907)

## [0.0.8] - 2026-04-12

### Added

- Systematic combinatorial test coverage in `tests/spec/combinatorial/` (#628): 113 tests across 9 files covering type×operation matrix (equality, fn argument/return, collection element, match, nested types, syntax combinations, print/display, stdlib boundary inputs)
- `@it("description")` directive on named functions: test cases can now be defined as ordinary named functions with the `@it` directive (#634)
- `@describe("group")` directive on named functions: test groups can now be defined as ordinary named functions with the `@describe` directive (#635)
- `@each` and `@property` directives compose with `@it` on named functions for parameterized and property-based tests (#634)
- Shared setup in `@describe`: variables declared in a describe function body are automatically captured by inner `@it` functions (#635)
- Nested `@describe` output indentation: test output is now indented proportionally to nesting depth (#635)
- `NativeFnSignature` registry that captures full type information (parameter names/types, return type, package) from `@native` function declarations (#646)
- Documented the `__ry_<pkg>_<name>` native function naming convention (#646)
- `@native("libname")` directive syntax for specifying shared library module names (#647)
- Dynamic library loading for `@native("libname")` declarations — the JIT now loads shared libraries at startup (#649)
- Stdlib runtime packages are built as shared libraries (`.dylib`/`.so`) in addition to the existing static linking (#649)
- Nested named functions now obey lexical scoping: they are visible only within their enclosing function and do not collide with same-named functions in sibling scopes (#660)
- Nested named functions can now capture variables from enclosing scopes, behaving as closures just like lambdas (#661)
- `==` and `!=` operators now work for `List<T>`, `Set<T>`, `Map<K,V>`, `Result<T,E>`, and union types (#725)
  - List: element-wise comparison (supports `int`, `float`, `str`, `bool` elements)
  - Set: unordered equality — `{1,2,3} == {3,2,1}` is `true`
  - Map: key/value equality — maps with the same key-value pairs are equal regardless of insertion order
  - Result: compares `is_ok` flag and the inner `Ok` or `Err` value
  - Union (`A|B`): compares tag (variant kind) first, then the inner value for matching tags
- `ry` and `ry test` can resolve a bare `*.ry` filename (e.g. `ry main.ry`) when the file is not in the current directory: the project root is tried first, then each `[paths]` directory in key order; the first match wins (#741).
- `?` operator now accepts `Option<T>` operands in addition to `Result<T, E>`. When used on a `Some(v)` it evaluates to `v`; when used on a `None` the enclosing function returns `None` early. The enclosing function must declare an `Option` return type. `!!` is an alias with identical semantics. (#795)
- `??` operator now accepts `Result<T, E>` on the left-hand side in addition to `Option<T>`. For `Ok(v)` it evaluates to `v`; for `Err(_)` it evaluates to the right-hand default (the error value is discarded). (#796)
- `?` / `!!` can now be used directly at the top level of a script. When the operand is `Err(e)` or `None`, the error message is written to stderr and the process exits with status `1`. `__ry_main__`'s existing return-type contract is unchanged. (#745)
- `for c in s:` now iterates a string character by character, yielding each UTF-8 code point as a single-character `str`. `enumerate(s)` and `zip(s, t)` also accept `str` arguments with the same semantics. (#746, #827)
- `type_of(expr)` built-in function that returns a `Type` value representing the compile-time type identity of its argument. Supports `==` / `!=` for identity-based comparison and is printable via `print` / `to_str`. Covers primitives, low-level numeric types, collections (`List`, `Map`, `Set`), records, enums, `Option`, `Result`, functions/closures, `None`, and `Type` itself (reflective) (#793)
- `Type` primitive type representing the compile-time identity of a Ry type. Each distinct type definition receives a unique identity, so different records (or a record and an enum sharing a name) are always distinguishable by `==` (#793)
- `case` statement and expression unify `when` (conditional branching) and `match` (pattern matching) into a single construct (#799). Two forms are supported: `case:` for multi-branch conditionals without a subject (replaces `when:`) and `case <expr>:` for pattern matching with a subject (replaces `match`). Both forms support a block body (`:`) and a single-expression body (`=>`). Use `_` as the wildcard/default arm instead of `else`.
- `if` expression syntax for two-branch conditional values (#798). Supports both a single-expression form (`if cond => true_value else false_value`) and a block form (`if cond: body else: body`) with tail-expression semantics. For multi-branch expressions, use `case:` instead.
- Scientific notation float literals (`1e10`, `1.5e-3`, `2.5E+2`, `1_000e3`). Overflowing exponents (`1e400`) produce `+Inf` to match the runtime `to_float` converter (#819)
- `math.round(x, digits)`, `math.floor(x, digits)`, and `math.ceil(x, digits)`
  overloads for rounding a `float` to a given number of decimal places,
  returning a `float`. Negative `digits` rounds to powers of ten
  (`round(1234.5, -2) == 1200.0`). The two-argument forms reuse C99
  half-away-from-zero semantics so the result matches the one-argument
  `round()` applied to the scaled value — note this differs from Python's
  banker's rounding (`round(2.675, 2) == 2.68`, not `2.67`). `NaN` and `±Inf`
  pass through unchanged. (#842)
- `math.log(x, base)` overload for computing a logarithm with an arbitrary
  base, defined as `log(x) / log(base)`. Domain errors on either argument
  propagate as `NaN` or `-Inf`. (#842)
- `math.pow(x, y)` overload for `(int, int) -> int` using fast-exponentiation
  (O(log y)). A negative exponent raises a runtime error
  (`pow() integer exponent must be non-negative`). Overflow wraps silently,
  matching Ry's existing integer arithmetic model. (#842)

### Changed

- Captured variables in closures are now effectively final — reassignment inside the closure body produces a compile error (#213)
- `print()` now delegates to `to_str()` for all type formatting, ensuring consistent output between `print()`, `to_str()`, and f-string interpolation (#616)
- All C runtime memory allocations now use OOM-safe wrappers (`checked_malloc`, `checked_strdup`, etc.) that abort with a clear message instead of silently returning NULL (#631)
- Integer overflow checks added to array-size calculations in hash table rehash, UTF-8 reverse, and JSON parser (#631)
- CI now enforces a lint check that blocks raw `malloc`/`realloc`/`strdup` in new code (#631)
- `describe()` and `it()` lambda call syntax is deprecated; use `@describe("name")` and `@it("name")` directives on named functions instead (#635)
- Stdlib source files moved from `lib/std/` to `share/std/` following Unix FHS conventions (#645)
- Refactored math, io, json package dispatch to use table-driven native call dispatch (#650)
- Stdlib native dispatch migrated to table-driven architecture for net, http, and thread packages (#651)
- Stdlib `.ry` declarations updated from `@native` to `@native("libname")` for dynamic library resolution (#651)
- Stdlib runtime implementations separated from the static compiler library into shared libraries (#651)
- Directive invocation syntax is now generalized: all directives use a unified argument model supporting positional arguments, named arguments, and mixed forms (e.g. `@it("description")`, `@describe("group")`, `@property(count=100)`)
- Built-in directive signatures are now defined in a registry (`DirectiveSignature`) with allowed argument shapes and target kinds, enabling consistent validation and future user-defined directives (#663)
- Migrated all test descriptions (`it()` / `@it()`) to "should-style" wording for natural "it should ..." readability in test output and `--outline` mode (#664)
- Added test description style guideline to `docs/reference/testing.md` (#664)
- Stdlib package dispatch now uses self-registering pattern instead of X-macros; adding a new stdlib package with custom codegen no longer requires modifying core compiler headers (#674)
- Resource type tracking is now dynamic via `ResourceKindRegistry` instead of a hardcoded enum; new opaque resource types can be added without modifying `codegen.hpp` (#674)
- Error messages for `?` and `??` operator misuse now mention both `Option` and `Result` in the offending context.
- String elements inside collections (`List`, `Set`, `Map`, `Array`, `Tuple`, record) are now wrapped in double quotes when displayed via `print()` or `to_str()`, following Rust's debug display convention. Empty strings are now visible: `[""]` instead of `[]` (#756)
- `to_float(str)` now returns `Result<float, Error>` instead of `float`, matching the shape of `to_int(str)`. Invalid input previously returned `0.0` silently; it now returns `Err(Error(...))`. Empty strings, non-numeric content, and out-of-range values are reported as errors. **Breaking change**: existing code must unwrap the `Result` (e.g., via `case` or `?`). (#806)
- Assigning to a top-level mutable `let` from inside a function now writes through to the top-level binding instead of silently shadowing it with a new local. Code that relied on the old shadowing behavior must rename the inner variable explicitly (#817)
- `remove_at(values: List<int>, index: int)` in `share/std/list.ry` is now declared to return `int` instead of `Unit`, matching both the runtime implementation and the existing `collections.test.ry` expectations (#889)

### Removed

- Legacy `native_fn_arg_counts_` dispatch guard replaced by `native_fn_sigs_` (#651)
- Removed dedicated codegen dispatch files for base64, filesystem, and gc packages (now handled by generic native dispatch) (#651)
- **Breaking**: The `when` and `match` keywords have been removed (#800). Legacy code using these keywords must migrate to `case`. Migration table:
  | Before | After |
  |---|---|
  | `when:` | `case:` |
  | `match value:` with `case pattern:` arms | `case value:` with bare `pattern:` arms |
  | `else:` / `else =>` inside `when` arms | `_:` / `_ =>` |

### Fixed

- `to_str()` on ADT enums with associated data now correctly formats all field types (previously only supported int, float, str, bool) (#616)
- `@parallel for` no longer corrupts captured `List` / `Map` / `Set` / `str`
  values. Worker-local ARC retain/release on captured collections now uses
  atomic operations, captured allocas are re-marked as ARC-managed inside the
  thunk, and every ARC-managed capture is retained at worker entry so the
  copy-on-write `strong_count > 1` invariant holds — preventing workers from
  mutating the shared buffer in place (which previously caused heap corruption
  under contention). (#630)
- `emitCowCheck` now uses an Acquire atomic load for `strong_count` in an
  atomic context, pairing with the `atomicrmw` retain/release and closing a
  TOCTOU race window that TSan flagged when multiple workers CoW-copied the
  same captured collection. (#630)
- `runtime_gc.cpp::collect_locked()` now reads and writes `strong_count` via
  `__atomic_load_n(ACQUIRE)` / `__atomic_store_n(RELEASE)` so garbage
  collection no longer races with concurrent ARC retain/release performed by
  `@parallel for` workers. (#630)
- `ExpectStmt` was not scanned during free-variable analysis, preventing closure capture of variables referenced in `expect(x).to_eq(...)` assertions inside nested `@it` functions (#635)
- Installed `ry` binary no longer crashes with `dyld: Library not loaded` when using native packages (#659)
- Native shared libraries are now included in release and nightly distribution tarballs (#659)
- `self-update` now installs native shared libraries alongside the binary and stdlib (#659)
- Broadened SSRF private address filter to block carrier-grade NAT (`100.64.0.0/10`), benchmarking (`198.18.0.0/15`), multicast (`224.0.0.0/4`), reserved (`240.0.0.0/4`), IPv6 unspecified (`::`), and IPv6 multicast (`ff00::/8`) (#667)
- Added error handling for `fcntl` failure when restoring blocking mode after non-blocking connect (#667)
- Passing a capturing closure as a `function(...)` argument no longer crashes (#688)
- Directive arguments now support compound expressions such as function calls and binary operators (`@each(make_inputs())`, `@foo(x + 1)`) (#694)
- Unknown or invalid directive arguments on `record`, record fields, variable assignments, and `for` loops now produce a compile-time error, consistent with how function directives are validated (#696)
- Option equality (`==` / `!=`) now correctly compares inner values when both operands are `Some`, instead of comparing only the `has_value` flag (#726)
- Element type metadata is now preserved when accessing elements of `List<Map<K,V>>`, `List<Set<T>>`, and `List<closure>` by index or in a `for` loop (#727)
  - `xs[0]["key"]` on `List<Map<str, int>>` now works correctly
  - `for m in xs: m["key"]` on `List<Map<str, int>>` now works correctly
  - `xs[0]` on `List<Set<int>>` supports the `in` operator
  - Closures stored in a list (`fns[0](arg)`) are now callable after retrieval
- `print()`, `to_str()`, and f-string interpolation now work with closure values — they produce `"<closure>"` instead of a compile-time error (#728)
- Parser no longer crashes on out-of-range integer literals such as `9223372036854775808` (INT64_MAX + 1); a clear compile error is reported instead (#729)
- Missing explicit paths to `*.ry` files (e.g. `ry src/missing.ry`) now report **no such file** instead of unknown command (#741).
- `package.toml` `[paths]` entries (other than `src`) round-trip through `serialize`/`load` (#741).
- Fixed SEGFAULT when calling a two-level nested function return with type annotation (#752)
- f-string interpolation inside closures now correctly captures outer variables (#753)
- Integer division by zero (`1 / 0`) now raises a runtime error instead of returning `inf` (#754)
- Lambda expressions returning pointer types (f-string, record `str` field, string concatenation, cast to `float`) no longer cause IR verify errors (#755)
- Return type inference now correctly handles local variables instead of falling back to `int` (#770)
- "return type mismatch" errors now show expected and actual types
- `Any`-typed string values inside collections are now displayed with double quotes, consistent with statically-typed strings (#771)
- Double quotes and backslashes inside strings are now escaped when displayed in collections (#772)
- Sprint buffer depth overflow now aborts with a clear error message instead of silently corrupting output (#773)
- Closure capture analysis now handles `CastExpr`, `WhenCondExpr`, `MatchExpr`, `RangeExpr`, `ErrorPropagateExpr`, `AwaitExpr`, `WeakExpr`, and `SetExpr`, preventing "undefined variable" errors when these expression types reference captured variables (#776)
- Match/when pattern bindings are now correctly excluded from closure capture analysis, preventing incorrect capture of outer variables with the same name (#779)
- Low-level integer types (`i32`, `u8`, etc.) now raise a runtime error on division/modulo by zero instead of causing undefined behavior (#783)
- Expression-bodied lambdas returning collection literals (List, Map, Set) now produce correct values (#788)
- Expression-bodied lambdas now correctly retain ARC references and clean up scope before returning, preventing potential use-after-free when returning captured ARC-managed values (#789)
- Propagate collection return type metadata for block-bodied lambdas with inferred return types, so `result.length()` / indexing work on the value returned by `f = (x: int):\n  return [x, x * 2]` style lambdas (#790).
- `1num = 1` now correctly produces a syntax error instead of silently succeeding (#794)
- `replace(s, "", repl)` no longer hangs with an infinite loop; an empty pattern now returns a fresh copy of the input unchanged (#802)
- `NaN != NaN` now returns `true` as required by IEEE 754; float `!=` comparisons use `fcmp une` (unordered not-equal) instead of `fcmp one` (#803)
- `is_empty()` now accepts `str` arguments in addition to lists, maps, and sets (#831)
- `Result<JsonValue, Error>` returned by `json.get` / `json.at` no longer
  sneaks past JSON type checks via metadata alone. `isJsonValue()` now
  also requires the underlying LLVM value to be a pointer, so passing a
  `Result` to `kind` / `stringify` / `get` / `at` produces the existing
  "requires a JsonValue argument" diagnostic instead of an LLVM IR verify
  error. `to_str(result)` and `print(result)` still work and format as
  `Ok(...)` / `Err(...)` via the generic `valueToString` path (#805).
- Using `List` / `str` / `Map` / `Set` (or any other ptr-backed value) as
  a boolean condition in `if` / `while` / `case` or under the unary `not`
  operator now produces a clear compile-time error suggesting
  `length(x) > 0` or `not is_empty(x)`, replacing the previous
  `icmp ne ptr, i0 0` IR verify failure (#818).
- `exit(0)` followed by more statements no longer triggers
  `Terminator found in the middle of a basic block`. `emitExit()` now
  switches to a fresh dead basic block so trailing IR lands on a valid
  (unreachable) block and LLVM DCE removes it during optimization (#821).
- `u64` maximum value (`18446744073709551615`) now parses successfully when written with a `u64` suffix or under a `u64` / unsigned type annotation. Hex and binary forms (`0xFFFFFFFFFFFFFFFFu64`, `0b11...1u64`) are accepted too; range checking for `int` / `i64` / `u8`-`u32` happens in codegen against the target type (#807)
- `print()` / `to_str()` on `Map<K, List|Map|Set<...>>` now shows the actual nested container contents instead of empty strings for the values (#811)
- `print()` / `to_str()` on union types with `List`, `Map`, or `Set` variants now works instead of failing at compile time with "cannot convert ... variant of union to string" (#836)
- Whole-number `float` values now print with a trailing `.0` (e.g. `3.0`, `0.0`) instead of being indistinguishable from `int`, matching Python behavior (#808)
- `print()` / `to_str()` on a `Map` whose value type is a function now outputs `<closure>` instead of garbage bytes (#810)
- `wrapInUnion` now disambiguates same-LLVM-type variants (e.g. `List<int> | Map<str, int>`) by the value's collection metadata instead of always picking the first pointer-typed variant, fixing runtime miscategorization for collection/function unions
- Chained assignment targets are now accepted by the parser and codegen,
  including `list[i].field = v`, `record.a.b = v`, `list[i][j] = v`, and
  compound forms such as `list[i] += v` and `record.field[i] *= v` (#812).
  Previously these raised "expected '=' after index expression" or
  "expected '=' after field name". Compound assignment to a missing map key
  (`m["absent"] += 1`) now produces a clear runtime error instead of
  silently inserting a default value.
- Compiler now rejects defining a `record` and an `enum` with the same name in the same compilation unit. This also covers generic enum templates: `record Foo` and `enum Foo<T>` can no longer coexist, and duplicate generic enum declarations are rejected. Previously both declarations were accepted, leading to inconsistent type lookup. (#815)
- Top-level `let` bindings and `@const` declarations are now visible from any top-level function defined after them in the same source file. This includes reads and field access for all types — primitives, strings, lists, maps, sets, records, enums, and option/result values. Previously any such reference produced `undefined variable` at codegen (#817)
- Enum values returned from user functions now print as variant names
  (or `Variant(payload)` for ADT enums) instead of raw integers. Simple
  enums, ADT enums, and already-instantiated generic enums are all
  handled. Enum-typed elements stored in `List<Color>` literals also
  propagate correctly. (#820)
- `for i, x in enumerate(...)`, `for a, b in zip(...)`, and
  `for k, v in Map<K, V>` now preserve collection-element metadata on
  destructured variables, so `print` / `sum` / `length` work correctly
  when the elements are themselves `List` / `Map` / `Set` / enum. (#813)
- Generic function type inference now succeeds when the type parameter
  appears inside a container type in the declared parameter. `List<T>`,
  `Map<K, V>`, `Set<T>`, tuples `(T, T)`, and function types
  `function(T) -> T` now infer their type arguments from the call site,
  including nested combinations and cross-parameter unification. Previously
  calls such as `first_of([1, 2, 3])` for
  `function first_of<T>(xs: List<T>)` failed with
  "could not infer type parameter 'T'" even though the shape was
  unambiguous. The existing `name[T](args)` explicit syntax continues to
  work for cases where inference cannot determine the type (e.g., empty
  containers) (#823).
- `thread_join(t)` now returns the worker's value wrapped in `Ok(v)`
  instead of always `Ok(0)`. Workers using an expression-bodied lambda
  may return `int`, `float`, `bool`, or `Unit`. Joining an
  already-joined thread returns `Err("thread already joined")`. ARC
  types (`str`, `List`, `Map`, `Set`, records) and sum types
  (`Option`, `Result`, enums), block-bodied lambdas with a non-`Unit`
  return value, and panic-to-`Err` propagation remain unsupported and
  are tracked as follow-up issues. (#828)
- Type aliases targeting union types (e.g., `type Simple = int | str | bool`) now work correctly in variable annotations, function parameters, and function return types. Previously the compiler reported `annotation 'Simple' does not match expression type` because the union check examined the unresolved alias name instead of its target (#833)
- Nested type aliases over union types are now fully flattened. Previously, given `type A = int | str; type B = A | bool`, declaring `x: B = 42` failed with *"type is not in union"* because the alias `A` inside the union was not expanded. `B` is now equivalent to `int | str | bool`, and overlapping members are deduplicated — so `type C = A | int` collapses to `int | str`, and `type D = B | A` (where `B` already transitively includes `A`) flattens to `bool | int | str` (#835)
- Compiler now rejects a `type` alias whose name collides with an existing `record`, `enum`, generic `enum`, or previously-defined `type` alias, in either declaration order. This extends the cross-category duplicate check added in #815 to type aliases (including named unions such as `type Foo = int | str`). Duplicate error messages also now point at the offending declaration instead of a stale location. (#850)
- Chained writes through nested collections (`a[i][j] = v`, `r.items[i] = v`, `m[k1][k2] = v`) no longer leak through aliases. Path copy-on-write walks the LHS from root to leaf and clones every level whose reference count is greater than one before the mutation (#854)
- Record-to-record assignment (`r2 = r1`) now retains ARC-managed fields (`List<T>`, `Map<K, V>`, `Set<T>`) so both aliases share ownership of the inner containers. A subsequent mutation through one alias is isolated from the other by path copy-on-write (#854)
- `list[i] = v`, `m[k] = v`, and their compound forms now release the
  previously-held value before storing the new one when the element type
  is itself an ARC-managed collection (`List<List<T>>`, `List<Map<K,V>>`,
  `Map<K, List<V>>`, `List<Set<T>>`, and nested combinations). Previously
  every overwrite leaked the prior inner collection's heap allocation.
  The fix is safe under self-assignment (`xs[i] = xs[i]`) and cross-slot
  copy (`xs[i] = ys[j]`) by retaining the new value before releasing the
  old one. (#855)
- `rec.arcField = newList` now releases the previously-stored ARC-managed
  collection (`List`/`Map`/`Set`) before the overwrite, matching the
  element-slot fix from #855. Applies to plain and compound assignment on
  `VariableExpr`, `FieldAccessExpr` (chained `outer.inner.items = ...`),
  and `IndexExpr` (`list[i].arcField = ...`) left-hand sides. Sibling
  `fieldTypeIsArcManaged` predicate added so record field types are
  classified from their declared AST type rather than container metadata.
  (#857)
- `xs[i] += v` and `m[k] += v` now dispatch correctly when the element
  type is itself an ARC-managed collection (`List<List<T>>`,
  `Map<K, List<V>>`, and nested combinations reached via chained LHS such
  as `rec.items[i] += v`). Previously the loaded slot value lost its
  type metadata, so `emitArithmeticOp`'s list-concat dispatch fell
  through to the string path and produced a misleading
  `operator '+' not supported between str and non-str types` error.
  The fix propagates the container's element type name onto the loaded
  SSA value via `propagateTypeMeta` — the same pattern the formatter
  already uses for nested element loads. As a secondary fix, the
  empty-declaration path (`xs: List<List<int>> = []`) now records
  `list_elem_type_name` symmetric to the existing `List<Map>` /
  `List<Set>` branches so compound ops work on append-grown containers
  as well. (#858)
- `rec.arcField += v` now dispatches correctly when the field type is
  itself an ARC-managed collection (`List<T>`, `List<List<T>>`, etc.).
  This covers plain record field assignment (`b.items += [3]`), nested
  record field access (`outer.inner.items += [3]`), and chained LHS
  through a list of records (`lst[0].items += [3]`). Previously the
  field extracted from the struct lost its type metadata, so
  `emitArithmeticOp`'s list-concat dispatch fell through to the string
  path and produced a misleading `operator '+' not supported between
  str and non-str types` error. The fix propagates the field's declared
  type name onto the extracted SSA value via `propagateTypeMeta` at all
  three `FieldAssignStmt` compound branches — sibling fix to #858,
  which addressed the same class of metadata-loss bug on the
  `IndexAssignStmt` compound path. (#862)
- `+` applied to `Map` or `Set` operands now produces a clear error that names the actual collection type instead of the misleading `"operator '+' not supported between str and non-str types"` message. Mixed cases such as `List<int> + Map<str, int>` also name both operand types. (#863)
- `rwlock_unlock` now dispatches between shared and exclusive release via
  a `thread_local` counter per RWLock, eliminating the two-step window in
  `rwlock_read_lock` where `std::shared_mutex::lock_shared()` was held
  but the tracking map had not yet been updated. Under the previous
  implementation an unlock that observed the transient state would have
  fallen through to exclusive `unlock()`, corrupting `std::shared_mutex`
  state. (#871, follow-up to #630 P1)
- `ThreadHandle::has_error` is now a `std::atomic<bool>`; the worker
  thread's catch blocks store it with `memory_order_release` after
  writing `error_msg`, and `thread_join` loads it with
  `memory_order_acquire`. This makes the error-field publish/subscribe
  contract explicit, TSan-friendly, and robust for any future pre-join
  error polling path. (#871, follow-up to #630 P1)
- Lambdas (expression-body and block-body) that return one of their own
  collection-typed parameters now correctly propagate the parameter's
  declared shape so that `result.length()` and indexing work on the
  returned value (#886).
- Corrected `` `match value:` `` references to `` `case value:` `` in the pattern matching tutorial — the actual keyword is `case` (#889)
- Rewrote the networking example in the concurrency tutorial so the server/client snippets match runnable `net` test code (#889)
- Replaced outdated "struct" phrasing in `README.md` and `docs/README.md` with "record" to match the Ry keyword (#889)
- Updated the install one-liner in `README.md` to the current release version (#889)
- Added the `@describe` / `@it` directive-based test style to the testing tutorial and to the directives reference, so the new preferred syntax is actually documented (#889)
- Expanded the `README.md` feature list to mention pattern matching, the built-in testing framework, union types, GC (`std.gc`), and the `?` error propagation operator (#889)
- Expanded the `README.md` directives line beyond `@deprecated` to include the other common directives (#889)
- Added an explicit "In-Place Mutating Variants" section to the collections reference covering `append!`, `sort!`, `reverse!`, and the non-mutating `appended` counterpart (#889)
- Corrected stdlib `@native` declaration return types that had silently drifted from their codegen dispatcher implementations (#890):
  - `items(map: Map<str, int>)` now declared as `-> List<(str, int)>` (was `-> List<int>`)
  - `enumerate(values: List<int>)` now declared as `-> List<(int, int)>` (was `-> List<int>`)
  - `zip(values: List<int>, other_values: List<int>)` now declared as `-> List<(int, int)>` (was `-> List<int>`)
  The dispatchers (`emitCollOp_items`, `emitBuiltinQuery` for `enumerate`/`zip`) always returned lists of tuples; only the declarations were wrong. No behavior change — this corrects the stdlib documentation to match reality.

## [0.0.7] - 2026-04-03

### Fixed

- Fix Linux (GCC 11) build failure caused by incomplete type in self-referential `FnTypeInfo` struct (#623)

## [0.0.6] - 2026-04-03

### Added

- Empty list literal `[]` is now supported with type annotation (e.g., `xs: List<int> = []`) (#545)
- List concatenation with `+` and `+=` operators (e.g., `[1, 2] + [3, 4]` → `[1, 2, 3, 4]`) (#546)
- Tuple `==` and `!=` comparison now works via element-wise comparison (e.g., `(1, 2) == (1, 2)` → `true`) (#542)
- Single-element tuple type annotation `(int,)` is now supported in variable declarations and function signatures (#561)
- `split(s, "")` now splits a string into individual characters with full UTF-8 support (e.g., `split("hello", "")` → `["h", "e", "l", "l", "o"]`) (#549)
- Match expression syntax with `=>` for single-expression arms, enabling `res = match x: case Some(v) => v case None => 0` — all pattern types (literal, variable, enum, Option, Result, OR, guard) are supported (#499)
- The `as` cast operator now accepts the full type syntax including generic types (e.g., `x as Option<int>`, `x as Map<str, int>`) (#490)
- Regex literal syntax (`/pattern/`) that produces a `Regex` type, enabling type-based overload resolution and UFCS-compatible function calls (e.g., `"hello".is_match(/[a-z]+/)`, `"a1b2".split(/[0-9]/)`) (#458)
- New text-first regex functions: `is_match`, `search`, `replace`, `split`, `find_all` — overloaded to accept `Regex` type patterns alongside existing string functions
- `print()` now accepts multiple arguments with space-separated output (e.g., `print(1, "hello", true)` → `1 hello true`), and calling `print()` with no arguments now prints only a newline
- `body_bytes()` function for `HttpRequest` and `HttpClientResponse` that returns `List<u8>`, enabling binary-safe HTTP body access without NUL-byte truncation (#284)
- Structured `--trace` / `--trace-out=PATH` CLI mode for machine-readable internal execution tracing as JSON Lines, covering parse/import/codegen/jit/runtime milestones plus function and branch events
- Restructured tutorials from 11 to 12 files: dismantled overcrowded `08-advanced.md` (14 topics) into focused chapters, added new `08-error-handling`, `10-concurrency`, and `12-building-a-project` tutorials, expanded `05-functions` with closures/default args/UFCS, `06-records` with ADT/operator overloading, `07-collections` with lazy iterators, and `02-variables-and-types` with f-strings/type casting. Each tutorial now includes "Why" explanations, exercises, and common mistakes (#444)
- Bare `ry` command runs the entry point file specified in `package.toml`, with `ry -- arg1 arg2` to pass arguments (#443)
- `--outline` option for `ry test`: prints the `describe`/`it` structure of test files without executing test bodies, useful for reviewing test organization at a glance (#442)
- Cycle collector for ARC: CPython-style trial deletion algorithm detects and reclaims circular reference chains that ARC alone cannot free. Includes `gc` stdlib package with `collect()`, `enable()`, `disable()`, `set_threshold()` API. Static analysis identifies potentially cyclic types at compile time — non-cyclic types have zero GC overhead (#417)
- ARC for closures: closures with captured variables are now ARC-managed — automatically freed when no longer referenced, with proper retain/release of captured ARC-typed variables (collections, resources, other closures) (#415)
- Copy-on-Write (CoW) semantics for collection types (List, Map, Set): shared collections are automatically deep-copied before mutation, preserving value semantics while avoiding unnecessary copies when the collection has a single owner (#414)
- ARC integration with resource types: `TcpStream`, `TcpListener`, `TlsStream`, `Lock`, `RWLock`, `Semaphore`, `Barrier`, `Thread`, `AtomicInt`, `AtomicBool`, `HttpRequest`, `HttpResponse`, `HttpClientResponse`, `JsonValue` are now automatically cleaned up when no longer referenced — deterministic RAII-style resource management via ARC destructors (#418)
- `weak` reference type for ARC: non-owning references that do not prevent deallocation, with atomic CAS-based upgrade to `Option<T>`, automatic scope cleanup, and pattern matching support (#416)
- `ignore_case` parameter for `contains()`, `starts_with()`, `ends_with()` — optional boolean (default `false`) enables ASCII case-insensitive matching
- ARC (Automatic Reference Counting) for collection types (List, Map, Set) and strings: automatic memory management via retain/release with scope-based cleanup, destructor generation for internal buffers, and immortal sentinel for global string constants (#413)
- ARC infrastructure: header layout (`{ strong_count, weak_count }`), `arc_alloc`/`arc_retain`/`arc_release` codegen primitives with Swift-style atomic switching support (#412)
- Relative imports: `from .helper import greet`, `from .utils import add`, `from . import add, sub` for importing relative to the current file's directory
- Auto-convert non-str operands to str in `+` concatenation: `"abc" + 2` produces `"abc2"`, `1 + "abc"` produces `"1abc"` (#393)
- Leading-dot float literals (e.g. `.5`, `.01`, `.5f64`) are now supported as shorthand for `0.5`, `0.01`, etc.
- Numeric underscore separators for improved readability: `100_000`, `0xFF_FF`, `0b1010_0101`, `3.14_159`
- Ed25519 signature verification for self-update artifacts to prevent supply-chain attacks (#124)
- `thread` package: native OS thread API with Thread, Lock, RWLock, Semaphore, Barrier, AtomicInt, AtomicBool (#363)
- `ry run` command to execute scripts defined in `package.toml` `[scripts]` section (#384)
- `path` standard library package with file path operations: `join`, `basename`, `dirname`, `extension`, `resolve`, `is_absolute` (#185)
- `filesystem` standard library package with file/directory manipulation: `list_dir`, `walk`, `glob_files`, `copy`, `move`, `remove`, `remove_all`, `make_dir`, `make_dir_all`, `file_size`, `is_file`, `is_dir`, `is_symlink`, `chmod`, `symlink`, `read_link` (#184)
- Runtime bounds checking for `char_at()` on strings — out-of-bounds access now raises a descriptive runtime error instead of silently returning an empty string (#395)
- Python-style negative index wrap-around for lists, arrays, and `char_at()` — e.g. `xs[-1]` accesses the last element (#395)
- Boundary clamping for `substring()` — out-of-range indices are clamped to `[0, length]` (#395)
- Descriptive runtime error messages for out-of-bounds access, including the actual index and collection length (#395)
- Mutual recursion and forward function references: functions can now call each other regardless of definition order, as long as they have explicit return type annotations (#550)
- `and_then` and `map` method chaining for `Result` type, enabling flat error handling without nested `match` (#597)
- Parser now accepts keyword tokens (e.g., `and`, `or`, `not`) as method names after `.` for UFCS calls

### Changed

- **Breaking**: Pattern matching syntax renamed from `when value:` to `match value:`; conditional `when:` (without subject) is unchanged (#482)
- **Breaking**: Anonymous function lambda form `function(...) => ...` is no longer supported; use parenthesized lambda syntax `(x: int) => x + 1` instead (#483)
- **Breaking**: Single-expression lambda syntax changed from `(params): expr` to `(params) => expr`; block lambdas `(params):\n  body` are unchanged (#498)
- **Breaking**: Self-update now requires Ed25519 signature verification by default; set `RY_SKIP_SIGNATURE=1` to opt out (#469)
- HTTP and JSON parsing hot paths now use pointer-based parsing to avoid unnecessary `substr` copies and temporary string allocations (#467)
- **Breaking:** Renamed the function declaration keyword from `fn` to `function`; legacy `fn` / `async fn` now produce migration errors with guidance
- Added concise Option A lambda syntax: `(x: int) -> int => x + 1` and `(x: int) => x + 1`
- **Breaking:** `args()` renamed to `arguments()` for command-line argument access (#111)
- **Breaking:** `recv()` renamed to `receive()`, `set_recv_timeout()` renamed to `set_receive_timeout()` for network operations (#111)
- **Breaking:** HTTP server functions simplified for UFCS: `http_method` → `method`, `http_path` → `path`, `http_header` → `header`, `http_body` → `body`, `http_query` → `query`, `http_query_all` → `query_all`, `http_cookie` → `cookie`, `http_cookies` → `cookies`, `http_form_field` → `form_field`, `http_form_file` → `form_file`, `http_form_fields` → `form_fields`, `http_response` → `response`, `http_listen` → `listen` (#208)
- **Breaking:** HTTP client accessor functions simplified: `http_client_status` → `status`, `http_client_body` → `body`, `http_client_header` → `header` (#208)
- **Breaking:** JSON functions simplified for UFCS: `json_type` → `kind`, `json_get` → `get`, `json_at` → `at`, `json_str` → `to_str`, `json_int` → `to_int`, `json_float` → `to_float`, `json_bool` → `to_bool`, `json_len` → `length`, `json_keys` → `keys` (#208)
- **Breaking:** IO functions simplified: `file_exists` → `exists`, `str_to_bytes` → `to_bytes` (#208)
- Expanded abbreviated parameter names in stdlib declarations: path (`a,b,c,d,p`), list (`n,f`), thread (`a`) (#111)
- Synced stdlib declaration files (`.ry`) with implementations: added missing `remove`, `take`, `tap` to `list.ry`, `remove` to `map.ry`, and corrected IO function return types to `Result` in `io.ry` (#454)
- **Breaking:** Stdin execution now requires explicit `-c` flag (`echo 'code' | ry -c`). Bare `ry` without arguments runs the `entry` file from `package.toml` instead of reading stdin (#443)
- Control-flow syntax now keeps `if`/`else`, removes `elif`, replaces `match` with `when value:`, and replaces ternary `?:` with `when:` expressions
- `char_at()` now uses a single-pass UTF-8 traversal for bounds checking and character extraction, eliminating a redundant full-string scan (#407)
- `ry new` / `ry init` now normalize hyphens to underscores in package names (e.g. `ry new my-app` creates `name = "my_app"` in package.toml)
- `.test.ry` files are excluded from directory package loading
- Fixed-length array type syntax changed from `[T; N]` to `T[N]` (e.g. `buf: i32[4] = [1, 2, 3, 4]`)
- `to_int(str)` now returns `Result<int, Error>` instead of bare `int`, properly detecting invalid input (#543)
- `int` arithmetic (`+`, `-`, `*`, unary `-`) now raises a runtime error on overflow instead of silently wrapping (#544)
- Constant expressions that overflow are caught at compile time (#544)
- Clarified in documentation that closures capture by value in both directions: outer variable changes do not affect the closure, and mutations inside the closure do not affect the outer scope (#552)
- All collection headers (List, Set, Map) now use ARC allocation uniformly, ensuring correct reference counting and CoW behavior (#572)
- `json.keys()` now returns `Result<List<str>, Error>` instead of `List<str>`, with proper null-pointer handling for OOM and non-object inputs (#599)

### Fixed

- Single-element tuple literal `(42,)` and trailing commas in tuple literals now parse correctly (#556)
- `print()` and `to_str()` now support tuples including nested tuples, displaying them as `(elem1, elem2)` (#541)
- `print()` on lists of tuples (e.g., `zip()` result) now correctly displays tuple elements instead of empty entries (#540)
- f-string interpolation now supports collection types (List, Map, Set) and tuples (e.g., `f"items: {xs}"`) (#547)
- Operator overloads (`operator[]`, `operator+`, etc.) now correctly propagate return type metadata for collection types, fixing "cannot determine list element type for index access" when `operator[]` returns `List<T>` (#537)
- `operator as` overload resolution now uses semantic type names instead of LLVM types, preventing false matches between pointer-backed types (`str`, `List<T>`, `Map<K,V>`, etc.) (#537)
- `to_str()` on union-typed values now returns the string representation of the actual value instead of the discriminant index (#536)
- Option/Result type-meta guard now checks all collection metadata keys (`TM_ListElem`, `TM_SetElem`, `TM_TaskResult`), not just `TM_MapKey`, preventing potential metadata overwrites when wrapping collection types (#525)
- Generic function parameters with collection types (`List<T>`, `Map<K, V>`, `Set<T>`) are now properly marked as ARC-managed during instantiation, preventing potential memory leaks (#524)
- Return type inference now correctly resolves user-defined struct types in functions and lambdas without explicit type annotations (#515)
- Return-path analysis now recognizes exhaustive `match` statements on custom enums and `bool`, removing false "does not return a value on all code paths" errors when all variants are covered (#513)
- Indexing into `List`, `Map`, or `Set` fields of a record (e.g., `record.field[idx]`) no longer fails with "cannot determine list element type" (#511)
- `join()` now works correctly with UFCS string receiver (e.g., `",".join(parts)`) (#508)
- Closures returned from functions can now be called, and function-type parameters can be captured in closures — enabling higher-order patterns like `make_adder`, `compose`, and currying (#510)
- **Breaking**: Legacy regex functions (`regex_match`, `regex_search`, `regex_replace`, `regex_split`, `regex_find_all`) now use text-first argument order `(text, pattern)` consistent with the regex literal API; previously `(pattern, text)` which caused silent incorrect results (#512)
- SEGFAULT when multiple functions use `match` on ADT enum parameters — `resolveType()` now correctly returns the ADT struct type instead of `i64` for enums with variant data (#507)
- `append!` / `appended` and other collection operations now work correctly on `List`, `Map`, and `Set` values returned from user-defined functions (#509)
- `operator as` codegen now supports generic target types (e.g., `int?`, `Result<int, Error>`), not just struct types (#501)
- Self-update tar validation now uses a whitelist approach, rejecting all archive entries that are not regular files or directories (device nodes, FIFOs, sockets, etc.) (#471)
- `print()` output inside `@parallel for` loops is no longer interleaved across threads — each `print()` call now produces atomic output via thread-local buffering (#473)
- Mocked functions now still enforce the original function's `require` and `ensure` contracts, preventing tests from bypassing contract checks (#441)
- Hardened codegen type promotion (`promoteToInt`/`promoteToFloat`) to reject struct and pointer types, preventing invalid LLVM IR from arithmetic, comparison, bitwise, and unary operators on non-numeric types (#394)
- Added null/allocation-failure guards in runtime functions (`runtime_io`, `runtime_net`, `runtime_path`, `runtime_regex`, `runtime_sort`) to prevent undefined behavior from null pointer dereference and integer overflow (#394)
- Cycle collector now generates visit functions for record (struct) types, enabling GC traversal of ARC pointer fields embedded in record types within ADT enum payloads (#432)
- Explicit resource free/close functions (`lock_free`, `close`, `json_free`, etc.) now decrement the ARC reference count instead of immediately freeing — aliased resources no longer cause use-after-free (#427)
- Closure destructors now recursively release captured resources and nested closures, preventing memory/resource leaks when closures are freed (#429)
- Variable reassignment now uses the full destructor resolver (covering resources and closures) instead of only resolving collection destructors
- Parser error message for unexpected tokens in statement position now says `unexpected token 'X'` instead of listing all valid keywords — also removes `expect` from keyword listing since it is a function, not a keyword (#404)
- Eliminated DNS rebinding TOCTOU gap in HTTP client SSRF protection — DNS is now resolved once and the same result is used for both the private-host check and the connection, preventing attackers from bypassing SSRF guards via DNS rebinding; also added IPv4-mapped IPv6 address detection (#470)
- HTTP response headers containing CR or LF characters are now silently skipped to prevent response splitting attacks (#472)
- Fixed float output examples in operator tutorial to match actual `%g` formatting (e.g. `1024` not `1024.0`)
- Added curl one-liner installer to Getting Started tutorial
- Fixed "struct" terminology to "record" across tutorial and reference docs
- Improved error message when hyphens are used in import paths (e.g. `from my-pkg import foo` now suggests using underscores)
- Binary operations between `str` and non-`str` types (e.g. `"abc" - 2`, `"abc" / 2`) now raise compile-time type errors instead of producing garbage output or LLVM IR verification errors (#396)
- `ry version` now works as an alias for `ry --version` instead of trying to execute the VERSION file on case-insensitive filesystems (#381)
- Dev Release nightly build fails due to missing dependencies (`openssl@3`, `ninja`, `googletest`) and removed schedule trigger (#380)
- Chained Map index access now works correctly for nested Maps (#538)
- `return none` now works correctly in generic functions with `T?` return type (#539)
- UFCS call on list literal (e.g., `[1, 2, 3].map(...)`) no longer fails to parse at statement level (#551)
- Operator overloads now correctly propagate function-type return metadata (#554)
- Fix undefined behavior in collection header deallocation where scope cleanup read invalid memory before plain-malloc headers (#572)
- Fix memory leak when collection operation results (appended, slice, etc.) are discarded as expression statements (#572)
- Heap corruption after Iterator tests caused by leaked iterator headers and states; iterator memory is now freed at scope exit (#577)
- Inline `case` body in `match`/`when` statements now parses correctly (e.g., `case Ok(v): expr`) (#587)
- Fixed double-free heap corruption in JSON `get()`/`at()` child values (#594)
- Arithmetic, comparison, and bitwise operations between high-level `int` and low-level `i64`/`u64` now correctly produce a compile error instead of silently succeeding (#595)
- `print()` now supports `Result` types directly, displaying `Ok(value)` or `Err(error)` (#612)
- `to_str()` now correctly converts `Result` and `Option` types to their string representation instead of returning the internal tag value (#611)

## [0.0.5] - 2026-03-28

### Added

- Low-level numeric types: i8, i16, i32, i64, u8, u16, u32, u64, f32 (#288)
- Numeric literal suffixes e.g. `42i32`, `3.14f32` (#289)
- Unsigned negation check — reject unary `-` on unsigned types (#312)
- `any` type with runtime dispatch, implicit conversion, and wrap/unwrap (#216, #219, #220, #221, #222, #223, #224, #225, #226, #227, #228)
- Return type inference for named functions when annotation is omitted
- `Result<V, E>` type for null-safe error handling (#104)
- `?` operator for Result error propagation (#176)
- `ensure` variable binding and remove `result`/`old` keywords (#105)
- Generic functions with type parameters (#210)
- Nested type parameter parsing (`>>`) (#263)
- Record auto-generated `operator==` and `operator!=` (#305)
- Record auto-generated `to_str` (#306)
- Record subtyping with `<` syntax for field inheritance and subtype coercion (#307)
- Record invariant inheritance: parent `invariant:` clauses are checked on child construction (#355)
- Auto-slice Error subtypes in `Err()` for Result return type coercion (#354)
- Subtype coercion for field assignment (#359)
- Subtype coercion for `?` error propagation operator (#360)
- Generic type constraints with record bounds (`<T: RecordName>`) (#297)
- `@inline` directive for function inlining hints (#299)
- Explicit value assignment for simple enum variants (#309)
- Named fields in ADT enum variants (#308)
- Subscript operator overloading `operator[]` / `operator[]=` with multi-index support (#202)
- Membership operator overloading `operator in` for user-defined types (#202)
- Call operator overloading `operator()` for callable records (#202)
- Cast operator overloading `operator as` for user-defined type conversions (#202)
- Tail call optimization (TCO) for self-recursive functions via LLVM `musttail` (#214)
- Compound assignment operator overloading with in-place optimization (#204)
- Enforce bool return type for comparison and logical operator overloads (#203)
- N-element tuple destructuring in for loops (#302)
- Implicit widening conversion in overload resolution (#212)
- `json` standard library package — parse/stringify with opaque JsonValue type (#179)
- `base64` standard library package (#183)
- TCP socket timeouts and TLS/SSL support (#76, #77)
- HTTP client functionality: `http_get`, `http_post`, `http_request` (#129)
- HTTP cookie parsing: `http_cookie`, `http_cookies` (#128)
- HTTP query parameter parsing: `http_query`, `http_query_all` (#127)
- HTTP chunked transfer encoding (#164)
- HTTP multipart/form-data parsing for server (#82)
- HTTP `max_requests` parameter for `http_listen` shutdown control (#165)
- Comprehensive HTTP status code reason phrases per RFC 9110 (#119, #125)
- `.env` file auto-loading and `env()` built-in function (#158)
- `RY_ENV` environment variable and `--env` CLI flag (#159)
- `.env` / `RY_ENV` integration with short aliases and environment-specific files (#171)
- `sleep(duration_ms: int)` built-in function (#146)
- `ry fmt` command for code formatting (#151)
- `ry new <project-name>` command (#149)
- `ry test --coverage` for line coverage measurement (#166)
- `ry test --watch` for auto-rerunning tests on file change (#163)
- Parallel test execution with `-p` / `--parallel` flag (#147)
- `--help` / `-h` option support for commands and subcommands (#337)
- HTTP keep-alive support for `http_listen` server (#79)
- Stdin execution via pipe and here-document (#250)
- `fail()` helper in test framework (#177)
- HTTP automatic redirect following for client requests (#148)
- Self-update artifact checksum verification (#116)
- Linux x86_64 (amd64) release build in CI (#154)
- Linux ARM64 (aarch64) release build in CI (#155)
- `block_on(task)` built-in function for synchronous Task waiting (#206)

### Changed

- Default return type changed from `Unit` to `any` when omitted (#218)
- Allow omitting parameter type annotations (defaults to `any`) (#217)
- Lambda expression syntax changed from `:` to `=>` (#301)
- Flatten stdlib imports — `from std.x` to `from x` (#178)
- Rename `ry.toml` to `package.toml` (#335)
- Restrict `await` to `async fn` context only — use `block_on()` in synchronous code (#206)

### Fixed

- Set literal now deduplicates elements at construction time — `{1, 2, 3, 2, 1}` correctly has length 3 (#376)
- Repo-built `ry` now prefers the checked-out stdlib over stale `~/.ry/lib/std`, restoring `base64`, `json`, and `net` timeout imports during language development (#367, #370)
- Floor division (`//`) now uses correct floor semantics instead of truncation (#239)
- Zero-division guards for integer `//` and `%` operators (#242)
- NaN comparison aligned with compiler's ordered semantics (#240)
- Require return on all code paths for non-Unit/any functions (#209)
- HTTP body NUL byte truncation (#281)
- Filter hop-by-hop headers in HTTP client requests (#280)
- `repeat()` type check and n<=0 guard (#272)
- ConstantInt metadata corruption from LLVM sharing (#311)
- Wrap value in `any` on reassignment to any-typed variable (#232)
- Reject non-str pointer types in `any` to prevent mistagging (#233)
- Overload ranking prefers concrete types over `any` (#252)
- OR pattern binding check — reject bindings but allow wildcards (#139)
- HTTP client response resource type tracking (#140)
- Directive move-only semantics to prevent silent expr loss (#102)
- Memory leak in `@property` test random strings (#100)
- UTF-8 `utf8_char_len_safe()` buffer overread (#99)
- TCP partial write handling (#114)
- TCP `recv` buffer freed on error (#115)
- TCP error handling unified to return Result instead of `exit(1)` (#120, #123)
- Truncated HTTP request body rejection (#117)
- `ry fmt` crash, `join()` arg mismatch, and multiple formatter bugs (#162)
- `ry fmt` duplicate blank line before section comments (#167)
- `ry fmt` round-trip verification to prevent code destruction (#168)
- `!` suffix restricted to function names only (#156)
- Nested stdlib modules copied recursively during self-update (#112)
- Self-update mandatory checksum verification and hardlink rejection (#126)
- Test timeout applied per `it`-block instead of per file (#333)

### Removed

- Concurrency primitives: channels, spawn, select, task_group, cancel (#304)
- `byte` type in favor of `u8` (#294)
- `join(task)` built-in — replaced by `block_on(task)` (#206)

## [0.0.4] - 2026-03-22

### Added

- Improved builtins — UTF-8, Option returns, mutating variants (#44)
- 9 new test matchers and extended existing ones (#46)
- `take` and `tap` list builtins (#47)
- Increment/decrement operators (`x++`, `x--`) (#48)
- Regex phase 2 — range quantifiers and non-greedy matching (#49)
- Lazy iterator abstraction (#50)
- Word boundary `\b`/`\B` and case-insensitive `(?i)` flag (#51)
- Concurrency primitives: spawn/await, channels, select, `@parallel for` (#54)
- `@each` / `@property` test directives (#57)
- `std.math` package (#58)
- `@native let` constants and `_`-prefix private symbols (#59)
- `std.io` module with file I/O, stdin, and byte operations (#60)
- TCP socket API for HTTP server foundation (#61)
- HTTP server API (#62)
- Directory path argument support in `ry test` (#64)
- Stable TimSort via C++ runtime replacing QuickSort (#52)

### Changed

- Replace `let`/`var` with Python-style assignment and `@const` directive (#75)

### Fixed

- Socket timeouts to prevent test hangs (#95)

## [0.0.3] - 2026-03-20

### Added

- `>>>`, string `*`, and `not in` operators (#10)
- `filter`, `map`, `sort` stream-like operations for lists (#11)
- Design by Contract support (#15)
- Directive support with `@deprecated` (#16)
- f-string, `as` cast, and `Result<T, E>` (#18)
- Compound assignment operators, `in`/`not in` for list/map, and `range()` step (#19)
- r-string (raw string) support (#20)
- Ternary operator, match OR pattern, list operations (#21)
- Lambda (`fn`), tuple destructuring, enum ADT, generic enum, collection ops (#22)
- `record` keyword, type alias, operators, naming enforcement, and collection ops (#23)
- `args()` and `exit(code)` built-in functions (#24)
- `@native` directive for built-in function declarations (#25)
- Collection functions: `remove`, `distinct`, `flatten`, `merge` (#26)
- Literal types and range types (#28)
- Function type aliases (#29)
- Generalized trailing block syntax, demoted `describe`/`it` to functions (#30)
- For-loop tuple destructuring and `@native` stdlib prelude (#31)
- `ry test` auto-discovery and removed `test_dir` (#32)
- Built-in Error type and `!!` operator replacing `Result<T, E>` (#33)
- Rust-style rich error messages (#35)
- Directory-based package system with std library (#36)
- NFA-based regex engine (Phase 1) (#37)
- `...` (ellipsis) no-op statement (#38)
- Mock/verify support in test framework (#39)
- Ry self-tests (#41)

### Changed

- Require type annotation for `none` and remove `unwrap()` (#34)

### Fixed

- Short-circuit eval, FnScope contract protection, lexer safety (#40)
- Three compiler bugs found during self-test development (#41)
- Self-update repo name and missing releases handling (#3)

## [0.0.2] - 2026-03-14

### Added

- `ry self-update` command (#1)

## [0.0.1] - 2026-03-14

Initial release.

[Unreleased]: https://github.com/t0k0sh1/ry/compare/v0.0.33...HEAD
[0.0.33]: https://github.com/t0k0sh1/ry/compare/v0.0.32...v0.0.33
[0.0.32]: https://github.com/t0k0sh1/ry/compare/v0.0.31...v0.0.32
[0.0.31]: https://github.com/t0k0sh1/ry/compare/v0.0.30...v0.0.31
[0.0.30]: https://github.com/t0k0sh1/ry/compare/v0.0.29...v0.0.30
[0.0.29]: https://github.com/t0k0sh1/ry/compare/v0.0.28...v0.0.29
[0.0.28]: https://github.com/t0k0sh1/ry/compare/v0.0.27...v0.0.28
[0.0.27]: https://github.com/t0k0sh1/ry/compare/v0.0.26...v0.0.27
[0.0.26]: https://github.com/t0k0sh1/ry/compare/v0.0.25...v0.0.26
[0.0.25]: https://github.com/t0k0sh1/ry/compare/v0.0.24...v0.0.25
[0.0.24]: https://github.com/t0k0sh1/ry/compare/v0.0.23...v0.0.24
[0.0.23]: https://github.com/t0k0sh1/ry/compare/v0.0.22...v0.0.23
[0.0.22]: https://github.com/t0k0sh1/ry/compare/v0.0.21...v0.0.22
[0.0.21]: https://github.com/t0k0sh1/ry/compare/v0.0.20...v0.0.21
[0.0.20]: https://github.com/t0k0sh1/ry/compare/v0.0.19...v0.0.20
[0.0.19]: https://github.com/t0k0sh1/ry/compare/v0.0.18...v0.0.19
[0.0.18]: https://github.com/t0k0sh1/ry/compare/v0.0.17...v0.0.18
[0.0.17]: https://github.com/t0k0sh1/ry/compare/v0.0.16...v0.0.17
[0.0.16]: https://github.com/t0k0sh1/ry/compare/v0.0.15...v0.0.16
[0.0.15]: https://github.com/t0k0sh1/ry/compare/v0.0.14...v0.0.15
[0.0.14]: https://github.com/t0k0sh1/ry/compare/v0.0.13...v0.0.14
[0.0.13]: https://github.com/t0k0sh1/ry/compare/v0.0.12...v0.0.13
[0.0.12]: https://github.com/t0k0sh1/ry/compare/v0.0.11...v0.0.12
[0.0.11]: https://github.com/t0k0sh1/ry/compare/v0.0.10...v0.0.11
[0.0.10]: https://github.com/t0k0sh1/ry/compare/v0.0.9...v0.0.10
[0.0.9]: https://github.com/t0k0sh1/ry/compare/v0.0.8...v0.0.9
[0.0.8]: https://github.com/t0k0sh1/ry/compare/v0.0.7...v0.0.8
[0.0.7]: https://github.com/t0k0sh1/ry/compare/v0.0.6...v0.0.7
[0.0.6]: https://github.com/t0k0sh1/ry/compare/v0.0.5...v0.0.6
[0.0.5]: https://github.com/t0k0sh1/ry/compare/v0.0.4...v0.0.5
[0.0.4]: https://github.com/t0k0sh1/ry/compare/v0.0.3...v0.0.4
[0.0.3]: https://github.com/t0k0sh1/ry/compare/v0.0.2...v0.0.3
[0.0.2]: https://github.com/t0k0sh1/ry/compare/v0.0.1...v0.0.2
[0.0.1]: https://github.com/t0k0sh1/ry/releases/tag/v0.0.1
