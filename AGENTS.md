# ry - 開発ガイドライン

## ビルド & テスト

```bash
cmake --preset default                                  # Ninja + LLVM（CMakePresets.json）
cmake --build build                                     # Ninja が自動並列ビルド
./build/ry_tests                                        # C++ テスト (GoogleTest)
./build/ry test -p                                      # Ry セルフテスト (全 *.test.ry)
./build/ry test tests/spec/<file>.test.ry               # 個別ファイル実行
```

> repo 内でビルドした `./build/ry` は `package.toml` の hidden 設定 `[paths]._dev_stdlib` に従ってプロジェクトローカルの `share/std/` を優先する。`RY_ENV=internal` は追加の isolation が必要な場合だけ使う。

## コンパイラ警告フラグ

内部ターゲット（`ry_lib`, `ry`, `ry_tests`, native libs）には厳格な警告フラグが有効化されている:

```text
-Wall -Wextra -Wpedantic -Wconversion -Wshadow
```

- 新規コードは警告ゼロを維持すること
- LLVM / GoogleTest のヘッダは `SYSTEM` include として扱われ、警告対象外
- `-Werror` は現時点では未導入（別 issue）
- フラグは `CMakeLists.txt` の `RY_WARNING_FLAGS` 変数で一元管理し、`target_compile_options(... PRIVATE ...)` で各ターゲットに適用

## Clang-Tidy 静的解析

プロジェクトルートの `.clang-tidy` でチェック設定を管理する。CI の `clang-tidy` ジョブが全 `src/*.cpp` ファイルに対して実行する。

```text
有効: bugprone-*, performance-*, cert-*, 選択的 modernize-*
除外: bugprone-easily-swappable-parameters, cert-err58-cpp 等（詳細は .clang-tidy 参照）
```

- `HeaderFilterRegex` はプロジェクトヘッダ (`include/ry/`) のみに制限
- LLVM / GoogleTest ヘッダは SYSTEM include のため自動除外
- `compile_commands.json` は `CMAKE_EXPORT_COMPILE_COMMANDS=ON` で自動生成（`build/` 内）
- ローカル実行: `find src -name '*.cpp' | xargs clang-tidy -p build --quiet`
- 新規コードは Clang-Tidy 警告ゼロを維持すること

## CI: LLVM ツールチェーン (ミラー)

CI は `.github/actions/setup-llvm/` composite action 経由で LLVM を取得する。優先順に:

1. **`actions/cache`** — キャッシュヒット時は即復元（< 5s）
2. **GitHub Releases ミラー** — `llvm-toolchain-${VERSION}` タグからダウンロード + SHA256 検証
3. **apt.llvm.org フォールバック** — ミラーが存在しない場合のみ

ミラー tarball は `.github/workflows/mirror-llvm-toolchain.yml`（手動 `workflow_dispatch`）で構築・アップロードする。

**キャッシュキー**: `llvm-${VERSION}-linux-x86_64-v2-${SHA256_SHORT}`。`restore-keys` は意図的に設定しない — 部分一致ヒットは異なるバージョンの LLVM を復元し、ビルド失敗や ABI 不整合を引き起こす。

**バージョンバンプ手順**:

1. `mirror-llvm-toolchain.yml` を `workflow_dispatch` で実行し、新バージョンの tarball をアップロード
2. 以下のワークフローの `env.LLVM_VERSION`（および `env.LLVM_SHA256_SHORT`）を更新:
   - `.github/workflows/ci.yml`
   - `.github/workflows/codeql.yml`

## ナレッジベース (KNOWLEDGE.md)

プロジェクトルートの `KNOWLEDGE.md` は、PR レビューで受けた指摘・実装中に発見した落とし穴・設計判断の理由など、コードを読んでも分からない知見を蓄積する long-term memory。リポジトリ管理されており、Claude Code も人間コントリビュータも読む。

- **読むタイミング**: Plan モード開始時に必ず一読する。該当しそうなカテゴリがあれば `grep -nE '\*\*Tags\*\*:.*<keyword>' KNOWLEDGE.md` で絞る
- **書くタイミング**:
  1. PR レビュー対応後 — 他 PR にも再発しうる指摘は必ず追記
  2. 実装中 — 非自明な事実・落とし穴を発見したら追記
  3. Plan 作成中 — 採用しなかった設計判断の理由を追記
  4. コマンド・環境変数のミスをリカバリした時 — 再発防止用に `Commands / Environment gotchas` セクションへ追記
- **書き方**: 1 つの教訓につき 1 エントリ。`Source` と `Tags` と `Rule` を必ず書く。詳細は `KNOWLEDGE.md` 冒頭の writing rules に従う
- **言語**: 英語推奨（CodeRabbit / Codex 等 AI レビュワーも読める）

## ASan + UBSan（Address + UndefinedBehavior Sanitizer）

ローカル開発では ASan と UBSan を同時に有効化してテストを実行する。`asan` preset は `ENABLE_ASAN=ON` と `ENABLE_UBSAN=ON` を両方設定する:

```bash
cmake --preset asan                                     # Debug + ASan + UBSan（build-asan/）
cmake --build build-asan                                # ビルド
ASAN_OPTIONS=detect_container_overflow=0 UBSAN_OPTIONS=print_stacktrace=1:halt_on_error=1 \
    ./build-asan/ry_tests                               # C++ テスト
ASAN_OPTIONS=detect_container_overflow=0 UBSAN_OPTIONS=print_stacktrace=1:halt_on_error=1 \
    ./build-asan/ry test -p                             # Ry セルフテスト
```

> `detect_container_overflow=0` は、ASan なしでビルドされた LLVM ライブラリとの混在で生じる false positive を抑制するために必要。
>
> UBSan は `-fno-sanitize=vptr,function` を付与してビルドされる。前者はプロジェクトが `-fno-rtti` を使うため動作せず、後者は LLVM が C 風の関数ポインタキャストを多用するため false positive の温床になる。

ASan または UBSan が検出した問題（メモリリーク、バッファオーバーフロー、use-after-free、未定義動作等）は必ず解消すること。サニタイザーエラーを残したままコミットしてはならない。

## TSan（ThreadSanitizer）

スレッド安全性の検証には TSan ビルドを使う。TSan は ASan / UBSan と排他で、別ディレクトリ（`build-tsan/`）にビルドされる:

```bash
cmake --preset tsan                                     # Debug + TSan（build-tsan/）
cmake --build build-tsan                                # ビルド
TSAN_OPTIONS=halt_on_error=1:second_deadlock_stack=1 ./build-tsan/ry_tests      # C++ テスト
TSAN_OPTIONS=halt_on_error=1:second_deadlock_stack=1 ./build-tsan/ry test -p    # Ry セルフテスト
```

> #630 の P0 race fix（`@parallel for` 捕捉値の atomic ARC retain/release、capture 時 retain による CoW `> 1` invariant 確保、CoW の atomic load、GC の `strong_count` atomic read）が landing 済み。CI の `tsan` ジョブのうち **C++ テスト (`ry_tests`) は required** で、`ConcurrencySpecSuite` (= `tests/spec/concurrency.test.ry` の stress test) をこのステップで検証する。**Ry self-test (`ry test -p`) は warn-only**（upstream TSan の `LargeMmapAllocator` CHECK 問題により Linux runner で crash する。詳細は KNOWLEDGE.md 「TSan LargeMmapAllocator CHECK failure on Linux」を参照）。
>
> 新しい race を導入した場合は同 PR 内で必ず修正すること。warn-only は TSan allocator バグの回避のみであり、実際の race 導入を許容するものではない。TSan が #630 の audit に無い race パターンを検出した場合は新規 concurrency issue を起票し、`tests/spec/concurrency*.test.ry` に再現テストを追加する。

## メモリ安全ルール（C++ ランタイム）

`include/ry/runtime_alloc.hpp` の安全なラッパーを使用すること。以下の関数は新規コードで直接呼び出してはならない:

| 禁止関数 | 代替 | 理由 |
|---------|------|------|
| `malloc` | `checked_malloc` | OOM 時の null 未チェック → segfault |
| `realloc` | `checked_realloc` | OOM 時の null 未チェック |
| `calloc` | `checked_malloc` + `memset` | OOM 時の null 未チェック |
| `strdup` | `checked_strdup` | OOM 時の null 未チェック |
| `strndup` | `checked_strndup` | OOM 時の null 未チェック |
| `malloc(count * sizeof(T))` | `checked_array_malloc(count, sizeof(T))` | 整数オーバーフロー → ヒープバッファオーバーフロー |

その他のルール:
- OOM 時は `oom_abort(n)` のように要求サイズを渡して即座に中断する（nullptr を返すパターンは使わない）
- 外部入力（HTTP リクエスト、JSON パース結果等）を `strcmp` / `strlen` に渡す前に NULL チェックを行う
- CI の `lint` ジョブが禁止関数の直接呼び出しを検出し、新規コードが追加された場合は自動でブロックする

## ワークフロー全体像

1. **issue 確認** — 対象 issue の内容を把握する
2. **`wip` ラベル付与** — 対象 issue に `wip` ラベルを付ける
3. **KNOWLEDGE.md 参照** — 関連しそうな既存エントリを grep して一読する
4. **Plan モード** — 実装計画を立てる
5. **実装** — TDD ベースで開発する
6. **セルフ検証** — テスト実行・ドキュメント反映・KNOWLEDGE.md 追記・ラベル整理チェック
7. **ユーザー指示を待つ** — 以降の操作は「責務の分離」セクションに従う

## issue 起点の開発

- **リポジトリ**: `t0k0sh1/ry`
- **開始パターン**:
  - ユーザーが issue 番号または URL を指定 → GitHub MCP で issue を読み取り、内容を把握して Plan モードへ
  - ユーザーが「次の issue を探して」と指示 → open な issue を取得し（`wip` ラベル付きは除外）、バグ優先・効果の高い改善を優先して候補を提示、ユーザーが選択後に Plan モードへ
- **Plan モードとの接続**: issue の内容を仕様として Plan に反映する
- **ラベル運用**:
  - issue に着手する時点で `wip` ラベルを付与する
  - `wip` ラベルの除去と issue クローズは **PR マージ後** に行う（詳細は「作業完了前チェックリスト > 4. ラベル整理」）

## Plan モードのルール

- **開始条件**: 対象 issue が特定されていること、対象 issue に `wip` ラベルが付与されていること、リリースブランチ `vx.x.x` にいること、かつリモートと最新化されていることを確認する
- **実装計画の最初のタスク**: フィーチャーブランチの作成
- **実装計画のスコープ**: セルフ検証まで（git add / commit / push / PR 作成は含めない）
- **実装計画に必ず含めるもの**:
  - `KNOWLEDGE.md` の関連エントリを参照したか（該当エントリがあれば Plan 本文に引用し、どう活用するかを明示する）
  - 仕様通りに実装できていることのセルフ検証タスク
  - 英語ドキュメント（README.md / docs）の更新（または変更不要の確認）
- **スコープ外の問題を発見した場合**: 実装計画の策定中にスコープ外の問題・改善点を発見した場合は、実装計画内に「スコープ外 issue の起票」タスクを含め、実装フェーズで issue を作成すること（既存の重複 issue がないか事前にチェックする）

## TDD ベースの開発プロセス

### 既存コードの変更時

1. 変更を検出できるテストが存在することを確認（なければ先に作成）
2. コード変更を実施（既存テストが失敗する状態になる）
3. 変更後の仕様に基づくテストを追加
4. 変更前仕様テスト失敗 & 変更後仕様テスト成功を確認
5. 失敗しているテスト（変更前仕様）を削除
6. リファクタリング

### 新機能追加時

1. 変更後の仕様に基づくテストを作成（失敗することを確認）
2. 実装してテスト成功を確認
3. リファクタリング

## stdlib パッケージの追加手順

新しい標準ライブラリパッケージ（例: `crypto`）を追加するための手順。

### 1. Ry 宣言ファイル作成

`share/std/<pkg>/<pkg>.ry` に `@native("pkg")` 宣言を記述する。`manifest.json` の更新は不要だが、宣言ファイルの追加だけでは package は使えるようにならない。

```ry
@native("crypto")
fn sha256(data: str) -> str
```

### 2. C++ ランタイム実装

`src/runtime_<pkg>.cpp` に `extern "C"` 関数を実装する。関数名は `__ry_<pkg>_<name>` の規約に従う。

```cpp
extern "C" const char *__ry_crypto_sha256(const char *data) { ... }
```

### 3. ビルド設定

`CMakeLists.txt` で `add_ry_native_lib(pkg src/runtime_<pkg>.cpp)` を追加して共有ライブラリを作成する。`RY_NATIVE_LIBS` リストにも追加して `ry` と `ry_tests` にリンクする。

### 4. Codegen dispatcher（カスタムロジックが必要な場合のみ）

単純な関数（引数をそのまま渡してランタイムを呼ぶだけ）は `emitGenericNativeCall` が自動処理するため、codegen ファイルの作成は不要。

リソーストラッキング、受信者型dispatch、Option wrapping 等のカスタムロジックが必要な場合は:
1. `src/codegen_call_<pkg>.cpp` を作成し、`RY_REGISTER_STDLIB_PACKAGE` マクロで自己登録 + `NativeDispatchEntry` テーブル + free function `custom_emitter` を定義
2. opaque リソース型がある場合は `ResourceKindRegistry::instance().registerKind(...)` で静的初期化時にリソース種別を登録
3. `CMakeLists.txt` の `ry_lib` にソースファイルを追加

共通ヘルパー（`codegen_call_dispatch.cpp` に実装済み）を活用する:

| ヘルパー | 用途 |
|---------|------|
| `wrapPtrAsResult(ptr, errFn)` | nullable ptr → `Result<T, Error>` |
| `wrapStatusAsResult(status, errFn)` | int status → `Result<Unit, Error>` |
| `emitResultBranch(isErr, resTy, buildOk, buildErr)` | カスタム Result 構築 |
| `buildErrorFromRuntime(errFn)` | ランタイムから Error struct を構築 |

### 5. テスト追加

- package import テストを追加する
- 代表的な native function の実行テストを追加する
- 必要なら declaration file / native constant の registry 整合テストも追加する

### 定数の追加

`share/std/<pkg>/<pkg>.ry` に `@const` 宣言を追加する。通常は `@native("pkg")` を使うが、`math` のように個別の shared library を持たないパッケージでは bare `@native` を使う（詳細は KNOWLEDGE.md「Bare `@native` vs `@native("pkg")`」参照）。dispatch ファイル内で `StdlibRegistry::instance().registerConstant(...)` を静的初期化時に呼び出す（registry 本体は `include/ry/stdlib_registry.hpp` の `StdlibRegistry` クラスで、`src/codegen_call.cpp` 内の `MathConstReg` が具体例）。`codegen_stmt.cpp` の変更は不要。

### 既存パッケージへの関数追加

既存パッケージに関数を追加する場合は、以下の箇所を確認する:

1. `share/std/<pkg>/<pkg>.ry` — `@native("pkg") fn` 宣言を追加
2. `src/runtime_<pkg>.cpp` — C++ 実装を追加
3. `src/codegen_call_<pkg>.cpp` — カスタム dispatch が必要なら custom_emitter を追加（単純な関数は不要）
4. テスト — selective import と実行ケースを追加

## repo build と stdlib 解決

- repo 内でビルドした `./build/ry` / `./build-current/ry` は、この project の `package.toml` にある hidden 設定 `[paths]._dev_stdlib` を使って project local の `share/std` を参照する
- OS にインストールされた `ry` はこの hidden 設定を無視し、`~/.ry/share/std` を参照する
- `RY_ENV=internal` は追加の isolation 用であり、repo 開発時の通常動作に必須ではない

## 内部挙動の解析に trace を使う

- Ry の内部挙動、コンパイルの流れ、import 解決、JIT 実行、関数呼び出し、分岐選択を把握したい場合は `./build/ry --trace` を優先して使う
- trace は人間向けログではなく JSON Lines の機械可読ストリームとして扱う
- プログラムの標準出力そのものも確認したい場合は `--trace-out=<path>` を使って trace を別ファイルへ逃がす
- テストの解析では `./build/ry test --trace ...` を使う
- trace は冗長になりやすいため、挙動が不明確な場面や根拠が必要な場面で選択的に使う
- trace を使って解析した場合は、Plan や調査結果の要約に「trace で確認した事実」を明示する

例:

```bash
./build/ry --trace app/main.ry
./build/ry --trace-out=/tmp/ry-trace.jsonl app/main.ry
./build/ry test --trace tests/spec
echo 'print(1)' | ./build/ry --trace -c
```

## Git ブランチ運用ルール

- コミット前に現在のブランチを確認し、`main` または `vx.x.x` 形式のブランチにいる場合はコミットを行わないこと
- コミット・PR 作成時は、常に現在のブランチから新しいフィーチャーブランチを作成すること
- PR のマージ先は、作業開始時のブランチ（分岐元）とする
- PR を非デフォルトブランチ（`vx.x.x` 等）にマージした場合、GitHub の `Closes #xx` による自動クローズは動作しない。ラベル整理は「作業完了前チェックリスト」に従うこと
- PR マージ前に、未追跡ファイルや未コミットの変更がないか確認すること。ある場合はマージ前にユーザーに報告し、コミットの要否を確認する
- `.serena/` ディレクトリに差分がある場合は、他の変更と一緒にコミットすること
- リリース時は `vx.x.x` を `main` にマージする PR を作成する。詳細は「リリース準備ワークフロー」を参照

## 責務の分離

### Claude Code が自律的に行うこと

- 実装
- テスト実行
- セルフ検証
- ドキュメント更新
- PR マージ後の issue クローズと `wip` ラベル除去（マージ完了直後に実行、ユーザーの指示を待たない）

#### スコープ外の問題を発見した場合の対応ルール

実装中・セルフ検証中・PR レビュー対応中など、あらゆる場面で以下のルールを適用する。

1. **当該変更に起因する不具合・バグ → フィーチャーブランチで対応する**
   - 変更前には発生していなかった不具合やバグは、基本的に当該変更が間接的にでも影響していると判断し、現在のフィーチャーブランチ内で修正すること
   - スコープ外であっても、変更によって引き起こされたバグは先送りしない

2. **将来的に対応が必要な改善項目 → issue を作成し報告する**
   - ユーザーに確認せず自律的に issue を作成する
   - 既存の対応 issue と重複しないよう `gh search issues` 等で事前にチェックすること。既存 issue があればコメントを追加する
   - 作成した issue はユーザーに報告すること（issue 番号とタイトルを提示）

### ユーザーが明示的に指示すること

- 外部レビュー（GitHub PR レビュー等）
- git add / commit / push
- PR 作成

### PR レビュー対応後の注意

PR レビュー指摘を修正した場合、修正内容がコミット・プッシュされていなければ PR に反映されない。レビュー対応の完了時に、未コミットの変更がある場合はその旨をユーザーに必ず伝え、コミット・プッシュを促すこと。

### PR レビューから得た学びの蓄積

PR レビュー（CodeRabbit / Copilot / 人間）で受けた指摘のうち、**他の PR にも再発しうる一般的なパターン**は `KNOWLEDGE.md` に追記する。単発のタイポ修正や、その PR 限りの local な指摘は追記不要。判断基準:

- 「次回同じミスをしないために記録すべき」と感じたら追記する
- 「この指摘は過去にも受けた気がする」と感じたら、既存 entry を更新する
- 追記はユーザの指示を待たず Claude Code が自律的に行う
- 追記は該当 PR のフィーチャーブランチ内で行い、レビュー対応コミットと一緒にプッシュする

## 作業完了前チェックリスト

タスクの完了前に、以下を必ず実行すること。

### 1. ドキュメント反映チェック（英語のみ）

機能の**追加・変更・削除**を行った場合、**英語ドキュメントのみ**を更新する。

**判断基準**: ドキュメントに現在記載があるかではなく、**ユーザーが知るべき内容かどうか**で更新要否を判断する。新機能・挙動変更・新オプションなど、ユーザーに影響する変更は必ずドキュメントに反映すること。

対象と確認観点:

- **`docs/reference/`** — 型・演算子・制御構文・関数・コレクション・組み込み関数・エラーなどの仕様変更があれば該当ファイルを更新
- **`docs/tutorial/`** — ユーザー向け新機能があれば関連するチュートリアルを更新
- **`docs/README.md`** — ドキュメント目次の更新（新ページ追加時）
- **`README.md`** — 以下の内容に関わる変更があれば更新（詳細は docs/ に委譲）:
  - Features（言語機能の追加・変更）
  - Sample Code（新機能のデモに適したコード変更）
  - Installation（インストール方法の変更）
  - Usage（CLI コマンドの追加・変更）

反映が不要と判断した場合は、その理由を明示すること（内部リファクタリングのみ、テスト追加のみ、等）。

### 2. CHANGELOG 更新チェック

ユーザーに影響のある変更（`feat:`, `fix:`, 破壊的変更）を行った場合、`changelog.d/` にフラグメントファイルを作成する。

**ファイル名**: `changelog.d/{issue番号}-{slug}.md`（例: `changelog.d/545-546-list-improvements.md`）

**内容**: `### Added` / `### Changed` / `### Fixed` / `### Removed` セクションのみを記述する。複数カテゴリにまたがる場合は 1 ファイルに複数セクションを含める。

```markdown
### Added

- Empty list literal `[]` is now supported with type annotation (#545)

### Fixed

- Some bugfix description (#545)
```

> **注意**: `CHANGELOG.md` を直接編集しないこと。フラグメントファイルはリリース準備時に `scripts/assemble-changelog.sh` で CHANGELOG.md に集約される。

内部リファクタリング・テスト追加・CI 変更のみの場合はフラグメント作成不要。

### 2.5. KNOWLEDGE.md 更新チェック

今回の作業で以下のいずれかが発生した場合、`KNOWLEDGE.md` に追記する:

1. **新しい拒否ブランチ・検証チェックを追加した** — 将来の回帰を防ぐテストルール entry が既にあるか確認し、無ければ追加する:
   ```bash
   git diff origin/<base> -- 'src/**' 'include/**' \
     | grep -nE '^\+.*(codegenError|parserError|return std::nullopt)'
   ```
   hit した各拒否ブランチに対して、直接トリガーする回帰テストが存在することを確認する（合法ケースのテストでは代替不可）。
2. **実装中に非自明な落とし穴を発見した** — 例: 特定の LLVM API の罠、opaque pointer 周りの注意点、ARC retain/release の順序依存等
3. **採用しなかった設計判断がある** — なぜその案を選ばなかったかを書いておくと、将来同じ検討を繰り返さずに済む
4. **コマンド・環境変数・シェル構文のミスをリカバリした** — 実行したコマンドが間違っていて失敗したが 2 回目以降で正解に辿り着いた場合、以下に該当するなら `KNOWLEDGE.md > Commands / Environment gotchas` に追記する:
   - フラグや引数の組み合わせを間違えて失敗した
   - 必要な環境変数（`ASAN_OPTIONS`, `RY_ENV` 等）を忘れて失敗した
   - `cmake --preset` 名や path を間違えた
   - `gh` / `git` コマンドの subcommand / flag を間違えた
   - heredoc / quoting / escaping を間違えた

   修正が自明でなかった（＝ドキュメントに書いていない、直感に反する）場合のみ記録する。単なるタイポは不要。

追記不要と判断した場合は、その理由を明示する（純粋なバグ修正で再発しない、その PR 限りの local な指摘、等）。

### 3. 全テスト実行

全テストを実行して成功を確認する。

```bash
cmake --preset default && cmake --build build && ./build/ry_tests && ./build/ry test -p
```

テストが失敗した場合は、原因を修正してから作業完了とすること。

### 3.5. サニタイザー検証

**ASan + UBSan**（メモリ安全性 + 未定義動作）:

```bash
cmake --preset asan && cmake --build build-asan && \
  ASAN_OPTIONS=detect_container_overflow=0 UBSAN_OPTIONS=print_stacktrace=1:halt_on_error=1 ./build-asan/ry_tests && \
  ASAN_OPTIONS=detect_container_overflow=0 UBSAN_OPTIONS=print_stacktrace=1:halt_on_error=1 ./build-asan/ry test -p
```

ASan / UBSan が検出した問題は原因を修正してから作業完了とする。これらのエラーを残したままコミットしてはならない。

**TSan**（スレッド安全性）:

```bash
cmake --preset tsan && cmake --build build-tsan && \
  TSAN_OPTIONS=halt_on_error=1:second_deadlock_stack=1 ./build-tsan/ry_tests && \
  TSAN_OPTIONS=halt_on_error=1:second_deadlock_stack=1 ./build-tsan/ry test -p
```

C++ TSan テスト (`ry_tests`) は required で、`ConcurrencySpecSuite` (= `tests/spec/concurrency.test.ry` stress test) を検証する。Ry self-test (`ry test -p`) は TSan `LargeMmapAllocator` CHECK 問題 (upstream #1716) により warn-only — ローカルでも CI でも C++ テストが clean run していれば本 PR スコープでは OK とする。race が検出された場合 (C++ / self-test どちらでも) は本 PR スコープ内で修正すること。既知 race として扱って先送りしてはならない。#630 の audit に無い新規 race パターンを発見した場合は新規 concurrency issue を起票し、再現テストを `tests/spec/concurrency*.test.ry` に追加する。

### 4. ラベル整理

**セルフ検証完了時点ではラベルを変更しない。** ラベルの切り替えは PR マージ後に行う:
- PR がマージされたら、Claude Code が自律的に対象 issue の `wip` ラベルを外し issue をクローズする
- PR を非デフォルトブランチ（`vx.x.x` 等）にマージした場合、GitHub の `Closes #xx` による自動クローズは動作しないため、Claude Code が GitHub API で issue をクローズすること
- この操作はマージ完了の直後に必ず実行し、ユーザーの指示を待たない

## リリース準備ワークフロー

リリース準備は `/release-prep` スキルを使用する。リリース（`vx.x.x` → `main` マージ）は `/release` スキルを使用する。
