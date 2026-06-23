### Added

- `NativeCallDescriptor` C++ struct (`include/ry/native_call_descriptor.hpp`) を新規導入し、`docs/architecture/native-call-boundary.md` follow-up #1 (#2231 子計画、tracking #2299) の foundation を実装。v1 では `library_name` のみを保持し、後続の consumer PR が必要に応じて field を追加する YAGNI 方針 (`docs/architecture/native-call-boundary.md` §"NativeCallDescriptor" の他フィールドは consumer 移行 PR で初出時に追加)。
- `inferLibraryName(directiveTag, declaringModule)` pure function (`src/native_call_descriptor.cpp`) で library inference rule (a)/(b) を実装。rule (a) は `@native("<lib>")` の明示タグを優先、rule (b) は bare `@native` でも declaring module が `knownNativeLibs()` (CMakeLists.txt:386 `RY_NATIVE_LIBS` の C++ side mirror、12 entries) に含まれていれば自動推論する。Pattern B carve-out (`int(s)` / `float(s)` / `input()` / `close()`) は依然として hand-written なので rule (b) の attach は inert。
- `CodeGen::native_call_descriptors_` storage + `getNativeCallDescriptors()` getter を追加 (`include/ry/codegen.hpp`)。`@native` 宣言処理 (`src/codegen_fn.cpp:640+`) で `NativeFnSignature` と同じ dedup ルールで descriptor を構築・保存する。key 形式は `native_fn_sigs_` と同一 (`ry::util::nativeSigKey(pkg, name)`)、`fn_name`/`module_name` は key suffix/prefix と `native_fn_sigs_` lookup で復元可能なので descriptor 側には持たない。
- `tests/test_native_call_descriptor.cpp` 新規追加。pure-function inference (5 ケース) + end-to-end descriptor storage (rule (a) のみ、1 ケース) + `knownNativeLibs()` の local literal 整合 guard (1 ケース、CMake との cross-file 整合は hand-maintained の制限を test 内コメントで明記)。rule (b) end-to-end は `runSource` harness が `SourceLocation::file_id` を `share/std/<M>/<M>.ry` に偽造できないため本 PR では skip し、後続 consumer PR の spec test で間接 verify する方針。

### Scope

- foundation のみ。任意の `dispatchXXX` (io/net/http/thread/path/json/json5/base64) は無変更で、`emitTableDrivenNativeCall` / `emitGenericNativeCall` 経路も書き換えていない。
- 本 PR の foundation 着地と先行 PR #2306 / #2332 の組み合わせで #2299 を close する。後続の per-module descriptor 移行は本 issue の範囲外として個別 issue で追跡する。 (#2299)
