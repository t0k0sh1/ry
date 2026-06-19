### fix(thread): `threadSpawn(workerFn)` now returns the worker's value for int/float/bool and rejects unsupported return types up front (#2262)

`threadSpawn(myFn)` に named function (`fn worker() -> T:`) や変数に束縛された lambda / closure を直接渡したとき、`T ∈ {int, float, bool}` は silent に `0`/`false` を返し、`T = any` は `std::thread` 内部の `__thread_proxy` で SIGSEGV していた (inline lambda `threadSpawn(() => 42)` は正しく動いていた)。`src/codegen_call_thread.cpp` の `emitThreadSpawn` 内 variable-reference 経路 (no-captures / with-captures いずれも) の trampoline thunk で `callTy` が `void(...)` 固定だったため worker の戻り値が捨てられ、さらに `workerRetTy` が `nullptr` のまま `setTypeMeta(TypeMeta::ThreadResult, ...)` をスキップして `threadJoin` 側が Unit パスに流れ込んでいたのが原因。

両分岐を inline-lambda 経路と同じ ABI に揃え、`FnTypeInfo::returnType` から worker の実際の戻り型を取り出して `Unit` / `int` / `float` / `bool` の値を `result_buf` に store するようにした。`any` / ARC 型 / sum 型は MVP (#828) と同じ文言で codegen error として明示的に reject する。
