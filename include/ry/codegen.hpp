#pragma once

#include "ry/ry_layout.hpp"
#include "ry/ast.hpp"
#include "ry/sema_return.hpp"
#include "ry/source_location.hpp"
#include "ry/source_manager.hpp"
#include "ry/trace.hpp"
#include <llvm/ExecutionEngine/Orc/ThreadSafeModule.h>
#include <llvm/ADT/SmallPtrSet.h>
#include <llvm/ADT/SmallVector.h>
#include <llvm/ADT/STLFunctionalExtras.h>
#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/LLVMContext.h>
#include <llvm/IR/Module.h>

#include <functional>
#include <map>
#include <memory>
#include <optional>
#include <string>
#include <tuple>
#include <unordered_map>
#include <unordered_set>
#include <vector>


namespace ry {

class CodeGen {
public:
    explicit CodeGen(bool test_mode = false, const SourceManager *sm = nullptr,
                     bool coverage_mode = false, int coverage_file_id_offset = 0,
                     bool outline_mode = false);
    llvm::orc::ThreadSafeModule compile(Program &prog);
    const std::vector<std::string>& getWarnings() const { return warnings_; }

    // --- @native function signature types ---

    struct NativeFnParam {
        std::string name;
        std::string typeName;    // Ry type name ("str", "int", "List<int>", etc.)
    };

    struct NativeFnSignature {
        std::string name;              // function name ("encode", "sin", etc.)
        std::string package;           // package name ("base64", "math", etc.; empty for builtins)
        std::string library;           // @native("libname") argument; empty = built-in (static link)
        std::vector<NativeFnParam> params;
        std::string returnTypeName;   // Ry return type name
        std::vector<std::string> directiveNames;  // e.g. {"native", "deprecated"}
    };

    // --- Table-driven native call dispatch ---

    enum class ReturnWrapping {
        Direct,               // return as-is (ptr→ptr, void→Unit, i64→int)
        ResultPtr,            // wrapPtrAsResult(ptr, errFn)
        ResultStatus,         // wrapStatusAsResult(status, errFn)
        ResultOutParam,       // alloca + out-param + emitResultBranch
        BoolFromI64,          // i64 → trunc to i1
        ResultPtrWithListMeta // ResultPtr + type_meta_[ListElem] annotation
    };

    struct NativeDispatchEntry;  // forward declaration

    // Free function pointer for custom emitter escape hatch.
    // Custom emitters are defined as free functions in codegen_call_<pkg>.cpp.
    using CustomEmitterFn = llvm::Value *(*)(CodeGen &cg, const CallExpr &e);

    // Post-call type metadata annotation for TypeMeta::ListElem.
    enum class ListElemMeta : uint8_t { None, I8, Ptr };

    struct NativeDispatchEntry {
        const char *fnName = nullptr;         // e.g. "encode", "collect"
        const char *rtSuffix = nullptr;       // runtime name suffix override (nullptr = use fnName)
        ReturnWrapping wrapping = ReturnWrapping::Direct;
        int arity = 0;                        // -1 = variadic (e.g. path::join 2-4 args)
        const char *outParamType = nullptr;   // for ResultOutParam only (e.g. "int"); nullptr otherwise

        // --- Tier 2 additions ---
        CustomEmitterFn customEmitter = nullptr;   // escape hatch for complex logic
        const char *rtNameOverride = nullptr;      // full runtime name (e.g. "sin", "__ry_file_exists")
        const char *errFnOverride = nullptr;       // error function override (nullptr = derive from package)
        ListElemMeta listElemMeta = ListElemMeta::None;  // post-call TypeMeta::ListElem annotation
        int requireListU8Arg = -1;  // arg index that must carry TypeMeta::ListElem==i8; -1=no check
    };

    // Access the @native function signature registry.
    // Keyed by "package::name" for package functions, bare name for builtins.
    const std::unordered_map<std::string, std::vector<NativeFnSignature>>&
    getNativeFnSigs() const { return native_fn_sigs_; }

    // Libraries that must be dynamically loaded at JIT time (demand-driven:
    // only includes libraries for functions actually called during codegen).
    const std::unordered_set<std::string>& getRequiredLibraries() const;

    // Derive the base runtime function name for a stdlib package function.
    // e.g. ("base64", "encode") → "__ry_base64_encode"
    // For overloaded functions, callers must append an arity suffix
    // (e.g. "__ry_path_join2", "__ry_path_join3") as needed.
    // Only meaningful for package functions; builtins use varied naming.
    static std::string deriveRuntimeFnName(const std::string &package,
                                           const std::string &fn_name);

    // Build the registry key for native_fn_sigs_.
    static std::string nativeSigKey(const std::string &package,
                                    const std::string &name) {
        return package.empty() ? name : package + "::" + name;
    }

    // Table-driven native call dispatch (public for use by self-registering
    // stdlib packages in codegen_call_<pkg>.cpp files).
    llvm::Value *emitTableDrivenNativeCall(const CallExpr &e,
                                            const char *package,
                                            const NativeDispatchEntry *table,
                                            size_t table_size);

    // ======== LLVM Types & Builder ========
    // Members below are accessed by free-function custom emitters in
    // codegen_call_<pkg>.cpp files, so they must be public.
public:
    std::unique_ptr<llvm::LLVMContext> ctx_;
    std::unique_ptr<llvm::Module> mod_;
    llvm::IRBuilder<> builder_;
    llvm::Function *fn_ = nullptr;
    llvm::Type *i64Ty_, *i32Ty_, *i16Ty_, *i8Ty_, *f64Ty_, *f32Ty_, *i1Ty_, *ptrTy_;
    llvm::FunctionType *fnTy_ptr_to_ptr_;
    llvm::FunctionType *fnTy_ptr_to_i64_;
    llvm::FunctionType *fnTy_ptr_to_void_;
    llvm::FunctionType *fnTy_ptr_ptr_to_ptr_;
    llvm::FunctionType *fnTy_ptr_ptr_to_i64_;
    llvm::FunctionType *fnTy_ptr_i64_to_ptr_;
    llvm::FunctionType *fnTy_ptr_ptr_ptr_to_ptr_;
    // Length-aware variants used by NUL-safe regex ABI (#1052)
    llvm::FunctionType *fnTy_ptr_i64_ptr_i64_to_i64_;
    llvm::FunctionType *fnTy_ptr_i64_ptr_i64_to_ptr_;
    llvm::FunctionType *fnTy_ptr_i64_ptr_i64_ptr_i64_to_ptr_;
    llvm::FunctionType *fnTy_void_to_ptr_;
    llvm::StructType *listHeaderTy_;
    llvm::StructType *mapHeaderTy_;
    llvm::StructType *setHeaderTy_;
    llvm::StructType *iteratorHeaderTy_;
    llvm::StructType *errorTy_;
    llvm::StructType *anyTy_;
    llvm::StructType *typeTy_;            // { i64 id, ptr name } — returned by type_of

    // ======== ARC Infrastructure ========
    // Resource kind IDs are assigned dynamically by ResourceKindRegistry.
    // -1 (ResourceKindRegistry::NONE) means "not a resource".
    using ResourceKind = int;
    llvm::StructType *arcHeaderTy_;                       // { i64 strong_count, i64 weak_count }

    // Cycle collector — static analysis & visit function generation
    std::unordered_set<std::string> potentially_cyclic_types_;
    std::unordered_map<std::string, llvm::Function*> gc_visit_functions_;
    void collectTypeGraphFromStmt(
        const StmtNode &stmt,
        std::unordered_map<std::string, std::unordered_set<std::string>> &graph,
        std::unordered_set<std::string> &all_types);
    void runCyclicTypeAnalysis(
        std::unordered_map<std::string, std::unordered_set<std::string>> &graph,
        const std::unordered_set<std::string> &all_types);
    bool isPotentiallyCyclic(const std::string &typeName) const;
    llvm::Function *getOrCreateVisitFunction(const std::string &typeName);
    std::unordered_set<llvm::Value*> arc_atomic_values_;  // values requiring atomic refcount ops
    // Depth counter for enclosing @parallel for codegen scope. While > 0,
    // `isArcAtomic()` returns true unconditionally so every retain/release/CoW
    // emitted inside the worker uses the atomic path. Mutated only via
    // ParallelForScope (RAII) so a mid-emission codegenError cannot leak a
    // non-zero depth into subsequent unrelated codegen. See #630.
    int parallel_for_depth_ = 0;

    // Hint for None()/none inner type in branch-merge positions (#1154).
    // Set to the expected inner type before emitting an arm expression that
    // may contain None() or bare none. nullptr means "no hint; fall back to
    // fn_->getReturnType() or i8/i64 defaults".
    // Written by branch-arm guard paths (IfExpr, IfBlockExpr, CaseCondExpr,
    // and CaseExpr arm-scoped/scrutinee-derived flows); None()/NoneExpr
    // emitters read it.
    llvm::Type *option_none_hint_inner_ = nullptr;

    // Declaration-context annotation inner type (#1154). Written by
    // emitVarDecl / local-reassign / global-reassign to carry the LHS
    // Option<T> inner into the RHS emitter. Read by branch-arm guards
    // (IfExpr, IfBlockExpr, CaseCondExpr, and CaseExpr scrutinee path) as a
    // fallback when computeBranchOptionInnerHint or scrutinee-derived hints
    // are unavailable.
    // None()/NoneExpr emitters do NOT read this field directly, so it cannot
    // leak into arbitrary sub-expressions (e.g., function arguments).
    llvm::Type *option_decl_annotation_inner_ = nullptr;

    // RAII guard: saves and restores option_none_hint_inner_ so nested
    // branches have independent scopes.
    struct OptionNoneHintGuard {
        CodeGen &cg_;
        llvm::Type *saved_;
        explicit OptionNoneHintGuard(CodeGen &cg, llvm::Type *hint)
            : cg_(cg), saved_(cg.option_none_hint_inner_) {
            cg_.option_none_hint_inner_ = hint;
        }
        ~OptionNoneHintGuard() { cg_.option_none_hint_inner_ = saved_; }
        OptionNoneHintGuard(const OptionNoneHintGuard &) = delete;
        OptionNoneHintGuard &operator=(const OptionNoneHintGuard &) = delete;
    };

    // RAII guard: saves and restores option_decl_annotation_inner_.
    struct DeclAnnotationInnerGuard {
        CodeGen &cg_;
        llvm::Type *saved_;
        explicit DeclAnnotationInnerGuard(CodeGen &cg, llvm::Type *inner)
            : cg_(cg), saved_(cg.option_decl_annotation_inner_) {
            cg_.option_decl_annotation_inner_ = inner;
        }
        ~DeclAnnotationInnerGuard() { cg_.option_decl_annotation_inner_ = saved_; }
        DeclAnnotationInnerGuard(const DeclAnnotationInnerGuard &) = delete;
        DeclAnnotationInnerGuard &operator=(const DeclAnnotationInnerGuard &) = delete;
    };

    // Compute an Option inner-type hint from a pair of branch-arm expressions.
    // Returns nullptr when no concrete inner type can be derived
    // (e.g., non-Option arms or placeholder-only outcomes).
    llvm::Type *computeBranchOptionInnerHint(const ExprNode &a, const ExprNode &b);

    // RAII guard that increments parallel_for_depth_ on construction and
    // decrements on destruction (including during stack unwinding from a
    // codegenError thrown inside the thunk body).
    struct ParallelForScope {
        CodeGen &cg_;
        explicit ParallelForScope(CodeGen &cg) : cg_(cg) { ++cg_.parallel_for_depth_; }
        ~ParallelForScope() { --cg_.parallel_for_depth_; }
        ParallelForScope(const ParallelForScope &) = delete;
        ParallelForScope &operator=(const ParallelForScope &) = delete;
    };
    std::unordered_set<llvm::AllocaInst*> arc_managed_vars_; // allocas holding ARC-managed ptrs
    std::unordered_set<llvm::Value*> arc_owned_values_;  // values produced by emitArcAlloc (data ptrs)
    std::unordered_set<llvm::AllocaInst*> arc_backed_vars_; // allocas that hold ARC-allocated collections (have ARC header)
    std::unordered_set<llvm::AllocaInst*> weak_managed_vars_; // allocas holding weak ref ptrs (header ptrs)
    std::unordered_map<llvm::AllocaInst*, std::string> weak_inner_type_names_; // inner type name for upgrade
    std::unordered_map<llvm::AllocaInst*, ResourceKind> resource_managed_vars_;
    std::unordered_set<llvm::AllocaInst*> closure_managed_vars_; // allocas holding ARC-managed closures
    // ARC-managed str tracking: str header is at handle-STRING_HEADER_SIZE (24), not handle-ARC_HEADER_SIZE (16)
    std::unordered_set<llvm::AllocaInst*> arc_str_managed_vars_; // allocas holding str handles
    std::unordered_set<llvm::Value*>      arc_str_owned_values_;  // str handles produced by makeString/runtime

    // ARC emit methods
    llvm::Value *emitArcAlloc(llvm::Value *dataSize);
    void emitArcRetain(llvm::Value *headerPtr, bool atomic = false);
    void emitArcRelease(llvm::Value *headerPtr, bool atomic = false,
                        llvm::FunctionCallee destructor = {},
                        llvm::Function *gcVisitFn = nullptr);
    llvm::Value *emitArcGetDataPtr(llvm::Value *headerPtr);
    llvm::Value *emitArcGetHeaderFromData(llvm::Value *dataPtr);
    // String variant: subtracts STRING_HEADER_SIZE (24) instead of ARC_HEADER_SIZE (16).
    // Must be used whenever the data pointer is a Ry str (StringHeader-prefixed).
    llvm::Value *emitStrGetHeaderFromData(llvm::Value *strHandle);
    // String variant: adds STRING_HEADER_SIZE (24) back to get the str handle.
    llvm::Value *emitStrGetDataPtr(llvm::Value *strHeaderPtr);
    // Dispatch helper: uses emitStrGetHeaderFromData when srcAlloca is in
    // arc_str_managed_vars_; falls back to emitArcGetHeaderFromData otherwise.
    // Prefer this over inline ternaries at alloca-origin retain/release sites.
    llvm::Value *emitArcHeaderForAlloca(llvm::Value *handle, llvm::AllocaInst *srcAlloca);
    llvm::Value *emitArcAllocCollectionHeader(llvm::Type *headerTy);
    // Emit a load of the byte_len field from a StringHeader handle.
    // `handle` must be a Ry str value (StringHeader data pointer).
    // Returns an i64 value representing the byte length of the string.
    llvm::Value *emitStringByteLen(llvm::Value *handle);
    // Aligned i64 load at `ptr` with the given atomic ordering (or plain
    // load when ordering == NotAtomic). Used for ARC strong_count reads
    // that race with atomicrmw retain/release (#630).
    llvm::LoadInst *emitAtomicI64Load(llvm::Value *ptr,
                                      llvm::AtomicOrdering ordering,
                                      const llvm::Twine &name);
    bool isArcAtomic(llvm::Value *val) const;
    void markArcAtomic(llvm::Value *val);
    void markArcManaged(llvm::AllocaInst *alloca);
    bool isArcManaged(llvm::AllocaInst *alloca) const;
    void emitScopeCleanup();
    void emitScopeCleanupToDepth(size_t targetDepth);
    llvm::FunctionCallee resolveCollectionDestructor(llvm::AllocaInst *alloca);
    void emitArcReleaseVar(const std::string &name, llvm::AllocaInst *alloca);
    bool tryRetainArcSource(llvm::Value *val);
    // Unconditional retain for a pointer-typed ARC value: tries the
    // `tryRetainArcSource` fast path (LoadInst-from-alloca and arc_owned
    // values) and falls back to a header-based retain for any other
    // source (e.g. a Load from a GEP — cross-slot reads like `xs[i]`).
    // Required to keep the strong count balanced when transferring an
    // ARC pointer into a slot that owns its element (#855).
    void retainArcValue(llvm::Value *val);

    // Collection type name predicates
    static bool isListTypeName(const std::string &typeName);
    static bool isMapTypeName(const std::string &typeName);
    static bool isSetTypeName(const std::string &typeName);
    static bool isCollectionTypeName(const std::string &typeName);

    // ======== Weak References ========
    static bool isWeakTypeName(const std::string &typeName);
    static std::string weakInnerTypeName(const std::string &typeName);
    void markWeakManaged(llvm::AllocaInst *alloca);
    bool isWeakManaged(llvm::AllocaInst *alloca) const;
    void emitWeakRetain(llvm::Value *headerPtr);
    void emitWeakRelease(llvm::Value *headerPtr);
    llvm::Value *emitWeakUpgrade(llvm::Value *headerPtr, const std::string &innerTypeName);
    void emitWeakReleaseVar(const std::string &name, llvm::AllocaInst *alloca);

    // ======== Copy-on-Write & Destructors ========
    enum class CollectionKind { List, Map, Set, Str };
    llvm::FunctionCallee getOrCreateCollectionDestructor(CollectionKind kind,
                                                          const std::string &elemSig = "",
                                                          const std::string &valSig = "");

    // Splits a generic type name like "List<str>" into head="List" and
    // inner=["str"].  Returns false for non-generic names (no '<').
    // Extracted from codegen_fn_generic.cpp so callers across translation
    // units can use the same parsing logic.
    static bool splitGenericTypeName(const std::string &s,
                                     std::string &head,
                                     std::vector<std::string> &inner);

    // Predicate: does the container's *element* type itself own an
    // ARC-managed allocation that needs releasing on slot overwrite?
    // Returns true only for non-weak nested collection element types
    // (`List<List<T>>`, `Map<K, List<V>>`, `List<Set<T>>`, …). Records
    // and record fields with ARC fields are intentionally excluded —
    // they have the same root cause but a different fix path.
    // `containerKind` selects which metadata field to consult; on a
    // true return, `*outElemKind` (if non-null) is set to the inner
    // collection's kind so the caller can resolve a destructor.
    bool elementTypeIsArcManaged(llvm::Value *containerPtr,
                                 CollectionKind containerKind,
                                 CollectionKind *outElemKind = nullptr);

    // Core type-name predicate shared by `elementTypeIsArcManaged` and
    // the `FieldAssignStmt` write path. Returns true only for non-weak
    // List/Map/Set type names — the same subset that owns an ARC
    // allocation per slot. Callers should pass either a declared
    // `FieldDef.type->toString()` or a container metadata element type
    // name; the predicate resolves type aliases at entry, so fields
    // declared via `type Ints = List<int>` are classified correctly.
    // (#855 / #857)
    bool fieldTypeIsArcManaged(const std::string &fieldTypeName,
                                CollectionKind *outFieldKind = nullptr);

    // Returns true if the record type has at least one ARC-managed field
    // (List/Map/Set, non-weak). Caller must pass a record type registered
    // in `record_types_`. Used to drive retain/release of record ARC
    // fields on copy and scope exit (#854 Layer 2).
    bool recordHasArcFields(llvm::StructType *st);

    // Retains each ARC-managed field of an in-flight record SSA value.
    // Emits null-guarded `emitArcRetain` on each field's header pointer.
    // Used when copying a record value into a new variable so both
    // aliases share ownership of the inner ARC containers (#854 Layer
    // 2). Without this, a subsequent path-CoW through one alias would
    // see strong_count==1 and mutate in place, breaking isolation from
    // the other alias.
    void emitRecordArcFieldsRetain(llvm::Value *recordVal,
                                     llvm::StructType *st);

    // Releases each ARC-managed field of an in-flight record SSA value.
    // Symmetric to emitRecordArcFieldsRetain. Used on record scope exit
    // and record re-assignment (#854 Layer 2).
    void emitRecordArcFieldsRelease(llvm::Value *recordVal,
                                      llvm::StructType *st);

    // Allocas holding records whose type has at least one ARC-managed
    // field. Populated at declaration time so `emitScopeCleanupToDepth`
    // can walk and release the ARC fields before the record dies.
    std::unordered_set<llvm::AllocaInst*> arc_field_record_vars_;

    // Releases an already-loaded ARC element pointer with a null guard
    // and CFG branching. The builder's insertion point is left at the
    // join block on return so the caller can continue emitting the
    // store. Used by IndexAssignStmt to release the previously-stored
    // ARC element before overwriting a list/map slot (#855).
    // `elemTypeName` is the declared type of the element (e.g. "List<str>")
    // and is used to select the specialized destructor that releases inner
    // str handles (#1108). Pass "" when the type name is not available;
    // the function falls back to the generic destructor.
    void emitArcReleaseLoadedElement(llvm::Value *oldElemVal,
                                     CollectionKind elemKind,
                                     const std::string &elemTypeName,
                                     const llvm::Twine &label);

    llvm::AllocaInst *tryGetReceiverAlloca(const ExprNode &expr);
    llvm::Value *emitCowCheck(llvm::Value *dataPtr, llvm::AllocaInst *alloca, CollectionKind kind);
    // Generalized CoW: `slotPtr` is any pointer to the storage cell holding
    // the container data pointer (alloca, module-global storage, record
    // field slot, or heap container slot). When `retainElements` is true
    // AND the container's element type is ARC-managed, each element pointer
    // in the cloned buffer is retained so the clone shares ownership of
    // nested ARC state with the original. Used by path CoW (#854) to walk
    // chained LHS hops and privatize each level whose refcount > 1.
    llvm::Value *emitCowCheckSlot(llvm::Value *dataPtr, llvm::Value *slotPtr,
                                    CollectionKind kind, bool retainElements);
    // Walk a chained LHS `object` expression (typically `s.object` from
    // IndexAssignStmt / FieldAssignStmt) inside-out, privatize every
    // container level in the chain, and return the privatized leaf
    // container pointer. For IndexExpr and FieldAccessExpr bases the walk
    // recursively descends until it reaches a VariableExpr root. For
    // VariableExpr it is a 1-hop privatization equivalent to the existing
    // trivial emitCowCheck path (but with retainElements=true). Used to
    // implement deep (path) copy-on-write for nested collection writes
    // through aliases (#854).
    llvm::Value *emitPathCowForChain(ExprNode &chain);
    llvm::Value *emitCowDeepCopyList(llvm::Value *oldDataPtr, llvm::Type *elemTy);
    llvm::Value *emitCowDeepCopyMap(llvm::Value *oldDataPtr, llvm::Type *keyTy, llvm::Type *valTy);
    llvm::Value *emitCowDeepCopySet(llvm::Value *oldDataPtr, llvm::Type *elemTy);
    void emitCowRetainArcElements(llvm::Value *buf, llvm::Value *len, const std::string &tag,
                                   CollectionKind elemArcKind = CollectionKind::List);
    std::map<std::tuple<CollectionKind, std::string, std::string>,
             llvm::FunctionCallee> arc_destructors_cache_;
    llvm::FunctionCallee getOrCreateResourceDestructor(int rk);
    std::map<int, llvm::FunctionCallee> resource_destructors_cache_;
    llvm::FunctionCallee resolveDestructor(llvm::AllocaInst *alloca);
    int detectResourceKind(llvm::Value *val);
    void nullifyResourceVar(const ExprNode &argExpr);
    llvm::Value *emitResourceFree(llvm::Value *dataPtr, int rk,
                                   const ExprNode &argExpr);

    // ======== Scope & Variable Management ========
    std::unordered_map<std::string, llvm::Constant*> global_string_cache_;
    std::unordered_map<std::string, llvm::Constant*> regex_global_cache_;
    llvm::Constant *buildArcGlobal(const std::string &str, const llvm::Twine &name,
                                    std::unordered_map<std::string, llvm::Constant*> &cache);
    llvm::Constant *cachedGlobalString(const std::string &str, const llvm::Twine &name = "");
    std::vector<std::unordered_map<std::string, llvm::AllocaInst*>> scope_stack_;
    std::vector<std::unordered_set<std::string>> immutable_scope_stack_;
    llvm::SmallPtrSet<llvm::AllocaInst*, 8> captured_vars_; // reject reassignment of captured vars inside closure body
    std::vector<std::vector<llvm::Value*>> iterator_malloc_stack_; // per-scope iterator malloc tracking

    // ======== Module-level bindings (#817) ========
    // Top-level `let` and `@const` declarations are stored as alloca in
    // __ry_main__'s entry block (unchanged). To make them accessible from any
    // top-level function, we also create a module-level GlobalVariable<ptr>
    // that holds the address of the alloca ("pointer trampoline"). Other
    // functions load this trampoline to obtain the storage pointer, then
    // load/store the value through it.
    //
    // Registered from emitStmt(AssignStmt&) right after emitVarDecl, only
    // when isTopLevelContext() was true at that point. FnScope does NOT
    // save/restore this map — it is module-lifetime state.
    struct ModuleBinding {
        llvm::GlobalVariable *gv_ptr;      // GlobalVariable<ptr> holding &original_alloca
        llvm::AllocaInst *original_alloca; // the real storage in __ry_main__; also the metadata anchor
        bool is_immutable;                 // true for @const
        // Lifetime-management classification captured at registration time
        // (i.e. while still in __ry_main__ context where the per-function
        // tracking sets are populated). FnScope clears these sets when a
        // top-level function begins, so checks that run inside a function
        // body must consult these cached flags rather than calling
        // isWeakManaged()/isArcManaged()/resource_managed_vars_ directly.
        bool is_weak = false;
        bool is_arc_managed = false;
        bool is_arc_atomic = false;
        bool is_resource = false;
        bool is_str = false; // str uses offset -24 (STRING_HEADER_SIZE), not -16
        // Destructor resolved at registration time; may be empty for values
        // that do not need one.
        llvm::FunctionCallee destructor{};
        llvm::Type *valueTy() const { return original_alloca->getAllocatedType(); }
    };
    std::unordered_map<std::string, ModuleBinding> module_globals_;

    bool isTopLevelContext() const {
        return fn_nesting_depth_ == 0 && scope_stack_.size() == 1;
    }
    const ModuleBinding *findModuleGlobal(const std::string &name) const;
    llvm::Value *loadModuleGlobalStorage(const ModuleBinding &b, const std::string &name);
    void registerModuleGlobal(const std::string &name, llvm::AllocaInst *alloca,
                              bool is_immutable);
    void emitModuleGlobalWriteThrough(const ModuleBinding &b, AssignStmt &s);

    // Side-table: storage pointers produced by `loadModuleGlobalStorage` for
    // fixed-length array module globals. Maps the LoadInst result to the
    // original alloca so consumers like `emitExprVariant(IndexExpr)` that
    // historically switched on `dyn_cast<AllocaInst>` can resolve back to the
    // alloca (and its `ArrayType`/`array_elem_type_names_` metadata) when the
    // value comes through the module-global trampoline (#817).
    std::unordered_map<llvm::Value*, llvm::AllocaInst*> array_storage_to_alloca_;

    // ======== Closure Capture ARC Kinds ========
    // Determines which destructor to use when releasing a captured value.
    // Defined here (before OverloadEntry) so nested-function capture metadata
    // can reference it.
    enum class CapturedArcKind {
        None,       // not ARC-managed
        List,
        Map,
        Set,
        Str,        // str handle — uses STRING_HEADER_SIZE (24) offset, no sub-destructor
        Closure,
        Resource,   // generic resource (destructor not tracked per-capture)
        Generic,    // ARC-managed but no sub-destructor (e.g., f-strings)
    };

    // ======== Function Overloads & Dispatch ========
    struct FnTypeInfo;  // forward declaration for capturedClosureInfos
    struct OverloadEntry {
        llvm::Function *func;
        std::vector<llvm::Type*> paramTypes;
        std::vector<std::string> paramNames;
        std::vector<std::string> paramTypeNames;
        std::string returnTypeName;
        size_t minArity = 0;
        std::vector<ExprPtr> defaultValues;
        const std::vector<ExprPtr> *preconditions = nullptr;
        const std::vector<ExprPtr> *postconditions = nullptr;
        const std::vector<std::string> *ensureBindings = nullptr;
        // Capture metadata for nested functions with closures (empty = no captures)
        std::vector<std::string> capturedNames;
        std::vector<llvm::Type*> capturedTypes;
        std::vector<CapturedArcKind> capturedArcKinds;
        std::vector<ResourceKind> capturedResourceKinds;
        std::unique_ptr<std::unordered_map<size_t, FnTypeInfo>> capturedClosureInfos;
    };
    std::unordered_map<std::string, std::vector<OverloadEntry>> functions_;
    std::unordered_set<llvm::Function*> forward_declared_fns_;
    // Lexical scope stack for nested named functions (parallel to scope_stack_)
    std::vector<std::unordered_map<std::string, std::vector<OverloadEntry>>> fn_scope_stack_;
    int nested_fn_counter_ = 0;   // monotonic counter for unique IR names
    int fn_nesting_depth_ = 0;    // 0 = top-level, incremented per FnScope
    std::vector<OverloadEntry> *findFunction(const std::string &name);
    void forwardDeclareFunctionsInBody(std::vector<StmtNode> &stmts, bool validateOperatorReturn);
    void forwardDeclareNestedFunctions(std::vector<StmtNode> &body);
    using BuiltinFn = std::function<void(const std::vector<ExprPtr>&, const std::vector<NamedArg>&)>;
    std::unordered_map<std::string, BuiltinFn> builtins_;

    // ======== Type System (Records, Enums, Unions, Generics) ========
    struct RecordInfo {
        llvm::StructType *llvmType;
        std::vector<FieldDef> fields;
        std::vector<ExprPtr> invariants;
        std::string parentName;
        int64_t type_id = -1;   // unique identity for type_of

        // Linear scan for a field by name. Returns the field index, or -1
        // if the name is not present.
        int findField(const std::string &name) const {
            for (unsigned i = 0; i < fields.size(); ++i)
                if (fields[i].name == name)
                    return static_cast<int>(i);
            return -1;
        }
    };
    std::unordered_map<std::string, RecordInfo> record_types_;

    // Type ID registry for type_of builtin.
    // Each distinct type definition gets a unique id, allowing identity-based
    // equality on values returned by type_of (rather than name-based comparison).
    int64_t next_type_id_ = 0;
    std::unordered_map<std::string, int64_t> canonical_type_ids_;
    int64_t getOrAllocateCanonicalTypeId(const std::string &canonicalName);
    std::unordered_map<std::string, std::string> type_aliases_;
    std::unordered_map<llvm::Type*, llvm::StructType*> option_types_;
    std::map<std::pair<llvm::Type*, llvm::Type*>, llvm::StructType*> result_types_;
    enum class TypeMeta {
        ListElem, MapKey, MapValue, SetElem,
        NestedListElem, TaskResult, ThreadResult, IteratorElem, COUNT
    };


    // Fixed-length array element type names (e.g., "i32", "u8")
    // elementType and size are derivable from AllocaInst->getAllocatedType()
    std::unordered_map<llvm::AllocaInst*, std::string> array_elem_type_names_;

    int iterator_fn_counter_ = 0;

    struct UnionTypeInfo {
        llvm::StructType *llvmType;
        std::vector<std::string> componentNames;
        std::vector<llvm::Type*> componentTypes;
    };
    std::unordered_map<std::string, UnionTypeInfo> union_type_info_;
    std::string current_fn_return_type_;

    struct VariantFieldInfo {
        std::vector<llvm::Type*> fieldTypes;
        std::vector<std::string> fieldTypeNames;
    };
    struct EnumInfo {
        std::string name;
        std::unordered_map<std::string, int64_t> variants;
        std::vector<std::string> variantOrder;
        llvm::GlobalVariable *nameArray;
        size_t variantCount;
        bool isADT = false;
        bool hasExplicitValues = false;
        llvm::StructType *adtType = nullptr;   // { i64 tag, [N x i8] payload }
        size_t maxPayloadSize = 0;
        std::unordered_map<std::string, VariantFieldInfo> variantFields;
        int64_t type_id = -1;   // unique identity for type_of
    };
    std::unordered_map<std::string, EnumInfo> enum_types_;
    EnumVariantRegistry buildEnumVariantRegistry() const;
    std::string findAdtEnumName(llvm::StructType *st) const;
    std::string findRecordTypeName(llvm::StructType *st) const;
    llvm::Function *createAdtVisitFunction(const std::string &typeName, const EnumInfo &info);
    llvm::Function *createRecordVisitFunction(const std::string &typeName, const RecordInfo &info);
    void emitGcVisitField(llvm::Value *fieldPtr, llvm::Type *fieldTy,
                          const std::string &fieldTypeName,
                          llvm::Value *visitorFn,
                          llvm::FunctionType *visitorCallTy,
                          llvm::FunctionType *visitFnTy,
                          llvm::Function *parentFn);

    struct GenericEnumTemplate {
        std::string name;
        std::vector<TypeParam> typeParams;
        std::vector<EnumVariant> variants;
    };
    std::unordered_map<std::string, GenericEnumTemplate> generic_enum_templates_;
    void instantiateGenericEnum(const std::string &fullName, const std::string &baseName,
                                const std::vector<std::string> &typeArgs);
    // Ensure a concrete or generic enum named `typeName` is registered in
    // `enum_types_`. If `typeName` is a generic enum instantiation like
    // `Option<int>` not yet instantiated, parse the base/args and call
    // `instantiateGenericEnum`. Returns true iff `enum_types_.count(typeName)`
    // after the call. Used by enum construction sites and metadata propagation.
    bool ensureEnumInstantiated(const std::string &typeName);
    // Strip ASCII spaces from both ends of a type-name substring. Shared by
    // the comma-separated annotation parsers (Map key/value, tuple destructure).
    static std::string trimTypeNameSpaces(const std::string &s);
    // Return the best source-level element type name for a list value.
    // Used by enumerate()/zip() to build the tuple `list_elem_type_name`.
    std::string snapshotListElemName(llvm::Value *listVal, llvm::Type *elemTy);

    // Desugar a Ry `str` value into a `List<str>` of UTF-8 code points by
    // calling `__ry_split_chars` at codegen time. Returns the new list value
    // already tagged with `TypeMeta::ListElem = ptrTy_` and
    // `list_elem_type_name = "str"`. Shared by `for c in s:` iteration and
    // by `enumerate(s)` / `zip(s, ...)` (#746, #827).
    llvm::Value *emitStringToCharList(llvm::Value *s, const char *label);

    // Reject `name` if it is already registered under a different type kind
    // (record, enum, generic enum). Callers must check same-kind duplicates
    // themselves so they can emit a caller-specific error.
    void rejectIfTypeNameTakenByOtherKind(const std::string &name);

    // Generic function templates
    struct GenericFnTemplate {
        std::unique_ptr<FnStmt> fnStmt;
    };
    std::unordered_map<std::string, GenericFnTemplate> generic_fn_templates_;
    std::unordered_set<std::string> generic_fn_instantiated_;
    std::unordered_map<std::string, std::string> type_param_scope_;

    void instantiateGenericFn(const std::string &baseName,
                              const std::vector<std::string> &typeArgs);
    void validateTypeBounds(const std::vector<TypeParam> &typeParams,
                            const std::vector<std::string> &typeArgs,
                            const std::string &context);
    std::vector<std::string> inferTypeArgs(const std::string &baseName,
                                           const std::vector<ExprPtr> &args);

    // Recursively unify the declared parameter type (an AST TypeNode,
    // which may reference type parameters like `T`) against the
    // inferred argument type-name string (e.g., `"List<int>"`). On
    // success, records bindings in `inferred`. Returns false when the
    // argument side lacks information (e.g., empty string) so the
    // caller can continue with other arguments. Emits codegenError on
    // shape mismatches or conflicting bindings.
    bool unifyTypeParam(const TypeNode &paramType,
                        const std::string &argTypeName,
                        const std::unordered_set<std::string> &typeParamSet,
                        std::unordered_map<std::string, std::string> &inferred,
                        const std::string &fnName);

    // Merge a new binding into `inferred`; emit a clear conflict error
    // if the name is already bound to a different resolved type.
    void mergeInferredBinding(std::unordered_map<std::string, std::string> &inferred,
                              const std::string &paramName,
                              const std::string &resolved,
                              const std::string &fnName);

    std::string reverseResolveType(llvm::Value *val);

    // ======== Closure & Lambda Support ========
    // Function type info for indirect calls (lambda / function pointers)
    struct FnTypeInfo {
        std::vector<llvm::Type*> paramTypes;
        std::vector<std::string> paramTypeNames;
        llvm::Type *returnType;
        std::vector<std::string> capturedVars;   // for closure support
        std::vector<llvm::Type*> capturedTypes;  // types of captured variables
        std::vector<CapturedArcKind> capturedArcKinds; // ARC kind per captured variable
        std::vector<ResourceKind> capturedResourceKinds; // per-capture ResourceKind (RK_COUNT if N/A)
        // unique_ptr to break incomplete-type cycle (GCC 11 requires complete type for unordered_map value)
        std::unique_ptr<std::unordered_map<size_t, FnTypeInfo>> capturedClosureInfos;
        std::unique_ptr<FnTypeInfo> returnFnTypeInfo;  // nested function return type info
        std::string returnTypeName;  // e.g. "List<int>", "Map<str, int>" for metadata propagation
        bool isUniformClosure = false;  // true when value uses {thunk, env} layout
        llvm::Function *sourceFn = nullptr;  // underlying LLVM function (for thunk generation)

        FnTypeInfo() = default;
        ~FnTypeInfo() = default;
        FnTypeInfo(FnTypeInfo&&) = default;
        FnTypeInfo& operator=(FnTypeInfo&&) = default;
        FnTypeInfo(const FnTypeInfo &o)
            : paramTypes(o.paramTypes), paramTypeNames(o.paramTypeNames),
              returnType(o.returnType), capturedVars(o.capturedVars),
              capturedTypes(o.capturedTypes), capturedArcKinds(o.capturedArcKinds),
              capturedResourceKinds(o.capturedResourceKinds),
              capturedClosureInfos(o.capturedClosureInfos
                  ? std::make_unique<std::unordered_map<size_t, FnTypeInfo>>(*o.capturedClosureInfos)
                  : nullptr),
              returnFnTypeInfo(o.returnFnTypeInfo
                  ? std::make_unique<FnTypeInfo>(*o.returnFnTypeInfo)
                  : nullptr),
              returnTypeName(o.returnTypeName),
              isUniformClosure(o.isUniformClosure),
              sourceFn(o.sourceFn) {}
        FnTypeInfo& operator=(const FnTypeInfo &o) {
            if (this != &o) {
                paramTypes = o.paramTypes;
                paramTypeNames = o.paramTypeNames;
                returnType = o.returnType;
                capturedVars = o.capturedVars;
                capturedTypes = o.capturedTypes;
                capturedArcKinds = o.capturedArcKinds;
                capturedResourceKinds = o.capturedResourceKinds;
                capturedClosureInfos = o.capturedClosureInfos
                    ? std::make_unique<std::unordered_map<size_t, FnTypeInfo>>(*o.capturedClosureInfos)
                    : nullptr;
                returnFnTypeInfo = o.returnFnTypeInfo
                    ? std::make_unique<FnTypeInfo>(*o.returnFnTypeInfo)
                    : nullptr;
                returnTypeName = o.returnTypeName;
                isUniformClosure = o.isUniformClosure;
                sourceFn = o.sourceFn;
            }
            return *this;
        }
    };
    FnTypeInfo *lookupFnTypeInfo(llvm::Value *val);
    std::unordered_map<llvm::Function*, FnTypeInfo> return_fn_type_info_;
    llvm::FunctionCallee getOrCreateClosureDestructor(const FnTypeInfo &info);

    // Uniform closure support: {thunk_ptr, env_ptr, env_dtor_ptr} for
    // function-type boundaries. `thunk_ptr` is a forwarding or capturing thunk
    // that adapts the callee to the uniform ABI; `env_ptr` is the ARC-managed
    // captured environment (null for non-capturing functions); `env_dtor_ptr`
    // is the destructor used to release `env_ptr` when the closure is dropped
    // (null for non-capturing functions). Layout mirrored in
    // `getOrCreateUniformClosureDestructor` in src/codegen_lambda.cpp.
    static bool isFunctionTypeName(const std::string &s) {
        return s.size() > 9 && s.compare(0, 9, "function(") == 0;
    }
    llvm::StructType *getUniformClosureTy() {
        if (!uniformClosureTy_)
            uniformClosureTy_ = llvm::StructType::get(*ctx_, {ptrTy_, ptrTy_, ptrTy_});
        return uniformClosureTy_;
    }
    llvm::StructType *uniformClosureTy_ = nullptr;
    std::unordered_map<llvm::Function*, llvm::Function*> forwarding_thunk_cache_;
    std::unordered_map<llvm::Function*, llvm::Function*> capturing_thunk_cache_;
    llvm::Function *uniform_closure_dtor_ = nullptr;
    llvm::Function *getOrCreateForwardingThunk(llvm::Function *realFn, const FnTypeInfo &info);
    llvm::Function *getOrCreateCapturingThunk(llvm::Function *realFn, const FnTypeInfo &info);
    llvm::Value *wrapAsUniformClosure(llvm::Value *val, const FnTypeInfo &info);
    llvm::Function *getOrCreateUniformClosureDestructor();
    std::vector<llvm::Value*> wrapFnTypedArgs(
        std::vector<llvm::Value*> &argVals,
        const std::vector<std::string> &paramTypeNames);
    void releaseUniformClosureTemps(const std::vector<llvm::Value*> &temps);
    // Nested closure shape for cache key differentiation
    struct NestedClosureShape {
        size_t index;
        std::vector<CapturedArcKind> arcKinds;
        std::vector<llvm::Type*> types;
        std::vector<ResourceKind> resourceKinds;
        bool operator<(const NestedClosureShape &o) const {
            if (index != o.index) return index < o.index;
            if (arcKinds != o.arcKinds) return arcKinds < o.arcKinds;
            if (types.size() != o.types.size()) return types.size() < o.types.size();
            for (size_t i = 0; i < types.size(); ++i) {
                if (types[i] != o.types[i])
                    return std::less<llvm::Type*>{}(types[i], o.types[i]);
            }
            return resourceKinds < o.resourceKinds;
        }
    };
    struct ClosureDtorKey {
        std::vector<CapturedArcKind> arcKinds;
        std::vector<llvm::Type*> types;
        std::vector<ResourceKind> resourceKinds;
        std::vector<NestedClosureShape> nestedShapes;
        bool operator<(const ClosureDtorKey &o) const {
            if (arcKinds != o.arcKinds) return arcKinds < o.arcKinds;
            if (types.size() != o.types.size()) return types.size() < o.types.size();
            for (size_t i = 0; i < types.size(); ++i) {
                if (types[i] != o.types[i])
                    return std::less<llvm::Type*>{}(types[i], o.types[i]);
            }
            if (resourceKinds != o.resourceKinds) return resourceKinds < o.resourceKinds;
            return nestedShapes < o.nestedShapes;
        }
    };
    std::map<ClosureDtorKey, llvm::FunctionCallee> closure_destructors_cache_;
    CapturedArcKind detectCapturedArcKind(llvm::AllocaInst *alloca) const;

    // Free-variable analysis result — shared between lambda and nested named function codegen
    struct CaptureAnalysisResult {
        std::vector<std::string> capturedNames;
        std::vector<llvm::Value*> capturedValues;
        std::vector<llvm::Type*> capturedTypes;
        std::vector<CapturedArcKind> capturedArcKinds;
        std::vector<ResourceKind> capturedResourceKinds;
        std::unordered_map<size_t, FnTypeInfo> capturedClosureInfos;
        llvm::SmallVector<bool, 8> capturedIsConst;
    };
    CaptureAnalysisResult analyzeFreeVariables(
        const std::vector<StmtNode> &body,
        const ExprPtr &expr_body,
        const std::unordered_set<std::string> &paramNames,
        bool emitLoads = true);

    // Build ARC-managed closure struct {fn_ptr, cap1, cap2, ...} and return the closure pointer.
    // Returns the raw function pointer if capturedValues is empty.
    llvm::Value *buildClosureStruct(
        llvm::Function *func,
        const FnTypeInfo &info,
        const std::vector<llvm::Value*> &capturedValues);

    int lambda_counter_ = 0;
    int for_snap_counter_ = 0;   // monotonic counter for unique __for_iter_snap names (#1021)
    bool test_mode_ = false;
    bool outline_mode_ = false;
    int outline_depth_ = 0;
    bool coverage_mode_ = false;
    int coverage_file_id_offset_ = 0;
    int test_fn_counter_ = 0;
    const SourceManager *sm_ = nullptr;
    SourceLocation current_loc_;
    std::string current_function_name_;
    std::unordered_set<int64_t> registered_coverage_lines_;

    // ======== Coverage & Tracing ========
    void emitCoverage(const SourceLocation &loc);
    void emitTraceSymbolDefine(const std::string &kind, const std::string &name,
                               const SourceLocation &loc);
    llvm::Value *emitTraceSourceString(const std::string &text);
    llvm::Value *emitTraceFileString(const SourceLocation &loc);
    void emitTraceFunctionEnter(const std::string &fnName, const SourceLocation &loc);
    void emitTraceFunctionExit(const std::string &fnName, const SourceLocation &loc);
    void emitTraceReturn(const SourceLocation &loc);
    void emitTraceIfBranch(llvm::Value *cond, const SourceLocation &loc);
    void emitTraceWhenBranch(int armIndex, const SourceLocation &loc);

    // ======== Contract (Design by Contract) ========
    std::vector<ExprPtr> *current_postconditions_ = nullptr;
    std::vector<std::string> *ensure_bindings_ = nullptr;
    bool in_ensure_context_ = false;
    int contract_err_counter_ = 0;

    // Directive support
    std::unordered_set<std::string> deprecated_functions_;
    std::unordered_set<std::string> deprecated_types_;
    std::unordered_set<std::string> deprecated_variables_;
    std::unordered_set<std::string> deprecated_fields_;  // "TypeName.fieldName"
    std::vector<std::string> warnings_;
    std::unordered_set<std::string> warned_call_deprecations_;

    void emitDeprecationWarning(const std::string &name);

    // @native let constants
    std::unordered_set<std::string> native_constants_;
    static bool isNativeConstant(const std::string &name);
    llvm::Value *emitNativeConstant(const std::string &name);

    // @native fn rich signature registry
    std::unordered_map<std::string, std::vector<NativeFnSignature>> native_fn_sigs_;

    // Libraries actually used during codegen (populated by dispatch functions).
    // Only these libraries need to be loaded at JIT startup.
    std::unordered_set<std::string> used_native_libraries_;

    // Secondary index: fn_name → library names, for @native("libname") functions.
    // Enables O(1) lookup in emitGenericNativeCall instead of scanning all sigs.
    // Multiple libraries may declare functions with the same name.
    std::unordered_map<std::string, std::vector<std::string>> native_lib_index_;

    // Derive package name from the source file path of a native function declaration.
    // e.g. "share/std/base64/base64.ry" → "base64", "share/std/builtins.ry" → ""
    std::string deriveNativePackage(const SourceLocation &loc) const;

    // Literal/range type constraints
    struct TypeConstraint {
        enum class Kind { IntLiteral, StrLiteral, IntRange };
        Kind kind;
        std::vector<int64_t> int_values;    // for IntLiteral
        std::vector<std::string> str_values; // for StrLiteral
        int64_t range_low = 0, range_high = 0; // for IntRange
    };
    // ======== Unified Value Metadata ========
    // All per-Value* metadata consolidated into a single struct.
    // Value* keys are LLVM SSA values, unique per function — FnScope does NOT
    // need to save/restore this map because nested function compilation uses
    // distinct Value* pointers that never collide with the outer function's.
    struct ValueMetadata {
        // Collection element types
        llvm::Type *list_elem = nullptr;
        llvm::Type *map_key = nullptr;
        llvm::Type *map_value = nullptr;
        llvm::Type *set_elem = nullptr;
        llvm::Type *nested_list_elem = nullptr;
        llvm::Type *task_result = nullptr;
        llvm::Type *thread_result = nullptr;
        llvm::Type *iterator_elem = nullptr;

        // String-typed metadata
        std::string low_level_type_name;
        std::string map_key_type_name;      // Ry type name for map keys (#813)
        std::string map_value_type_name;    // Ry type name for map values
        std::string list_elem_type_name;    // Ry type name for list elements (e.g. "Map<str, int>")
        std::string set_elem_type_name;     // Ry type name for set elements (e.g. "List<int>")
        std::string union_value_type;       // normalized union type name
        std::string enum_value_type;        // enum type name

        // Closure/function type info for collection elements
        std::optional<FnTypeInfo> list_elem_fn_type_info;
        std::optional<FnTypeInfo> map_key_fn_type_info;
        std::optional<FnTypeInfo> map_value_fn_type_info;
        std::optional<FnTypeInfo> set_elem_fn_type_info;

        // Closure/function type info
        std::optional<FnTypeInfo> fn_type_info;

        // Literal/range type constraint (for AllocaInst* values)
        std::optional<TypeConstraint> type_constraint;

        // Resource tracking: list of resource kind IDs this value belongs to
        llvm::SmallVector<int, 2> resource_kinds;
        bool json_type_only = false;

        // Mutation helper (avoids duplicates in resource_kinds)
        void addResourceKind(int rk);

        // Query helpers
        bool hasAnyCollectionType() const;
        bool hasAnyResourceKind() const;
        bool hasAnyMeta() const;
        llvm::Type *getCollectionType(TypeMeta kind) const;
        void setCollectionType(TypeMeta kind, llvm::Type *ty);
    };
    std::unordered_map<llvm::Value*, ValueMetadata> value_metadata_;

    // Unified metadata accessors (resolve through LoadInst automatically)
    ValueMetadata *getMeta(llvm::Value *val);
    const ValueMetadata *getMeta(llvm::Value *val) const;
    ValueMetadata &getOrCreateMeta(llvm::Value *val);

    // TypeMeta convenience (minimize rewrite in consumers)
    void setTypeMeta(TypeMeta kind, llvm::Value *val, llvm::Type *ty);
    llvm::Type *getTypeMeta(TypeMeta kind, llvm::Value *val) const;

    // Type-name accessors — return "" when metadata is absent
    std::string getSetElemName(llvm::Value *setPtr) const;

    // Validate that elem (ptrTy_) matches the container's expected collection type
    // name, then propagate type metadata.  No-op when elemName is empty (primitive
    // element types don't carry a name).  errorContext is the operator/function
    // name used in the error message (e.g. "add()", "'in' operator").
    void validateSetElemType(const std::string &elemName, llvm::Value *elem,
                             const std::string &errorContext);

    // Unified propagation (replaces propagateCollectionMetadata + propagateResourceTracking)
    void propagateMeta(llvm::Value *src, llvm::Value *dst);
    void propagateMetaWide(llvm::Value *src, llvm::Value *dst);

    // Resource kind helpers
    void addResourceKind(llvm::Value *val, int rk);
    bool hasResourceKind(llvm::Value *val, int rk) const;
    void removeResourceKind(llvm::Value *val, int rk);

    int constraint_err_counter_ = 0;
    int arith_zero_err_counter_ = 0;
    int overflow_err_counter_ = 0;

    bool isIntLiteralType(const std::string &typeName);
    bool isStrLiteralType(const std::string &typeName);
    bool isRangeType(const std::string &typeName);
    bool isLiteralUnionType(const std::string &typeName);
    std::optional<TypeConstraint> parseTypeConstraint(const std::string &typeName);
    void emitConstraintCheck(llvm::Value *val, const TypeConstraint &constraint,
                              const std::string &varName);
    std::string resolveTypeAlias(const std::string &typeName);

    // Loop context stack for break/continue (condBB, endBB)
    // { condBB, endBB, scopeDepth at loop entry }
    std::vector<std::tuple<llvm::BasicBlock*, llvm::BasicBlock*, size_t>> loop_stack_;

    // Indexed for-loop helper: emits loop scaffolding and calls bindVars for each iteration
    void emitIndexedForLoop(llvm::Value *length,
                            std::vector<StmtNode> &body,
                            std::function<void(llvm::Value *iCur)> bindVars);
    // `tupleTypeName` is the Ry tuple type string (e.g. "(int, List<int>)")
    // used to propagate per-component metadata onto the bound variables.
    // Pass "" when no source-level name is available (e.g. iterator-sourced
    // tuples); metadata propagation is skipped in that case.
    void emitTupleDestructure(const std::vector<std::string> &var_names,
                              llvm::Value *tupleVal, llvm::StructType *structTy,
                              const std::string &tupleTypeName);
    void emitParallelForRange(ForStmt &s, llvm::Value *begin, llvm::Value *end, llvm::Value *step);
    void validateParallelFor(const ForStmt &s);

    // RAII scope for function emission (B5)
    class FnScope {
    public:
        explicit FnScope(CodeGen &cg);
        ~FnScope();
        FnScope(const FnScope&) = delete;
        FnScope& operator=(const FnScope&) = delete;
    private:
        CodeGen &cg_;
        llvm::Function *savedFn_;
        std::vector<std::unordered_map<std::string, llvm::AllocaInst*>> savedScope_;
        std::vector<std::unordered_set<std::string>> savedConstScope_;
        std::unordered_set<llvm::AllocaInst*> savedArcManaged_;
        std::unordered_set<llvm::Value*> savedArcOwned_;
        std::unordered_set<llvm::AllocaInst*> savedArcBacked_;
        std::unordered_set<llvm::AllocaInst*> savedWeakManaged_;
        std::unordered_map<llvm::AllocaInst*, std::string> savedWeakInnerTypeNames_;
        std::unordered_map<llvm::AllocaInst*, ResourceKind> savedResourceManaged_;
        std::unordered_set<llvm::AllocaInst*> savedClosureManaged_;
        std::vector<std::vector<llvm::Value*>> savedIteratorMallocs_;
        llvm::BasicBlock *savedBlock_;
        llvm::BasicBlock::iterator savedPoint_;
        std::vector<ExprPtr> *savedPostconditions_;
        std::vector<std::string> *savedEnsureBindings_;
        bool savedInEnsureContext_;
        std::string savedFnReturnType_;
        std::string savedFnName_;
        llvm::SmallPtrSet<llvm::AllocaInst*, 8> savedCapturedVars_;
        int savedFnNestingDepth_;
        size_t savedFnScopeStackSize_;
    };

    // ======== Statement Emission ========
    [[noreturn]] void codegenError(const SourceLocation &loc, const std::string &msg);
    [[noreturn]] void codegenError(const std::string &msg);

    void requireArgs(const CallExpr &e, size_t expected);
    void requireArgs(const std::string &callee, size_t actual, size_t expected);

    void pushScope();
    void popScope();
    llvm::AllocaInst *findVar(const std::string &name);
    bool isImmutable(const std::string &name) const;
    bool isCapturedVar(llvm::AllocaInst *ptr) const;
    llvm::AllocaInst *getOrCreateVar(const std::string &name, llvm::Type *ty);

    // Variable declaration (B3)
    void emitVarDecl(const std::string &name,
                     const TypeNodePtr &type_annotation,
                     ExprNode &value, bool is_immutable);

    void emitStmt(AssignStmt &s);
    void emitStmt(CallStmt &s);
    void emitStmt(ExprStmt &s);
    void emitStmt(ReturnStmt &s);
    void emitStmt(ImportStmt &s);
    void emitStmt(RecordStmt &s);
    void emitStmt(TypeAliasStmt &s);
    void emitStmt(IndexAssignStmt &s);
    void emitStmt(BreakStmt &s);
    void emitStmt(ContinueStmt &s);
    void emitStmt(EllipsisStmt &s);
    void emitStmt(FieldAssignStmt &s);

    // Helpers for chained LHS assignment (#812).
    //
    // `applyCompoundOp` resolves a compound assignment operator
    // (`+=` / `-=` / ... / `**=`) against a current value and rhs expression,
    // trying user-defined `operator+=` first, then user-defined `operator+`,
    // and finally the built-in binary op. Used by both `AssignStmt`'s
    // re-assignment path and the chained `IndexAssignStmt` / `FieldAssignStmt`
    // paths so the evaluation order (LHS address computed exactly once) is
    // shared.
    llvm::Value *applyCompoundOp(const std::string &op,
                                  llvm::Value *currentVal,
                                  llvm::Value *rhs,
                                  ExprNode &rhsExpr,
                                  llvm::Type *targetTy,
                                  const std::string &contextName);

    // Walk a FieldAccessExpr chain (possibly 1-deep) down to its base, which
    // must be a VariableExpr referring to an alloca or module-global of
    // record type. Performs the immutable/@const rejection on the base and
    // writes `newInnerVal` as the new value of the chain's innermost record
    // by `InsertValue`-ing it up through all intermediate record hops, then
    // storing the fully-updated root record back to its alloca. Throws
    // `codegenError` if any node in the chain is not a VariableExpr /
    // FieldAccessExpr (e.g. IndexExpr / CallExpr — those cases are handled
    // separately by the FieldAssignStmt dispatcher).
    //
    // `writeBackInvariant` re-runs the outermost record's invariant check
    // after the store completes.
    void writeBackFieldChain(ExprNode &chainTarget, llvm::Value *newInnerVal);

    void emitStmt(EnumStmt &s);
    void emitStmt(ExpectStmt &s);
    void emitStmt(AwaitStmt &s);
    void emitStmt(TupleDestructStmt &s);
    void emitStmt(std::unique_ptr<IfStmt> &s);
    void emitStmt(std::unique_ptr<CaseCondStmt> &s);
    void emitStmt(std::unique_ptr<WhileStmt> &s);
    void emitStmt(std::unique_ptr<ForStmt> &s);
    void emitStmt(std::unique_ptr<FnStmt> &s);
    void forwardDeclareFunctions(Program &prog);
    llvm::Function *declareFunction(
        const std::string &name,
        std::vector<llvm::Type*> &paramTypes,
        std::vector<FnParam> &params,
        llvm::Type *exposedRetTy,
        const std::string &exposedReturnTypeName,
        const std::vector<Directive> &directives,
        const std::vector<ExprPtr> *preconditions,
        const std::vector<ExprPtr> *postconditions,
        const std::vector<std::string> *ensureBindings);
    void applyInlineDirective(llvm::Function *func, const std::vector<Directive> &directives);
    void validateDirectives(const std::vector<Directive> &directives);
    llvm::Type *tryResolveType(const std::string &typeName);
    void emitStmt(std::unique_ptr<CaseStmt> &s);
    llvm::Value *emitPatternTest(const Pattern &pattern, llvm::Value *subjectVal,
                                  llvm::Type *subjectTy, const std::string &subjectEnumType);
    void emitPatternBindings(const Pattern &pattern, llvm::AllocaInst *subjectAlloca,
                              llvm::Type *subjectTy, const std::string &subjectEnumType);
    void emitPatternBindingArc(llvm::Value *val, llvm::AllocaInst *bindAlloca,
                                const std::string &typeSig);
    void checkMatchExhaustiveness(const std::vector<std::pair<const Pattern*, bool>> &armPatterns,
                                   llvm::Type *subjectTy, const std::string &subjectEnumType);
    void validateBranchTypes(llvm::Value *lhs, llvm::Value *rhs, const char *exprKind);
    std::string resolveEnumType(llvm::Value *val) const;
    void emitDescribeCall(CallStmt &s);
    void emitItCall(CallStmt &s);
    void emitEachItCall(CallStmt &s);
    void emitPropertyItCall(CallStmt &s);
    llvm::SmallVector<llvm::Value*, 4> loadCapturedArgs(const OverloadEntry &entry, const std::string &directive);
    void emitItDirective(std::unique_ptr<FnStmt> &s);
    void emitEachItDirective(std::unique_ptr<FnStmt> &s);
    void emitPropertyItDirective(std::unique_ptr<FnStmt> &s);
    void emitDescribeDirective(std::unique_ptr<FnStmt> &s);
    void emitEachItLoop(llvm::Value *listPtr, llvm::Type *elemTy, unsigned numFields,
                        const std::string &fmtStr, llvm::Function *testFunc,
                        const std::vector<llvm::Value*> &capturedVals = {});
    void emitPropertyItLoop(llvm::Function *testFunc, llvm::Value *descVal,
                            const std::vector<llvm::Type*> &paramTypes,
                            const std::vector<std::string> &paramNames, int64_t count,
                            const std::vector<llvm::Value*> &capturedVals = {});
    void emitOutlinePrintf(const std::string &label, llvm::Value *nameVal = nullptr);
    std::pair<llvm::FunctionCallee, llvm::FunctionCallee> getTestItFunctions();
    std::pair<llvm::FunctionCallee, llvm::FunctionCallee> getTestDescribeFunctions();
    llvm::Function *emitTestFunction(const std::string &namePrefix,
        const std::vector<llvm::Type*> &paramTypes, LambdaExpr &lam, const std::string &context);
    void emitMockCall(CallStmt &s);
    void emitFailCall(CallStmt &s);
    std::unordered_set<std::string> mocked_functions_;
    std::unordered_map<std::string, llvm::Constant*> mock_name_strings_;
    llvm::Constant *fail_empty_msg_ = nullptr;
    llvm::Value *toBool(llvm::Value *v);

    // Low-level type helpers
    const std::string &getLowLevelTypeName(llvm::Value *val) const;
    bool isUnsignedLowLevel(llvm::Value *val) const;
    static bool isUnsignedLowLevelName(const std::string &name);
    static bool isLowLevelTypeName(const std::string &name);
    static std::string getExprLowLevelSuffix(const ExprNode &node);
    bool isLowLevelIntTy(llvm::Type *ty) const;
    bool isLowLevelIntTy(llvm::Value *val) const;
    bool isLowLevelFloatTy(llvm::Type *ty) const;
    bool isLowLevelTy(llvm::Type *ty) const;
    bool isLowLevelTy(llvm::Value *val) const;
    void checkLowLevelTypeMix(llvm::Value *lhs, llvm::Value *rhs, const std::string &op,
                              const std::string &lhsHint, const std::string &rhsHint);
    llvm::Value *coerceToLowLevelType(llvm::Value *val, llvm::Type *targetTy,
                                       const std::string &typeName,
                                       const std::string &context,
                                       const std::string &truncName);

    // Checked/Saturating/Wrapping arithmetic helpers
    void validateCheckedArithArgs(llvm::Value *lhs, llvm::Value *rhs, const std::string &callee);
    llvm::Value *emitCheckedArithmetic(const std::string &callee, llvm::Value *lhs, llvm::Value *rhs);
    llvm::Value *emitSaturatingArithmetic(const std::string &callee, llvm::Value *lhs, llvm::Value *rhs);
    llvm::Value *emitWrappingArithmetic(const std::string &callee, llvm::Value *lhs, llvm::Value *rhs);
    llvm::Value *emitIntOverflowCheck(llvm::Intrinsic::ID intrinsicId,
                                       llvm::Value *lhs, llvm::Value *rhs,
                                       const std::string &opName);

    // Type promotion helpers (B1)
    void ensureNumericType(llvm::Value *v, const std::string &context);
    llvm::Value *promoteToInt(llvm::Value *v);
    std::pair<llvm::Value*, llvm::Value*> promoteToFloat(llvm::Value *lhs, llvm::Value *rhs);
    // Emit a compile error if `v` is i1 (bool). `op_display` is the operator
    // text (e.g. "+" or "&"); `category` is "arithmetic" or "bitwise".
    void rejectBoolInOperator(llvm::Value *v, const std::string &op_display,
                              const char *category);

    // Implicit widening conversion helpers
    bool isWideningConversion(llvm::Value *argVal, llvm::Type *paramTy,
                              const std::string &paramTypeName) const;
    llvm::Value *emitWideningConversion(llvm::Value *argVal, llvm::Type *paramTy);
    bool isSubtypeOf(const std::string &childType, const std::string &parentType) const;
    llvm::Value *emitSubtypeSlice(llvm::Value *childVal,
                                   const std::string &childTypeName,
                                   const std::string &parentTypeName);
    llvm::Value *tryEmitSubtypeCoerce(llvm::Value *val, llvm::Type *targetTy);

    // ======== Expression Emission ========
    llvm::Value *emitExpr(const ExprNode &node);
    llvm::Value *emitExprVariant(const NumberExpr &e);
    llvm::Value *emitExprVariant(const FloatExpr &e);
    llvm::Value *emitExprVariant(const BoolExpr &e);
    llvm::Value *emitExprVariant(const StringExpr &e);
    llvm::Value *emitExprVariant(const RegexExpr &e);
    llvm::Value *emitExprVariant(const VariableExpr &e);
    llvm::Value *emitExprVariant(const std::unique_ptr<UnaryExpr> &e);
    llvm::Value *emitExprVariant(const std::unique_ptr<BinaryExpr> &e);
    llvm::Value *emitExprVariant(const std::unique_ptr<CallExpr> &e);
    llvm::Value *emitExprVariant(const std::unique_ptr<FieldAccessExpr> &e);
    llvm::Value *emitExprVariant(const std::unique_ptr<TupleExpr> &e);
    llvm::Value *emitExprVariant(const std::unique_ptr<ListExpr> &e);
    llvm::Value *emitExprVariant(const std::unique_ptr<IndexExpr> &e);
    llvm::Value *emitExprVariant(const std::unique_ptr<MapExpr> &e);
    llvm::Value *emitExprVariant(const std::unique_ptr<SetExpr> &e);
    llvm::Value *emitExprVariant(const EnumAccessExpr &e);
    llvm::Value *emitExprVariant(const std::unique_ptr<LambdaExpr> &e);
    llvm::Value *emitExprVariant(const std::unique_ptr<CastExpr> &e);
    llvm::Value *emitExprVariant(const std::unique_ptr<InterpolatedStringExpr> &e);
    llvm::Value *emitExprVariant(const std::unique_ptr<CaseCondExpr> &e);
    llvm::Value *emitExprVariant(const std::unique_ptr<CaseExpr> &e);
    llvm::Value *emitExprVariant(const std::unique_ptr<IfExpr> &e);
    llvm::Value *emitExprVariant(const std::unique_ptr<IfBlockExpr> &e);
    llvm::Value *emitExprVariant(const std::unique_ptr<RangeExpr> &e);
    llvm::Value *emitExprVariant(const NoneExpr &e);
    llvm::Value *emitExprVariant(const std::unique_ptr<ErrorPropagateExpr> &e);
    llvm::Value *emitExprVariant(const std::unique_ptr<AwaitExpr> &e);
    llvm::Value *emitExprVariant(const std::unique_ptr<WeakExpr> &e);
    llvm::Value *valueToString(llvm::Value *val, bool inCollection = false);
    llvm::Value *recordToString(llvm::Value *val);
    bool isTupleStructType(llvm::StructType *st);
    llvm::Value *tupleToString(llvm::Value *val, llvm::StructType *st);
    void emitSprintBegin();
    llvm::Value *emitSprintEnd(const llvm::Twine &name = "");
    llvm::Value *concatStringParts(
        const std::vector<std::pair<llvm::Value*, llvm::Value*>> &parts,
        const std::string &prefix);

    // Operator overload helpers
    llvm::Value *findAndCallOverload(const std::string &opFnName,
                                      llvm::ArrayRef<llvm::Value*> args,
                                      const char *callName = "opcall");
    llvm::Value *tryOperatorCall(const std::string &opFnName,
                                 llvm::Value *lhs, llvm::Value *rhs);
    llvm::Value *tryUnaryOperatorCall(const std::string &opFnName,
                                      llvm::Value *operand);
    llvm::Value *trySubscriptOperatorCall(llvm::Value *object,
                                           llvm::ArrayRef<llvm::Value*> indices);
    llvm::Value *tryCallOperator(const std::string &callee,
                                 const std::vector<ExprPtr> &args);
    bool trySubscriptAssignOperatorCall(llvm::Value *object,
                                         llvm::ArrayRef<llvm::Value*> indices,
                                         llvm::Value *value);

    // Binary operation dispatch (user-defined → any → built-in)
    llvm::Value *emitBinaryOp(const std::string &op, llvm::Value *lhs, llvm::Value *rhs,
                               const std::string &lhsHint = "", const std::string &rhsHint = "");

    // BinaryExpr sub-dispatchers (B2)
    llvm::Value *emitComparisonOp(const std::string &op, llvm::Value *lhs, llvm::Value *rhs,
                                   const std::string &lhsHint = "", const std::string &rhsHint = "");
    llvm::Value *emitRecordComparison(const std::string &op, llvm::Value *lhs,
                                       llvm::Value *rhs, const RecordInfo &info);
    llvm::Value *emitBitwiseOp(const std::string &op, llvm::Value *lhs, llvm::Value *rhs,
                                const std::string &lhsHint = "", const std::string &rhsHint = "");
    llvm::Value *emitArithmeticOp(const std::string &op, llvm::Value *lhs, llvm::Value *rhs,
                                   const std::string &lhsHint = "", const std::string &rhsHint = "");
    llvm::Value *emitAnyBinaryOp(const std::string &op, llvm::Value *lhs, llvm::Value *rhs);
    llvm::Value *emitAnyUnaryNeg(llvm::Value *operand);

    // Function call helper (B4)
    llvm::Value *emitUserFnCall(const std::string &callee, const std::vector<ExprPtr> &args);
    llvm::Function *resolveOverload(const std::string &callee,
                                    const std::vector<ExprPtr> &args,
                                    std::vector<llvm::Value*> &outArgVals);

    llvm::Value *emitRecordConstructor(const RecordInfo &info, const std::string &name, const std::vector<ExprPtr> &args);
    llvm::Type *resolveType(const std::string &typeName);
    llvm::StructType *getOptionType(llvm::Type *innerTy);
    bool isOptionType(llvm::Type *ty);
    llvm::Value *buildNoneValue(llvm::Type *optionTy);
    llvm::Value *buildSomeValue(llvm::Value *inner, llvm::Type *optionTy);
    llvm::StructType *getResultType(llvm::Type *okTy, llvm::Type *errTy);
    bool isResultType(llvm::Type *ty);
    llvm::Value *buildOkValue(llvm::Value *inner, llvm::StructType *resultTy);
    llvm::Value *buildErrValue(llvm::Value *inner, llvm::StructType *resultTy);
    // Coerce a Result value to a different Result struct type by rebuilding it
    // with the destination layout.  Returns null if both payload types differ
    // (genuine type error).
    llvm::Value *coerceResultType(llvm::Value *val, llvm::StructType *dstResTy);
    // Walk the InsertValueInst chain of val to find the index-{0} (disc) slot.
    // Returns true and writes 0 or 1 to *staticDisc when the disc was inserted
    // with a ConstantInt.  For PHINodes, recurses into all incoming values and
    // returns true only when every incoming yields the same constant disc.
    // Returns false for any runtime-produced value (CallInst, LoadInst, etc.).
    static bool tryGetStaticResultDisc(llvm::Value *val, int *staticDisc);
    static bool tryGetStaticResultDiscImpl(llvm::Value *val, int *staticDisc,
                                           llvm::SmallPtrSetImpl<llvm::Value *> &visited);
    llvm::Value *buildStaticError(const std::string &msg, const std::string &globalName);
    static std::vector<std::string> splitTypeArgs(const std::string &argsStr);
    std::vector<std::string> splitTupleSig(const std::string &tupleTypeSig);
    static size_t findMatchingCloseParen(const std::string &s, size_t openParen);
    std::pair<llvm::Type*, llvm::Type*> parseMapTypeAnnotation(const std::string &typeStr);
    FnTypeInfo parseFnTypeAnnotation(const std::string &typeStr);
    void emitRuntimeError(const std::string &message, const std::string &globalName,
                          llvm::ArrayRef<llvm::Value *> extraArgs = {});
    void emitIntZeroDivGuard(llvm::Value *divisor, const std::string &bbPrefix,
                             const std::string &errMsg);
    void emitBoundsError(llvm::Value *index, llvm::Value *size,
                         const std::string &fmtMsg, const std::string &globalName);
    llvm::Value *emitNegativeIndexWrap(llvm::Value *idx, llvm::Value *wrapBase,
                                        const std::string &prefix);
    void emitBoundsCheck(llvm::Value *&index, llvm::Value *size,
                         const std::string &errMsg, const std::string &globalName,
                         const std::string &bbPrefix);
    void emitContractCheck(const std::string &kind, const std::string &fn_name,
                           const ExprPtr &cond);
    void emitEnsureChecks(llvm::Value *retVal);
    void emitInvariantCheck(const std::string &typeName, const RecordInfo &info,
                            llvm::Value *recordVal);
    llvm::Type *getListElementType(llvm::Value *listAlloca);
    llvm::Type *getMapKeyType(llvm::Value *mapVal);
    llvm::Type *getMapValueType(llvm::Value *mapVal);
    llvm::Value *emitMapKeyLookup(llvm::Value *mapPtr, llvm::Value *key, llvm::Type *keyTy,
                                  const std::string &keyName = "");
    llvm::Value *emitIsWhitespace(llvm::Value *ch);

    // Runtime function helper: getOrInsertFunction with inline FunctionType creation
    llvm::FunctionCallee getRuntimeFn(const char *name, llvm::Type *retTy,
                                       llvm::ArrayRef<llvm::Type*> argTys);

    // C stdlib function helpers
    llvm::FunctionCallee getStdlibMalloc();
    llvm::FunctionCallee getStdlibRealloc();
    llvm::FunctionCallee getStdlibFree();
    llvm::FunctionCallee getStdlibStrlen();
    llvm::FunctionCallee getStdlibMemcpy();
    llvm::FunctionCallee getStdlibMemmove();
    llvm::FunctionCallee getStdlibMemset();
    llvm::FunctionCallee getStdlibMemcmp();
    llvm::FunctionCallee getStdlibStrcmp();
    llvm::FunctionCallee getStdlibStrncmp();
    llvm::FunctionCallee getStdlibStrstr();
    llvm::FunctionCallee getStdlibStrcasestr();
    llvm::FunctionCallee getStdlibStrncasecmp();
    llvm::FunctionCallee getStdlibStrcpy();
    llvm::FunctionCallee getStdlibStrcat();
    llvm::FunctionCallee getStdlibSnprintf();
    llvm::FunctionCallee getStdlibPrintf();
    llvm::FunctionCallee getBufferedPrintf();
    llvm::FunctionCallee getSprintPrintf();
    llvm::FunctionCallee getStdlibExit();
    llvm::Type *getSetElementType(llvm::Value *setVal);
    llvm::Type *getNestedListElementType(llvm::Value *listVal);
    llvm::Value *emitSetElementLookup(llvm::Value *setPtr, llvm::Value *elem, llvm::Type *elemTy,
                                       const std::string &elemName = "");
    llvm::Type *getTaskResultType(llvm::Value *taskVal);
    llvm::Type *getThreadResultType(llvm::Value *threadVal);
    llvm::Value *emitTaskWait(llvm::Value *taskVal, const char *runtimeFn, const char *label);

    // Hash function resolution helper (Step 1)
    struct HashFnInfo {
        std::string hashFnName;
        std::string rehashFnName;
        llvm::Type *hashArgTy;
    };
    HashFnInfo resolveHashFn(llvm::Type *keyTy);
    llvm::Value *coerceHashKey(llvm::Value *key, llvm::Type *keyTy,
                               llvm::Type *hashArgTy, const llvm::Twine &prefix);


    // Unified hash table lookup helper (Step 3)
    struct HashTableLayout {
        unsigned lenIdx;
        unsigned bucketCountIdx;
        unsigned bucketsPtrIdx;
        unsigned keysPtrIdx;
    };
    // Set header:  { len, cap, elems*, bucketCount, buckets* }
    static constexpr HashTableLayout kSetLayout = {0, 3, 4, 2};
    // Map header:  { len, cap, keys*, vals*, bucketCount, buckets* }
    static constexpr HashTableLayout kMapLayout = {0, 4, 5, 2};

    llvm::Value *emitHashTableLookup(llvm::Value *containerPtr, llvm::StructType *headerTy,
                                      const HashTableLayout &layout,
                                      llvm::Value *key, llvm::Type *keyTy);

    struct BucketContext {
        llvm::Value *bucketsPtr;
        llvm::Value *bucketMask;
        llvm::Type *hashArgTy;
        llvm::FunctionCallee hashFn;
    };
    BucketContext emitHashTableRemoveBucket(
        llvm::Value *headerPtr, llvm::StructType *headerTy,
        const HashTableLayout &layout,
        llvm::Value *key, llvm::Type *keyTy, llvm::Value *denseIndex,
        const std::string &prefix);
    void emitHashTableUpdateIndex(
        const BucketContext &bc,
        llvm::Value *value, llvm::Type *valueTy,
        llvm::Value *oldIndex, llvm::Value *newIndex,
        const std::string &prefix);

    // ===== Collection Header Allocation Policy =====
    //
    // All collection headers (List, Set, Map) are allocated with an ARC header
    // prepended via emitArcAllocCollectionHeader(). This ensures:
    // - Scope cleanup (emitArcReleaseVar) can safely read the ARC header
    // - CoW checks (emitCowCheck) work correctly via arc_backed_vars_
    // - Discarded expression results (ExprStmt) are released immediately
    // - Collection values are compatible with scope cleanup and assignment
    //
    // Data buffers (element arrays, key/value arrays) inside collection headers
    // are allocated with plain malloc and freed by the collection destructor
    // (getOrCreateCollectionDestructor).

    // Data structure field helpers
    struct ListFields {
        llvm::Value *lenPtr = nullptr;
        llvm::Value *len = nullptr;
        llvm::Value *capPtr = nullptr;
        llvm::Value *cap = nullptr;
        llvm::Value *dataPtr = nullptr;
        llvm::Value *data = nullptr;
    };
    ListFields loadListHeader(llvm::Value *listVal, const std::string &prefix);

    struct SetFields {
        llvm::Value *lenPtr = nullptr;
        llvm::Value *len = nullptr;
        llvm::Value *capPtr = nullptr;
        llvm::Value *cap = nullptr;
        llvm::Value *elemsPtr = nullptr;
        llvm::Value *elems = nullptr;
    };
    SetFields loadSetHeader(llvm::Value *setVal, const std::string &prefix);

    struct MapFields {
        llvm::Value *lenPtr = nullptr;
        llvm::Value *len = nullptr;
        llvm::Value *capPtr = nullptr;
        llvm::Value *cap = nullptr;
        llvm::Value *keysPtr = nullptr;
        llvm::Value *keys = nullptr;
        llvm::Value *valsPtr = nullptr;
        llvm::Value *vals = nullptr;
    };
    MapFields loadMapHeader(llvm::Value *mapVal, const std::string &prefix);

    void storeListHeaderFields(llvm::Value *headerPtr, llvm::Value *len,
                               llvm::Value *cap, llvm::Value *data);
    void storeSetHeaderFields(llvm::Value *headerPtr, llvm::Value *len,
                              llvm::Value *cap, llvm::Value *elems);
    void storeMapHeaderFields(llvm::Value *headerPtr, llvm::Value *len,
                              llvm::Value *cap, llvm::Value *keys,
                              llvm::Value *vals);

    llvm::Value *wrapPtrAsOption(llvm::Value *ptr, const std::string &hint);

    // ======== Collection Operations ========
    llvm::Value *emitBuiltinCore(const CallExpr &e);
    llvm::Value *emitBuiltinCollection(const CallExpr &e, llvm::Value *preEmittedArg0 = nullptr);

    // Collection operation handlers
    llvm::Value *emitCollOp_add(const CallExpr &e);
    llvm::Value *emitCollOp_remove(const CallExpr &e);
    llvm::Value *emitSetRemove(llvm::Value *containerPtr, llvm::Value *elem, llvm::Type *elemTy);
    llvm::Value *emitListRemove(llvm::Value *containerPtr, llvm::Value *val, llvm::Type *listElemTy);
    llvm::Value *emitMapRemove(llvm::Value *containerPtr, llvm::Value *key, llvm::Type *keyTy, llvm::Type *valTy);
    llvm::Value *emitCollOp_append(const CallExpr &e);
    llvm::Value *emitCollOp_appended(const CallExpr &e);
    llvm::Value *emitCollOp_pop(const CallExpr &e);
    llvm::Value *emitCollOp_slice(const CallExpr &e);
    // Shared slice helper: emit IR for list[start, endExcl).
    // startVal/endExclVal must be i64 and already negative-wrapped if needed.
    // emitListSlice clamps the half-open range to [0, lf.len] internally.
    llvm::Value *emitListSlice(llvm::Value *listPtr,
                                llvm::Value *startVal,
                                llvm::Value *endExclVal,
                                llvm::Type *elemTy);
    llvm::Value *emitCollOp_take(const CallExpr &e);
    llvm::Value *emitCollOp_take_impl(const CallExpr &e, llvm::Value *listPtr);
    llvm::Value *emitCollOp_insert(const CallExpr &e);
    llvm::Value *emitCollOp_remove_at(const CallExpr &e);
    llvm::Value *emitCollOp_distinct(const CallExpr &e);
    llvm::Value *emitCollOp_flatten(const CallExpr &e);
    llvm::Value *emitCollOp_items(const CallExpr &e);
    llvm::Value *emitCollOp_get(const CallExpr &e);
    llvm::Value *emitCollOp_merge(const CallExpr &e);

    // ======== String Operations ========
    llvm::Value *emitBuiltinString(const CallExpr &e);
    llvm::Value *emitStrOp_contains(const CallExpr &e);
    llvm::Value *emitStrOp_starts_with(const CallExpr &e);
    llvm::Value *emitStrOp_ends_with(const CallExpr &e);
    llvm::Value *emitStrOp_find(const CallExpr &e);
    llvm::Value *emitStrOp_substring(const CallExpr &e);
    llvm::Value *emitStrOp_char_at(const CallExpr &e);
    llvm::Value *emitStrOp_replace(const CallExpr &e);
    llvm::Value *emitStrOp_to_upper(const CallExpr &e);
    llvm::Value *emitStrOp_to_lower(const CallExpr &e);
    llvm::Value *emitStrOp_trim(const CallExpr &e);
    llvm::Value *emitStrOp_trim_start(const CallExpr &e);
    llvm::Value *emitStrOp_trim_end(const CallExpr &e);
    llvm::Value *emitStrOp_repeat(const CallExpr &e);
    llvm::Value *emitStringRepeat(llvm::Value *strVal, llvm::Value *n);
    llvm::Value *emitListConcat(llvm::Value *lhs, llvm::Value *rhs, llvm::Type *elemTy);
    llvm::Value *emitMapMergeCore(llvm::Value *map1, llvm::Value *map2,
                                   llvm::Type *keyTy, llvm::Type *valTy);
    llvm::Value *emitSetUnionCore(llvm::Value *set1, llvm::Value *set2,
                                   llvm::Type *elemTy);
    llvm::Value *emitStrOp_reverse(const CallExpr &e);
    llvm::Value *emitStrOp_split(const CallExpr &e);
    llvm::Value *emitStrOp_join(const CallExpr &e);

    llvm::Value *emitBuiltinHigherOrder(const CallExpr &e, llvm::Value *preEmittedArg0 = nullptr);
    llvm::Value *emitSortCore(llvm::Value *listVal, const std::vector<ExprPtr> &args, const std::string &callee);
    llvm::Value *emitBuiltinQuery(const CallExpr &e);
    llvm::Value *emitTypeOf(const CallExpr &e);
    std::pair<int64_t, std::string> resolveTypeOfKey(llvm::Value *val);
    llvm::Value *buildTypeValue(int64_t id, const std::string &name);
    llvm::Value *emitBuiltinSetOps(const CallExpr &e);
    llvm::Value *emitSubsetCheck(llvm::Value *iterSet, llvm::Value *lookupSet,
                                  const std::string &prefix);
    // Set operation handlers
    llvm::Value *emitSetOp_union(const CallExpr &e);
    llvm::Value *emitSetOp_intersection(const CallExpr &e);
    llvm::Value *emitSetOp_difference(const CallExpr &e);
    llvm::Value *emitSetOp_symmetric_difference(const CallExpr &e);
    llvm::Value *emitSetOp_is_subset(const CallExpr &e);
    llvm::Value *emitSetOp_is_superset(const CallExpr &e);
    // ======== Native/Stdlib & Resource Tracking ========
    llvm::Value *emitBuiltinConversion(const CallExpr &e);
    llvm::Value *emitBuiltinRegex(const CallExpr &e);
    // Generic dispatch for @native("libname") functions not covered by
    // self-registering stdlib dispatch tables. Uses the signature registry
    // to derive the C calling convention from Ry type annotations.
    llvm::Value *emitGenericNativeCall(const CallExpr &e);
    bool isResourceKind(int rk, llvm::Value *val);
    bool isTcpListener(llvm::Value *val);
    bool isTcpStream(llvm::Value *val);
    bool isTlsStream(llvm::Value *val);
    bool isHttpRequest(llvm::Value *val);
    bool isHttpResponse(llvm::Value *val);
    bool isHttpClientResponse(llvm::Value *val);
    bool isJsonValue(llvm::Value *val);
    bool isThread(llvm::Value *val);
    bool isLock(llvm::Value *val);
    bool isRWLock(llvm::Value *val);
    bool isSemaphore(llvm::Value *val);
    bool isBarrier(llvm::Value *val);
    bool isAtomicInt(llvm::Value *val);
    bool isAtomicBool(llvm::Value *val);
    bool isRegex(llvm::Value *val);
    void propagateTypeMeta(const std::string &typeName, llvm::Value *val);
    void propagateReturnTypeMeta(const OverloadEntry *entry, llvm::Value *val);
    void propagateReturnFnTypeMeta(const OverloadEntry *entry, llvm::Function *fn, llvm::Value *result);
    void registerResourceByTypeName(const std::string &typeName, llvm::Value *val);
    void applyParamTypeMeta(const std::string &ptype, llvm::AllocaInst *alloca,
                            llvm::Type *paramLLVMType, const std::string &paramName);
    // Shared Result-wrapping helpers for stdlib dispatchers
    llvm::Value *emitResultBranch(llvm::Value *isErr, llvm::StructType *resTy,
                                   llvm::function_ref<llvm::Value*()> buildOk,
                                   llvm::function_ref<llvm::Value*()> buildErr);
    llvm::Value *buildErrorFromRuntime(const char *errFnName = "__ry_get_last_error");
    llvm::Value *wrapPtrAsResult(llvm::Value *ptr, const char *errFnName = "__ry_get_last_error");
    llvm::Value *wrapStatusAsResult(llvm::Value *status, const char *errFnName = "__ry_get_last_error");

    llvm::Value *emitPtrToResult(llvm::Value *ptr, const std::string &name,
                                 const std::string &errMsg, int rk);
    llvm::Value *emitBuiltinResult(const CallExpr &e, llvm::Value *preEmittedArg0 = nullptr);
    llvm::Value *emitBuiltinOption(const CallExpr &e, llvm::Value *preEmittedArg0 = nullptr);
    llvm::Value *emitBuiltinIterator(const CallExpr &e, llvm::Value *preEmittedArg0 = nullptr);
    llvm::Type *getIteratorElementType(llvm::Value *iterVal);
    void emitBucketInit(llvm::Value *headerPtr, llvm::StructType *headerTy,
                        unsigned bucketCountIdx, unsigned bucketsPtrIdx,
                        int64_t initialBucketCount);
    void emitBucketInsertAndRehashCheck(llvm::Value *headerPtr, llvm::StructType *headerTy,
                                         unsigned lenIdx, unsigned bucketCountIdx, unsigned bucketsPtrIdx,
                                         llvm::Value *key, llvm::Type *keyTy, llvm::Value *denseIndex);
    void emitPrint(const std::vector<ExprPtr> &args, const std::vector<NamedArg> &named_args);
    void emitExit(const std::vector<ExprPtr> &args);

    // ======== Lambda & Type Inference ========
    // Lambda call helper: invoke a lambda/closure value with given args
    std::vector<llvm::Value*> coerceCallArgs(const FnTypeInfo &info,
                                             std::vector<llvm::Value*> args,
                                             const std::string &context);
    llvm::Value *emitLambdaCall(llvm::Value *lambdaVal, const FnTypeInfo &info,
                                std::vector<llvm::Value*> args, const std::string &name);

    // Return type inference
    void buildLocalTypeMap(const std::vector<StmtNode> &body,
        std::unordered_map<std::string, llvm::Type*> &typeMap);
    llvm::Type *inferExprType(const ExprNode &expr,
        const std::unordered_map<std::string, llvm::Type*> &paramTypeMap);
    std::string inferExprTypeName(const ExprNode &expr,
        const std::unordered_map<std::string, llvm::Type*> &paramTypeMap,
        const std::unordered_map<std::string, std::string> &paramTypeNameMap);
    llvm::Type *inferReturnType(const std::vector<StmtNode> &body,
        const std::unordered_map<std::string, llvm::Type*> &paramTypeMap);
    void collectReturnTypes(const std::vector<StmtNode> &body,
        const std::unordered_map<std::string, llvm::Type*> &paramTypeMap,
        std::vector<llvm::Type*> &out);
    std::string inferReturnTypeName(const std::vector<StmtNode> &body,
        const std::unordered_map<std::string, llvm::Type*> &paramTypeMap,
        const std::unordered_map<std::string, std::string> &paramTypeNameMap);
    void collectReturnTypeNames(const std::vector<StmtNode> &body,
        const std::unordered_map<std::string, llvm::Type*> &paramTypeMap,
        const std::unordered_map<std::string, std::string> &paramTypeNameMap,
        std::vector<std::string> &out);
    llvm::Type *deduceReturnType(const std::vector<llvm::Type*> &types);
    std::string reverseResolveTypeName(llvm::Type *ty);
    std::string inferCollectionTypeName(llvm::Value *val);
    std::string buildTypeNameFromMeta(llvm::Value *val);
    std::string extractMapKeyTypeName(const std::string &mapTypeName);
    std::string extractMapValueTypeName(const std::string &mapTypeName);

    // ======== Union & Any Type Helpers ========
    std::vector<std::string> parseUnionComponents(const std::string &typeName);
    std::string normalizeUnionType(const std::string &typeName);
    // Resolves typeName through type aliases and, for non-literal unions,
    // recursively expands alias components and deduplicates members,
    // returning the canonical sorted form (e.g. "bool | int | str").
    std::string flattenUnionWithAliases(const std::string &typeName);
    // Flattens `typeName` and, if the result is still a union after dedupe,
    // stores it as `union_value_type` metadata on `target`. Skips the write
    // when the flattened form collapses to a single leaf (which would make
    // downstream wrapInUnion lookups crash).
    void storeFlattenedUnionMeta(llvm::Value *target, const std::string &typeName);
    bool isUnionType(const std::string &typeName);
    llvm::Value *wrapInUnion(llvm::Value *val, const std::string &unionTypeName);

    int64_t getAnyTypeTag(llvm::Type *ty);
    llvm::Value *wrapInAny(llvm::Value *val);
    llvm::Value *buildUnitAny();
    llvm::Value *unwrapFromAny(llvm::Value *anyVal, llvm::Type *targetTy);
    bool isAnyType(llvm::Type *ty) const;
    bool canAnyHoldType(llvm::Type *ty) const;
    bool isNonStrPointer(llvm::Value *val);
    bool isStringValue(llvm::Value *val);
    llvm::Value *emitAnyToString(llvm::Value *anyVal, bool inCollection = false);
    static bool isNoneLiteral(const ExprNode &expr);
};

} // namespace ry
