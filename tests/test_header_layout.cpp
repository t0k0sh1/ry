// Cross-language parity guard for the internal collection / ARC header layout
// (#2071). The Rust emit crate rebuilds the list/map/set/arc header structs from
// a single field-kind table (crates/emit/src/core.rs `header_fields`), used both
// to emit IR (cow_ensure_unique / arc_header_type) and reported here via the
// test-only extern `ry_emit_test_header_layout`. This test asserts that mirror
// against C++'s canonical listHeaderTy_ / mapHeaderTy_ / setHeaderTy_ /
// arcHeaderTy_ (src/codegen.cpp:57-61), so a C++ field-order/shape change the
// Rust mirror does not follow fails the build instead of silently corrupting
// CoW deep-copy at runtime (which compiled AND passed test_abi_layout.cpp, since
// that pins only the boundary descriptors, not the internal headers).
//
// Coverage boundary: same-type swaps (len<->cap, keys<->vals, strong<->weak; all
// 8-byte fields) are invisible to any layout check and are covered behaviorally
// in tests/spec/cow.test.ry instead. See #2071 / the grill-me design closure
// (harmful same-type swap <=> behaviorally catchable; symmetric swap <=> benign).

#include "ry/codegen.hpp"
#include <gtest/gtest.h>
#include <llvm/IR/DataLayout.h>
#include <llvm/IR/DerivedTypes.h>
#include <llvm/IR/Module.h>
#include <cstdint>
#include <vector>

using namespace ry;

extern "C" {
// Mirror of the Rust #[repr(C)] RyTestHeaderLayout
// (crates/emit/src/abi/test_introspect.rs): { u32 count; u8 kinds[8] }.
struct RyTestHeaderLayout {
    uint32_t count;
    uint8_t kinds[8]; // first `count` valid; 0 = i64, 1 = ptr
};
RyTestHeaderLayout ry_emit_test_header_layout(int kind);

// Mirror of the Rust #[repr(C)] RyTestFieldIndices
// (crates/emit/src/abi/test_introspect.rs): named GEP-field positions for
// List / Map headers. Exposed so the C++ side can assert each constant
// indexes a field of the expected type in the canonical struct, pinning a
// constant-value drift that the per-index slice parity test alone misses.
struct RyTestFieldIndices {
    uint32_t list_len;
    uint32_t list_cap;
    uint32_t list_data;
    uint32_t map_len;
    uint32_t map_cap;
    uint32_t map_keys;
    uint32_t map_vals;
    uint32_t map_bucket_count;
    uint32_t map_buckets;
};
RyTestFieldIndices ry_emit_test_field_indices();
}

namespace {

// Kind selectors — must match the Rust RY_TEST_HDR_* constants.
constexpr int HDR_LIST = 0;
constexpr int HDR_MAP = 1;
constexpr int HDR_SET = 2;
constexpr int HDR_ARC = 3;

// Per-field type tags — must match the Rust FIELD_* constants.
constexpr uint8_t FIELD_I64 = 0;
constexpr uint8_t FIELD_PTR = 1;

// Pure comparator (the unit under test for the negative case): does the canonical
// LLVM struct match the Rust-reported layout? Same field count, and each field's
// LLVM type-kind agrees with the reported tag.
bool layoutMatches(llvm::StructType *canonical, const RyTestHeaderLayout &rust) {
    if (canonical->getNumElements() != rust.count)
        return false;
    for (uint32_t i = 0; i < rust.count; ++i) {
        llvm::Type *fld = canonical->getElementType(i);
        bool ok;
        switch (rust.kinds[i]) {
        case FIELD_I64:
            ok = fld->isIntegerTy(64);
            break;
        case FIELD_PTR:
            ok = fld->isPointerTy();
            break;
        default:
            ok = false;
            break;
        }
        if (!ok)
            return false;
    }
    return true;
}

// Build a literal LLVM struct in `cg`'s context from the Rust-reported kinds, so
// the ABI size can be compared like-for-like under one DataLayout (catches a
// future non-8-byte field drift that per-index i64/ptr tags alone would miss).
llvm::StructType *rebuildFromRust(CodeGen &cg, const RyTestHeaderLayout &rust) {
    std::vector<llvm::Type *> elems;
    elems.reserve(rust.count);
    for (uint32_t i = 0; i < rust.count; ++i)
        elems.push_back(rust.kinds[i] == FIELD_PTR ? cg.ptrTy_ : cg.i64Ty_);
    return llvm::StructType::get(*cg.ctx_, elems);
}

void expectParity(CodeGen &cg, int kind, llvm::StructType *canonical, const char *name) {
    RyTestHeaderLayout rust = ry_emit_test_header_layout(kind);
    ASSERT_GT(rust.count, 0u) << name << ": Rust reported an unknown kind selector";
    EXPECT_EQ(canonical->getNumElements(), rust.count) << name << ": field count";
    EXPECT_TRUE(layoutMatches(canonical, rust)) << name << ": per-index field type";
    const llvm::DataLayout &dl = cg.mod_->getDataLayout();
    llvm::StructType *rebuilt = rebuildFromRust(cg, rust);
    EXPECT_EQ(dl.getTypeAllocSize(canonical), dl.getTypeAllocSize(rebuilt)) << name << ": ABI size";
}

} // namespace

// Positive: the Rust mirror matches C++'s canonical header definitions.
TEST(HeaderLayoutParity, RustMirrorMatchesCanonical) {
    CodeGen cg;
    expectParity(cg, HDR_LIST, cg.listHeaderTy_, "ListHeader");
    expectParity(cg, HDR_MAP, cg.mapHeaderTy_, "MapHeader");
    expectParity(cg, HDR_SET, cg.setHeaderTy_, "SetHeader");
    expectParity(cg, HDR_ARC, cg.arcHeaderTy_, "ArcHeader");
}

// Named GEP-field index parity: the Rust LIST_FIELD_* / MAP_FIELD_* constants
// (the single source of truth migration sites use, #2095) must index a field
// of the expected semantic type in C++'s canonical struct. A constant-value
// drift (e.g. someone setting LIST_FIELD_DATA = 0 by mistake) is invisible to
// the per-index slice parity test above — it would still report Ptr at slice
// position 2 — but is caught here because position 0 in `listHeaderTy_` is
// i64 (len), not ptr (the data semantic the constant names).
TEST(HeaderLayoutParity, NamedFieldIndicesMatchCanonical) {
    CodeGen cg;
    RyTestFieldIndices idx = ry_emit_test_field_indices();

    // List header: {len:i64, cap:i64, data:ptr}
    ASSERT_LT(idx.list_len, cg.listHeaderTy_->getNumElements());
    EXPECT_TRUE(cg.listHeaderTy_->getElementType(idx.list_len)->isIntegerTy(64))
        << "LIST_FIELD_LEN must index an i64 (len)";
    ASSERT_LT(idx.list_cap, cg.listHeaderTy_->getNumElements());
    EXPECT_TRUE(cg.listHeaderTy_->getElementType(idx.list_cap)->isIntegerTy(64))
        << "LIST_FIELD_CAP must index an i64 (cap)";
    ASSERT_LT(idx.list_data, cg.listHeaderTy_->getNumElements());
    EXPECT_TRUE(cg.listHeaderTy_->getElementType(idx.list_data)->isPointerTy())
        << "LIST_FIELD_DATA must index a ptr (data)";

    // Map header: {len:i64, cap:i64, keys:ptr, vals:ptr, bucket_count:i64, buckets:ptr}
    ASSERT_LT(idx.map_len, cg.mapHeaderTy_->getNumElements());
    EXPECT_TRUE(cg.mapHeaderTy_->getElementType(idx.map_len)->isIntegerTy(64))
        << "MAP_FIELD_LEN must index an i64 (len)";
    ASSERT_LT(idx.map_cap, cg.mapHeaderTy_->getNumElements());
    EXPECT_TRUE(cg.mapHeaderTy_->getElementType(idx.map_cap)->isIntegerTy(64))
        << "MAP_FIELD_CAP must index an i64 (cap)";
    ASSERT_LT(idx.map_keys, cg.mapHeaderTy_->getNumElements());
    EXPECT_TRUE(cg.mapHeaderTy_->getElementType(idx.map_keys)->isPointerTy())
        << "MAP_FIELD_KEYS must index a ptr (keys)";
    ASSERT_LT(idx.map_vals, cg.mapHeaderTy_->getNumElements());
    EXPECT_TRUE(cg.mapHeaderTy_->getElementType(idx.map_vals)->isPointerTy())
        << "MAP_FIELD_VALS must index a ptr (vals)";
    ASSERT_LT(idx.map_bucket_count, cg.mapHeaderTy_->getNumElements());
    EXPECT_TRUE(cg.mapHeaderTy_->getElementType(idx.map_bucket_count)->isIntegerTy(64))
        << "MAP_FIELD_BUCKET_COUNT must index an i64";
    ASSERT_LT(idx.map_buckets, cg.mapHeaderTy_->getNumElements());
    EXPECT_TRUE(cg.mapHeaderTy_->getElementType(idx.map_buckets)->isPointerTy())
        << "MAP_FIELD_BUCKETS must index a ptr";
}

// Negative (teeth): the comparator must REJECT a layout that disagrees, so the
// positive test above cannot pass vacuously. A cross-type reorder and a
// field-count drift must both be detected. (Same-type swaps are intentionally
// invisible to layout checks — tests/spec/cow.test.ry covers those.)
TEST(HeaderLayoutParity, ComparatorRejectsDrift) {
    CodeGen cg;
    // Canonical ListHeader = {i64, i64, ptr}.
    // Cross-type reorder: claim {i64, ptr, i64}.
    RyTestHeaderLayout reorder{3, {FIELD_I64, FIELD_PTR, FIELD_I64, 0, 0, 0, 0, 0}};
    EXPECT_FALSE(layoutMatches(cg.listHeaderTy_, reorder)) << "cross-type reorder must be rejected";
    // Field-count drift: claim only 2 fields.
    RyTestHeaderLayout shortened{2, {FIELD_I64, FIELD_I64, 0, 0, 0, 0, 0, 0}};
    EXPECT_FALSE(layoutMatches(cg.listHeaderTy_, shortened)) << "field-count drift must be rejected";
    // Sanity: the true layout matches (guards against an always-false comparator).
    RyTestHeaderLayout correct{3, {FIELD_I64, FIELD_I64, FIELD_PTR, 0, 0, 0, 0, 0}};
    EXPECT_TRUE(layoutMatches(cg.listHeaderTy_, correct)) << "correct layout must match";
}
