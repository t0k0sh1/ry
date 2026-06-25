// Test taxonomy: docs/reference/test-taxonomy.md
// Section header tags: [contract] / [regression #NNNN] / [internal].
// Per-test exceptions use an inline `// [regression: #NNNN]` comment.

#include "test_codegen_common.hpp"
#include "ry/runtime/native/gc.hpp"
#include "ry/ry_layout.hpp"
#include <cstdlib>
#include <cstring>
#include <vector>


using namespace ry;
// ===== [internal] Cycle Collector tests (pure C++ — validates the runtime trial deletion algorithm) =====

namespace {


struct ArcHeader {
    int64_t strong_count;
    int64_t weak_count;
};

// Allocate an ARC object with the given data size.
void *gcAllocObj(size_t dataSize) {
    void *block = std::malloc(ARC_HEADER_SIZE + dataSize);
    auto *hdr = static_cast<ArcHeader *>(block);
    hdr->strong_count = 1;
    hdr->weak_count = 0;
    std::memset(static_cast<char *>(block) + ARC_HEADER_SIZE, 0, dataSize);
    return block;  // returns header pointer
}

void *headerToData(void *hdr) {
    return static_cast<char *>(hdr) + ARC_HEADER_SIZE;
}

ArcHeader *getHeader(void *hdr) {
    return static_cast<ArcHeader *>(hdr);
}

// A simple "node" type for cycle tests.
// Data layout: { void *child_header }  (one ARC pointer field)
struct NodeData {
    void *child_header;  // pointer to another node's header (or null)
};

// Visit function for NodeData: calls visitor for the child if non-null.
void visitNode(void *data, void (*visitor)(void *child_header)) {
    auto *node = static_cast<NodeData *>(data);
    if (node->child_header) {
        visitor(node->child_header);
    }
}

// Destructor for NodeData: nullifies the child pointer.
void dtorNode(void *data) {
    auto *node = static_cast<NodeData *>(data);
    node->child_header = nullptr;
}

// A "pair node" type with two children, for more complex cycle tests.
struct PairNodeData {
    void *child_a;
    void *child_b;
};

[[maybe_unused]] void visitPairNode(void *data, void (*visitor)(void *child_header)) {
    auto *node = static_cast<PairNodeData *>(data);
    if (node->child_a) visitor(node->child_a);
    if (node->child_b) visitor(node->child_b);
}

[[maybe_unused]] void dtorPairNode(void *data) {
    auto *node = static_cast<PairNodeData *>(data);
    node->child_a = nullptr;
    node->child_b = nullptr;
}

// Record with a non-pointer prefix field followed by an ARC pointer field.
// The int64_t label models non-pointer data preceding a GC-managed pointer;
// the runtime GC tests use visitContainer to ensure only the pointer field
// is reported to the collector.
struct ContainerData {
    int64_t label;
    void *child_header;
};

void visitContainer(void *data, void (*visitor)(void *child_header)) {
    auto *c = static_cast<ContainerData *>(data);
    if (c->child_header) {
        visitor(c->child_header);
    }
}

void dtorContainer(void *data) {
    auto *c = static_cast<ContainerData *>(data);
    c->child_header = nullptr;
}

} // anonymous namespace

// ===== [internal] Basic API tests =====

TEST(GcTest, CollectEmptyReturnsZero) {
    // Ensure initial state is clean.
    __ry_gc_enable();
    __ry_gc_setThreshold(700);
    EXPECT_EQ(__ry_gc_collect(), 0);
}

TEST(GcTest, EnableDisable) {
    __ry_gc_disable();
    // When disabled, track should not add candidates.
    void *h = gcAllocObj(sizeof(NodeData));
    __ry_gc_track(h, visitNode, dtorNode);
    // Collect should still work when called explicitly (but should find nothing
    // because track was ignored while disabled).
    __ry_gc_enable();
    // Re-enable and collect — the object was not tracked, so nothing to collect.
    // But we need to free our test object manually.
    std::free(h);
}

TEST(GcTest, SetThreshold) {
    __ry_gc_enable();
    __ry_gc_setThreshold(1000);
    // Just verifying it doesn't crash.
    __ry_gc_setThreshold(700);
}

// ===== [internal] Cycle detection and collection =====

TEST(GcTest, SimpleCycleCollected) {
    __ry_gc_enable();
    __ry_gc_setThreshold(10000);  // high threshold to prevent auto-collect

    // Create a cycle: A -> B -> A
    void *hdrA = gcAllocObj(sizeof(NodeData));
    void *hdrB = gcAllocObj(sizeof(NodeData));

    auto *dataA = static_cast<NodeData *>(headerToData(hdrA));
    auto *dataB = static_cast<NodeData *>(headerToData(hdrB));

    dataA->child_header = hdrB;
    dataB->child_header = hdrA;

    // Simulate: both objects have strong_count = 2 (one external, one from cycle).
    getHeader(hdrA)->strong_count = 2;
    getHeader(hdrB)->strong_count = 2;

    // Simulate external reference being dropped: strong_count -> 1.
    // This is what arc_release does when count > 0.
    getHeader(hdrA)->strong_count = 1;
    getHeader(hdrB)->strong_count = 1;

    // Track both as candidates.
    __ry_gc_track(hdrA, visitNode, dtorNode);
    __ry_gc_track(hdrB, visitNode, dtorNode);

    // Collect — should find the cycle and free both.
    int64_t collected = __ry_gc_collect();
    EXPECT_EQ(collected, 2);
}

TEST(GcTest, SelfCycleCollected) {
    __ry_gc_enable();
    __ry_gc_setThreshold(10000);

    // Create a self-referencing object: A -> A
    void *hdrA = gcAllocObj(sizeof(NodeData));
    auto *dataA = static_cast<NodeData *>(headerToData(hdrA));
    dataA->child_header = hdrA;

    // strong_count = 1 (only reference is from itself)
    getHeader(hdrA)->strong_count = 1;

    __ry_gc_track(hdrA, visitNode, dtorNode);

    int64_t collected = __ry_gc_collect();
    EXPECT_EQ(collected, 1);
}

TEST(GcTest, ReachableObjectNotCollected) {
    __ry_gc_enable();
    __ry_gc_setThreshold(10000);

    // A -> B, but A has an external reference (strong_count = 2).
    void *hdrA = gcAllocObj(sizeof(NodeData));
    void *hdrB = gcAllocObj(sizeof(NodeData));

    auto *dataA = static_cast<NodeData *>(headerToData(hdrA));
    dataA->child_header = hdrB;

    // A has external ref + ref to B = 2
    // B has ref from A = 1
    getHeader(hdrA)->strong_count = 2;
    getHeader(hdrB)->strong_count = 1;

    __ry_gc_track(hdrA, visitNode, dtorNode);
    __ry_gc_track(hdrB, visitNode, dtorNode);

    // Neither should be collected: A is reachable externally, B is reachable from A.
    int64_t collected = __ry_gc_collect();
    EXPECT_EQ(collected, 0);

    // Clean up manually.
    std::free(hdrA);
    std::free(hdrB);
}

TEST(GcTest, ThreeCycleCollected) {
    __ry_gc_enable();
    __ry_gc_setThreshold(10000);

    // A -> B -> C -> A (triangle cycle)
    void *hdrA = gcAllocObj(sizeof(NodeData));
    void *hdrB = gcAllocObj(sizeof(NodeData));
    void *hdrC = gcAllocObj(sizeof(NodeData));

    static_cast<NodeData *>(headerToData(hdrA))->child_header = hdrB;
    static_cast<NodeData *>(headerToData(hdrB))->child_header = hdrC;
    static_cast<NodeData *>(headerToData(hdrC))->child_header = hdrA;

    getHeader(hdrA)->strong_count = 1;
    getHeader(hdrB)->strong_count = 1;
    getHeader(hdrC)->strong_count = 1;

    __ry_gc_track(hdrA, visitNode, dtorNode);
    __ry_gc_track(hdrB, visitNode, dtorNode);
    __ry_gc_track(hdrC, visitNode, dtorNode);

    int64_t collected = __ry_gc_collect();
    EXPECT_EQ(collected, 3);
}

TEST(GcTest, MixedReachableAndCycle) {
    __ry_gc_enable();
    __ry_gc_setThreshold(10000);

    // Reachable: R (strong_count=2, external ref)
    // Cycle: A -> B -> A (strong_count=1 each)
    // R -> A (R has a reference to A, making A reachable... but wait,
    // R's reference to A increases A's strong_count)
    //
    // Actually, let's test: R is reachable, A and B form a cycle with no
    // external refs, and R has nothing to do with A/B.

    void *hdrR = gcAllocObj(sizeof(NodeData));
    void *hdrA = gcAllocObj(sizeof(NodeData));
    void *hdrB = gcAllocObj(sizeof(NodeData));

    static_cast<NodeData *>(headerToData(hdrR))->child_header = nullptr;
    static_cast<NodeData *>(headerToData(hdrA))->child_header = hdrB;
    static_cast<NodeData *>(headerToData(hdrB))->child_header = hdrA;

    getHeader(hdrR)->strong_count = 2;  // reachable externally
    getHeader(hdrA)->strong_count = 1;  // only from B
    getHeader(hdrB)->strong_count = 1;  // only from A

    __ry_gc_track(hdrR, visitNode, dtorNode);
    __ry_gc_track(hdrA, visitNode, dtorNode);
    __ry_gc_track(hdrB, visitNode, dtorNode);

    int64_t collected = __ry_gc_collect();
    // R survives (external ref), A and B are collected.
    EXPECT_EQ(collected, 2);

    // R is still alive — free it manually.
    std::free(hdrR);
}

TEST(GcTest, WeakCountPreservesHeader) {
    __ry_gc_enable();
    __ry_gc_setThreshold(10000);

    // Self-cycle with weak_count > 0.
    void *hdrA = gcAllocObj(sizeof(NodeData));
    auto *dataA = static_cast<NodeData *>(headerToData(hdrA));
    dataA->child_header = hdrA;

    getHeader(hdrA)->strong_count = 1;
    getHeader(hdrA)->weak_count = 1;  // weak ref exists

    __ry_gc_track(hdrA, visitNode, dtorNode);

    int64_t collected = __ry_gc_collect();
    EXPECT_EQ(collected, 1);

    // Header should still be alive because weak_count > 0.
    // strong_count should be 0 (set by collector).
    EXPECT_EQ(getHeader(hdrA)->strong_count, 0);
    EXPECT_EQ(getHeader(hdrA)->weak_count, 1);

    // Simulate weak release freeing the header.
    std::free(hdrA);
}

TEST(GcTest, UntrackRemovesCandidate) {
    __ry_gc_enable();
    __ry_gc_setThreshold(10000);

    void *hdrA = gcAllocObj(sizeof(NodeData));
    auto *dataA = static_cast<NodeData *>(headerToData(hdrA));
    dataA->child_header = hdrA;
    getHeader(hdrA)->strong_count = 1;

    __ry_gc_track(hdrA, visitNode, dtorNode);
    __ry_gc_untrack(hdrA);

    int64_t collected = __ry_gc_collect();
    EXPECT_EQ(collected, 0);

    std::free(hdrA);
}

TEST(GcTest, ImmortalObjectSkipped) {
    __ry_gc_enable();
    __ry_gc_setThreshold(10000);

    void *hdrA = gcAllocObj(sizeof(NodeData));
    getHeader(hdrA)->strong_count = ARC_IMMORTAL;  // immortal

    __ry_gc_track(hdrA, visitNode, dtorNode);

    int64_t collected = __ry_gc_collect();
    EXPECT_EQ(collected, 0);

    std::free(hdrA);
}

// ===== [internal] Record visit function tests =====

TEST(GcTest, RecordCycleCollected) {
    // Simulates a cycle through record types embedded in ARC objects:
    // Container A (label=1, child -> B) <-> Container B (label=2, child -> A)
    __ry_gc_enable();
    __ry_gc_setThreshold(10000);

    void *hdrA = gcAllocObj(sizeof(ContainerData));
    void *hdrB = gcAllocObj(sizeof(ContainerData));

    auto *dataA = static_cast<ContainerData *>(headerToData(hdrA));
    auto *dataB = static_cast<ContainerData *>(headerToData(hdrB));

    dataA->label = 1;
    dataA->child_header = hdrB;
    dataB->label = 2;
    dataB->child_header = hdrA;

    getHeader(hdrA)->strong_count = 1;
    getHeader(hdrB)->strong_count = 1;

    __ry_gc_track(hdrA, visitContainer, dtorContainer);
    __ry_gc_track(hdrB, visitContainer, dtorContainer);

    int64_t collected = __ry_gc_collect();
    EXPECT_EQ(collected, 2);
}

TEST(GcTest, RecordReachableNotCollected) {
    // Container with an external reference should not be collected.
    __ry_gc_enable();
    __ry_gc_setThreshold(10000);

    void *hdrA = gcAllocObj(sizeof(ContainerData));
    void *hdrB = gcAllocObj(sizeof(ContainerData));

    auto *dataA = static_cast<ContainerData *>(headerToData(hdrA));
    auto *dataB = static_cast<ContainerData *>(headerToData(hdrB));

    dataA->label = 1;
    dataA->child_header = hdrB;
    dataB->label = 2;
    dataB->child_header = nullptr;

    getHeader(hdrA)->strong_count = 2;  // external ref
    getHeader(hdrB)->strong_count = 1;

    __ry_gc_track(hdrA, visitContainer, dtorContainer);
    __ry_gc_track(hdrB, visitContainer, dtorContainer);

    int64_t collected = __ry_gc_collect();
    EXPECT_EQ(collected, 0);

    std::free(hdrA);
    std::free(hdrB);
}
