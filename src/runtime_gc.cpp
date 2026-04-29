#include "ry/runtime_gc.hpp"
#include "ry/runtime_arc.hpp"

#include <cstdlib>
#include <mutex>
#include <unordered_map>
#include <unordered_set>
#include <vector>


namespace ry {

// ---------------------------------------------------------------------------
// GC state
// ---------------------------------------------------------------------------

struct GcObjectInfo {
    RyGcVisitFn visit_fn;
    RyGcDtorFn  dtor_fn;
};

struct GcState {
    std::recursive_mutex                         mutex;
    std::unordered_map<void *, GcObjectInfo>     candidates;
    bool                                         enabled    = true;
    int64_t                                      threshold  = 700;
    bool                                         collecting = false;
};

static GcState &gc() {
    static GcState s;
    return s;
}

// ---------------------------------------------------------------------------
// ARC header field accessors
// ---------------------------------------------------------------------------

static inline int64_t *strong_ptr(void *hdr) { return static_cast<int64_t *>(hdr); }
static inline int64_t *weak_ptr(void *hdr)   { return static_cast<int64_t *>(hdr) + 1; }

static inline void *header_to_data(void *hdr) {
    return static_cast<char *>(hdr) + ARC_HEADER_SIZE;
}

// ---------------------------------------------------------------------------
// Trial deletion — collect_locked assumes the caller holds gc().mutex
// ---------------------------------------------------------------------------

struct CollectCtx {
    std::unordered_map<void *, GcObjectInfo> working;
    std::unordered_map<void *, int64_t> gc_refs;
    std::unordered_set<void *> survivors;
};

// Thread-local context pointer for the visitor callback (which is a plain
// function pointer and cannot capture). Safe because collection is serialised
// under gc().mutex.
static thread_local CollectCtx *tl_collect_ctx = nullptr;

static void trial_decrement_visitor(void *child_header) {
    auto it = tl_collect_ctx->gc_refs.find(child_header);
    if (it != tl_collect_ctx->gc_refs.end()) {
        it->second--;
    }
}

// Phase 3: recursively mark obj and all its reachable children as survivors.
static void mark_reachable(CollectCtx &ctx, void *headerPtr) {
    if (ctx.survivors.count(headerPtr))
        return;
    ctx.survivors.insert(headerPtr);

    auto it = ctx.working.find(headerPtr);
    if (it == ctx.working.end())
        return;

    // Gather children into a local vector, then recurse.
    // A local (not static) vector avoids iterator-invalidation across recursive calls.
    std::vector<void *> children;

    // Use a thread-local pointer so the non-capturing gather callback
    // can access the current children vector.
    static thread_local std::vector<void *> *tl_children = nullptr;
    tl_children = &children;

    auto gather = [](void *child) {
        tl_children->push_back(child);
    };

    void *data = header_to_data(headerPtr);
    it->second.visit_fn(data, gather);
    tl_children = nullptr;

    for (void *child : children) {
        mark_reachable(ctx, child);
    }
}

static int64_t collect_locked() {
    auto &g = gc();
    if (g.candidates.empty() || g.collecting)
        return 0;

    g.collecting = true;

    CollectCtx ctx;

    // Phase 1: Snapshot candidates into a single map (used for both iteration and lookup).
    ctx.working = g.candidates;

    // Initialise gc_refs from current strong_counts.
    //
    // Use atomic acquire loads so the snapshot is consistent with concurrent
    // ARC retain/release operations performed by @parallel for workers
    // (which hit strong_count via CreateAtomicRMW SequentiallyConsistent).
    // A plain load here would be a TOCTOU race (#630).
    for (auto &[hdr, info] : ctx.working) {
        int64_t sc = __atomic_load_n(strong_ptr(hdr), __ATOMIC_ACQUIRE);
        if (sc == ARC_IMMORTAL) continue;
        ctx.gc_refs[hdr] = sc;
    }

    // Clear the candidate set so that new tracks during collection go into a
    // fresh generation (they won't be collected in this cycle).
    g.candidates.clear();

    // Phase 2: Trial subtraction.
    tl_collect_ctx = &ctx;
    for (auto &[hdr, info] : ctx.working) {
        if (ctx.gc_refs.find(hdr) == ctx.gc_refs.end())
            continue;  // immortal, skipped
        void *data = header_to_data(hdr);
        info.visit_fn(data, trial_decrement_visitor);
    }
    tl_collect_ctx = nullptr;

    // Phase 3: Find survivors — objects with gc_ref_count > 0 are reachable
    // from outside the candidate set.
    for (auto &[hdr, gc_rc] : ctx.gc_refs) {
        if (gc_rc > 0) {
            mark_reachable(ctx, hdr);
        }
    }

    // Phase 4: Collect garbage — objects NOT in survivors.
    int64_t collected = 0;
    for (auto &[hdr, info] : ctx.working) {
        if (ctx.survivors.count(hdr))
            continue;
        if (ctx.gc_refs.find(hdr) == ctx.gc_refs.end())
            continue;  // immortal

        int64_t sc = __atomic_load_n(strong_ptr(hdr), __ATOMIC_ACQUIRE);
        if (sc == 0 || sc == ARC_IMMORTAL)
            continue;  // already freed or immortal

        // Break references — set strong_count to 0 to prevent destructor-
        // triggered releases from cascading. This also invalidates weak
        // references (weak upgrade checks strong_count == 0 and returns
        // None).
        //
        // Use a compare-exchange instead of a blind store so we do not
        // overwrite a strong_count that was revived by a concurrent
        // retain or weak-upgrade between the load above and the store
        // here. If CAS fails the object has been resurrected and must
        // not be destroyed. Widening this guarantee to cover the whole
        // algorithmic TOCTOU between the phase-1 snapshot and phase 4
        // (ABA on strong_count across the full collect cycle) is tracked
        // as a follow-up under #872.
        if (!__atomic_compare_exchange_n(strong_ptr(hdr), &sc, 0,
                                         /*weak=*/false,
                                         __ATOMIC_ACQ_REL, __ATOMIC_ACQUIRE))
            continue;  // Resurrected by concurrent retain — skip destruction.

        if (info.dtor_fn) {
            void *data = header_to_data(hdr);
            info.dtor_fn(data);
        }

        // Free the header block if no weak refs remain.
        if (*weak_ptr(hdr) <= 0) {
            std::free(hdr);
        }
        // If weak_count > 0, the header stays alive for weak ref resolution.

        collected++;
    }

    g.collecting = false;
    return collected;
}

// ---------------------------------------------------------------------------
// Public API
// ---------------------------------------------------------------------------

extern "C" {

void __ry_gc_track(void *headerPtr, RyGcVisitFn visit_fn, RyGcDtorFn dtor_fn) {
    if (!headerPtr || !visit_fn)
        return;

    auto &g = gc();
    std::lock_guard<std::recursive_mutex> lock(g.mutex);

    if (!g.enabled)
        return;

    auto [it, inserted] = g.candidates.emplace(headerPtr, GcObjectInfo{visit_fn, dtor_fn});
    if (!inserted)
        it->second = GcObjectInfo{visit_fn, dtor_fn};

    if (static_cast<int64_t>(g.candidates.size()) >= g.threshold) {
        collect_locked();
    }
}

void __ry_gc_untrack(void *headerPtr) {
    if (!headerPtr)
        return;
    auto &g = gc();
    std::lock_guard<std::recursive_mutex> lock(g.mutex);
    g.candidates.erase(headerPtr);
}

int64_t __ry_gc_collect() {
    auto &g = gc();
    std::lock_guard<std::recursive_mutex> lock(g.mutex);
    return collect_locked();
}

void __ry_gc_enable() {
    auto &g = gc();
    std::lock_guard<std::recursive_mutex> lock(g.mutex);
    g.enabled = true;
}

void __ry_gc_disable() {
    auto &g = gc();
    std::lock_guard<std::recursive_mutex> lock(g.mutex);
    g.enabled = false;
}

void __ry_gc_setThreshold(int64_t n) {
    auto &g = gc();
    std::lock_guard<std::recursive_mutex> lock(g.mutex);
    g.threshold = n;
}

}  // extern "C"

} // namespace ry
