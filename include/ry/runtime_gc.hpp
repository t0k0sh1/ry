#pragma once
#include <cstdint>

// Cycle collector for ARC — detects and reclaims reference cycles.
// Uses CPython-style trial deletion algorithm.

// Visit function signature: given a data pointer and a visitor callback,
// call the visitor for each ARC child header pointer within the object.
using RyGcVisitFn = void (*)(void *data, void (*visitor)(void *child_header));

// Destructor function signature: given a data pointer, destroy the object
// (break internal references, free internal buffers).
using RyGcDtorFn = void (*)(void *data);

extern "C" {
    // Track an object as a potential cycle candidate.
    // Called when arc_release decrements strong_count but it remains > 0.
    // headerPtr: pointer to the ARC header (before data area).
    // visit_fn: type-specific function to enumerate ARC child references.
    // dtor_fn: type-specific destructor (may be null if no internal cleanup needed).
    void __ry_gc_track(void *headerPtr, RyGcVisitFn visit_fn, RyGcDtorFn dtor_fn);

    // Untrack an object (called when an object is freed normally via ARC).
    void __ry_gc_untrack(void *headerPtr);

    // Run a full collection cycle. Returns the number of objects collected.
    int64_t __ry_gc_collect();

    // Enable automatic collection (enabled by default).
    void __ry_gc_enable();

    // Disable automatic collection.
    void __ry_gc_disable();

    // Set the candidate count threshold for automatic collection.
    void __ry_gc_set_threshold(int64_t n);
}
