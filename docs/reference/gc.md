# GC Reference

The `gc` package provides control over the cycle collector, a safety net that works alongside ARC (Automatic Reference Counting) to detect and reclaim circular reference chains.

## Overview

Ry uses ARC for memory management, which handles most deallocations immediately and deterministically. However, ARC alone cannot reclaim circular references (e.g., A → B → A). The cycle collector uses a CPython-style trial deletion algorithm to periodically find and free such unreachable cycles.

Using `weak` references is still the recommended way to avoid cycles, but the cycle collector catches cases the user misses.

## Import

```ry
from gc import collect, enable, disable, set_threshold
```

## Functions

| Function | Signature | Description |
|----------|-----------|-------------|
| `collect` | `() -> int` | Runs a full collection cycle. Returns the number of objects collected. |
| `enable` | `() -> Unit` | Enables automatic collection (enabled by default). |
| `disable` | `() -> Unit` | Disables automatic collection AND candidate tracking. Useful for performance-critical sections. Cycles created while disabled are not queued as collection candidates; they remain unreclaimed until one of their objects undergoes another ARC decrement after `enable()` re-enables tracking. |
| `set_threshold` | `(n: int) -> Unit` | Sets the candidate count threshold for automatic collection (default: 700). Negative values force a collection on every candidate addition (not recommended). |

## How It Works

1. **Candidate tracking**: When `arc_release` decrements an object's reference count but it remains above zero, the object is added to a candidate set (only for types that can potentially form cycles).
2. **Trial deletion**: The collector tentatively decrements reference counts of all objects referenced by candidates. If an object's count reaches zero through trial deletion alone, it is only reachable within the candidate set — it is part of a cycle.
3. **Resurrection check**: Objects still reachable from outside the candidate set have their counts restored.
4. **Collection**: Remaining unreachable objects are deallocated.

## Static Analysis Optimization

The compiler performs static analysis at compile time to identify which types can potentially form reference cycles (e.g., recursive ADT enums). Only these types participate in GC candidate tracking. Non-cyclic types have zero GC overhead.

## Example

```ry
from gc import collect, disable, enable

# Disable automatic collection for a performance-critical section
disable()

# ... performance-critical code ...

# Re-enable and force a collection
enable()
collected = collect()
print(f"Collected {collected} cyclic objects")
```
