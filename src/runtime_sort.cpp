#include <cstdint>
#include <cstdlib>
#include <cstring>
#include <algorithm>

// TimSort implementation for the Ry language runtime.
// Called from JIT-generated code via __ry_timsort.

static const int64_t MIN_MERGE = 32;

static int64_t computeMinRun(int64_t n) {
    int64_t r = 0;
    while (n >= MIN_MERGE) {
        r |= (n & 1);
        n >>= 1;
    }
    return n + r;
}

using CmpFn = bool (*)(const void *, const void *, void *);

static inline void elemCopy(void *dst, const void *src, int64_t elemSize) {
    memcpy(dst, src, elemSize);
}

static inline void *elemAt(void *data, int64_t index, int64_t elemSize) {
    return static_cast<char *>(data) + index * elemSize;
}


static void reverseRange(void *data, int64_t lo, int64_t hi, int64_t elemSize) {
    char tmp[256];
    void *tmpBuf = elemSize <= (int64_t)sizeof(tmp) ? tmp : malloc(elemSize);
    int64_t left = lo, right = hi - 1;
    while (left < right) {
        elemCopy(tmpBuf, elemAt(data, left, elemSize), elemSize);
        elemCopy(elemAt(data, left, elemSize), elemAt(data, right, elemSize), elemSize);
        elemCopy(elemAt(data, right, elemSize), tmpBuf, elemSize);
        left++;
        right--;
    }
    if (elemSize > (int64_t)sizeof(tmp)) free(tmpBuf);
}

// Binary insertion sort for [lo, hi)
static void binaryInsertionSort(void *data, int64_t lo, int64_t hi,
                                 int64_t start, int64_t elemSize,
                                 CmpFn cmp, void *ctx) {
    char tmp[256];
    void *tmpBuf = elemSize <= (int64_t)sizeof(tmp) ? tmp : malloc(elemSize);

    for (int64_t i = start; i < hi; i++) {
        elemCopy(tmpBuf, elemAt(data, i, elemSize), elemSize);

        // Binary search for insertion point
        int64_t left = lo, right = i;
        while (left < right) {
            int64_t mid = left + (right - left) / 2;
            if (cmp(tmpBuf, elemAt(data, mid, elemSize), ctx)) {
                right = mid;
            } else {
                left = mid + 1;
            }
        }

        // Shift elements right
        int64_t n = i - left;
        if (n > 0) {
            if (elemSize <= 0 || n > (int64_t)(SIZE_MAX / (size_t)elemSize))
                std::abort();
            memmove(elemAt(data, left + 1, elemSize),
                    elemAt(data, left, elemSize),
                    (size_t)n * (size_t)elemSize);
        }
        elemCopy(elemAt(data, left, elemSize), tmpBuf, elemSize);
    }

    if (elemSize > (int64_t)sizeof(tmp)) free(tmpBuf);
}

// Detect a natural run starting at pos, returns length.
// If descending, reverses it.
static int64_t detectRun(void *data, int64_t pos, int64_t len,
                          int64_t elemSize, CmpFn cmp, void *ctx) {
    if (pos + 1 >= len) return 1;

    int64_t runLen = 2;
    if (cmp(elemAt(data, pos + 1, elemSize),
            elemAt(data, pos, elemSize), ctx)) {
        // Strictly descending
        while (pos + runLen < len &&
               cmp(elemAt(data, pos + runLen, elemSize),
                   elemAt(data, pos + runLen - 1, elemSize), ctx)) {
            runLen++;
        }
        reverseRange(data, pos, pos + runLen, elemSize);
    } else {
        // Non-descending
        while (pos + runLen < len &&
               !cmp(elemAt(data, pos + runLen, elemSize),
                    elemAt(data, pos + runLen - 1, elemSize), ctx)) {
            runLen++;
        }
    }
    return runLen;
}

// Merge [lo1, lo1+len1) and [lo2, lo2+len2) where lo2 = lo1+len1
static void merge(void *data, void *tmpBuf, int64_t lo1, int64_t len1,
                  int64_t lo2, int64_t len2, int64_t elemSize,
                  CmpFn cmp, void *ctx) {
    // Overflow guard: ensure len * elemSize fits in size_t
    if (elemSize <= 0) {
        std::abort();
    }
    int64_t maxLen = std::max(len1, len2);
    if (maxLen > (int64_t)(SIZE_MAX / (size_t)elemSize)) {
        std::abort();
    }

    // Copy smaller run to tmp buffer
    if (len1 <= len2) {
        // mergeLo: copy left run to tmp, merge left-to-right
        memcpy(tmpBuf, elemAt(data, lo1, elemSize), (size_t)len1 * (size_t)elemSize);
        int64_t i = 0;     // index into tmp (left run)
        int64_t j = lo2;   // index into data (right run)
        int64_t dest = lo1;
        int64_t jEnd = lo2 + len2;

        while (i < len1 && j < jEnd) {
            // Use !(right < left) to ensure stability
            if (!cmp(elemAt(data, j, elemSize),
                     elemAt(tmpBuf, i, elemSize), ctx)) {
                elemCopy(elemAt(data, dest, elemSize),
                         elemAt(tmpBuf, i, elemSize), elemSize);
                i++;
            } else {
                elemCopy(elemAt(data, dest, elemSize),
                         elemAt(data, j, elemSize), elemSize);
                j++;
            }
            dest++;
        }
        // Copy remaining from tmp
        if (i < len1) {
            memcpy(elemAt(data, dest, elemSize),
                   elemAt(tmpBuf, i, elemSize),
                   (size_t)(len1 - i) * (size_t)elemSize);
        }
        // Remaining right elements are already in place
    } else {
        // mergeHi: copy right run to tmp, merge right-to-left
        memcpy(tmpBuf, elemAt(data, lo2, elemSize), (size_t)len2 * (size_t)elemSize);
        int64_t i = len1 - 1;  // index into data (left run)
        int64_t j = len2 - 1;  // index into tmp (right run)
        int64_t dest = lo2 + len2 - 1;

        while (i >= 0 && j >= 0) {
            // Use !(left < right) → right goes to dest for stability
            if (cmp(elemAt(tmpBuf, j, elemSize),
                    elemAt(data, lo1 + i, elemSize), ctx)) {
                elemCopy(elemAt(data, dest, elemSize),
                         elemAt(data, lo1 + i, elemSize), elemSize);
                i--;
            } else {
                elemCopy(elemAt(data, dest, elemSize),
                         elemAt(tmpBuf, j, elemSize), elemSize);
                j--;
            }
            dest--;
        }
        // Copy remaining from tmp
        if (j >= 0) {
            memcpy(elemAt(data, lo1, elemSize),
                   tmpBuf,
                   (size_t)(j + 1) * (size_t)elemSize);
        }
        // Remaining left elements are already in place
    }
}

struct RunEntry {
    int64_t base;
    int64_t len;
};

extern "C" {

void __ry_timsort(void *data, int64_t len, int64_t elemSize,
                  bool (*cmp)(const void *, const void *, void *),
                  void *cmpCtx) {
    if (len < 2) return;

    int64_t minRun = computeMinRun(len);

    // For small arrays, just use insertion sort
    if (len <= minRun) {
        int64_t initRunLen = detectRun(data, 0, len, elemSize, cmp, cmpCtx);
        binaryInsertionSort(data, 0, len, initRunLen, elemSize, cmp, cmpCtx);
        return;
    }

    // Allocate temp buffer for merging (max half of array)
    int64_t tmpSize = (len / 2 + 1) * elemSize;
    void *tmpBuf = malloc(tmpSize);

    // Run stack (max 85 entries for 2^64 elements)
    RunEntry runStack[85];
    int stackSize = 0;

    int64_t pos = 0;
    while (pos < len) {
        // Detect natural run
        int64_t runLen = detectRun(data, pos, len, elemSize, cmp, cmpCtx);

        // Extend to minRun if needed
        if (runLen < minRun) {
            int64_t force = std::min(minRun, len - pos);
            binaryInsertionSort(data, pos, pos + force, pos + runLen,
                                elemSize, cmp, cmpCtx);
            runLen = force;
        }

        // If the stack is full, merge top runs to free space instead of
        // breaking out and leaving the tail of the array unprocessed.
        while (stackSize >= 85) {
            int n = stackSize - 2;
            merge(data, tmpBuf,
                  runStack[n].base, runStack[n].len,
                  runStack[n + 1].base, runStack[n + 1].len,
                  elemSize, cmp, cmpCtx);
            runStack[n].len += runStack[n + 1].len;
            if (n + 2 < stackSize) {
                runStack[n + 1] = runStack[n + 2];
            }
            stackSize--;
        }

        // Push run onto stack
        runStack[stackSize].base = pos;
        runStack[stackSize].len = runLen;
        stackSize++;

        // Merge collapse: maintain invariants
        // 1. runStack[i-2].len > runStack[i-1].len + runStack[i].len
        // 2. runStack[i-1].len > runStack[i].len
        while (stackSize > 1) {
            int n = stackSize - 2;
            if ((n >= 1 && runStack[n - 1].len <=
                           runStack[n].len + runStack[n + 1].len) ||
                (n >= 2 && runStack[n - 2].len <=
                           runStack[n - 1].len + runStack[n].len)) {
                if (n >= 1 && runStack[n - 1].len < runStack[n + 1].len) {
                    n--;
                }
            } else if (runStack[n].len > runStack[n + 1].len) {
                break;
            }

            // Merge run[n] with run[n+1]
            merge(data, tmpBuf,
                  runStack[n].base, runStack[n].len,
                  runStack[n + 1].base, runStack[n + 1].len,
                  elemSize, cmp, cmpCtx);
            runStack[n].len += runStack[n + 1].len;

            // Remove run[n+1] from stack
            if (n + 2 < stackSize) {
                runStack[n + 1] = runStack[n + 2];
            }
            stackSize--;
        }

        pos += runLen;
    }

    // Force merge remaining runs
    while (stackSize > 1) {
        int n = stackSize - 2;
        if (n >= 1 && runStack[n - 1].len < runStack[n + 1].len) {
            n--;
        }
        merge(data, tmpBuf,
              runStack[n].base, runStack[n].len,
              runStack[n + 1].base, runStack[n + 1].len,
              elemSize, cmp, cmpCtx);
        runStack[n].len += runStack[n + 1].len;
        if (n + 2 < stackSize) {
            runStack[n + 1] = runStack[n + 2];
        }
        stackSize--;
    }

    free(tmpBuf);
}

} // extern "C"
