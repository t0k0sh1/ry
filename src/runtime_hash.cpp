#include <cstdint>
#include <cstdlib>
#include <cstring>

namespace ry {

static const int64_t EMPTY = -1;
static const int64_t TOMBSTONE = -2;

extern "C" {

uint64_t __ry_hash_i64(int64_t key) {
    return (uint64_t)key * 0x9E3779B97F4A7C15ULL;
}

uint64_t __ry_hash_f64(double key) {
    uint64_t bits;
    memcpy(&bits, &key, sizeof(bits));
    // Normalize -0.0 to +0.0
    if (bits == 0x8000000000000000ULL) bits = 0;
    return bits * 0x9E3779B97F4A7C15ULL;
}

uint64_t __ry_hash_str(const char *key) {
    // FNV-1a
    uint64_t h = 14695981039346656037ULL;
    for (const unsigned char *p = (const unsigned char *)key; *p; ++p) {
        h ^= *p;
        h *= 1099511628211ULL;
    }
    return h;
}

// Find key in hash table, return dense index or -1
int64_t __ry_ht_find_i64(int64_t *buckets, int64_t bucketMask,
                          int64_t *keys, int64_t len, int64_t key) {
    uint64_t h = __ry_hash_i64(key);
    int64_t bc = bucketMask + 1;
    for (int64_t i = 0; i < bc; ++i) {
        int64_t idx = (int64_t)((h + (uint64_t)i) & (uint64_t)bucketMask);
        int64_t slot = buckets[idx];
        if (slot == EMPTY) return -1;
        if (slot >= 0 && slot < len && keys[slot] == key) return slot;
    }
    return -1;
}

int64_t __ry_ht_find_f64(int64_t *buckets, int64_t bucketMask,
                          double *keys, int64_t len, double key) {
    uint64_t h = __ry_hash_f64(key);
    int64_t bc = bucketMask + 1;
    for (int64_t i = 0; i < bc; ++i) {
        int64_t idx = (int64_t)((h + (uint64_t)i) & (uint64_t)bucketMask);
        int64_t slot = buckets[idx];
        if (slot == EMPTY) return -1;
        if (slot >= 0 && slot < len && keys[slot] == key) return slot;
    }
    return -1;
}

int64_t __ry_ht_find_str(int64_t *buckets, int64_t bucketMask,
                          const char **keys, int64_t len, const char *key) {
    uint64_t h = __ry_hash_str(key);
    int64_t bc = bucketMask + 1;
    for (int64_t i = 0; i < bc; ++i) {
        int64_t idx = (int64_t)((h + (uint64_t)i) & (uint64_t)bucketMask);
        int64_t slot = buckets[idx];
        if (slot == EMPTY) return -1;
        if (slot >= 0 && slot < len && strcmp(keys[slot], key) == 0) return slot;
    }
    return -1;
}

// Insert a dense index into the bucket array
void __ry_ht_insert(int64_t *buckets, int64_t bucketMask,
                     uint64_t hash, int64_t denseIndex) {
    int64_t bc = bucketMask + 1;
    for (int64_t i = 0; i < bc; ++i) {
        int64_t idx = (int64_t)((hash + (uint64_t)i) & (uint64_t)bucketMask);
        int64_t slot = buckets[idx];
        if (slot == EMPTY || slot == TOMBSTONE) {
            buckets[idx] = denseIndex;
            return;
        }
    }
}

// Rehash: rebuild bucket array from dense keys array
int64_t *__ry_ht_rehash_i64(int64_t *keys, int64_t count, int64_t newBucketCount) {
    int64_t *buckets = (int64_t *)malloc((size_t)newBucketCount * sizeof(int64_t));
    memset(buckets, 0xFF, (size_t)newBucketCount * sizeof(int64_t)); // fill with -1 (EMPTY)
    int64_t mask = newBucketCount - 1;
    for (int64_t i = 0; i < count; ++i) {
        __ry_ht_insert(buckets, mask, __ry_hash_i64(keys[i]), i);
    }
    return buckets;
}

int64_t *__ry_ht_rehash_f64(double *keys, int64_t count, int64_t newBucketCount) {
    int64_t *buckets = (int64_t *)malloc((size_t)newBucketCount * sizeof(int64_t));
    memset(buckets, 0xFF, (size_t)newBucketCount * sizeof(int64_t));
    int64_t mask = newBucketCount - 1;
    for (int64_t i = 0; i < count; ++i) {
        __ry_ht_insert(buckets, mask, __ry_hash_f64(keys[i]), i);
    }
    return buckets;
}

int64_t *__ry_ht_rehash_str(const char **keys, int64_t count, int64_t newBucketCount) {
    int64_t *buckets = (int64_t *)malloc((size_t)newBucketCount * sizeof(int64_t));
    memset(buckets, 0xFF, (size_t)newBucketCount * sizeof(int64_t));
    int64_t mask = newBucketCount - 1;
    for (int64_t i = 0; i < count; ++i) {
        __ry_ht_insert(buckets, mask, __ry_hash_str(keys[i]), i);
    }
    return buckets;
}

// Set tombstone at position of denseIndex, used for remove
void __ry_ht_remove(int64_t *buckets, int64_t bucketMask,
                     uint64_t hash, int64_t denseIndex) {
    int64_t bc = bucketMask + 1;
    for (int64_t i = 0; i < bc; ++i) {
        int64_t idx = (int64_t)((hash + (uint64_t)i) & (uint64_t)bucketMask);
        if (buckets[idx] == denseIndex) {
            buckets[idx] = TOMBSTONE;
            return;
        }
        if (buckets[idx] == EMPTY) return;
    }
}

// Update bucket entry: change oldIndex to newIndex
void __ry_ht_update_index(int64_t *buckets, int64_t bucketMask,
                           uint64_t hash, int64_t oldIndex, int64_t newIndex) {
    int64_t bc = bucketMask + 1;
    for (int64_t i = 0; i < bc; ++i) {
        int64_t idx = (int64_t)((hash + (uint64_t)i) & (uint64_t)bucketMask);
        if (buckets[idx] == oldIndex) {
            buckets[idx] = newIndex;
            return;
        }
        if (buckets[idx] == EMPTY) return;
    }
}

} // extern "C"

} // namespace ry
