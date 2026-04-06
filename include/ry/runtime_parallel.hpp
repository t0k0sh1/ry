#pragma once

#include <cstdint>


namespace ry {

extern "C" {

using __ry_task_entry_fn = void (*)(void *, void *);
using __ry_parallel_for_fn = void (*)(void *, int64_t, int64_t, int64_t);

void *__ry_task_spawn(__ry_task_entry_fn entry, void *env, int64_t result_size);
void __ry_task_join(void *task, void *out_buf);
void __ry_block_on(void *task, void *out_buf);
void __ry_parallel_for_i64(int64_t begin, int64_t end, int64_t step,
                           void *env, __ry_parallel_for_fn fn);
int64_t __ry_available_parallelism();
void __ry_sleep(int64_t duration_ms);

}

} // namespace ry
