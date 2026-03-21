#pragma once

#include <cstdint>

extern "C" {

using __ry_task_entry_fn = void (*)(void *, void *);
using __ry_parallel_for_fn = void (*)(void *, int64_t, int64_t, int64_t);

void *__ry_task_spawn(__ry_task_entry_fn entry, void *env, int64_t result_size);
void __ry_task_join(void *task, void *out_buf);
void __ry_parallel_for_i64(int64_t begin, int64_t end, int64_t step,
                           void *env, __ry_parallel_for_fn fn);
int64_t __ry_available_parallelism();
void *__ry_channel_new(int64_t elem_size, int64_t capacity);
void __ry_channel_send(void *channel, void *value_ptr);
bool __ry_channel_try_send(void *channel, void *value_ptr);
void __ry_channel_recv(void *channel, void *out_buf);
bool __ry_channel_recv_opt(void *channel, void *out_buf);
bool __ry_channel_try_recv(void *channel, void *out_buf);
void __ry_channel_close(void *channel);
void *__ry_select_begin(int64_t case_count);
void __ry_select_add_recv(void *select_state, void *channel, void *out_buf);
void __ry_select_add_recv_opt(void *select_state, void *channel, void *out_buf, void *flag_ptr);
void __ry_select_add_send(void *select_state, void *channel, void *value_ptr);
int64_t __ry_select_wait(void *select_state, int64_t default_index, int64_t timeout_ms, int64_t timeout_index);
void __ry_select_destroy(void *select_state);

}
