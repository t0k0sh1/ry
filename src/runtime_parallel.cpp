#include "ry/runtime_parallel.hpp"

#include <algorithm>
#include <atomic>
#include <chrono>
#include <condition_variable>
#include <cstdlib>
#include <cstring>
#include <deque>
#include <exception>
#include <functional>
#include <memory>
#include <mutex>
#include <unordered_set>
#include <stdexcept>
#include <thread>
#include <utility>
#include <vector>

namespace {

thread_local bool g_runtime_worker = false;

struct TaskHandle {
    std::mutex mu;
    std::condition_variable cv;
    bool done = false;
    std::exception_ptr error;
    std::vector<unsigned char> result;
};

struct ParallelWaitState {
    std::mutex mu;
    std::condition_variable cv;
    int64_t pending = 0;
    std::exception_ptr error;
};

struct ChannelHandle {
    explicit ChannelHandle(size_t elem_size_in, int64_t capacity_in)
        : elem_size(elem_size_in), capacity(capacity_in) {
        if (capacity < 0)
            throw std::runtime_error("runtime error: channel() capacity must not be negative");
        rendezvous.resize(elem_size);
    }

    size_t elem_size;
    int64_t capacity;
    bool closed = false;

    std::mutex mu;
    std::condition_variable cv;

    std::deque<std::vector<unsigned char>> queue;

    std::vector<unsigned char> rendezvous;
    bool has_rendezvous = false;
    int64_t waiting_receivers = 0;
    int64_t waiting_select_receivers = 0;
};

enum class SelectCaseKind {
    Recv,
    RecvOpt,
    Send,
};

struct SelectCaseEntry {
    SelectCaseKind kind;
    ChannelHandle *channel = nullptr;
    void *value_ptr = nullptr;
    void *out_ptr = nullptr;
    void *flag_ptr = nullptr;
};

struct SelectState {
    std::vector<SelectCaseEntry> cases;
};

std::mutex g_select_mu;
std::condition_variable g_select_cv;
std::atomic<uint64_t> g_select_epoch{0};

void notifySelectWaiters() {
    {
        std::lock_guard<std::mutex> lock(g_select_mu);
        g_select_epoch.fetch_add(1, std::memory_order_relaxed);
    }
    g_select_cv.notify_all();
}

bool selectCaseInvalid(const SelectCaseEntry &entry) {
    ChannelHandle *channel = entry.channel;
    if (entry.kind == SelectCaseKind::Recv)
        return channel->closed && !channel->has_rendezvous && channel->queue.empty();
    if (entry.kind == SelectCaseKind::RecvOpt)
        return false;
    return channel->closed;
}

bool selectCaseReady(const SelectCaseEntry &entry) {
    ChannelHandle *channel = entry.channel;
    if (entry.kind == SelectCaseKind::Recv || entry.kind == SelectCaseKind::RecvOpt) {
        if (channel->capacity > 0)
            return !channel->queue.empty() || (entry.kind == SelectCaseKind::RecvOpt && channel->closed);
        return channel->has_rendezvous || (entry.kind == SelectCaseKind::RecvOpt && channel->closed);
    }

    if (channel->capacity > 0)
        return static_cast<int64_t>(channel->queue.size()) < channel->capacity;
    return !channel->has_rendezvous &&
           (channel->waiting_receivers + channel->waiting_select_receivers) > 0;
}

bool commitSelectCase(const SelectCaseEntry &entry) {
    ChannelHandle *channel = entry.channel;
    std::unique_lock<std::mutex> lock(channel->mu);

    if (selectCaseInvalid(entry)) {
        if (entry.kind == SelectCaseKind::Recv)
            throw std::runtime_error("runtime error: recv() on closed empty channel");
        throw std::runtime_error("runtime error: send() on closed channel");
    }
    if (!selectCaseReady(entry))
        return false;

    if (entry.kind == SelectCaseKind::Recv || entry.kind == SelectCaseKind::RecvOpt) {
        auto setRecvOptFlag = [&](bool has_value) {
            if (entry.kind == SelectCaseKind::RecvOpt && entry.flag_ptr)
                *static_cast<bool *>(entry.flag_ptr) = has_value;
        };

        if (channel->capacity > 0) {
            if (channel->queue.empty()) {
                setRecvOptFlag(false);
                return true;
            }
            std::vector<unsigned char> item = std::move(channel->queue.front());
            channel->queue.pop_front();
            if (channel->elem_size > 0 && entry.out_ptr)
                std::memcpy(entry.out_ptr, item.data(), channel->elem_size);
            setRecvOptFlag(true);
            channel->cv.notify_all();
            lock.unlock();
            notifySelectWaiters();
            return true;
        }

        if (!channel->has_rendezvous) {
            setRecvOptFlag(false);
            return true;
        }
        if (channel->elem_size > 0 && entry.out_ptr)
            std::memcpy(entry.out_ptr, channel->rendezvous.data(), channel->elem_size);
        setRecvOptFlag(true);
        channel->has_rendezvous = false;
        channel->cv.notify_all();
        lock.unlock();
        notifySelectWaiters();
        return true;
    }

    if (channel->capacity > 0) {
        std::vector<unsigned char> item(channel->elem_size);
        if (channel->elem_size > 0)
            std::memcpy(item.data(), entry.value_ptr, channel->elem_size);
        channel->queue.push_back(std::move(item));
        channel->cv.notify_all();
        lock.unlock();
        notifySelectWaiters();
        return true;
    }

    if (channel->elem_size > 0)
        std::memcpy(channel->rendezvous.data(), entry.value_ptr, channel->elem_size);
    channel->has_rendezvous = true;
    channel->cv.notify_all();
    lock.unlock();
    notifySelectWaiters();

    lock.lock();
    channel->cv.wait(lock, [&]() { return channel->closed || !channel->has_rendezvous; });
    if (channel->closed && channel->has_rendezvous)
        throw std::runtime_error("runtime error: send() on closed channel");
    return true;
}

int64_t normalize_parallelism() {
    const unsigned hw = std::thread::hardware_concurrency();
    if (hw == 0)
        return 4;
    return std::max<int64_t>(2, static_cast<int64_t>(hw));
}

int64_t iteration_count(int64_t begin, int64_t end, int64_t step) {
    if (step == 0) return 0;
    if (step > 0) {
        if (begin >= end) return 0;
        return (end - begin + step - 1) / step;
    }
    if (begin <= end) return 0;
    const int64_t abs_step = -step;
    return (begin - end + abs_step - 1) / abs_step;
}

class RuntimeScheduler {
public:
    RuntimeScheduler()
        : parallelism_(normalize_parallelism()) {
        workers_.reserve(static_cast<size_t>(parallelism_));
        for (int64_t i = 0; i < parallelism_; ++i)
            workers_.emplace_back([this]() { workerLoop(); });
    }

    ~RuntimeScheduler() {
        {
            std::lock_guard<std::mutex> lock(mu_);
            stopping_ = true;
        }
        cv_.notify_all();
        for (std::thread &worker : workers_) {
            if (worker.joinable()) {
                try { worker.join(); } catch (...) {}
            }
        }

        {
            std::lock_guard<std::mutex> lock(task_registry_mu_);
            for (TaskHandle *task : live_tasks_)
                delete task;
            live_tasks_.clear();
        }
        {
            std::lock_guard<std::mutex> lock(channel_registry_mu_);
            for (ChannelHandle *ch : live_channels_)
                delete ch;
            live_channels_.clear();
        }
    }

    int64_t parallelism() const {
        return parallelism_;
    }

    void registerTask(TaskHandle *task) {
        std::lock_guard<std::mutex> lock(task_registry_mu_);
        live_tasks_.insert(task);
    }

    bool unregisterTask(TaskHandle *task) {
        std::lock_guard<std::mutex> lock(task_registry_mu_);
        return live_tasks_.erase(task) > 0;
    }

    void registerChannel(ChannelHandle *ch) {
        std::lock_guard<std::mutex> lock(channel_registry_mu_);
        live_channels_.insert(ch);
    }

    void unregisterChannel(ChannelHandle *ch) {
        std::lock_guard<std::mutex> lock(channel_registry_mu_);
        live_channels_.erase(ch);
    }

    void submit(std::function<void()> job) {
        {
            std::lock_guard<std::mutex> lock(mu_);
            jobs_.push_back(std::move(job));
        }
        cv_.notify_one();
    }

    bool tryRunOne() {
        std::function<void()> job;
        {
            std::lock_guard<std::mutex> lock(mu_);
            if (jobs_.empty())
                return false;
            job = std::move(jobs_.front());
            jobs_.pop_front();
        }
        job();
        return true;
    }

private:
    void workerLoop() {
        g_runtime_worker = true;
        while (true) {
            std::function<void()> job;
            {
                std::unique_lock<std::mutex> lock(mu_);
                cv_.wait(lock, [this]() { return stopping_ || !jobs_.empty(); });
                if (stopping_ && jobs_.empty())
                    break;
                job = std::move(jobs_.front());
                jobs_.pop_front();
            }
            job();
        }
        g_runtime_worker = false;
    }

    int64_t parallelism_;
    std::mutex mu_;
    std::condition_variable cv_;
    bool stopping_ = false;
    std::deque<std::function<void()>> jobs_;
    std::vector<std::thread> workers_;

    std::mutex task_registry_mu_;
    std::unordered_set<TaskHandle *> live_tasks_;
    std::mutex channel_registry_mu_;
    std::unordered_set<ChannelHandle *> live_channels_;
};

// Intentionally heap-allocated and never deleted: avoids destructor
// running in forked child processes (death tests) where worker threads
// do not exist, which would cause thread::join to fail/abort.
RuntimeScheduler &scheduler() {
    static RuntimeScheduler *runtime = new RuntimeScheduler();
    return *runtime;
}

template <typename Predicate>
void waitWithWorkerHelp(std::unique_lock<std::mutex> &lock,
                        std::condition_variable &cv,
                        Predicate pred) {
    while (!pred()) {
        if (g_runtime_worker) {
            lock.unlock();
            const bool ran = scheduler().tryRunOne();
            lock.lock();
            if (ran)
                continue;
        }
        cv.wait(lock);
    }
}

} // namespace

extern "C" void *__ry_task_spawn(__ry_task_entry_fn entry, void *env, int64_t result_size) {
    auto *task = new TaskHandle;
    if (result_size > 0)
        task->result.resize(static_cast<size_t>(result_size));

    scheduler().registerTask(task);
    scheduler().submit([entry, env, task]() {
        std::unique_ptr<void, decltype(&std::free)> env_guard(env, &std::free);
        try {
            entry(env, task->result.empty() ? nullptr : task->result.data());
        } catch (...) {
            std::lock_guard<std::mutex> lock(task->mu);
            task->error = std::current_exception();
            task->done = true;
            task->cv.notify_all();
            return;
        }

        std::lock_guard<std::mutex> lock(task->mu);
        task->done = true;
        task->cv.notify_all();
    });
    return task;
}

extern "C" void __ry_task_join(void *task_ptr, void *out_buf) {
    auto *task = static_cast<TaskHandle *>(task_ptr);

    if (!scheduler().unregisterTask(task))
        throw std::runtime_error("runtime error: join() on already-joined task");

    std::unique_lock<std::mutex> lock(task->mu);
    waitWithWorkerHelp(lock, task->cv, [&]() { return task->done; });
    std::exception_ptr error = task->error;
    if (out_buf && !task->result.empty())
        std::memcpy(out_buf, task->result.data(), task->result.size());
    lock.unlock();

    delete task;
    if (error)
        std::rethrow_exception(error);
}

extern "C" void __ry_parallel_for_i64(int64_t begin, int64_t end, int64_t step,
                                      void *env, __ry_parallel_for_fn fn) {
    if (step == 0)
        return;

    std::unique_ptr<void, decltype(&std::free)> env_guard(env, &std::free);
    const int64_t total = iteration_count(begin, end, step);
    if (total <= 0)
        return;

    const int64_t workers = std::min<int64_t>(scheduler().parallelism(), total);
    if (workers <= 1) {
        fn(env, begin, end, step);
        return;
    }

    ParallelWaitState state;
    {
        std::lock_guard<std::mutex> lock(state.mu);
        state.pending = workers;
    }

    for (int64_t worker = 0; worker < workers; ++worker) {
        const int64_t start_iter = (total * worker) / workers;
        const int64_t end_iter = (total * (worker + 1)) / workers;
        if (start_iter == end_iter) {
            std::lock_guard<std::mutex> lock(state.mu);
            state.pending -= 1;
            continue;
        }

        const int64_t chunk_begin = begin + start_iter * step;
        const int64_t chunk_end = begin + end_iter * step;
        scheduler().submit([&, chunk_begin, chunk_end]() {
            try {
                fn(env, chunk_begin, chunk_end, step);
            } catch (...) {
                std::lock_guard<std::mutex> lock(state.mu);
                if (!state.error)
                    state.error = std::current_exception();
                state.pending -= 1;
                state.cv.notify_all();
                return;
            }

            std::lock_guard<std::mutex> lock(state.mu);
            state.pending -= 1;
            state.cv.notify_all();
        });
    }

    std::unique_lock<std::mutex> lock(state.mu);
    waitWithWorkerHelp(lock, state.cv, [&]() { return state.pending == 0; });
    std::exception_ptr error = state.error;
    lock.unlock();
    if (error)
        std::rethrow_exception(error);
}

extern "C" int64_t __ry_available_parallelism() {
    return scheduler().parallelism();
}

// NOTE: blocks the calling OS thread. When called from a spawned task,
// this occupies a worker thread for the full duration.
extern "C" void __ry_sleep(int64_t duration_ms) {
    if (duration_ms > 0)
        std::this_thread::sleep_for(std::chrono::milliseconds(duration_ms));
}

extern "C" void *__ry_channel_new(int64_t elem_size, int64_t capacity) {
    auto *ch = new ChannelHandle(
        elem_size <= 0 ? static_cast<size_t>(0) : static_cast<size_t>(elem_size),
        capacity);
    scheduler().registerChannel(ch);
    return ch;
}

extern "C" void __ry_channel_send(void *channel_ptr, void *value_ptr) {
    auto *channel = static_cast<ChannelHandle *>(channel_ptr);
    std::unique_lock<std::mutex> lock(channel->mu);

    if (channel->capacity > 0) {
        channel->cv.wait(lock, [&]() {
            return channel->closed || static_cast<int64_t>(channel->queue.size()) < channel->capacity;
        });
        if (channel->closed)
            throw std::runtime_error("runtime error: send() on closed channel");

        std::vector<unsigned char> item(channel->elem_size);
        if (channel->elem_size > 0)
            std::memcpy(item.data(), value_ptr, channel->elem_size);
        channel->queue.push_back(std::move(item));
        channel->cv.notify_all();
        lock.unlock();
        notifySelectWaiters();
        return;
    }

    channel->cv.wait(lock, [&]() {
        return channel->closed ||
               (!channel->has_rendezvous &&
                (channel->waiting_receivers + channel->waiting_select_receivers) > 0);
    });
    if (channel->closed)
        throw std::runtime_error("runtime error: send() on closed channel");

    if (channel->elem_size > 0)
        std::memcpy(channel->rendezvous.data(), value_ptr, channel->elem_size);
    channel->has_rendezvous = true;
    channel->cv.notify_all();
    lock.unlock();
    notifySelectWaiters();
    lock.lock();
    channel->cv.wait(lock, [&]() { return channel->closed || !channel->has_rendezvous; });
    if (channel->closed && channel->has_rendezvous)
        throw std::runtime_error("runtime error: send() on closed channel");
}

extern "C" bool __ry_channel_try_send(void *channel_ptr, void *value_ptr) {
    auto *channel = static_cast<ChannelHandle *>(channel_ptr);
    std::unique_lock<std::mutex> lock(channel->mu);

    if (channel->closed)
        return false;

    if (channel->capacity > 0) {
        if (static_cast<int64_t>(channel->queue.size()) >= channel->capacity)
            return false;

        std::vector<unsigned char> item(channel->elem_size);
        if (channel->elem_size > 0)
            std::memcpy(item.data(), value_ptr, channel->elem_size);
        channel->queue.push_back(std::move(item));
        channel->cv.notify_all();
        lock.unlock();
        notifySelectWaiters();
        return true;
    }

    if (channel->has_rendezvous ||
        (channel->waiting_receivers + channel->waiting_select_receivers) <= 0)
        return false;

    if (channel->elem_size > 0)
        std::memcpy(channel->rendezvous.data(), value_ptr, channel->elem_size);
    channel->has_rendezvous = true;
    channel->cv.notify_all();
    lock.unlock();
    notifySelectWaiters();
    return true;
}

extern "C" void __ry_channel_recv(void *channel_ptr, void *out_buf) {
    auto *channel = static_cast<ChannelHandle *>(channel_ptr);
    std::unique_lock<std::mutex> lock(channel->mu);

    if (channel->capacity > 0) {
        channel->cv.wait(lock, [&]() {
            return channel->closed || !channel->queue.empty();
        });
        if (channel->queue.empty())
            throw std::runtime_error("runtime error: recv() on closed empty channel");

        std::vector<unsigned char> item = std::move(channel->queue.front());
        channel->queue.pop_front();
        channel->cv.notify_all();
        lock.unlock();
        notifySelectWaiters();
        if (channel->elem_size > 0 && out_buf)
            std::memcpy(out_buf, item.data(), channel->elem_size);
        return;
    }

    channel->waiting_receivers += 1;
    channel->cv.notify_all();
    lock.unlock();
    notifySelectWaiters();
    lock.lock();
    channel->cv.wait(lock, [&]() {
        return channel->closed || channel->has_rendezvous;
    });
    channel->waiting_receivers -= 1;
    channel->cv.notify_all();
    lock.unlock();
    notifySelectWaiters();
    lock.lock();
    if (!channel->has_rendezvous)
        throw std::runtime_error("runtime error: recv() on closed empty channel");

    if (channel->elem_size > 0 && out_buf)
        std::memcpy(out_buf, channel->rendezvous.data(), channel->elem_size);
    channel->has_rendezvous = false;
    channel->cv.notify_all();
    lock.unlock();
    notifySelectWaiters();
}

extern "C" bool __ry_channel_try_recv(void *channel_ptr, void *out_buf) {
    auto *channel = static_cast<ChannelHandle *>(channel_ptr);
    std::unique_lock<std::mutex> lock(channel->mu);

    if (channel->capacity > 0) {
        if (channel->queue.empty())
            return false;

        std::vector<unsigned char> item = std::move(channel->queue.front());
        channel->queue.pop_front();
        channel->cv.notify_all();
        lock.unlock();
        notifySelectWaiters();
        if (channel->elem_size > 0 && out_buf)
            std::memcpy(out_buf, item.data(), channel->elem_size);
        return true;
    }

    if (!channel->has_rendezvous)
        return false;

    if (channel->elem_size > 0 && out_buf)
        std::memcpy(out_buf, channel->rendezvous.data(), channel->elem_size);
    channel->has_rendezvous = false;
    channel->cv.notify_all();
    lock.unlock();
    notifySelectWaiters();
    return true;
}

extern "C" bool __ry_channel_recv_opt(void *channel_ptr, void *out_buf) {
    auto *channel = static_cast<ChannelHandle *>(channel_ptr);
    std::unique_lock<std::mutex> lock(channel->mu);

    if (channel->capacity > 0) {
        channel->cv.wait(lock, [&]() {
            return channel->closed || !channel->queue.empty();
        });
        if (channel->queue.empty())
            return false;

        std::vector<unsigned char> item = std::move(channel->queue.front());
        channel->queue.pop_front();
        channel->cv.notify_all();
        lock.unlock();
        notifySelectWaiters();
        if (channel->elem_size > 0 && out_buf)
            std::memcpy(out_buf, item.data(), channel->elem_size);
        return true;
    }

    channel->waiting_receivers += 1;
    channel->cv.notify_all();
    lock.unlock();
    notifySelectWaiters();
    lock.lock();
    channel->cv.wait(lock, [&]() {
        return channel->closed || channel->has_rendezvous;
    });
    channel->waiting_receivers -= 1;
    channel->cv.notify_all();
    lock.unlock();
    notifySelectWaiters();
    lock.lock();
    if (!channel->has_rendezvous)
        return false;

    if (channel->elem_size > 0 && out_buf)
        std::memcpy(out_buf, channel->rendezvous.data(), channel->elem_size);
    channel->has_rendezvous = false;
    channel->cv.notify_all();
    lock.unlock();
    notifySelectWaiters();
    return true;
}

extern "C" void __ry_channel_close(void *channel_ptr) {
    auto *channel = static_cast<ChannelHandle *>(channel_ptr);
    std::lock_guard<std::mutex> lock(channel->mu);
    if (channel->closed)
        throw std::runtime_error("runtime error: close() on closed channel");
    channel->closed = true;
    channel->cv.notify_all();
    notifySelectWaiters();
}

extern "C" void *__ry_select_begin(int64_t case_count) {
    auto *state = new SelectState;
    if (case_count > 0)
        state->cases.reserve(static_cast<size_t>(case_count));
    return state;
}

extern "C" void __ry_select_add_recv(void *select_state_ptr, void *channel_ptr, void *out_buf) {
    auto *state = static_cast<SelectState *>(select_state_ptr);
    state->cases.push_back({SelectCaseKind::Recv, static_cast<ChannelHandle *>(channel_ptr), nullptr, out_buf, nullptr});
}

extern "C" void __ry_select_add_recv_opt(void *select_state_ptr, void *channel_ptr, void *out_buf, void *flag_ptr) {
    auto *state = static_cast<SelectState *>(select_state_ptr);
    state->cases.push_back({SelectCaseKind::RecvOpt, static_cast<ChannelHandle *>(channel_ptr), nullptr, out_buf, flag_ptr});
}

extern "C" void __ry_select_add_send(void *select_state_ptr, void *channel_ptr, void *value_ptr) {
    auto *state = static_cast<SelectState *>(select_state_ptr);
    state->cases.push_back({SelectCaseKind::Send, static_cast<ChannelHandle *>(channel_ptr), value_ptr, nullptr, nullptr});
}

extern "C" int64_t __ry_select_wait(void *select_state_ptr, int64_t default_index, int64_t timeout_ms, int64_t timeout_index) {
    auto *state = static_cast<SelectState *>(select_state_ptr);
    if (state->cases.empty())
        throw std::runtime_error("runtime error: select requires at least one case");
    if (timeout_index >= 0 && timeout_ms < 0)
        throw std::runtime_error("runtime error: select timeout must not be negative");

    static std::atomic<uint64_t> next_start{0};
    const bool has_timeout = timeout_index >= 0;
    const auto deadline = has_timeout
        ? std::chrono::steady_clock::now() + std::chrono::milliseconds(timeout_ms)
        : std::chrono::steady_clock::time_point{};

    while (true) {
        const uint64_t probe_epoch = g_select_epoch.load(std::memory_order_relaxed);
        const size_t count = state->cases.size();
        const size_t start = static_cast<size_t>(next_start.fetch_add(1, std::memory_order_relaxed) % count);
        int64_t ready_index = -1;

        for (size_t offset = 0; offset < count; ++offset) {
            const size_t idx = (start + offset) % count;
            const SelectCaseEntry &entry = state->cases[idx];
            std::lock_guard<std::mutex> lock(entry.channel->mu);
            if (selectCaseInvalid(entry)) {
                if (entry.kind == SelectCaseKind::Recv)
                    throw std::runtime_error("runtime error: recv() on closed empty channel");
                throw std::runtime_error("runtime error: send() on closed channel");
            }
            if (ready_index < 0 && selectCaseReady(entry))
                ready_index = static_cast<int64_t>(idx);
        }

        if (ready_index >= 0) {
            if (commitSelectCase(state->cases[static_cast<size_t>(ready_index)]))
                return ready_index;
            continue;
        }

        if (default_index >= 0)
            return default_index;

        if (has_timeout && std::chrono::steady_clock::now() >= deadline)
            return timeout_index;

        std::vector<ChannelHandle *> registered_channels;
        auto unregisterChannels = [&]() {
            for (ChannelHandle *channel : registered_channels) {
                std::lock_guard<std::mutex> lock(channel->mu);
                channel->waiting_select_receivers -= 1;
                channel->cv.notify_all();
            }
        };

        try {
            for (const SelectCaseEntry &entry : state->cases) {
                if ((entry.kind != SelectCaseKind::Recv && entry.kind != SelectCaseKind::RecvOpt) ||
                    entry.channel->capacity > 0)
                    continue;
                if (std::find(registered_channels.begin(), registered_channels.end(), entry.channel) != registered_channels.end())
                    continue;

                std::lock_guard<std::mutex> lock(entry.channel->mu);
                if (selectCaseInvalid(entry))
                    throw std::runtime_error("runtime error: recv() on closed empty channel");
                entry.channel->waiting_select_receivers += 1;
                entry.channel->cv.notify_all();
                registered_channels.push_back(entry.channel);
            }

            std::unique_lock<std::mutex> lock(g_select_mu);
            const uint64_t current_epoch = g_select_epoch.load(std::memory_order_relaxed);
            if (current_epoch != probe_epoch) {
                lock.unlock();
                unregisterChannels();
                continue;
            }
            if (has_timeout) {
                if (!g_select_cv.wait_until(lock, deadline, [&]() {
                        return g_select_epoch.load(std::memory_order_relaxed) != current_epoch;
                    })) {
                    lock.unlock();
                    unregisterChannels();
                    return timeout_index;
                }
            } else {
                g_select_cv.wait(lock, [&]() {
                    return g_select_epoch.load(std::memory_order_relaxed) != current_epoch;
                });
            }
            lock.unlock();
            unregisterChannels();
        } catch (...) {
            unregisterChannels();
            throw;
        }
    }
}

extern "C" void __ry_select_destroy(void *select_state_ptr) {
    delete static_cast<SelectState *>(select_state_ptr);
}
