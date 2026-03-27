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
thread_local void *g_current_task = nullptr;
thread_local int g_worker_id = -1;

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

struct WorkerQueue {
    std::mutex mu;
    std::deque<std::function<void()>> jobs;
};

class RuntimeScheduler {
public:
    RuntimeScheduler()
        : parallelism_(normalize_parallelism()),
          worker_queues_(static_cast<size_t>(parallelism_)) {
        workers_.reserve(static_cast<size_t>(parallelism_));
        for (int64_t i = 0; i < parallelism_; ++i)
            workers_.emplace_back([this, i]() { workerLoop(static_cast<int>(i)); });
    }

    ~RuntimeScheduler() {
        {
            std::lock_guard<std::mutex> lock(global_mu_);
            stopping_ = true;
        }
        global_cv_.notify_all();
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

    void submit(std::function<void()> job) {
        pending_jobs_.fetch_add(1, std::memory_order_relaxed);
        if (g_worker_id >= 0) {
            auto &wq = worker_queues_[static_cast<size_t>(g_worker_id)];
            {
                std::lock_guard<std::mutex> lock(wq.mu);
                wq.jobs.push_back(std::move(job));
            }
            global_cv_.notify_one();
            return;
        }
        size_t target = next_submit_.fetch_add(1, std::memory_order_relaxed)
                        % worker_queues_.size();
        {
            std::lock_guard<std::mutex> lock(worker_queues_[target].mu);
            worker_queues_[target].jobs.push_back(std::move(job));
        }
        global_cv_.notify_one();
    }

    bool tryRunOne() {
        if (g_worker_id >= 0) {
            auto &wq = worker_queues_[static_cast<size_t>(g_worker_id)];
            std::function<void()> job;
            {
                std::lock_guard<std::mutex> lock(wq.mu);
                if (!wq.jobs.empty()) {
                    job = std::move(wq.jobs.front());
                    wq.jobs.pop_front();
                    pending_jobs_.fetch_sub(1, std::memory_order_relaxed);
                }
            }
            if (job) { job(); return true; }
        }
        return trySteal();
    }

private:
    bool trySteal() {
        for (size_t i = 0; i < worker_queues_.size(); ++i) {
            if (static_cast<int>(i) == g_worker_id) continue;
            auto &wq = worker_queues_[i];
            std::function<void()> job;
            {
                std::lock_guard<std::mutex> lock(wq.mu);
                if (!wq.jobs.empty()) {
                    job = std::move(wq.jobs.back());
                    wq.jobs.pop_back();
                    pending_jobs_.fetch_sub(1, std::memory_order_relaxed);
                }
            }
            if (job) { job(); return true; }
        }
        return false;
    }

    void workerLoop(int id) {
        g_runtime_worker = true;
        g_worker_id = id;
        auto &local = worker_queues_[static_cast<size_t>(id)];

        while (true) {
            std::function<void()> job;

            {
                std::lock_guard<std::mutex> lock(local.mu);
                if (!local.jobs.empty()) {
                    job = std::move(local.jobs.front());
                    local.jobs.pop_front();
                    pending_jobs_.fetch_sub(1, std::memory_order_relaxed);
                }
            }

            if (!job) {
                for (size_t i = 0; i < worker_queues_.size() && !job; ++i) {
                    if (static_cast<int>(i) == id) continue;
                    auto &wq = worker_queues_[i];
                    std::lock_guard<std::mutex> lock(wq.mu);
                    if (!wq.jobs.empty()) {
                        job = std::move(wq.jobs.back());
                        wq.jobs.pop_back();
                        pending_jobs_.fetch_sub(1, std::memory_order_relaxed);
                    }
                }
            }

            if (job) {
                job();
                continue;
            }

            // No work found, wait for new submissions or shutdown
            std::unique_lock<std::mutex> lock(global_mu_);
            global_cv_.wait(lock, [this]() {
                return stopping_ || pending_jobs_.load(std::memory_order_relaxed) > 0;
            });
            if (stopping_ && pending_jobs_.load(std::memory_order_relaxed) == 0)
                break;
        }
        g_worker_id = -1;
        g_runtime_worker = false;
    }

    int64_t parallelism_;
    std::atomic<size_t> next_submit_{0};
    std::atomic<int64_t> pending_jobs_{0};
    std::mutex global_mu_;
    std::condition_variable global_cv_;
    bool stopping_ = false;
    std::vector<WorkerQueue> worker_queues_;
    std::vector<std::thread> workers_;

    std::mutex task_registry_mu_;
    std::unordered_set<TaskHandle *> live_tasks_;
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

inline TaskHandle *as_task(void *p) { return static_cast<TaskHandle*>(p); }

} // namespace

extern "C" void *__ry_task_spawn(__ry_task_entry_fn entry, void *env, int64_t result_size) {
    auto task = std::make_unique<TaskHandle>();
    if (result_size > 0)
        task->result.resize(static_cast<size_t>(result_size));

    TaskHandle *raw = task.get();
    scheduler().submit([entry, env, raw]() {
        std::unique_ptr<void, decltype(&std::free)> env_guard(env, &std::free);
        void *prev_task = g_current_task;
        g_current_task = raw;
        try {
            entry(env, raw->result.empty() ? nullptr : raw->result.data());
        } catch (...) {
            g_current_task = prev_task;
            std::lock_guard<std::mutex> lock(raw->mu);
            raw->error = std::current_exception();
            raw->done = true;
            raw->cv.notify_all();
            return;
        }

        g_current_task = prev_task;
        std::lock_guard<std::mutex> lock(raw->mu);
        raw->done = true;
        raw->cv.notify_all();
    });
    scheduler().registerTask(raw);
    return task.release();
}

extern "C" void __ry_task_join(void *task_ptr, void *out_buf) {
    auto *task = as_task(task_ptr);

    if (!scheduler().unregisterTask(task))
        throw std::runtime_error("runtime error: join() on already-joined task");

    // Take ownership only after confirming the task is still registered
    std::unique_ptr<TaskHandle> owned(task);

    std::unique_lock<std::mutex> lock(owned->mu);
    waitWithWorkerHelp(lock, owned->cv, [&]() { return owned->done; });
    std::exception_ptr error = owned->error;
    if (out_buf && !owned->result.empty())
        std::memcpy(out_buf, owned->result.data(), owned->result.size());
    lock.unlock();

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

extern "C" void __ry_sleep(int64_t duration_ms) {
    if (duration_ms <= 0)
        return;
    std::this_thread::sleep_for(std::chrono::milliseconds(duration_ms));
}
