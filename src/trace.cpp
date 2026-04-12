#include "ry/trace.hpp"
#include <chrono>
#include <fstream>
#include <iostream>
#include <memory>
#include <mutex>
#include <sstream>
#include <thread>

namespace ry {

namespace {

class TraceRecorder {
public:
    void configure(bool enabled, const std::string &destination) {
        std::lock_guard<std::mutex> lock(mu_);
        enabled_ = enabled;
        destination_ = destination;
        seq_ = 0;
        file_stream_.reset();
        out_ = nullptr;

        if (!enabled_) return;

        if (destination.empty() || destination == "-") {
            out_ = &std::cerr;
            return;
        }

        file_stream_ = std::make_unique<std::ofstream>(destination, std::ios::out | std::ios::trunc);
        if (file_stream_ && file_stream_->is_open()) {
            out_ = file_stream_.get();
            return;
        }

        std::cerr << "Warning: --trace-out: cannot open '" << destination
                  << "', tracing disabled\n";
        enabled_ = false;
        out_ = nullptr;
    }

    bool enabled() const {
        std::lock_guard<std::mutex> lock(mu_);
        return enabled_ && out_ != nullptr;
    }

    void emit(const std::string &event, const std::string &phase,
              const SourceLocation *loc,
              std::initializer_list<TraceField> fields) {
        std::lock_guard<std::mutex> lock(mu_);
        if (!enabled_ || !out_) return;

        uint64_t seq = ++seq_;
        auto ts = std::chrono::duration_cast<std::chrono::microseconds>(
            std::chrono::system_clock::now().time_since_epoch()).count();
        std::ostringstream tid_ss;
        tid_ss << std::this_thread::get_id();
        std::string tid_str = tid_ss.str();

        *out_ << "{";
        writeKeyValue("event", event, true);
        writeKeyValue("phase", phase, false);
        writeKeyValue("ts_us", static_cast<int64_t>(ts), false);
        writeKeyValue("seq", static_cast<int64_t>(seq), false);
        writeKeyValue("tid", tid_str, false);

        if (loc && loc->isValid()) {
            writeKeyValue("line", static_cast<int64_t>(loc->line), false);
            writeKeyValue("col", static_cast<int64_t>(loc->col), false);
        }

        for (const auto &field : fields) {
            std::visit([&](const auto &value) {
                writeKeyValue(field.key, value, false);
            }, field.value);
        }

        *out_ << "}\n";
        out_->flush();
    }

private:
    template <typename T>
    void writeKeyValue(const std::string &key, const T &value, bool first) {
        if (!first) *out_ << ",";
        *out_ << "\"" << escapeJson(key) << "\":";
        writeValue(value);
    }

    void writeValue(const std::string &value) {
        *out_ << "\"" << escapeJson(value) << "\"";
    }

    void writeValue(const char *value) {
        writeValue(std::string(value ? value : ""));
    }

    void writeValue(int64_t value) {
        *out_ << value;
    }

    void writeValue(bool value) {
        *out_ << (value ? "true" : "false");
    }

    static std::string escapeJson(const std::string &value) {
        std::string out;
        out.reserve(value.size() + 8);
        for (char ch : value) {
            unsigned char c = static_cast<unsigned char>(ch);
            switch (c) {
                case '\\': out += "\\\\"; break;
                case '"': out += "\\\""; break;
                case '\n': out += "\\n"; break;
                case '\r': out += "\\r"; break;
                case '\t': out += "\\t"; break;
                default:
                    if (c < 0x20) {
                        static const char *hex = "0123456789abcdef";
                        out += "\\u00";
                        out += hex[(c >> 4) & 0x0f];
                        out += hex[c & 0x0f];
                    } else {
                        out += ch;
                    }
            }
        }
        return out;
    }

    mutable std::mutex mu_;
    bool enabled_ = false;
    uint64_t seq_ = 0;
    std::string destination_;
    std::unique_ptr<std::ofstream> file_stream_;
    std::ostream *out_ = nullptr;
};

TraceRecorder &recorder() {
    static TraceRecorder instance;
    return instance;
}

SourceLocation runtimeLoc(int line, int col) {
    SourceLocation loc;
    loc.line = line;
    loc.col = col;
    return loc;
}

} // namespace

void configureTrace(bool enabled, const std::string &destination) {
    recorder().configure(enabled, destination);
}

bool traceEnabled() {
    return recorder().enabled();
}

void emitTraceEvent(const std::string &event, const std::string &phase,
                    const SourceLocation *loc,
                    std::initializer_list<TraceField> fields) {
    recorder().emit(event, phase, loc, fields);
}

void emitTraceDiagnostic(const std::string &event, const std::string &phase,
                         const SourceLocation *loc, const std::string &detail) {
    recorder().emit(event, phase, loc, {TraceField("detail", detail)});
}

} // namespace ry

extern "C" void __ry_trace_function_enter(const char *fn, const char *file, int line, int col) {
    ry::SourceLocation loc = ry::runtimeLoc(line, col);
    ry::emitTraceEvent("call.enter", "runtime", &loc,
                       {ry::TraceField("fn", fn ? fn : ""),
                        ry::TraceField("file", file ? file : "")});
}

extern "C" void __ry_trace_function_exit(const char *fn, const char *file, int line, int col) {
    ry::SourceLocation loc = ry::runtimeLoc(line, col);
    ry::emitTraceEvent("call.exit", "runtime", &loc,
                       {ry::TraceField("fn", fn ? fn : ""),
                        ry::TraceField("file", file ? file : "")});
}

extern "C" void __ry_trace_return(const char *fn, const char *file, int line, int col) {
    ry::SourceLocation loc = ry::runtimeLoc(line, col);
    ry::emitTraceEvent("return", "runtime", &loc,
                       {ry::TraceField("fn", fn ? fn : ""),
                        ry::TraceField("file", file ? file : "")});
}

extern "C" void __ry_trace_branch_if(const char *file, int line, int col, int taken) {
    ry::SourceLocation loc = ry::runtimeLoc(line, col);
    ry::emitTraceEvent("branch.if", "runtime", &loc,
                       {ry::TraceField("file", file ? file : ""),
                        ry::TraceField("taken", taken != 0)});
}

extern "C" void __ry_trace_branch_when(const char *file, int line, int col, int arm_index) {
    ry::SourceLocation loc = ry::runtimeLoc(line, col);
    ry::emitTraceEvent("branch.when", "runtime", &loc,
                       {ry::TraceField("file", file ? file : ""),
                        ry::TraceField("arm_index", arm_index)});
}
