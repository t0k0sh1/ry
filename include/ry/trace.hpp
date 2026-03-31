#pragma once

#include "ry/source_location.hpp"
#include <cstdint>
#include <initializer_list>
#include <string>
#include <variant>

namespace ry {

struct TraceField {
    std::string key;
    std::variant<std::string, int64_t, bool> value;

    TraceField(std::string k, const char *v) : key(std::move(k)), value(std::string(v ? v : "")) {}
    TraceField(std::string k, std::string v) : key(std::move(k)), value(std::move(v)) {}
    TraceField(std::string k, int v) : key(std::move(k)), value(static_cast<int64_t>(v)) {}
    TraceField(std::string k, int64_t v) : key(std::move(k)), value(v) {}
    TraceField(std::string k, bool v) : key(std::move(k)), value(v) {}
};

void configureTrace(bool enabled, const std::string &destination = "-");
bool traceEnabled();
void emitTraceEvent(const std::string &event, const std::string &phase,
                    const SourceLocation *loc = nullptr,
                    std::initializer_list<TraceField> fields = {});
void emitTraceDiagnostic(const std::string &event, const std::string &phase,
                         const SourceLocation *loc, const std::string &detail);

} // namespace ry

extern "C" {
void __ry_trace_function_enter(const char *fn, const char *file, int line, int col);
void __ry_trace_function_exit(const char *fn, const char *file, int line, int col);
void __ry_trace_return(const char *fn, const char *file, int line, int col);
void __ry_trace_branch_if(const char *file, int line, int col, int taken);
void __ry_trace_branch_when(const char *file, int line, int col, int arm_index);
}
