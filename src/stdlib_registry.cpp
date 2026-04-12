#include "ry/stdlib_registry.hpp"
#include <algorithm>
#include <cstring>


namespace ry {

StdlibRegistry &StdlibRegistry::instance() {
    static StdlibRegistry reg;
    return reg;
}

void StdlibRegistry::registerPackage(const StdlibPackageEntry &entry) {
    packages_.push_back(entry);
    sorted_ = false;
}

const std::vector<StdlibPackageEntry> &StdlibRegistry::packages() {
    if (!sorted_) {
        std::stable_sort(packages_.begin(), packages_.end(),
                         [](const StdlibPackageEntry &a,
                            const StdlibPackageEntry &b) {
                             if (a.priority != b.priority)
                                 return a.priority < b.priority;
                             return std::strcmp(a.package_name, b.package_name) < 0;
                         });
        sorted_ = true;
    }
    return packages_;
}

void StdlibRegistry::registerConstant(const char *name,
                                      NativeConstantEntry entry) {
    constants_.emplace(name, entry);
}

// --- ResourceKindRegistry ---

ResourceKindRegistry &ResourceKindRegistry::instance() {
    static ResourceKindRegistry reg;
    return reg;
}

int ResourceKindRegistry::registerKind(const char *typeName,
                                       const char *dtorName,
                                       const char *cleanupFnName,
                                       const char *library) {
    auto it = name_to_id_.find(typeName);
    if (it != name_to_id_.end())
        return it->second;
    int id = static_cast<int>(entries_.size());
    entries_.push_back({typeName, dtorName, cleanupFnName, library});
    name_to_id_.emplace(typeName, id);
    return id;
}

int ResourceKindRegistry::lookupByTypeName(const std::string &typeName) const {
    auto it = name_to_id_.find(typeName);
    return it != name_to_id_.end() ? it->second : NONE;
}

const ResourceKindRegistry::Info *ResourceKindRegistry::getInfo(int id) const {
    if (id < 0 || static_cast<size_t>(id) >= entries_.size())
        return nullptr;
    return &entries_[static_cast<size_t>(id)];
}

} // namespace ry
