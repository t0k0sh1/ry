#include "ry/source_manager.hpp"


namespace ry {

void SourceManager::buildLineOffsets(SourceFile &sf) {
    sf.lineOffsets.clear();
    sf.lineOffsets.reserve(sf.content.size() / 40 + 1);
    sf.lineOffsets.push_back(0);
    for (size_t i = 0; i < sf.content.size(); ++i) {
        if (sf.content[i] == '\n') {
            sf.lineOffsets.push_back(i + 1);
        }
    }
}

int SourceManager::addSource(std::string filename, std::string content) {
    int id = static_cast<int>(files_.size());
    SourceFile sf;
    sf.filename = std::move(filename);
    sf.content = std::move(content);
    buildLineOffsets(sf);
    files_.push_back(std::move(sf));
    return id;
}

std::string_view SourceManager::getLine(int fileId, int line) const {
    if (fileId < 0 || static_cast<size_t>(fileId) >= files_.size())
        return "";
    const auto &sf = files_[static_cast<size_t>(fileId)];
    int idx = line - 1;
    if (idx < 0 || static_cast<size_t>(idx) >= sf.lineOffsets.size())
        return "";
    auto uidx = static_cast<size_t>(idx);
    size_t start = sf.lineOffsets[uidx];
    size_t end;
    if (uidx + 1 < sf.lineOffsets.size()) {
        end = sf.lineOffsets[uidx + 1];
    } else {
        end = sf.content.size();
    }
    // Strip trailing newline
    if (end > start && sf.content[end - 1] == '\n') --end;
    if (end > start && sf.content[end - 1] == '\r') --end;
    return std::string_view(sf.content).substr(start, end - start);
}

const std::string& SourceManager::getFilename(int fileId) const {
    static const std::string empty;
    if (fileId < 0 || static_cast<size_t>(fileId) >= files_.size())
        return empty;
    return files_[static_cast<size_t>(fileId)].filename;
}

} // namespace ry
