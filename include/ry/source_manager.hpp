#pragma once

#include <string>
#include <string_view>
#include <vector>


namespace ry {

class SourceManager {
public:
    int addSource(std::string filename, std::string content);
    std::string_view getLine(int fileId, int line) const;
    const std::string& getFilename(int fileId) const;
    int getFileCount() const { return static_cast<int>(files_.size()); }

private:
    struct SourceFile {
        std::string filename;
        std::string content;
        std::vector<size_t> lineOffsets;
    };
    std::vector<SourceFile> files_;

    void buildLineOffsets(SourceFile &sf);
};

} // namespace ry
