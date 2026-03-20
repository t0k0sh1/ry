#pragma once

struct SourceLocation {
    int line = 0;
    int col = 0;
    int file_id = 0;
    bool isValid() const { return line > 0; }
};
