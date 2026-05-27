#include <gtest/gtest.h>
#include "ry/diagnostic/diagnostic.hpp"


using namespace ry;
TEST(DiagnosticTest, FormatWithSourceLocation) {
    SourceManager sm;
    int fid = sm.addSource("main.ry", "x = 42\ny = \nz = 0\n");

    Diagnostic diag;
    diag.level = DiagLevel::Error;
    diag.loc = {2, 9, fid};
    diag.message = "expected expression";
    diag.label = "expected expression";

    std::string result = formatDiagnostic(diag, &sm);

    EXPECT_NE(result.find("error: expected expression"), std::string::npos);
    EXPECT_NE(result.find("--> main.ry:2:9"), std::string::npos);
    EXPECT_NE(result.find("2 | y = "), std::string::npos);
    EXPECT_NE(result.find("^"), std::string::npos);
}

TEST(DiagnosticTest, FormatWithoutSourceManager) {
    Diagnostic diag;
    diag.level = DiagLevel::Error;
    diag.loc = {5, 3, 0};
    diag.message = "unknown variable 'x'";

    std::string result = formatDiagnostic(diag, nullptr);

    EXPECT_NE(result.find("error: unknown variable 'x'"), std::string::npos);
    EXPECT_EQ(result.find("-->"), std::string::npos);
}

TEST(DiagnosticTest, FormatWithoutValidLocation) {
    SourceManager sm;
    sm.addSource("test.ry", "hello\n");

    Diagnostic diag;
    diag.level = DiagLevel::Warning;
    diag.message = "unused variable";

    std::string result = formatDiagnostic(diag, &sm);

    EXPECT_NE(result.find("warning: unused variable"), std::string::npos);
    EXPECT_EQ(result.find("-->"), std::string::npos);
}

TEST(DiagnosticTest, DiagnosticErrorInheritsRuntimeError) {
    SourceManager sm;
    sm.addSource("test.ry", "x = \n");

    Diagnostic diag;
    diag.level = DiagLevel::Error;
    diag.loc = {1, 9, 0};
    diag.message = "expected expression";
    diag.label = "expected expression";

    DiagnosticError err(diag, &sm);

    // Must be catchable as std::runtime_error
    EXPECT_NO_THROW({
        try {
            throw err;
        } catch (const std::runtime_error &e) {
            EXPECT_NE(std::string(e.what()).find("error: expected expression"), std::string::npos);
        }
    });
}

TEST(DiagnosticTest, MultiDigitLineNumber) {
    SourceManager sm;
    std::string content;
    for (int i = 1; i <= 100; ++i)
        content += "line " + std::to_string(i) + "\n";
    sm.addSource("big.ry", content);

    Diagnostic diag;
    diag.level = DiagLevel::Error;
    diag.loc = {100, 1, 0};
    diag.message = "test error";

    std::string result = formatDiagnostic(diag, &sm);

    EXPECT_NE(result.find("--> big.ry:100:1"), std::string::npos);
    EXPECT_NE(result.find("100 | line 100"), std::string::npos);
}

TEST(SourceManagerTest, AddAndRetrieve) {
    SourceManager sm;
    int id = sm.addSource("hello.ry", "first line\nsecond line\nthird\n");

    EXPECT_EQ(id, 0);
    EXPECT_EQ(sm.getFilename(0), "hello.ry");
    EXPECT_EQ(sm.getLine(0, 1), "first line");
    EXPECT_EQ(sm.getLine(0, 2), "second line");
    EXPECT_EQ(sm.getLine(0, 3), "third");
}

TEST(SourceManagerTest, MultipleFiles) {
    SourceManager sm;
    int id0 = sm.addSource("a.ry", "aaa\n");
    int id1 = sm.addSource("b.ry", "bbb\n");

    EXPECT_EQ(id0, 0);
    EXPECT_EQ(id1, 1);
    EXPECT_EQ(sm.getLine(0, 1), "aaa");
    EXPECT_EQ(sm.getLine(1, 1), "bbb");
}

TEST(SourceManagerTest, OutOfBoundsReturnsEmpty) {
    SourceManager sm;
    sm.addSource("test.ry", "one\ntwo\n");

    EXPECT_EQ(sm.getLine(0, 0), "");
    EXPECT_EQ(sm.getLine(0, 99), "");
    EXPECT_EQ(sm.getLine(5, 1), "");
}
