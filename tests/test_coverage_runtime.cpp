// Test taxonomy: docs/reference/test-taxonomy.md
// Section header tags: [contract] / [regression #NNNN] / [internal].
// Per-test exceptions use an inline `// [regression: #NNNN]` comment.
// Whole-file dominant tag: [internal] — drives __ry_coverage_* runtime entry
// points directly with hand-built file/line pairs.

#include "ry/coverage/coverage_runtime.hpp"
#include <gtest/gtest.h>
#include <cstdio>
#include <string>


using namespace ry;
class CoverageRuntimeTest : public ::testing::Test {
protected:
    void SetUp() override {
        __ry_coverage_reset();
    }
    void TearDown() override {
        __ry_coverage_reset();
    }
};

TEST_F(CoverageRuntimeTest, RegisterAndHit) {
    __ry_coverage_register_line(0, 1);
    __ry_coverage_register_line(0, 2);
    __ry_coverage_register_line(0, 3);

    __ry_coverage_hit(0, 1);
    __ry_coverage_hit(0, 1);
    __ry_coverage_hit(0, 3);

    // Verify via summary output (capture stdout)
    const char *filenames[] = {"test.ry"};
    testing::internal::CaptureStdout();
    __ry_coverage_report_summary(filenames, 1);
    std::string output = testing::internal::GetCapturedStdout();

    EXPECT_NE(output.find("test.ry"), std::string::npos);
    EXPECT_NE(output.find("2/3"), std::string::npos);  // 2 out of 3 lines covered
    EXPECT_NE(output.find("66.7%"), std::string::npos);
}

TEST_F(CoverageRuntimeTest, MultipleFiles) {
    __ry_coverage_register_line(0, 1);
    __ry_coverage_register_line(0, 2);
    __ry_coverage_register_line(1, 1);

    __ry_coverage_hit(0, 1);
    __ry_coverage_hit(0, 2);
    __ry_coverage_hit(1, 1);

    const char *filenames[] = {"a.ry", "b.ry"};
    testing::internal::CaptureStdout();
    __ry_coverage_report_summary(filenames, 2);
    std::string output = testing::internal::GetCapturedStdout();

    EXPECT_NE(output.find("a.ry"), std::string::npos);
    EXPECT_NE(output.find("b.ry"), std::string::npos);
    EXPECT_NE(output.find("100.0%"), std::string::npos);
    EXPECT_NE(output.find("Total"), std::string::npos);
    EXPECT_NE(output.find("3/3"), std::string::npos);
}

TEST_F(CoverageRuntimeTest, ResetClearsData) {
    __ry_coverage_register_line(0, 1);
    __ry_coverage_hit(0, 1);
    __ry_coverage_reset();

    const char *filenames[] = {"test.ry"};
    testing::internal::CaptureStdout();
    __ry_coverage_report_summary(filenames, 1);
    std::string output = testing::internal::GetCapturedStdout();

    // After reset, no coverable lines -> no output
    EXPECT_EQ(output.find("test.ry"), std::string::npos);
}

TEST_F(CoverageRuntimeTest, NoFilesProducesNoOutput) {
    testing::internal::CaptureStdout();
    __ry_coverage_report_summary(nullptr, 0);
    std::string output = testing::internal::GetCapturedStdout();
    EXPECT_TRUE(output.empty());
}

TEST_F(CoverageRuntimeTest, SummaryHeader) {
    __ry_coverage_register_line(0, 1);
    __ry_coverage_hit(0, 1);

    const char *filenames[] = {"hello.ry"};
    testing::internal::CaptureStdout();
    __ry_coverage_report_summary(filenames, 1);
    std::string output = testing::internal::GetCapturedStdout();

    EXPECT_NE(output.find("Test Coverage Summary:"), std::string::npos);
}
