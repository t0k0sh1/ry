#include "ry/dotenv.hpp"
#include <gtest/gtest.h>

#include <cstdlib>
#include <filesystem>
#include <fstream>
#include <unistd.h>

namespace fs = std::filesystem;

// ===== resolveRyEnv tests =====

TEST(RyEnv, ProductionAlias) {
    EXPECT_EQ(ry::resolveRyEnv("production"), "prod");
}

TEST(RyEnv, DevelopmentAlias) {
    EXPECT_EQ(ry::resolveRyEnv("development"), "dev");
}

TEST(RyEnv, ProdPassthrough) {
    EXPECT_EQ(ry::resolveRyEnv("prod"), "prod");
}

TEST(RyEnv, DevPassthrough) {
    EXPECT_EQ(ry::resolveRyEnv("dev"), "dev");
}

TEST(RyEnv, InternalPassthrough) {
    EXPECT_EQ(ry::resolveRyEnv("internal"), "internal");
}

TEST(RyEnv, StagingPassthrough) {
    EXPECT_EQ(ry::resolveRyEnv("staging"), "staging");
}

TEST(RyEnv, CustomPassthrough) {
    EXPECT_EQ(ry::resolveRyEnv("custom"), "custom");
}

// ===== parseDotEnv tests =====

TEST(DotEnv, BasicKeyValue) {
    auto result = ry::parseDotEnv("FOO=bar\nBAZ=qux\n");
    ASSERT_EQ(result.size(), 2u);
    EXPECT_EQ(result[0].first, "FOO");
    EXPECT_EQ(result[0].second, "bar");
    EXPECT_EQ(result[1].first, "BAZ");
    EXPECT_EQ(result[1].second, "qux");
}

TEST(DotEnv, CommentsSkipped) {
    auto result = ry::parseDotEnv("# this is a comment\nKEY=value\n");
    ASSERT_EQ(result.size(), 1u);
    EXPECT_EQ(result[0].first, "KEY");
    EXPECT_EQ(result[0].second, "value");
}

TEST(DotEnv, BlankLinesSkipped) {
    auto result = ry::parseDotEnv("\n\nKEY=value\n\n");
    ASSERT_EQ(result.size(), 1u);
    EXPECT_EQ(result[0].first, "KEY");
}

TEST(DotEnv, DoubleQuotedValue) {
    auto result = ry::parseDotEnv("KEY=\"hello world\"\n");
    ASSERT_EQ(result.size(), 1u);
    EXPECT_EQ(result[0].second, "hello world");
}

TEST(DotEnv, SingleQuotedValue) {
    auto result = ry::parseDotEnv("KEY='hello world'\n");
    ASSERT_EQ(result.size(), 1u);
    EXPECT_EQ(result[0].second, "hello world");
}

TEST(DotEnv, EmptyValue) {
    auto result = ry::parseDotEnv("KEY=\n");
    ASSERT_EQ(result.size(), 1u);
    EXPECT_EQ(result[0].first, "KEY");
    EXPECT_EQ(result[0].second, "");
}

TEST(DotEnv, ValueContainsEquals) {
    auto result = ry::parseDotEnv("KEY=a=b=c\n");
    ASSERT_EQ(result.size(), 1u);
    EXPECT_EQ(result[0].second, "a=b=c");
}

TEST(DotEnv, KeyWhitespaceTrimmed) {
    auto result = ry::parseDotEnv("  KEY  =value\n");
    ASSERT_EQ(result.size(), 1u);
    EXPECT_EQ(result[0].first, "KEY");
    EXPECT_EQ(result[0].second, "value");
}

TEST(DotEnv, CRLFHandled) {
    auto result = ry::parseDotEnv("KEY=value\r\n");
    ASSERT_EQ(result.size(), 1u);
    EXPECT_EQ(result[0].second, "value");
}

TEST(DotEnv, EmptyInput) {
    auto result = ry::parseDotEnv("");
    EXPECT_TRUE(result.empty());
}

TEST(DotEnv, MixedContent) {
    std::string content =
        "# Database config\n"
        "DB_HOST=localhost\n"
        "DB_PORT=5432\n"
        "\n"
        "# API\n"
        "API_KEY=\"secret-key-123\"\n"
        "EMPTY=\n"
        "QUOTED='single'\n";
    auto result = ry::parseDotEnv(content);
    ASSERT_EQ(result.size(), 5u);
    EXPECT_EQ(result[0].first, "DB_HOST");
    EXPECT_EQ(result[0].second, "localhost");
    EXPECT_EQ(result[1].first, "DB_PORT");
    EXPECT_EQ(result[1].second, "5432");
    EXPECT_EQ(result[2].first, "API_KEY");
    EXPECT_EQ(result[2].second, "secret-key-123");
    EXPECT_EQ(result[3].first, "EMPTY");
    EXPECT_EQ(result[3].second, "");
    EXPECT_EQ(result[4].first, "QUOTED");
    EXPECT_EQ(result[4].second, "single");
}

TEST(DotEnv, LineWithoutEqualsSkipped) {
    auto result = ry::parseDotEnv("NOEQUALSSIGN\nKEY=val\n");
    ASSERT_EQ(result.size(), 1u);
    EXPECT_EQ(result[0].first, "KEY");
}

// ===== loadDotEnv tests =====

class DotEnvLoadTest : public ::testing::Test {
protected:
    fs::path tmp_dir;

    void SetUp() override {
        tmp_dir = fs::temp_directory_path() /
            ("ry_dotenv_test_" + std::to_string(getpid()));
        fs::create_directories(tmp_dir);
    }

    void TearDown() override {
        fs::remove_all(tmp_dir);
    }
};

TEST_F(DotEnvLoadTest, LoadsEnvFile) {
    std::ofstream(tmp_dir / ".env") << "RY_TEST_DOTENV_A=hello\n";
    unsetenv("RY_TEST_DOTENV_A");

    ry::loadDotEnv(tmp_dir.string());

    const char *val = std::getenv("RY_TEST_DOTENV_A");
    ASSERT_NE(val, nullptr);
    EXPECT_STREQ(val, "hello");

    unsetenv("RY_TEST_DOTENV_A");
}

TEST_F(DotEnvLoadTest, DoesNotOverwriteExisting) {
    std::ofstream(tmp_dir / ".env") << "RY_TEST_DOTENV_B=new\n";
    setenv("RY_TEST_DOTENV_B", "existing", 1);

    ry::loadDotEnv(tmp_dir.string());

    EXPECT_STREQ(std::getenv("RY_TEST_DOTENV_B"), "existing");

    unsetenv("RY_TEST_DOTENV_B");
}

TEST_F(DotEnvLoadTest, NoErrorWhenFileAbsent) {
    // tmp_dir exists but has no .env — should not throw or crash
    EXPECT_NO_THROW(ry::loadDotEnv(tmp_dir.string()));
}

// ===== Environment-specific loadDotEnv tests =====

TEST_F(DotEnvLoadTest, LoadsEnvSpecificFile) {
    std::ofstream(tmp_dir / ".env.dev") << "RY_TEST_ENVSPEC_A=dev_val\n";
    unsetenv("RY_TEST_ENVSPEC_A");

    ry::loadDotEnv(tmp_dir.string(), "dev");

    const char *val = std::getenv("RY_TEST_ENVSPEC_A");
    ASSERT_NE(val, nullptr);
    EXPECT_STREQ(val, "dev_val");

    unsetenv("RY_TEST_ENVSPEC_A");
}

TEST_F(DotEnvLoadTest, EnvSpecificOverridesBase) {
    std::ofstream(tmp_dir / ".env") << "RY_TEST_ENVSPEC_B=base\n";
    std::ofstream(tmp_dir / ".env.dev") << "RY_TEST_ENVSPEC_B=dev\n";
    unsetenv("RY_TEST_ENVSPEC_B");

    ry::loadDotEnv(tmp_dir.string(), "dev");

    EXPECT_STREQ(std::getenv("RY_TEST_ENVSPEC_B"), "dev");

    unsetenv("RY_TEST_ENVSPEC_B");
}

TEST_F(DotEnvLoadTest, BaseLoadedWhenEnvSpecificMissing) {
    std::ofstream(tmp_dir / ".env") << "RY_TEST_ENVSPEC_C=base\n";
    unsetenv("RY_TEST_ENVSPEC_C");

    ry::loadDotEnv(tmp_dir.string(), "test");

    EXPECT_STREQ(std::getenv("RY_TEST_ENVSPEC_C"), "base");

    unsetenv("RY_TEST_ENVSPEC_C");
}

TEST_F(DotEnvLoadTest, ProdSkipsAllEnvFiles) {
    std::ofstream(tmp_dir / ".env") << "RY_TEST_ENVSPEC_D=base\n";
    std::ofstream(tmp_dir / ".env.prod") << "RY_TEST_ENVSPEC_D2=prod\n";
    unsetenv("RY_TEST_ENVSPEC_D");
    unsetenv("RY_TEST_ENVSPEC_D2");

    ry::loadDotEnv(tmp_dir.string(), "prod");

    EXPECT_EQ(std::getenv("RY_TEST_ENVSPEC_D"), nullptr);
    EXPECT_EQ(std::getenv("RY_TEST_ENVSPEC_D2"), nullptr);
}

TEST_F(DotEnvLoadTest, EmptyEnvNameLoadsOnlyBase) {
    std::ofstream(tmp_dir / ".env") << "RY_TEST_ENVSPEC_E=base\n";
    std::ofstream(tmp_dir / ".env.dev") << "RY_TEST_ENVSPEC_E=dev\n";
    unsetenv("RY_TEST_ENVSPEC_E");

    ry::loadDotEnv(tmp_dir.string(), "");

    EXPECT_STREQ(std::getenv("RY_TEST_ENVSPEC_E"), "base");

    unsetenv("RY_TEST_ENVSPEC_E");
}
