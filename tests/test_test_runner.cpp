#include <gtest/gtest.h>

#include "ry/jit/test_runner.hpp"

#include <algorithm>
#include <cstddef>
#include <limits>
#include <thread>

TEST(ComputeParallelism, RequestedCappedByFileCount) {
    EXPECT_EQ(ry::computeParallelism(16, 4), 4);
}

TEST(ComputeParallelism, RequestedWithinFileCount) {
    EXPECT_EQ(ry::computeParallelism(4, 100), 4);
}

TEST(ComputeParallelism, ZeroRequestedUsesHardwareDefault) {
    int p = ry::computeParallelism(0, 100);
    EXPECT_GE(p, 1);
    EXPECT_LE(p, 100);
    unsigned hw = std::thread::hardware_concurrency();
    unsigned expected = std::min(ry::computeDefaultWorkers(hw), 100u);
    EXPECT_EQ(static_cast<unsigned>(p), expected);
}

TEST(ComputeParallelism, MinimumIsOneWithSingleFile) {
    EXPECT_EQ(ry::computeParallelism(8, 1), 1);
    EXPECT_EQ(ry::computeParallelism(1, 1), 1);
}

TEST(ComputeParallelism, MinimumIsOneWithZeroFiles) {
    EXPECT_EQ(ry::computeParallelism(0, 0), 1);
    EXPECT_EQ(ry::computeParallelism(8, 0), 1);
}

TEST(ComputeParallelism, LargeNCappedByFiles) {
    EXPECT_EQ(ry::computeParallelism(9999, 10), 10);
}

TEST(ComputeParallelism, RequestedOneAlwaysReturnsOne) {
    EXPECT_EQ(ry::computeParallelism(1, 100), 1);
}

TEST(ComputeParallelism, ExplicitRequestExceedsDefault) {
    unsigned hw = std::thread::hardware_concurrency();
    int requested = static_cast<int>(hw == 0 ? 8u : hw + 4u);
    std::size_t files = static_cast<std::size_t>(requested) + 10;
    EXPECT_EQ(ry::computeParallelism(requested, files), requested);
}

TEST(ComputeParallelism, ZeroRequestedSingleFileCapsToOne) {
    EXPECT_EQ(ry::computeParallelism(0, 1), 1);
}

TEST(ComputeParallelism, ExplicitAtDefaultBoundary) {
    unsigned hw = std::thread::hardware_concurrency();
    if (hw <= 1) GTEST_SKIP();
    int requested = static_cast<int>(hw - 1u);
    EXPECT_EQ(ry::computeParallelism(requested, 1000), requested);
}

TEST(ComputeParallelism, ExplicitJustAboveDefault) {
    unsigned hw = std::thread::hardware_concurrency();
    if (hw == 0) GTEST_SKIP();
    int requested = static_cast<int>(hw);
    EXPECT_EQ(ry::computeParallelism(requested, 1000), requested);
}

TEST(ComputeDefaultWorkers, ZeroReturnsOne) {
    EXPECT_EQ(ry::computeDefaultWorkers(0u), 1u);
}

TEST(ComputeDefaultWorkers, OneReturnsOne) {
    EXPECT_EQ(ry::computeDefaultWorkers(1u), 1u);
}

TEST(ComputeDefaultWorkers, TwoReturnsOne) {
    EXPECT_EQ(ry::computeDefaultWorkers(2u), 1u);
}

TEST(ComputeDefaultWorkers, ThreeReturnsTwo) {
    EXPECT_EQ(ry::computeDefaultWorkers(3u), 2u);
}

TEST(ComputeDefaultWorkers, EightReturnsSeven) {
    EXPECT_EQ(ry::computeDefaultWorkers(8u), 7u);
}

TEST(ComputeDefaultWorkers, LargeValueReturnsMinusOne) {
    EXPECT_EQ(ry::computeDefaultWorkers(100u), 99u);
}

TEST(ComputeDefaultWorkers, UintMaxReturnsMaxMinusOne) {
    constexpr unsigned umax = std::numeric_limits<unsigned>::max();
    EXPECT_EQ(ry::computeDefaultWorkers(umax), umax - 1u);
}
