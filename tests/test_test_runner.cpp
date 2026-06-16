#include <gtest/gtest.h>

#include "ry/jit/test_runner.hpp"

#include <cstddef>
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
    if (hw > 0)
        EXPECT_EQ(static_cast<unsigned>(p), std::min(hw, 100u));
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
