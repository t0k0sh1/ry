#include "test_codegen_common.hpp"

// ===== Iterator tests =====

TEST_F(CodeGenTest, IteratorBasicToList) {
    std::string src =
        "let xs = [1, 2, 3]\n"
        "let ys = xs.iter().to_list()\n"
        "print(ys[0])\n"
        "print(ys[1])\n"
        "print(ys[2])\n"
        "print(len(ys))";
    EXPECT_EQ(runSource(src), "1\n2\n3\n3\n");
}

TEST_F(CodeGenTest, IteratorLazyFilter) {
    std::string src =
        "let xs = [1, 2, 3, 4, 5]\n"
        "let ys = xs.iter().filter(fn(x: int): x > 2).to_list()\n"
        "print(len(ys))\n"
        "print(ys[0])\n"
        "print(ys[1])\n"
        "print(ys[2])";
    EXPECT_EQ(runSource(src), "3\n3\n4\n5\n");
}

TEST_F(CodeGenTest, IteratorLazyMap) {
    std::string src =
        "let xs = [1, 2, 3]\n"
        "let ys = xs.iter().map(fn(x: int): x * 10).to_list()\n"
        "print(ys[0])\n"
        "print(ys[1])\n"
        "print(ys[2])";
    EXPECT_EQ(runSource(src), "10\n20\n30\n");
}

TEST_F(CodeGenTest, IteratorLazyTake) {
    std::string src =
        "let xs = [1, 2, 3, 4, 5]\n"
        "let ys = xs.iter().take(3).to_list()\n"
        "print(len(ys))\n"
        "print(ys[0])\n"
        "print(ys[2])";
    EXPECT_EQ(runSource(src), "3\n1\n3\n");
}

TEST_F(CodeGenTest, IteratorChained) {
    std::string src =
        "let xs = [1, 2, 3, 4, 5]\n"
        "let ys = xs.iter().filter(fn(x: int): x > 2).map(fn(x: int): x * 2).take(2).to_list()\n"
        "print(len(ys))\n"
        "print(ys[0])\n"
        "print(ys[1])";
    EXPECT_EQ(runSource(src), "2\n6\n8\n");
}

TEST_F(CodeGenTest, IteratorForLoop) {
    std::string src =
        "let xs = [10, 20, 30]\n"
        "var sum = 0\n"
        "for x in xs.iter():\n"
        "    sum = sum + x\n"
        "print(sum)";
    EXPECT_EQ(runSource(src), "60\n");
}

TEST_F(CodeGenTest, IteratorForLoopWithFilter) {
    std::string src =
        "let xs = [1, 2, 3, 4, 5]\n"
        "var sum = 0\n"
        "for x in xs.iter().filter(fn(x: int): x > 3):\n"
        "    sum = sum + x\n"
        "print(sum)";
    EXPECT_EQ(runSource(src), "9\n");
}

TEST_F(CodeGenTest, IteratorNext) {
    std::string src =
        "let xs = [10, 20]\n"
        "let it = xs.iter()\n"
        "let a = it.next()\n"
        "print(a)\n"
        "let b = it.next()\n"
        "print(b)\n"
        "let c = it.next()\n"
        "print(c)";
    EXPECT_EQ(runSource(src), "Some(10)\nSome(20)\nNone\n");
}

TEST_F(CodeGenTest, IteratorTakeZero) {
    std::string src =
        "let xs = [1, 2, 3]\n"
        "let ys = xs.iter().take(0).to_list()\n"
        "print(len(ys))";
    EXPECT_EQ(runSource(src), "0\n");
}

TEST_F(CodeGenTest, IteratorTakeMoreThanLength) {
    std::string src =
        "let xs = [1, 2, 3]\n"
        "let ys = xs.iter().take(10).to_list()\n"
        "print(len(ys))";
    EXPECT_EQ(runSource(src), "3\n");
}

TEST_F(CodeGenTest, IteratorSetToList) {
    std::string src =
        "let s = {1, 2, 3}\n"
        "let ys = s.iter().to_list()\n"
        "print(len(ys))";
    EXPECT_EQ(runSource(src), "3\n");
}

TEST_F(CodeGenTest, IteratorMapIterable) {
    std::string src =
        "let m = {\"a\": 1, \"b\": 2}\n"
        "var count = 0\n"
        "for k, v in m.iter():\n"
        "    count = count + 1\n"
        "print(count)";
    EXPECT_EQ(runSource(src), "2\n");
}

TEST_F(CodeGenTest, IteratorUFCS) {
    std::string src =
        "let xs = [1, 2, 3]\n"
        "let ys = iter(xs).to_list()\n"
        "print(len(ys))\n"
        "print(ys[0])";
    EXPECT_EQ(runSource(src), "3\n1\n");
}
