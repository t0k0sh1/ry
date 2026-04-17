#include "test_codegen_common.hpp"


using namespace ry;
// ===== リスト型テスト =====

TEST_F(CodeGenTest, ListBasics) {
    // ListCreateAndAccess
    EXPECT_EQ(runSource("xs = [1, 2, 3]\nprint(xs[0])"), "1\n");
    // ListLen
    EXPECT_EQ(runSource("xs = [10, 20, 30]\nprint(length(xs))"), "3\n");
    // ListMixedAccess
    EXPECT_EQ(runSource("xs = [10, 20, 30]\nprint(xs[0])\nprint(xs[1])\nprint(xs[2])"), "10\n20\n30\n");
    // ListWithTypeAnnotation
    EXPECT_EQ(runSource("xs: List<int> = [1, 2, 3]\nprint(xs[0])\nprint(length(xs))"), "1\n3\n");
    // ListPrint
    EXPECT_EQ(runSource("xs = [1, 2, 3]\nprint(xs)"), "[1, 2, 3]\n");
    // ListFloatElements
    EXPECT_EQ(runSource("xs = [1.5, 2.5, 3.5]\nprint(xs[1])"), "2.5\n");
    // ListPrintFloat
    EXPECT_EQ(runSource("xs = [1.5, 2.5]\nprint(xs)"), "[1.5, 2.5]\n");
    // ListIndexWithExpression
    EXPECT_EQ(runSource("xs = [10, 20, 30]\ni = 1\nprint(xs[i])"), "20\n");
    // ListStringElements
    EXPECT_EQ(runSource("xs = [\"hello\", \"world\"]\nprint(xs[0])\nprint(xs[1])"), "hello\nworld\n");
}

TEST_F(CodeGenTest, ListInFunctions) {
    // ListInFunction
    {
        std::string src =
            "function head(xs: List<int>) -> int:\n"
            "    return xs[0]\n"
            "xs = [10, 20, 30]\n"
            "print(head(xs))";
        EXPECT_EQ(runSource(src), "10\n");
    }
    // ListLenInFunction
    {
        std::string src =
            "function size(xs: List<int>) -> int:\n"
            "    return length(xs)\n"
            "xs = [1, 2, 3, 4, 5]\n"
            "print(size(xs))";
        EXPECT_EQ(runSource(src), "5\n");
    }
}

TEST_F(CodeGenTest, ListIndexOutOfRange) {
    std::string src =
        "xs = [1, 2, 3]\n"
        "print(xs[5])";
    EXPECT_EXIT(runSource(src), ::testing::ExitedWithCode(1), "");
}

TEST_F(CodeGenTest, ListTypeMismatch) {
    EXPECT_THROW(runSource("xs = [1, 3.14]"), std::runtime_error);
}

TEST_F(CodeGenTest, ListEmpty) {
    EXPECT_THROW(runSource("xs = []"), std::runtime_error);
}

TEST_F(CodeGenTest, ListNegativeIndex) {
    // Negative index wraps around (Python-style): -1 → last element
    std::string src =
        "xs = [1, 2, 3]\n"
        "print(xs[-1])";
    EXPECT_EQ(runSource(src), "3\n");
}

TEST_F(CodeGenTest, ListNegativeIndexOutOfBounds) {
    // -4 wraps to -1 which is still out of bounds for a list of length 3
    std::string src =
        "xs = [1, 2, 3]\n"
        "print(xs[-4])";
    EXPECT_EXIT(runSource(src), ::testing::ExitedWithCode(1), "");
}

// ===== UFCS コード生成テスト =====

TEST_F(CodeGenTest, UFCSIntLiteral) {
    std::string src =
        "function double(x: int) -> int:\n"
        "    return x * 2\n"
        "print(5.double())";
    EXPECT_EQ(runSource(src), "10\n");
}

TEST_F(CodeGenTest, UFCSBasicCall) {
    std::string src =
        "function add(a: int, b: int) -> int:\n"
        "    return a + b\n"
        "x = 1\n"
        "print(x.add(2))";
    EXPECT_EQ(runSource(src), "3\n");
}

TEST_F(CodeGenTest, UFCSChainedCall) {
    std::string src =
        "function add(a: int, b: int) -> int:\n"
        "    return a + b\n"
        "function mul(a: int, b: int) -> int:\n"
        "    return a * b\n"
        "x = 2\n"
        "print(x.add(3).mul(4))";
    EXPECT_EQ(runSource(src), "20\n");
}

TEST_F(CodeGenTest, UFCSWithStruct) {
    std::string src =
        "record Point:\n"
        "    x: int\n"
        "    y: int\n"
        "function get_x(p: Point) -> int:\n"
        "    return p.x\n"
        "p = Point(42, 99)\n"
        "print(p.get_x())";
    EXPECT_EQ(runSource(src), "42\n");
}

// ===== Map型テスト =====

TEST_F(CodeGenTest, MapBasicOperations) {
    // MapCreateAndAccess
    EXPECT_EQ(runSource("m = {\"a\": 1, \"b\": 2}\nprint(m[\"a\"])"), "1\n");
    // MapAccessSecondKey
    EXPECT_EQ(runSource("m = {\"a\": 1, \"b\": 2}\nprint(m[\"b\"])"), "2\n");
    // MapLen
    EXPECT_EQ(runSource("m = {\"a\": 1, \"b\": 2}\nprint(length(m))"), "2\n");
    // MapPrint
    EXPECT_EQ(runSource("m = {\"a\": 1, \"b\": 2}\nprint(m)"), "{\"a\": 1, \"b\": 2}\n");
    // MapWithTypeAnnotation
    EXPECT_EQ(runSource("m: Map<str, int> = {\"x\": 42}\nprint(m[\"x\"])"), "42\n");
    // MapIntKeys
    EXPECT_EQ(runSource("m = {1: \"hello\", 2: \"world\"}\nprint(m[1])"), "hello\n");
    // MapInFunction
    {
        std::string src =
            "function get_val(m: Map<str, int>, k: str) -> int:\n"
            "    return m[k]\n"
            "m = {\"a\": 10, \"b\": 20}\n"
            "print(get_val(m, \"b\"))";
        EXPECT_EQ(runSource(src), "20\n");
    }
    // MapHasKey
    {
        std::string src =
            "m = {\"a\": 1, \"b\": 2}\n"
            "print(m.has_key(\"a\"))\n"
            "print(m.has_key(\"z\"))";
        EXPECT_EQ(runSource(src), "true\nfalse\n");
    }
}

TEST_F(CodeGenTest, MapKeyMutation) {
    // MapKeyAssign
    {
        std::string src =
            "m = {\"a\": 1, \"b\": 2}\n"
            "m[\"c\"] = 3\n"
            "print(m[\"c\"])\n"
            "print(length(m))";
        EXPECT_EQ(runSource(src), "3\n3\n");
    }
    // MapKeyUpdate
    {
        std::string src =
            "m = {\"a\": 1, \"b\": 2}\n"
            "m[\"a\"] = 99\n"
            "print(m[\"a\"])";
        EXPECT_EQ(runSource(src), "99\n");
    }
}

TEST_F(CodeGenTest, MapKeyNotFound) {
    std::string src =
        "m = {\"a\": 1}\n"
        "print(m[\"z\"])";
    EXPECT_EXIT(runSource(src), ::testing::ExitedWithCode(1), "");
}

TEST_F(CodeGenTest, MapTypeMismatch) {
    EXPECT_THROW(runSource("m = {\"a\": 1, \"b\": 3.14}"), std::runtime_error);
}

// ===== 文字列メソッドテスト =====

TEST_F(CodeGenTest, StringLength) {
    // StringLen
    EXPECT_EQ(runSource("print(length(\"hello\"))"), "5\n");
    // StringLenEmpty
    EXPECT_EQ(runSource("print(length(\"\"))"), "0\n");
    // StringLenUFCS
    EXPECT_EQ(runSource("s = \"hello\"\nprint(s.length())"), "5\n");
}

TEST_F(CodeGenTest, StringEquality) {
    // StringEqual
    {
        std::string src =
            "a = \"hello\"\n"
            "b = \"hello\"\n"
            "print(a == b)";
        EXPECT_EQ(runSource(src), "true\n");
    }
    // StringNotEqual
    {
        std::string src =
            "a = \"hello\"\n"
            "b = \"world\"\n"
            "print(a != b)";
        EXPECT_EQ(runSource(src), "true\n");
    }
    // StringEqualFalse
    EXPECT_EQ(runSource("print(\"abc\" == \"def\")"), "false\n");
    // StringNotEqualFalse
    EXPECT_EQ(runSource("print(\"abc\" != \"abc\")"), "false\n");
}

TEST_F(CodeGenTest, StringOrdering) {
    // StringLessThan
    EXPECT_EQ(runSource("print(\"apple\" < \"banana\")"), "true\n");
    // StringGreaterThan
    EXPECT_EQ(runSource("print(\"banana\" > \"apple\")"), "true\n");
    // StringLessEqual
    EXPECT_EQ(runSource("print(\"abc\" <= \"abc\")"), "true\n");
    // StringGreaterEqualFalse
    EXPECT_EQ(runSource("print(\"abc\" >= \"abd\")"), "false\n");
    // StringLessEqualSame
    {
        std::string src =
            "print(\"xyz\" <= \"xyz\")\n"
            "print(\"xyz\" >= \"xyz\")";
        EXPECT_EQ(runSource(src), "true\ntrue\n");
    }
    // StringLessThanFalse
    EXPECT_EQ(runSource("print(\"banana\" < \"apple\")"), "false\n");
}

TEST_F(CodeGenTest, StringConcatFn) {
    // StringConcat
    {
        std::string src =
            "a = \"hello\"\n"
            "b = \" world\"\n"
            "print(a + b)";
        EXPECT_EQ(runSource(src), "hello world\n");
    }
    // StringConcatLiterals
    EXPECT_EQ(runSource("print(\"foo\" + \"bar\")"), "foobar\n");
    // StringConcatEmpty
    EXPECT_EQ(runSource("print(\"hello\" + \"\")"), "hello\n");
}

TEST_F(CodeGenTest, StringContainsFn) {
    // StringContains
    {
        std::string src =
            "s = \"hello world\"\n"
            "print(contains(s, \"world\"))";
        EXPECT_EQ(runSource(src), "true\n");
    }
    // StringContainsFalse
    EXPECT_EQ(runSource("print(contains(\"hello\", \"xyz\"))"), "false\n");
    // StringContainsUFCS
    {
        std::string src =
            "s = \"hello world\"\n"
            "print(s.contains(\"hello\"))";
        EXPECT_EQ(runSource(src), "true\n");
    }
}

TEST_F(CodeGenTest, StringStartsWithFn) {
    // StringStartsWith
    EXPECT_EQ(runSource("print(starts_with(\"hello world\", \"hello\"))"), "true\n");
    // StringStartsWithFalse
    EXPECT_EQ(runSource("print(starts_with(\"hello world\", \"world\"))"), "false\n");
    // StringStartsWithUFCS
    {
        std::string src =
            "s = \"hello\"\n"
            "print(s.starts_with(\"hel\"))";
        EXPECT_EQ(runSource(src), "true\n");
    }
}

TEST_F(CodeGenTest, StringEndsWithFn) {
    // StringEndsWith
    EXPECT_EQ(runSource("print(ends_with(\"hello world\", \"world\"))"), "true\n");
    // StringEndsWithFalse
    EXPECT_EQ(runSource("print(ends_with(\"hello world\", \"hello\"))"), "false\n");
    // StringEndsWithSuffixTooLong
    EXPECT_EQ(runSource("print(ends_with(\"hi\", \"hello\"))"), "false\n");
    // StringEndsWithUFCS
    {
        std::string src =
            "s = \"hello\"\n"
            "print(s.ends_with(\"llo\"))";
        EXPECT_EQ(runSource(src), "true\n");
    }
}

TEST_F(CodeGenTest, StringContainsIgnoreCase) {
    EXPECT_EQ(runSource("print(contains(\"Hello World\", \"hello\", true))"), "true\n");
    EXPECT_EQ(runSource("print(contains(\"Hello World\", \"WORLD\", true))"), "true\n");
    EXPECT_EQ(runSource("print(contains(\"Hello World\", \"xyz\", true))"), "false\n");
    EXPECT_EQ(runSource("print(contains(\"Hello\", \"hello\", false))"), "false\n");
    // UFCS
    EXPECT_EQ(runSource("print(\"Hello\".contains(\"hello\", true))"), "true\n");
}

TEST_F(CodeGenTest, StringStartsWithIgnoreCase) {
    EXPECT_EQ(runSource("print(starts_with(\"Hello World\", \"hello\", true))"), "true\n");
    EXPECT_EQ(runSource("print(starts_with(\"Hello World\", \"HELLO\", true))"), "true\n");
    EXPECT_EQ(runSource("print(starts_with(\"Hello World\", \"world\", true))"), "false\n");
    // UFCS
    EXPECT_EQ(runSource("print(\"Hello\".starts_with(\"hel\", true))"), "true\n");
}

TEST_F(CodeGenTest, StringEndsWithIgnoreCase) {
    EXPECT_EQ(runSource("print(ends_with(\"Hello World\", \"world\", true))"), "true\n");
    EXPECT_EQ(runSource("print(ends_with(\"Hello World\", \"WORLD\", true))"), "true\n");
    EXPECT_EQ(runSource("print(ends_with(\"Hello World\", \"hello\", true))"), "false\n");
    EXPECT_EQ(runSource("print(ends_with(\"hi\", \"HELLO\", true))"), "false\n");
    // UFCS
    EXPECT_EQ(runSource("print(\"Hello\".ends_with(\"LLO\", true))"), "true\n");
}

TEST_F(CodeGenTest, StringToInt) {
    auto checkToInt = [&](const char *input, const char *expected) {
        std::string src = "case to_int(\"";
        src += input;
        src += "\"):\n    Ok(v):\n        print(v)\n    Err(e):\n        print(\"err\")";
        EXPECT_EQ(runSource(src), expected);
    };
    // Valid input returns Ok
    checkToInt("42", "42\n");
    checkToInt("-7", "-7\n");
    checkToInt("0", "0\n");
    // Invalid input returns Err
    checkToInt("abc", "err\n");
    checkToInt("", "err\n");
    checkToInt("12abc", "err\n");
    // Overflow returns Err
    checkToInt("9223372036854775808", "err\n");
    checkToInt("-9223372036854775809", "err\n");
    // UFCS
    EXPECT_EQ(runSource(R"(
s = "123"
case s.to_int():
    Ok(v):
        print(v)
    Err(e):
        print("err")
)"), "123\n");
}

TEST_F(CodeGenTest, StringToFloat) {
    auto checkToFloat = [&](const char *input, const char *expected) {
        std::string src = "case to_float(\"";
        src += input;
        src += "\"):\n    Ok(v):\n        print(v)\n    Err(e):\n        print(\"err\")";
        EXPECT_EQ(runSource(src), expected);
    };
    // Valid input returns Ok
    checkToFloat("3.14", "3.14\n");
    checkToFloat("42", "42.0\n");
    checkToFloat("-0.5", "-0.5\n");
    checkToFloat("0", "0.0\n");
    // Invalid input returns Err
    checkToFloat("abc", "err\n");
    checkToFloat("", "err\n");
    checkToFloat("1.2abc", "err\n");
    // Overflow returns Err
    checkToFloat("1e400", "err\n");
    // UFCS
    EXPECT_EQ(runSource(R"(
s = "2.5"
case s.to_float():
    Ok(v):
        print(v)
    Err(e):
        print("err")
)"), "2.5\n");
}

TEST_F(CodeGenTest, ToStrVariants) {
    // ToStrInt
    EXPECT_EQ(runSource("print(to_str(42))"), "42\n");
    // ToStrNegativeInt
    EXPECT_EQ(runSource("print(to_str(-7))"), "-7\n");
    // ToStrFloat
    EXPECT_EQ(runSource("print(to_str(3.14))"), "3.14\n");
    // ToStrBoolTrue
    EXPECT_EQ(runSource("print(to_str(true))"), "true\n");
    // ToStrBoolFalse
    EXPECT_EQ(runSource("print(to_str(false))"), "false\n");
    // ToStrStr
    EXPECT_EQ(runSource("print(to_str(\"hello\"))"), "hello\n");
    // ToStrUFCS
    EXPECT_EQ(runSource("x = 99\nprint(x.to_str())"), "99\n");
}

TEST_F(CodeGenTest, StringFind) {
    // FindBasic
    EXPECT_EQ(runSource("print(find(\"hello world\", \"world\"))"), "Some(6)\n");
    // FindNotFound
    EXPECT_EQ(runSource("print(find(\"hello\", \"xyz\"))"), "None\n");
    // FindAtStart
    EXPECT_EQ(runSource("print(find(\"hello\", \"hel\"))"), "Some(0)\n");
    // FindEmpty
    EXPECT_EQ(runSource("print(find(\"hello\", \"\"))"), "Some(0)\n");
    // FindUFCS
    EXPECT_EQ(runSource("s = \"abcdef\"\nprint(s.find(\"cd\"))"), "Some(2)\n");
}

TEST_F(CodeGenTest, StringSubstring) {
    // SubstringBasic
    EXPECT_EQ(runSource("print(substring(\"hello world\", 0, 5))"), "hello\n");
    // SubstringMiddle
    EXPECT_EQ(runSource("print(substring(\"hello world\", 6, 11))"), "world\n");
    // SubstringEmpty
    EXPECT_EQ(runSource("print(substring(\"hello\", 2, 2))"), "\n");
    // SubstringUFCS
    EXPECT_EQ(runSource("s = \"abcdef\"\nprint(s.substring(1, 4))"), "bcd\n");
}

TEST_F(CodeGenTest, StringCharAt) {
    // CharAtBasic
    EXPECT_EQ(runSource("print(char_at(\"hello\", 0))"), "h\n");
    // CharAtMiddle
    EXPECT_EQ(runSource("print(char_at(\"hello\", 4))"), "o\n");
    // CharAtUFCS
    EXPECT_EQ(runSource("s = \"abc\"\nprint(s.char_at(1))"), "b\n");
}

TEST_F(CodeGenTest, StringReplace) {
    // ReplaceBasic
    EXPECT_EQ(runSource("print(replace(\"hello world\", \"world\", \"ry\"))"), "hello ry\n");
    // ReplaceMultiple
    EXPECT_EQ(runSource("print(replace(\"aaa\", \"a\", \"bb\"))"), "bbbbbb\n");
    // ReplaceNotFound
    EXPECT_EQ(runSource("print(replace(\"hello\", \"xyz\", \"abc\"))"), "hello\n");
    // ReplaceEmpty
    EXPECT_EQ(runSource("print(replace(\"hello\", \"l\", \"\"))"), "heo\n");
    // ReplaceUFCS
    EXPECT_EQ(runSource("s = \"foo bar foo\"\nprint(s.replace(\"foo\", \"baz\"))"), "baz bar baz\n");
}

// ===== Phase 2: テキスト変換テスト =====

TEST_F(CodeGenTest, StringToUpper) {
    // ToUpperBasic
    EXPECT_EQ(runSource("print(to_upper(\"hello\"))"), "HELLO\n");
    // ToUpperMixed
    EXPECT_EQ(runSource("print(to_upper(\"Hello World\"))"), "HELLO WORLD\n");
    // ToUpperAlreadyUpper
    EXPECT_EQ(runSource("print(to_upper(\"ABC\"))"), "ABC\n");
    // ToUpperEmpty
    EXPECT_EQ(runSource("print(to_upper(\"\"))"), "\n");
    // ToUpperUFCS
    EXPECT_EQ(runSource("s = \"hi\"\nprint(s.to_upper())"), "HI\n");
}

TEST_F(CodeGenTest, StringToLower) {
    // ToLowerBasic
    EXPECT_EQ(runSource("print(to_lower(\"HELLO\"))"), "hello\n");
    // ToLowerMixed
    EXPECT_EQ(runSource("print(to_lower(\"Hello World\"))"), "hello world\n");
    // ToLowerAlreadyLower
    EXPECT_EQ(runSource("print(to_lower(\"abc\"))"), "abc\n");
    // ToLowerEmpty
    EXPECT_EQ(runSource("print(to_lower(\"\"))"), "\n");
    // ToLowerUFCS
    EXPECT_EQ(runSource("s = \"HI\"\nprint(s.to_lower())"), "hi\n");
}

TEST_F(CodeGenTest, StringTrim) {
    // TrimBasic
    EXPECT_EQ(runSource("print(trim(\"  hello  \"))"), "hello\n");
    // TrimTabs
    EXPECT_EQ(runSource("print(trim(\"\\thello\\t\"))"), "hello\n");
    // TrimNoWhitespace
    EXPECT_EQ(runSource("print(trim(\"hello\"))"), "hello\n");
    // TrimAllWhitespace
    EXPECT_EQ(runSource("print(trim(\"   \"))"), "\n");
    // TrimEmpty
    EXPECT_EQ(runSource("print(trim(\"\"))"), "\n");
    // TrimUFCS
    EXPECT_EQ(runSource("s = \"  hi  \"\nprint(s.trim())"), "hi\n");
}

TEST_F(CodeGenTest, StringTrimStartEnd) {
    // TrimStartBasic
    EXPECT_EQ(runSource("print(trim_start(\"  hello  \"))"), "hello  \n");
    // TrimStartNoWhitespace
    EXPECT_EQ(runSource("print(trim_start(\"hello\"))"), "hello\n");
    // TrimStartUFCS
    EXPECT_EQ(runSource("s = \"  hi\"\nprint(s.trim_start())"), "hi\n");
    // TrimEndBasic
    EXPECT_EQ(runSource("print(trim_end(\"  hello  \"))"), "  hello\n");
    // TrimEndNoWhitespace
    EXPECT_EQ(runSource("print(trim_end(\"hello\"))"), "hello\n");
    // TrimEndUFCS
    EXPECT_EQ(runSource("s = \"hi  \"\nprint(s.trim_end())"), "hi\n");
}

TEST_F(CodeGenTest, StringRepeatFn) {
    // RepeatBasic
    EXPECT_EQ(runSource("print(repeat(\"ab\", 3))"), "ababab\n");
    // RepeatOne
    EXPECT_EQ(runSource("print(repeat(\"hello\", 1))"), "hello\n");
    // RepeatZero
    EXPECT_EQ(runSource("print(repeat(\"hello\", 0))"), "\n");
    // RepeatUFCS
    EXPECT_EQ(runSource("s = \"ha\"\nprint(s.repeat(3))"), "hahaha\n");
    // RepeatNegative
    EXPECT_EQ(runSource("print(repeat(\"hello\", -1))"), "\n");
    // RepeatBoolTypeError
    EXPECT_THROW(runSource("print(repeat(\"a\", true))"), std::exception);
}

TEST_F(CodeGenTest, StringReverse) {
    // ReverseBasic
    EXPECT_EQ(runSource("print(reverse(\"hello\"))"), "olleh\n");
    // ReverseEmpty
    EXPECT_EQ(runSource("print(reverse(\"\"))"), "\n");
    // ReverseSingleChar
    EXPECT_EQ(runSource("print(reverse(\"a\"))"), "a\n");
    // ReversePalindrome
    EXPECT_EQ(runSource("print(reverse(\"abcba\"))"), "abcba\n");
    // ReverseUFCS
    EXPECT_EQ(runSource("s = \"abc\"\nprint(s.reverse())"), "cba\n");
}

// ===== Phase 3: List<str> 連携テスト =====

TEST_F(CodeGenTest, SplitVariants) {
    // SplitBasic
    {
        std::string src =
            "parts = split(\"a,b,c\", \",\")\n"
            "print(parts[0])\n"
            "print(parts[1])\n"
            "print(parts[2])";
        EXPECT_EQ(runSource(src), "a\nb\nc\n");
    }
    // SplitNoDelimiter
    {
        std::string src =
            "parts = split(\"hello\", \",\")\n"
            "print(length(parts))\n"
            "print(parts[0])";
        EXPECT_EQ(runSource(src), "1\nhello\n");
    }
    // SplitMultiCharDelim
    {
        std::string src =
            "parts = split(\"a::b::c\", \"::\")\n"
            "print(parts[0])\n"
            "print(parts[1])\n"
            "print(parts[2])";
        EXPECT_EQ(runSource(src), "a\nb\nc\n");
    }
    // SplitLen
    EXPECT_EQ(runSource("print(length(split(\"a,b,c,d\", \",\")))"), "4\n");
    // SplitUFCS
    {
        std::string src =
            "s = \"hello world\"\n"
            "parts = s.split(\" \")\n"
            "print(parts[0])\n"
            "print(parts[1])";
        EXPECT_EQ(runSource(src), "hello\nworld\n");
    }
    // SplitForLoop
    {
        std::string src =
            "for part in split(\"x,y,z\", \",\"):\n"
            "    print(part)";
        EXPECT_EQ(runSource(src), "x\ny\nz\n");
    }
    // SplitJoinRoundTrip
    {
        std::string src =
            "s = \"hello,world,ry\"\n"
            "print(join(split(s, \",\"), \",\"))";
        EXPECT_EQ(runSource(src), "hello,world,ry\n");
    }
}

TEST_F(CodeGenTest, JoinVariants) {
    // JoinBasic
    {
        std::string src =
            "parts = [\"a\", \"b\", \"c\"]\n"
            "print(join(parts, \",\"))";
        EXPECT_EQ(runSource(src), "a,b,c\n");
    }
    // JoinSingleElement
    {
        std::string src =
            "parts = [\"hello\"]\n"
            "print(join(parts, \",\"))";
        EXPECT_EQ(runSource(src), "hello\n");
    }
    // JoinWithSpace
    {
        std::string src =
            "words = [\"hello\", \"world\"]\n"
            "print(join(words, \" \"))";
        EXPECT_EQ(runSource(src), "hello world\n");
    }
    // JoinUFCS
    {
        std::string src =
            "parts = [\"x\", \"y\", \"z\"]\n"
            "print(parts.join(\"-\"))";
        EXPECT_EQ(runSource(src), "x-y-z\n");
    }
    // JoinUFCS_StringReceiver (Python-style: separator.join(list))
    {
        std::string src =
            "parts = [\"a\", \"b\", \"c\"]\n"
            "print(\",\".join(parts))";
        EXPECT_EQ(runSource(src), "a,b,c\n");
    }
    {
        std::string src =
            "words = [\"hello\", \"world\"]\n"
            "print(\" \".join(words))";
        EXPECT_EQ(runSource(src), "hello world\n");
    }
    {
        std::string src =
            "parts = [\"only\"]\n"
            "print(\",\".join(parts))";
        EXPECT_EQ(runSource(src), "only\n");
    }
}

// ===== 関数オーバーロードテスト =====

TEST_F(CodeGenTest, OverloadBasicResolution) {
    // OverloadByArgCount
    {
        std::string src =
            "function f(x: int) -> int:\n"
            "    return x\n"
            "function f(x: int, y: int) -> int:\n"
            "    return x + y\n"
            "print(f(10))\n"
            "print(f(3, 4))";
        EXPECT_EQ(runSource(src), "10\n7\n");
    }
    // OverloadByArgType
    {
        std::string src =
            "function f(x: int) -> int:\n"
            "    return x * 2\n"
            "function f(x: float) -> float:\n"
            "    return x * 3.0\n"
            "print(f(5))\n"
            "print(f(2.0))";
        EXPECT_EQ(runSource(src), "10\n6.0\n");
    }
    // OverloadDifferentReturn
    {
        std::string src =
            "function f(x: int) -> int:\n"
            "    return x\n"
            "function f(x: float) -> bool:\n"
            "    return x > 0.0\n"
            "print(f(42))\n"
            "print(f(1.5))";
        EXPECT_EQ(runSource(src), "42\ntrue\n");
    }
    // OverloadWithUFCS
    {
        std::string src =
            "function process(x: int) -> int:\n"
            "    return x * 2\n"
            "function process(x: int, y: int) -> int:\n"
            "    return x + y\n"
            "a = 5\n"
            "print(a.process())\n"
            "print(a.process(3))";
        EXPECT_EQ(runSource(src), "10\n8\n");
    }
    // OverloadRecursive
    {
        std::string src =
            "function fact(n: int) -> int:\n"
            "    if n <= 1:\n"
            "        return 1\n"
            "    return n * fact(n - 1)\n"
            "function fact(n: int, acc: int) -> int:\n"
            "    if n <= 1:\n"
            "        return acc\n"
            "    return fact(n - 1, acc * n)\n"
            "print(fact(5))\n"
            "print(fact(5, 1))";
        EXPECT_EQ(runSource(src), "120\n120\n");
    }
}

TEST_F(CodeGenTest, OverloadErrors) {
    // OverloadSameSignatureError
    {
        std::string src =
            "function f(x: int) -> int:\n"
            "    return x\n"
            "function f(x: int) -> int:\n"
            "    return x + 1\n";
        EXPECT_THROW(runSource(src), std::runtime_error);
    }
    // OverloadSameParamsDiffReturnError
    {
        std::string src =
            "function f(x: int) -> int:\n"
            "    return x\n"
            "function f(x: int) -> float:\n"
            "    return 1.0\n";
        EXPECT_THROW(runSource(src), std::runtime_error);
    }
    // OverloadNoMatchError
    {
        std::string src =
            "function f(x: int) -> int:\n"
            "    return x\n"
            "f(3.14)";
        EXPECT_THROW(runSource(src), std::runtime_error);
    }
    // OverloadSameSpecificityRemainsAmbiguous
    {
        std::string src =
            "function f(x: int | str) -> int:\n"
            "    return 1\n"
            "function f(x: int | float) -> int:\n"
            "    return 2\n"
            "f(42)";
        EXPECT_THROW(runSource(src), std::runtime_error);
    }
}

TEST_F(CodeGenTest, OverloadRanking) {
    // OverloadPrefersExactOverAny
    {
        std::string src =
            "function f(x: int) -> int:\n"
            "    return 1\n"
            "function f(x) -> int:\n"
            "    return 2\n"
            "print(f(42))\n"
            "print(f(\"hello\"))";
        EXPECT_EQ(runSource(src), "1\n2\n");
    }
    // OverloadPrefersUnionOverAny
    {
        std::string src =
            "function f(x: int | float) -> int:\n"
            "    return 1\n"
            "function f(x) -> int:\n"
            "    return 2\n"
            "print(f(42))\n"
            "print(f(3.14))\n"
            "print(f(\"hello\"))";
        EXPECT_EQ(runSource(src), "1\n1\n2\n");
    }
    // OverloadPrefersUnionAliasOverAny
    {
        std::string src =
            "type Num = int | float\n"
            "function f(x: Num) -> int:\n"
            "    return 1\n"
            "function f(x) -> int:\n"
            "    return 2\n"
            "print(f(42))\n"
            "print(f(3.14))\n"
            "print(f(\"hello\"))";
        EXPECT_EQ(runSource(src), "1\n1\n2\n");
    }
    // OverloadPrefersExactOverUnion
    {
        std::string src =
            "function f(x: int | float) -> int:\n"
            "    return 1\n"
            "function f(x: int) -> int:\n"
            "    return 2\n"
            "print(f(42))\n"
            "print(f(3.14))";
        EXPECT_EQ(runSource(src), "2\n1\n");
    }
}

TEST_F(CodeGenTest, OverloadSpecialArgs) {
    // OverloadWithNone
    {
        std::string src =
            "function f(x: Option<int>) -> int:\n"
            "    return 0\n"
            "function f(x: int) -> int:\n"
            "    return x\n"
            "print(f(None))\n"
            "print(f(42))";
        EXPECT_EQ(runSource(src), "0\n42\n");
    }
    // OverloadWrapsUnionAliasArgument
    {
        std::string src =
            "type Num = int | float\n"
            "function show(x: Num) -> Unit:\n"
            "    print(x)\n"
            "show(42)\n"
            "show(3.14)";
        EXPECT_EQ(runSource(src), "42\n3.14\n");
    }
}

// ===== Operator overloading =====

TEST_F(CodeGenTest, OperatorOverloadStruct) {
    // OperatorOverloadStructPlus
    {
        std::string src =
            "record Vec2:\n"
            "    x: int\n"
            "    y: int\n"
            "function operator+(a: Vec2, b: Vec2) -> Vec2:\n"
            "    return Vec2(a.x + b.x, a.y + b.y)\n"
            "v1 = Vec2(1, 2)\n"
            "v2 = Vec2(3, 4)\n"
            "v3 = v1 + v2\n"
            "print(v3.x)\n"
            "print(v3.y)";
        EXPECT_EQ(runSource(src), "4\n6\n");
    }
    // OperatorOverloadStructMinus
    {
        std::string src =
            "record Vec2:\n"
            "    x: int\n"
            "    y: int\n"
            "function operator-(a: Vec2, b: Vec2) -> Vec2:\n"
            "    return Vec2(a.x - b.x, a.y - b.y)\n"
            "v1 = Vec2(5, 8)\n"
            "v2 = Vec2(2, 3)\n"
            "v3 = v1 - v2\n"
            "print(v3.x)\n"
            "print(v3.y)";
        EXPECT_EQ(runSource(src), "3\n5\n");
    }
    // OperatorOverloadStructEq
    {
        std::string src =
            "record Point:\n"
            "    x: int\n"
            "    y: int\n"
            "function operator==(a: Point, b: Point) -> bool:\n"
            "    return a.x == b.x and a.y == b.y\n"
            "p1 = Point(1, 2)\n"
            "p2 = Point(1, 2)\n"
            "p3 = Point(3, 4)\n"
            "print(p1 == p2)\n"
            "print(p1 == p3)";
        EXPECT_EQ(runSource(src), "true\nfalse\n");
    }
    // OperatorOverloadUnaryMinus
    {
        std::string src =
            "record Vec2:\n"
            "    x: int\n"
            "    y: int\n"
            "function operator-(a: Vec2) -> Vec2:\n"
            "    return Vec2(0 - a.x, 0 - a.y)\n"
            "v = Vec2(3, 5)\n"
            "neg = -v\n"
            "print(neg.x)\n"
            "print(neg.y)";
        EXPECT_EQ(runSource(src), "-3\n-5\n");
    }
    // OperatorOverloadMultipleOps
    {
        std::string src =
            "record Vec2:\n"
            "    x: int\n"
            "    y: int\n"
            "function operator+(a: Vec2, b: Vec2) -> Vec2:\n"
            "    return Vec2(a.x + b.x, a.y + b.y)\n"
            "function operator*(a: Vec2, s: int) -> Vec2:\n"
            "    return Vec2(a.x * s, a.y * s)\n"
            "v = Vec2(1, 2)\n"
            "v2 = v * 3\n"
            "v3 = v + v2\n"
            "print(v3.x)\n"
            "print(v3.y)";
        EXPECT_EQ(runSource(src), "4\n8\n");
    }
}

// ===== Compound assignment operator overloading =====

TEST_F(CodeGenTest, CompoundAssignWithOperatorPlusEq) {
    // operator+= defined → called directly
    std::string src =
        "record Vec2:\n"
        "    x: int\n"
        "    y: int\n"
        "function operator+=(a: Vec2, b: Vec2) -> Vec2:\n"
        "    return Vec2(a.x + b.x, a.y + b.y)\n"
        "v = Vec2(1, 2)\n"
        "v += Vec2(3, 4)\n"
        "print(v.x)\n"
        "print(v.y)";
    EXPECT_EQ(runSource(src), "4\n6\n");
}

TEST_F(CodeGenTest, CompoundAssignFallbackToOperatorPlus) {
    // operator+= NOT defined, operator+ defined → fallback
    std::string src =
        "record Vec2:\n"
        "    x: int\n"
        "    y: int\n"
        "function operator+(a: Vec2, b: Vec2) -> Vec2:\n"
        "    return Vec2(a.x + b.x, a.y + b.y)\n"
        "v = Vec2(1, 2)\n"
        "v += Vec2(3, 4)\n"
        "print(v.x)\n"
        "print(v.y)";
    EXPECT_EQ(runSource(src), "4\n6\n");
}

TEST_F(CodeGenTest, CompoundAssignBuiltinStillWorks) {
    // Built-in int compound assignment
    std::string src =
        "x = 10\n"
        "x += 5\n"
        "x -= 3\n"
        "x *= 2\n"
        "print(x)";
    EXPECT_EQ(runSource(src), "24\n");
}

TEST_F(CodeGenTest, CompoundAssignPriorityOverPlus) {
    // operator+= takes priority over operator+ when both are defined
    std::string src =
        "record Counter:\n"
        "    val: int\n"
        "function operator+(a: Counter, b: Counter) -> Counter:\n"
        "    return Counter(a.val + b.val + 100)\n"
        "function operator+=(a: Counter, b: Counter) -> Counter:\n"
        "    return Counter(a.val + b.val)\n"
        "c = Counter(1)\n"
        "c += Counter(2)\n"
        "print(c.val)";
    EXPECT_EQ(runSource(src), "3\n");
}

TEST_F(CodeGenTest, CompoundAssignAllBuiltinOps) {
    std::string src =
        "x = 100\n"
        "x += 10\n"   // 110
        "x -= 10\n"   // 100
        "x *= 3\n"    // 300
        "x //= 7\n"   // 42
        "x %= 10\n"   // 2
        "print(x)\n"
        "z = 0b1100\n"
        "z &= 0b1010\n"  // 0b1000 = 8
        "z |= 0b0011\n"  // 0b1011 = 11
        "z ^= 0b0001\n"  // 0b1010 = 10
        "print(z)\n"
        "w = 1\n"
        "w <<= 3\n"   // 8
        "w >>= 1\n"   // 4
        "print(w)";
    EXPECT_EQ(runSource(src), "2\n10\n4\n");
}

TEST_F(CodeGenTest, IncrementDecrementStillWorks) {
    std::string src =
        "x = 5\n"
        "x++\n"
        "x++\n"
        "x--\n"
        "print(x)";
    EXPECT_EQ(runSource(src), "6\n");
}

// ===== List element assignment =====

TEST_F(CodeGenTest, ListIndexAssignTypes) {
    // ListIndexAssignInt
    {
        std::string src =
            "xs = [1, 2, 3]\n"
            "xs[0] = 99\n"
            "xs[1] = 88\n"
            "xs[2] = 77\n"
            "print(xs[0])\n"
            "print(xs[1])\n"
            "print(xs[2])";
        EXPECT_EQ(runSource(src), "99\n88\n77\n");
    }
    // ListIndexAssignFloat
    {
        std::string src =
            "xs = [1.0, 2.0, 3.0]\n"
            "xs[1] = 9.5\n"
            "print(xs[1])";
        EXPECT_EQ(runSource(src), "9.5\n");
    }
    // ListIndexAssignBool
    {
        std::string src =
            "xs = [true, false, true]\n"
            "xs[0] = false\n"
            "print(xs[0])";
        EXPECT_EQ(runSource(src), "false\n");
    }
    // ListIndexAssignStr
    {
        std::string src =
            "xs = [\"hello\", \"world\"]\n"
            "xs[0] = \"goodbye\"\n"
            "print(xs[0])";
        EXPECT_EQ(runSource(src), "goodbye\n");
    }
}

TEST_F(CodeGenTest, ListIndexAssignOutOfRange) {
    std::string src =
        "xs = [1, 2, 3]\n"
        "xs[5] = 99";
    EXPECT_EXIT(runSource(src), ::testing::ExitedWithCode(1), "");
}

TEST_F(CodeGenTest, ListIndexAssignNegative) {
    // Negative index wraps around for assignment too
    std::string src =
        "xs = [1, 2, 3]\n"
        "xs[-1] = 99\n"
        "print(xs[2])";
    EXPECT_EQ(runSource(src), "99\n");
}

TEST_F(CodeGenTest, ListIndexAssignNegativeOOB) {
    std::string src =
        "xs = [1, 2, 3]\n"
        "xs[-4] = 99";
    EXPECT_EXIT(runSource(src), ::testing::ExitedWithCode(1), "");
}

// ===== For loop =====

TEST_F(CodeGenTest, ForLoopVariants) {
    // ForLoopBasic
    {
        std::string src =
            "xs = [10, 20, 30]\n"
            "for x in xs:\n"
            "    print(x)";
        EXPECT_EQ(runSource(src), "10\n20\n30\n");
    }
    // ForLoopRange
    {
        std::string src =
            "for i in range(5):\n"
            "    print(i)";
        EXPECT_EQ(runSource(src), "0\n1\n2\n3\n4\n");
    }
    // ForLoopRangeStartEnd
    {
        std::string src =
            "for i in range(2, 5):\n"
            "    print(i)";
        EXPECT_EQ(runSource(src), "2\n3\n4\n");
    }
    // ForLoopAccumulate
    {
        std::string src =
            "xs = [1, 2, 3, 4, 5]\n"
            "sum = 0\n"
            "for x in xs:\n"
            "    sum = sum + x\n"
            "print(sum)";
        EXPECT_EQ(runSource(src), "15\n");
    }
    // ForLoopNested
    {
        std::string src =
            "for i in range(3):\n"
            "    for j in range(2):\n"
            "        print(i * 10 + j)";
        EXPECT_EQ(runSource(src), "0\n1\n10\n11\n20\n21\n");
    }
    // ForLoopStringList
    {
        std::string src =
            "words = [\"hello\", \"world\"]\n"
            "for w in words:\n"
            "    print(w)";
        EXPECT_EQ(runSource(src), "hello\nworld\n");
    }
}

// ===== Compound assignment =====

TEST_F(CodeGenTest, CompoundAssignBasic) {
    // CompoundAssignPlus
    EXPECT_EQ(runSource("x = 10\nx += 5\nprint(x)"), "15\n");
    // CompoundAssignMinus
    EXPECT_EQ(runSource("x = 10\nx -= 3\nprint(x)"), "7\n");
    // CompoundAssignStar
    EXPECT_EQ(runSource("x = 4\nx *= 3\nprint(x)"), "12\n");
    // CompoundAssignSlash
    EXPECT_EQ(runSource("x = 10.0\nx /= 4.0\nprint(x)"), "2.5\n");
    // CompoundAssignPercent
    EXPECT_EQ(runSource("x = 10\nx %= 3\nprint(x)"), "1\n");
    // CompoundAssignInLoop
    {
        std::string src =
            "sum = 0\n"
            "for i in range(1, 6):\n"
            "    sum += i\n"
            "print(sum)";
        EXPECT_EQ(runSource(src), "15\n");
    }
}

TEST_F(CodeGenTest, CompoundAssignExtended) {
    // CompoundAssignFloorDiv
    EXPECT_EQ(runSource("x = 7\nx //= 2\nprint(x)"), "3\n");
    // CompoundAssignPower
    EXPECT_EQ(runSource("x = 2.0\nx **= 3.0\nprint(x)"), "8.0\n");
    // CompoundAssignBitAnd
    EXPECT_EQ(runSource("x = 12\nx &= 10\nprint(x)"), "8\n");
    // CompoundAssignBitOr
    EXPECT_EQ(runSource("x = 12\nx |= 3\nprint(x)"), "15\n");
    // CompoundAssignBitXor
    EXPECT_EQ(runSource("x = 12\nx ^= 10\nprint(x)"), "6\n");
    // CompoundAssignLeftShift
    EXPECT_EQ(runSource("x = 1\nx <<= 3\nprint(x)"), "8\n");
    // CompoundAssignRightShift
    EXPECT_EQ(runSource("x = 16\nx >>= 2\nprint(x)"), "4\n");
}

// ===== in / not in: List・Map対応 =====

TEST_F(CodeGenTest, InNotInOperators) {
    // InListInt
    EXPECT_EQ(runSource("print(2 in [1, 2, 3])"), "true\n");
    // InListIntFalse
    EXPECT_EQ(runSource("print(4 in [1, 2, 3])"), "false\n");
    // InListStr
    EXPECT_EQ(runSource("print(\"a\" in [\"a\", \"b\"])"), "true\n");
    // NotInList
    EXPECT_EQ(runSource("print(4 not in [1, 2, 3])"), "true\n");
    // InMapKey
    EXPECT_EQ(runSource("print(\"a\" in {\"a\": 1})"), "true\n");
    // InMapKeyFalse
    EXPECT_EQ(runSource("print(\"c\" in {\"a\": 1})"), "false\n");
    // NotInMapKey
    EXPECT_EQ(runSource("print(\"b\" not in {\"a\": 1})"), "true\n");
}

TEST_F(CodeGenTest, InNotInOperatorsStr) {
    // InStrTrue
    EXPECT_EQ(runSource("print(\"world\" in \"hello world\")"), "true\n");
    // InStrFalse
    EXPECT_EQ(runSource("print(\"xyz\" in \"hello world\")"), "false\n");
    // NotInStrFalse
    EXPECT_EQ(runSource("print(\"world\" not in \"hello world\")"), "false\n");
    // NotInStrTrue
    EXPECT_EQ(runSource("print(\"xyz\" not in \"hello world\")"), "true\n");
    // EmptyNeedleInStr
    EXPECT_EQ(runSource("print(\"\" in \"hello\")"), "true\n");
    // StrInEmpty
    EXPECT_EQ(runSource("print(\"hello\" in \"\")"), "false\n");
    // FullMatchInStr
    EXPECT_EQ(runSource("print(\"hello\" in \"hello\")"), "true\n");
    // InStrRejectionNonStr
    EXPECT_THROW(runSource("print(1 in \"hello\")"), std::runtime_error);
    // InStrAnyWidening: any-typed LHS holding a str should work
    EXPECT_EQ(runSource("x: any = \"world\"\nprint(x in \"hello world\")"), "true\n");
}

// ===== List append / pop / reverse / slice =====

TEST_F(CodeGenTest, ListAppendVariants) {
    // ListAppend
    {
        std::string src =
            "xs = [1, 2]\n"
            "xs.append(3)\n"
            "print(xs)";
        EXPECT_EQ(runSource(src), "[1, 2, 3]\n");
    }
    // ListAppendString
    {
        std::string src =
            "xs = [\"a\"]\n"
            "xs.append(\"b\")\n"
            "print(xs)";
        EXPECT_EQ(runSource(src), "[\"a\", \"b\"]\n");
    }
}

TEST_F(CodeGenTest, ListPopVariants) {
    // ListPop
    {
        std::string src =
            "xs = [1, 2, 3]\n"
            "v = xs.pop()\n"
            "print(v)\n"
            "print(xs)";
        EXPECT_EQ(runSource(src), "Some(3)\n[1, 2]\n");
    }
    // ListPopEmpty
    {
        std::string src =
            "xs: List<int> = [1]\n"
            "ys = xs.filter((x: int) => x > 10)\n"
            "print(ys.pop())";
        EXPECT_EQ(runSource(src), "None\n");
    }
}

TEST_F(CodeGenTest, ListReverseVariants) {
    // ListReverse
    EXPECT_EQ(runSource("print(reverse([1, 2, 3]))"), "[3, 2, 1]\n");
    // ListReverseOriginal
    {
        std::string src =
            "xs = [1, 2, 3]\n"
            "ys = reverse(xs)\n"
            "print(xs)";
        EXPECT_EQ(runSource(src), "[1, 2, 3]\n");
    }
    // ListReverseStr (str reverse still works)
    EXPECT_EQ(runSource("print(reverse(\"hello\"))"), "olleh\n");
}

TEST_F(CodeGenTest, ListSliceVariants) {
    // ListSlice
    EXPECT_EQ(runSource("print(slice([1, 2, 3, 4, 5], 1, 3))"), "[2, 3]\n");
    // ListSliceClamp
    EXPECT_EQ(runSource("print(slice([1, 2, 3], 0, 100))"), "[1, 2, 3]\n");
    // ListSliceEmpty
    EXPECT_EQ(runSource("print(slice([1, 2, 3], 2, 2))"), "[]\n");
}

// ===== filter テスト =====

TEST_F(CodeGenTest, FilterBasics) {
    // FilterIntBasic
    {
        std::string src =
            "xs = [1, 2, 3, 4, 5]\n"
            "ys = filter(xs, (x: int) => x > 3)\n"
            "print(ys)";
        EXPECT_EQ(runSource(src), "[4, 5]\n");
    }
    // FilterIntEmpty
    {
        std::string src =
            "xs = [1, 2, 3]\n"
            "ys = filter(xs, (x: int) => x > 10)\n"
            "print(length(ys))";
        EXPECT_EQ(runSource(src), "0\n");
    }
    // FilterAllMatch
    {
        std::string src =
            "xs = [1, 2, 3]\n"
            "ys = filter(xs, (x: int) => x > 0)\n"
            "print(ys)";
        EXPECT_EQ(runSource(src), "[1, 2, 3]\n");
    }
    // FilterFloat
    {
        std::string src =
            "xs = [1.5, 2.5, 0.5]\n"
            "ys = filter(xs, (x: float) => x > 1.0)\n"
            "print(ys)";
        EXPECT_EQ(runSource(src), "[1.5, 2.5]\n");
    }
    // FilterString
    {
        std::string src =
            "xs = [\"hello\", \"hi\", \"hey\"]\n"
            "ys = filter(xs, (s: str) => s.starts_with(\"he\"))\n"
            "print(length(ys))";
        EXPECT_EQ(runSource(src), "2\n");
    }
}

TEST_F(CodeGenTest, FilterClosureAndUntyped) {
    // FilterWithClosure
    {
        std::string src =
            "threshold = 3\n"
            "xs = [1, 2, 3, 4, 5]\n"
            "ys = filter(xs, (x: int) => x > threshold)\n"
            "print(ys)";
        EXPECT_EQ(runSource(src), "[4, 5]\n");
    }
    // FilterUntypedLambdaParam
    {
        std::string src =
            "xs = [1, 2, 3, 4, 5]\n"
            "ys = filter(xs, (x) => x > 3)\n"
            "print(ys)";
        EXPECT_EQ(runSource(src), "[4, 5]\n");
    }
}

TEST_F(CodeGenTest, FilterUFCSAndErrors) {
    // FilterUFCS
    {
        std::string src =
            "xs = [1, 2, 3, 4, 5]\n"
            "ys = xs.filter((x: int) => x > 3)\n"
            "print(ys)";
        EXPECT_EQ(runSource(src), "[4, 5]\n");
    }
    // FilterStillRejectsTypedMismatch
    EXPECT_THROW(runSource(
        "xs = [1, 2, 3]\n"
        "ys = filter(xs, (x: str) => true)\n"
        "print(ys)"
    ), std::runtime_error);
}

// ===== map テスト =====

TEST_F(CodeGenTest, MapBasics) {
    // MapIntToInt
    {
        std::string src =
            "xs = [1, 2, 3]\n"
            "ys = map(xs, (x: int) => x * 2)\n"
            "print(ys)";
        EXPECT_EQ(runSource(src), "[2, 4, 6]\n");
    }
    // MapIntToFloat
    {
        std::string src =
            "xs = [1, 2, 3]\n"
            "ys = map(xs, (x: int) => x * 1.5)\n"
            "print(ys)";
        EXPECT_EQ(runSource(src), "[1.5, 3.0, 4.5]\n");
    }
    // MapStrToInt
    {
        std::string src =
            "xs = [\"hello\", \"hi\"]\n"
            "ys = map(xs, (s: str) => length(s))\n"
            "print(ys)";
        EXPECT_EQ(runSource(src), "[5, 2]\n");
    }
    // MapFloat
    {
        std::string src =
            "xs = [1.0, 2.0]\n"
            "ys = map(xs, (x: float) => x + 0.5)\n"
            "print(ys)";
        EXPECT_EQ(runSource(src), "[1.5, 2.5]\n");
    }
    // MapPreservesOriginal
    {
        std::string src =
            "xs = [1, 2, 3]\n"
            "ys = map(xs, (x: int) => x * 2)\n"
            "print(xs)\n"
            "print(ys)";
        EXPECT_EQ(runSource(src), "[1, 2, 3]\n[2, 4, 6]\n");
    }
}

TEST_F(CodeGenTest, MapClosureAndUntyped) {
    // MapWithClosure
    {
        std::string src =
            "factor = 10\n"
            "xs = [1, 2, 3]\n"
            "ys = map(xs, (x: int) => x * factor)\n"
            "print(ys)";
        EXPECT_EQ(runSource(src), "[10, 20, 30]\n");
    }
    // MapUntypedLambdaParam
    {
        std::string src =
            "xs = [1, 2, 3]\n"
            "ys = map(xs, (x) -> int => 7)\n"
            "print(ys)";
        EXPECT_EQ(runSource(src), "[7, 7, 7]\n");
    }
}

// ===== filter + map チェーンテスト =====

TEST_F(CodeGenTest, FilterMapChains) {
    // FilterThenMap
    {
        std::string src =
            "xs = [1, 2, 3, 4, 5]\n"
            "ys = xs.filter((x: int) => x > 2).map((x: int) => x * 10)\n"
            "print(ys)";
        EXPECT_EQ(runSource(src), "[30, 40, 50]\n");
    }
    // MapThenFilter
    {
        std::string src =
            "xs = [1, 2, 3]\n"
            "ys = xs.map((x: int) => x * 2).filter((x: int) => x > 4)\n"
            "print(ys)";
        EXPECT_EQ(runSource(src), "[6]\n");
    }
    // FilterThenFilter
    {
        std::string src =
            "xs = [1, 2, 3, 4, 5]\n"
            "ys = xs.filter((x: int) => x > 1).filter((x: int) => x < 5)\n"
            "print(ys)";
        EXPECT_EQ(runSource(src), "[2, 3, 4]\n");
    }
}

// ===== sort テスト =====

TEST_F(CodeGenTest, SortBasics) {
    // SortIntAsc
    {
        std::string src =
            "xs = [3, 1, 2]\n"
            "ys = sort(xs)\n"
            "print(ys)";
        EXPECT_EQ(runSource(src), "[1, 2, 3]\n");
    }
    // SortFloatAsc
    {
        std::string src =
            "xs = [3.0, 1.0, 2.0]\n"
            "ys = sort(xs)\n"
            "print(ys)";
        EXPECT_EQ(runSource(src), "[1.0, 2.0, 3.0]\n");
    }
    // SortStrAsc
    {
        std::string src =
            "xs = [\"banana\", \"apple\", \"cherry\"]\n"
            "ys = sort(xs)\n"
            "print(ys[0])\n"
            "print(ys[1])\n"
            "print(ys[2])";
        EXPECT_EQ(runSource(src), "apple\nbanana\ncherry\n");
    }
    // SortSingleElement
    {
        std::string src =
            "xs = [42]\n"
            "ys = sort(xs)\n"
            "print(ys)";
        EXPECT_EQ(runSource(src), "[42]\n");
    }
    // SortPreservesOriginal
    {
        std::string src =
            "xs = [3, 1, 2]\n"
            "ys = sort(xs)\n"
            "print(xs)\n"
            "print(ys)";
        EXPECT_EQ(runSource(src), "[3, 1, 2]\n[1, 2, 3]\n");
    }
    // SortAlreadySorted
    EXPECT_EQ(runSource("xs = [1, 2, 3, 4, 5]\nprint(sort(xs))"), "[1, 2, 3, 4, 5]\n");
    // SortReverseSorted
    EXPECT_EQ(runSource("xs = [5, 4, 3, 2, 1]\nprint(sort(xs))"), "[1, 2, 3, 4, 5]\n");
    // SortDuplicates
    EXPECT_EQ(runSource("xs = [3, 1, 2, 1, 3]\nprint(sort(xs))"), "[1, 1, 2, 3, 3]\n");
    // SortEmpty
    {
        std::string src =
            "xs: List<int> = [1]\n"
            "ys = xs.filter((x: int) => x > 10)\n"
            "print(sort(ys))";
        EXPECT_EQ(runSource(src), "[]\n");
    }
}

TEST_F(CodeGenTest, SortUFCSAndComparator) {
    // SortUFCS
    {
        std::string src =
            "xs = [3, 1, 2]\n"
            "ys = xs.sort()\n"
            "print(ys)";
        EXPECT_EQ(runSource(src), "[1, 2, 3]\n");
    }
    // SortWithComparator
    {
        std::string src =
            "xs = [3, 1, 2]\n"
            "ys = sort(xs, (a: int, b: int) => a > b)\n"
            "print(ys)";
        EXPECT_EQ(runSource(src), "[3, 2, 1]\n");
    }
    // SortWithComparatorUFCS
    {
        std::string src =
            "xs = [3, 1, 2]\n"
            "ys = xs.sort((a: int, b: int) => a > b)\n"
            "print(ys)";
        EXPECT_EQ(runSource(src), "[3, 2, 1]\n");
    }
}

// ===== filter + map + sort 全チェーンテスト =====

TEST_F(CodeGenTest, SortChains) {
    // FilterMapSort
    {
        std::string src =
            "xs = [5, 3, 1, 4, 2]\n"
            "ys = xs.filter((x: int) => x > 1).map((x: int) => x * 10).sort()\n"
            "print(ys)";
        EXPECT_EQ(runSource(src), "[20, 30, 40, 50]\n");
    }
    // SortThenFilter
    {
        std::string src =
            "xs = [3, 1, 2]\n"
            "ys = xs.sort().filter((x: int) => x > 1)\n"
            "print(ys)";
        EXPECT_EQ(runSource(src), "[2, 3]\n");
    }
    // SortThenMap
    {
        std::string src =
            "xs = [3, 1, 2]\n"
            "ys = xs.sort().map((x: int) => x * 2)\n"
            "print(ys)";
        EXPECT_EQ(runSource(src), "[2, 4, 6]\n");
    }
}

// ===== TimSort / Hybrid Sort テスト =====

TEST_F(CodeGenTest, SortLargeAndTimSort) {
    // SortLargerList
    {
        std::string src =
            "xs = [15, 3, 9, 1, 14, 7, 20, 5, 18, 2, 12, 8, 16, 4, 11, 6, 19, 10, 17, 13]\n"
            "print(sort(xs))";
        EXPECT_EQ(runSource(src), "[1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20]\n");
    }
    // SortExactly2Elements
    EXPECT_EQ(runSource("xs = [2, 1]\nprint(sort(xs))"), "[1, 2]\n");
    // SortExactly32Elements
    {
        std::string src =
            "xs = [32, 31, 30, 29, 28, 27, 26, 25, 24, 23, 22, 21, 20, 19, 18, 17, "
            "16, 15, 14, 13, 12, 11, 10, 9, 8, 7, 6, 5, 4, 3, 2, 1]\n"
            "print(sort(xs))";
        EXPECT_EQ(runSource(src), "[1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, "
                  "17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32]\n");
    }
    // SortExactly33Elements
    {
        std::string src =
            "xs = [33, 32, 31, 30, 29, 28, 27, 26, 25, 24, 23, 22, 21, 20, 19, 18, 17, "
            "16, 15, 14, 13, 12, 11, 10, 9, 8, 7, 6, 5, 4, 3, 2, 1]\n"
            "print(sort(xs))";
        EXPECT_EQ(runSource(src), "[1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, "
                  "17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 33]\n");
    }
    // SortLargeListWithDuplicates
    {
        std::string src =
            "xs = [5, 3, 8, 1, 9, 2, 7, 4, 6, 10, "
            "5, 3, 8, 1, 9, 2, 7, 4, 6, 10, "
            "5, 3, 8, 1, 9, 2, 7, 4, 6, 10, "
            "5, 3, 8, 1, 9, 2, 7, 4, 6, 10, "
            "5, 3, 8, 1, 9, 2, 7, 4, 6, 10]\n"
            "ys = sort(xs)\n"
            "print(length(ys))\n"
            "print(ys[0])\n"
            "print(ys[49])";
        EXPECT_EQ(runSource(src), "50\n1\n10\n");
    }
}

TEST_F(CodeGenTest, SortStability) {
    // SortStability
    {
        std::string src =
            "xs = [201, 102, 303, 104, 205]\n"
            "ys = sort(xs, (a: int, b: int) => a / 100 < b / 100)\n"
            "print(ys)";
        EXPECT_EQ(runSource(src), "[102, 104, 201, 205, 303]\n");
    }
    // SortStabilityLarger
    {
        std::string src =
            "xs = range(40).map((i: int) => (i % 5) * 1000 + i)\n"
            "ys = sort(xs, (a: int, b: int) => a / 1000 < b / 1000)\n"
            "stable = true\n"
            "for i in range(1, 40):\n"
            "    ka = ys[i - 1] / 1000\n"
            "    kb = ys[i] / 1000\n"
            "    if ka == kb:\n"
            "        if ys[i - 1] % 1000 > ys[i] % 1000:\n"
            "            stable = false\n"
            "print(stable)";
        EXPECT_EQ(runSource(src), "true\n");
    }
}

// ===== range() ステップ対応 =====

TEST_F(CodeGenTest, RangeStepVariants) {
    // RangeStep
    {
        std::string src =
            "for i in range(0, 10, 2):\n"
            "    print(i)";
        EXPECT_EQ(runSource(src), "0\n2\n4\n6\n8\n");
    }
    // RangeStepNegative
    {
        std::string src =
            "for i in range(10, 0, -1):\n"
            "    print(i)";
        EXPECT_EQ(runSource(src), "10\n9\n8\n7\n6\n5\n4\n3\n2\n1\n");
    }
    // RangeStepNegative3
    {
        std::string src =
            "for i in range(10, 0, -3):\n"
            "    print(i)";
        EXPECT_EQ(runSource(src), "10\n7\n4\n1\n");
    }
    // RangeStepEmpty
    {
        std::string src =
            "for i in range(0, 10, -1):\n"
            "    print(i)";
        EXPECT_EQ(runSource(src), "");
    }
    // RangeStepList
    {
        std::string src =
            "xs = range(0, 10, 3)\n"
            "print(length(xs))";
        EXPECT_EQ(runSource(src), "4\n");
    }
}

// ===== reduce / fold =====

TEST_F(CodeGenTest, ReduceVariants) {
    // ReduceBasic
    {
        std::string src =
            "xs = [1, 2, 3, 4]\n"
            "sum = reduce(xs, (a: int, b: int) => a + b)\n"
            "print(sum)";
        EXPECT_EQ(runSource(src), "10\n");
    }
    // ReduceUFCS
    {
        std::string src =
            "xs = [1, 2, 3, 4]\n"
            "print(xs.reduce((a: int, b: int) => a + b))";
        EXPECT_EQ(runSource(src), "10\n");
    }
    // ReduceUntypedLambdaParams
    {
        std::string src =
            "xs = [1, 2, 3, 4]\n"
            "sum = reduce(xs, (a, b) -> int => 99)\n"
            "print(sum)";
        EXPECT_EQ(runSource(src), "99\n");
    }
}

TEST_F(CodeGenTest, FoldVariants) {
    // FoldBasic
    {
        std::string src =
            "xs = [1, 2, 3]\n"
            "sum = fold(xs, 10, (a: int, b: int) => a + b)\n"
            "print(sum)";
        EXPECT_EQ(runSource(src), "16\n");
    }
    // FoldUFCS
    {
        std::string src =
            "xs = [1, 2, 3]\n"
            "print(xs.fold(0, (a: int, b: int) => a + b))";
        EXPECT_EQ(runSource(src), "6\n");
    }
}

// ===== keys / values / items =====

TEST_F(CodeGenTest, MapKeys) {
    std::string src =
        "m = {\"a\": 1, \"b\": 2}\n"
        "ks = keys(m)\n"
        "print(length(ks))";
    EXPECT_EQ(runSource(src), "2\n");
}

TEST_F(CodeGenTest, MapValues) {
    std::string src =
        "m = {\"a\": 1, \"b\": 2}\n"
        "vs = values(m)\n"
        "print(length(vs))";
    EXPECT_EQ(runSource(src), "2\n");
}

// ===== any / all =====

TEST_F(CodeGenTest, AnyAllPredicates) {
    // AnyTrue
    {
        std::string src =
            "xs = [1, 2, 3, 4]\n"
            "print(any(xs, (x: int) => x > 3))";
        EXPECT_EQ(runSource(src), "true\n");
    }
    // AnyFalse
    {
        std::string src =
            "xs = [1, 2, 3]\n"
            "print(any(xs, (x: int) => x > 10))";
        EXPECT_EQ(runSource(src), "false\n");
    }
    // AllTrue
    {
        std::string src =
            "xs = [2, 4, 6]\n"
            "print(all(xs, (x: int) => x > 0))";
        EXPECT_EQ(runSource(src), "true\n");
    }
    // AllFalse
    {
        std::string src =
            "xs = [2, 4, 6]\n"
            "print(all(xs, (x: int) => x > 3))";
        EXPECT_EQ(runSource(src), "false\n");
    }
}

// ===== sum / min / max =====

TEST_F(CodeGenTest, SumMinMax) {
    // SumInt
    {
        std::string src =
            "xs = [1, 2, 3, 4, 5]\n"
            "print(sum(xs))";
        EXPECT_EQ(runSource(src), "15\n");
    }
    // MinInt
    {
        std::string src =
            "xs = [3, 1, 4, 1, 5]\n"
            "print(min(xs))";
        EXPECT_EQ(runSource(src), "1\n");
    }
    // MaxInt
    {
        std::string src =
            "xs = [3, 1, 4, 1, 5]\n"
            "print(max(xs))";
        EXPECT_EQ(runSource(src), "5\n");
    }
}

// ===== enumerate / zip =====

TEST_F(CodeGenTest, EnumerateBasic) {
    std::string src =
        "xs = [10, 20, 30]\n"
        "es = enumerate(xs)\n"
        "print(length(es))";
    EXPECT_EQ(runSource(src), "3\n");
}

TEST_F(CodeGenTest, ZipBasic) {
    std::string src =
        "xs = [1, 2, 3]\n"
        "ys = [\"a\", \"b\", \"c\"]\n"
        "zs = zip(xs, ys)\n"
        "print(length(zs))";
    EXPECT_EQ(runSource(src), "3\n");
}

// ===== first / last / is_empty =====

TEST_F(CodeGenTest, FirstLastIsEmpty) {
    // FirstBasic
    EXPECT_EQ(runSource("print(first([10, 20, 30]))"), "Some(10)\n");
    // LastBasic
    EXPECT_EQ(runSource("print(last([10, 20, 30]))"), "Some(30)\n");
    // IsEmptyFalse
    EXPECT_EQ(runSource("print(is_empty([1, 2]))"), "false\n");
    // IsEmptyTrue
    {
        std::string src =
            "xs: List<int> = [1]\n"
            "ys = xs.filter((x: int) => x > 10)\n"
            "print(is_empty(ys))";
        EXPECT_EQ(runSource(src), "true\n");
    }
}

// ===== insert(list, i, val) =====

TEST_F(CodeGenTest, ListInsert) {
    std::string src =
        "xs = [1, 3, 4]\n"
        "insert(xs, 1, 2)\n"
        "print(xs[0])\n"
        "print(xs[1])\n"
        "print(xs[2])\n"
        "print(length(xs))";
    EXPECT_EQ(runSource(src), "1\n2\n3\n4\n");
}

// ===== remove_at(list, i) =====

TEST_F(CodeGenTest, ListRemoveAt) {
    std::string src =
        "xs = [10, 20, 30]\n"
        "removed = remove_at(xs, 1)\n"
        "print(removed)\n"
        "print(length(xs))\n"
        "print(xs[0])\n"
        "print(xs[1])";
    EXPECT_EQ(runSource(src), "20\n2\n10\n30\n");
}

// ===== items(map) =====

TEST_F(CodeGenTest, MapItems) {
    std::string src =
        "m = {\"a\": 1}\n"
        "pairs = items(m)\n"
        "print(length(pairs))";
    EXPECT_EQ(runSource(src), "1\n");
}

// ===== get(map, key, default) =====

TEST_F(CodeGenTest, MapGetWithDefault) {
    std::string src =
        "m = {\"a\": 10}\n"
        "print(get(m, \"a\", 0))\n"
        "print(get(m, \"b\", 99))";
    EXPECT_EQ(runSource(src), "10\n99\n");
}

// ===== remove(map, key) =====

TEST_F(CodeGenTest, MapRemove) {
    std::string src =
        "m = {\"a\": 1, \"b\": 2}\n"
        "remove(m, \"a\")\n"
        "print(length(m))";
    EXPECT_EQ(runSource(src), "1\n");
}

// ===== Set operations =====

TEST_F(CodeGenTest, SetOperations) {
    // SetUnion
    {
        std::string src =
            "a: Set<int> = {1, 2, 3}\n"
            "b: Set<int> = {3, 4, 5}\n"
            "c = union(a, b)\n"
            "print(length(c))";
        EXPECT_EQ(runSource(src), "5\n");
    }
    // SetIntersection
    {
        std::string src =
            "a: Set<int> = {1, 2, 3}\n"
            "b: Set<int> = {2, 3, 4}\n"
            "c = intersection(a, b)\n"
            "print(length(c))";
        EXPECT_EQ(runSource(src), "2\n");
    }
    // SetDifference
    {
        std::string src =
            "a: Set<int> = {1, 2, 3}\n"
            "b: Set<int> = {2, 3, 4}\n"
            "c = difference(a, b)\n"
            "print(length(c))";
        EXPECT_EQ(runSource(src), "1\n");
    }
    // SetSymmetricDifference
    {
        std::string src =
            "a: Set<int> = {1, 2, 3}\n"
            "b: Set<int> = {2, 3, 4}\n"
            "c = symmetric_difference(a, b)\n"
            "print(length(c))";
        EXPECT_EQ(runSource(src), "2\n");
    }
    // SetIsSubset
    {
        std::string src =
            "a: Set<int> = {1, 2}\n"
            "b: Set<int> = {1, 2, 3}\n"
            "print(is_subset(a, b))\n"
            "print(is_subset(b, a))";
        EXPECT_EQ(runSource(src), "true\nfalse\n");
    }
    // SetIsSuperset
    {
        std::string src =
            "a: Set<int> = {1, 2, 3}\n"
            "b: Set<int> = {1, 2}\n"
            "print(is_superset(a, b))\n"
            "print(is_superset(b, a))";
        EXPECT_EQ(runSource(src), "true\nfalse\n");
    }
}

// ===== remove(list, val) テスト =====

TEST_F(CodeGenTest, ListRemoveByType) {
    // ListRemoveInt
    {
        std::string src =
            "xs = [1, 2, 3, 4]\n"
            "remove(xs, 3)\n"
            "print(xs)";
        EXPECT_EQ(runSource(src), "[1, 2, 4]\n");
    }
    // ListRemoveStr
    {
        std::string src =
            "xs = [\"a\", \"b\", \"c\"]\n"
            "remove(xs, \"b\")\n"
            "print(xs)";
        EXPECT_EQ(runSource(src), "[\"a\", \"c\"]\n");
    }
    // ListRemoveFloat
    {
        std::string src =
            "xs = [1.0, 2.5, 3.0]\n"
            "remove(xs, 2.5)\n"
            "print(xs)";
        EXPECT_EQ(runSource(src), "[1.0, 3.0]\n");
    }
}

TEST_F(CodeGenTest, ListRemoveEdgeCases) {
    // ListRemoveNotFound
    {
        std::string src =
            "xs = [1, 2, 3]\n"
            "remove(xs, 99)\n"
            "print(xs)";
        EXPECT_EQ(runSource(src), "[1, 2, 3]\n");
    }
    // ListRemoveFirst
    {
        std::string src =
            "xs = [10, 20, 30]\n"
            "remove(xs, 10)\n"
            "print(xs)";
        EXPECT_EQ(runSource(src), "[20, 30]\n");
    }
    // ListRemoveLast
    {
        std::string src =
            "xs = [10, 20, 30]\n"
            "remove(xs, 30)\n"
            "print(xs)";
        EXPECT_EQ(runSource(src), "[10, 20]\n");
    }
    // ListRemoveDuplicate
    {
        std::string src =
            "xs = [1, 2, 3, 2, 4]\n"
            "remove(xs, 2)\n"
            "print(xs)";
        EXPECT_EQ(runSource(src), "[1, 3, 2, 4]\n");
    }
    // ListRemoveSingleElement
    {
        std::string src =
            "xs = [42]\n"
            "remove(xs, 42)\n"
            "print(xs)";
        EXPECT_EQ(runSource(src), "[]\n");
    }
}

// ===== distinct(list) テスト =====

TEST_F(CodeGenTest, DistinctVariants) {
    // DistinctInt
    EXPECT_EQ(runSource("xs = [1, 2, 3, 2, 1, 4]\nprint(distinct(xs))"), "[1, 2, 3, 4]\n");
    // DistinctStr
    EXPECT_EQ(runSource("xs = [\"a\", \"b\", \"a\", \"c\", \"b\"]\nprint(distinct(xs))"), "[\"a\", \"b\", \"c\"]\n");
    // DistinctFloat
    EXPECT_EQ(runSource("xs = [1.0, 2.0, 1.0, 3.0]\nprint(distinct(xs))"), "[1.0, 2.0, 3.0]\n");
    // DistinctAlreadyUnique
    EXPECT_EQ(runSource("xs = [1, 2, 3]\nprint(distinct(xs))"), "[1, 2, 3]\n");
    // DistinctAllSame
    EXPECT_EQ(runSource("xs = [5, 5, 5, 5]\nprint(distinct(xs))"), "[5]\n");
    // DistinctPreservesOrder
    EXPECT_EQ(runSource("xs = [3, 1, 2, 1, 3]\nprint(distinct(xs))"), "[3, 1, 2]\n");
    // DistinctNonDestructive
    {
        std::string src =
            "xs = [1, 2, 1, 3]\n"
            "ys = distinct(xs)\n"
            "print(xs)\n"
            "print(ys)";
        EXPECT_EQ(runSource(src), "[1, 2, 1, 3]\n[1, 2, 3]\n");
    }
}

// ===== merge(map1, map2) テスト =====

TEST_F(CodeGenTest, MapMergeVariants) {
    // MergeNoOverlap
    {
        std::string src =
            "m1 = {\"a\": 1, \"b\": 2}\n"
            "m2 = {\"c\": 3, \"d\": 4}\n"
            "m3 = merge(m1, m2)\n"
            "print(length(m3))\n"
            "print(m3[\"a\"])\n"
            "print(m3[\"b\"])\n"
            "print(m3[\"c\"])\n"
            "print(m3[\"d\"])";
        EXPECT_EQ(runSource(src), "4\n1\n2\n3\n4\n");
    }
    // MergeOverlap
    {
        std::string src =
            "m1 = {\"a\": 1, \"b\": 2}\n"
            "m2 = {\"b\": 99, \"c\": 3}\n"
            "m3 = merge(m1, m2)\n"
            "print(length(m3))\n"
            "print(m3[\"a\"])\n"
            "print(m3[\"b\"])\n"
            "print(m3[\"c\"])";
        EXPECT_EQ(runSource(src), "3\n1\n99\n3\n");
    }
    // MergeIntKeys
    {
        std::string src =
            "m1 = {1: \"a\", 2: \"b\"}\n"
            "m2 = {2: \"x\", 3: \"c\"}\n"
            "m3 = merge(m1, m2)\n"
            "print(length(m3))\n"
            "print(m3[1])\n"
            "print(m3[2])\n"
            "print(m3[3])";
        EXPECT_EQ(runSource(src), "3\na\nx\nc\n");
    }
    // MergeNonDestructive
    {
        std::string src =
            "m1 = {\"a\": 1}\n"
            "m2 = {\"b\": 2}\n"
            "m3 = merge(m1, m2)\n"
            "print(length(m1))\n"
            "print(length(m2))\n"
            "print(length(m3))";
        EXPECT_EQ(runSource(src), "1\n1\n2\n");
    }
}

// ===== flatten(list) テスト =====

TEST_F(CodeGenTest, FlattenVariants) {
    // FlattenIntLists
    EXPECT_EQ(runSource("xs = [[1, 2], [3, 4]]\nprint(flatten(xs))"), "[1, 2, 3, 4]\n");
    // FlattenStrLists
    EXPECT_EQ(runSource("xs = [[\"a\", \"b\"], [\"c\", \"d\"]]\nprint(flatten(xs))"), "[\"a\", \"b\", \"c\", \"d\"]\n");
    // FlattenUnevenInner
    {
        std::string src =
            "xs = [[1], [2, 3, 4], [5, 6]]\n"
            "ys = flatten(xs)\n"
            "print(ys)";
        EXPECT_EQ(runSource(src), "[1, 2, 3, 4, 5, 6]\n");
    }
    // FlattenSingleInner
    EXPECT_EQ(runSource("xs = [[10, 20, 30]]\nprint(flatten(xs))"), "[10, 20, 30]\n");
    // FlattenNonDestructive
    {
        std::string src =
            "xs = [[1, 2], [3, 4]]\n"
            "ys = flatten(xs)\n"
            "print(length(xs))\n"
            "print(ys)";
        EXPECT_EQ(runSource(src), "2\n[1, 2, 3, 4]\n");
    }
}

// ===== for tuple destructuring =====

TEST_F(CodeGenTest, ForTupleDestructuring) {
    // ForEnumerateDestructure
    {
        std::string src =
            "xs = [10, 20, 30]\n"
            "total = 0\n"
            "for i, x in enumerate(xs):\n"
            "    total = total + i + x\n"
            "print(total)";
        EXPECT_EQ(runSource(src), "63\n");
    }
    // ForZipDestructure
    {
        std::string src =
            "xs = [1, 2, 3]\n"
            "ys = [10, 20, 30]\n"
            "total = 0\n"
            "for a, b in zip(xs, ys):\n"
            "    total = total + a + b\n"
            "print(total)";
        EXPECT_EQ(runSource(src), "66\n");
    }
    // ForEnumerateWildcardFirst
    {
        std::string src =
            "xs = [10, 20, 30]\n"
            "total = 0\n"
            "for _, x in enumerate(xs):\n"
            "    total = total + x\n"
            "print(total)";
        EXPECT_EQ(runSource(src), "60\n");
    }
    // ForEnumerateWildcardSecond
    {
        std::string src =
            "xs = [10, 20, 30]\n"
            "total = 0\n"
            "for i, _ in enumerate(xs):\n"
            "    total = total + i\n"
            "print(total)";
        EXPECT_EQ(runSource(src), "3\n");
    }
    // ForEnumerateWithPrint
    {
        std::string src =
            "xs = [10, 20, 30]\n"
            "for i, x in enumerate(xs):\n"
            "    print(f\"{i}: {x}\")";
        EXPECT_EQ(runSource(src), "0: 10\n1: 20\n2: 30\n");
    }
}

TEST_F(CodeGenTest, ForTwoVarNonTupleError) {
    std::string src =
        "xs = [1, 2, 3]\n"
        "for a, b in xs:\n"
        "    print(a)";
    EXPECT_THROW(runSource(src), std::runtime_error);
}

TEST_F(CodeGenTest, ForThreeVarTupleDestructuring) {
    std::string src =
        "pairs = [(1, 2, 3), (4, 5, 6)]\n"
        "sum_a = 0\n"
        "sum_b = 0\n"
        "sum_c = 0\n"
        "for a, b, c in pairs:\n"
        "    sum_a = sum_a + a\n"
        "    sum_b = sum_b + b\n"
        "    sum_c = sum_c + c\n"
        "print(sum_a)\n"
        "print(sum_b)\n"
        "print(sum_c)";
    EXPECT_EQ(runSource(src), "5\n7\n9\n");
}

TEST_F(CodeGenTest, ForThreeVarWildcardMiddle) {
    std::string src =
        "pairs = [(1, 2, 3), (4, 5, 6)]\n"
        "sum = 0\n"
        "for a, _, c in pairs:\n"
        "    sum = sum + a + c\n"
        "print(sum)";
    EXPECT_EQ(runSource(src), "14\n");
}

TEST_F(CodeGenTest, ForThreeVarCountMismatch) {
    std::string src =
        "pairs = [(1, 2), (3, 4)]\n"
        "for a, b, c in pairs:\n"
        "    print(a)";
    EXPECT_THROW(runSource(src), std::runtime_error);
}

// ===== operator[] / operator[]= / operator in =====

TEST_F(CodeGenTest, OperatorSubscriptReadSingleIndex) {
    std::string src =
        "record Point:\n"
        "    x: int\n"
        "    y: int\n"
        "    z: int\n"
        "function operator[](p: Point, i: int) -> int:\n"
        "    if i == 0:\n"
        "        return p.x\n"
        "    if i == 1:\n"
        "        return p.y\n"
        "    return p.z\n"
        "p = Point(10, 20, 30)\n"
        "print(p[0])\n"
        "print(p[2])";
    EXPECT_EQ(runSource(src), "10\n30\n");
}

TEST_F(CodeGenTest, OperatorSubscriptReadMultiIndex) {
    std::string src =
        "record Grid:\n"
        "    a: int\n"
        "    b: int\n"
        "    c: int\n"
        "    d: int\n"
        "function operator[](g: Grid, row: int, col: int) -> int:\n"
        "    if row == 0 and col == 0:\n"
        "        return g.a\n"
        "    if row == 0 and col == 1:\n"
        "        return g.b\n"
        "    if row == 1 and col == 0:\n"
        "        return g.c\n"
        "    return g.d\n"
        "g = Grid(1, 2, 3, 4)\n"
        "print(g[0, 1])\n"
        "print(g[1, 0])";
    EXPECT_EQ(runSource(src), "2\n3\n");
}

TEST_F(CodeGenTest, OperatorSubscriptWrite) {
    // operator[]= dispatches correctly (uses side effect to verify)
    std::string src =
        "record Counter:\n"
        "    count: int\n"
        "function operator[]=(c: Counter, key: int, value: int):\n"
        "    print(key)\n"
        "    print(value)\n"
        "c = Counter(0)\n"
        "c[3] = 42";
    EXPECT_EQ(runSource(src), "3\n42\n");
}

TEST_F(CodeGenTest, OperatorIn) {
    std::string src =
        "record Range:\n"
        "    start: int\n"
        "    end_val: int\n"
        "function operator in(value: int, r: Range) -> bool:\n"
        "    return value >= r.start and value < r.end_val\n"
        "r = Range(1, 10)\n"
        "print(5 in r)\n"
        "print(15 in r)\n"
        "print(5 not in r)";
    EXPECT_EQ(runSource(src), "true\nfalse\nfalse\n");
}

TEST_F(CodeGenTest, OperatorInFallbackBuiltin) {
    // Built-in in should still work for lists/maps/sets
    EXPECT_EQ(runSource("print(2 in [1, 2, 3])"), "true\n");
    EXPECT_EQ(runSource("print(\"a\" in {\"a\": 1})"), "true\n");
    EXPECT_EQ(runSource("print(5 in [1, 2, 3])"), "false\n");
}

TEST_F(CodeGenTest, OperatorSubscriptParamValidation) {
    // operator[] with < 2 params should fail
    EXPECT_THROW(runSource(
        "record Foo:\n"
        "    x: int\n"
        "function operator[](f: Foo) -> int:\n"
        "    return f.x\n"), std::runtime_error);
    // operator[]= with < 3 params should fail
    EXPECT_THROW(runSource(
        "record Foo:\n"
        "    x: int\n"
        "function operator[]=(f: Foo, v: int):\n"
        "    f.x = v\n"), std::runtime_error);
    // operator in with != 2 params should fail
    EXPECT_THROW(runSource(
        "record Foo:\n"
        "    x: int\n"
        "function operator in(a: int) -> bool:\n"
        "    return true\n"), std::runtime_error);
    // operator in must return bool
    EXPECT_THROW(runSource(
        "record Foo:\n"
        "    x: int\n"
        "function operator in(a: int, f: Foo) -> int:\n"
        "    return 1\n"), std::runtime_error);
}

TEST_F(CodeGenTest, BuiltinIndexStillWorks) {
    // List indexing
    EXPECT_EQ(runSource("xs = [10, 20, 30]\nprint(xs[1])"), "20\n");
    // Map indexing
    EXPECT_EQ(runSource("m = {\"a\": 1, \"b\": 2}\nprint(m[\"b\"])"), "2\n");
    // List index assignment
    EXPECT_EQ(runSource("xs = [1, 2, 3]\nxs[0] = 99\nprint(xs[0])"), "99\n");
    // Map index assignment
    EXPECT_EQ(runSource("m = {\"a\": 1}\nm[\"b\"] = 2\nprint(m[\"b\"])"), "2\n");
}

TEST_F(CodeGenTest, OperatorCallSingleArg) {
    std::string src =
        "record Adder:\n"
        "    base: int\n"
        "function operator()(a: Adder, x: int) -> int:\n"
        "    return a.base + x\n"
        "add5 = Adder(5)\n"
        "print(add5(10))";
    EXPECT_EQ(runSource(src), "15\n");
}

TEST_F(CodeGenTest, OperatorCallMultiArg) {
    std::string src =
        "record Multiplier:\n"
        "    factor: int\n"
        "function operator()(m: Multiplier, x: int, y: int) -> int:\n"
        "    return m.factor * (x + y)\n"
        "mul = Multiplier(3)\n"
        "print(mul(4, 5))";
    EXPECT_EQ(runSource(src), "27\n");
}

TEST_F(CodeGenTest, OperatorCallVoid) {
    std::string src =
        "record Printer:\n"
        "    prefix: str\n"
        "function operator()(p: Printer, msg: str):\n"
        "    print(p.prefix + msg)\n"
        "p = Printer(\">> \")\n"
        "p(\"hello\")";
    EXPECT_EQ(runSource(src), ">> hello\n");
}

TEST_F(CodeGenTest, OperatorAs) {
    std::string src =
        "record Celsius:\n"
        "    value: int\n"
        "record Fahrenheit:\n"
        "    value: int\n"
        "function operator as(c: Celsius) -> Fahrenheit:\n"
        "    return Fahrenheit(c.value * 9 // 5 + 32)\n"
        "c = Celsius(100)\n"
        "f = c as Fahrenheit\n"
        "print(f.value)";
    EXPECT_EQ(runSource(src), "212\n");
}

TEST_F(CodeGenTest, OperatorAsFallbackBuiltin) {
    EXPECT_EQ(runSource("x = 42\nprint(x as float)"), "42.0\n");
}

TEST_F(CodeGenTest, OperatorCallParamValidation) {
    EXPECT_THROW(runSource(
        "record Foo:\n"
        "    x: int\n"
        "function operator()(f: Foo) -> int:\n"
        "    return f.x\n"), std::runtime_error);
}

TEST_F(CodeGenTest, OperatorAsParamValidation) {
    EXPECT_THROW(runSource(
        "record Foo:\n"
        "    x: int\n"
        "function operator as(a: int, b: int) -> int:\n"
        "    return a\n"), std::runtime_error);
}

TEST_F(CodeGenTest, OperatorAsGenericOptionTarget) {
    std::string src =
        "record Celsius:\n"
        "    value: int\n"
        "function operator as(c: Celsius) -> int?:\n"
        "    return Some(c.value)\n"
        "c = Celsius(42)\n"
        "result: int? = c as int?\n"
        "print(result ?? -1)\n";
    EXPECT_EQ(runSource(src), "42\n");
}

// ===== Bounds checking tests =====

TEST_F(CodeGenTest, ListNegativeIndexWrapAll) {
    // -1 → last, -2 → second to last, -3 → first
    EXPECT_EQ(runSource("xs = [10, 20, 30]\nprint(xs[-1])"), "30\n");
    EXPECT_EQ(runSource("xs = [10, 20, 30]\nprint(xs[-2])"), "20\n");
    EXPECT_EQ(runSource("xs = [10, 20, 30]\nprint(xs[-3])"), "10\n");
}

TEST_F(CodeGenTest, StringCharAtOutOfRange) {
    std::string src = "print(char_at(\"hello\", 5))";
    EXPECT_EXIT(runSource(src), ::testing::ExitedWithCode(1), "");
}

TEST_F(CodeGenTest, StringCharAtNegativeIndex) {
    // Negative index wraps around for char_at
    EXPECT_EQ(runSource("print(char_at(\"hello\", -1))"), "o\n");
    EXPECT_EQ(runSource("print(char_at(\"hello\", -5))"), "h\n");
}

TEST_F(CodeGenTest, StringCharAtNegativeOOB) {
    std::string src = "print(char_at(\"hello\", -6))";
    EXPECT_EXIT(runSource(src), ::testing::ExitedWithCode(1), "");
}

TEST_F(CodeGenTest, StringCharAtEmptyString) {
    std::string src = "print(char_at(\"\", 0))";
    EXPECT_EXIT(runSource(src), ::testing::ExitedWithCode(1), "");
}

TEST_F(CodeGenTest, StringCharAtUTF8) {
    // UTF-8 string indexing
    EXPECT_EQ(runSource("print(char_at(\"あいう\", 0))"), "あ\n");
    EXPECT_EQ(runSource("print(char_at(\"あいう\", -1))"), "う\n");
}

TEST_F(CodeGenTest, StringCharAtUTF8OutOfRange) {
    std::string src = "print(char_at(\"あいう\", 3))";
    EXPECT_EXIT(runSource(src), ::testing::ExitedWithCode(1), "");
}

TEST_F(CodeGenTest, SubstringClamp) {
    // Out-of-range values are clamped
    EXPECT_EQ(runSource("print(substring(\"hello\", -1, 10))"), "hello\n");
    EXPECT_EQ(runSource("print(substring(\"hello\", 0, 100))"), "hello\n");
    EXPECT_EQ(runSource("print(substring(\"hello\", 3, 1))"), "\n");
    EXPECT_EQ(runSource("print(substring(\"hello\", -5, -2))"), "\n");
}

TEST_F(CodeGenTest, ArrayNegativeIndexWrap) {
    std::string src =
        "buf: i32[3] = [10, 20, 30]\n"
        "print(buf[-1] as int)";
    EXPECT_EQ(runSource(src), "30\n");
}

TEST_F(CodeGenTest, ArrayNegativeIndexOOB) {
    std::string src =
        "buf: i32[3] = [10, 20, 30]\n"
        "print(buf[-4])";
    EXPECT_THROW(runSource(src), std::runtime_error);
}

TEST_F(CodeGenTest, InsertNegativeIndex) {
    // insert at -1 wraps to len+1-1 = len (append position)
    std::string src =
        "xs = [1, 2, 3]\n"
        "insert(xs, -1, 99)\n"
        "print(length(xs))\n"
        "print(xs[3])";
    EXPECT_EQ(runSource(src), "4\n99\n");
}

TEST_F(CodeGenTest, InsertNegativeIndexOOB) {
    std::string src =
        "xs = [1, 2, 3]\n"
        "insert(xs, -5, 99)";
    EXPECT_EXIT(runSource(src), ::testing::ExitedWithCode(1), "");
}

TEST_F(CodeGenTest, RemoveAtNegativeIndex) {
    // remove_at(xs, -1) removes the last element
    std::string src =
        "xs = [10, 20, 30]\n"
        "v = remove_at(xs, -1)\n"
        "print(v)";
    EXPECT_EQ(runSource(src), "30\n");
}

TEST_F(CodeGenTest, RemoveAtNegativeIndexOOB) {
    std::string src =
        "xs = [1, 2, 3]\n"
        "remove_at(xs, -4)";
    EXPECT_EXIT(runSource(src), ::testing::ExitedWithCode(1), "");
}

// ===== Discarded collection operation results (ExprStmt) =====

TEST_F(CodeGenTest, DiscardedCollectionResults) {
    // Collection operation results used as bare expression statements
    // should not crash or leak (ARC release handles cleanup).
    EXPECT_EQ(runSource(
        "xs = [1, 2, 3]\n"
        "appended(xs, 4)\n"
        "print(length(xs))"), "3\n");
    EXPECT_EQ(runSource(
        "xs = [1, 2, 3]\n"
        "slice(xs, 0, 1)\n"
        "print(length(xs))"), "3\n");
    EXPECT_EQ(runSource(
        "xs = [1, 2, 3]\n"
        "take(xs, 2)\n"
        "print(length(xs))"), "3\n");
    EXPECT_EQ(runSource(
        "xs = [1, 2, 3]\n"
        "distinct(xs)\n"
        "print(length(xs))"), "3\n");
}

// ===== Collection equality: compile-time rejection of unsupported element types =====

// List<function> == List<function> must be rejected at compile time.
// Regression for the list_elem_fn_type_info guard added in #736.
TEST_F(CodeGenTest, ListFnElemEqualityRejected) {
    expectCompileError(
        "a: List<function(int) -> int> = []\n"
        "b: List<function(int) -> int> = []\n"
        "_ = a == b\n",
        "function-typed elements");
}

// Set<function> == Set<function> must be rejected at compile time.
// Regression: closure/function elements are never equatable in Ry.
// (Set<record> equality was lifted in #958.)
TEST_F(CodeGenTest, SetFnElemEqualityRejected) {
    expectCompileError(
        "a: Set<function(int) -> int> = {}\n"
        "b: Set<function(int) -> int> = {}\n"
        "_ = a == b\n",
        "function-typed");
}

// Map with a function-typed key via type alias must be rejected at == / != (#961).
// Direct Map<function(...)...> annotations are rejected by annotation parsing;
// the alias path exercises the map_key_fn_type_info guard in codegen_expr.cpp.
TEST_F(CodeGenTest, MapFnKeyEqualityRejected) {
    expectCompileError(
        "type FnKey = function(int) -> int\n"
        "a: Map<FnKey, int> = {}\n"
        "b: Map<FnKey, int> = {}\n"
        "_ = a == b\n",
        "function-typed keys");
}

// ADT enum with a function-typed payload must be rejected at compile time.
// Regression for the closure-payload guard added in #959.
TEST_F(CodeGenTest, AdtEnumFnPayloadEqualityRejected) {
    expectCompileError(
        "enum Bad:\n"
        "    F(function(int) -> int)\n"
        "function make_id() -> function(int) -> int:\n"
        "    return (x: int) => x\n"
        "function make_inc() -> function(int) -> int:\n"
        "    return (x: int) => x + 1\n"
        "f1 = Bad::F(make_id())\n"
        "f2 = Bad::F(make_inc())\n"
        "_ = f1 == f2\n",
        "function-typed payload");
}

// Union with a function-typed variant must be rejected at compile time.
// Regression for the isFunctionTypeName guard added in #960.
TEST_F(CodeGenTest, UnionFnVariantEqualityRejected) {
    expectCompileError(
        "function make_fn() -> function(int) -> int:\n"
        "    return (x: int) => x\n"
        "x: function(int) -> int | int = make_fn()\n"
        "y: function(int) -> int | int = make_fn()\n"
        "_ = x == y\n",
        "function-typed variant");
}
