[English](../README.md) | [日本語](../ja/README.md) | [简体中文](README.md)

# Ry 语言文档

Ry 是一个基于 LLVM JIT 的简洁编程语言。采用 Python 风格的缩进块语法，结合静态类型与类型推断，设计简单易用。

---

## 教程

初次使用 Ry 的用户，请依序阅读以下内容。

| # | 标题 | 内容 |
|---|------|------|
| 01 | [入门](tutorial/01-getting-started.md) | 环境搭建、Hello World、注释 |
| 02 | [变量与类型](tutorial/02-variables-and-types.md) | 变量声明、常量、类型注解、基本类型、f-string、类型转换 |
| 03 | [运算符](tutorial/03-operators.md) | 算术、比较、逻辑、位运算、复合赋值运算符 |
| 04 | [控制流](tutorial/04-control-flow.md) | if/else、when、while、for/range、break/continue |
| 05 | [函数定义](tutorial/05-functions.md) | function 定义、递归、重载、默认参数、Lambda、闭包、高阶函数、UFCS |
| 06 | [Record 与枚举类型](tutorial/06-records.md) | type 定义、字段访问、enum、ADT、泛型 enum、运算符重载 |
| 07 | [集合与迭代器](tutorial/07-collections.md) | 元组、列表、映射、集合、惰性迭代器 |
| 08 | [错误处理](tutorial/08-error-handling.md) | Option、Result、`?` 运算符、契约式设计 |
| 09 | [包](tutorial/09-modules.md) | 包、std 标准库、目录包 |
| 10 | [并发](tutorial/10-concurrency.md) | async/await、@parallel、线程、网络 |
| 11 | [测试](tutorial/11-testing.md) | describe/it/expect、匹配器、mock/verify、契约测试 |
| 12 | [构建项目](tutorial/12-building-a-project.md) | 结合所有功能的实战 CLI 项目 |

环境搭建与构建方法请参阅 [01 - 入门](tutorial/01-getting-started.md)。

---

## 参考手册

语言规格的详细内容请参阅参考手册。

| 页面 | 内容 |
|------|------|
| [类型一览与类型规则](reference/types.md) | 所有类型的说明、类型提升规则、类型转换 |
| [运算符一览与优先级](reference/operators.md) | 所有运算符与优先级表 |
| [控制流](reference/control-flow.md) | if/else、when、while、for 的完整语法 |
| [函数、Lambda、UFCS、运算符重载](reference/functions.md) | 函数定义的所有形式 |
| [结构体与枚举类型](reference/structs.md) | type 定义、enum 定义的完整语法 |
| [元组、列表、映射、集合](reference/collections.md) | 集合类型的操作方法 |
| [内置函数](reference/builtins.md) | print、length、Some、range 等 |
| [字符串操作函数](reference/builtins-string.md) | contains、find、replace、split、join 等 |
| [正则表达式](reference/regex.md) | regex_match、regex_search、regex_replace、regex_split、regex_find_all |
| [数学函数](reference/math.md) | PI、E、sqrt、sin、cos、abs、floor、ceil、round 等 |
| [I/O 函数](reference/io.md) | read_text、write_text、exists、read_bytes、to_bytes 等 |
| [JSON](reference/json.md) | parse、stringify、get、at、to_str、to_int 等 |
| [网络（TCP）](reference/net.md) | bind、listen、accept、connect、TCP 套接字的 send/receive/close |
| [HTTP 服务器](reference/http.md) | listen、method、path、header、body、response |
| [Base64](reference/base64.md) | encode、decode、encode_url_safe、decode_url_safe |
| [Path](reference/path.md) | join、basename、dirname、extension、resolve、is_absolute |
| [文件系统](reference/filesystem.md) | list_dir、walk、glob_files、copy、move、remove、make_dir、chmod、symlink |
| [线程](reference/thread.md) | thread_spawn、thread_join、Lock、RWLock、Semaphore、Barrier、AtomicInt、AtomicBool |
| [GC](reference/gc.md) | collect、enable、disable、set_threshold — ARC 循环收集器 |
| [包系统](reference/packages.md) | from/import 的语法、目录包、std、RY_HOME |
| [测试功能](reference/testing.md) | 使用 describe/it/expect 进行测试 |
| [项目管理](reference/project.md) | ry init 与 package.toml 的规格 |
| [契约式设计](reference/contracts.md) | require、ensure、invariant、old、result |
| [指令](reference/directives.md) | @deprecated 与编译时元数据 |
| [错误一览](reference/errors.md) | 编译错误与运行时错误的说明 |
