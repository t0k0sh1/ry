#include "ry/cli/docs.hpp"

#include "ry/ast/ast.hpp"
#include "ry/module/module_loader.hpp"
#include "ry/project/project_config.hpp"
#include "ry/source_manager/source_manager.hpp"

#include <algorithm>
#include <cstdio>
#include <cstring>
#include <filesystem>
#include <fstream>
#include <string>
#include <string_view>
#include <unordered_set>
#include <utility>
#include <variant>
#include <vector>

#include <llvm/Support/raw_ostream.h>

namespace fs = std::filesystem;

namespace {

struct DocSymbol {
    std::string kind;
    std::string name;
    std::string signature;
    std::string doc;
    bool is_block_doc = false;
    bool is_public = true;
    std::string source;
    int line = 0;
};

struct DocModule {
    std::string name;
    std::string rel_path;
    std::vector<DocSymbol> symbols;
};

std::string htmlEscape(std::string_view s) {
    std::string out;
    out.reserve(s.size());
    for (char c : s) {
        switch (c) {
            case '<': out += "&lt;"; break;
            case '>': out += "&gt;"; break;
            case '&': out += "&amp;"; break;
            case '"': out += "&quot;"; break;
            case '\'': out += "&#39;"; break;
            default: out += c;
        }
    }
    return out;
}

std::string htmlEscapeDoc(std::string_view text, bool is_block) {
    std::string escaped = htmlEscape(text);
    if (is_block) {
        std::string out = "<pre class=\"doc\">";
        out += escaped;
        out += "</pre>";
        return out;
    }
    std::string out = "<p class=\"doc\">";
    for (char c : escaped) {
        if (c == '\n') out += "<br>";
        else out += c;
    }
    out += "</p>";
    return out;
}

std::string buildFnSignature(const ry::FnStmt &fn) {
    std::string sig = "fn ";
    sig += fn.name;
    sig += "(";
    for (size_t i = 0; i < fn.params.size(); ++i) {
        if (i > 0) sig += ", ";
        sig += fn.params[i].name;
        sig += ": ";
        sig += fn.params[i].type ? fn.params[i].type->toString() : "?";
    }
    sig += ")";
    if (fn.return_type) {
        sig += " -> ";
        sig += fn.return_type->toString();
    }
    return sig;
}

void extractDocPayload(const std::vector<ry::Directive> &directives,
                       std::string &out_doc, bool &out_is_block) {
    const ry::Directive *doc = ry::findDirective(directives, "doc");
    if (!doc || doc->args.empty()) return;
    const ry::ExprNode *expr = doc->args[0].value.get();
    if (!expr) return;
    if (const auto *s = std::get_if<ry::StringExpr>(&expr->data)) {
        out_doc = s->value;
        out_is_block = s->is_block;
    }
}

void collectFromProgram(const ry::Program &prog, DocModule &mod, bool include_private) {
    for (const auto &stmt : prog) {
        const std::vector<ry::Directive> *directives = nullptr;
        DocSymbol sym;

        if (std::holds_alternative<std::unique_ptr<ry::FnStmt>>(stmt)) {
            const auto &fn = *std::get<std::unique_ptr<ry::FnStmt>>(stmt);
            directives = &fn.directives;
            sym.kind = "fn";
            sym.name = fn.name;
            sym.signature = buildFnSignature(fn);
            sym.line = fn.loc.line;
        } else if (std::holds_alternative<ry::RecordStmt>(stmt)) {
            const auto &rec = std::get<ry::RecordStmt>(stmt);
            directives = &rec.directives;
            sym.kind = "record";
            sym.name = rec.name;
            sym.signature = "record ";
            sym.signature += rec.name;
            sym.line = rec.loc.line;
        } else if (std::holds_alternative<ry::EnumStmt>(stmt)) {
            const auto &en = std::get<ry::EnumStmt>(stmt);
            directives = &en.directives;
            sym.kind = "enum";
            sym.name = en.name;
            sym.signature = "enum ";
            sym.signature += en.name;
            sym.line = en.loc.line;
        } else if (std::holds_alternative<ry::TypeAliasStmt>(stmt)) {
            const auto &ta = std::get<ry::TypeAliasStmt>(stmt);
            directives = &ta.directives;
            sym.kind = "typeAlias";
            sym.name = ta.name;
            std::string s = "type ";
            s += ta.name;
            if (ta.target_type) {
                s += " = ";
                s += ta.target_type->toString();
            }
            sym.signature = std::move(s);
            sym.line = ta.loc.line;
        } else if (std::holds_alternative<ry::AssignStmt>(stmt)) {
            const auto &c = std::get<ry::AssignStmt>(stmt);
            if (!ry::hasDirective(c.directives, "const")) continue;
            directives = &c.directives;
            sym.kind = "const";
            sym.name = c.name;
            std::string s = c.name;
            if (c.type_annotation) {
                s += ": ";
                s += c.type_annotation->toString();
            }
            sym.signature = std::move(s);
            sym.line = c.loc.line;
        } else {
            continue;
        }

        sym.is_public = ry::hasDirective(*directives, "public");
        if (!include_private && !sym.is_public) continue;

        extractDocPayload(*directives, sym.doc, sym.is_block_doc);
        bool record_visible = sym.is_public;
        mod.symbols.push_back(std::move(sym));

        if (std::holds_alternative<ry::RecordStmt>(stmt)) {
            const auto &rec = std::get<ry::RecordStmt>(stmt);
            for (const auto &field : rec.fields) {
                DocSymbol fsym;
                fsym.kind = "field";
                fsym.name = rec.name + "." + field.name;
                std::string fsig = field.name;
                fsig += ": ";
                fsig += field.type ? field.type->toString() : "?";
                fsym.signature = std::move(fsig);
                fsym.line = rec.loc.line;
                fsym.is_public = record_visible;
                extractDocPayload(field.directives, fsym.doc, fsym.is_block_doc);
                mod.symbols.push_back(std::move(fsym));
            }
        }
    }
}

std::string deriveModuleName(const fs::path &src_root, const fs::path &abs_file) {
    fs::path rel = fs::relative(abs_file, src_root);
    rel.replace_extension();
    std::string s = rel.generic_string();
    std::replace(s.begin(), s.end(), '/', '.');
    return s;
}

bool writeOutFile(const fs::path &path, std::string_view content) {
    fs::create_directories(path.parent_path());
    std::ofstream ofs(path);
    if (!ofs) return false;
    ofs << content;
    return static_cast<bool>(ofs);
}

std::string readPackageToml(const fs::path &project_root) {
    std::ifstream f(project_root / "package.toml");
    if (!f) return {};
    std::string content((std::istreambuf_iterator<char>(f)),
                        std::istreambuf_iterator<char>{});
    return content;
}

constexpr const char *kManifestFileName = ".ry-docs-manifest";

bool isPathSafeForManifest(std::string_view rel) {
    if (rel.empty()) return false;
    if (rel.front() == '/' || rel.front() == '\\') return false;
    std::string_view cur = rel;
    while (!cur.empty()) {
        size_t pos = cur.find_first_of("/\\");
        std::string_view seg = pos == std::string_view::npos ? cur : cur.substr(0, pos);
        if (seg == "..") return false;
        if (pos == std::string_view::npos) break;
        cur = cur.substr(pos + 1);
    }
    return true;
}

std::vector<std::string> readDocsManifest(const fs::path &out_abs) {
    std::vector<std::string> entries;
    std::ifstream f(out_abs / kManifestFileName);
    if (!f) return entries;
    std::string line;
    while (std::getline(f, line)) {
        if (!line.empty() && line.back() == '\r') line.pop_back();
        if (line.empty()) continue;
        if (isPathSafeForManifest(line)) entries.push_back(std::move(line));
    }
    return entries;
}

bool writeDocsManifest(const fs::path &out_abs, const std::vector<std::string> &entries) {
    fs::create_directories(out_abs);
    std::ofstream f(out_abs / kManifestFileName);
    if (!f) return false;
    for (const auto &e : entries) {
        f << e << "\n";
        if (!f) return false;
    }
    return true;
}

bool checkOverwriteSafe(const fs::path &out_abs,
                         const std::vector<std::string> &new_files,
                         const std::unordered_set<std::string> &tracked,
                         std::string &error_msg) {
    for (const auto &rel : new_files) {
        fs::path abs = out_abs / rel;
        std::error_code ec;
        if (fs::exists(abs, ec) && !ec) {
            if (tracked.find(rel) == tracked.end()) {
                error_msg = "Error: refusing to overwrite untracked file '";
                error_msg += abs.generic_string();
                error_msg += "' \xe2\x80\x94 was it hand-written? Use --out to pick a different directory.\n";
                return false;
            }
        }
    }
    return true;
}

void removeEntryAndPruneParents(const fs::path &out_abs, const std::string &rel) {
    fs::path abs = out_abs / rel;
    std::error_code rm_ec;
    fs::remove(abs, rm_ec);
    fs::path parent = abs.parent_path();
    while (parent != out_abs && parent.has_relative_path()) {
        std::error_code dir_ec;
        if (fs::exists(parent, dir_ec) && !dir_ec &&
            fs::is_empty(parent, dir_ec) && !dir_ec) {
            std::error_code rm_dir_ec;
            fs::remove(parent, rm_dir_ec);
        } else {
            break;
        }
        parent = parent.parent_path();
    }
}

int doClean(const fs::path &out_abs) {
    fs::path manifest_path = out_abs / kManifestFileName;
    std::error_code ec;
    if (!fs::exists(manifest_path, ec) || ec) {
        llvm::errs() << "Error: --clean requires a prior 'ry docs' run (no "
                     << kManifestFileName << " found in " << out_abs.generic_string() << ")\n";
        return 1;
    }
    for (const auto &rel : readDocsManifest(out_abs)) {
        removeEntryAndPruneParents(out_abs, rel);
    }
    std::error_code rm_manifest_ec;
    fs::remove(manifest_path, rm_manifest_ec);
    return 0;
}

void collectRyFiles(const fs::path &src_root, std::vector<fs::path> &out) {
    std::error_code ec;
    if (!fs::exists(src_root, ec)) return;
    for (auto it = fs::recursive_directory_iterator(src_root, ec);
         !ec && it != fs::recursive_directory_iterator();
         it.increment(ec)) {
        const fs::directory_entry &entry = *it;
        std::error_code reg_ec;
        if (entry.is_regular_file(reg_ec) && !reg_ec &&
            entry.path().extension() == ".ry") {
            out.push_back(entry.path());
        }
    }
}

const char *kStyleCss =
    "body{font-family:-apple-system,BlinkMacSystemFont,Helvetica,Arial,sans-serif;"
    "max-width:48rem;margin:2rem auto;padding:0 1rem;color:#24292e;line-height:1.5;}"
    "h1,h2{border-bottom:1px solid #e1e4e8;padding-bottom:0.3em;}"
    "code{background:#f6f8fa;padding:0.1rem 0.4rem;border-radius:4px;font-size:0.9em;}"
    "pre.doc{background:#f6f8fa;padding:0.75rem;border-radius:4px;overflow-x:auto;}"
    ".doc{margin:0.5rem 0;}"
    ".source{color:#586069;font-size:0.875rem;margin-top:0;}"
    "article{margin-bottom:2.5rem;}"
    "a{color:#0366d6;text-decoration:none;}"
    "a:hover{text-decoration:underline;}"
    "ul.modules{padding-left:1.25rem;}";

void appendHtmlPreamble(std::string &out, std::string_view title) {
    out += "<!doctype html>\n<html lang=\"en\">\n<head>\n";
    out += "<meta charset=\"utf-8\">\n";
    out += "<title>";
    out += htmlEscape(title);
    out += "</title>\n";
    out += "<style>";
    out += kStyleCss;
    out += "</style>\n";
    out += "</head>\n<body>\n";
}

std::string buildIndexHtml(const std::vector<DocModule> &modules) {
    std::string out;
    appendHtmlPreamble(out, "API Documentation");
    out += "<h1>API Documentation</h1>\n";
    out += "<ul class=\"modules\">\n";
    for (const auto &mod : modules) {
        std::string escaped_name = htmlEscape(mod.name);
        out += "<li><a href=\"modules/";
        out += escaped_name;
        out += ".html\">";
        out += escaped_name;
        out += "</a></li>\n";
    }
    out += "</ul>\n</body>\n</html>\n";
    return out;
}

std::string jsonEscape(std::string_view s) {
    std::string out;
    out.reserve(s.size() + 2);
    for (char c : s) {
        switch (c) {
            case '"': out += "\\\""; break;
            case '\\': out += "\\\\"; break;
            case '\b': out += "\\b"; break;
            case '\f': out += "\\f"; break;
            case '\n': out += "\\n"; break;
            case '\r': out += "\\r"; break;
            case '\t': out += "\\t"; break;
            default:
                if (static_cast<unsigned char>(c) < 0x20) {
                    char buf[8];
                    std::snprintf(buf, sizeof(buf), "\\u%04x",
                                  static_cast<unsigned int>(static_cast<unsigned char>(c)));
                    out += buf;
                } else {
                    out += c;
                }
        }
    }
    return out;
}

std::string buildDocsJson(const std::vector<DocModule> &modules) {
    std::string out;
    out += "{\n";
    out += "  \"ry_version\": \"";
    out += jsonEscape(RY_VERSION);
    out += "\",\n";
    out += "  \"modules\": [";
    if (modules.empty()) {
        out += "]\n";
    } else {
        out += "\n";
        for (size_t mi = 0; mi < modules.size(); ++mi) {
            const auto &m = modules[mi];
            out += "    {\n";
            out += "      \"name\": \"";
            out += jsonEscape(m.name);
            out += "\",\n";
            out += "      \"path\": \"";
            out += jsonEscape(m.rel_path);
            out += "\",\n";
            out += "      \"symbols\": [";
            if (m.symbols.empty()) {
                out += "]\n";
            } else {
                out += "\n";
                for (size_t si = 0; si < m.symbols.size(); ++si) {
                    const auto &s = m.symbols[si];
                    out += "        {\n";
                    out += "          \"kind\": \"";
                    out += jsonEscape(s.kind);
                    out += "\",\n";
                    out += "          \"name\": \"";
                    out += jsonEscape(s.name);
                    out += "\",\n";
                    out += "          \"signature\": \"";
                    out += jsonEscape(s.signature);
                    out += "\",\n";
                    out += "          \"doc\": \"";
                    out += jsonEscape(s.doc);
                    out += "\",\n";
                    out += "          \"is_public\": ";
                    out += s.is_public ? "true" : "false";
                    out += ",\n";
                    out += "          \"source\": \"";
                    out += jsonEscape(s.source);
                    out += "\"\n";
                    out += "        }";
                    out += si + 1 < m.symbols.size() ? ",\n" : "\n";
                }
                out += "      ]\n";
            }
            out += "    }";
            out += mi + 1 < modules.size() ? ",\n" : "\n";
        }
        out += "  ]\n";
    }
    out += "}\n";
    return out;
}

std::string buildModuleHtml(const DocModule &mod) {
    std::string out;
    std::string escaped_name = htmlEscape(mod.name);
    appendHtmlPreamble(out, mod.name);
    out += "<p><a href=\"../index.html\">&larr; back to index</a></p>\n";
    out += "<h1>";
    out += escaped_name;
    out += "</h1>\n";
    for (const auto &sym : mod.symbols) {
        out += "<article data-kind=\"";
        out += sym.kind;
        out += "\" data-name=\"";
        out += htmlEscape(sym.name);
        out += "\" data-visibility=\"";
        out += sym.is_public ? "public" : "private";
        out += "\">\n";
        out += "<h2><code>";
        out += htmlEscape(sym.signature);
        out += "</code></h2>\n";
        if (!sym.doc.empty()) {
            out += htmlEscapeDoc(sym.doc, sym.is_block_doc);
            out += "\n";
        }
        out += "<p class=\"source\">";
        out += htmlEscape(sym.source);
        out += "</p>\n";
        out += "</article>\n";
    }
    out += "</body>\n</html>\n";
    return out;
}

} // namespace

int cmd_docs(int argc, char *argv[]) {
    std::string out_dir = "docs/api";
    bool include_private = false;
    bool clean = false;

    for (int i = 0; i < argc; ++i) {
        const char *arg = argv[i];
        if (std::strcmp(arg, "--include-private") == 0) {
            include_private = true;
        } else if (std::strcmp(arg, "--clean") == 0) {
            clean = true;
        } else if (std::strcmp(arg, "--emit-json") == 0) {
            // docs.json is always emitted; flag accepted for forward compatibility.
        } else if (std::strncmp(arg, "--out=", 6) == 0) {
            out_dir = arg + 6;
        } else if (std::strcmp(arg, "--out") == 0) {
            if (i + 1 >= argc) {
                llvm::errs() << "Error: --out requires a value\n";
                return 1;
            }
            out_dir = argv[++i];
        } else if (std::strncmp(arg, "--format=", 9) == 0) {
            const char *value = arg + 9;
            if (std::strcmp(value, "html") != 0) {
                llvm::errs() << "Error: unsupported format '" << value
                             << "'; only 'html' is supported\n";
                return 1;
            }
        } else if (std::strcmp(arg, "--format") == 0) {
            if (i + 1 >= argc) {
                llvm::errs() << "Error: --format requires a value (only 'html' is supported)\n";
                return 1;
            }
            const char *value = argv[++i];
            if (std::strcmp(value, "html") != 0) {
                llvm::errs() << "Error: unsupported format '" << value
                             << "'; only 'html' is supported\n";
                return 1;
            }
        } else {
            llvm::errs() << "Error: unknown option '" << arg << "'\n";
            return 1;
        }
    }

    auto root_opt = ry::findProjectRoot();
    if (!root_opt) {
        llvm::errs() << "Error: package.toml not found. Run 'ry init' first.\n";
        return 1;
    }
    fs::path project_root = *root_opt;
    fs::path out_abs = fs::path(out_dir).is_absolute()
                           ? fs::path(out_dir)
                           : project_root / out_dir;

    if (clean) return doClean(out_abs);

    std::string toml = readPackageToml(project_root);
    if (toml.empty()) {
        llvm::errs() << "Error: cannot read package.toml\n";
        return 1;
    }
    ry::ProjectConfig config;
    try {
        config = ry::ProjectConfigParser::load(toml);
    } catch (const std::exception &e) {
        llvm::errs() << "Error: failed to parse package.toml: " << e.what() << "\n";
        return 1;
    }
    std::string src_rel = config.src_dir.empty() ? "src" : config.src_dir;
    fs::path src_root = project_root / src_rel;

    std::vector<fs::path> ry_files;
    collectRyFiles(src_root, ry_files);
    std::sort(ry_files.begin(), ry_files.end());

    ry::SourceManager sm;
    std::vector<DocModule> modules;
    modules.reserve(ry_files.size());

    for (const auto &file : ry_files) {
        DocModule mod;
        mod.name = deriveModuleName(src_root, file);
        mod.rel_path = fs::relative(file, project_root).generic_string();

        try {
            ry::Program prog = ry::loadAndParse(file.string(), &sm);
            collectFromProgram(prog, mod, include_private);
        } catch (const std::exception &e) {
            llvm::errs() << "Warning: failed to parse " << mod.rel_path << ": "
                         << e.what() << "\n";
            continue;
        }

        std::sort(mod.symbols.begin(), mod.symbols.end(),
                  [](const DocSymbol &a, const DocSymbol &b) {
                      if (a.line != b.line) return a.line < b.line;
                      return a.name < b.name;
                  });

        std::string source_prefix = mod.rel_path + ":";
        for (auto &sym : mod.symbols) {
            sym.source = source_prefix + std::to_string(sym.line);
        }

        modules.push_back(std::move(mod));
    }

    std::sort(modules.begin(), modules.end(),
              [](const DocModule &a, const DocModule &b) { return a.name < b.name; });

    std::vector<std::string> planned_files;
    planned_files.reserve(modules.size() + 2);
    planned_files.emplace_back("index.html");
    for (const auto &mod : modules) {
        std::string rel = "modules/";
        rel += mod.name;
        rel += ".html";
        planned_files.push_back(std::move(rel));
    }
    planned_files.emplace_back("docs.json");

    auto tracked_vec = readDocsManifest(out_abs);
    std::unordered_set<std::string> tracked(tracked_vec.begin(), tracked_vec.end());

    std::string err;
    if (!checkOverwriteSafe(out_abs, planned_files, tracked, err)) {
        llvm::errs() << err;
        return 1;
    }

    fs::create_directories(out_abs / "modules");

    auto writeOrFail = [](const fs::path &p, std::string_view content) {
        if (writeOutFile(p, content)) return true;
        llvm::errs() << "Error: failed to write " << p.generic_string() << "\n";
        return false;
    };

    if (!writeOrFail(out_abs / "index.html", buildIndexHtml(modules))) return 1;
    for (const auto &mod : modules) {
        const fs::path p = out_abs / "modules" / (mod.name + ".html");
        if (!writeOrFail(p, buildModuleHtml(mod))) return 1;
    }
    if (!writeOrFail(out_abs / "docs.json", buildDocsJson(modules))) return 1;

    std::unordered_set<std::string> planned_set(planned_files.begin(), planned_files.end());
    for (const auto &rel : tracked_vec) {
        if (!planned_set.count(rel)) removeEntryAndPruneParents(out_abs, rel);
    }

    if (!writeDocsManifest(out_abs, planned_files)) {
        llvm::errs() << "Error: failed to write "
                     << (out_abs / kManifestFileName).generic_string() << "\n";
        return 1;
    }

    return 0;
}
