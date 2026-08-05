#!/usr/bin/env python3
"""Bound ggen-engine production modules and extract tests from refactored roots."""

from __future__ import annotations

import os
import re
import sys
from pathlib import Path

REPO = Path(__file__).resolve().parents[2]
CRATE = REPO / "crates" / "ggen-engine"
SRC = CRATE / "src"
TEST_ROOT = CRATE / "tests" / "unit"
MAX_SOURCE_LINES = 1_000
LARGE_MODULES = (
    "generation_rules.rs",
    "graph.rs",
    "pack.rs",
    "sync.rs",
    "template.rs",
    "types.rs",
    "write.rs",
)
REFACTORED_MODULES = LARGE_MODULES + ("verbs/handlers.rs",)


def write(path: Path, content: str) -> None:
    path.parent.mkdir(parents=True, exist_ok=True)
    if not content.endswith("\n"):
        content += "\n"
    path.write_text(content)


def scan(text: str) -> tuple[list[int], list[bool]]:
    n = len(text)
    depth = 0
    i = 0
    block = 0
    state = "code"
    raw_hashes = 0
    depths = [0] * (n + 1)
    code = [False] * n
    while i < n:
        depths[i] = depth
        c = text[i]
        nxt = text[i + 1] if i + 1 < n else ""
        if state == "line":
            if c == "\n":
                state = "code"
            i += 1
            continue
        if state == "block":
            if c == "/" and nxt == "*":
                block += 1
                i += 2
                continue
            if c == "*" and nxt == "/":
                block -= 1
                i += 2
                if block == 0:
                    state = "code"
                continue
            i += 1
            continue
        if state == "string":
            if c == "\\":
                i += 2
                continue
            if c == '"':
                state = "code"
            i += 1
            continue
        if state == "char":
            if c == "\\":
                i += 2
                continue
            if c == "'":
                state = "code"
            i += 1
            continue
        if state == "raw":
            if c == '"' and text.startswith("#" * raw_hashes, i + 1):
                i += 1 + raw_hashes
                state = "code"
                continue
            i += 1
            continue

        code[i] = True
        if c == "/" and nxt == "/":
            state = "line"
            i += 2
            continue
        if c == "/" and nxt == "*":
            state = "block"
            block = 1
            i += 2
            continue
        raw_match = None
        if c == "r":
            raw_match = re.match(r'r(#{0,255})"', text[i:])
        elif c == "b" and nxt == "r":
            raw_match = re.match(r'br(#{0,255})"', text[i:])
        if raw_match:
            raw_hashes = len(raw_match.group(1))
            i += len(raw_match.group(0))
            state = "raw"
            continue
        if c == "b" and nxt == '"':
            state = "string"
            i += 2
            continue
        if c == '"':
            state = "string"
            i += 1
            continue
        if c == "'":
            j = i + 1
            j += 2 if j < n and text[j] == "\\" else 1
            if j < n and text[j] == "'":
                state = "char"
                i += 1
                continue
        if c == "{":
            depth += 1
        elif c == "}":
            depth -= 1
        i += 1
    depths[n] = depth
    return depths, code


def matching_brace(text: str, open_index: int, code: list[bool]) -> int:
    depth = 0
    for index in range(open_index, len(text)):
        if not code[index]:
            continue
        if text[index] == "{":
            depth += 1
        elif text[index] == "}":
            depth -= 1
            if depth == 0:
                return index
    raise ValueError("unmatched test-module brace")


def find_test_modules(text: str) -> list[dict[str, object]]:
    depths, code = scan(text)
    pattern = re.compile(r"(?m)^(?P<indent>[ \t]*)#\[cfg\(test\)\][ \t]*\n")
    modules: list[dict[str, object]] = []
    for match in pattern.finditer(text):
        start = match.start()
        base_depth = depths[start]
        pos = match.end()
        while True:
            attribute = re.match(r"[ \t]*#\[[^\n]*\][ \t]*\n", text[pos:])
            if not attribute:
                break
            pos += attribute.end()
        module = re.match(
            r"(?P<indent>[ \t]*)(?P<vis>pub(?:\([^)]*\))?[ \t]+)?"
            r"mod[ \t]+(?P<name>[A-Za-z_][A-Za-z0-9_]*)[ \t]*\{",
            text[pos:],
        )
        if not module:
            continue
        module_start = pos + module.start()
        open_index = pos + module.end() - 1
        if depths[module_start] != base_depth:
            continue
        close_index = matching_brace(text, open_index, code)
        end = close_index + 1
        if end < len(text) and text[end] == "\n":
            end += 1
        indent = module.group("indent")
        inner = text[open_index + 1 : close_index]
        if inner.startswith("\n"):
            inner = inner[1:]
        unit = indent + "    "
        normalized = "".join(
            line[len(unit) :] if line.startswith(unit) else line
            for line in inner.splitlines(True)
        )
        modules.append(
            {
                "start": start,
                "end": end,
                "attrs": text[start:module_start],
                "name": module.group("name"),
                "indent": indent,
                "depth": base_depth,
                "inner": normalized,
            }
        )
    return [
        module
        for module in modules
        if not any(
            parent["start"] < module["start"] and module["end"] <= parent["end"]
            for parent in modules
        )
    ]


def extract_tests(source: Path) -> None:
    text = source.read_text()
    modules = find_test_modules(text)
    for module in reversed(modules):
        relative = source.relative_to(SRC).with_suffix("")
        nested_generation_tests = source.name == "generation_rules.rs" and int(module["depth"]) > 0
        if nested_generation_tests:
            output = TEST_ROOT / "generation_rules" / "merge_tests.rs"
            body = str(module["inner"]).replace("use super::*;", "use super::merge::merge_sections;", 1)
            write(output, body)
            replacement = ""
        else:
            output = (TEST_ROOT / relative / str(module["name"])).with_suffix(".rs")
            write(output, str(module["inner"]))
            attrs = str(module["attrs"])
            indent = str(module["indent"])
            path = os.path.relpath(output, source.parent).replace(os.sep, "/")
            if module["depth"] == 0:
                replacement = f'{attrs}{indent}#[path = "{path}"]\n{indent}mod {module["name"]};\n'
            else:
                replacement = (
                    f"{attrs}{indent}mod {module['name']} {{\n"
                    f'{indent}    include!("{path}");\n'
                    f"{indent}}}\n"
                )
        text = text[: int(module["start"])] + replacement + text[int(module["end"]) :]
    if source.name == "generation_rules.rs" and any(int(module["depth"]) > 0 for module in modules):
        text = text.rstrip() + "\n\n#[cfg(test)]\n#[allow(clippy::unwrap_used, clippy::expect_used)]\n#[path = \"../tests/unit/generation_rules/merge_tests.rs\"]\nmod merge_tests;\n"
    source.write_text(text)


def extract_tail_test_decl(text: str, module_name: str = "tests") -> tuple[str, str]:
    pattern = re.compile(
        r"(?ms)\n#\[cfg\(test\)\]\n(?:#\[[^\n]+\]\n)*"
        r'\#\[path = "[^"]+"\]\nmod '
        + re.escape(module_name)
        + r";\n?\s*$"
    )
    match = pattern.search(text)
    if not match:
        raise ValueError(f"tail test declaration `{module_name}` not found")
    return text[: match.start()] + "\n", text[match.start() + 1 :].rstrip() + "\n"


def split_sync() -> None:
    path = SRC / "sync.rs"
    text = path.read_text()
    start_marker = "    // ── Stage 2b: Law — materialize rules, then gate (denials, SPARQL gates)\n"
    end_marker = "    let graph_hash_hex = hex32(&graph.state_hash()?);\n"
    start = text.index(start_marker)
    end = text.index(end_marker, start)
    block = text[start:end]
    dedented = "".join(line[4:] if line.startswith("    ") else line for line in block.splitlines(True))
    helper = (
        "fn validate_law_stage(\n"
        "    root: &Path,\n"
        "    config: &GgenConfig,\n"
        "    graph: &Arc<dyn GraphEngine>,\n"
        "    packs: &[crate::pack::Pack],\n"
        "    closure: &mut BTreeMap<String, String>,\n"
        ") -> Result<()> {\n"
        + "".join("    " + line if line.strip() else line for line in dedented.splitlines(True))
        + "    Ok(())\n}\n\n"
    )
    text = text[:start] + "    validate_law_stage(root, &config, &graph, &packs, &mut closure)?;\n\n" + text[end:]
    marker = "// ---------------------------------------------------------------------------\n// SPARQL gate queries — the engine-independent sync gate convention\n"
    insert = text.index(marker)
    path.write_text(text[:insert] + helper + text[insert:])

    production, test_decl = extract_tail_test_decl(path.read_text())
    model = production.index("/// Which [`GraphEngine`] a sync runs on.")
    pipeline = production.index("/// Run the five-stage pipeline rooted at `root`")
    law = production.index("fn validate_law_stage(")
    render = production.index("/// Root-relative display form of a closure input path")
    templates = production.index("/// Discover and parse every template a sync would process")
    receipt = production.index("fn read_prev_head(")
    header = production[:model].rstrip() + "\n\n"
    segments = (
        ("model.rs", production[model:pipeline]),
        ("pipeline.rs", production[pipeline:law]),
        ("law.rs", production[law:render]),
        ("render.rs", production[render:templates]),
        ("templates.rs", production[templates:receipt]),
        ("receipt.rs", production[receipt:]),
    )
    for name, content in segments:
        write(SRC / "sync" / name, content)
    path.write_text(header + "".join(f'include!("sync/{name}");\n' for name, _ in segments) + "\n" + test_decl)


def split_graph() -> None:
    path = SRC / "graph.rs"
    production, test_decl = extract_tail_test_decl(path.read_text())
    core = production.index("/// Number of color-refinement iterations")
    law = production.index("// ---------------------------------------------------------------------------\n// GraphLawStore — praxis-graphlaw as the live law-state engine")
    header = production[:core].rstrip() + "\n\n"
    write(SRC / "graph" / "core.rs", production[core:law])
    write(SRC / "graph" / "law.rs", production[law:])
    path.write_text(header + 'include!("graph/core.rs");\ninclude!("graph/law.rs");\n\n' + test_decl)


def split_template() -> None:
    path = SRC / "template.rs"
    production, test_decl = extract_tail_test_decl(path.read_text())
    model = production.index("/// Closed frontmatter key set")
    runtime = production.index("fn collect_files_recursive(")
    header = production[:model].rstrip() + "\n\n"
    write(SRC / "template" / "model.rs", production[model:runtime])
    write(SRC / "template" / "runtime.rs", production[runtime:])
    path.write_text(header + 'include!("template/model.rs");\ninclude!("template/runtime.rs");\n\n' + test_decl)


def split_generation_rules() -> None:
    path = SRC / "generation_rules.rs"
    production, test_decl = extract_tail_test_decl(path.read_text(), "merge_tests")
    pipeline = production.index("pub(crate) fn duration_ms(")
    output = production.index("struct PendingGenWrite")
    header = production[:pipeline].rstrip() + "\n\n"
    write(SRC / "generation_rules" / "pipeline.rs", production[pipeline:output])
    write(SRC / "generation_rules" / "output.rs", production[output:])
    path.write_text(
        header
        + 'include!("generation_rules/pipeline.rs");\ninclude!("generation_rules/output.rs");\n\n'
        + test_decl
    )


def split_handlers() -> None:
    path = SRC / "verbs" / "handlers.rs"
    text = path.read_text()
    start = text.index("/// Resolve the project root: the process working directory.")
    receipts = text.index("pub fn handle_receipt_verify()")
    receipts = text.rfind("///", start, receipts)
    doctor = text.index("/// The `orphaned_artifacts` doctor check:")
    law = text.index("// ---------------------------------------------------------------------------\n// `ggen law *` — law-state operations on the project graph (GraphLaw engine)")
    header = text[:start].rstrip() + "\n\n"
    parts = (
        ("sync_graph.rs", text[start:receipts]),
        ("receipts.rs", text[receipts:doctor]),
        ("doctor.rs", text[doctor:law]),
        ("law.rs", text[law:]),
    )
    for name, content in parts:
        write(SRC / "verbs" / "handlers" / name, content)
    path.write_text(header + "".join(f'include!("handlers/{name}");\n' for name, _ in parts))


def write_layout_guard() -> None:
    modules = "\n".join(f'    "{module}",' for module in REFACTORED_MODULES)
    write(
        CRATE / "tests" / "source_layout.rs",
        f'''//! Structural guard for bounded production source and extracted large-module tests.

use std::path::{{Path, PathBuf}};

const MAX_SOURCE_LINES: usize = 1_000;
const REFACTORED_MODULES: &[&str] = &[
{modules}
];

fn rust_sources(root: &Path) -> Vec<PathBuf> {{
    fn visit(dir: &Path, out: &mut Vec<PathBuf>) {{
        for entry in std::fs::read_dir(dir).expect("read source directory") {{
            let path = entry.expect("read source entry").path();
            if path.is_dir() {{
                visit(&path, out);
            }} else if path.extension().is_some_and(|ext| ext == "rs") {{
                out.push(path);
            }}
        }}
    }}

    let mut files = Vec::new();
    visit(root, &mut files);
    files.sort();
    files
}}

#[test]
fn production_rust_files_are_bounded_and_large_module_tests_are_out_of_tree() {{
    let manifest = Path::new(env!("CARGO_MANIFEST_DIR"));
    let source_root = manifest.join("src");
    let mut violations = Vec::new();

    for path in rust_sources(&source_root) {{
        let source = std::fs::read_to_string(&path).expect("read Rust source");
        let line_count = source.lines().count();
        let relative_path = path.strip_prefix(&source_root).unwrap_or(&path);
        let relative = relative_path.display();

        if line_count > MAX_SOURCE_LINES {{
            violations.push(format!(
                "src/{{relative}}: {{line_count}} lines exceeds {{MAX_SOURCE_LINES}}"
            ));
        }}

        let relative_string = relative_path.to_string_lossy();
        if REFACTORED_MODULES.contains(&relative_string.as_ref())
            && (source.contains("#[test]")
                || source.contains("#[tokio::test]")
                || source.contains("#[rstest]"))
        {{
            violations.push(format!(
                "src/{{relative}}: executable test body remains in a refactored module"
            ));
        }}
    }}

    assert!(
        violations.is_empty(),
        "source-layout contract violated:\n{{}}",
        violations.join("\n")
    );
}}
''',
    )


def verify() -> None:
    violations: list[str] = []
    for path in sorted(SRC.rglob("*.rs")):
        source = path.read_text()
        lines = len(source.splitlines())
        relative = path.relative_to(SRC).as_posix()
        if lines > MAX_SOURCE_LINES:
            violations.append(f"src/{relative}: {lines} lines")
        if relative in REFACTORED_MODULES and any(
            marker in source for marker in ("#[test]", "#[tokio::test]", "#[rstest]")
        ):
            violations.append(f"src/{relative}: executable test body remains")
    if violations:
        raise SystemExit("layout verification failed:\n" + "\n".join(violations))


for relative in LARGE_MODULES:
    extract_tests(SRC / relative)
split_sync()
split_graph()
split_template()
split_generation_rules()
split_handlers()
write_layout_guard()
verify()
print("ggen-engine refactor manufactured and structurally verified")
