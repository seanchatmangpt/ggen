#!/usr/bin/env python3
"""Render one Mermaid class-diagram Markdown file per Rust source file.

The parser phase is deterministic and source-file scoped. The renderer phase can
be delegated to ggen through GGEN_CLASS_DIAGRAM_RENDER_CMD. The command receives
three environment variables:

  GGEN_CLASS_DIAGRAM_CONTEXT  JSON context path
  GGEN_CLASS_DIAGRAM_TEMPLATE Tera template path
  GGEN_CLASS_DIAGRAM_OUTPUT   output Markdown path

When no command is configured, the built-in renderer is used for local replay.
"""

from __future__ import annotations

import argparse
import hashlib
import json
import os
import re
import shlex
import subprocess
import sys
from dataclasses import asdict, dataclass
from pathlib import Path
from typing import Iterable

ITEM_RE = re.compile(
    r"(?m)^\s*(?:pub(?:\([^)]*\))?\s+)?"
    r"(?P<kind>struct|enum|trait|type|fn|mod)\s+"
    r"(?P<name>[A-Za-z_][A-Za-z0-9_]*)"
)
IMPL_RE = re.compile(
    r"(?m)^\s*impl(?:<[^>{}]*>)?\s+"
    r"(?:(?P<trait>[A-Za-z_][A-Za-z0-9_:<>]*)\s+for\s+)?"
    r"(?P<target>[A-Za-z_][A-Za-z0-9_:<>]*)"
)
USE_RE = re.compile(r"(?m)^\s*(?:pub\s+)?use\s+([^;]+);")
FIELD_RE = re.compile(
    r"(?m)^\s*(?:pub(?:\([^)]*\))?\s+)?"
    r"(?P<name>[A-Za-z_][A-Za-z0-9_]*)\s*:\s*(?P<type>[^,\n]+)"
)
METHOD_RE = re.compile(
    r"(?m)^\s*(?:pub(?:\([^)]*\))?\s+)?"
    r"(?:async\s+)?fn\s+(?P<name>[A-Za-z_][A-Za-z0-9_]*)\s*"
    r"(?P<sig>\([^)]*\)(?:\s*->\s*[^{;\n]+)?)"
)

EXCLUDED_PARTS = {
    ".git", "target", "node_modules", "vendor", "generated", "archive",
    "archive_2025", "tcps-generated",
}

@dataclass(frozen=True)
class Symbol:
    kind: str
    name: str
    fields: tuple[str, ...] = ()
    methods: tuple[str, ...] = ()

@dataclass(frozen=True)
class FileModel:
    source: str
    digest: str
    symbols: tuple[Symbol, ...] = ()
    implementations: tuple[str, ...] = ()
    dependencies: tuple[str, ...] = ()
    generated: bool = False

def strip_comments_and_strings(source: str) -> str:
    """Preserve newlines while masking comments and string contents."""
    out: list[str] = []
    i = 0
    n = len(source)
    state = "code"
    raw_hashes = 0
    while i < n:
        ch = source[i]
        nxt = source[i + 1] if i + 1 < n else ""
        if state == "code":
            if ch == "/" and nxt == "/":
                state = "line_comment"; out += [" ", " "]; i += 2; continue
            if ch == "/" and nxt == "*":
                state = "block_comment"; out += [" ", " "]; i += 2; continue
            if ch == '"':
                state = "string"; out.append(" "); i += 1; continue
            if ch == "r":
                m = re.match(r'r(#{0,16})"', source[i:])
                if m:
                    raw_hashes = len(m.group(1)); state = "raw"
                    out.extend(" " * len(m.group(0))); i += len(m.group(0)); continue
            out.append(ch); i += 1
        elif state == "line_comment":
            if ch == "\n":
                state = "code"; out.append("\n")
            else:
                out.append(" ")
            i += 1
        elif state == "block_comment":
            if ch == "*" and nxt == "/":
                out += [" ", " "]; i += 2; state = "code"
            else:
                out.append("\n" if ch == "\n" else " "); i += 1
        elif state == "string":
            if ch == "\\":
                out.append(" "); i += 1
                if i < n:
                    out.append("\n" if source[i] == "\n" else " "); i += 1
            elif ch == '"':
                out.append(" "); i += 1; state = "code"
            else:
                out.append("\n" if ch == "\n" else " "); i += 1
        else:
            end = '"' + ('#' * raw_hashes)
            if source.startswith(end, i):
                out.extend(" " * len(end)); i += len(end); state = "code"
            else:
                out.append("\n" if ch == "\n" else " "); i += 1
    return "".join(out)

def block_after(source: str, start: int) -> str:
    brace = source.find("{", start)
    semi = source.find(";", start)
    if brace < 0 or (semi >= 0 and semi < brace):
        return ""
    depth = 0
    for i in range(brace, len(source)):
        if source[i] == "{":
            depth += 1
        elif source[i] == "}":
            depth -= 1
            if depth == 0:
                return source[brace + 1:i]
    return ""

def parse_rust(path: Path, root: Path) -> FileModel:
    raw = path.read_text(encoding="utf-8")
    clean = strip_comments_and_strings(raw)
    depth = 0
    depth_at = [0] * (len(clean) + 1)
    for index, ch in enumerate(clean):
        depth_at[index] = depth
        if ch == "{":
            depth += 1
        elif ch == "}":
            depth = max(0, depth - 1)

    symbols: list[Symbol] = []
    for match in ITEM_RE.finditer(clean):
        if depth_at[match.start()] != 0:
            continue
        kind, name = match.group("kind"), match.group("name")
        body = block_after(clean, match.end())
        fields: tuple[str, ...] = ()
        methods: tuple[str, ...] = ()
        if kind == "struct" and body:
            fields = tuple(
                f"{m.group('name')}: {re.sub(r'\\s+', ' ', m.group('type').strip())}"
                for m in FIELD_RE.finditer(body)
            )
        if kind == "trait" and body:
            methods = tuple(
                f"{m.group('name')}{re.sub(r'\\s+', ' ', m.group('sig').strip())}"
                for m in METHOD_RE.finditer(body)
            )
        symbols.append(Symbol(kind=kind, name=name, fields=fields, methods=methods))
    impls = []
    for match in IMPL_RE.finditer(clean):
        if depth_at[match.start()] != 0:
            continue
        trait = match.group("trait")
        target = match.group("target")
        impls.append(f"{trait} for {target}" if trait else target)
    deps = sorted({re.sub(r"\s+", " ", m.group(1).strip()) for m in USE_RE.finditer(clean)})
    rel = path.relative_to(root).as_posix()
    generated = "generated" in path.parts or "GENERATED" in raw[:512]
    return FileModel(
        source=rel,
        digest=hashlib.sha256(raw.encode()).hexdigest(),
        symbols=tuple(symbols),
        implementations=tuple(sorted(set(impls))),
        dependencies=tuple(deps),
        generated=generated,
    )

def safe_mermaid(value: str) -> str:
    return value.replace('"', "'").replace("<", "~").replace(">", "~")

def render_builtin(model: FileModel) -> str:
    lines = [
        f"# `{model.source}`", "",
        f"Source SHA-256: `{model.digest}`", "",
        "```mermaid", "classDiagram",
    ]
    if not model.symbols:
        ident = re.sub(r"[^A-Za-z0-9_]", "_", model.source)
        lines += [f"    class {ident} {{", "      <<module>>", "    }"]
    for symbol in model.symbols:
        ident = re.sub(r"[^A-Za-z0-9_]", "_", f"{symbol.kind}_{symbol.name}")
        stereotype = "generated" if model.generated else symbol.kind
        lines += [f"    class {ident} {{", f"      <<{stereotype}>>"]
        for field in symbol.fields:
            lines.append(f'      +"{safe_mermaid(field)}"')
        for method in symbol.methods:
            lines.append(f'      +"{safe_mermaid(method)}"')
        lines.append("    }")
    for impl in model.implementations:
        lines.append(f'    note "{safe_mermaid(impl)}"')
    lines += ["```", "", "## Dependencies", ""]
    lines += [f"- `{dep}`" for dep in model.dependencies] or ["- None observed."]
    lines += ["", "## Standing", "", "- Structural parse: `ALIVE`", "- Runtime behavior: `UNKNOWN`", ""]
    return "\n".join(lines)

def output_path(root: Path, output_root: Path, source: Path) -> Path:
    rel = source.relative_to(root)
    return output_root / Path(str(rel) + ".md")

def invoke_ggen(command: str, context: Path, template: Path, output: Path) -> None:
    env = os.environ.copy()
    env.update({
        "GGEN_CLASS_DIAGRAM_CONTEXT": str(context),
        "GGEN_CLASS_DIAGRAM_TEMPLATE": str(template),
        "GGEN_CLASS_DIAGRAM_OUTPUT": str(output),
    })
    subprocess.run(shlex.split(command), check=True, env=env)

def iter_sources(root: Path) -> Iterable[Path]:
    for path in sorted(root.rglob("*.rs")):
        if any(part in EXCLUDED_PARTS for part in path.relative_to(root).parts):
            continue
        yield path

def main() -> int:
    parser = argparse.ArgumentParser()
    parser.add_argument("--root", type=Path, default=Path("."))
    parser.add_argument("--output", type=Path, default=Path("docs/architecture/class-diagrams/files"))
    parser.add_argument("--template", type=Path, default=Path("templates/class-diagram.md.tera"))
    parser.add_argument("--ggen-command", default=os.getenv("GGEN_CLASS_DIAGRAM_RENDER_CMD"))
    parser.add_argument("--check", action="store_true")
    parser.add_argument("--source", action="append", type=Path)
    args = parser.parse_args()

    root = args.root.resolve()
    output_root = (root / args.output).resolve()
    sources = [root / p for p in args.source] if args.source else list(iter_sources(root))
    if not sources:
        raise SystemExit("no Rust source files admitted")

    changed = 0
    for source in sources:
        model = parse_rust(source.resolve(), root)
        destination = output_path(root, output_root, source.resolve())
        destination.parent.mkdir(parents=True, exist_ok=True)
        expected = render_builtin(model)
        if args.ggen_command:
            context = destination.with_suffix(destination.suffix + ".json")
            context.write_text(json.dumps(asdict(model), indent=2) + "\n", encoding="utf-8")
            invoke_ggen(args.ggen_command, context, root / args.template, destination)
            context.unlink()
            expected = destination.read_text(encoding="utf-8")
        current = destination.read_text(encoding="utf-8") if destination.exists() else None
        if args.check:
            if current != expected:
                print(f"DRIFT {destination.relative_to(root)}", file=sys.stderr)
                changed += 1
        elif current != expected:
            destination.write_text(expected, encoding="utf-8")
            changed += 1

    print(json.dumps({"sources": len(sources), "changed": changed, "check": args.check}))
    return 1 if args.check and changed else 0

if __name__ == "__main__":
    raise SystemExit(main())
