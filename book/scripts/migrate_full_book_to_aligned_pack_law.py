#!/usr/bin/env python3
"""Rebuild, rewrite, align, and promote the complete ggen pack-writer book.

This one-time migration repairs the authority inversion created when generated
book/src files were edited directly. It uses every existing book:Chapter in
level-five-book-pack, reconstructs a complete 19-field SUMMARY, applies the
Alexander rewrite to the complete linked set, appends real repository evidence,
and writes the exact resulting bytes back into book:sourceText pack law.
"""

from __future__ import annotations

from collections import defaultdict
from pathlib import Path
import re
import subprocess
import sys

import align_and_promote_book_source as base
from book_ontology_source import parse_chapters, replace_sources

REPO = base.REPO
SRC = base.SRC
ONTOLOGY = base.ONTOLOGY
SUMMARY = base.SUMMARY
CAPABILITY_MAP_PATH = "CAPABILITY_MAP.md"

FIELD_SPECS = [
    (
        "Field I — The Whole Manufacturing System",
        "These patterns establish the largest boundary: a pack is a lawful manufacturing system spanning admission, projection, bounded writes, consumers, and receipts.",
        ("foundations",),
    ),
    (
        "Field II — The Pack as a Living Part",
        "A pack must have identity, a declared boundary, a consumption path, owned outputs, executable gates, and replayable provenance.",
        ("pack-model",),
    ),
    (
        "Field III — Choose the Right Kind of Part",
        "Select the smallest pack form that carries the required law, behavior, proof, and lifecycle.",
        ("pack-taxonomy",),
    ),
    (
        "Field IV — Standing before Scale",
        "Advance only through observed maturity cells; a directory or successful render is not completion.",
        ("maturity",),
    ),
    (
        "Field V — Independent Reality",
        "A pack cannot certify itself against an oracle manufactured from the same assumptions.",
        ("reference",),
    ),
    (
        "Field VI — The Ontology Neighborhood",
        "Model stable semantic identities and relations before selecting target-language syntax.",
        ("ontology",),
    ),
    (
        "Field VII — Admission Gates",
        "Parse first, admit second; malformed or unsafe states must be refused before actuation.",
        ("admission",),
    ),
    (
        "Field VIII — Deterministic Projection",
        "SPARQL selects admitted facts and Tera projects them without silently broadening ownership.",
        ("sparql", "tera"),
    ),
    (
        "Field IX — Complete Product Surfaces",
        "Generate coherent modules, crates, interfaces, and manifests rather than fragments requiring hidden repair.",
        ("rust-generation",),
    ),
    (
        "Field X — Proof beside Product",
        "Every generated capability needs an independently meaningful verifier at the same scale.",
        ("generated-proof",),
    ),
    (
        "Field XI — The Consumer Place",
        "Standing is established in a clean consumer without access to the pack author's tacit knowledge.",
        ("consumer",),
    ),
    (
        "Field XII — The Pack Neighborhood",
        "Composition creates a union graph and shared output tree, introducing interference forces that must be proven absent.",
        ("composition",),
    ),
    (
        "Field XIII — Time, Change, and Repair",
        "Regeneration is ordinary operation and must preserve lawful local work while exposing drift.",
        ("regeneration",),
    ),
    (
        "Field XIV — Receipts and Standing",
        "Evidence binds admitted observations, actuation, consequences, and replayable standing.",
        ("receipts",),
    ),
    (
        "Field XV — Change the Engine Only at the Boundary",
        "Prefer pack law; extend shared engine machinery only for semantics no pack can lawfully carry alone.",
        ("engine",),
    ),
    (
        "Field XVI — The Level Five Sequence",
        "Apply the patterns as a generative sequence that names every artifact, verifier, drift rule, and receipt before implementation closes options.",
        ("level-five-design",),
    ),
    (
        "Field XVII — A Complete Pattern in Practice: TCPS",
        "TCPS demonstrates the language at product scale through canonical vocabulary, complete generation, release manufacturing, defects, and standing.",
        ("tcps-core", "tcps-generation", "tcps-product", "tcps-release", "tcps-failures", "tcps-standing"),
    ),
    (
        "Field XVIII — Make a New Language of Your Own",
        "Apply the complete language to a new bounded domain and manufacture a certification bundle rather than a demonstration.",
        ("practicum",),
    ),
    (
        "Field XIX — Certification Laboratories",
        "Break, observe, repair, and receipt each crown property under adversarial laboratory conditions.",
        ("certification",),
    ),
]

FRONT_ORDER = {
    "front-matter/preface.md": 10,
    "front-matter/audience.md": 20,
    "front-matter/level-five-outcome.md": 30,
    "front-matter/how-to-read.md": 40,
    "front-matter/tcps-case-study.md": 50,
    "front-matter/notation-and-standing.md": 60,
    "front-matter/laboratory-requirements.md": 70,
    "front-matter/final-acceptance-test.md": 80,
    "SOURCE_NOTES.md": 90,
}


def path_category(path: str) -> str:
    if "/" not in path:
        return "front-matter"
    return path.split("/", 1)[0]


def sort_key(path: str) -> tuple[int, str]:
    if path in FRONT_ORDER:
        return FRONT_ORDER[path], path
    name = Path(path).name
    match = re.match(r"(\d+)", name)
    if match:
        return int(match.group(1)), path
    appendix = re.match(r"([a-z])-", name, re.IGNORECASE)
    if appendix:
        return 10_000 + ord(appendix.group(1).lower()), path
    return 50_000, path


def chapter_label(path: str) -> str:
    text = (SRC / path).read_text(encoding="utf-8")
    for line in text.splitlines():
        if line.startswith("# "):
            return line[2:].strip()
    return Path(path).stem.replace("-", " ").title()


def bullet(path: str) -> str:
    return f"- [{chapter_label(path)}]({path})"


def build_complete_summary(paths: list[str]) -> str:
    groups: dict[str, list[str]] = defaultdict(list)
    for path in paths:
        if path in {"README.md", "SUMMARY.md", CAPABILITY_MAP_PATH}:
            continue
        groups[path_category(path)].append(path)

    lines = [
        "# Summary",
        "",
        "[**The ggen Pack Language: Patterns for Manufacturing Verified Software**](README.md)",
        "",
        "> A complete pattern language generated from every `book:Chapter` carried by `level-five-book-pack`.",
        "> Read from the whole manufacturing system toward local construction, then return through consumers, verification, receipts, and replay.",
        "",
        "# Using the Pattern Language",
        "",
    ]

    for path in sorted(groups.get("front-matter", []), key=sort_key):
        lines.append(bullet(path))

    lines.extend(["", "---", ""])

    admitted_categories: set[str] = {"front-matter"}
    for heading, description, categories in FIELD_SPECS:
        admitted_categories.update(categories)
        lines.extend([f"# {heading}", "", description, ""])
        field_paths: list[str] = []
        for category in categories:
            field_paths.extend(groups.get(category, []))
        for path in sorted(field_paths, key=sort_key):
            lines.append(bullet(path))
        lines.append("")

    lines.extend(["---", "", "# Pattern Reference", ""])
    for path in sorted(groups.get("appendices", []), key=sort_key):
        lines.append(bullet(path))
    lines.append("")
    admitted_categories.add("appendices")

    unexpected = sorted(set(groups) - admitted_categories)
    if unexpected:
        raise SystemExit(f"chapter categories missing from 19-field summary: {unexpected}")

    return "\n".join(lines)


def add_capability_link(summary: str) -> str:
    heading = "# Using the Pattern Language\n"
    link = "- [Repository Capability and Pack Map](CAPABILITY_MAP.md)\n"
    if heading not in summary:
        raise SystemExit("generated summary lost Using the Pattern Language heading")
    return summary.replace(heading, heading + "\n" + link, 1)


def main() -> None:
    ontology = ONTOLOGY.read_text(encoding="utf-8")
    ontology = base.append_capability_subject(ontology)
    records = parse_chapters(ontology)
    if len(records) < 367:
        raise SystemExit(f"expected at least 367 chapter records; found {len(records)}")

    paths = [record.path for record in records]
    SUMMARY.write_text(build_complete_summary(paths), encoding="utf-8")

    # With the full graph restored, the existing deterministic rewriter now
    # processes every linked chapter rather than the earlier hand-selected set.
    subprocess.run(
        [sys.executable, str(BOOK_REWRITER)],
        cwd=REPO,
        check=True,
    )

    SUMMARY.write_text(
        add_capability_link(SUMMARY.read_text(encoding="utf-8")),
        encoding="utf-8",
    )

    aligned: dict[str, str] = {}
    for path in paths:
        target = SRC / path
        if not target.exists():
            raise SystemExit(f"chapter product missing: {path}")
        updated = base.align_chapter(path, target.read_text(encoding="utf-8"))
        target.write_text(updated, encoding="utf-8")
        aligned[path] = base.escape_turtle_long(updated)

    updated_ontology = replace_sources(ontology, aligned)
    ONTOLOGY.write_text(updated_ontology.rstrip() + "\n", encoding="utf-8")

    summary_links = re.findall(
        r"\[[^\]]+\]\(([^)]+\.md)\)",
        SUMMARY.read_text(encoding="utf-8"),
    )
    expected_links = {path for path in paths if path != "SUMMARY.md"}
    if set(summary_links) != expected_links:
        missing = sorted(expected_links - set(summary_links))
        extra = sorted(set(summary_links) - expected_links)
        raise SystemExit(f"complete summary mismatch: missing={missing} extra={extra}")

    print(
        f"complete book promoted: chapters={len(paths)} summary_links={len(summary_links)} "
        f"ontology={ONTOLOGY.relative_to(REPO)}"
    )


BOOK_REWRITER = REPO / "book" / "scripts" / "rewrite_pattern_language.py"


if __name__ == "__main__":
    main()
