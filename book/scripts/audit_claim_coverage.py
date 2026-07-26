#!/usr/bin/env python3
"""Audit the book's capability claims against concrete repository witnesses.

This is intentionally stricter than link validation and intentionally narrower
than a full-workspace test. It asks whether each claim in CAPABILITY_MAP.md has
an extant owner, pack, consumer, or explicit bounded gap.
"""

from __future__ import annotations

from dataclasses import dataclass
from pathlib import Path
import argparse
import re
import sys

REPO = Path(__file__).resolve().parents[2]
BOOK = REPO / "book" / "src"
MAP = BOOK / "CAPABILITY_MAP.md"

STANDINGS = {"IMPLEMENTED", "PACK_WITNESS", "PARTIAL", "TARGET", "ARCHIVE_ONLY"}
PATH_TOKEN = re.compile(r"`([^`]+)`")
PACK_ROW = re.compile(
    r"^\|\s*`(?P<pack>packs/[^`]+)`\s*\|(?P<cap>.*?)\|(?P<evidence>.*?)\|\s*\*\*(?P<standing>[A-Z_]+)\*\*\s*\|$"
)
FIELD_ROW = re.compile(
    r"^\|\s*(?P<field>\*\*.+?)\s*\|(?P<binding>.*?)\|(?P<witnesses>.*?)\|(?P<standing>.*?)\|$"
)


@dataclass(frozen=True)
class Finding:
    severity: str
    code: str
    subject: str
    detail: str


def path_like(token: str) -> bool:
    if token.startswith(("http://", "https://")):
        return False
    if any(ch in token for ch in ("*", "{", "}", "$", "\n")):
        return False
    return "/" in token and not token.startswith("/")


def resolve(token: str) -> bool:
    return (REPO / token.rstrip(".,;:")).exists()


def consumer_refs(pack_name: str) -> list[str]:
    refs: list[str] = []
    needle = f'path = "../../packs/{pack_name}"'
    examples = REPO / "examples"
    if examples.exists():
        for manifest in examples.rglob("ggen.toml"):
            try:
                text = manifest.read_text(encoding="utf-8")
            except UnicodeDecodeError:
                continue
            if needle in text or f"{pack_name} =" in text:
                refs.append(manifest.parent.relative_to(REPO).as_posix())
    return sorted(set(refs))


def audit() -> tuple[list[Finding], dict[str, int]]:
    text = MAP.read_text(encoding="utf-8")
    lines = text.splitlines()
    findings: list[Finding] = []
    counters = {
        "pack_rows": 0,
        "field_rows": 0,
        "chapter_files": 0,
        "chapter_alignment_sections": 0,
        "evidence_paths": 0,
        "consumer_witnesses": 0,
        "declared_gaps": 0,
    }

    # Every path-like repository citation in the capability ledger must resolve.
    for line_no, line in enumerate(lines, 1):
        for token in PATH_TOKEN.findall(line):
            if not path_like(token):
                continue
            counters["evidence_paths"] += 1
            if not resolve(token):
                findings.append(Finding("ERROR", "MISSING_EVIDENCE", f"line {line_no}", token))

    # Pack witness rows require pack law. PACK_WITNESS additionally requires a
    # current consumer or a named test boundary, not merely a pack directory.
    for line_no, line in enumerate(lines, 1):
        match = PACK_ROW.match(line)
        if not match:
            continue
        counters["pack_rows"] += 1
        pack = match.group("pack")
        standing = match.group("standing")
        pack_dir = REPO / pack
        if standing not in STANDINGS:
            findings.append(Finding("ERROR", "UNKNOWN_STANDING", pack, standing))
        if not pack_dir.is_dir():
            findings.append(Finding("ERROR", "MISSING_PACK", pack, "directory does not exist"))
            continue
        if not (pack_dir / "pack.toml").is_file():
            findings.append(Finding("ERROR", "MISSING_PACK_MANIFEST", pack, "pack.toml is absent"))

        name = pack_dir.name
        consumers = consumer_refs(name)
        evidence = match.group("evidence")
        named_test = any(word in evidence.lower() for word in ("test", "consumer", "verify", "e2e"))
        if consumers:
            counters["consumer_witnesses"] += 1
        if standing == "PACK_WITNESS" and not consumers and not named_test:
            findings.append(
                Finding(
                    "ERROR",
                    "UNBOUND_PACK_WITNESS",
                    pack,
                    "PACK_WITNESS has neither a discoverable examples/* consumer nor a named test boundary",
                )
            )
        if standing in {"PARTIAL", "TARGET", "ARCHIVE_ONLY"}:
            counters["declared_gaps"] += 1
            findings.append(
                Finding(
                    "GAP",
                    f"DECLARED_{standing}",
                    pack,
                    "standing is intentionally below PACK_WITNESS and requires a future executable witness",
                )
            )

    # Every field row must carry a recognized standing term. Lower standings are
    # surfaced as gaps, not silently promoted by broad CI.
    in_fields = False
    for line_no, line in enumerate(lines, 1):
        if line.startswith("## Field-to-capability alignment"):
            in_fields = True
            continue
        if in_fields and line.startswith("## "):
            in_fields = False
        if not in_fields:
            continue
        match = FIELD_ROW.match(line)
        if not match or "Book field" in line or line.startswith("|---"):
            continue
        counters["field_rows"] += 1
        standing_text = match.group("standing")
        observed = {item for item in STANDINGS if item in standing_text}
        if not observed:
            findings.append(Finding("ERROR", "FIELD_WITHOUT_STANDING", f"line {line_no}", match.group("field")))
        if observed & {"PARTIAL", "TARGET", "ARCHIVE_ONLY"}:
            counters["declared_gaps"] += 1
            findings.append(
                Finding(
                    "GAP",
                    "FIELD_NOT_CROWN_COMPLETE",
                    re.sub(r"[*`]", "", match.group("field")).strip(),
                    standing_text.strip(),
                )
            )

    # Chapter-level alignment must name owner/evidence/witness/standing/gap/falsifier.
    required = (
        "### Owning capabilities",
        "### Current repository evidence",
        "### Pack witnesses",
        "### Bounded standing",
        "### Open gap",
        "### Required falsifier",
    )
    for chapter in BOOK.rglob("*.md"):
        if chapter.name in {"README.md", "SUMMARY.md", "CAPABILITY_MAP.md"}:
            continue
        counters["chapter_files"] += 1
        chapter_text = chapter.read_text(encoding="utf-8", errors="replace")
        if "## Repository capability alignment" not in chapter_text:
            findings.append(
                Finding("ERROR", "CHAPTER_WITHOUT_ALIGNMENT", chapter.relative_to(REPO).as_posix(), "missing alignment section")
            )
            continue
        counters["chapter_alignment_sections"] += 1
        for marker in required:
            if marker not in chapter_text:
                findings.append(
                    Finding("ERROR", "INCOMPLETE_ALIGNMENT", chapter.relative_to(REPO).as_posix(), f"missing {marker}")
                )

    return findings, counters


def main() -> int:
    parser = argparse.ArgumentParser()
    parser.add_argument(
        "--fail-on-gaps",
        action="store_true",
        help="also fail for explicitly declared PARTIAL/TARGET/ARCHIVE_ONLY surfaces",
    )
    args = parser.parse_args()

    findings, counters = audit()
    for finding in findings:
        print(f"{finding.severity} {finding.code} {finding.subject}: {finding.detail}")

    errors = sum(item.severity == "ERROR" for item in findings)
    gaps = sum(item.severity == "GAP" for item in findings)
    print(
        "CLAIM_COVERAGE "
        + " ".join(f"{key}={value}" for key, value in counters.items())
        + f" errors={errors} gaps={gaps}"
    )
    if errors or (args.fail_on_gaps and gaps):
        return 1
    return 0


if __name__ == "__main__":
    sys.exit(main())
