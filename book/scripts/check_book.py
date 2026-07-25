from pathlib import Path
import re

root = Path(__file__).resolve().parents[1]
repo = root.parent
src = root / "src"
summary = (src / "SUMMARY.md").read_text(encoding="utf-8")
links = re.findall(r"\[[^\]]+\]\(([^)]+)\)", summary)
missing = [p for p in links if not (src / p).exists()]
if missing:
    raise SystemExit("missing SUMMARY links:\n" + "\n".join(missing))

capability_map_path = src / "CAPABILITY_MAP.md"
if not capability_map_path.exists():
    raise SystemExit("missing book/src/CAPABILITY_MAP.md")
capability_map = capability_map_path.read_text(encoding="utf-8")

required_standing = ["IMPLEMENTED", "PACK_WITNESS", "PARTIAL", "TARGET", "ARCHIVE_ONLY"]
for value in required_standing:
    if value not in capability_map:
        raise SystemExit(f"CAPABILITY_MAP.md: missing standing value {value}")

# Every SUMMARY field must have an explicit alignment row. Field titles may be
# abbreviated in the table, so the Roman numeral is the stable key.
field_numbers = re.findall(r"^# Field ([IVXLCDM]+) —", summary, flags=re.MULTILINE)
for number in field_numbers:
    if not re.search(rf"\| \*\*{re.escape(number)}\.", capability_map):
        raise SystemExit(f"CAPABILITY_MAP.md: missing alignment for Field {number}")

# Repository paths written in backticks are admitted only when they resolve.
# Exempt commands, glob-like examples and conceptual crate aliases.
path_candidates = set(re.findall(r"`((?:crates|packs|examples|tests|book|marketplace)/[^`]+)`", capability_map))
unresolved = []
for candidate in sorted(path_candidates):
    if any(token in candidate for token in ("*", "<", ">", " ")):
        continue
    if not (repo / candidate.rstrip("/")).exists():
        unresolved.append(candidate)
if unresolved:
    raise SystemExit("CAPABILITY_MAP.md: unresolved repository paths:\n" + "\n".join(unresolved))

# The capability map is a repository ledger and intentionally does not use the
# Alexander pattern grammar.  Validate its own required structure instead.
for marker in (
    "## Standing vocabulary",
    "## Capability ownership",
    "## Pack witnesses",
    "## Field-to-capability alignment",
    "## Chapter citation rule",
    "## Verification",
):
    if marker not in capability_map:
        raise SystemExit(f"CAPABILITY_MAP.md: missing ledger marker {marker}")

required_pattern_markers = [
    "## Context",
    "## Problem",
    "## Forces",
    "## Therefore",
    "## Configuration",
    "## Reference implementation",
    "## Verification procedure",
    "## Resulting context",
    "## Falsifier",
    "## Laboratory",
    "## Acceptance gate",
    "## Connections",
    "## Standing statement",
    "## Repository capability alignment",
]

chapters = []
pattern_chapters = []
for relative in links:
    path = src / relative
    if path.suffix != ".md" or not path.exists():
        continue
    chapters.append(path)
    if relative in {"README.md", "CAPABILITY_MAP.md"}:
        continue
    pattern_chapters.append(path)
    text = path.read_text(encoding="utf-8")
    for marker in required_pattern_markers:
        if marker not in text:
            raise SystemExit(f"{path}: missing pattern marker {marker}")
    if "```" not in text:
        raise SystemExit(f"{path}: no executable or structural code block")
    if "ALIVE" not in text or "Falsifier" not in text:
        raise SystemExit(f"{path}: missing standing or falsifier vocabulary")

print(
    f"OK: {len(links)} links, {len(chapters)} markdown files, "
    f"{len(pattern_chapters)} patterns, {len(field_numbers)} fields aligned"
)
