from pathlib import Path
import re

root = Path(__file__).resolve().parents[1]
src = root / "src"
summary = (src / "SUMMARY.md").read_text(encoding="utf-8")
links = re.findall(r"\[[^\]]+\]\(([^)]+)\)", summary)
missing = [p for p in links if not (src / p).exists()]
if missing:
    raise SystemExit("missing SUMMARY links:\n" + "\n".join(missing))

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
]

chapters = []
for relative in links:
    path = src / relative
    if path.suffix != ".md" or not path.exists():
        continue
    chapters.append(path)
    if relative == "README.md":
        continue
    text = path.read_text(encoding="utf-8")
    for marker in required_pattern_markers:
        if marker not in text:
            raise SystemExit(f"{path}: missing pattern marker {marker}")
    if "```" not in text:
        raise SystemExit(f"{path}: no executable or structural code block")
    if "ALIVE" not in text or "Falsifier" not in text:
        raise SystemExit(f"{path}: missing standing or falsifier vocabulary")

print(f"OK: {len(links)} links, {len(chapters)} markdown files, Alexander grammar admitted")
