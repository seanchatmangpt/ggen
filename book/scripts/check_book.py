from pathlib import Path
import re
import sys

root = Path(__file__).resolve().parents[1]
src = root / "src"
summary = (src / "SUMMARY.md").read_text(encoding="utf-8")
links = re.findall(r"\[[^\]]+\]\(([^)]+)\)", summary)
missing = [p for p in links if not (src / p).exists()]
chapters = []
for p in links:
    path = src / p
    if path.suffix == ".md" and path.exists():
        text = path.read_text(encoding="utf-8")
        chapters.append(path)
        for marker in ["## Production problem", "## Reference implementation", "## Acceptance gate"]:
            if marker not in text and p != "README.md":
                raise SystemExit(f"{path}: missing {marker}")
        if p != "README.md" and "```" not in text:
            raise SystemExit(f"{path}: no code block")
if missing:
    raise SystemExit("missing SUMMARY links:\n" + "\n".join(missing))
print(f"OK: {len(links)} links, {len(chapters)} markdown files")
