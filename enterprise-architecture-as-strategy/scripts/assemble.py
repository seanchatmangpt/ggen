from pathlib import Path
import re
root = Path(__file__).resolve().parents[1]
src = root / "src"
summary = (src / "SUMMARY.md").read_text(encoding="utf-8")
paths = re.findall(r"\]\(([^)]+\.md)\)", summary)
seen = set()
ordered = []
for path in paths:
    if path == "README.md" or path in seen:
        continue
    seen.add(path)
    ordered.append(path)
out = [
    "---",
    'title: "Enterprise Architecture as Strategy with ggen"',
    'author: "Sean Chatman"',
    'date: "Version 26.7.30"',
    "lang: en-US",
    "documentclass: book",
    "classoption: openany",
    "fontsize: 10pt",
    "toc: true",
    "colorlinks: true",
    "---",
    "",
    (src / "README.md").read_text(encoding="utf-8"),
]
for path in ordered:
    out.extend(["", "\\newpage", "", (src / path).read_text(encoding="utf-8")])
dist = root / "dist"
dist.mkdir(exist_ok=True)
(dist / "enterprise-architecture-as-strategy.md").write_text("\n".join(out), encoding="utf-8")
