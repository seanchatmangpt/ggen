from pathlib import Path
import re

root = Path(__file__).resolve().parents[1]
src = root / "src"
dist = root / "dist"
dist.mkdir(exist_ok=True)
summary = (src / "SUMMARY.md").read_text(encoding="utf-8")
links = re.findall(r"\[[^\]]+\]\(([^)]+\.md)\)", summary)
parts = []
for rel in links:
    path = src / rel
    if path.exists():
        parts.append(path.read_text(encoding="utf-8"))
text = "\n\n---\n\n".join(parts)
(dist / "ggen-level-five-packs-v26.7.19.md").write_text(text, encoding="utf-8")
print(f"assembled {len(parts)} markdown files")
