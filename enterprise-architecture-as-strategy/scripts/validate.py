from pathlib import Path
import re, sys
root = Path(__file__).resolve().parents[1]
src = root / "src"
summary = (src / "SUMMARY.md").read_text(encoding="utf-8")
links = re.findall(r"\]\(([^)]+)\)", summary)
missing = [p for p in links if not (src / p).exists()]
if missing:
    print("missing:", *missing, sep="\n")
    sys.exit(1)
numbers = []
for p in links:
    m = re.match(r"(?:[^/]+/)?(\d+)-", p)
    if m:
        numbers.append(int(m.group(1)))
if numbers and numbers != sorted(numbers):
    print("chapter order is not monotonic")
    sys.exit(1)
print(f"ALIVE: {len(links)} summary links; all targets exist")
