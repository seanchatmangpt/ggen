from pathlib import Path
import hashlib

root = Path(__file__).resolve().parents[1]
rows = []
for path in sorted(p for p in root.rglob("*") if p.is_file() and p.name != "MANIFEST.sha256" and ".git" not in p.parts):
    rows.append(f"{hashlib.sha256(path.read_bytes()).hexdigest()}  {path.relative_to(root).as_posix()}")
(root / "MANIFEST.sha256").write_text("\n".join(rows) + "\n", encoding="utf-8")
print(f"manifested {len(rows)} files")
