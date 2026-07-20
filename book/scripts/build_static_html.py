from pathlib import Path
import html
import re

root = Path(__file__).resolve().parents[1]
src_md = root / "dist/ggen-level-five-packs-v26.7.19.md"
out = root / "dist/ggen-level-five-packs-v26.7.19.html"
text = src_md.read_text(encoding="utf-8")
# Deliberately simple, dependency-free readable rendering.
escaped = html.escape(text)
escaped = re.sub(r"^# (.+)$", r"<h1>\1</h1>", escaped, flags=re.M)
escaped = re.sub(r"^## (.+)$", r"<h2>\1</h2>", escaped, flags=re.M)
escaped = re.sub(r"^### (.+)$", r"<h3>\1</h3>", escaped, flags=re.M)
html_doc = f"<!doctype html><meta charset='utf-8'><title>ggen Level Five Packs</title><style>body{{max-width:980px;margin:3rem auto;font:17px/1.55 system-ui;padding:0 2rem}}pre{{white-space:pre-wrap;background:#f4f4f4;padding:1rem}}h1{{margin-top:4rem}}</style><body><pre>{escaped}</pre></body>"
out.write_text(html_doc, encoding="utf-8")
print(out)
