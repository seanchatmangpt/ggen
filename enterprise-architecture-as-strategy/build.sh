#!/usr/bin/env bash
set -euo pipefail
cd "$(dirname "$0")"

command -v mdbook >/dev/null || { echo "mdbook is required" >&2; exit 127; }
command -v pandoc >/dev/null || { echo "pandoc is required" >&2; exit 127; }
command -v xelatex >/dev/null || { echo "xelatex is required" >&2; exit 127; }

python3 scripts/validate.py
mdbook build
python3 scripts/assemble.py
pandoc dist/enterprise-architecture-as-strategy.md \
  -o dist/enterprise-architecture-as-strategy-with-ggen.pdf \
  --pdf-engine=xelatex \
  --resource-path=src \
  --toc \
  -V geometry:margin=0.8in
