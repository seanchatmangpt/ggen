#!/usr/bin/env bash
set -euo pipefail
cd "$(dirname "$0")"
mdbook build
python3 scripts/assemble.py
python /home/oai/skills/pdfs/scripts/md_to_pdf.py \
  dist/enterprise-architecture-as-strategy.md \
  --output dist/enterprise-architecture-as-strategy-with-ggen.pdf \
  --pdf_engine xelatex \
  --resource_path src \
  --extra=--toc \
  --extra=-V \
  --extra=geometry:margin=0.8in
