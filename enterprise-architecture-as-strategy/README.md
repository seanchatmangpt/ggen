# Enterprise Architecture as Strategy with ggen

This directory contains a standalone mdBook.

## Build HTML

```bash
mdbook build
```

## Build the combined manuscript and PDF

```bash
python3 scripts/validate.py
python3 scripts/assemble.py
python /home/oai/skills/pdfs/scripts/md_to_pdf.py \
  dist/enterprise-architecture-as-strategy.md \
  --output dist/enterprise-architecture-as-strategy-with-ggen.pdf \
  --pdf_engine xelatex \
  --resource_path src \
  --extra=--toc
```

The committed Markdown is authoritative. Generated HTML and PDF outputs are release artifacts and should not replace source.
