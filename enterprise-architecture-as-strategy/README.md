# Enterprise Architecture as Strategy with ggen

This directory contains a standalone mdBook and a portable PDF build path.

## Build and validate

Requirements: Python 3, mdBook, Pandoc, and XeLaTeX.

```bash
./build.sh
```

The build performs four stages:

1. validate every `SUMMARY.md` target;
2. build the mdBook HTML site;
3. assemble the ordered Markdown manuscript;
4. render the PDF with Pandoc and XeLaTeX.

Individual stages may also be run directly:

```bash
python3 scripts/validate.py
mdbook build
python3 scripts/assemble.py
pandoc dist/enterprise-architecture-as-strategy.md \
  -o dist/enterprise-architecture-as-strategy-with-ggen.pdf \
  --pdf-engine=xelatex \
  --resource-path=src \
  --toc \
  -V geometry:margin=0.8in
```

The committed Markdown is authoritative. Generated HTML, combined Markdown, and PDF outputs are release artifacts and should not replace source.
