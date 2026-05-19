# Usage

## Install
```bash
# Dry-run
mcpp marketplace install --package academic-bibliography-manager --dry_run
# Install to working directory
mcpp marketplace install --package academic-bibliography-manager
```

## Generate BibTeX from RDF
```bash
mcpp generate \
  --template marketplace/packages/academic-bibliography-manager/templates/bibtex-references.tmpl \
  --domain examples/paper.rdf \
  --output ./out/refs.bib \
  --config examples/config.toml
```

## Validate and Export
```bash
mcpp bibliography validate examples/references.bib

mcpp bibliography export --input examples/references.bib --format csl-json
mcpp bibliography export --input examples/references.bib --format ris
```

## Config Options (config.toml)
- `bibliography_name` (string, default `references`)
- `citation_style` (string, default `ieee`)
- `include_dois` (bool, default `true`)
- `include_urls` (bool, default `true`)

## Expected Files
- `examples/paper.rdf` — sample RDF input
- `examples/references.bib` — sample BibTeX for validation
- `templates/bibtex-references.tmpl` — generator template
- `data/bibtex-schema.ttl` — schema for bib fields
- `tools/bibtex-parser.rs` — reference parser/validator




