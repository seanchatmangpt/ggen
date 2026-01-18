# Academic Bibliography Manager

Production-ready bibliography management package with BibTeX parsing/generation, citation validation, duplicate detection, and multi-format export (BibTeX, CSL-JSON, RIS). Ships as a ggen marketplace tool for academic workflows.

## Contents
- Templates: `templates/bibtex-references.tmpl` (RDF → BibTeX generator)
- Schema: `data/bibtex-schema.ttl`
- Tooling: `tools/bibtex-parser.rs` (lightweight parser/validator)
- Examples: `examples/references.bib`, `examples/paper.rdf`, `examples/config.toml`

## Quickstart
1) Install (dry-run first):
```bash
# Preview
ggen marketplace install --package academic-bibliography-manager --dry_run
# Install to current dir
ggen marketplace install --package academic-bibliography-manager
```

2) Generate BibTeX from an RDF paper graph:
```bash
cat > examples/config.toml <<'EOF'
bibliography_name = "references"
citation_style = "ieee"
include_dois = true
include_urls = true
EOF

ggen generate \
  --template marketplace/packages/academic-bibliography-manager/templates/bibtex-references.tmpl \
  --domain examples/paper.rdf \
  --output ./out/refs.bib \
  --config examples/config.toml
```

3) Validate and export:
```bash
ggen bibliography validate examples/references.bib
ggen bibliography export --input examples/references.bib --format csl-json
ggen bibliography export --input examples/references.bib --format ris
```

## Features (80/20)
- BibTeX parsing and generation
- Citation key validation and duplicate detection (parser tool)
- CSL/RIS export paths via CLI commands
- DOI and URL inclusion flags
- RDF-driven generation (paper ontology → BibTeX)

## Notes
- Keep `references.bib` present when running validation.
- Template is deterministic: fixed ordering by year, stable key rendering.
- Parser tool avoids `unwrap` and returns actionable errors.




