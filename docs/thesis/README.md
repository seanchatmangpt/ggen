# ggen Thesis & Research

## Canonical Source

The PhD thesis (LaTeX) is the authoritative version. All other formats are derivatives.

| Version | Path | Status |
|---------|------|--------|
| **PhD Thesis (LaTeX)** | `phd-thesis/` | Canonical — edit here |
| Fusion Thesis (LaTeX) | `fusion-thesis/` | Derivative — autonomic/business focus |
| Executive Outline | `FUSION_THESIS.md` | Summary only |
| Research Papers | `research/` | Supporting papers |

## Build the PDF

```bash
cd phd-thesis
xelatex thesis.tex && bibtex thesis && xelatex thesis.tex && xelatex thesis.tex
# Note: LaTeX compilation has known issues — see phd-thesis/STATUS.md
```

## Thesis Structure (11 chapters)

1. Introduction
2. Foundations
3. Information Theory
4. Category Theory
5. RDF/SPARQL
6. Five-Stage Pipeline (μ₁-μ₅)
7. Ontology Packs
8. Empirical Evaluation
9. Related Work
10. Conclusions
11. Extensions

## Key Thesis Documents

- `phd-thesis/README.md` — build instructions, status
- `phd-thesis/STATUS.md` — compilation status
- `phd-thesis/CODE_VERIFICATION_REPORT.md` — code verification
- `fusion-thesis/chapters/` — 8 chapters on autonomic systems

## Warning

Four thesis versions existed at root level and have been consolidated here.
Do not create new thesis content at the repo root — use this directory.
