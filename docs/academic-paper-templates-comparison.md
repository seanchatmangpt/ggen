<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->
**Table of Contents**

- [Academic Paper Templates Comparison](#academic-paper-templates-comparison)
  - [Quick comparison](#quick-comparison)
  - [Choosing the right template](#choosing-the-right-template)
  - [Notable differentiators (once wired)](#notable-differentiators-once-wired)
  - [Typical CLI flows (from package examples) — these currently stub out rendering/validation](#typical-cli-flows-from-package-examples--these-currently-stub-out-renderingvalidation)
  - [Quick recommendations](#quick-recommendations)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

# Academic Paper Templates Comparison

This note compares the four `academic-paper` marketplace packages so you can pick the right template quickly. Current state: only the metadata `package.toml` files exist under `marketplace/packages/*`; the actual template payloads (README/USAGE/templates/TTL/examples) are not present there. The previous `ggen paper` subcommands have been removed because they were stubs; wiring to real templates under `templates/papers/` is still needed before these packages can be used end-to-end.

## Quick comparison

| Package | Path | Target format | Key strengths | Required inputs (variables + validation) |
| --- | --- | --- | --- | --- |
| arxiv-paper-template | `marketplace/packages/arxiv-paper-template` (metadata only) | arXiv preprint (single column) | arXiv-compliant LaTeX, metadata extraction, submission API support, DOI + version tracking | Variables: `title`, `authors`, `abstract`, `arxiv_category` (default `cs.AI`), `subjects`.<br>Validation requires: `paper.rdf`, `references.bib`; category must be one of configured arXiv codes. |
| ieee-paper-template | `marketplace/packages/ieee-paper-template` (metadata only) | IEEE conference (two-column) | Author/affiliation management, BibTeX, figure/table generation from RDF, citation checks, PDF compilation flows | Variables: `paper_title`, `authors`, `abstract`, `keywords` (array), `discipline` (default `Computer Science`).<br>Validation requires: `paper.rdf`, `references.bib`; checks: metadata, citations, formatting, compliance. |
| neurips-paper-template | `marketplace/packages/neurips-paper-template` (metadata only) | NeurIPS conference (double-blind) | Anonymization, appendix handling, algorithm env, page-limit enforcement (8 pages + refs), submission validation | Variables: `title`, `authors` (anonymized), `abstract` (100-150 words), `keywords`.<br>Validation: strict compliance/page-limit/citations; requires `paper.rdf`. |
| phd-thesis-template | `marketplace/packages/phd-thesis-template` (metadata only) | PhD thesis (multi-chapter, v2.0) | Front/back matter, chapter structure, appendices, biblatex/biber, cleveref, microtype, university-compliance options, enhanced figures/tables/code listings | Variables: `title`, `author`, `department`, `university` (default "University"), `degree` (default "Doctor of Philosophy"), `year` (default 2024).<br>Validation requires: `thesis.rdf`, `chapters/`, `references.bib`. |

All four list `production = true`, `maturity = 95`, and have no additional package dependencies.

## Choosing the right template

- Use **arxiv-paper-template** for rapid preprint distribution and arXiv submission workflows (metadata extraction + submission API).
- Use **ieee-paper-template** for two-column IEEE conferences that need structured author blocks, figures/tables from RDF, and citation validation.
- Use **neurips-paper-template** for NeurIPS-style submissions that require double-blind anonymity, page-limit checks, and algorithm-friendly environments.
- Use **phd-thesis-template** for long-form dissertations with chapter/appendix organization, university formatting controls, and modern LaTeX/biblatex setup.

## Notable differentiators (once wired)

- **Compliance focus**: NeurIPS enforces anonymity and page limits; IEEE emphasizes citation/formatting checks; arXiv focuses on submission readiness; PhD template focuses on institutional formatting.
- **Bibliography toolchain**: IEEE/NeurIPS/ArXiv use BibTeX; PhD template is biblatex/biber with cleveref + microtype for typography.
- **Artifacts**: Each package ships README, USAGE, templates, RDF schema (`*.ttl`), and examples; PhD adds a full `examples/sample-thesis/` directory.
- **Variables**: ArXiv/NeurIPS keep minimal required variables; IEEE adds discipline and keywords; PhD adds university/degree/year knobs for compliance.

## Typical CLI flows (from package examples) — these currently stub out rendering/validation

- ArXiv: `ggen paper new "<title>" --template arxiv` → `ggen paper generate paper.rdf --style arxiv --output paper.tex` → compile/submit.
- IEEE: `ggen paper new "<title>" --template ieee` → edit RDF → `ggen paper generate ... --style ieee` → `ggen paper compile`.
- NeurIPS: `ggen paper new "<title>" --template neurips` → `ggen paper validate paper.rdf --strict` → compile blind PDF.
- PhD: `ggen paper new "<title>" --template phd` → edit chapters and bibliography → `ggen paper compile thesis.tex --engine xelatex --bibtex`.

## Quick recommendations

- Need fastest preprint: pick **arxiv**.
- Conference (general IEEE-style): pick **ieee**.
- ML conference with double-blind: pick **neurips**.
- Dissertation/long-form: pick **phd-thesis**.






