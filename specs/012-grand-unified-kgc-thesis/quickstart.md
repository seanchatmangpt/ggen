# Quickstart: Grand Unified KGC Thesis Generation

**Feature Branch**: `012-grand-unified-kgc-thesis`
**Estimated Time**: 30 minutes to first PDF

## Prerequisites

- mcpp v5 CLI installed (`cargo install mcpp`)
- LaTeX distribution with pdflatex and biber
- Required packages: memoir, amsmath, amsthm, biblatex, algorithm2e, hyperref, cleveref

## Quick Start

### 1. Navigate to Feature Directory

```bash
cd ~/.ggen/mcpp/specs/012-grand-unified-kgc-thesis
```

### 2. Generate LaTeX from Ontology

```bash
mcpp sync --manifest mcpp.toml
```

**Expected Output**:
```
[INFO] Loading ontology from ontology/kgc-unified-content.ttl
[INFO] Executing 15 generation rules
[INFO] Generated: output/thesis.tex
[INFO] Generated: output/front-matter.tex
[INFO] Generated: output/chapters/all-chapters.tex
[INFO] Generated: output/theorems.tex
[INFO] Generated: output/equations.tex
[INFO] Generated: output/algorithms.tex
[INFO] Generated: output/figures.tex
[INFO] Generated: output/tables.tex
[INFO] Generated: output/references.bib
[INFO] Generated: output/appendices.tex
[INFO] Generated: output/code-listings.tex
[INFO] Generated: output/preamble.tex
[INFO] Generation complete: 15 files in 4.2s
```

### 3. Compile to PDF

```bash
cd output
pdflatex thesis.tex
biber thesis
pdflatex thesis.tex
pdflatex thesis.tex
```

**Expected Output**:
```
Output written on thesis.pdf (150 pages, 2.4MB)
```

### 4. Verify Output

Check that the PDF contains:
- [ ] Title page with author and institution
- [ ] Abstract (500-1000 words)
- [ ] Table of Contents
- [ ] 7 chapters with sections
- [ ] 10+ theorems with proofs
- [ ] 20+ numbered equations
- [ ] 5+ algorithms
- [ ] 30+ bibliography entries
- [ ] Appendices with code listings

---

## File Structure

```
specs/012-grand-unified-kgc-thesis/
├── mcpp.toml                    # Generation manifest
├── ontology/
│   ├── thesis-schema.ttl        # Class/property definitions
│   └── kgc-unified-content.ttl  # Thesis content
├── templates/
│   ├── thesis-main.tera
│   ├── front-matter.tera
│   ├── chapter.tera
│   ├── theorem.tera
│   ├── equation.tera
│   ├── algorithm.tera
│   ├── figure.tera
│   ├── table.tera
│   ├── bibliography.tera
│   ├── appendix.tera
│   ├── code-listing.tera
│   ├── subsection.tera
│   ├── preamble.tera
│   └── chapter-index.tera
└── output/                      # Generated LaTeX files
    ├── thesis.tex               # Main document
    ├── front-matter.tex
    ├── chapters/
    │   └── all-chapters.tex
    ├── theorems.tex
    ├── equations.tex
    ├── algorithms.tex
    ├── figures.tex
    ├── tables.tex
    ├── references.bib
    ├── appendices.tex
    ├── code-listings.tex
    └── preamble.tex
```

---

## Common Tasks

### Modify Thesis Content

1. Edit `ontology/kgc-unified-content.ttl`
2. Run `mcpp sync --manifest mcpp.toml`
3. Recompile PDF

### Add a New Theorem

```turtle
thesis:thm-new-theorem a thesis:Theorem ;
    thesis:orderIndex 15 ;
    thesis:theoremType "theorem" ;
    thesis:theoremName "New Theorem Name" ;
    thesis:statement "For all $x \\in X$, we have $f(x) = g(x)$." ;
    thesis:proof "The proof follows from Lemma \\ref{lem:supporting}." ;
    thesis:labelId "thm:new-theorem" .

# Link to parent section
thesis:sec-target thesis:hasTheorem thesis:thm-new-theorem .
```

### Add a New Equation

```turtle
thesis:eq-new-equation a thesis:Equation ;
    thesis:orderIndex 25 ;
    thesis:latex "E = mc^2" ;
    thesis:description "Mass-energy equivalence" ;
    thesis:labelId "eq:mass-energy" .

# Link to parent section
thesis:sec-target thesis:hasEquation thesis:eq-new-equation .
```

### Add a Bibliography Reference

```turtle
thesis:ref-newauthor2025 a thesis:Reference ;
    thesis:citeKey "newauthor2025" ;
    thesis:bibType "article" ;
    thesis:author "New Author" ;
    thesis:title "Groundbreaking Paper Title" ;
    thesis:year 2025 ;
    thesis:journal "Journal of Important Things" ;
    thesis:volume "42" ;
    thesis:pages "1--100" ;
    thesis:doi "10.1234/example.2025" .
```

---

## Verification Commands

### Check Ontology Validity

```bash
mcpp validate --ontology ontology/kgc-unified-content.ttl
```

### Preview SPARQL Query Results

```bash
mcpp query --ontology ontology/kgc-unified-content.ttl --query "
  SELECT ?title ?orderIndex WHERE {
    ?chapter a thesis:Chapter ;
             thesis:title ?title ;
             thesis:orderIndex ?orderIndex .
  } ORDER BY ?orderIndex
"
```

### Check LaTeX Compilation Warnings

```bash
cd output
pdflatex thesis.tex 2>&1 | grep -E "Warning|Error|Undefined"
```

### Verify Cross-References

```bash
grep -c "\\\\ref{" output/chapters/all-chapters.tex
grep -c "\\\\label{" output/*.tex
```

---

## Troubleshooting

### "Undefined reference" warnings

**Cause**: Label not defined in ontology
**Fix**: Add `thesis:labelId` property to referenced entity

### "Citation undefined" warnings

**Cause**: citeKey not in references.bib
**Fix**: Add Reference entity with matching citeKey

### Generation takes >10 seconds

**Cause**: Ontology too large or inefficient SPARQL
**Fix**: Run `mcpp benchmark` to identify slow queries

### Biber errors

**Cause**: Invalid BibTeX entry
**Fix**: Check Reference entities have required fields for their bibType

---

## Success Criteria Checklist

- [ ] `mcpp sync` completes in <10 seconds
- [ ] PDF compiles without errors on first attempt
- [ ] All cross-references resolve (no "??")
- [ ] All citations resolve (no "undefined")
- [ ] Thesis contains 100+ pages
- [ ] All 10+ theorems have proofs
- [ ] All 20+ equations are numbered
- [ ] Bibliography has 30+ entries
- [ ] Regeneration produces byte-identical output
