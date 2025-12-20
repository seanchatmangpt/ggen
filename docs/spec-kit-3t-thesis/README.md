# Spec-Kit-3T PhD Thesis: A Diataxis-Structured Framework for RDF-First Software Specification

**Meta-Circular Property**: This thesis about spec-kit-3t is ITSELF specified using spec-kit-3t (3T methodology: TOML + Tera + Turtle).

## Overview

This directory contains a complete PhD thesis specification using the **3T methodology** (TOML + Tera + Turtle) and structured according to the **Diataxis framework** (Tutorial, How-to, Reference, Explanation).

**Constitutional Equation**:
```
thesis.tex = μ(ontology.ttl)
```

Where:
- `ontology.ttl` = RDF triples encoding thesis content (semantic substrate)
- `μ` = Deterministic transformation pipeline (μ₁→μ₂→μ₃→μ₄→μ₅)
- `thesis.tex` = Generated LaTeX document (memoir class)
- `μ∘μ = μ` = Idempotence (running twice produces zero changes)

## Directory Structure (3T Architecture)

```
docs/spec-kit-3t-thesis/
├── README.md                           # ← You are here
├── ggen.toml                           # TOML: Configuration for thesis generation
├── ontology/                           # TURTLE: Semantic substrate (source of truth)
│   ├── thesis-schema.ttl               # RDF schema for PhD theses
│   └── spec-kit-3t-content.ttl         # Actual thesis content (430+ triples)
├── templates/                          # TERA: LaTeX rendering templates
│   ├── thesis-main.tera                # Main document structure (memoir)
│   ├── frontmatter.tera                # Abstract, dedication, acknowledgments
│   ├── chapter.tera                    # Standard chapter template
│   ├── diataxis-tutorial.tera          # Tutorial chapter (green quadrant)
│   ├── diataxis-howto.tera             # How-to chapter (blue quadrant)
│   ├── diataxis-reference.tera         # Reference chapter (purple quadrant)
│   └── diataxis-explanation.tera       # Explanation chapter (orange quadrant)
├── generated/                          # Generated artifacts (gitignored)
│   ├── thesis-main.tex                 # Main LaTeX file
│   ├── frontmatter.tex                 # Front matter
│   ├── chapter-01.tex                  # Chapter 1: Introduction
│   ├── chapter-02.tex                  # Chapter 2: Literature Review
│   ├── chapter-03.tex                  # Chapter 3: Tutorial (Diataxis)
│   ├── chapter-04.tex                  # Chapter 4: How-to (Diataxis)
│   ├── chapter-05.tex                  # Chapter 5: Reference (Diataxis)
│   ├── chapter-06.tex                  # Chapter 6: Explanation (Diataxis)
│   ├── bibliography.bib                # BibTeX bibliography
│   └── thesis-main.pdf                 # Final PDF output
└── .gitignore                          # Ignores generated/ directory
```

## The 3T Methodology

**3T** = **TOML + Tera + Turtle**

1. **TOML** (`ggen.toml`): Configuration-as-code
   - SPARQL queries for extraction (μ₂)
   - Template mappings
   - LaTeX compiler settings
   - Output configuration

2. **Tera** (`templates/*.tera`): Templates for LaTeX rendering (μ₃)
   - Transform SPARQL bindings into LaTeX
   - Memoir document class structure
   - Diataxis quadrant styling
   - Bibliography integration

3. **Turtle** (`ontology/*.ttl`): Semantic substrate
   - RDF triples encode all thesis content
   - SHACL shapes enforce academic structure
   - SPARQL-queryable, machine-readable
   - Single source of truth

## The Diataxis Framework

This thesis demonstrates the **Diataxis documentation framework** by structuring Chapters 3-6 along two orthogonal axes:

### Diataxis 2×2 Matrix

```
                    PRACTICAL           THEORETICAL
                    ─────────           ───────────
LEARNING-ORIENTED │ Tutorial (Ch. 3)   Explanation (Ch. 6) │
                  │ [GREEN]            [ORANGE]            │
                  ├─────────────────────────────────────────┤
USING-ORIENTED    │ How-to (Ch. 4)     Reference (Ch. 5)   │
                  │ [BLUE]             [PURPLE]            │
                    ─────────           ───────────
```

**Chapter 3: Tutorial** (Green - Learning + Practical)
- Learning objective: Create your first specification
- Step-by-step walkthrough with photo album example
- Hands-on: Setup → Ontology → Queries → Templates → Generation

**Chapter 4: How-to Guides** (Blue - Using + Practical)
- Task-oriented solutions to common problems
- How to add user stories, validate SHACL, generate multiple outputs
- Problem → Solution → Example pattern

**Chapter 5: Reference** (Purple - Using + Theoretical)
- Complete API documentation
- RDF schema reference, SPARQL patterns, Tera API, TOML schema
- Dry, precise, technical

**Chapter 6: Explanation** (Orange - Learning + Theoretical)
- Why 3T works: separation of concerns, constitutional equation
- Conceptual understanding of knowledge graphs
- Theory and principles

## How to Generate the Thesis

### Prerequisites

- **ggen v6** CLI (with RDF/SPARQL/Tera support)
- **LaTeX distribution** (TeX Live, MiKTeX, or MacTeX)
- **pdflatex** and **biber** compilers

**Note**: ggen v6 is currently being specified (this thesis is part of that specification). Once implemented, the commands below will work.

### Generation Workflow

```bash
cd /Users/sac/ggen/docs/spec-kit-3t-thesis

# Option 1: Generate all LaTeX files
ggen sync

# This executes the five-stage pipeline:
# μ₁: Normalization (SHACL validation of ontology)
# μ₂: Extraction (SPARQL queries)
# μ₃: Emission (Tera template rendering)
# μ₄: Canonicalization (format normalization)
# μ₅: Receipt (cryptographic provenance)

# Option 2: Compile to PDF (after ggen sync)
cd generated
pdflatex thesis-main.tex
biber thesis-main
pdflatex thesis-main.tex
pdflatex thesis-main.tex  # Third pass for final references

# Option 3: One-command build (when implemented)
ggen sync --compile-pdf
```

### What Happens During `ggen sync`

The five-stage pipeline executes:

#### 1. μ₁ - Normalization (SHACL Validation)
- Validates `ontology/spec-kit-3t-content.ttl` against `thesis-schema.ttl`
- Ensures required thesis components exist (abstract, chapters, etc.)
- Validates Diataxis structure constraints

#### 2. μ₂ - Extraction (SPARQL SELECT)
- Executes 10+ SPARQL queries (defined in `ggen.toml`)
- Extracts thesis metadata, chapter content, sections
- Separate queries for each Diataxis quadrant

#### 3. μ₃ - Emission (Tera Template Rendering)
- Renders LaTeX using Tera templates
- `thesis-main.tera` → `thesis-main.tex` (main document)
- Diataxis templates → chapter .tex files (colored boxes per quadrant)
- Bibliography template → `bibliography.bib`

#### 4. μ₄ - Canonicalization (Deterministic Formatting)
- Normalize line endings (LF)
- Trim trailing whitespace
- Ensure consistent indentation
- Guarantees bit-for-bit reproducibility

#### 5. μ₅ - Receipt Generation (Cryptographic Provenance)
- SHA-256 hashes of input ontology files
- SHA-256 hashes of output LaTeX files
- Timestamp and pipeline configuration hash
- Proves: `hash(thesis.tex) = hash(μ(ontology))`

## Modifying the Thesis

**CRITICAL**: To modify the thesis, edit the ONTOLOGY, not the generated LaTeX.

### Example: Add a New Section to Chapter 3 (Tutorial)

1. Edit `ontology/spec-kit-3t-content.ttl`:
   ```turtle
   :ch03-sec06-advanced
       a thesis:Section ;
       thesis:sectionTitle "Step 6: Advanced SPARQL Patterns" ;
       thesis:hasContent """Learn how to write complex SPARQL queries
       for extracting nested relationships from your ontology...""" .

   # Link to chapter
   :ch03-tutorial thesis:hasSection :ch03-sec06-advanced .
   ```

2. Regenerate:
   ```bash
   ggen sync
   ```

3. Verify in `generated/chapter-03.tex`

### Example: Update Thesis Metadata

1. Edit `ontology/spec-kit-3t-content.ttl`:
   ```turtle
   :spec-kit-3t-thesis
       thesis:hasTitle "NEW TITLE HERE" ;
       thesis:hasYear "2026"^^xsd:gYear .
   ```

2. Regenerate and rebuild PDF:
   ```bash
   ggen sync --compile-pdf
   ```

## Diataxis Color-Coded Chapters

The generated LaTeX uses custom colors for each Diataxis quadrant:

```latex
\definecolor{TutorialColor}{RGB}{46, 204, 113}      % Green
\definecolor{HowToColor}{RGB}{52, 152, 219}         % Blue
\definecolor{ReferenceColor}{RGB}{155, 89, 182}     % Purple
\definecolor{ExplanationColor}{RGB}{230, 126, 34}   % Orange
```

Each Diataxis chapter (3-6) includes:
- Colored header box identifying the quadrant
- Quadrant-specific framing (learning objective, task, technical detail, concept)
- Colored completion box linking to complementary quadrants

**Example from Tutorial chapter**:
```latex
\begin{center}
\colorbox{TutorialColor!20}{
    \textbf{Diataxis Framework: TUTORIAL}\\
    \textit{Learning-oriented | Practical steps | Taking first steps}
}
\end{center}
```

## Constitutional Invariants

This thesis specification enforces the constitutional laws:

1. **Idempotence** (μ∘μ = μ): Running `ggen sync` twice produces zero file changes
2. **Determinism**: Same ontology generates bit-for-bit identical LaTeX across platforms
3. **Provenance**: Receipt cryptographically proves thesis.tex derived from ontology
4. **No-Edit Law**: Generated LaTeX in `generated/` is never hand-edited
5. **Substrate Primacy**: Only ontology (.ttl files) is version-controlled as source of truth

## Version Control

**What to Commit**:
- ✅ `ggen.toml` - Configuration
- ✅ `ontology/*.ttl` - Source of truth
- ✅ `templates/*.tera` - LaTeX templates
- ✅ `README.md` - This file
- ✅ `.gitignore` - Ignore patterns

**What NOT to Commit** (in `.gitignore`):
- ❌ `generated/*.tex` - Generated from ontology
- ❌ `generated/*.pdf` - Compiled output
- ❌ `generated/*.aux`, `*.log`, `*.bbl`, etc. - LaTeX auxiliary files
- ❌ `.receipt.json` - Cryptographic receipt

## RDF Triple Count

Current thesis ontology contains:
- **Schema**: 100+ triples (thesis-schema.ttl)
- **Content**: 430+ triples (spec-kit-3t-content.ttl)
- **Total**: 530+ triples encoding complete thesis

## Bibliography Management

Citations are managed in RDF:

```turtle
# Cite a work
:ch02-sec01-literate-programming
    thesis:cites :cite-knuth-literate .

# Citation metadata
:cite-knuth-literate
    a thesis:Citation ;
    thesis:citationKey "knuth1984literate" .
```

BibTeX entries are in separate `.bib` file (not shown in this example but would be generated from RDF metadata if bibliographic details were included in the ontology).

## Validation

Run SHACL validation before generation:

```bash
# Validate ontology structure
ggen validate ontology/spec-kit-3t-content.ttl

# Check for:
# - Missing required thesis components
# - Invalid chapter numbering
# - Orphaned sections
# - Diataxis structure violations
```

## Meta-Circular Property

This thesis demonstrates **self-application**:

1. The thesis DESCRIBES spec-kit-3t methodology
2. The thesis is SPECIFIED using spec-kit-3t methodology
3. The thesis VALIDATES spec-kit-3t by being its own proof

This is analogous to:
- A compiler written in the language it compiles (self-hosting)
- A LaTeX document about LaTeX typesetting
- An RDF ontology describing RDF ontologies

## Future Work

Once ggen v6 is implemented, this thesis specification will:

1. Generate a complete PhD thesis PDF
2. Demonstrate all five pipeline stages (μ₁ through μ₅)
3. Prove idempotence: `ggen sync && ggen sync` produces zero changes
4. Validate cryptographic provenance via receipts
5. Serve as reference implementation for academic document generation

## References

- **Diataxis Framework**: https://diataxis.fr/
- **RDF Primer**: https://www.w3.org/TR/rdf11-primer/
- **SPARQL 1.1 Specification**: https://www.w3.org/TR/sparql11-query/
- **SHACL Specification**: https://www.w3.org/TR/shacl/
- **Tera Template Engine**: https://tera.netlify.app/
- **LaTeX Memoir Class**: https://ctan.org/pkg/memoir
- **ggen Project**: https://github.com/seanchatmangpt/ggen

## License

This thesis specification is part of the ggen project. See project LICENSE.

---

**Generated**: 2025-12-20
**Project**: Spec-Kit-3T PhD Thesis
**Methodology**: 3T (TOML + Tera + Turtle)
**Framework**: Diataxis (Tutorial + How-to + Reference + Explanation)
**Constitutional Equation**: `thesis.tex = μ(ontology.ttl)`
**Status**: ✅ Specification complete, ready for ggen v6 implementation

## CLI Usage

The thesis generator now includes a comprehensive Typer CLI:

```bash
# Install
pip install -e .

# Generate thesis
spec-kit-3t generate --verbose

# Validate RDF
spec-kit-3t validate spec-kit-3t-content.ttl

# Extract and clean PDF text
spec-kit-3t extract generated/thesis-main.pdf --analyze
spec-kit-3t clean extracted.txt --output cleaned.txt

# Compile PDF
spec-kit-3t pdf
```

See [CLI_USAGE.md](docs/CLI_USAGE.md) for complete documentation.

## Test Suite

```bash
$ python -m pytest tests/cli/ -v
======================== 21 passed, 2 skipped =========================
Required test coverage of 55% reached. Total coverage: 56.32%
```

**Test coverage:**
- Unit tests: 18 tests (commands, validation, help)
- Integration tests: 2 tests (end-to-end workflows)
- Performance tests: 1 test (validation speed)
- Security tests: 2 tests (path traversal, command injection)
