# Quickstart: Generate a PhD Thesis with ggen

Generate a complete 50+ page PhD thesis PDF from RDF ontology using `ggen sync`.

## Prerequisites

- ggen CLI installed (`cargo install ggen`)
- pdflatex installed (TeX Live or MacTeX)
- bibtex installed (comes with TeX distributions)

## Quick Start (5 minutes)

### 1. Clone the thesis template

```bash
# Copy the thesis-gen example to your project
cp -r /path/to/ggen/examples/thesis-gen my-thesis
cd my-thesis
```

### 2. Edit your thesis content

Modify `ontology/thesis-content.ttl` with your thesis data:

```turtle
@prefix content: <https://ggen.io/thesis/content#> .
@prefix thesis: <https://ggen.io/ontology/thesis#> .

content:MainThesis a thesis:Thesis ;
    thesis:title "Your Thesis Title Here" ;
    thesis:subtitle "Your Subtitle Here" ;
    thesis:author "Your Name" ;
    thesis:institution "Your University" ;
    thesis:date "December 2025" ;
    thesis:abstract "Your 200-500 word abstract..." ;
    thesis:hasChapter content:Chapter1 .

content:Chapter1 a thesis:Chapter ;
    thesis:orderIndex 1 ;
    thesis:title "Introduction" ;
    thesis:labelId "ch:intro" ;
    thesis:hasSection content:Section1_1 .

# ... continue with your content
```

### 3. Generate LaTeX files

```bash
ggen sync
```

Output:
```
[ggen] Loading ontology from ontology/thesis-content.ttl
[ggen] Executing 15 generation rules...
[ggen] Generated: output/thesis.tex
[ggen] Generated: output/front-matter.tex
[ggen] Generated: output/chapters/chapter-1.tex
...
[ggen] Synced 25 files in 0.8s
```

### 4. Compile to PDF

```bash
cd output
pdflatex thesis.tex
bibtex thesis
pdflatex thesis.tex
pdflatex thesis.tex
```

(Or use the provided Makefile: `make pdf`)

### 5. View your thesis

Open `output/thesis.pdf` - a complete 50+ page thesis!

## Project Structure

```
my-thesis/
├── ggen.toml                  # Generation rules (don't modify)
├── ontology/
│   ├── thesis-schema.ttl      # Schema (don't modify)
│   └── thesis-content.ttl     # YOUR CONTENT (edit this!)
├── templates/                 # LaTeX templates (don't modify)
│   ├── thesis-main.tera
│   ├── front-matter.tera
│   ├── chapter.tera
│   └── ...
├── output/                    # Generated files
│   ├── thesis.tex
│   ├── references.bib
│   └── chapters/
└── figures/                   # Your images (add here)
```

## Key Concepts

### Zero Hardcoded Content

Templates contain ONLY LaTeX structure and Tera variables. All text comes from your ontology:

```tera
{# This template has NO hardcoded strings #}
{% for row in sparql_results %}
{{ row["?envBegin"] }}[{{ row["?theoremName"] }}]
{{ row["?statement"] }}
{{ row["?envEnd"] }}
{% if row["?proof"] %}
{{ row["?proofEnvBegin"] }}
{{ row["?proof"] }}
{{ row["?proofEnvEnd"] }}
{% endif %}
{% endfor %}
```

### Reusability

Change topics by editing `thesis-content.ttl` only:
- Same templates work for any thesis topic
- Different thesis = different ontology content

### Cross-References

Use `labelId` for all cross-references:

```turtle
content:Theorem1 a thesis:Theorem ;
    thesis:labelId "thm:main-result" ;
    thesis:statement "Our main theorem states..." .

content:Section2_3 a thesis:Section ;
    thesis:content "As proven in Theorem~\\ref{thm:main-result}, we see that..." .
```

### Citations

Define references and cite them:

```turtle
content:RefSmith2020 a thesis:Reference ;
    thesis:citeKey "smith2020" ;
    thesis:bibType "article" ;
    thesis:author "Smith, John" ;
    thesis:title "Important Paper" ;
    thesis:year "2020" ;
    thesis:journal "Journal of Things" .

content:Section1_1 a thesis:Section ;
    thesis:content "Prior work \\cite{smith2020} established..." ;
    thesis:cites content:RefSmith2020 .
```

## Content Requirements (for 50+ pages)

To achieve 50+ pages, include:

| Element | Minimum | Word Count |
|---------|---------|------------|
| Chapters | 7 | - |
| Sections | 30 (4-5 per chapter) | ~14,000 |
| Theorems | 10 | ~2,000 |
| Equations | 20 | ~1,000 |
| Figures | 10 | ~1,000 |
| Tables | 5 | ~500 |
| References | 30 | ~600 |

**Total**: ~19,000 words ≈ 55-60 pages

## Common Tasks

### Add a new chapter

```turtle
content:Chapter8 a thesis:Chapter ;
    thesis:orderIndex 8 ;
    thesis:title "Future Work" ;
    thesis:labelId "ch:future" ;
    thesis:abstract "This chapter discusses..." ;
    thesis:hasSection content:Section8_1 .
```

### Add a theorem with proof

```turtle
content:Theorem5 a thesis:Theorem ;
    thesis:orderIndex 5 ;
    thesis:theoremType "theorem" ;
    thesis:theoremName "Completeness" ;
    thesis:statement "For any valid input, the algorithm terminates." ;
    thesis:proof "We prove by induction on the input size..." ;
    thesis:labelId "thm:completeness" .
```

### Add a figure

```turtle
content:Figure3 a thesis:Figure ;
    thesis:orderIndex 3 ;
    thesis:caption "System architecture overview" ;
    thesis:imagePath "figures/architecture.pdf" ;
    thesis:width "0.9\\textwidth" ;
    thesis:position "htbp" ;
    thesis:labelId "fig:arch" .
```

### Add a table

```turtle
content:Table2 a thesis:Table ;
    thesis:orderIndex 2 ;
    thesis:caption "Experimental results comparison" ;
    thesis:labelId "tab:results" ;
    thesis:hasRow content:Table2Row1, content:Table2Row2 .

# Headers defined via thesis:header properties
content:Table2 thesis:header "Method" ; thesis:headerIndex 1 .
content:Table2 thesis:header "Accuracy" ; thesis:headerIndex 2 .
content:Table2 thesis:header "Time (s)" ; thesis:headerIndex 3 .

content:Table2Row1 a thesis:TableRow ;
    thesis:rowIndex 1 ;
    thesis:cell "Baseline" ; thesis:cellIndex 1 .
    thesis:cell "85.2\\%" ; thesis:cellIndex 2 .
    thesis:cell "12.3" ; thesis:cellIndex 3 .
```

## Troubleshooting

### "Undefined reference" in PDF

Run pdflatex twice after bibtex:
```bash
pdflatex thesis && bibtex thesis && pdflatex thesis && pdflatex thesis
```

### Missing figure

Ensure image path in ontology matches actual file location relative to `output/`.

### LaTeX special characters

Pre-escape these in ontology content: `& % $ # _ { } ~ ^`
- `&` → `\&`
- `%` → `\%`
- `$` → `\$`
- `#` → `\#`
- `_` → `\_`

### ggen sync errors

Check that your ontology validates:
```bash
ggen check ontology/thesis-content.ttl
```

## Next Steps

1. Read the full data model: `specs/010-thesis-gen-system/data-model.md`
2. See example thesis content: `examples/thesis-gen/ontology/thesis-content.ttl`
3. Customize LaTeX styling by editing `templates/thesis-main.tera` (advanced)

---

**Remember**: Edit `ontology/thesis-content.ttl`, run `ggen sync`, compile with `make pdf`. That's it!
