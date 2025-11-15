# Academic Paper Lifecycle - Quick Start Guide

Get up and running with the academic paper lifecycle in 5 minutes.

## 1. Create Your First Paper (1 minute)

```bash
# Create a new arXiv preprint
ggen paper new "My Research Paper" --template arxiv

# Navigate to your paper directory
cd my-research-paper

# See what was created
ls -la
```

**Output**:
```
my-research-paper.rdf         # Paper metadata (RDF ontology)
references.bib                # Bibliography (BibTeX)
sections/                     # Directory for content
README.md                     # Getting started guide
```

## 2. Edit Paper Metadata (1 minute)

Edit the paper RDF ontology:

```bash
# Open the paper file
cat my-research-paper.rdf
```

Update with your details:

```xml
ap:MyPaper a ap:Paper ;
    ap:title "My Research Paper" ;
    ap:abstract "This paper investigates..." ;
    ap:keywords "research, code generation, AI" ;
    ap:hasAuthor ap:Author1 .

ap:Author1 a ap:Author ;
    ap:authorName "Your Name" ;
    ap:authorOrder 1 ;
    ap:email "your@email.com" ;
    ap:authorAffiliation "Your University" .
```

Or use the editor:

```bash
ggen paper edit my-research-paper.rdf
```

## 3. Add Content (2 minutes)

Create content sections:

```bash
# Create introduction
mkdir -p sections
echo "# Introduction

Your introduction text here..." > sections/01-introduction.md

# Add more sections
echo "# Methods

Description of your methodology..." > sections/02-methods.md

echo "# Results

Your findings..." > sections/03-results.md
```

Update RDF to reference sections:

```bash
ggen paper add-section my-research-paper.rdf \
  --title "Introduction" \
  --file sections/01-introduction.md

ggen paper add-section my-research-paper.rdf \
  --title "Methods" \
  --file sections/02-methods.md

ggen paper add-section my-research-paper.rdf \
  --title "Results" \
  --file sections/03-results.md
```

## 4. Generate LaTeX & PDF (1 minute)

Generate LaTeX from RDF and compile to PDF:

```bash
# Generate LaTeX
ggen paper generate my-research-paper.rdf --style arxiv

# Compile to PDF
ggen paper compile my-research-paper.tex

# View the PDF
open my-research-paper.pdf
```

**Done!** You now have a professional PDF of your research paper.

---

## Next Steps

### Add Bibliography

```bash
# Initialize bibliography
ggen paper init-bibliography my-research-paper.rdf

# Add references
ggen bibliography add \
  --key "smith2020" \
  --authors "John Smith" \
  --title "My Great Paper" \
  --journal "Nature" \
  --year 2020

# Recompile with bibliography
ggen paper compile my-research-paper.tex --bibtex
```

### Change Format

```bash
# Generate IEEE format instead
ggen paper generate my-research-paper.rdf --style ieee

# Generate NeurIPS format
ggen paper generate my-research-paper.rdf --style neurips

# Generate as thesis
ggen paper generate my-research-paper.rdf --style phd
```

### Submit to arXiv

```bash
# Prepare for arXiv submission
ggen paper submit my-research-paper.pdf --venue arxiv

# Check status
ggen paper track my-research-paper.rdf
```

### Collaborate with Others

```bash
# Enable multi-author collaboration
ggen paper add-author my-research-paper.rdf \
  --name "Jane Collaborator" \
  --email "jane@university.edu"

# Share with collaborators
ggen paper share my-research-paper.rdf --link
```

---

## Available Commands

| Command | Purpose |
|---------|---------|
| `ggen paper new` | Create new paper |
| `ggen paper edit` | Edit paper metadata |
| `ggen paper generate` | Generate LaTeX from RDF |
| `ggen paper compile` | Compile LaTeX to PDF |
| `ggen paper validate` | Validate paper structure |
| `ggen paper submit` | Submit to venue (arXiv, conference, journal) |
| `ggen paper track` | Track submission status |
| `ggen bibliography add` | Add bibliography entry |
| `ggen bibliography import` | Import BibTeX file |
| `ggen bibliography export` | Export to different formats |

---

## Get Help

```bash
# See all paper commands
ggen paper --help

# Get help on specific command
ggen paper generate --help

# View documentation
ggen paper new --help
```

---

## Marketplace Templates

Discover more paper templates from the marketplace:

```bash
# List available templates
ggen marketplace search "academic paper"

# Install IEEE template
ggen marketplace install ieee-paper-template

# Install peer review workflow
ggen marketplace install academic-peer-review-workflow

# Install bibliography manager
ggen marketplace install academic-bibliography-manager
```

---

## Tips & Tricks

### Use RDF Queries

Query your paper using SPARQL:

```bash
# Find all authors
ggen graph query "SELECT ?name WHERE { ?paper ap:hasAuthor ?author . ?author ap:authorName ?name }"

# Find papers by discipline
ggen graph query "SELECT ?title WHERE { ?paper ap:discipline ap:ComputerScience . ?paper ap:title ?title }"
```

### Version Control

Keep track of changes with Git:

```bash
# Initialize Git
git init

# Commit your paper
git add my-research-paper.rdf sections/ references.bib
git commit -m "Initial paper draft"

# Compare versions
git diff my-research-paper.rdf
```

### Online Compilation

Compile to PDF online without LaTeX installed:

```bash
ggen paper compile my-research-paper.tex --online
```

---

## Example: Complete Workflow

Here's a complete workflow from start to finish:

```bash
# 1. Create paper
ggen paper new "Deep Learning for Code" --template arxiv

cd deep-learning-for-code

# 2. Edit metadata
ggen paper edit deep-learning-for-code.rdf

# 3. Add content
mkdir sections
echo "# Introduction\nYour text here..." > sections/01-intro.md
ggen paper add-section deep-learning-for-code.rdf --title "Introduction" --file sections/01-intro.md

# 4. Add references
ggen paper init-bibliography deep-learning-for-code.rdf
ggen bibliography add --key "lecun2015" --authors "LeCun, et al." --title "Deep Learning" --journal "Nature" --year 2015

# 5. Generate LaTeX
ggen paper generate deep-learning-for-code.rdf --style arxiv

# 6. Compile PDF
ggen paper compile deep-learning-for-code.tex --bibtex

# 7. Submit to arXiv
ggen paper submit deep-learning-for-code.pdf --venue arxiv --category cs.AI

# 8. Track submission
ggen paper track deep-learning-for-code.rdf
```

Done! You've successfully created, compiled, and submitted an academic paper using ggen.

---

**For more details, see [ACADEMIC_PAPER_LIFECYCLE.md](./ACADEMIC_PAPER_LIFECYCLE.md)**
