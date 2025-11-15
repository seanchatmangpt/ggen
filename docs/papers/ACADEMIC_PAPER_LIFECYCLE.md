# Academic Paper Lifecycle with LaTeX and ggen Marketplace

Complete guide for the full academic paper lifecycle using ggen, including paper creation, LaTeX generation, peer review management, and publishing workflows.

## Table of Contents

- [Overview](#overview)
- [Architecture](#architecture)
- [Getting Started](#getting-started)
- [Paper Creation](#paper-creation)
- [LaTeX Generation](#latex-generation)
- [Bibliography Management](#bibliography-management)
- [Peer Review Workflow](#peer-review-workflow)
- [Publishing and Submission](#publishing-and-submission)
- [Marketplace Templates](#marketplace-templates)
- [Advanced Features](#advanced-features)
- [Troubleshooting](#troubleshooting)

---

## Overview

The ggen Academic Paper Lifecycle provides an integrated, RDF-driven system for managing the complete lifecycle of academic papers from initial conception through publication. Key features include:

- **RDF-Based Metadata**: Papers are stored as RDF ontologies, enabling semantic queries and automated processing
- **Multi-Format LaTeX Generation**: Generate papers in IEEE, ACM, NeurIPS, arXiv, thesis, and custom formats
- **Integrated Bibliography**: BibTeX-compatible bibliography management with citation validation
- **Peer Review Tracking**: Coordinate multi-reviewer processes with automated feedback compilation
- **Publishing Workflows**: Direct integration with arXiv, CrossRef DOI assignment, and journal submission systems
- **Collaboration Features**: Multi-author support with version control, roles, and contribution tracking
- **Marketplace Integration**: Share paper templates and workflows via the ggen marketplace

### Core Components

```
Paper RDF Ontology
    ↓
Content Sections (Markdown/LaTeX)
    ↓
Bibliography Entries (BibTeX/RDF)
    ↓
LaTeX Template + SPARQL Queries
    ↓
Generated LaTeX Document
    ↓
PDF Compilation (pdflatex/xelatex)
    ↓
Publishing (arXiv, DOI, Journals)
```

---

## Architecture

### Data Model: RDF Ontology

Papers are represented using the Academic Paper Ontology (v1.0.0):

```ttl
# Paper with metadata
ap:MyPaper a ap:Paper ;
    ap:title "Deep Learning for Code Generation" ;
    ap:abstract "This paper presents..." ;
    ap:keywords "deep learning, code generation, neural networks" ;
    ap:status "draft" ;
    ap:hasAuthor ap:Author1, ap:Author2 ;
    ap:hasSection ap:Introduction, ap:Methods, ap:Results ;
    ap:hasBibliography ap:Ref1, ap:Ref2, ap:Ref3 .

# Authors with affiliations
ap:Author1 a ap:Author ;
    ap:authorName "Jane Researcher" ;
    ap:authorOrder 1 ;
    ap:email "jane@university.edu" ;
    ap:orcid "0000-0001-2345-6789" ;
    ap:hasAffiliation ap:MIT ;
    ap:correspondingAuthor true .

# Sections
ap:Introduction a ap:Section ;
    ap:sectionType "introduction" ;
    ap:sectionTitle "Introduction" ;
    ap:sectionOrder 1 ;
    ap:sectionContent "Markdown or LaTeX content here..." .

# Bibliography entries
ap:Ref1 a ap:BibliographyEntry ;
    ap:citeKey "lecun2015deep" ;
    ap:authors "Yann LeCun, Yoshua Bengio, Geoffrey Hinton" ;
    ap:title "Deep Learning" ;
    ap:journal "Nature" ;
    ap:year 2015 ;
    ap:volume "521" ;
    ap:pages "436-444" ;
    ap:doi "10.1038/nature14539" .
```

### Template System

LaTeX generation uses Tera templates with RDF/SPARQL integration:

```tera
\documentclass{article}
\title{ {{- paper.title -}} }
\author{
{% for author in paper.hasAuthor | sort_by("authorOrder") %}
  {{- author.authorName -}}{% if not loop.last %}, {% endif %}
{% endfor %}
}
\begin{document}
\maketitle

{% for section in paper.hasSection | sort_by("sectionOrder") %}
\section{ {{- section.sectionTitle -}} }
{{- section.sectionContent }}
{% endfor %}

\bibliographystyle{plain}
\bibliography{references}
\end{document}
```

---

## Getting Started

### Installation

1. **Install ggen with paper support**:

```bash
# Ensure ggen is installed and updated
ggen --version

# Verify paper commands are available
ggen paper --help
```

2. **Install academic paper templates from marketplace**:

```bash
# List available paper templates
ggen marketplace search "academic paper"

# Install IEEE template
ggen marketplace install ieee-paper-template

# Install arXiv template
ggen marketplace install arxiv-paper-template

# Install peer review workflow
ggen marketplace install academic-peer-review-workflow

# Install bibliography manager
ggen marketplace install academic-bibliography-manager
```

### Quick Start: Create Your First Paper

```bash
# Create a new IEEE conference paper
ggen paper new "My Research Paper" --template ieee --discipline computer-science

# Navigate to paper directory
cd my-research-paper

# List created files
ls -la
# Output:
# my-research-paper.rdf       (RDF ontology with paper metadata)
# references.bib              (BibTeX bibliography)
# sections/                   (Directory for content sections)
# templates/                  (LaTeX templates)
```

---

## Paper Creation

### 1. Create New Paper

Create a new paper with a specified template and academic discipline:

```bash
ggen paper new "Semantic Code Projections" \
  --template arxiv \
  --discipline computer-science \
  --output ./papers
```

**Available templates**:
- `ieee` - IEEE conference paper (two-column)
- `acm` - ACM journal paper
- `neurips` - NeurIPS conference (double-blind)
- `arxiv` - arXiv preprint
- `phd` - PhD thesis (multi-chapter)
- `masters` - Master's thesis
- `nature` - Nature journal
- `pnas` - PNAS journal
- `icml` - ICML conference
- `iclr` - ICLR conference
- `cvpr` - CVPR conference

**Disciplines**:
- `computer-science` (CS)
- `mathematics` (Math)
- `physics` (Physics)
- `biology` (Biology)
- `medicine` (Medicine)
- `economics` (Economics)
- `psychology` (Psychology)
- `engineering` (Engineering)

### 2. Edit Paper Metadata

Edit the paper's RDF ontology to add metadata:

```bash
ggen paper edit my-paper.rdf --section abstract

# Edit in the default editor (vim/nano)
# Update the abstract, keywords, authors, etc.
```

### 3. Paper Structure

Papers are organized as follows:

```
my-paper/
├── my-paper.rdf                    # Paper metadata (RDF)
├── references.bib                  # Bibliography (BibTeX)
├── sections/
│   ├── 01-introduction.md          # Introduction
│   ├── 02-related-work.md          # Related work
│   ├── 03-methods.md               # Methods/Methodology
│   ├── 04-results.md               # Results/Experiments
│   ├── 05-discussion.md            # Discussion
│   └── 06-conclusion.md            # Conclusion
├── figures/
│   ├── figure-1.png                # Figures and diagrams
│   └── figure-2.pdf
├── tables/
│   └── results-table.csv           # Tables
└── appendices/
    └── appendix-a.md               # Supplementary material
```

### 4. Add Sections

Add sections to your paper:

```bash
# Create introduction section
echo "# Introduction

## Background

Your introduction text here..." > sections/01-introduction.md

# Add to RDF ontology
ggen paper edit my-paper.rdf --add-section "Introduction" --file sections/01-introduction.md

# Verify sections
ggen paper validate my-paper.rdf --check structure
```

### 5. Add Figures and Tables

Include figures and tables in your paper:

```bash
# Add figure
ggen paper add-figure my-paper.rdf \
  --name "architecture-diagram" \
  --caption "System architecture overview" \
  --file figures/architecture.png \
  --width "0.7\\textwidth"

# Add table
ggen paper add-table my-paper.rdf \
  --name "results-comparison" \
  --caption "Performance comparison with baselines" \
  --file tables/results.csv
```

---

## LaTeX Generation

### 1. Generate LaTeX from RDF

Convert your paper RDF ontology to LaTeX:

```bash
# Generate IEEE format
ggen paper generate my-paper.rdf --style ieee --output my-paper.tex

# Generate NeurIPS format
ggen paper generate my-paper.rdf --style neurips --output my-paper.tex

# Generate arXiv preprint
ggen paper generate my-paper.rdf --style arxiv --output my-paper.tex

# Generate PhD thesis
ggen paper generate my-paper.rdf --style phd --output thesis.tex

# Generate with custom template
ggen paper generate my-paper.rdf --template custom-template.tmpl --output my-paper.tex
```

### 2. Compile LaTeX to PDF

Compile the generated LaTeX to PDF:

```bash
# Compile with pdflatex (default)
ggen paper compile my-paper.tex

# Compile with xelatex (for Unicode/fonts)
ggen paper compile my-paper.tex --engine xelatex

# Compile with BibTeX bibliography
ggen paper compile my-paper.tex --bibtex

# Full compilation pipeline
ggen paper compile my-paper.tex --engine pdflatex --bibtex
# Runs: pdflatex → bibtex → pdflatex → pdflatex
```

### 3. Available LaTeX Styles

#### IEEE Conference Format
```latex
\documentclass[conference]{IEEEtran}
% Two-column format, page limits, author biographies
```

#### ACM Journal Format
```latex
\documentclass[manuscript,screen,review]{acmart}
% Modern ACM style, flexible formatting
```

#### NeurIPS Format
```latex
\documentclass{article}
\usepackage[preprint]{neurips_2023}
% Double-blind review support, anonymization
```

#### arXiv Format
```latex
\documentclass[11pt,a4paper]{article}
% Minimal styling, arXiv-compatible
```

#### Thesis Format
```latex
\documentclass[12pt,twoside,openright]{book}
% Multi-chapter organization, front/back matter
```

### 4. LaTeX Customization

Customize the LaTeX output:

```bash
# Generate with custom preamble
ggen paper generate my-paper.rdf \
  --style ieee \
  --preamble custom-preamble.tex \
  --output my-paper.tex

# Add additional packages
ggen paper generate my-paper.rdf \
  --style arxiv \
  --packages "algorithm,algpseudocode,listings" \
  --output my-paper.tex
```

---

## Bibliography Management

### 1. Initialize Bibliography

Set up bibliography management for your paper:

```bash
# Initialize from paper RDF
ggen paper init-bibliography my-paper.rdf

# Initialize with custom BibTeX file
ggen paper init-bibliography my-paper.rdf --output my-refs.bib
```

### 2. Add Bibliography Entries

Add references to your bibliography:

```bash
# Add journal article
ggen bibliography add \
  --type article \
  --key "lecun2015deep" \
  --authors "Yann LeCun, Yoshua Bengio, Geoffrey Hinton" \
  --title "Deep Learning" \
  --journal "Nature" \
  --year 2015 \
  --volume "521" \
  --pages "436-444" \
  --doi "10.1038/nature14539"

# Add conference paper
ggen bibliography add \
  --type inproceedings \
  --key "vaswani2017attention" \
  --authors "Ashish Vaswani, et al." \
  --title "Attention Is All You Need" \
  --booktitle "Proceedings of NeurIPS" \
  --year 2017

# Add book
ggen bibliography add \
  --type book \
  --key "goodfellow2016deep" \
  --authors "Ian Goodfellow, Yoshua Bengio, Aaron Courville" \
  --title "Deep Learning" \
  --publisher "MIT Press" \
  --year 2016
```

### 3. Import BibTeX Files

Import existing BibTeX files:

```bash
# Import from existing BibTeX file
ggen bibliography import --input my-references.bib

# Merge multiple BibTeX files
ggen bibliography merge refs1.bib refs2.bib --output merged.bib

# Import from external source (CrossRef, DOI)
ggen bibliography import-doi "10.1038/nature14539"
```

### 4. Citation Validation

Validate and check citations:

```bash
# Validate all citations in paper
ggen paper validate my-paper.rdf --check citations

# Check for missing required fields
ggen bibliography validate references.bib

# Find duplicate entries
ggen bibliography deduplicate references.bib

# Standardize author names
ggen bibliography normalize references.bib --output normalized.bib
```

### 5. Export Bibliography

Export bibliography to different formats:

```bash
# Export to BibTeX
ggen bibliography export references.bib --format bibtex

# Export to CSL JSON
ggen bibliography export references.bib --format csl-json

# Export to RIS format
ggen bibliography export references.bib --format ris

# Export to JSON-LD
ggen bibliography export references.bib --format json-ld
```

### 6. Citation Styles

Generate citations in different styles:

```bash
# Generate citations in IEEE style
ggen bibliography cite "lecun2015deep" --style ieee

# Generate in APA style
ggen bibliography cite "lecun2015deep" --style apa

# Generate in Chicago style
ggen bibliography cite "lecun2015deep" --style chicago

# Generate in MLA style
ggen bibliography cite "lecun2015deep" --style mla
```

---

## Peer Review Workflow

### 1. Submit for Review

Submit your paper to a venue for peer review:

```bash
# Submit to conference
ggen paper submit my-paper.pdf --venue neurips-2025

# Submit to journal with metadata
ggen paper submit my-paper.pdf --venue nature --metadata my-paper.rdf

# Submit with specific number of reviewers
ggen paper submit my-paper.pdf \
  --venue conference \
  --num-reviewers 3 \
  --deadline "2025-01-15"
```

### 2. Track Submission Status

Monitor your submission progress:

```bash
# Check submission status
ggen paper track my-paper.rdf

# Check specific venue
ggen paper track my-paper.rdf --venue neurips-2025

# Watch for updates (continuous polling)
ggen paper track my-paper.rdf --watch

# Export submission history
ggen paper track my-paper.rdf --export json
```

### 3. Reviewer Comments and Feedback

Manage reviewer feedback:

```bash
# View all reviews
ggen paper show-reviews my-paper.rdf

# View specific reviewer comments
ggen paper show-reviews my-paper.rdf --reviewer 1

# Export review summary
ggen paper export-reviews my-paper.rdf --format json

# Generate response template
ggen paper generate-response my-paper.rdf --reviewer 1
```

### 4. Author Response

Generate author responses to reviewer comments:

```bash
# Create response template
ggen paper init-response my-paper.rdf

# Edit responses
ggen paper edit-response my-paper.rdf --reviewer 1

# Validate response completeness
ggen paper validate-response my-paper.rdf

# Export formatted response
ggen paper export-response my-paper.rdf --format pdf
```

### 5. Revision Management

Track revisions and changes:

```bash
# Create revision version
ggen paper create-revision my-paper.rdf --version 2

# Track changes from v1 to v2
ggen paper diff my-paper-v1.rdf my-paper-v2.rdf

# Generate change summary
ggen paper revision-summary my-paper.rdf --version 2

# Highlight changes in PDF
ggen paper highlight-changes my-paper-v1.pdf my-paper-v2.pdf
```

---

## Publishing and Submission

### 1. arXiv Submission

Submit to arXiv for open-access preprint:

```bash
# Prepare for arXiv
ggen paper prepare-arxiv my-paper.rdf --category cs.AI

# Validate arXiv requirements
ggen paper validate my-paper.rdf --venue arxiv

# Submit to arXiv
ggen paper submit-arxiv my-paper.pdf \
  --title "My Research Paper" \
  --abstract "Research abstract..." \
  --authors "Jane Researcher, John Author" \
  --category "cs.AI" \
  --subject-class "cs.CL, cs.LG"

# Track arXiv submission
ggen paper track my-paper.rdf --venue arxiv
```

### 2. DOI Registration

Register Digital Object Identifiers:

```bash
# Register DOI with CrossRef
ggen paper register-doi my-paper.rdf \
  --doi-agency crossref \
  --publisher "My University" \
  --publication-date "2025-01-15"

# Look up existing DOI
ggen bibliography lookup-doi "10.1038/nature14539"

# Add DOI to bibliography entry
ggen bibliography add-doi "lecun2015deep" "10.1038/nature14539"
```

### 3. Journal Submission

Submit to journal or conference:

```bash
# Submit to journal
ggen paper submit-journal my-paper.pdf \
  --journal "Nature Machine Intelligence" \
  --corresponding-author "jane@university.edu"

# Check journal requirements
ggen paper validate my-paper.rdf --journal "Nature"

# Track journal submission
ggen paper track my-paper.rdf --venue "Nature Machine Intelligence"
```

### 4. Export for Submission

Export paper in required formats for submission:

```bash
# Export as single PDF (single submission file)
ggen paper export my-paper.rdf --format pdf --single-file

# Export with supplementary materials
ggen paper export my-paper.rdf --format zip --include-supplementary

# Export manuscript without author info (blind review)
ggen paper export my-paper.rdf --format pdf --blind
```

---

## Marketplace Templates

### 1. Available Marketplace Packages

Discover and install academic paper templates:

```bash
# List all academic paper templates
ggen marketplace search "academic paper"

# Search for specific format
ggen marketplace search "ieee" --category academic-paper

# View template details
ggen marketplace show ieee-paper-template

# Check template maturity
ggen marketplace show arxiv-paper-template --verbose
```

### 2. Install and Use Templates

Install templates from marketplace:

```bash
# Install IEEE template
ggen marketplace install ieee-paper-template

# Install multiple templates
ggen marketplace install ieee-paper-template acm-journal-template arxiv-paper-template

# Create paper from installed template
ggen paper new "Research Title" --template ieee

# Update installed templates
ggen marketplace update ieee-paper-template
```

### 3. Available Marketplace Packages

#### Academic Paper Templates

| Package | Version | Description | Maturity |
|---------|---------|-------------|----------|
| `ieee-paper-template` | 1.0.0 | IEEE conference paper (two-column) | 95% |
| `acm-journal-template` | 1.0.0 | ACM journal format | 95% |
| `neurips-paper-template` | 1.0.0 | NeurIPS conference (double-blind) | 95% |
| `arxiv-paper-template` | 1.0.0 | arXiv preprint | 95% |
| `phd-thesis-template` | 1.0.0 | PhD thesis (multi-chapter) | 95% |
| `nature-journal-template` | 1.0.0 | Nature journal | 92% |
| `science-journal-template` | 1.0.0 | Science journal | 92% |
| `icml-paper-template` | 1.0.0 | ICML conference | 90% |
| `iclr-paper-template` | 1.0.0 | ICLR conference | 90% |
| `cvpr-paper-template` | 1.0.0 | CVPR conference | 88% |

#### Academic Workflows

| Package | Version | Description | Maturity |
|---------|---------|-------------|----------|
| `academic-peer-review-workflow` | 1.0.0 | Multi-reviewer peer review | 90% |
| `academic-bibliography-manager` | 1.0.0 | BibTeX management & validation | 92% |
| `paper-submission-tracker` | 1.0.0 | Submission & review tracking | 85% |
| `latex-compiler-toolkit` | 1.0.0 | PDF compilation & optimization | 88% |
| `paper-publishing-pipeline` | 1.0.0 | arXiv, DOI, journal publishing | 85% |

### 4. Create Custom Templates

Create and publish your own academic paper templates:

```bash
# Create custom template
mkdir -p my-journal-template/{templates,data,docs}

# Create package.toml
cat > my-journal-template/package.toml << 'EOF'
[package]
name = "my-journal-template"
version = "1.0.0"
description = "My custom journal template"
category = "academic-paper"
EOF

# Create LaTeX template
cat > my-journal-template/templates/my-journal.tmpl << 'EOF'
\documentclass{article}
...
EOF

# Publish to marketplace
ggen marketplace publish my-journal-template

# Share with community
ggen marketplace share my-journal-template --github
```

---

## Advanced Features

### 1. Multi-Author Collaboration

Manage papers with multiple authors:

```bash
# Add authors to paper
ggen paper add-author my-paper.rdf \
  --name "Jane Researcher" \
  --email "jane@university.edu" \
  --affiliation "MIT" \
  --order 1

# Add corresponding author
ggen paper set-corresponding-author my-paper.rdf --author "Jane Researcher"

# Track author contributions (CRediT)
ggen paper add-contribution my-paper.rdf \
  --author "Jane Researcher" \
  --role "Conceptualization,Methodology,Writing" \
  --percent 50

# Manage conflicts of interest
ggen paper add-coi my-paper.rdf \
  --author "Jane" \
  --type "financial" \
  --description "Employed by ABC Corp"
```

### 2. Version Control Integration

Integrate with Git for version control:

```bash
# Initialize paper with Git
ggen paper init-git my-paper

# Commit version to Git
ggen paper commit my-paper.rdf \
  --message "Update results section" \
  --author "Jane Researcher"

# Track versions in Git
ggen paper versions my-paper.rdf

# Compare versions
ggen paper diff my-paper-v1.rdf my-paper-v2.rdf --format git
```

### 3. Real-Time Collaboration

Enable real-time multi-author editing:

```bash
# Start collaboration session
ggen paper collaborate my-paper.rdf --mode realtime

# Share editing link
ggen paper share my-paper.rdf --link

# Manage access permissions
ggen paper permission my-paper.rdf --user jane@uni.edu --role editor
ggen paper permission my-paper.rdf --user john@uni.edu --role viewer

# Sync changes
ggen paper sync my-paper.rdf
```

### 4. SPARQL Queries on Paper Data

Query paper metadata using SPARQL:

```bash
# Find all papers by author
ggen graph query "SELECT ?paper WHERE { ?paper ap:hasAuthor ?author . ?author ap:authorName 'Jane Researcher' }"

# Find papers by discipline
ggen graph query "SELECT ?paper ?title WHERE { ?paper ap:discipline ap:ComputerScience . ?paper ap:title ?title }"

# Find papers with missing abstracts
ggen graph query "SELECT ?paper WHERE { ?paper a ap:Paper . FILTER NOT EXISTS { ?paper ap:abstract ?abstract } }"
```

### 5. CI/CD Integration

Automate paper workflows with CI/CD:

```bash
# Create GitHub Actions workflow
cat > .github/workflows/paper-publish.yml << 'EOF'
name: Paper Publishing Pipeline
on: [push]
jobs:
  paper:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v2
      - name: Validate paper
        run: ggen paper validate paper.rdf
      - name: Generate LaTeX
        run: ggen paper generate paper.rdf --style arxiv
      - name: Compile PDF
        run: ggen paper compile paper.tex
      - name: Upload artifacts
        uses: actions/upload-artifact@v2
        with:
          name: paper.pdf
          path: paper.pdf
EOF
```

---

## Troubleshooting

### Common Issues

#### 1. LaTeX Compilation Errors

**Problem**: `pdflatex: command not found`

**Solution**: Install LaTeX distribution

```bash
# Ubuntu/Debian
sudo apt-get install texlive-full texlive-fonts-recommended

# macOS
brew install basictex

# Or use online compilation
ggen paper compile my-paper.tex --online
```

#### 2. Missing Bibliography

**Problem**: Bibliography entries not showing in PDF

**Solution**: Run full compilation pipeline

```bash
ggen paper compile my-paper.tex --bibtex --full

# Or manually:
pdflatex my-paper.tex
bibtex my-paper.aux
pdflatex my-paper.tex
pdflatex my-paper.tex
```

#### 3. RDF Ontology Validation Errors

**Problem**: "Invalid RDF syntax"

**Solution**: Validate ontology format

```bash
# Validate RDF format
ggen graph validate paper.rdf

# Convert from other formats
ggen graph convert paper.json --output-format turtle
```

#### 4. Encoding Issues

**Problem**: "! Package inputenc Error: Unicode character"

**Solution**: Use xelatex engine

```bash
ggen paper compile my-paper.tex --engine xelatex
```

#### 5. File Not Found Errors

**Problem**: "! I can't find file 'figures/diagram.png'"

**Solution**: Ensure relative paths are correct

```bash
# Check figure paths
ggen paper validate my-paper.rdf --check figures

# Update paths in RDF
ggen paper edit my-paper.rdf --fix-paths
```

### Debug Mode

Enable verbose logging for troubleshooting:

```bash
# Run with debug output
ggen paper generate my-paper.rdf --style ieee --verbose

# Generate debug report
ggen paper debug-report my-paper.rdf --output debug.log

# Check system requirements
ggen utils doctor
```

---

## Resources

### Documentation

- [Academic Paper Ontology Reference](./ontology-reference.md)
- [LaTeX Style Guide](./latex-guide.md)
- [Bibliography Management Guide](./bibliography-guide.md)
- [Peer Review Workflow Guide](./peer-review-guide.md)

### Examples

- [Sample IEEE Paper](../examples/ieee-sample-paper/)
- [Sample arXiv Paper](../examples/arxiv-sample-paper/)
- [Sample PhD Thesis](../examples/phd-sample-thesis/)
- [Sample Peer Review Process](../examples/peer-review-example/)

### External Resources

- [arXiv Submission Guide](https://arxiv.org/help)
- [CrossRef DOI Registration](https://www.crossref.org/)
- [ORCID Researcher Identifiers](https://orcid.org/)
- [IEEE LaTeX Templates](https://template-selector.ieee.org/)
- [ACM LaTeX Templates](https://www.acm.org/publications/proceedings-template)

---

## Getting Help

For issues and questions:

- GitHub Issues: https://github.com/seanchatmangpt/ggen/issues
- Documentation: https://docs.claude.com/ggen
- Community Forum: https://community.ggen.dev
- Email Support: support@ggen.dev

---

**Last Updated**: 2025-01-15
**Version**: 1.0.0
**Authors**: ggen core team
