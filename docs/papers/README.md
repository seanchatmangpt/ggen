<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->
**Table of Contents**

- [Academic Paper Lifecycle Documentation](#academic-paper-lifecycle-documentation)
  - [📚 Documentation Index](#-documentation-index)
    - [Getting Started](#getting-started)
    - [Core Guides](#core-guides)
    - [Reference Documentation](#reference-documentation)
    - [Marketplace](#marketplace)
    - [Examples](#examples)
    - [Tutorials](#tutorials)
    - [How-To Guides](#how-to-guides)
    - [Troubleshooting](#troubleshooting)
    - [Advanced Topics](#advanced-topics)
  - [Quick Navigation](#quick-navigation)
    - [By Use Case](#by-use-case)
    - [By Role](#by-role)
  - [Key Features](#key-features)
    - [📄 Paper Management](#-paper-management)
    - [📝 LaTeX Generation](#-latex-generation)
    - [📚 Bibliography Management](#-bibliography-management)
    - [👥 Peer Review Workflow](#-peer-review-workflow)
    - [📤 Publishing Integration](#-publishing-integration)
    - [🏪 Marketplace Integration](#-marketplace-integration)
  - [System Requirements](#system-requirements)
    - [Required](#required)
    - [Optional](#optional)
  - [Common Commands](#common-commands)
  - [File Structure](#file-structure)
  - [Getting Help](#getting-help)
    - [Documentation](#documentation)
    - [Online Resources](#online-resources)
    - [Support](#support)
  - [Contributing](#contributing)
  - [License](#license)
  - [Acknowledgments](#acknowledgments)
  - [Version History](#version-history)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

# Academic Paper Lifecycle Documentation

Complete documentation for the ggen Academic Paper Lifecycle system for creating, managing, and publishing research papers using RDF ontologies and LaTeX generation.

## 📚 Documentation Index

### Getting Started
- **[Quick Start Guide](./QUICKSTART.md)** - Get up and running in 5 minutes
- **[Installation](./INSTALLATION.md)** - Install ggen and paper templates
- **[First Paper Tutorial](./FIRST_PAPER.md)** - Create your first paper step-by-step

### Core Guides
- **[Academic Paper Lifecycle](./ACADEMIC_PAPER_LIFECYCLE.md)** - Complete lifecycle guide
  - Paper creation and metadata
  - LaTeX generation and compilation
  - Bibliography management
  - Peer review workflows
  - Publishing and submission
  - Marketplace templates
  - Advanced features

### Reference Documentation
- **[Ontology Reference](./ONTOLOGY_REFERENCE.md)** - Academic Paper Ontology v1.0.0
  - Data model and schema
  - SPARQL queries
  - Type mappings
  - Inference rules

- **[LaTeX Template Reference](./LATEX_TEMPLATES.md)** - Available LaTeX styles
  - IEEE Conference Format
  - ACM Journal Format
  - NeurIPS Conference Format
  - arXiv Preprint Format
  - PhD Thesis Format
  - Custom template creation

- **[Bibliography Management](./BIBLIOGRAPHY_GUIDE.md)** - Citation and reference management
  - BibTeX integration
  - Citation styles
  - Import/export formats
  - Citation validation
  - DOI lookup and assignment

- **[Peer Review Workflow](./PEER_REVIEW_GUIDE.md)** - Collaborative review process
  - Submission management
  - Reviewer tracking
  - Comment handling
  - Author responses
  - Revision management

- **[Publishing Guide](./PUBLISHING_GUIDE.md)** - Academic publishing workflows
  - arXiv submission
  - Journal submission
  - DOI registration
  - CrossRef integration
  - Open access publishing

### Marketplace
- **[Marketplace Templates](./MARKETPLACE_TEMPLATES.md)** - Available paper templates
  - IEEE Paper Template
  - ACM Journal Template
  - NeurIPS Template
  - arXiv Template
  - PhD Thesis Template
  - Peer Review Workflow
  - Bibliography Manager

- **[Create Custom Templates](./CUSTOM_TEMPLATES.md)** - Create and share templates
  - Template structure
  - Package.toml format
  - Publishing to marketplace
  - Community guidelines

### Examples
- **[Example Papers](./EXAMPLES.md)** - Complete example papers
  - [Sample IEEE Paper](../examples/ieee-sample-paper/)
  - [Sample arXiv Paper](../examples/arxiv-sample-paper/)
  - [Sample PhD Thesis](../examples/phd-sample-thesis/)
  - [Sample Peer Review](../examples/peer-review-example/)

### Tutorials
- **[Paper Creation Tutorial](./TUTORIALS/01-paper-creation.md)** - Create and edit papers
- **[LaTeX Generation Tutorial](./TUTORIALS/02-latex-generation.md)** - Generate and compile
- **[Bibliography Tutorial](./TUTORIALS/03-bibliography.md)** - Manage citations
- **[Peer Review Tutorial](./TUTORIALS/04-peer-review.md)** - Coordinate reviews
- **[Publishing Tutorial](./TUTORIALS/05-publishing.md)** - Submit and track

### How-To Guides
- **[How to Submit to arXiv](./HOWTO/arxiv-submission.md)**
- **[How to Submit to Conferences](./HOWTO/conference-submission.md)**
- **[How to Submit to Journals](./HOWTO/journal-submission.md)**
- **[How to Manage Multi-Author Papers](./HOWTO/multi-author.md)**
- **[How to Track Submissions](./HOWTO/submission-tracking.md)**
- **[How to Handle Peer Reviews](./HOWTO/peer-reviews.md)**
- **[How to Revise Based on Feedback](./HOWTO/revisions.md)**
- **[How to Integrate with Git](./HOWTO/git-integration.md)**
- **[How to Set Up CI/CD](./HOWTO/cicd-pipelines.md)**

### Troubleshooting
- **[Troubleshooting Guide](./TROUBLESHOOTING.md)** - Common issues and solutions
  - LaTeX compilation errors
  - RDF parsing errors
  - Bibliography issues
  - Submission problems
  - Performance optimization

### Advanced Topics
- **[SPARQL Queries](./ADVANCED/sparql-queries.md)** - Query papers with SPARQL
- **[RDF Integration](./ADVANCED/rdf-integration.md)** - Work with RDF ontologies
- **[Custom Workflows](./ADVANCED/custom-workflows.md)** - Create custom paper workflows
- **[API Integration](./ADVANCED/api-integration.md)** - Integrate with external APIs
- **[CI/CD Integration](./ADVANCED/cicd-integration.md)** - Automate paper workflows

---

## Quick Navigation

### By Use Case

**I want to...**

- **Create a paper** → See [Quick Start](./QUICKSTART.md) or [First Paper Tutorial](./FIRST_PAPER.md)
- **Generate PDF** → See [LaTeX Generation Tutorial](./TUTORIALS/02-latex-generation.md)
- **Manage citations** → See [Bibliography Guide](./BIBLIOGRAPHY_GUIDE.md)
- **Submit to arXiv** → See [arXiv Submission How-To](./HOWTO/arxiv-submission.md)
- **Submit to a conference** → See [Conference Submission How-To](./HOWTO/conference-submission.md)
- **Coordinate peer review** → See [Peer Review Workflow Guide](./PEER_REVIEW_GUIDE.md)
- **Collaborate with others** → See [Multi-Author How-To](./HOWTO/multi-author.md)
- **Automate workflows** → See [CI/CD Integration Guide](./ADVANCED/cicd-integration.md)
- **Query papers with SPARQL** → See [SPARQL Queries Guide](./ADVANCED/sparql-queries.md)
- **Create custom templates** → See [Custom Templates Guide](./CUSTOM_TEMPLATES.md)

### By Role

**I'm a...**

- **First-time user** → Start with [Quick Start](./QUICKSTART.md)
- **Researcher** → Read [Academic Paper Lifecycle](./ACADEMIC_PAPER_LIFECYCLE.md)
- **PhD student** → Check [PhD Thesis Template](./MARKETPLACE_TEMPLATES.md#phd-thesis)
- **Journal author** → See [Journal Submission How-To](./HOWTO/journal-submission.md)
- **Conference author** → See [Conference Submission How-To](./HOWTO/conference-submission.md)
- **Team lead** → Read [Multi-Author How-To](./HOWTO/multi-author.md)
- **Tool developer** → Check [Custom Templates Guide](./CUSTOM_TEMPLATES.md)
- **DevOps engineer** → See [CI/CD Integration](./ADVANCED/cicd-integration.md)

---

## Key Features

### 📄 Paper Management
- Create papers from templates
- Edit RDF metadata
- Organize sections, figures, tables
- Track document versions
- Multi-author collaboration

### 📝 LaTeX Generation
- Multiple LaTeX styles (IEEE, ACM, NeurIPS, arXiv, Thesis)
- Automatic bibliography integration
- Figure and table generation
- PDF compilation with error handling
- Citation management

### 📚 Bibliography Management
- BibTeX support
- Citation validation
- DOI lookup and assignment
- ORCID integration
- Multiple citation styles
- Import/export formats

### 👥 Peer Review Workflow
- Submission tracking
- Multi-reviewer coordination
- Comment and feedback management
- Author response templates
- Revision change tracking
- Decision workflow management

### 📤 Publishing Integration
- arXiv submission API
- CrossRef DOI registration
- Journal submission workflows
- Preprint server support
- Open-access publishing

### 🏪 Marketplace Integration
- 10+ paper templates
- Peer review workflows
- Bibliography managers
- Publishing pipelines
- Community sharing

---

## System Requirements

### Required
- **ggen 2.7.0+** - Latest version with paper support
- **Python 3.8+** - For bibliography management
- **Git 2.20+** - For version control integration

### Optional
- **pdflatex/xelatex** - For local PDF compilation
  - Ubuntu/Debian: `apt-get install texlive-full`
  - macOS: `brew install basictex`
  - Windows: [MiKTeX](https://miktex.org/)
- **Bibtex** - For bibliography compilation
- **ImageMagick** - For figure processing
- **pandoc** - For format conversion

---

## Common Commands

```bash
# Paper Management
ggen paper new "Title" --template arxiv
ggen paper edit paper.rdf
ggen paper validate paper.rdf
ggen paper list-templates

# LaTeX Generation
ggen paper generate paper.rdf --style ieee
ggen paper compile paper.tex --bibtex

# Bibliography
ggen paper init-bibliography paper.rdf
ggen bibliography add --key "key2024" --title "Title" --year 2024
ggen bibliography import references.bib

# Publishing
ggen paper submit paper.pdf --venue arxiv
ggen paper track paper.rdf
ggen paper register-doi paper.rdf

# Marketplace
ggen marketplace search "paper template"
ggen marketplace install ieee-paper-template
```

---

## File Structure

```
ggen/
├── ontologies/
│   └── academic-paper_v1.0.0.ttl          # Paper RDF ontology
├── templates/papers/
│   ├── ieee-conference.tmpl               # IEEE LaTeX template
│   ├── acm-journal.tmpl                   # ACM LaTeX template
│   ├── neurips-conference.tmpl            # NeurIPS LaTeX template
│   ├── arxiv-preprint.tmpl                # arXiv LaTeX template
│   ├── phd-thesis.tmpl                    # PhD thesis LaTeX template
│   └── bibtex-references.tmpl             # BibTeX template
├── marketplace/packages/
│   ├── ieee-paper-template/               # IEEE template package
│   ├── acm-journal-template/              # ACM template package
│   ├── neurips-paper-template/            # NeurIPS template package
│   ├── arxiv-paper-template/              # arXiv template package
│   ├── phd-thesis-template/               # PhD thesis template package
│   ├── academic-peer-review-workflow/     # Peer review workflow
│   └── academic-bibliography-manager/     # Bibliography manager
├── crates/ggen-cli/src/cmds/
│   └── paper.rs                           # Paper CLI commands
├── docs/papers/
│   ├── README.md                          # This file
│   ├── QUICKSTART.md                      # Quick start guide
│   ├── ACADEMIC_PAPER_LIFECYCLE.md        # Complete lifecycle guide
│   └── [other documentation files]
└── examples/
    └── academic-paper-example.rdf         # Example paper
```

---

## Getting Help

### Documentation
- Full documentation: [ACADEMIC_PAPER_LIFECYCLE.md](./ACADEMIC_PAPER_LIFECYCLE.md)
- Quick start: [QUICKSTART.md](./QUICKSTART.md)
- Troubleshooting: [TROUBLESHOOTING.md](./TROUBLESHOOTING.md)

### Online Resources
- GitHub: https://github.com/seanchatmangpt/ggen
- Issues: https://github.com/seanchatmangpt/ggen/issues
- Discussions: https://github.com/seanchatmangpt/ggen/discussions
- Documentation: https://docs.claude.com/ggen

### Support
- Email: support@ggen.dev
- Community Forum: https://community.ggen.dev
- Discord: https://discord.gg/ggen

---

## Contributing

We welcome contributions! See [CONTRIBUTING.md](../../CONTRIBUTING.md) for:
- How to report issues
- How to submit pull requests
- Code of conduct
- Development setup

---

## License

The Academic Paper Lifecycle system is part of ggen and is licensed under the MIT License.
See [LICENSE](../../LICENSE) for details.

---

## Acknowledgments

The Academic Paper Lifecycle system builds on:
- [RDF 1.1 Concepts](https://www.w3.org/TR/rdf11-concepts/)
- [SPARQL 1.1 Query Language](https://www.w3.org/TR/sparql11-query/)
- [BibTeX Format](http://www.bibtex.org/)
- [IEEE LaTeX Class](https://www.ctan.org/pkg/ieeetran)
- [ACM LaTeX Class](https://www.acm.org/publications/proceedings-template)
- [NeurIPS LaTeX Class](https://www.neurips.cc/Conferences/2023/PapersAuthors/PapersAuthorsLaTeXGuidelines)

---

## Version History

- **v1.0.0** (2025-01-15) - Initial release
  - Complete paper lifecycle system
  - 5 LaTeX templates
  - Peer review workflow
  - Bibliography management
  - Publishing integration
  - 10+ marketplace packages

---

**Last Updated**: 2025-01-15
**Maintained by**: ggen core team
**Status**: Production Ready (95% maturity)
