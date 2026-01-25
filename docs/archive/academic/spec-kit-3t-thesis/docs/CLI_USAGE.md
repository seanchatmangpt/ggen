<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->
**Table of Contents**

- [Spec-Kit-3T CLI Usage Guide](#spec-kit-3t-cli-usage-guide)
  - [Installation](#installation)
  - [Commands](#commands)
    - [1. Generate LaTeX from RDF](#1-generate-latex-from-rdf)
    - [2. Validate RDF Ontology](#2-validate-rdf-ontology)
    - [3. Extract Text from PDF](#3-extract-text-from-pdf)
    - [4. Clean Extracted Text](#4-clean-extracted-text)
    - [5. Compile LaTeX to PDF](#5-compile-latex-to-pdf)
    - [6. Version Information](#6-version-information)
  - [Complete Workflow](#complete-workflow)
  - [Test Results](#test-results)
  - [Punctuation Analysis Results](#punctuation-analysis-results)
  - [Files Created](#files-created)
    - [CLI Implementation](#cli-implementation)
    - [Tests](#tests)
    - [Configuration](#configuration)
  - [Dependencies](#dependencies)
  - [Constitutional Equation](#constitutional-equation)
  - [Next Steps](#next-steps)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

# Spec-Kit-3T CLI Usage Guide

## Installation

```bash
pip install -e .
```

## Commands

### 1. Generate LaTeX from RDF

```bash
spec-kit-3t generate [OPTIONS]
```

**Options:**
- `--ontology-dir, -o PATH`: Directory containing RDF ontology files (default: `.`)
- `--output-dir, -d PATH`: Output directory for LaTeX files (default: `generated`)
- `--force, -f`: Force regeneration of all files
- `--verbose, -v`: Enable verbose output

**Example:**
```bash
spec-kit-3t generate --ontology-dir . --output-dir generated --verbose
```

### 2. Validate RDF Ontology

```bash
spec-kit-3t validate [ONTOLOGY_FILE] [OPTIONS]
```

**Options:**
- `--schema, -s PATH`: SHACL shape file (default: `thesis-schema.ttl`)
- `--verbose, -v`: Enable verbose output

**Example:**
```bash
spec-kit-3t validate spec-kit-3t-content.ttl --schema thesis-schema.ttl
```

### 3. Extract Text from PDF

```bash
spec-kit-3t extract [PDF_FILE] [OPTIONS]
```

**Options:**
- `--output, -o PATH`: Output text file (default: stdout)
- `--analyze, -a`: Analyze punctuation patterns

**Example:**
```bash
# Extract to stdout
spec-kit-3t extract generated/thesis-main.pdf

# Extract to file with analysis
spec-kit-3t extract generated/thesis-main.pdf --output extracted.txt --analyze
```

**Analysis Output:**
```
Punctuation Analysis:

┏━━━━━━━━━┳━━━━━━━┓
┃ Pattern ┃ Count ┃
┡━━━━━━━━━╇━━━━━━━┩
│ '**: '  │   101 │
│ '\n**'  │    37 │
│ '. **'  │    34 │
└─────────┴───────┘

⚠️  Found 145 instances of double spacing
```

### 4. Clean Extracted Text

```bash
spec-kit-3t clean [INPUT_FILE] --output [OUTPUT_FILE] [OPTIONS]
```

**Options:**
- `--output, -o PATH`: Output cleaned text file (required)
- `--fix-spacing/--no-fix-spacing`: Fix double spacing (default: enabled)
- `--fix-punctuation/--no-fix-punctuation`: Fix errant punctuation (default: enabled)

**Example:**
```bash
spec-kit-3t clean extracted.txt --output cleaned.txt
```

**Cleaning Rules:**
- Removes double/triple spacing
- Removes LaTeX bold markers (`**`)
- Fixes patterns like `**:`, `)**`, etc.

### 5. Compile LaTeX to PDF

```bash
spec-kit-3t pdf [OPTIONS]
```

**Options:**
- `--latex-dir, -d PATH`: Directory containing LaTeX files (default: `generated`)
- `--main, -m TEXT`: Main LaTeX file name (default: `thesis-main.tex`)
- `--verbose, -v`: Show pdflatex output

**Example:**
```bash
spec-kit-3t pdf --latex-dir generated --main thesis-main.tex
```

**Compilation Pipeline:**
1. pdflatex (first pass)
2. biber (bibliography)
3. pdflatex (second pass)
4. pdflatex (third pass for references)

### 6. Version Information

```bash
spec-kit-3t version
```

**Output:**
```
Spec-Kit-3T Thesis Generator v2.0.0
Constitutional Equation: thesis.tex = μ(ontology.ttl)
```

## Complete Workflow

```bash
# 1. Validate RDF ontology
spec-kit-3t validate spec-kit-3t-content.ttl --schema thesis-schema.ttl

# 2. Generate LaTeX files
spec-kit-3t generate --force --verbose

# 3. Compile to PDF
spec-kit-3t pdf --verbose

# 4. Extract text for analysis
spec-kit-3t extract generated/thesis-main.pdf --output extracted.txt --analyze

# 5. Clean extracted text
spec-kit-3t clean extracted.txt --output cleaned.txt
```

## Test Results

```bash
$ python -m pytest tests/cli/ -v

======================== 21 passed, 2 skipped in 1.83s =========================
Required test coverage of 55% reached. Total coverage: 56.32%
```

**Test Categories:**
- ✅ Unit tests (18 tests)
- ✅ Integration tests (2 tests)
- ✅ Performance tests (1 test)
- ✅ Security tests (2 tests)

## Punctuation Analysis Results

**From generated thesis (38 pages):**
- **Total lines**: 1,126
- **LaTeX markers removed**: 379 instances of `**`
- **Common patterns detected**:
  - `**: ` (101 times) - LaTeX bold + colon
  - `\n**` (37 times) - Newline + bold marker
  - `. **` (34 times) - Period + bold marker

## Files Created

### CLI Implementation
- `cli/__init__.py` - Package initialization
- `cli/main.py` - Main CLI with all commands (173 statements, 57% coverage)

### Tests
- `tests/cli/__init__.py` - Test package
- `tests/cli/test_main.py` - Unit tests (18 tests)
- `tests/cli/test_integration.py` - Integration tests (5 tests)

### Configuration
- `requirements.txt` - Python dependencies
- `setup.py` - Package setup for installation
- `pytest.ini` - Pytest configuration

## Dependencies

**Core:**
- `rdflib>=7.0.0` - RDF graph operations
- `pyshacl>=0.25.0` - SHACL validation
- `Jinja2>=3.1.0` - Template rendering
- `typer>=0.9.0` - CLI framework
- `rich>=13.0.0` - Terminal formatting

**Development:**
- `pytest>=7.4.0` - Testing framework
- `pytest-cov>=4.1.0` - Coverage reporting

**System Requirements:**
- `pdftotext` - PDF text extraction (via poppler-utils)
- `pdflatex` - LaTeX compilation
- `biber` - Bibliography processing

## Constitutional Equation

```
thesis.tex = μ(ontology.ttl)
```

Where:
- **μ₁**: Load RDF + SHACL validation
- **μ₂**: SPARQL extraction
- **μ₃**: Template rendering
- **μ₄**: Write LaTeX files
- **μ₅**: Generate cryptographic receipt

## Next Steps

1. **Add more templates** - Support for articles, presentations, etc.
2. **Enhance validation** - More SHACL constraints for template variables
3. **Performance optimization** - Parallel SPARQL queries
4. **Cloud integration** - S3 storage for generated artifacts
5. **CI/CD** - GitHub Actions workflow for automated builds
