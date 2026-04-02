<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->
**Table of Contents**

- [DSPy Integration Usage Guide](#dspy-integration-usage-guide)
  - [Overview](#overview)
  - [Prerequisites](#prerequisites)
    - [1. Install Ollama](#1-install-ollama)
    - [2. Pull olmo-3:7b-instruct Model](#2-pull-olmo-37b-instruct-model)
    - [3. Install Spec-Kit-3T with DSPy](#3-install-spec-kit-3t-with-dspy)
  - [Quick Start](#quick-start)
    - [Basic Enhancement Workflow](#basic-enhancement-workflow)
    - [Output Example](#output-example)
  - [CLI Commands](#cli-commands)
    - [`dspy enhance`](#dspy-enhance)
    - [`dspy suggest`](#dspy-suggest)
    - [`dspy cache-stats`](#dspy-cache-stats)
    - [`dspy cache-clear`](#dspy-cache-clear)
  - [Configuration](#configuration)
    - [DSPy Configuration File](#dspy-configuration-file)
    - [Customizing Configuration](#customizing-configuration)
  - [DSPy Modules](#dspy-modules)
    - [1. Abstract Enhancement](#1-abstract-enhancement)
    - [2. Section Suggestions](#2-section-suggestions)
    - [3. Diataxis Validation](#3-diataxis-validation)
    - [4. Citation Suggestions](#4-citation-suggestions)
    - [5. Coherence Analysis](#5-coherence-analysis)
  - [Cache Management](#cache-management)
    - [Cache Strategy](#cache-strategy)
    - [Cache Workflow](#cache-workflow)
    - [Cache Statistics](#cache-statistics)
  - [Error Handling](#error-handling)
    - [Ollama Not Running](#ollama-not-running)
    - [Model Response Timeout](#model-response-timeout)
    - [Cache Corruption](#cache-corruption)
    - [Import Error](#import-error)
  - [Performance Tips](#performance-tips)
    - [1. Use Cache Aggressively](#1-use-cache-aggressively)
    - [2. Selective Enhancement](#2-selective-enhancement)
    - [3. Adjust Timeout](#3-adjust-timeout)
    - [4. Batch Operations](#4-batch-operations)
  - [Integration with Existing Pipeline](#integration-with-existing-pipeline)
    - [Constitutional Equation: μ + λ](#constitutional-equation-%CE%BC--%CE%BB)
    - [Workflow Integration](#workflow-integration)
  - [Advanced Usage](#advanced-usage)
    - [Custom DSPy Modules](#custom-dspy-modules)
    - [Programmatic API](#programmatic-api)
  - [Troubleshooting](#troubleshooting)
    - [Issue: "No abstract found in RDF"](#issue-no-abstract-found-in-rdf)
    - [Issue: Poor suggestion quality](#issue-poor-suggestion-quality)
    - [Issue: Slow performance](#issue-slow-performance)
  - [Best Practices](#best-practices)
  - [Future Enhancements](#future-enhancements)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

# DSPy Integration Usage Guide

## Overview

Spec-Kit-3T v2.1.0 integrates DSPy framework with Ollama's olmo-3:7b-instruct model to enhance thesis generation with LLM capabilities while maintaining deterministic core pipeline.

**Constitutional Equation Evolution**:
```
v2.0: thesis.tex = μ(ontology.ttl)
v2.1: thesis.tex = μ(ontology.ttl) + λ(context, suggestions)
```

Where:
- `μ`: Deterministic pipeline (μ₁-μ₅) - ALWAYS runs first
- `λ`: LLM enhancement layer (optional, adds value without breaking determinism)

## Prerequisites

### 1. Install Ollama

```bash
# macOS
brew install ollama

# Linux
curl -fsSL https://ollama.com/install.sh | sh

# Windows
# Download from https://ollama.com/download
```

### 2. Pull olmo-3:7b-instruct Model

```bash
# Start Ollama server
ollama serve

# Pull model (in another terminal)
ollama pull olmo-3:7b-instruct

# Verify model is available
ollama list
```

### 3. Install Spec-Kit-3T with DSPy

```bash
pip install -e .
# This installs dspy-ai>=2.5.0 and all dependencies
```

## Quick Start

### Basic Enhancement Workflow

```bash
# 1. Run deterministic pipeline first (REQUIRED)
spec-kit-3t generate

# 2. Enhance with LLM (all modules)
spec-kit-3t dspy enhance

# 3. View suggestions
cat llm_suggestions.json

# 4. Optionally regenerate with enhancements incorporated
spec-kit-3t generate
```

### Output Example

```json
{
  "summary": {
    "total_suggestions": 15,
    "modules_run": ["abstract", "sections", "diataxis", "citations", "coherence"],
    "cache_hits": 3,
    "cache_misses": 12
  },
  "modules": {
    "abstract": {
      "status": "success",
      "enhancement": {
        "expanded_abstract": "This thesis presents a novel approach to...",
        "motivation": "The need for deterministic knowledge graph generation..."
      },
      "original_length": 45,
      "enhanced_length": 210
    }
  }
}
```

## CLI Commands

### `dspy enhance`

Enhance thesis using all or selected DSPy modules.

**Usage**:
```bash
spec-kit-3t dspy enhance [OPTIONS]
```

**Options**:
- `--ontology-dir, -o PATH` - Directory containing RDF ontology (default: `.`)
- `--output, -out PATH` - Output file for suggestions (default: `llm_suggestions.json`)
- `--modules, -m TEXT` - Comma-separated modules (default: `all`)
  - Options: `abstract`, `sections`, `diataxis`, `citations`, `coherence`
- `--cache/--no-cache` - Use LLM output cache (default: `--cache`)
- `--verbose, -v` - Enable verbose output

**Examples**:
```bash
# Enhance entire thesis (all modules)
spec-kit-3t dspy enhance

# Only enhance abstract
spec-kit-3t dspy enhance --modules abstract

# Multiple modules
spec-kit-3t dspy enhance --modules abstract,sections,diataxis

# Skip cache for fresh suggestions
spec-kit-3t dspy enhance --no-cache

# Verbose mode
spec-kit-3t dspy enhance --verbose
```

### `dspy suggest`

Get content suggestions for a specific section.

**Usage**:
```bash
spec-kit-3t dspy suggest SECTION [OPTIONS]
```

**Arguments**:
- `SECTION` - Section path in format: `chapter-N/section-title`

**Options**:
- `--ontology-dir, -o PATH` - Directory containing RDF ontology

**Examples**:
```bash
# Get suggestions for Introduction section in Chapter 3
spec-kit-3t dspy suggest "chapter-3/Introduction"

# Get suggestions for Methodology
spec-kit-3t dspy suggest "chapter-4/Methodology"
```

**Output Example**:
```
┌─────────────────────────────────────────────────────────────┐
│ Section: chapter-3/Introduction                             │
├─────────────────────────────────────────────────────────────┤
│ Content Suggestions:                                        │
│                                                             │
│ 1. Add research context and motivation                     │
│ 2. Define key terminology                                  │
│ 3. State research questions explicitly                     │
│ 4. Outline chapter structure                               │
│                                                             │
│ Related Concepts:                                           │
│ knowledge graphs, RDF, ontologies, semantic web            │
└─────────────────────────────────────────────────────────────┘
```

### `dspy cache-stats`

Show DSPy cache statistics.

**Usage**:
```bash
spec-kit-3t dspy cache-stats
```

**Output Example**:
```
┏━━━━━━━━━━━━━━━━━┳━━━━━━━━┓
┃ Metric          ┃ Value  ┃
┡━━━━━━━━━━━━━━━━━╇━━━━━━━━┩
│ Total Entries   │ 24     │
│ Cache Size (MB) │ 3.45   │
│ Cache Directory │ .dspy_cache │
│ TTL (hours)     │ 168    │
└─────────────────┴────────┘
```

### `dspy cache-clear`

Clear DSPy LLM output cache.

**Usage**:
```bash
spec-kit-3t dspy cache-clear [OPTIONS]
```

**Options**:
- `--yes, -y` - Skip confirmation prompt

**Examples**:
```bash
# Clear cache with confirmation
spec-kit-3t dspy cache-clear

# Clear cache without confirmation
spec-kit-3t dspy cache-clear --yes
```

## Configuration

### DSPy Configuration File

**Location**: `dspy_config.toml` (in ontology directory or package root)

```toml
[dspy]
# Model configuration
model_provider = "ollama"
model_name = "olmo-3:7b-instruct"
api_base = "http://localhost:11434"
api_key = ""  # Ollama doesn't require API key

# Generation parameters
temperature = 0.7
max_tokens = 2048
top_p = 0.9

# Cache configuration
enable_cache = true
cache_dir = ".dspy_cache"
cache_ttl_hours = 168  # 7 days

# Performance tuning
max_concurrent_requests = 3
request_timeout_seconds = 60
retry_attempts = 3
retry_delay_seconds = 2

[dspy.features]
# Feature flags
enable_abstract_enhancement = true
enable_section_suggestions = true
enable_diataxis_validation = true
enable_citation_suggestions = true
enable_coherence_analysis = true

# Quality thresholds
min_coherence_score = 6.0
min_diataxis_score = 7.0
max_suggestions_per_section = 5
```

### Customizing Configuration

**Edit model settings**:
```toml
[dspy]
model_name = "llama3:70b"  # Use different model
temperature = 0.5          # Lower temperature for more focused outputs
max_tokens = 4096          # Increase for longer outputs
```

**Adjust cache settings**:
```toml
[dspy]
cache_ttl_hours = 24      # Cache expires after 24 hours
cache_dir = "/tmp/.dspy"  # Use different cache location
```

**Disable features**:
```toml
[dspy.features]
enable_citation_suggestions = false  # Disable citation module
max_suggestions_per_section = 3      # Limit suggestions
```

## DSPy Modules

### 1. Abstract Enhancement

Expands thesis abstract with academic context and motivation.

**Inputs**:
- Current abstract from RDF
- Thesis title
- Research area

**Outputs**:
- Enhanced abstract (200-300 words)
- Research motivation (2-3 sentences)

**Example**:
```bash
spec-kit-3t dspy enhance --modules abstract --verbose
```

### 2. Section Suggestions

Suggests additional content for thesis sections.

**Inputs**:
- Section title
- Current content
- Chapter context

**Outputs**:
- 3-5 content suggestion bullet points
- Related concepts to explore

**Example**:
```bash
spec-kit-3t dspy suggest "chapter-1/Introduction"
```

### 3. Diataxis Validation

Validates chapter structure against Diataxis framework.

**Inputs**:
- Chapter type (tutorial, howto, reference, explanation)
- Chapter title
- Section titles

**Outputs**:
- Structure score (0-10 with reasoning)
- Improvement suggestions

**Example**:
```bash
spec-kit-3t dspy enhance --modules diataxis
```

### 4. Citation Suggestions

Suggests relevant academic citations.

**Inputs**:
- Section content
- Research area
- Year range

**Outputs**:
- Citation suggestions (3-5)
- Key academic search terms

**Example**:
```bash
spec-kit-3t dspy enhance --modules citations
```

### 5. Coherence Analysis

Analyzes coherence between adjacent chapters.

**Inputs**:
- Summary of first chapter
- Summary of second chapter

**Outputs**:
- Coherence score (0-10)
- Transition suggestions

**Example**:
```bash
spec-kit-3t dspy enhance --modules coherence
```

## Cache Management

### Cache Strategy

DSPy uses **content-based caching** with SHA-256 hashing:

```
cache_key = SHA-256(module_name + sorted(inputs))
```

**Benefits**:
- ✅ **Idempotent**: Same input → same cached output
- ✅ **Fast**: Instant retrieval from disk
- ✅ **Space-efficient**: Only unique outputs stored

### Cache Workflow

```bash
# First run - cache miss
spec-kit-3t dspy enhance --modules abstract
# LLM called, result cached

# Second run - cache hit
spec-kit-3t dspy enhance --modules abstract
# Instant retrieval from cache (no LLM call)

# Clear cache for fresh outputs
spec-kit-3t dspy cache-clear --yes

# Third run - cache miss again
spec-kit-3t dspy enhance --modules abstract
# LLM called, new result cached
```

### Cache Statistics

```bash
spec-kit-3t dspy cache-stats
```

Shows:
- Total cached entries
- Total cache size (MB)
- Cache directory location
- TTL (time-to-live) in hours

## Error Handling

### Ollama Not Running

**Symptom**:
```
❌ Enhancement failed: Failed to connect to Ollama at http://localhost:11434
```

**Solution**:
```bash
# Start Ollama server
ollama serve

# Pull model if not available
ollama pull olmo-3:7b-instruct

# Retry enhancement
spec-kit-3t dspy enhance
```

### Model Response Timeout

**Symptom**:
```
❌ ERROR: LLM request timed out after 60s
```

**Solution**:

Edit `dspy_config.toml`:
```toml
[dspy]
request_timeout_seconds = 120  # Increased from 60
```

### Cache Corruption

**Symptom**:
```
❌ Failed to load cache: JSON decode error
```

**Solution**:
```bash
# Clear corrupted cache
spec-kit-3t dspy cache-clear --yes

# Regenerate cache
spec-kit-3t dspy enhance --no-cache
```

### Import Error

**Symptom**:
```
ImportError: No module named 'dspy'
```

**Solution**:
```bash
# Install DSPy dependencies
pip install dspy-ai>=2.5.0

# Or reinstall Spec-Kit-3T
pip install -e .
```

## Performance Tips

### 1. Use Cache Aggressively

```bash
# Enable cache (default)
spec-kit-3t dspy enhance --cache

# Cache stats before/after
spec-kit-3t dspy cache-stats
```

### 2. Selective Enhancement

```bash
# Only enhance what you need
spec-kit-3t dspy enhance --modules abstract,sections

# Don't run all modules if not needed
```

### 3. Adjust Timeout

```toml
[dspy]
request_timeout_seconds = 30  # Faster for simple queries
```

### 4. Batch Operations

```bash
# Enhance multiple theses in parallel (future feature)
# Currently: Run sequentially
for dir in thesis1 thesis2 thesis3; do
  cd $dir && spec-kit-3t dspy enhance --modules abstract
done
```

## Integration with Existing Pipeline

### Constitutional Equation: μ + λ

**Deterministic Core (μ)** - ALWAYS runs:
1. μ₁: SHACL validation
2. μ₂: SPARQL extraction
3. μ₃: Tera rendering
4. μ₄: LaTeX writing
5. μ₅: Cryptographic receipts

**LLM Enhancement (λ)** - OPTIONAL:
1. λ₁: Context augmentation (DSPy Predict)
2. λ₂: Content expansion (DSPy ChainOfThought)
3. λ₃: Structural suggestions (DSPy ReAct)

### Workflow Integration

```bash
# Standard workflow (deterministic only)
spec-kit-3t generate
spec-kit-3t pdf

# Enhanced workflow (deterministic + LLM)
spec-kit-3t generate                  # μ pipeline
spec-kit-3t dspy enhance              # λ enhancement
# Review llm_suggestions.json
# Optionally update RDF ontology with suggestions
spec-kit-3t generate                  # Regenerate with updates
spec-kit-3t pdf
```

## Advanced Usage

### Custom DSPy Modules

Create custom modules by extending `dspy.Module`:

```python
# my_custom_module.py
import dspy

class MyCustomSignature(dspy.Signature):
    \"\"\"Custom signature for my use case.\"\"\"
    input_text: str = dspy.InputField(desc="Input")
    output_text: str = dspy.OutputField(desc="Output")

class MyCustomModule(dspy.Module):
    def __init__(self):
        super().__init__()
        self.process = dspy.ChainOfThought(MyCustomSignature)

    def forward(self, input_text: str):
        return self.process(input_text=input_text)
```

### Programmatic API

```python
from pathlib import Path
from dspy_modules.llm_integration import ThesisEnhancer

# Initialize enhancer
enhancer = ThesisEnhancer(
    ontology_dir=Path("."),
    use_cache=True
)

# Enhance abstract
result = enhancer.enhance_thesis(
    modules=["abstract"],
    verbose=True
)

print(result["modules"]["abstract"]["enhancement"])
```

## Troubleshooting

### Issue: "No abstract found in RDF"

**Cause**: RDF ontology missing abstract

**Solution**: Add abstract to RDF:
```turtle
:my-thesis
    thesis:hasAbstract :my-abstract .

:my-abstract
    a thesis:Abstract ;
    thesis:hasContent "This thesis presents..." .
```

### Issue: Poor suggestion quality

**Cause**: Model temperature too high or low

**Solution**: Adjust temperature in config:
```toml
[dspy]
temperature = 0.5  # Try lower for more focused outputs
```

### Issue: Slow performance

**Cause**: Network latency, model loading

**Solutions**:
1. Use cache: `--cache` (default)
2. Pull model locally: `ollama pull olmo-3:7b-instruct`
3. Use smaller model: `ollama pull tinyllama`

## Best Practices

1. **Run deterministic pipeline first**: Always `spec-kit-3t generate` before enhancement
2. **Review suggestions before applying**: LLM outputs require human judgment
3. **Use cache for iteration**: Cache speeds up repeated runs
4. **Clear cache periodically**: Prevent stale suggestions
5. **Validate enhanced content**: Run `spec-kit-3t validate` after manual edits
6. **Version control RDF**: Only RDF is source of truth, not LLM suggestions
7. **Monitor cache size**: Run `cache-stats` and `cache-clear` as needed

## Future Enhancements

Planned features for v3.0:

- **Real-time enhancement**: Stream LLM outputs as they generate
- **Multi-model support**: GPT-4, Claude, Mistral integration
- **Fine-tuning**: Train custom models on thesis corpus
- **Batch processing**: Enhance multiple theses in parallel
- **Export to RDF**: Convert LLM suggestions back to RDF triples
- **Interactive mode**: Chat-based thesis assistance
- **Quality metrics**: Automated scoring of enhancement quality

---

**Version**: 2.1.0
**Last Updated**: 2025-12-20
**DSPy Version**: >=2.5.0
**Ollama Model**: olmo-3:7b-instruct
