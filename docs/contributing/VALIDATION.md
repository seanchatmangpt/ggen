<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->
**Table of Contents**

- [Documentation Validation Guide](#documentation-validation-guide)
  - [Quick Start](#quick-start)
    - [Run All Validations](#run-all-validations)
  - [Configuration with ggen.toml](#configuration-with-ggentoml)
    - [View Current Configuration](#view-current-configuration)
    - [Environment-Specific Validation](#environment-specific-validation)
  - [Validation Suites](#validation-suites)
    - [1. Quick Start Tutorial Validation](#1-quick-start-tutorial-validation)
    - [2. SPARQL Query Guide Validation](#2-sparql-query-guide-validation)
    - [3. CLI Reference Validation](#3-cli-reference-validation)
    - [4. Watch Mode Validation](#4-watch-mode-validation)
    - [5. TOML Configuration Validation](#5-toml-configuration-validation)
    - [6. Diataxis Case Study Validation](#6-diataxis-case-study-validation)
    - [7. TypeScript Detection](#7-typescript-detection)
    - [8. Internal Link Validation](#8-internal-link-validation)
  - [Running Specific Tests](#running-specific-tests)
    - [Test Individual Documents](#test-individual-documents)
    - [Test Specific Validation Aspects](#test-specific-validation-aspects)
  - [Validation Reports](#validation-reports)
    - [Main Report](#main-report)
    - [Example Report](#example-report)
  - [Continuous Integration](#continuous-integration)
    - [GitHub Actions Integration](#github-actions-integration)
    - [Pre-Commit Hook](#pre-commit-hook)
  - [Lifecycle Hooks](#lifecycle-hooks)
  - [Troubleshooting](#troubleshooting)
    - [Validation Fails](#validation-fails)
    - [ggen Not Found](#ggen-not-found)
    - [Timeout Issues](#timeout-issues)
  - [Best Practices](#best-practices)
    - [1. Validate Before Committing](#1-validate-before-committing)
    - [2. Watch Mode for Live Feedback](#2-watch-mode-for-live-feedback)
    - [3. Test Your Changes](#3-test-your-changes)
    - [4. Use Environment Variables](#4-use-environment-variables)
  - [Adding New Validations](#adding-new-validations)
    - [Step 1: Create Validation Script](#step-1-create-validation-script)
    - [Step 2: Add to Master Script](#step-2-add-to-master-script)
    - [Step 3: Test](#step-3-test)
  - [Performance Metrics](#performance-metrics)
  - [Configuration Reference](#configuration-reference)
    - [ggen.toml Validation Settings](#ggentoml-validation-settings)
  - [Related Documentation](#related-documentation)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

# Documentation Validation Guide

Complete guide for validating ggen documentation using the validation suite and ggen.toml configuration.

---

## Quick Start

### Run All Validations

```bash
# Run complete validation suite
./scripts/run-validation-suite.sh

# Or run master validation directly
./scripts/validate-docs/validate-all.sh
```

**Output**: Colored terminal output + Markdown report

---

## Configuration with ggen.toml

The project uses `ggen.toml` for configuration. This affects validation behavior.

### View Current Configuration

```bash
# Show merged configuration
ggen config show

# Show specific section
ggen config show lifecycle
```

### Environment-Specific Validation

```bash
# Development (default)
./scripts/run-validation-suite.sh

# CI environment (uses local Ollama)
GGEN_ENV=ci ./scripts/run-validation-suite.sh

# Production environment
GGEN_ENV=production ./scripts/run-validation-suite.sh
```

---

## Validation Suites

### 1. Quick Start Tutorial Validation

**What it tests**: 10-minute tutorial works end-to-end

```bash
./scripts/validate-docs/validate-quick-start.sh
```

**Tests**:
- ✅ ggen installation verified
- ✅ RDF ontology creation
- ✅ Graph loading
- ✅ SPARQL queries
- ✅ Schema extraction
- ✅ Template listing

**Duration**: ~5 seconds

---

### 2. SPARQL Query Guide Validation

**What it tests**: All SPARQL examples execute correctly

```bash
./scripts/validate-docs/validate-sparql-guide.sh
```

**Tests**:
- ✅ Basic SELECT queries
- ✅ FILTER operations
- ✅ Aggregation (COUNT, GROUP BY)
- ✅ LIMIT and DISTINCT
- ✅ Common patterns

**Duration**: ~6 seconds

---

### 3. CLI Reference Validation

**What it tests**: All documented CLI commands work

```bash
./scripts/validate-docs/validate-cli-reference.sh
```

**Tests**:
- ✅ Template commands
- ✅ Graph commands
- ✅ Ontology commands
- ✅ Project commands
- ✅ Global options

**Duration**: ~3 seconds

---

### 4. Watch Mode Validation

**What it tests**: `ggen project watch` functionality

```bash
./scripts/validate-docs/validate-watch-mode.sh
```

**Tests**:
- ✅ Watch command exists
- ✅ --path option works
- ✅ --debounce option works
- ✅ Process can start

**Duration**: ~3 seconds

---

### 5. TOML Configuration Validation

**What it tests**: All TOML configuration examples are valid

```bash
./scripts/validate-docs/validate-toml-reference.sh
```

**Tests**:
- ✅ Basic ggen.toml syntax
- ✅ AI provider configs (OpenAI, Anthropic, Ollama)
- ✅ RDF configuration
- ✅ SPARQL configuration
- ✅ Environment overrides
- ✅ gpack.toml package format
- ✅ Security configuration
- ✅ Performance tuning
- ✅ Lifecycle hooks
- ✅ Multi-language projects

**Duration**: ~2 seconds

---

### 6. Diataxis Case Study Validation

**What it tests**: Case study demonstrates proper Diataxis structure

```bash
./scripts/validate-docs/validate-case-study.sh
```

**Tests**:
- ✅ All 4 quadrants present (Tutorial, How-to, Explanation, Reference)
- ✅ Tutorial structure (numbered steps, outcomes, meta-lessons)
- ✅ How-to structure (problem, solution, troubleshooting)
- ✅ Explanation structure (concepts, trade-offs, no instructions)
- ✅ Reference structure (API, parameters, examples)
- ✅ Cross-links between quadrants
- ✅ Meta-lessons included
- ✅ JavaScript + Zod (not TypeScript)

**Duration**: ~1 second

---

### 7. TypeScript Detection

**What it tests**: Ensures documentation uses JavaScript + JSDoc (not TypeScript)

```bash
./scripts/validate-docs/check-no-typescript.sh
```

**Tests**:
- ✅ No TypeScript code blocks (```typescript)
- ✅ No TypeScript interfaces/types
- ✅ No 'import type' statements
- ✅ Type annotations use JSDoc (@typedef, @property)
- ✅ Compliance with project standard: "JavaScript + Zod + JSDoc"

**Why important**: Project requirement mandates JavaScript + JSDoc, not TypeScript

**Duration**: ~1 second

---

### 8. Internal Link Validation

**What it tests**: All internal markdown links are valid

```bash
./scripts/validate-docs/check-broken-links.sh
```

**Tests**:
- ✅ All [text](path.md) links resolve
- ✅ Relative paths are correct
- ✅ Referenced files exist
- ✅ Validation script references are accurate
- ✅ No orphaned documentation

**Why important**: Broken links create poor user experience

**Duration**: ~2 seconds

---

## Running Specific Tests

### Test Individual Documents

```bash
# Test just the quick start
./scripts/validate-docs/validate-quick-start.sh

# Test just SPARQL guide
./scripts/validate-docs/validate-sparql-guide.sh

# Test just TOML examples
./scripts/validate-docs/validate-toml-reference.sh
```

### Test Specific Validation Aspects

```bash
# Test code examples only
./scripts/validate-docs/validate-quick-start.sh 2>&1 | grep "✓.*example"

# Test links only
./scripts/validate-docs/validate-case-study.sh 2>&1 | grep "✓.*link"
```

---

## Validation Reports

### Main Report

After running `validate-all.sh`, view the report:

```bash
cat scripts/validate-docs/validation-report.md
```

**Report includes**:
- Summary table (suites passed/failed)
- Individual test results
- Recommendations for fixes
- Re-run instructions

### Example Report

```markdown
# ggen Documentation Validation Report

**Generated**: 2025-12-10 15:30:00
**Duration**: 20s

## Summary

| Metric | Count |
|--------|-------|
| **Test Suites Run** | 6 |
| **Suites Passed** | ✅ 6 |
| **Individual Tests Run** | 75 |
| **Tests Passed** | ✅ 75 |

**Overall Status**: ✅ **PASS**
```

---

## Continuous Integration

### GitHub Actions Integration

The validation suite runs automatically on:
- Every pull request
- Pushes to main branch
- Nightly builds

**Workflow file**: `.github/workflows/validate-docs.yml`

### Pre-Commit Hook

Set up automatic validation before commits:

```bash
# Add to .git/hooks/pre-commit
#!/bin/bash
./scripts/run-validation-suite.sh
```

---

## Lifecycle Hooks

The `ggen.toml` configuration enables lifecycle hooks:

```toml
[lifecycle]
enabled = true

[lifecycle.phases.pre_generate]
scripts = ["scripts/validate-docs/validate-all.sh"]

[lifecycle.phases.post_generate]
scripts = ["scripts/format-docs.sh"]
```

**This means validation runs automatically** when generating code with ggen!

---

## Troubleshooting

### Validation Fails

**Step 1**: Check which suite failed

```bash
./scripts/validate-docs/validate-all.sh 2>&1 | grep "✗"
```

**Step 2**: Run that suite individually

```bash
./scripts/validate-docs/validate-quick-start.sh
```

**Step 3**: Fix the issue

- Update documentation
- Fix code examples
- Verify links

**Step 4**: Re-run validation

```bash
./scripts/validate-docs/validate-all.sh
```

### ggen Not Found

```bash
# Install ggen
cargo install --path crates/ggen-cli

# Or use development build
GGEN_BIN="cargo run --package ggen-cli-lib --bin ggen --" ./scripts/validate-docs/validate-all.sh
```

### Timeout Issues

```bash
# Increase timeout in ggen.toml
[sparql]
timeout = 120  # 2 minutes
```

---

## Best Practices

### 1. Validate Before Committing

```bash
# Run full validation
./scripts/run-validation-suite.sh

# Only commit if validation passes
git add .
git commit -m "Update documentation"
```

### 2. Watch Mode for Live Feedback

```bash
# Auto-validate on file changes
./scripts/validate-docs/watch-docs-live.sh
```

### 3. Test Your Changes

If you:
- Add new documentation → Create validation script
- Update examples → Run relevant validation
- Change configuration → Test with different GGEN_ENV

### 4. Use Environment Variables

```bash
# Test against development build
GGEN_BIN=./target/debug/ggen ./scripts/validate-docs/validate-all.sh

# Test with different environment
GGEN_ENV=ci ./scripts/run-validation-suite.sh

# Custom report location
REPORT_FILE=/tmp/validation.md ./scripts/validate-docs/validate-all.sh
```

---

## Adding New Validations

### Step 1: Create Validation Script

```bash
cat > scripts/validate-docs/validate-my-feature.sh << 'EOF'
#!/usr/bin/env bash
set -e

# Test your feature
ggen my-feature test

echo "✓ My feature works"
EOF

chmod +x scripts/validate-docs/validate-my-feature.sh
```

### Step 2: Add to Master Script

Edit `scripts/validate-docs/validate-all.sh`:

```bash
# Add your validation
run_validation "validate-my-feature.sh" "My New Feature"
```

### Step 3: Test

```bash
# Test individually
./scripts/validate-docs/validate-my-feature.sh

# Test in suite
./scripts/validate-docs/validate-all.sh
```

---

## Performance Metrics

**Typical validation times**:

| Suite | Duration | Tests |
|-------|----------|-------|
| Quick Start | ~5s | 12 |
| SPARQL Guide | ~6s | 20 |
| CLI Reference | ~3s | 18 |
| Watch Mode | ~3s | 4 |
| TOML Reference | ~2s | 11 |
| Case Study | ~1s | 30 |
| TypeScript Detection | ~1s | 5 |
| Link Validation | ~2s | Variable |
| **Total** | **~23s** | **100+** |

**Optimization**:
- Scripts run in parallel where possible
- Use `LIMIT` in SPARQL queries
- Minimal test data
- Automatic cleanup

---

## Configuration Reference

### ggen.toml Validation Settings

```toml
# Enable lifecycle hooks for automatic validation
[lifecycle]
enabled = true

# Run validation before generation
[lifecycle.phases.pre_generate]
scripts = ["scripts/validate-docs/validate-all.sh"]

# SPARQL timeout for query validation
[sparql]
timeout = 60

# Security checks for documentation
[security]
allowed_domains = ["schema.org", "w3.org", "ggen.dev"]
max_file_size = 104857600  # 100MB
```

---

## Related Documentation

- [Writing Documentation](WRITING-DOCS.md) - How to write docs
- [ggen.toml Reference](../reference/configuration/ggen-toml-reference.md) - Configuration options
- [Diataxis Case Study](../examples/diataxis-case-study/README.md) - Documentation framework

---

**Remember**: Documentation that's tested is documentation that's trusted.

Every code example must work. Every link must resolve. Every configuration must be valid.
