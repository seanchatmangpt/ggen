<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->
**Table of Contents**

- [Performance Optimization for Code Generation](#performance-optimization-for-code-generation)
  - [Profiling Generation Performance](#profiling-generation-performance)
    - [Measure Baseline](#measure-baseline)
    - [Enable Verbose Timing](#enable-verbose-timing)
  - [Optimization Strategies](#optimization-strategies)
    - [1. Reduce RDF Graph Size](#1-reduce-rdf-graph-size)
    - [2. Use Template Caching](#2-use-template-caching)
    - [3. Parallelize Generation](#3-parallelize-generation)
    - [4. Optimize SPARQL Queries](#4-optimize-sparql-queries)
    - [5. Pre-Validate Ontologies](#5-pre-validate-ontologies)
  - [Benchmark Optimization Results](#benchmark-optimization-results)
    - [Example: E-commerce Platform](#example-e-commerce-platform)
    - [Timeline Improvements](#timeline-improvements)
  - [Memory Optimization](#memory-optimization)
    - [For Large Ontologies](#for-large-ontologies)
    - [Stream Processing](#stream-processing)
  - [CI/CD Performance](#cicd-performance)
    - [Parallel Pipeline](#parallel-pipeline)
  - [Caching Strategies](#caching-strategies)
    - [Local Development](#local-development)
    - [CI/CD Caching](#cicd-caching)
  - [Performance SLOs](#performance-slos)
  - [Troubleshooting Performance](#troubleshooting-performance)
  - [Summary](#summary)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

# Performance Optimization for Code Generation

Optimize your ggen workflows for speed and efficiency.

## Profiling Generation Performance

### Measure Baseline

```bash
# Time a generation operation
time ggen ontology generate large-schema.ttl \
  --language typescript \
  --output models.ts

# Real time should be < 5 seconds for typical ontologies
# If > 5s, optimization is needed
```

### Enable Verbose Timing

```bash
# See breakdown of operations
ggen ontology generate schema.ttl \
  --language typescript \
  --output models.ts \
  --profile

# Output:
# Parsing RDF: 0.5s
# Validation: 0.2s
# Code generation: 0.8s
# Total: 1.5s
```

## Optimization Strategies

### 1. Reduce RDF Graph Size

**Problem**: Large ontologies slow down parsing

**Solution**: Split into modules

```turtle
# Before: single 10MB ontology
# all-in-one.ttl (10MB)

# After: modular structure
@prefix base: <http://example.org/> .
@import <./entities.ttl> .
@import <./relationships.ttl> .
@import <./validations.ttl> .
```

Then generate only what you need:

```bash
ggen ontology generate entities.ttl \
  --language typescript \
  --output models.ts
```

### 2. Use Template Caching

If generating multiple languages from same ontology:

```bash
# First generation - full processing
ggen ontology generate schema.ttl \
  --language typescript \
  --output ts-models.ts

# Subsequent generations - cache enabled
ggen ontology generate schema.ttl \
  --language python \
  --output python-models.py \
  --cache

# Result: 3x faster
```

### 3. Parallelize Generation

Generate for multiple languages in parallel:

```bash
# Sequential (slow)
ggen ontology generate schema.ttl --language rust --output rust-models.rs
ggen ontology generate schema.ttl --language typescript --output ts-models.ts
ggen ontology generate schema.ttl --language python --output py-models.py

# Parallel (fast) - bash
(ggen ontology generate schema.ttl --language rust --output rust-models.rs) &
(ggen ontology generate schema.ttl --language typescript --output ts-models.ts) &
(ggen ontology generate schema.ttl --language python --output py-models.py) &
wait
```

### 4. Optimize SPARQL Queries

If using custom SPARQL queries with `ggen packs sparql`:

```sparql
# ❌ Slow: Broad query
SELECT ?class ?property WHERE {
  ?class rdf:type rdfs:Class .
  ?class ?p ?property .
}

# ✅ Fast: Specific query
SELECT ?class ?property WHERE {
  ?class rdf:type rdfs:Class .
  ?class rdfs:subClassOf|rdf:type ?property .
  FILTER(?class = <http://example.org/User>)
}
```

### 5. Pre-Validate Ontologies

Catch errors before generation:

```bash
# Validate first (fast)
ggen ontology validate schema.ttl

# Only generate if valid
if [ $? -eq 0 ]; then
  ggen ontology generate schema.ttl --language typescript --output models.ts
fi
```

## Benchmark Optimization Results

### Example: E-commerce Platform

**Before optimization**:
- 100 classes × 3 languages = 300 separate definitions
- Generation time: 45 seconds
- Memory usage: 2GB

**After optimization**:
- Single ontology × 3 languages = 300 auto-generated definitions
- Generation time: 3 seconds (15x faster)
- Memory usage: 50MB (40x less)

### Timeline Improvements

| Operation | Before | After | Improvement |
|-----------|--------|-------|-------------|
| Parse ontology | 2.1s | 0.3s | 7x faster |
| Generate Rust | 15.2s | 0.8s | 19x faster |
| Generate TypeScript | 12.5s | 0.7s | 18x faster |
| Generate Python | 18.3s | 1.2s | 15x faster |
| **Total** | **45.1s** | **2.8s** | **16x faster** |

## Memory Optimization

### For Large Ontologies

```bash
# Monitor memory usage
/usr/bin/time -v ggen ontology generate huge-schema.ttl \
  --language typescript \
  --output models.ts

# If using > 500MB, try:
# 1. Split into modules
# 2. Use --incremental flag
# 3. Generate for one language at a time
```

### Stream Processing

For very large files, use streaming:

```bash
# Process in chunks
split -l 1000 large-schema.ttl part-
for part in part-*; do
  ggen ontology generate "$part" \
    --language typescript \
    --append \
    --output models.ts
done
```

## CI/CD Performance

### Parallel Pipeline

```yaml
# GitHub Actions - parallel generation
jobs:
  generate-rust:
    runs-on: ubuntu-latest
    steps:
      - run: ggen ontology generate schema.ttl --language rust --output rust-models.rs

  generate-typescript:
    runs-on: ubuntu-latest
    steps:
      - run: ggen ontology generate schema.ttl --language typescript --output ts-models.ts

  generate-python:
    runs-on: ubuntu-latest
    steps:
      - run: ggen ontology generate schema.ttl --language python --output py-models.py
```

Result: 3 languages in ~3 seconds (parallel) vs ~10 seconds (sequential)

## Caching Strategies

### Local Development

Cache generated code:

```bash
# .gitignore
generated/
*.generated.ts
*.generated.rs
*.generated.py

# Makefile
generate:
	ggen ontology generate schema.ttl --language typescript --output models.ts
	ggen ontology generate schema.ttl --language rust --output models.rs
	ggen ontology generate schema.ttl --language python --output models.py

generate-cached:
	$(MAKE) generate 2>/dev/null || true
```

### CI/CD Caching

```yaml
name: Generate with Cache

on: push

jobs:
  generate:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v3

      - name: Cache generated files
        uses: actions/cache@v3
        with:
          path: |
            rust/src/models.rs
            typescript/src/models.ts
            python/models.py
          key: ${{ hashFiles('schema.ttl') }}

      - name: Generate if needed
        run: |
          if ! test -f models.ts; then
            make generate
          fi
```

## Performance SLOs

Set and monitor performance targets:

```bash
# Typical SLOs
- Parse RDF: < 1s
- Generate TypeScript: < 2s
- Generate Rust: < 2s
- Generate Python: < 2s
- Validate: < 0.5s
- Total pipeline: < 10s

# Monitor in CI
ggen utils doctor --performance
```

## Troubleshooting Performance

**Problem**: Generation taking > 10 seconds

**Checklist**:
1. Is ontology unnecessarily large?
   ```bash
   wc -l schema.ttl  # Should be < 5000 lines for typical projects
   ```

2. Are there circular dependencies?
   ```bash
   ggen ontology validate schema.ttl --check-cycles
   ```

3. Is system resource-constrained?
   ```bash
   top -l 1 | head -20
   ```

4. Can generation be parallelized?
   ```bash
   # Use parallel generation approach above
   ```

## Summary

You now know how to:
- ✅ Profile generation performance
- ✅ Optimize for speed (15-20x possible)
- ✅ Parallelize generation
- ✅ Reduce memory consumption
- ✅ Cache in CI/CD
- ✅ Monitor performance SLOs

Your code generation pipeline is now fast!
