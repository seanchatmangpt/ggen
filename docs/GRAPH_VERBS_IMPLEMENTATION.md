<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->
**Table of Contents**

- [Graph Verbs Implementation Summary](#graph-verbs-implementation-summary)
  - [Overview](#overview)
  - [Implementation Status](#implementation-status)
    - [✅ Fully Implemented](#-fully-implemented)
  - [Command Examples](#command-examples)
    - [1. Query - SPARQL Execution](#1-query---sparql-execution)
    - [2. Validate - SHACL Validation](#2-validate---shacl-validation)
    - [3. Load - RDF Data Ingestion](#3-load---rdf-data-ingestion)
    - [4. Export - Graph Serialization](#4-export---graph-serialization)
    - [5. Diff - Semantic Comparison](#5-diff---semantic-comparison)
    - [6. Stats - Graph Analytics](#6-stats---graph-analytics)
    - [7. Snapshot - Delta-Driven Projection](#7-snapshot---delta-driven-projection)
  - [Architecture](#architecture)
    - [Core Components](#core-components)
    - [Security Features](#security-features)
    - [Testing](#testing)
  - [Core Graph Implementation](#core-graph-implementation)
  - [Dependencies](#dependencies)
  - [Usage Patterns](#usage-patterns)
    - [1. Simple Query Workflow](#1-simple-query-workflow)
    - [2. Validation Workflow](#2-validation-workflow)
    - [3. Delta Analysis Workflow](#3-delta-analysis-workflow)
  - [Future Enhancements](#future-enhancements)
  - [Integration](#integration)
  - [Summary](#summary)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

# Graph Verbs Implementation Summary

## Overview

The `ggen graph` noun has **7 verbs** for comprehensive RDF operations:

1. **query** - Execute SPARQL queries
2. **validate** - SHACL shape validation
3. **load** - Load RDF files into graph store
4. **export** - Export graphs in various formats
5. **diff** - Semantic diff between graphs
6. **stats** - Display graph statistics
7. **snapshot** - Manage graph snapshots for delta-driven projection

## Implementation Status

### ✅ Fully Implemented

All verb files exist with:
- ✅ CLI argument parsing with `clap`
- ✅ Input validation and sanitization
- ✅ Mockable trait-based architecture (London TDD)
- ✅ Comprehensive unit tests
- ✅ Security validations (path traversal, SQL injection prevention)
- ✅ Error handling with proper Result types

## Command Examples

### 1. Query - SPARQL Execution

```bash
# Execute SPARQL query
ggen graph query "SELECT ?s ?p ?o WHERE { ?s ?p ?o }" --graph data.ttl --format table

# Output formats: json, csv, table
ggen graph query "SELECT ?name WHERE { ?person :name ?name }" --format json

# Without specifying graph (uses current graph)
ggen graph query "ASK { ?s ?p ?o }" --format table
```

**Features:**
- SPARQL 1.1 query support (SELECT, ASK, CONSTRUCT, DESCRIBE)
- Multiple output formats (JSON, CSV, table)
- Query validation (prevents write operations)
- Length and syntax validation

### 2. Validate - SHACL Validation

```bash
# Validate graph against SHACL shapes
ggen graph validate shapes.ttl --graph data.ttl

# Validate current graph
ggen graph validate schema.shacl
```

**Features:**
- SHACL shape constraint validation
- Detailed violation reporting
- Severity levels (Violation, Warning, Info)
- Focus node and property tracking

### 3. Load - RDF Data Ingestion

```bash
# Load RDF file (auto-detect format from extension)
ggen graph load data.ttl

# Specify format explicitly
ggen graph load ontology.rdf --format rdfxml

# Load with base IRI for relative URIs
ggen graph load relative.ttl --base http://example.org/

# Merge with existing graph
ggen graph load additional.ttl --merge
```

**Supported Formats:**
- Turtle (.ttl)
- N-Triples (.nt)
- RDF/XML (.rdf, .xml)
- JSON-LD (.jsonld)
- N3 (.n3)

**Features:**
- Auto format detection
- Base IRI resolution
- Merge mode for incremental loading
- Load statistics reporting

### 4. Export - Graph Serialization

```bash
# Export to Turtle with pretty printing
ggen graph export output.ttl --format turtle --pretty

# Export to JSON-LD
ggen graph export data.jsonld --format jsonld

# Export to N-Triples
ggen graph export triples.nt --format ntriples

# Export to RDF/XML
ggen graph export graph.rdf --format rdfxml
```

**Features:**
- Multiple RDF serialization formats
- Pretty printing option
- Export statistics (triple count, file size)
- Secure path validation

### 5. Diff - Semantic Comparison

```bash
# Compare two RDF graphs
ggen graph diff --baseline before.ttl --current after.ttl

# Output as JSON
ggen graph diff -b before.ttl -c after.ttl --format json

# Filter by specific IRIs
ggen graph diff -b old.ttl -c new.ttl --filter http://example.org/Person1

# Show template impacts
ggen graph diff -b v1.ttl -c v2.ttl --show-templates -T template1.tmpl -T template2.tmpl
```

**Features:**
- Delta computation (additions, deletions, modifications)
- Hash-based change tracking
- IRI filtering
- Template impact analysis
- Multiple output formats (human, json, hash)

### 6. Stats - Graph Analytics

```bash
# Basic statistics
ggen graph stats --graph data.ttl

# Detailed statistics with predicate usage
ggen graph stats --detailed

# Stats for current graph
ggen graph stats
```

**Features:**
- Triple count
- Unique subjects/predicates/objects
- Namespace discovery
- Predicate usage frequency (detailed mode)

### 7. Snapshot - Delta-Driven Projection

```bash
# Snapshot commands for managing graph versions
ggen graph snapshot [subcommand]
```

**Features:**
- Graph version management
- Delta-driven projection support
- Baseline hash tracking
- Impact analysis integration

## Architecture

### Core Components

1. **CLI Layer** (`cli/src/cmds/graph/`)
   - Argument parsing with validation
   - Input sanitization
   - Security checks

2. **Business Logic** (traits with `run_with_deps`)
   - `SparqlExecutor` - Query execution
   - `ShaclValidator` - Shape validation
   - `RdfLoader` - Data loading
   - `GraphExporter` - Serialization
   - `GraphAnalyzer` - Statistics

3. **Core Implementation** (`ggen-core/src/graph.rs`)
   - Oxigraph store wrapper
   - SPARQL query caching
   - Multi-format RDF parsing
   - Thread-safe operations

### Security Features

All verbs implement:
- **Path traversal protection** - Rejects paths with `..`
- **Length validation** - Max limits on inputs
- **Format validation** - Whitelist of allowed formats
- **Character validation** - Alphanumeric + safe characters only
- **Query validation** - Prevents write operations in queries

### Testing

All verbs include:
- Unit tests with mocking (London TDD style)
- Multiple test cases per verb
- Edge case coverage
- Error condition testing

## Core Graph Implementation

**File:** `ggen-core/src/graph.rs`

**Key Features:**
- Thread-safe Oxigraph wrapper
- SPARQL query caching (LRU cache)
- Epoch-based cache invalidation
- Multiple RDF format support
- Pattern-based quad filtering

**Methods:**
- `insert_turtle()` - Load Turtle data
- `insert_quad()` - Add individual triples
- `load_path()` - Load from file
- `query()` - Execute SPARQL
- `query_cached()` - Cached query execution
- `query_with_prolog()` - Query with prefixes
- `quads_for_pattern()` - Pattern matching

## Dependencies

```toml
oxigraph = "0.4"  # RDF store and SPARQL engine
serde = "1.0"     # Serialization
serde_json = "1.0" # JSON output
mockall = "0.13"  # Testing mocks
```

## Usage Patterns

### 1. Simple Query Workflow

```bash
# Load data
ggen graph load people.ttl

# Query data
ggen graph query "SELECT ?name WHERE { ?p :name ?name }" --format table

# Export results
ggen graph export output.ttl --format turtle --pretty
```

### 2. Validation Workflow

```bash
# Load data
ggen graph load data.ttl

# Validate against shapes
ggen graph validate schema.shacl --graph data.ttl

# If valid, export
ggen graph export validated.ttl --format jsonld
```

### 3. Delta Analysis Workflow

```bash
# Compare versions
ggen graph diff --baseline v1.ttl --current v2.ttl --format json

# Analyze template impacts
ggen graph diff -b v1.ttl -c v2.ttl --show-templates -T app.tmpl

# Export if acceptable
ggen graph export v2-approved.ttl --format turtle
```

## Future Enhancements

Potential additions:
1. **Reasoning** - RDFS/OWL inference
2. **Federation** - Query multiple endpoints
3. **Streaming** - Handle large RDF files
4. **Compression** - HDT format support
5. **Visualization** - Generate graph diagrams
6. **Benchmarking** - Performance metrics

## Integration

The graph verbs integrate with:
- **Template system** - RDF data for code generation
- **Delta system** - Change tracking and projection
- **Marketplace** - Graph-based template metadata
- **Registry** - RDF package descriptors

## Summary

The graph noun provides a complete RDF operations toolkit:
- ✅ 7 verbs covering all major RDF operations
- ✅ Security-hardened with input validation
- ✅ Well-tested with London TDD approach
- ✅ Multiple format support (Turtle, N-Triples, RDF/XML, JSON-LD)
- ✅ SPARQL 1.1 query engine
- ✅ SHACL validation ready
- ✅ Delta-driven projection support

All requested features are implemented and ready for use!
