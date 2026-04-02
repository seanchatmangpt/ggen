# ggen-core

Core graph-aware code generation engine for ggen, providing RDF processing, template management, and code generation capabilities.

## Overview

ggen-core implements the five-stage μ₁-μ₅ pipeline for ontology-governed code generation:

```mermaid
flowchart LR
    ONTO["μ₁: Load Ontology<br/>RDF/Turtle processing"] -->
    QUERY["μ₂: Extract<br/>SPARQL queries"] -->
    TMPL["μ₃: Generate<br/>Tera templates"] -->
    VALID["μ₄: Validate<br/>Quality gates"] -->
    EMIT["μ₅: Emit<br/>Receipts + Artifacts"]

    style ONTO fill:#e1f5ff
    style QUERY fill:#fff4e6
    style TMPL fill:#c8e6c9
    style VALID fill:#ffcdd2
    style EMIT fill:#fce4ec
```

## Features

- RDF/SPARQL processing and querying
- Template engine with frontmatter support
- Deterministic code generation
- Marketplace and registry support
- Pipeline management for code generation workflows
- Graph delta detection and three-way merging
- Cryptographic receipt generation (Ed25519)
- SHACL validation for ontology constraints

## Module Organization (49 modules)

```mermaid
flowchart TD
    subgraph Core["Core Engine"]
        GRAPH["Graph<br/>RDF triplestore"]
        PIPELINE["Pipeline<br/>μ₁-μ₅ orchestration"]
        GEN["Generator<br/>Template rendering"]
    end

    subgraph Validation["Quality Gates"]
        SHACL["SHACL<br/>Shape validation"]
        VALIDATOR["Validator<br/>SPARQL checks"]
    end

    subgraph Advanced["Advanced Features"]
        DELTA["Delta Detection<br/>Graph changes"]
        MERGE["Three-way Merger<br/>Conflict resolution"]
        RECEIPT["Receipts<br/>Ed25519 signing"]
    end

    PIPELINE --> GRAPH
    PIPELINE --> GEN
    PIPELINE --> VALID

    VALID --> SHACL
    VALID --> VALIDATOR

    DELTA --> MERGE
    MERGE --> RECEIPT

    style Core fill:#e1f5ff
    style Validation fill:#fff4e6
    style Advanced fill:#c8e6c9
```

## Usage

This crate is primarily used internally by the main ggen binary. See the main ggen documentation for usage examples.

**Key Types:**
- `Graph` - RDF triplestore wrapper
- `Pipeline` - μ₁-μ₅ orchestration
- `Generator` - Template rendering engine
- `Receipt` - Cryptographic proof of generation
- `ThreeWayMerger` - Conflict-aware merging

## Related Crates

- `ggen-cli` - Command-line interface
- `ggen-domain` - Business logic layer
- `ggen-ontology-core` - RDF/SPARQL utilities
- `ggen-receipt` - Ed25519 receipt infrastructure

