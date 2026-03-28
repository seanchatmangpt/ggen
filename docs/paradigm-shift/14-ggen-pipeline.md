<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->
**Table of Contents**

- [The ggen Compilation Pipeline: μ₁-μ₅](#the-ggen-compilation-pipeline-%CE%BC%E2%82%81-%CE%BC%E2%82%85)
  - [Table of Contents](#table-of-contents)
  - [Overview](#overview)
  - [Pipeline Architecture](#pipeline-architecture)
    - [High-Level Flow](#high-level-flow)
    - [Stage Dependencies](#stage-dependencies)
  - [The Formula: A = μ(O)](#the-formula-a--%CE%BCo)
    - [Mathematical Properties](#mathematical-properties)
    - [Breaking the Formula](#breaking-the-formula)
  - [Stage μ₁: Normalize](#stage-%CE%BC%E2%82%81-normalize)
    - [Responsibilities](#responsibilities)
    - [Implementation](#implementation)
    - [Output: NormalizedData](#output-normalizeddata)
    - [Fail-Fast Examples](#fail-fast-examples)
    - [Best Practices](#best-practices)
  - [Stage μ₂: Extract](#stage-%CE%BC%E2%82%82-extract)
    - [Responsibilities](#responsibilities-1)
    - [CONSTRUCT-Only Rule](#construct-only-rule)
    - [Implementation](#implementation-1)
    - [Output: ExtractedData (Generation IR)](#output-extracteddata-generation-ir)
    - [Parallel Tensor Queries](#parallel-tensor-queries)
    - [Best Practices](#best-practices-1)
  - [Stage μ₃: Emit](#stage-%CE%BC%E2%82%83-emit)
    - [Responsibilities](#responsibilities-2)
    - [Tera as Emitter ISA](#tera-as-emitter-isa)
    - [Implementation](#implementation-2)
    - [Example Template](#example-template)
    - [Pure Fold Pattern](#pure-fold-pattern)
    - [Forbidden Operations in μ₃](#forbidden-operations-in-%CE%BC%E2%82%83)
    - [Best Practices](#best-practices-2)
  - [Stage μ₄: Canonicalize](#stage-%CE%BC%E2%82%84-canonicalize)
    - [Responsibilities](#responsibilities-3)
    - [Implementation](#implementation-3)
    - [Formatter Integration](#formatter-integration)
    - [Stop-the-Line on Formatter Failure](#stop-the-line-on-formatter-failure)
    - [Determinism Verification](#determinism-verification)
    - [Best Practices](#best-practices-3)
  - [Stage μ₅: Receipt](#stage-%CE%BC%E2%82%85-receipt)
    - [Responsibilities](#responsibilities-4)
    - [Implementation](#implementation-4)
    - [Receipt Structure](#receipt-structure)
    - [Provenance Chain](#provenance-chain)
    - [Best Practices](#best-practices-4)
  - [End-to-End Example](#end-to-end-example)
    - [Step 1: Create RDF Ontology](#step-1-create-rdf-ontology)
    - [Step 2: Run Pipeline](#step-2-run-pipeline)
    - [Step 3: μ₁ (Normalize)](#step-3-%CE%BC%E2%82%81-normalize)
    - [Step 4: μ₂ (Extract)](#step-4-%CE%BC%E2%82%82-extract)
    - [Step 5: μ₃ (Emit)](#step-5-%CE%BC%E2%82%83-emit)
    - [Step 6: μ₄ (Canonicalize)](#step-6-%CE%BC%E2%82%84-canonicalize)
    - [Step 7: μ₅ (Receipt)](#step-7-%CE%BC%E2%82%85-receipt)
    - [Step 8: Verify Receipt](#step-8-verify-receipt)
  - [Fail-Fast Principles](#fail-fast-principles)
    - [1. Parse Failures (μ₁)](#1-parse-failures-%CE%BC%E2%82%81)
    - [2. Validation Failures (μ₁)](#2-validation-failures-%CE%BC%E2%82%81)
    - [3. Query Failures (μ₂)](#3-query-failures-%CE%BC%E2%82%82)
    - [4. Template Failures (μ₃)](#4-template-failures-%CE%BC%E2%82%83)
    - [5. Formatter Failures (μ₄)](#5-formatter-failures-%CE%BC%E2%82%84)
    - [6. Receipt Verification Failures (μ₅)](#6-receipt-verification-failures-%CE%BC%E2%82%85)
  - [Forbidden Operations](#forbidden-operations)
    - [During μ₂ (Extract)](#during-%CE%BC%E2%82%82-extract)
    - [During μ₃ (Emit)](#during-%CE%BC%E2%82%83-emit)
    - [During μ₄ (Canonicalize)](#during-%CE%BC%E2%82%84-canonicalize)
    - [During μ₅ (Receipt)](#during-%CE%BC%E2%82%85-receipt)
  - [Troubleshooting](#troubleshooting)
    - [Problem: μ₁ fails with "RDF store is empty"](#problem-%CE%BC%E2%82%81-fails-with-rdf-store-is-empty)
    - [Problem: μ₂ extracts zero entities](#problem-%CE%BC%E2%82%82-extracts-zero-entities)
    - [Problem: μ₃ template rendering fails](#problem-%CE%BC%E2%82%83-template-rendering-fails)
    - [Problem: μ₄ formatter fails](#problem-%CE%BC%E2%82%84-formatter-fails)
    - [Problem: μ₅ receipt verification fails](#problem-%CE%BC%E2%82%85-receipt-verification-fails)
  - [Best Practices](#best-practices-5)
    - [1. Specification-Driven Development](#1-specification-driven-development)
    - [2. SHACL Validation](#2-shacl-validation)
    - [3. CONSTRUCT-Only Extraction](#3-construct-only-extraction)
    - [4. Template Simplicity](#4-template-simplicity)
    - [5. Always Generate Receipts](#5-always-generate-receipts)
    - [6. CI Verification](#6-ci-verification)
    - [7. Version All Inputs](#7-version-all-inputs)
    - [8. Determinism Testing](#8-determinism-testing)
  - [References](#references)
    - [Implementation Files](#implementation-files)
    - [Related Documentation](#related-documentation)
    - [Key Concepts](#key-concepts)
    - [Further Reading](#further-reading)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

# The ggen Compilation Pipeline: μ₁-μ₅

**Version**: 1.0.0
**Last Updated**: 2026-02-09
**Status**: Canonical Reference

## Table of Contents

- [Overview](#overview)
- [Pipeline Architecture](#pipeline-architecture)
- [The Formula: A = μ(O)](#the-formula-a--μo)
- [Stage μ₁: Normalize](#stage-μ₁-normalize)
- [Stage μ₂: Extract](#stage-μ₂-extract)
- [Stage μ₃: Emit](#stage-μ₃-emit)
- [Stage μ₄: Canonicalize](#stage-μ₄-canonicalize)
- [Stage μ₅: Receipt](#stage-μ₅-receipt)
- [End-to-End Example](#end-to-end-example)
- [Fail-Fast Principles](#fail-fast-principles)
- [Forbidden Operations](#forbidden-operations)
- [Troubleshooting](#troubleshooting)
- [Best Practices](#best-practices)
- [References](#references)

## Overview

ggen transforms RDF ontologies into executable code through a **five-stage deterministic pipeline**. This is not compilation in the traditional sense - it is **code precipitation from semantic specifications**.

**Key Principle**: Code does not get written; it **crystallizes from RDF** through a series of pure transformations.

```
RDF Ontology (O) → [μ₁ → μ₂ → μ₃ → μ₄ → μ₅] → Generated Code (A)
```

Each stage is:
- **Deterministic**: Same input always produces same output
- **Pure**: No side effects except stage output
- **Verifiable**: Cryptographic receipts prove correctness
- **Fail-fast**: Invalid inputs rejected immediately
- **Composable**: Output of stage N is input to stage N+1

## Pipeline Architecture

### High-Level Flow

```
┌─────────────────────────────────────────────────────────────────────┐
│                        ggen Pipeline: μ₁ → μ₅                       │
└─────────────────────────────────────────────────────────────────────┘

    Input: ontology.ttl (RDF/Turtle specification)
       │
       ▼
┌──────────────────────────────────────────────────────────────────────┐
│ μ₁ NORMALIZE: Parse → Validate → Infer → Build Graph                │
│                                                                       │
│ • Load RDF from Turtle format                                        │
│ • Validate SHACL shapes (structural constraints)                     │
│ • Materialize OWL inference rules                                    │
│ • Build normalized RDF graph                                         │
│ • Resolve entity dependencies                                        │
│ • FAIL-FAST: Reject invalid RDF immediately                          │
│                                                                       │
│ Output: Normalized Graph (Store + Entity List)                       │
└──────────────────────────────────────────────────────────────────────┘
       │
       ▼
┌──────────────────────────────────────────────────────────────────────┐
│ μ₂ EXTRACT: Query → Shape → Transform                               │
│                                                                       │
│ • Execute CONSTRUCT queries (graph-to-graph transformation)          │
│ • Extract entities, attributes, relationships                        │
│ • Build generation IR (Intermediate Representation)                  │
│ • Parallel tensor queries for performance                            │
│ • CONSTRUCT-only: No SELECT, no human interpretation                 │
│                                                                       │
│ Output: Generation IR (G′ - ExtractedData)                          │
└──────────────────────────────────────────────────────────────────────┘
       │
       ▼
┌──────────────────────────────────────────────────────────────────────┐
│ μ₃ EMIT: Render Templates → Generate Code                           │
│                                                                       │
│ • Load Tera templates (emitter ISA)                                  │
│ • Pure fold over G′ (no external state)                             │
│ • Render code for each entity                                        │
│ • FORBIDDEN: No file I/O, no HTTP, no DB queries                    │
│ • Tera as pure function: G′ → Code                                  │
│                                                                       │
│ Output: Raw Generated Code (unformatted)                             │
└──────────────────────────────────────────────────────────────────────┘
       │
       ▼
┌──────────────────────────────────────────────────────────────────────┐
│ μ₄ CANONICALIZE: Format → Verify → Hash                             │
│                                                                       │
│ • Apply language formatter (rustfmt, mix format, etc.)               │
│ • Normalize whitespace deterministically                             │
│ • Compute SHA-256 hashes of all outputs                              │
│ • STOP-THE-LINE: Formatter failures are fatal                        │
│ • Ensure bit-for-bit reproducibility                                 │
│                                                                       │
│ Output: Canonical Code + Content Hashes                              │
└──────────────────────────────────────────────────────────────────────┘
       │
       ▼
┌──────────────────────────────────────────────────────────────────────┐
│ μ₅ RECEIPT: Bind → Prove → Audit Trail                              │
│                                                                       │
│ • Hash(ontology.ttl) - input binding                                 │
│ • Hash(manifest.toml) - configuration binding                        │
│ • Hash(queries/*.sparql) - transformation binding                    │
│ • Hash(templates/*.tera) - emitter binding                           │
│ • Hash(toolchain) - rustc/mix version binding                        │
│ • Build cryptographic provenance chain                               │
│ • Generate receipt.json with all hashes                              │
│                                                                       │
│ Output: Cryptographic Receipt (Provenance Proof)                     │
└──────────────────────────────────────────────────────────────────────┘
       │
       ▼
    Output: Generated code + Receipt + Audit trail
```

### Stage Dependencies

```
μ₁ (Normalize)
  ↓
μ₂ (Extract) ← depends on normalized graph
  ↓
μ₃ (Emit) ← depends on extracted IR
  ↓
μ₄ (Canonicalize) ← depends on raw code
  ↓
μ₅ (Receipt) ← depends on all previous stages
```

**Critical Property**: Stages are **pure functions** composed sequentially. Breaking this purity breaks the proof chain.

## The Formula: A = μ(O)

The core formula of ggen is deceptively simple:

```
A = μ(O)
```

Where:
- **A** = Generated Artifact (Rust code, Elixir modules, TypeScript types, etc.)
- **μ** = Five-stage transformation pipeline (μ₁ ∘ μ₂ ∘ μ₃ ∘ μ₄ ∘ μ₅)
- **O** = RDF Ontology (semantic specification in Turtle format)

### Mathematical Properties

1. **Determinism**: ∀ O: μ(O) produces identical A
2. **Purity**: μ has no side effects (except A)
3. **Composability**: μ = μ₅ ∘ μ₄ ∘ μ₃ ∘ μ₂ ∘ μ₁
4. **Verifiability**: Receipt R proves μ(O) = A
5. **Reproducibility**: Anyone can verify μ(O) = A using R

### Breaking the Formula

The following operations **break the formula** and are **strictly forbidden**:

```rust
// ❌ FORBIDDEN: Manual modification breaks proof
A′ = manual_edit(μ(O))  // Receipt no longer proves O → A′

// ❌ FORBIDDEN: SELECT/DO regime mixing
A′ = μ(O) + human_judgment()  // Non-deterministic

// ❌ FORBIDDEN: Partial generation
A′ = μ(O_partial) + handwritten_code  // Cannot replay

// ✅ CORRECT: Pure transformation
A = μ(O)  // Provable, reproducible, verifiable
```

## Stage μ₁: Normalize

**Purpose**: Parse RDF, validate structure, materialize inferences, fail fast on invalid input.

**Location**: `/home/user/ggen/crates/ggen-craftplan/src/normalize.rs`

### Responsibilities

1. **Parse RDF**: Load Turtle (.ttl) files into Oxigraph store
2. **Validate SHACL**: Enforce structural constraints (shapes)
3. **Materialize Inference**: Apply OWL reasoning rules
4. **Build Normalized Graph**: Create canonical RDF representation
5. **Resolve Dependencies**: Order entities for generation
6. **Fail-Fast**: Reject invalid RDF immediately

### Implementation

```rust
pub struct Normalizer {
    store: Store,  // Oxigraph RDF store
}

impl Normalizer {
    pub fn new() -> Result<Self> {
        Ok(Self {
            store: Store::new()?,
        })
    }

    pub fn load_rdf(&mut self, path: &Path) -> Result<()> {
        let rdf_content = std::fs::read_to_string(path)?;

        self.store
            .load_from_reader(RdfFormat::Turtle, rdf_content.as_bytes())?;

        Ok(())
    }

    pub fn validate(&self) -> Result<usize> {
        let count = self.store.len()?;

        if count == 0 {
            return Err(CraftplanError::rdf_validation(
                "RDF store is empty - no triples loaded"
            ));
        }

        Ok(count)
    }

    pub fn resolve_dependencies(&self) -> Result<Vec<String>> {
        let query = r#"
            PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
            PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
            SELECT ?entity WHERE {
                ?entity a ?type .
                FILTER(isBlank(?entity) = false)
            }
        "#;

        let results = SparqlEvaluator::new()
            .parse_query(query)?
            .on_store(&self.store)
            .execute()?;

        // Extract entities from query results
        let mut dependencies = Vec::new();
        match results {
            QueryResults::Solutions(mut solutions) => {
                while let Some(solution) = solutions.next() {
                    if let Some(entity) = solution?.get("entity") {
                        dependencies.push(entity.to_string());
                    }
                }
            }
            _ => return Err(/* unexpected result type */),
        }

        Ok(dependencies)
    }
}
```

### Output: NormalizedData

```rust
pub struct NormalizedData {
    /// The RDF store containing validated triples
    pub store: Store,

    /// List of entity IRIs resolved during validation
    pub entities: Vec<String>,
}
```

### Fail-Fast Examples

```rust
// ❌ Empty file
let result = normalizer.load_rdf("empty.ttl");
// Error: "RDF store is empty - no triples loaded"

// ❌ Invalid Turtle syntax
let result = normalizer.load_rdf("malformed.ttl");
// Error: Parse error at line 42: Unexpected token

// ❌ SHACL constraint violation
let result = normalizer.validate();
// Error: Entity :Product violates sh:minCount constraint on :sku

// ✅ Valid RDF
let result = normalizer.load_rdf("valid.ttl");
// Ok: 147 triples loaded
```

### Best Practices

1. **Always validate before proceeding**: Don't pass invalid RDF to μ₂
2. **Use SHACL shapes**: Encode all structural constraints
3. **Materialize early**: Run inference once in μ₁, not repeatedly later
4. **Closed world assumption**: Explicitly list all entities
5. **No network I/O**: RDF should be local, not fetched during generation

## Stage μ₂: Extract

**Purpose**: Transform RDF graph into generation IR through CONSTRUCT queries.

**Location**: `/home/user/ggen/crates/ggen-craftplan/src/extract.rs`

### Responsibilities

1. **Execute CONSTRUCT Queries**: Graph-to-graph transformations
2. **Extract Entities**: Find all classes to generate
3. **Extract Attributes**: Properties and fields for each entity
4. **Extract Relationships**: Links between entities
5. **Build IR**: Create intermediate representation (G′)
6. **Parallel Queries**: Tensor operations for performance

### CONSTRUCT-Only Rule

**Critical Constraint**: μ₂ uses **CONSTRUCT queries exclusively**, never SELECT.

**Why?**

```sparql
# ❌ SELECT returns bindings - requires human interpretation
SELECT ?entity ?name ?type WHERE {
    ?entity a craft:Entity ;
            craft:name ?name ;
            craft:type ?type .
}
# Output: Bindings that need interpretation
# Non-deterministic: How do you map ?type to code?

# ✅ CONSTRUCT returns RDF - no interpretation needed
CONSTRUCT {
    ?entity a ggen:CodegenTarget ;
            ggen:className ?name ;
            ggen:rustType ?rustType .
} WHERE {
    ?entity a craft:Entity ;
            craft:name ?name ;
            craft:type ?type .

    # Deterministic mapping encoded in query
    BIND(IF(?type = xsd:string, "String",
         IF(?type = xsd:integer, "i64",
         "Unknown")) AS ?rustType)
}
# Output: RDF graph ready for code generation
# Deterministic: All decisions encoded in CONSTRUCT
```

### Implementation

```rust
pub struct Extractor {
    context: ExtractionContext,
}

impl Extractor {
    pub fn extract_entities(&mut self, store: &Store) -> Result<Vec<Entity>> {
        let query = r#"
            PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
            PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
            PREFIX craft: <http://craftplan.org/ontology/>
            SELECT ?entity ?name ?plural WHERE {
                ?entity a craft:Entity ;
                        craft:name ?name .
                OPTIONAL { ?entity craft:pluralName ?plural }
            }
        "#;

        let results = SparqlEvaluator::new()
            .parse_query(query)?
            .on_store(store)
            .execute()?;

        let mut entities = Vec::new();
        match results {
            QueryResults::Solutions(mut solutions) => {
                while let Some(solution) = solutions.next() {
                    let solution = solution?;

                    let name = solution.get("name")
                        .ok_or_else(|| CraftplanError::rdf_validation("Missing 'name' binding"))?
                        .to_string();

                    let plural = solution.get("plural")
                        .map(|p| p.to_string());

                    entities.push(Entity {
                        name,
                        plural,
                        attributes: Vec::new(),
                        relationships: Vec::new(),
                    });
                }
            }
            _ => return Err(/* unexpected result type */),
        }

        Ok(entities)
    }

    pub fn extract_attributes(
        &self, store: &Store, entity_name: &str
    ) -> Result<Vec<Attribute>> {
        let query = format!(r#"
            PREFIX craft: <http://craftplan.org/ontology/>
            SELECT ?attr_name ?type ?required ?doc WHERE {{
                ?entity craft:name "{entity_name}" ;
                        craft:hasAttribute ?attr .
                ?attr craft:name ?attr_name ;
                      craft:type ?type .
                OPTIONAL {{ ?attr craft:required ?required }}
                OPTIONAL {{ ?attr craft:documentation ?doc }}
            }}
        "#);

        // Similar extraction logic...
        todo!("Extract attributes")
    }
}
```

### Output: ExtractedData (Generation IR)

```rust
pub struct ExtractedData {
    /// All entities extracted from RDF
    pub entities: Vec<ElixirModule>,

    /// SPARQL query results for reference
    pub query_results: BTreeMap<String, Vec<serde_json::Value>>,

    /// Prefix mappings used in extraction
    pub prefixes: BTreeMap<String, String>,
}

pub struct ElixirModule {
    pub name: String,
    pub module_type: ModuleType,
    pub entity: EntityMetadata,
    pub fields: Vec<Field>,
    pub relationships: Vec<Relationship>,
    pub actions: Vec<Action>,
    pub validations: Vec<Validation>,
}
```

### Parallel Tensor Queries

For large ontologies, μ₂ executes queries in parallel:

```rust
use rayon::prelude::*;

pub fn extract_all_parallel(
    &self, store: &Store, entities: &[String]
) -> Result<Vec<ExtractedData>> {
    entities.par_iter()
        .map(|entity_iri| self.extract_entity(store, entity_iri))
        .collect()
}
```

### Best Practices

1. **CONSTRUCT-only**: Never use SELECT for code generation
2. **Closed ontology**: All type mappings must be deterministic
3. **Parallel execution**: Use Rayon for independent extractions
4. **Cache queries**: Compile SPARQL queries once, reuse many times
5. **Validate IR**: Check ExtractedData completeness before μ₃

## Stage μ₃: Emit

**Purpose**: Render code from IR using Tera templates as pure functions.

**Location**: `/home/user/ggen/crates/ggen-craftplan/src/emit.rs`

### Responsibilities

1. **Load Templates**: Initialize Tera engine with built-in templates
2. **Render Code**: Transform IR to source code
3. **Pure Fold**: No side effects, no external state
4. **Forbidden Operations**: No file I/O, HTTP, database access
5. **Deterministic Output**: Same IR always produces same code

### Tera as Emitter ISA

Think of Tera templates as **instructions for code emission**:

```
Tera Template = Instruction Set Architecture (ISA) for Code Generation

Just as:
  x86 assembly → CPU instructions → machine code

In ggen:
  Tera template → Template engine → source code
```

### Implementation

```rust
pub struct Emitter {
    tera: Tera,
    output_dir: PathBuf,
}

impl Emitter {
    pub fn new(output_dir: &Path) -> Result<Self> {
        let mut tera = Tera::default();

        // Add built-in templates
        tera.add_raw_template(
            "ash_resource.tera",
            include_str!("templates/ash_resource.tera"),
        )?;

        tera.add_raw_template(
            "context_module.tera",
            include_str!("templates/context_module.tera"),
        )?;

        tera.add_raw_template(
            "live_view.tera",
            include_str!("templates/live_view.tera"),
        )?;

        Ok(Self {
            tera,
            output_dir: output_dir.to_path_buf(),
        })
    }

    pub fn render_ash_resource(
        &self, entity: &Entity, config: &GenerationConfig
    ) -> Result<String> {
        let mut context = Context::new();

        let entity_value = serde_json::to_value(entity)?;
        context.insert("entity", &entity_value);

        let config_value = serde_json::to_value(config)?;
        context.insert("config", &config_value);

        self.tera.render("ash_resource.tera", &context)
    }
}
```

### Example Template

```elixir
{# templates/ash_resource.tera #}
defmodule {{ config.module_prefix }}.{{ entity.name | title_case }} do
  @moduledoc """
  Ash resource for {{ entity.name }}
  {% if entity.plural %}Plural: {{ entity.plural }}{% endif %}
  """
  use Ash.Resource

  attributes do
    {% for attr in entity.attributes %}
    attribute :{{ attr.name }}, :{{ attr.type_ }}{% if attr.required %}, allow_nil?: false{% endif %}
    {% if attr.doc %}
    """
    {{ attr.doc }}
    """
    {% endif %}
    {% endfor %}
  end

  relationships do
    {% for rel in entity.relationships %}
    {% if rel.cardinality == "one" %}
    has_one :{{ rel.name }}, {{ config.module_prefix }}.{{ rel.target_entity }}
    {% elif rel.cardinality == "many" %}
    has_many :{{ rel.name }}, {{ config.module_prefix }}.{{ rel.target_entity }}
    {% endif %}
    {% endfor %}
  end
end
```

### Pure Fold Pattern

μ₃ is a **pure fold** over the IR:

```rust
// ✅ CORRECT: Pure transformation
pub fn emit_all(entities: &[Entity]) -> Vec<String> {
    entities.iter()
        .map(|entity| emit_entity(entity))  // Pure function
        .collect()
}

// ❌ FORBIDDEN: Side effects during emission
pub fn emit_all_impure(entities: &[Entity]) -> Vec<String> {
    entities.iter()
        .map(|entity| {
            // ❌ HTTP request during render
            let schema = fetch_schema_from_api(entity)?;

            // ❌ Database query during render
            let examples = db.query("SELECT * FROM examples")?;

            emit_entity(entity)
        })
        .collect()
}
```

### Forbidden Operations in μ₃

The following operations are **strictly forbidden** during emission:

1. **File I/O**: No reading/writing files
2. **Network**: No HTTP requests, no DNS lookups
3. **Database**: No SQL queries, no Redis access
4. **Environment**: No reading ENV vars
5. **Randomness**: No random numbers, no UUIDs
6. **Time**: No timestamps (breaks determinism)
7. **Process**: No spawning subprocesses

**Why?** These operations break determinism and make reproduction impossible.

### Best Practices

1. **Templates are data**: Treat Tera templates as immutable data structures
2. **No logic in templates**: Move complex logic to IR construction (μ₂)
3. **Test templates independently**: Unit test each template with sample IR
4. **Version templates**: Include template hash in receipt
5. **Compile templates**: Use `include_str!` for built-in templates

## Stage μ₄: Canonicalize

**Purpose**: Apply deterministic formatting and compute content hashes.

**Location**: `/home/user/ggen/crates/ggen-craftplan/src/canonicalize.rs`

### Responsibilities

1. **Apply Formatter**: Run language-specific formatter (rustfmt, mix format)
2. **Normalize Whitespace**: Ensure consistent line endings
3. **Compute Hashes**: SHA-256 hash of each output file
4. **Stop-the-Line**: Formatter failures are fatal errors
5. **Verify Determinism**: Ensure bit-for-bit reproducibility

### Implementation

```rust
pub struct Canonicalizer;

impl Canonicalizer {
    pub fn new() -> Self {
        Self
    }

    pub fn canonicalize(&self, files: &[String]) -> Result<Vec<String>> {
        let mut hashes = Vec::new();

        for file_path in files {
            let hash = self.canonicalize_file(file_path)?;
            hashes.push(hash);
        }

        Ok(hashes)
    }

    fn canonicalize_file(&self, file_path: &str) -> Result<String> {
        // Read file
        let content = std::fs::read_to_string(file_path)?;

        // Normalize whitespace
        let normalized = self.normalize_whitespace(&content);

        // Compute hash
        let hash = self.compute_hash(&normalized);

        Ok(hash)
    }

    fn normalize_whitespace(&self, content: &str) -> String {
        content
            .lines()
            .map(|line| line.trim_end().to_string())
            .collect::<Vec<_>>()
            .join("\n")
    }

    fn compute_hash(&self, content: &str) -> String {
        let mut hasher = Sha256::new();
        hasher.update(content.as_bytes());
        format!("{:x}", hasher.finalize())
    }
}
```

### Formatter Integration

For production systems, μ₄ invokes language formatters:

```rust
// Rust
std::process::Command::new("rustfmt")
    .arg("--edition=2021")
    .arg(file_path)
    .status()?;

// Elixir
std::process::Command::new("mix")
    .arg("format")
    .arg(file_path)
    .status()?;

// TypeScript
std::process::Command::new("prettier")
    .arg("--write")
    .arg(file_path)
    .status()?;
```

### Stop-the-Line on Formatter Failure

**Critical Rule**: Formatter failures **stop the entire pipeline**.

```rust
let result = format_file(path)?;

if !result.success() {
    // 🚨 ANDON: Stop the line
    return Err(CraftplanError::FormatterFailed {
        file: path,
        exit_code: result.code(),
        stderr: result.stderr(),
    });
}

// Never proceed with unformatted code
// Never "skip" formatting errors
// Never generate receipts for malformed output
```

**Why?** Unformatted code breaks determinism. If formatter fails, the specification is wrong.

### Determinism Verification

μ₄ ensures that running the pipeline twice produces identical output:

```rust
#[test]
fn test_determinism() {
    let ontology = "test.ttl";

    // Run pipeline twice
    let output1 = run_pipeline(ontology)?;
    let output2 = run_pipeline(ontology)?;

    // Assert bit-for-bit identical
    assert_eq!(
        compute_hash(&output1),
        compute_hash(&output2),
        "Pipeline is non-deterministic!"
    );
}
```

### Best Practices

1. **Always run formatter**: Never skip μ₄
2. **Fail fast on format errors**: Don't generate receipts for malformed code
3. **Version formatters**: Include formatter version in receipt
4. **Test determinism**: CI should verify bit-for-bit reproduction
5. **Normalize line endings**: Convert CRLF to LF

## Stage μ₅: Receipt

**Purpose**: Generate cryptographic proof of transformation provenance.

**Location**: `/home/user/ggen/crates/ggen-craftplan/src/receipt.rs`

### Responsibilities

1. **Hash Input**: Bind to input ontology
2. **Hash Manifest**: Bind to configuration
3. **Hash Queries**: Bind to SPARQL transformations
4. **Hash Templates**: Bind to Tera emitters
5. **Hash Toolchain**: Bind to compiler/formatter versions
6. **Build Provenance Chain**: Link all hashes
7. **Generate Receipt**: Output JSON with all proofs

### Implementation

```rust
pub struct ReceiptGenerator;

impl ReceiptGenerator {
    pub fn new() -> Self {
        Self
    }

    pub fn generate(
        &self,
        input_path: &str,
        output_files: &[String],
        entity_count: usize,
        duration_ms: u64
    ) -> Result<GenerationReceipt> {
        // Compute input hash
        let input_hash = self.hash_file(input_path)?;

        // Compute output hashes
        let mut output_hashes = BTreeMap::new();
        for file_path in output_files {
            let hash = self.hash_file(file_path)?;
            output_hashes.insert(file_path.clone(), hash);
        }

        // Build metadata
        let metadata = ReceiptMetadata {
            timestamp: chrono::Utc::now().to_rfc3339(),
            generator_version: env!("CARGO_PKG_VERSION").to_string(),
            entity_count,
            file_count: output_files.len(),
            duration_ms,
            stages: vec![
                "μ₁ (Normalize)".to_string(),
                "μ₂ (Extract)".to_string(),
                "μ₃ (Emit)".to_string(),
                "μ₄ (Canonicalize)".to_string(),
                "μ₅ (Receipt)".to_string(),
            ],
        };

        // Compute receipt hash
        let receipt_json = serde_json::to_string_pretty(&metadata)?;
        let receipt_hash = self.compute_hash(&receipt_json);

        Ok(GenerationReceipt {
            input_hash,
            output_hashes,
            metadata,
            receipt_hash,
        })
    }

    fn hash_file(&self, file_path: &str) -> Result<String> {
        let content = std::fs::read(file_path)?;
        Ok(self.compute_hash_bytes(&content))
    }

    fn compute_hash_bytes(&self, bytes: &[u8]) -> String {
        let mut hasher = Sha256::new();
        hasher.update(bytes);
        format!("{:x}", hasher.finalize())
    }
}
```

### Receipt Structure

```json
{
  "input_hash": "7f3a9b2c...",
  "output_hashes": {
    "crates/core/src/user.rs": "4d2e1a9f...",
    "crates/core/src/order.rs": "8b1c5f2a...",
    "crates/core/tests/user_test.rs": "3e7d9c4b..."
  },
  "metadata": {
    "timestamp": "2026-02-09T15:42:00Z",
    "generator_version": "6.0.0",
    "entity_count": 12,
    "file_count": 24,
    "duration_ms": 847,
    "stages": [
      "μ₁ (Normalize)",
      "μ₂ (Extract)",
      "μ₃ (Emit)",
      "μ₄ (Canonicalize)",
      "μ₅ (Receipt)"
    ]
  },
  "receipt_hash": "9a4e3d7c..."
}
```

### Provenance Chain

The receipt creates a **cryptographic provenance chain**:

```
Ontology Hash (O)
    ↓
Normalized Graph Hash (G)
    ↓
Extracted IR Hash (G′)
    ↓
Generated Code Hash (A)
    ↓
Receipt Hash (R)
```

**Verification**: Anyone with O and R can verify:

```rust
pub fn verify_receipt(
    ontology_path: &str,
    receipt: &GenerationReceipt,
    output_files: &[String]
) -> Result<bool> {
    // Verify input hash
    let current_input_hash = hash_file(ontology_path)?;
    if current_input_hash != receipt.input_hash {
        return Ok(false);
    }

    // Verify output hashes
    for (file_path, expected_hash) in &receipt.output_hashes {
        let current_hash = hash_file(file_path)?;
        if current_hash != expected_hash {
            return Ok(false);
        }
    }

    Ok(true)
}
```

### Best Practices

1. **Always generate receipts**: Never skip μ₅
2. **Store receipts separately**: `.ggen/receipts/` directory
3. **Include receipts in version control**: Track generation history
4. **Verify on CI**: Fail builds if receipt verification fails
5. **Bind all inputs**: Hash everything that affects output

## End-to-End Example

Let's walk through the complete pipeline with a real example.

### Step 1: Create RDF Ontology

```turtle
# product.ttl - Craftplan ERP Product Entity
@prefix craft: <http://craftplan.org/ontology/> .
@prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .
@prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .
@prefix xsd: <http://www.w3.org/2001/XMLSchema#> .
@prefix owl: <http://www.w3.org/2002/07/owl#> .

# Entity Definition
craft:Product a owl:Class ;
    rdfs:label "Product" ;
    rdfs:comment "A product in the catalog system" ;
    craft:namespace "Catalog" ;
    craft:pluralName "Products" .

# Attributes
craft:Product craft:hasAttribute craft:ProductSKU .
craft:ProductSKU a craft:Attribute ;
    craft:name "sku" ;
    craft:type xsd:string ;
    craft:required true ;
    craft:unique true ;
    craft:documentation "Stock Keeping Unit - unique product identifier" .

craft:Product craft:hasAttribute craft:ProductName .
craft:ProductName a craft:Attribute ;
    craft:name "name" ;
    craft:type xsd:string ;
    craft:required true ;
    craft:documentation "Human-readable product name" .

craft:Product craft:hasAttribute craft:ProductPrice .
craft:ProductPrice a craft:Attribute ;
    craft:name "price" ;
    craft:type craft:Decimal ;
    craft:required true ;
    craft:documentation "Unit price in base currency" .

craft:Product craft:hasAttribute craft:ProductQuantity .
craft:ProductQuantity a craft:Attribute ;
    craft:name "quantity_on_hand" ;
    craft:type xsd:integer ;
    craft:required false ;
    craft:default 0 ;
    craft:documentation "Current inventory quantity" .

# Relationships
craft:Product craft:hasRelationship craft:ProductCategory .
craft:ProductCategory a craft:Relationship ;
    craft:name "category" ;
    craft:target craft:Category ;
    craft:cardinality "one" ;
    craft:required true .

craft:Product craft:hasRelationship craft:ProductOrderItems .
craft:ProductOrderItems a craft:Relationship ;
    craft:name "order_items" ;
    craft:target craft:OrderItem ;
    craft:cardinality "many" ;
    craft:required false ;
    craft:inverse_of "product" .
```

### Step 2: Run Pipeline

```bash
$ ggen sync --input product.ttl --output crates/catalog/
```

### Step 3: μ₁ (Normalize)

```
[μ₁ Normalize] Loading RDF from: product.ttl
[μ₁ Normalize] Parsing Turtle format...
[μ₁ Normalize] ✓ Loaded 24 triples
[μ₁ Normalize] Validating SHACL shapes...
[μ₁ Normalize] ✓ All constraints satisfied
[μ₁ Normalize] Resolving entity dependencies...
[μ₁ Normalize] ✓ Found 1 entity: craft:Product
[μ₁ Normalize] Normalized graph ready
```

**Output**:
- Store with 24 triples
- Entity list: `["http://craftplan.org/ontology/Product"]`

### Step 4: μ₂ (Extract)

```
[μ₂ Extract] Extracting entities from normalized graph...
[μ₂ Extract] Query: entities.sparql
[μ₂ Extract] ✓ Found entity: Product
[μ₂ Extract] Query: attributes.sparql
[μ₂ Extract] ✓ Extracted 4 attributes
[μ₂ Extract] Query: relationships.sparql
[μ₂ Extract] ✓ Extracted 2 relationships
[μ₂ Extract] Building generation IR...
[μ₂ Extract] ✓ IR ready for emission
```

**Output** (Generation IR):

```rust
ExtractedData {
    entities: vec![
        ElixirModule {
            name: "Craftplan.Catalog.Product",
            module_type: ModuleType::AshResource,
            entity: EntityMetadata {
                iri: "http://craftplan.org/ontology/Product",
                local_name: "Product",
                description: Some("A product in the catalog system"),
                class_type: "owl:Class",
                namespace: "Catalog",
            },
            fields: vec![
                Field {
                    name: "sku",
                    field_type: ElixirType::String,
                    required: true,
                    unique: true,
                    primary_key: false,
                    default: None,
                    label: Some("SKU"),
                    help_text: Some("Stock Keeping Unit - unique product identifier"),
                },
                Field {
                    name: "name",
                    field_type: ElixirType::String,
                    required: true,
                    unique: false,
                    // ...
                },
                Field {
                    name: "price",
                    field_type: ElixirType::Custom("Decimal".to_string()),
                    required: true,
                    // ...
                },
                Field {
                    name: "quantity_on_hand",
                    field_type: ElixirType::Integer,
                    required: false,
                    default: Some("0"),
                    // ...
                },
            ],
            relationships: vec![
                Relationship {
                    name: "category",
                    target_iri: "http://craftplan.org/ontology/Category",
                    cardinality: Cardinality::BelongsTo,
                    required: true,
                    foreign_key: Some("category_id"),
                    // ...
                },
                Relationship {
                    name: "order_items",
                    target_iri: "http://craftplan.org/ontology/OrderItem",
                    cardinality: Cardinality::OneToMany,
                    required: false,
                    // ...
                },
            ],
            actions: vec![
                Action {
                    name: "create",
                    action_type: ActionType::Create,
                    primary: true,
                    // ...
                },
            ],
            validations: vec![
                Validation {
                    validation_type: ValidationType::Present,
                    field: Some("sku"),
                    // ...
                },
            ],
        }
    ],
    query_results: /* ... */,
    prefixes: /* ... */,
}
```

### Step 5: μ₃ (Emit)

```
[μ₃ Emit] Loading Tera templates...
[μ₃ Emit] ✓ Loaded 3 templates
[μ₃ Emit] Rendering: Craftplan.Catalog.Product
[μ₃ Emit] Template: ash_resource.tera
[μ₃ Emit] ✓ Generated: crates/catalog/src/product.ex
[μ₃ Emit] Template: context_module.tera
[μ₃ Emit] ✓ Generated: crates/catalog/src/catalog.ex
[μ₃ Emit] Template: live_view.tera
[μ₃ Emit] ✓ Generated: crates/catalog/lib/web/product_live.ex
```

**Output** (Generated Elixir):

```elixir
# crates/catalog/src/product.ex
defmodule Craftplan.Catalog.Product do
  @moduledoc """
  Ash resource for Product
  Plural: Products

  A product in the catalog system
  """
  use Ash.Resource,
    data_layer: AshPostgres.DataLayer

  postgres do
    table "products"
    repo Craftplan.Repo
  end

  attributes do
    uuid_primary_key :id

    attribute :sku, :string do
      allow_nil? false
    end
    """
    Stock Keeping Unit - unique product identifier
    """

    attribute :name, :string do
      allow_nil? false
    end
    """
    Human-readable product name
    """

    attribute :price, :decimal do
      allow_nil? false
    end
    """
    Unit price in base currency
    """

    attribute :quantity_on_hand, :integer do
      allow_nil? true
      default 0
    end
    """
    Current inventory quantity
    """

    timestamps()
  end

  relationships do
    belongs_to :category, Craftplan.Catalog.Category do
      allow_nil? false
    end

    has_many :order_items, Craftplan.Orders.OrderItem
  end

  validations do
    validate present(:sku)
    validate present(:name)
    validate present(:price)
  end

  actions do
    defaults [:create, :read, :update, :destroy]
  end
end
```

### Step 6: μ₄ (Canonicalize)

```
[μ₄ Canonicalize] Applying Elixir formatter...
[μ₄ Canonicalize] Running: mix format crates/catalog/src/product.ex
[μ₄ Canonicalize] ✓ Formatted successfully
[μ₄ Canonicalize] Computing content hashes...
[μ₄ Canonicalize] ✓ product.ex: 4d2e1a9f3c7b8d5e...
[μ₄ Canonicalize] ✓ catalog.ex: 8b1c5f2a6d9e3a7c...
[μ₄ Canonicalize] ✓ product_live.ex: 3e7d9c4b2f8a1e5d...
```

**Output**:
- Formatted code files
- SHA-256 hashes for each file

### Step 7: μ₅ (Receipt)

```
[μ₅ Receipt] Generating cryptographic receipt...
[μ₅ Receipt] Input hash: 7f3a9b2c1e5d4a8b...
[μ₅ Receipt] Output hashes: 3 files
[μ₅ Receipt] Metadata: 1 entities, 3 files, 847ms
[μ₅ Receipt] Receipt hash: 9a4e3d7c2b6f1a5e...
[μ₅ Receipt] ✓ Written to: .ggen/receipts/product-2026-02-09.json
```

**Output** (Receipt JSON):

```json
{
  "input_hash": "7f3a9b2c1e5d4a8b9c3f2e6d5a1b7c4e8f9a2b3c4d5e6f7a8b9c0d1e2f3a4b5",
  "output_hashes": {
    "crates/catalog/src/product.ex": "4d2e1a9f3c7b8d5e...",
    "crates/catalog/src/catalog.ex": "8b1c5f2a6d9e3a7c...",
    "crates/catalog/lib/web/product_live.ex": "3e7d9c4b2f8a1e5d..."
  },
  "metadata": {
    "timestamp": "2026-02-09T15:42:00Z",
    "generator_version": "6.0.0",
    "entity_count": 1,
    "file_count": 3,
    "duration_ms": 847,
    "stages": [
      "μ₁ (Normalize)",
      "μ₂ (Extract)",
      "μ₃ (Emit)",
      "μ₄ (Canonicalize)",
      "μ₅ (Receipt)"
    ]
  },
  "receipt_hash": "9a4e3d7c2b6f1a5e..."
}
```

### Step 8: Verify Receipt

```bash
$ ggen verify --receipt .ggen/receipts/product-2026-02-09.json

[Verify] Loading receipt...
[Verify] Verifying input hash...
[Verify] ✓ Input hash matches
[Verify] Verifying output hashes...
[Verify] ✓ product.ex hash matches
[Verify] ✓ catalog.ex hash matches
[Verify] ✓ product_live.ex hash matches
[Verify] ✓ Receipt verification successful
[Verify] All proofs valid - generation is reproducible
```

## Fail-Fast Principles

ggen follows **fail-fast principles** throughout the pipeline:

### 1. Parse Failures (μ₁)

```rust
// ❌ Invalid Turtle syntax
let result = normalizer.load_rdf("malformed.ttl");
// Error: Parse error at line 42: Unexpected token '@'
// Pipeline STOPS - do not proceed to μ₂
```

### 2. Validation Failures (μ₁)

```rust
// ❌ SHACL constraint violation
let result = normalizer.validate();
// Error: Entity :Product violates sh:minCount constraint on :sku
// Pipeline STOPS - specification is incomplete
```

### 3. Query Failures (μ₂)

```rust
// ❌ SPARQL query error
let result = extractor.extract_entities(store);
// Error: SPARQL syntax error at line 7: Expected WHERE clause
// Pipeline STOPS - query is malformed
```

### 4. Template Failures (μ₃)

```rust
// ❌ Tera template error
let result = emitter.render_ash_resource(entity, config);
// Error: Template error: Variable 'entity.unknown_field' not found
// Pipeline STOPS - template is invalid
```

### 5. Formatter Failures (μ₄)

```rust
// ❌ Formatter crashed
let result = canonicalizer.format_file("output.ex");
// Error: mix format exited with code 1
// Pipeline STOPS - generated code is malformed
```

### 6. Receipt Verification Failures (μ₅)

```rust
// ❌ Hash mismatch
let result = receipt_gen.verify(receipt, input, outputs);
// Error: Output hash mismatch for product.ex
// Pipeline STOPS - reproducibility violated
```

**Philosophy**: **Never proceed past an error**. Every failure is a signal that the specification is incomplete or incorrect.

## Forbidden Operations

The following operations are **strictly forbidden** in the pipeline:

### During μ₂ (Extract)

```rust
// ❌ FORBIDDEN: SELECT queries (non-deterministic interpretation)
let query = "SELECT ?entity ?type WHERE { ?entity a ?type }";

// ❌ FORBIDDEN: External data sources during extraction
let schema = http_client.get("https://schema.org/Product")?;

// ❌ FORBIDDEN: Runtime configuration
let target_lang = std::env::var("TARGET_LANGUAGE")?;
```

### During μ₃ (Emit)

```rust
// ❌ FORBIDDEN: File I/O during template rendering
{% for file in read_dir("examples/") %}
  {{ include_file(file) }}
{% endfor %}

// ❌ FORBIDDEN: HTTP requests during rendering
{% set schema = fetch("https://api.example.com/schema") %}

// ❌ FORBIDDEN: Database queries during rendering
{% set examples = query_db("SELECT * FROM examples") %}

// ❌ FORBIDDEN: Random values
{% set id = uuid() %}  # Non-deterministic!

// ❌ FORBIDDEN: Timestamps
{% set now = current_time() %}  # Breaks reproducibility!
```

### During μ₄ (Canonicalize)

```rust
// ❌ FORBIDDEN: Skipping formatter failures
if format_result.is_err() {
    log::warn!("Formatter failed, skipping...");
    // ❌ NEVER do this! Stop the pipeline!
}

// ❌ FORBIDDEN: Manual formatting
let formatted = content.replace("  ", " ");  # Non-standard

// ❌ FORBIDDEN: Conditional formatting
if cfg!(debug_assertions) {
    // Format in debug mode only ❌
}
```

### During μ₅ (Receipt)

```rust
// ❌ FORBIDDEN: Excluding files from receipt
if file.ends_with("_test.ex") {
    // Skip test files ❌
}

// ❌ FORBIDDEN: Partial receipts
if hash_computation_failed {
    return Ok(Receipt { partial: true }); // ❌
}

// ❌ FORBIDDEN: Receipt without verification
let receipt = generate_receipt_without_hashing(); // ❌
```

## Troubleshooting

### Problem: μ₁ fails with "RDF store is empty"

**Symptoms**:
```
Error: RDF store is empty - no triples loaded
```

**Causes**:
1. Empty Turtle file
2. Malformed RDF syntax
3. Wrong file encoding

**Solutions**:
```bash
# Check file is not empty
$ wc -l ontology.ttl
0 ontology.ttl  # ❌ Problem!

# Validate Turtle syntax
$ rapper -i turtle -o ntriples ontology.ttl
# Look for parse errors

# Check encoding
$ file ontology.ttl
ontology.ttl: UTF-8 Unicode text  # ✅ Correct
```

### Problem: μ₂ extracts zero entities

**Symptoms**:
```
[μ₂ Extract] ✓ Found 0 entities
```

**Causes**:
1. Query doesn't match ontology structure
2. Wrong prefix bindings
3. Missing `rdf:type` declarations

**Solutions**:
```turtle
# Ensure entities have rdf:type
craft:Product a owl:Class .  # ✅

# Check prefix definitions
@prefix craft: <http://craftplan.org/ontology/> .  # ✅

# Test query independently
$ sparql --data ontology.ttl --query extract_entities.sparql
```

### Problem: μ₃ template rendering fails

**Symptoms**:
```
Error: Template error: Variable 'entity.unknown_field' not found
```

**Causes**:
1. Template references field not in IR
2. Typo in template variable name
3. IR structure mismatch

**Solutions**:
```rust
// Debug: Print IR before rendering
eprintln!("IR: {:#?}", extracted_data);

// Check template variable names
{{ entity.name }}  # ✅
{{ entity.nam }}   # ❌ Typo

// Validate IR completeness
assert!(!entity.attributes.is_empty(), "No attributes extracted");
```

### Problem: μ₄ formatter fails

**Symptoms**:
```
Error: mix format exited with code 1
```

**Causes**:
1. Generated code has syntax errors
2. Formatter not installed
3. Formatter version mismatch

**Solutions**:
```bash
# Check formatter is installed
$ which mix
/usr/local/bin/mix  # ✅

# Run formatter manually to see error
$ mix format crates/catalog/src/product.ex
# Look for syntax error details

# Check formatter version
$ mix --version
Erlang/OTP 25 [erts-13.2]
Elixir 1.14.0  # ✅ Compatible
```

### Problem: μ₅ receipt verification fails

**Symptoms**:
```
Error: Output hash mismatch for product.ex
Expected: 4d2e1a9f...
Got:      8b3c2e5a...
```

**Causes**:
1. File was manually edited after generation
2. Non-deterministic template rendering
3. Formatter produced different output

**Solutions**:
```bash
# Check if file was modified
$ git diff crates/catalog/src/product.ex

# Re-generate without manual edits
$ ggen sync --input product.ttl --force

# Verify determinism
$ ggen sync && ggen sync
# Both runs should produce identical receipts
```

## Best Practices

### 1. Specification-Driven Development

```turtle
# ✅ GOOD: Complete specification upfront
craft:Product a owl:Class ;
    craft:hasAttribute craft:SKU ;
    craft:hasAttribute craft:Name ;
    craft:hasRelationship craft:Category .

# ❌ BAD: Partial specification, plan to "fix later"
craft:Product a owl:Class .
# TODO: Add attributes later
```

### 2. SHACL Validation

```turtle
# ✅ GOOD: Encode all constraints in SHACL
craft:ProductShape a sh:NodeShape ;
    sh:targetClass craft:Product ;
    sh:property [
        sh:path craft:sku ;
        sh:minCount 1 ;
        sh:maxCount 1 ;
        sh:datatype xsd:string ;
    ] .

# ❌ BAD: Rely on manual validation
# "We'll check SKU uniqueness in tests"
```

### 3. CONSTRUCT-Only Extraction

```sparql
# ✅ GOOD: CONSTRUCT query (deterministic)
CONSTRUCT {
    ?entity a ggen:RustStruct ;
            ggen:name ?name ;
            ggen:field ?field .
} WHERE {
    ?entity a craft:Entity ;
            craft:name ?name ;
            craft:hasAttribute ?field .
}

# ❌ BAD: SELECT query (requires interpretation)
SELECT ?entity ?name ?field WHERE {
    ?entity a craft:Entity ;
            craft:name ?name ;
            craft:hasAttribute ?field .
}
# How do you map ?field to Rust types? Human judgment required!
```

### 4. Template Simplicity

```elixir
{# ✅ GOOD: Simple template, logic in IR #}
defmodule {{ config.module_prefix }}.{{ entity.name }} do
  {% for field in entity.fields %}
  attribute :{{ field.name }}, :{{ field.type }}
  {% endfor %}
end

{# ❌ BAD: Complex logic in template #}
defmodule {{ config.module_prefix }}.{{ entity.name }} do
  {% for field in entity.fields %}
    {% if field.type == "string" %}
      {% set elixir_type = ":string" %}
    {% elif field.type == "integer" %}
      {% set elixir_type = ":integer" %}
    {% else %}
      {# Error: Unknown type! Should be in IR! #}
    {% endif %}
  attribute :{{ field.name }}, {{ elixir_type }}
  {% endfor %}
end
```

### 5. Always Generate Receipts

```bash
# ✅ GOOD: Generate and commit receipts
$ ggen sync --audit true
$ git add .ggen/receipts/
$ git commit -m "feat: Add Product entity [Receipt: 9a4e3d7c]"

# ❌ BAD: Skip receipts
$ ggen sync --no-receipt  # ❌ Never do this!
```

### 6. CI Verification

```yaml
# ✅ GOOD: Verify receipts in CI
- name: Verify ggen reproducibility
  run: |
    ggen sync --input .specify/*.ttl
    ggen verify --all-receipts
    if [ $? -ne 0 ]; then
      echo "Receipt verification failed!"
      exit 1
    fi

# ❌ BAD: Trust without verification
- name: Run tests
  run: cargo test
  # No receipt verification ❌
```

### 7. Version All Inputs

```toml
# Cargo.toml
[dependencies]
ggen-craftplan = "6.0.0"

# Lock formatter versions
[build-dependencies]
rustfmt-wrapper = "1.5.1"

# ✅ Receipt binds to these versions
```

### 8. Determinism Testing

```rust
#[test]
fn test_pipeline_determinism() {
    // Arrange: Same input
    let ontology = "test.ttl";

    // Act: Run pipeline 10 times
    let receipts: Vec<Receipt> = (0..10)
        .map(|_| run_pipeline(ontology).unwrap())
        .collect();

    // Assert: All receipts identical
    let first_hash = receipts[0].receipt_hash.clone();
    for receipt in &receipts {
        assert_eq!(receipt.receipt_hash, first_hash,
            "Pipeline is non-deterministic!");
    }
}
```

## References

### Implementation Files

- **Pipeline Orchestrator**: `/home/user/ggen/crates/ggen-craftplan/src/pipeline.rs`
- **μ₁ Normalize**: `/home/user/ggen/crates/ggen-craftplan/src/normalize.rs`
- **μ₂ Extract**: `/home/user/ggen/crates/ggen-craftplan/src/extract.rs`
- **μ₃ Emit**: `/home/user/ggen/crates/ggen-craftplan/src/emit.rs`
- **μ₄ Canonicalize**: `/home/user/ggen/crates/ggen-craftplan/src/canonicalize.rs`
- **μ₅ Receipt**: `/home/user/ggen/crates/ggen-craftplan/src/receipt.rs`
- **Domain Models**: `/home/user/ggen/crates/ggen-craftplan/src/models.rs`
- **Integration Tests**: `/home/user/ggen/crates/ggen-craftplan/tests/integration_test.rs`

### Related Documentation

- [01-regime-split.md](./01-regime-split.md) - SELECT/DO vs CONSTRUCT regimes
- [03-scm-vs-ccm.md](./03-scm-vs-ccm.md) - Configuration Management for Code
- [04-no-moving-parts.md](./04-no-moving-parts.md) - Eliminating runtime discretion
- [CLAUDE.md](/home/user/ggen/CLAUDE.md) - Project rules and conventions
- [.claude/rules/](./.claude/rules/) - Development rules and workflow

### Key Concepts

- **Determinism**: Same input always produces same output
- **CONSTRUCT Regime**: Closed ontology, no human interpretation
- **Fail-Fast**: Reject invalid inputs immediately
- **Pure Functions**: No side effects except declared output
- **Cryptographic Receipts**: Provable transformation chain
- **Reproducibility**: Anyone can verify A = μ(O)

### Further Reading

- **Oxigraph Documentation**: https://github.com/oxigraph/oxigraph
- **SPARQL 1.1 CONSTRUCT**: https://www.w3.org/TR/sparql11-query/#construct
- **SHACL Specification**: https://www.w3.org/TR/shacl/
- **Tera Templates**: https://tera.netlify.app/
- **SHA-256**: https://en.wikipedia.org/wiki/SHA-2

---

**Document Hash**: `sha256:TBD`
**Generated**: 2026-02-09
**Version**: 1.0.0
**Regime**: CONSTRUCT (this document is specification-driven)
**Maintainer**: ggen Development Team
