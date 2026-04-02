<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->
**Table of Contents**

- [Explanation: Ontology System Architecture](#explanation-ontology-system-architecture)
  - [System Overview](#system-overview)
  - [Design Principles](#design-principles)
    - [1. Separation of Concerns](#1-separation-of-concerns)
    - [2. Template-Driven Generation](#2-template-driven-generation)
    - [3. Ontologies as First-Class Packs](#3-ontologies-as-first-class-packs)
    - [4. Composition Over Hardcoding](#4-composition-over-hardcoding)
  - [Dataflow: End-to-End](#dataflow-end-to-end)
    - [Example: Generate TypeScript Types from Schema.org](#example-generate-typescript-types-from-schemaorg)
  - [Key Components](#key-components)
    - [OntologySchema (Core Data Structure)](#ontologyschema-core-data-structure)
    - [OntologyOperation Trait](#ontologyoperation-trait)
  - [Composition Example](#composition-example)
  - [Scalability Characteristics](#scalability-characteristics)
  - [Future Extensions](#future-extensions)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

# Explanation: Ontology System Architecture

How the ontology-as-packs system is designed and why.

## System Overview

```
┌─────────────────────────────────────────────────────────────┐
│                    Marketplace Registry                       │
│  (Discover & Download Ontology Packs like npm packages)      │
└────────────┬────────────────────────────────────────────────┘
             │
             ├──→ schema-org (788 classes)
             ├──→ dublin-core (35 classes)
             ├──→ foaf (35 classes)
             └──→ custom-ecommerce (50 classes)
             
                    ↓↓↓ Download & Extract
             
┌─────────────────────────────────────────────────────────────┐
│              Domain Layer (ggen-domain)                       │
│  ┌────────────────────────────────────────────────────────┐  │
│  │ Input → Business Logic → Output                        │  │
│  │ • extract:   Load RDF/OWL → Extract classes/props     │  │
│  │ • generate:  Schema + Templates → Code files          │  │
│  │ • validate:  Check ontology structure                 │  │
│  │ • compose:   Merge multiple ontologies                │  │
│  │ • discover:  Search marketplace                       │  │
│  └────────────────────────────────────────────────────────┘  │
└────────────────────────────────────────────────────────────┘
             
                    ↓↓↓ Coordinate
             
┌─────────────────────────────────────────────────────────────┐
│              Core Layer (ggen-core)                          │
│  • Graph: RDF/OWL parsing, SPARQL queries               │
│  • Template: Tera template engine                       │
│  • Registry: Pack discovery and resolution              │
│  • Pipeline: Template processing coordination           │
└─────────────────────────────────────────────────────────────┘
             
                    ↓↓↓ Produce
             
         TypeScript | Rust | Python | Go | ...
             Generated Code (Types + Validators)
```

## Design Principles

### 1. Separation of Concerns

**Domain Layer** (`ggen-domain`):
- Pure business logic
- No UI, no CLI details
- Testable, reusable functions
- Each operation is an `Input → Output` transformation

**Core Layer** (`ggen-core`):
- Infrastructure primitives
- Graph management, template rendering
- Doesn't know about ontologies specifically
- Provides building blocks for domains

**CLI Layer** (`ggen-cli`):
- Thin user interaction wrapper
- Calls domain layer functions
- Handles user input/output formatting
- No business logic

### 2. Template-Driven Generation

Rather than hardcoding Python/Rust/TypeScript generation:

```rust
// ✗ Wrong: Hardcode each language
if language == "typescript" {
    generate_typescript_types()
} else if language == "rust" {
    generate_rust_types()
} else if language == "python" {
    generate_python_types()
}

// ✓ Right: Use templates
render_template(
    "templates/{{ language }}/types.tera",
    &ontology_schema
)
```

**Benefits**:
- Add new language without touching Rust code
- Package provides templates, not logic
- Community can contribute templates
- Template changes don't require recompilation

### 3. Ontologies as First-Class Packs

Ontologies integrate with existing gpack infrastructure:

```toml
# gpack.toml (existing structure)
[package]
name = "schema-org"
version = "3.13.0"

# Ontology-specific section
[ontology]
source = "ontology.ttl"
namespace = "https://schema.org/"
targets = ["typescript", "rust", "python"]

[templates]
typescript = "./templates/typescript/"
rust = "./templates/rust/"
```

**Benefits**:
- Uses existing discovery, versioning, lockfiles
- No separate infrastructure
- Works with existing `ggen pack install`
- Registry handles ontologies like any pack

### 4. Composition Over Hardcoding

Combine multiple ontologies with merge strategies:

```
Pack A (Schema.org)     Pack B (Dublin Core)
├─ Product              ├─ Creator
├─ Offer                ├─ Subject
└─ Availability         └─ Date

  + (Union Merge) →

Combined Ontology
├─ Product
├─ Offer
├─ Availability
├─ Creator
├─ Subject
└─ Date
```

**Advantage**: No need to maintain separate "schema-org-with-metadata" pack.
Just compose existing packs.

## Dataflow: End-to-End

### Example: Generate TypeScript Types from Schema.org

```
1. CLI Input
   ggen ontology discover schema-org
   → Calls domain::discover::execute_discover()

2. Domain Layer
   execute_discover() {
       // Query marketplace for "schema-org"
       // Return DiscoverOutput { packs: [Pack], ... }
   }

3. CLI Output
   Display: 📦 schema-org (v3.13.0)
            ├─ Classes: 788
            ├─ Rating: ⭐⭐⭐⭐⭐

4. User Action
   ggen ontology install schema-org
   → Calls domain::extract::execute_extract()

5. Domain Layer
   execute_extract() {
       // Load ontology.ttl from pack
       // Parse RDF/OWL using ggen-core::Graph
       // Extract classes, properties, relationships
       // Return ExtractOutput { schema, ... }
   }

6. User Action
   ggen ontology generate \
     --schema schema-org.ttl \
     --language typescript

7. Domain Layer
   execute_generate() {
       // Render templates/typescript/types.tera
       // Context: { classes: [...], properties: [...] }
       // Write to src/generated/types.ts
       // Return GenerateOutput { files_generated, ... }
   }

8. Generated Output
   src/generated/types.ts (620 lines)
   src/generated/validators.ts (890 lines)
   src/generated/utilities.ts (337 lines)
```

## Key Components

### OntologySchema (Core Data Structure)

```rust
pub struct OntologySchema {
    pub namespace: String,
    pub classes: Vec<OntologyClass>,
    pub properties: Vec<OntologyProperty>,
    pub relationships: Vec<OntologyRelationship>,
    pub prefixes: BTreeMap<String, String>,
}
```

This is the intermediate representation that all operations work with.

### OntologyOperation Trait

```rust
pub trait OntologyOperation: Sized {
    type Input;
    type Output;
    fn execute(input: &Self::Input) 
        -> impl Future<Output = Result<Self::Output>>;
}
```

All operations (extract, generate, validate, etc.) implement this.
Makes them composable and testable.

## Composition Example

Combining schema-org with custom-ecommerce:

```bash
# 1. Extract both ontologies
ggen ontology extract --pack schema-org --output schema-org.ttl
ggen ontology extract --pack custom-ecommerce --output custom.ttl

# 2. Compose them
ggen ontology compose \
  --packs schema-org,custom-ecommerce \
  --merge-strategy union \
  --output combined.ttl

# 3. Generate code from combined
ggen ontology generate \
  --schema combined.ttl \
  --language typescript \
  --output src/generated
```

Result: Single `OntologySchema` with all classes and properties.

## Scalability Characteristics

| Factor | Scales How |
|--------|-----------|
| Languages | Linearly (add templates) |
| Ontologies | Linearly (compose/merge) |
| Classes per ontology | Linearly (template rendering) |
| Code generation time | Linear in output size |
| Memory usage | Linear in schema size |

**No exponential blowup** (unlike hardcoded language × ontology matrix).

## Future Extensions

The architecture supports:

- **SPARQL Endpoints** - Query live data sources
- **Dynamic Schemas** - Pull latest ontology from endpoints
- **Transformation Pipelines** - Apply post-processing to generated code
- **Custom Validators** - SHACL constraints → validation code
- **API Scaffolding** - Generate REST/GraphQL servers from ontology
- **Database Mapping** - Generate ORM models from schemas
