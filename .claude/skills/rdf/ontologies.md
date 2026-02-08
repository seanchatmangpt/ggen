# RDF Ontology Patterns

## Core Principle

**RDF is Truth**: Edit `.specify/*.ttl` (source). Never edit `.md` (generated projection via μ).

## Holographic Factory Pipeline (μ)

```
μ = μ₁ ∘ μ₂ ∘ μ₃ ∘ μ₄ ∘ μ₅

μ₁: Normalize   - RDF validation, SHACL shapes, dependency resolution
μ₂: Extract     - SPARQL queries, OWL inference, rule execution
μ₃: Emit        - Tera template rendering, code generation
μ₄: Canonicalize - Deterministic formatting, content hashing
μ₅: Receipt     - Cryptographic proof, audit trail
```

**Corollary**: Bug in generated code? Fix RDF spec (source), not output (projection).

## Ontology Structure

### Basic Ontology
```turtle
@prefix ex: <http://example.org/> .
@prefix owl: <http://www.w3.org/2002/07/owl#> .
@prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .
@prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .
@prefix xsd: <http://www.w3.org/2001/XMLSchema#> .

ex:Person a owl:Class ;
    rdfs:label "Person" ;
    rdfs:comment "A human being" .

ex:name a owl:DatatypeProperty ;
    rdfs:domain ex:Person ;
    rdfs:range xsd:string ;
    rdfs:label "name" .

ex:age a owl:DatatypeProperty ;
    rdfs:domain ex:Person ;
    rdfs:range xsd:integer ;
    rdfs:label "age" .

ex:knows a owl:ObjectProperty ;
    rdfs:domain ex:Person ;
    rdfs:range ex:Person ;
    rdfs:label "knows" .
```

### SHACL Validation
```turtle
@prefix sh: <http://www.w3.org/ns/shacl#> .

ex:PersonShape a sh:NodeShape ;
    sh:targetClass ex:Person ;
    sh:property [
        sh:path ex:name ;
        sh:minCount 1 ;
        sh:maxCount 1 ;
        sh:datatype xsd:string ;
    ] ;
    sh:property [
        sh:path ex:age ;
        sh:minCount 0 ;
        sh:maxCount 1 ;
        sh:datatype xsd:integer ;
        sh:minInclusive 0 ;
        sh:maxInclusive 150 ;
    ] .
```

## Common Patterns

### Class Hierarchy
```turtle
ex:Animal a owl:Class .
ex:Mammal a owl:Class ;
    rdfs:subClassOf ex:Animal .
ex:Dog a owl:Class ;
    rdfs:subClassOf ex:Mammal .
```

### Property Restrictions
```turtle
ex:Parent a owl:Class ;
    rdfs:subClassOf [
        a owl:Restriction ;
        owl:onProperty ex:hasChild ;
        owl:minCardinality 1 ;
    ] .
```

### Equivalence
```turtle
ex:Person owl:equivalentClass schema:Person .
ex:name owl:equivalentProperty schema:name .
```

## Specification Workflow

### 1. Create Spec
```bash
mkdir -p .specify/specs/NNN-feature
vim .specify/specs/NNN-feature/feature.ttl
```

### 2. Validate
```bash
cargo make speckit-validate
# Runs SHACL validation on all .specify/*.ttl files
```

### 3. Generate Markdown
```bash
cargo make speckit-render
# Generates .specify/specs/NNN-feature/feature.md from feature.ttl
```

### 4. Sync Code
```bash
ggen sync --dry_run true   # Preview changes
ggen sync --audit true     # Full sync with cryptographic audit trail
```

## File Organization

```
.specify/
├── specs/
│   ├── 001-user-model/
│   │   ├── user.ttl     # Source of truth
│   │   └── user.md      # Generated (DO NOT EDIT)
│   ├── 002-order-model/
│   │   ├── order.ttl    # Source of truth
│   │   └── order.md     # Generated (DO NOT EDIT)
│   └── ...
└── templates/
    ├── rust/
    │   ├── struct.tera
    │   ├── enum.tera
    │   └── impl.tera
    └── ...
```

## Best Practices

✅ **Spec Closure First**: 100% coverage in `.specify/*.ttl` before code generation
✅ **SHACL Validation**: Always validate before sync
✅ **Dry Run**: Preview changes with `--dry_run true`
✅ **Audit Trail**: Use `--audit true` for cryptographic receipts
✅ **Immutable TTL**: Once closed, fix source then regenerate

❌ **Never edit generated .md files**
❌ **Never skip validation**
❌ **Never edit generated code directly** (fix source TTL instead)

## Mental Model

```
RDF Ontology (.ttl)
    ↓ μ₁ (Normalize)
Validated RDF
    ↓ μ₂ (Extract via SPARQL)
Structured Data
    ↓ μ₃ (Emit via Tera templates)
Generated Code
    ↓ μ₄ (Canonicalize)
Deterministic Output
    ↓ μ₅ (Receipt)
Cryptographic Proof
```

**A = μ(O)**: Code (A) precipitates from RDF ontology (O) via five-stage pipeline (μ₁-μ₅)
