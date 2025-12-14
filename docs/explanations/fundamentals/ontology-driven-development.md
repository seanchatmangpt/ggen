<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->
**Table of Contents**

- [What is Ontology-Driven Development?](#what-is-ontology-driven-development)
  - [The Problem with Traditional Development](#the-problem-with-traditional-development)
    - [Traditional Approach: Repetitive Definitions](#traditional-approach-repetitive-definitions)
    - [Problems with This Approach](#problems-with-this-approach)
  - [The Ontology-Driven Solution](#the-ontology-driven-solution)
    - [Define Once in RDF](#define-once-in-rdf)
    - [Generate Everywhere](#generate-everywhere)
  - [Benefits](#benefits)
    - [1. Single Source of Truth](#1-single-source-of-truth)
    - [2. Guaranteed Consistency](#2-guaranteed-consistency)
    - [3. Change Once, Update Everywhere](#3-change-once-update-everywhere)
    - [4. Language-Agnostic Domain Modeling](#4-language-agnostic-domain-modeling)
    - [5. Semantic Richness](#5-semantic-richness)
    - [6. Interoperability](#6-interoperability)
  - [Real-World Examples](#real-world-examples)
    - [Example 1: E-commerce Platform](#example-1-e-commerce-platform)
    - [Example 2: Healthcare System](#example-2-healthcare-system)
    - [Example 3: Financial Services](#example-3-financial-services)
  - [How It Works with ggen](#how-it-works-with-ggen)
    - [Step 1: Define Ontology](#step-1-define-ontology)
    - [Step 2: Load into Graph](#step-2-load-into-graph)
    - [Step 3: Extract Schema](#step-3-extract-schema)
    - [Step 4: Generate Code](#step-4-generate-code)
    - [Step 5: Iterate](#step-5-iterate)
  - [When to Use Ontology-Driven Development](#when-to-use-ontology-driven-development)
  - [Comparison to Other Approaches](#comparison-to-other-approaches)
    - [vs. Protocol Buffers (Protobuf)](#vs-protocol-buffers-protobuf)
    - [vs. OpenAPI Spec](#vs-openapi-spec)
    - [vs. GraphQL Schema](#vs-graphql-schema)
  - [Getting Started](#getting-started)
  - [Further Reading](#further-reading)
  - [Summary](#summary)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

# What is Ontology-Driven Development?

**Concept**: Define your domain model once in RDF/OWL, then generate code for any language or framework.

**Why it matters**: Instead of maintaining separate class definitions in TypeScript, Python, Rust, Java, etc., you define your domain once and generate everywhere.

---

## The Problem with Traditional Development

### Traditional Approach: Repetitive Definitions

In a typical full-stack application, you define the same domain model multiple times:

**TypeScript (Frontend)**:
```typescript
interface Product {
  id: string;
  name: string;
  price: number;
  inStock: boolean;
}
```

**Python (Backend)**:
```python
from pydantic import BaseModel

class Product(BaseModel):
    id: str
    name: str
    price: float
    in_stock: bool
```

**Rust (Services)**:
```rust
#[derive(Serialize, Deserialize)]
struct Product {
    id: String,
    name: String,
    price: f64,
    in_stock: bool,
}
```

**Database (SQL)**:
```sql
CREATE TABLE products (
    id VARCHAR(255) PRIMARY KEY,
    name VARCHAR(255) NOT NULL,
    price DECIMAL(10, 2),
    in_stock BOOLEAN
);
```

**GraphQL (API)**:
```graphql
type Product {
  id: ID!
  name: String!
  price: Float!
  inStock: Boolean!
}
```

**OpenAPI (Documentation)**:
```yaml
Product:
  type: object
  properties:
    id:
      type: string
    name:
      type: string
    price:
      type: number
    inStock:
      type: boolean
```

### Problems with This Approach

1. **Duplication**: Same model defined 6+ times
2. **Inconsistency**: Frontend uses `inStock`, backend uses `in_stock`
3. **Drift**: Changes in one place don't propagate
4. **Maintenance burden**: Every field addition requires 6+ file updates
5. **Human error**: Easy to miss one definition
6. **No single source of truth**: Which definition is canonical?

---

## The Ontology-Driven Solution

### Define Once in RDF

**Single source of truth** (`product-ontology.ttl`):
```turtle
@prefix ex: <http://example.org/> .
@prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .
@prefix xsd: <http://www.w3.org/2001/XMLSchema#> .

ex:Product a rdfs:Class ;
    rdfs:label "Product" ;
    rdfs:comment "A product in the catalog" .

ex:id a rdf:Property ;
    rdfs:domain ex:Product ;
    rdfs:range xsd:string ;
    rdfs:label "id" .

ex:name a rdf:Property ;
    rdfs:domain ex:Product ;
    rdfs:range xsd:string ;
    rdfs:label "name" .

ex:price a rdf:Property ;
    rdfs:domain ex:Product ;
    rdfs:range xsd:decimal ;
    rdfs:label "price" .

ex:inStock a rdf:Property ;
    rdfs:domain ex:Product ;
    rdfs:range xsd:boolean ;
    rdfs:label "inStock" .
```

### Generate Everywhere

**One command generates all targets**:

```bash
# Extract schema once
ggen ontology extract --ontology_file product-ontology.ttl --output product-schema.json

# Generate JavaScript + Zod
ggen project gen \
  --template_ref javascript-zod.tmpl \
  --vars schema_file=product-schema.json \
  --output frontend/src/models/product.js

# Generate Python + Pydantic
ggen project gen \
  --template_ref python-pydantic.tmpl \
  --vars schema_file=product-schema.json \
  --output backend/models/product.py

# Generate Rust structs
ggen project gen \
  --template_ref rust-serde.tmpl \
  --vars schema_file=product-schema.json \
  --output services/src/models/product.rs

# Generate SQL schema
ggen project gen \
  --template_ref postgres-sql.tmpl \
  --vars schema_file=product-schema.json \
  --output database/migrations/001_create_products.sql

# Generate GraphQL schema
ggen project gen \
  --template_ref graphql-schema.tmpl \
  --vars schema_file=product-schema.json \
  --output api/schema/product.graphql

# Generate OpenAPI spec
ggen project gen \
  --template_ref openapi-spec.tmpl \
  --vars schema_file=product-schema.json \
  --output docs/openapi/product.yaml
```

---

## Benefits

### 1. Single Source of Truth

**Before**: 6 definitions that drift apart
**After**: 1 ontology that generates all 6

### 2. Guaranteed Consistency

All generated code uses the same:
- Field names
- Types
- Validation rules
- Documentation

### 3. Change Once, Update Everywhere

**Scenario**: Add a new field `description`

**Traditional approach** (30+ minutes):
1. Update TypeScript interface
2. Update Python model
3. Update Rust struct
4. Update SQL migration
5. Update GraphQL schema
6. Update OpenAPI spec
7. Update tests in all 6 places
8. Hope you didn't miss anything

**Ontology-driven approach** (2 minutes):
1. Add property to ontology:
```turtle
ex:description a rdf:Property ;
    rdfs:domain ex:Product ;
    rdfs:range xsd:string .
```
2. Re-run generation commands
3. Done! All 6 targets updated consistently

### 4. Language-Agnostic Domain Modeling

Your domain model isn't tied to any programming language. You can:
- Add a Go service tomorrow
- Generate Swift models for iOS
- Create C# DTOs for .NET
- Build Java POJOs for Android

All from the same ontology.

### 5. Semantic Richness

RDF/OWL ontologies support:
- **Relationships**: Link products to categories, brands, suppliers
- **Hierarchies**: Product → ElectronicProduct → Laptop
- **Constraints**: Price must be positive, SKU must match pattern
- **Documentation**: Comments and labels travel with the model
- **Reasoning**: Infer new relationships from existing ones

### 6. Interoperability

RDF is a W3C standard used by:
- **Schema.org**: Google, Microsoft, Yahoo
- **DBpedia**: Wikipedia as structured data
- **Wikidata**: Collaboratively edited knowledge base
- **FOAF**: Social network vocabulary
- **Dublin Core**: Metadata standards

You can integrate with these vocabularies directly.

---

## Real-World Examples

### Example 1: E-commerce Platform

**Problem**: Maintain product catalog across web, mobile, API, and data warehouse

**Solution**:
1. Define product ontology with Schema.org vocabulary
2. Generate JavaScript + Zod for React frontend
3. Generate Python + Pydantic for FastAPI backend
4. Generate Rust structs for search service
5. Generate SQL for PostgreSQL warehouse

**Result**: One source of truth, zero drift, instant updates

### Example 2: Healthcare System

**Problem**: Patient data model must comply with FHIR standard across systems

**Solution**:
1. Import FHIR ontology (already in RDF)
2. Extend with custom hospital domain
3. Generate TypeScript for EMR frontend
4. Generate Java POJOs for HL7 integration
5. Generate JSON Schema for API validation

**Result**: FHIR-compliant by definition, not by manual maintenance

### Example 3: Financial Services

**Problem**: Regulatory reporting requires consistent data definitions

**Solution**:
1. Define financial instrument ontology
2. Generate Python for ML pipelines
3. Generate Scala for Spark processing
4. Generate SQL for regulatory reports
5. Generate OpenAPI for external APIs

**Result**: Audit trail shows all code generated from canonical ontology

---

## How It Works with ggen

### Step 1: Define Ontology

Use RDF/OWL to define your domain:
- Classes (types)
- Properties (fields)
- Relationships
- Constraints
- Documentation

### Step 2: Load into Graph

```bash
ggen graph load --file your-ontology.ttl
```

ggen stores your ontology in an RDF graph (Oxigraph) for querying.

### Step 3: Extract Schema

```bash
ggen ontology extract --ontology_file your-ontology.ttl --output schema.json
```

Extracts classes, properties, and relationships into JSON for templates.

### Step 4: Generate Code

```bash
ggen project gen \
  --template_ref your-template.tmpl \
  --vars schema_file=schema.json \
  --output path/to/generated/code
```

Templates transform schema into target language code.

### Step 5: Iterate

Change ontology → Re-extract → Re-generate → Deploy

---

## When to Use Ontology-Driven Development

**✅ Good fit**:
- Multi-language projects (frontend + backend + services)
- Domain models that change frequently
- Need for semantic consistency across systems
- Integration with standard vocabularies (Schema.org, FHIR)
- Polyglot architectures
- Compliance requirements (audit trails)

**⚠️ May be overkill**:
- Single-language projects
- Simple CRUD apps with no domain complexity
- Prototypes and MVPs
- Projects with unstable domain models

---

## Comparison to Other Approaches

### vs. Protocol Buffers (Protobuf)

**Protobuf**:
- ✅ Great for service communication
- ❌ Not semantic (no reasoning, no standard vocabularies)
- ❌ Less expressive than RDF/OWL

**Ontology-Driven**:
- ✅ Semantic richness
- ✅ Standard vocabularies
- ✅ Reasoning capabilities
- ❌ Steeper learning curve

### vs. OpenAPI Spec

**OpenAPI**:
- ✅ Great for API documentation
- ❌ Covers only HTTP APIs
- ❌ No code generation for domain logic

**Ontology-Driven**:
- ✅ Covers entire domain, not just APIs
- ✅ Generates models, validators, databases, APIs
- ✅ Semantic meaning beyond HTTP

### vs. GraphQL Schema

**GraphQL**:
- ✅ Great for APIs
- ❌ Tied to GraphQL
- ❌ No database, validation, or backend models

**Ontology-Driven**:
- ✅ Generates GraphQL + everything else
- ✅ Language-agnostic
- ✅ Can generate GraphQL schema from ontology

---

## Getting Started

1. **[Quick Start Tutorial](../../getting-started/quick-start.md)** - Generate your first code in 10 minutes
2. **[RDF for Programmers](rdf-for-programmers.md)** - Learn RDF/Turtle syntax
3. **[Generate JavaScript + Zod](../../how-to/generation/generate-javascript-zod.md)** - Practical generation guide

---

## Further Reading

- **W3C RDF Primer**: https://www.w3.org/TR/rdf11-primer/
- **OWL 2 Primer**: https://www.w3.org/TR/owl2-primer/
- **Schema.org**: https://schema.org
- **Linked Data**: https://www.w3.org/DesignIssues/LinkedData.html

---

## Summary

**Ontology-Driven Development** means:
- ✅ Define domain model once in RDF/OWL
- ✅ Generate code for any language
- ✅ Single source of truth
- ✅ Guaranteed consistency
- ✅ Semantic richness
- ✅ Standards-based interoperability

**Trade-offs**:
- Initial learning curve for RDF/OWL
- Requires tooling like ggen
- Best for multi-language or complex domains

**Bottom line**: If you're maintaining the same model in 3+ languages, ontology-driven development will save you time and prevent errors.
