<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->
**Table of Contents**

- [Semantic Projections](#semantic-projections)
  - [The Core Concept](#the-core-concept)
  - [One Ontology, Many Languages](#one-ontology-many-languages)
  - [Type Mapping: Semantic to Language-Specific](#type-mapping-semantic-to-language-specific)
    - [XSD Datatypes to Language Types](#xsd-datatypes-to-language-types)
    - [Example: Product Price Across Languages](#example-product-price-across-languages)
  - [Relationship Mapping: Predicates to Methods](#relationship-mapping-predicates-to-methods)
    - [RDF Relationships](#rdf-relationships)
    - [Projected to Code](#projected-to-code)
  - [Complete Example: Product Catalog Projections](#complete-example-product-catalog-projections)
    - [The Ontology (Language-Agnostic)](#the-ontology-language-agnostic)
    - [Projection 1: Rust Struct](#projection-1-rust-struct)
    - [Projection 2: TypeScript Interface](#projection-2-typescript-interface)
    - [Projection 3: Python Dataclass](#projection-3-python-dataclass)
    - [Projection 4: SQL Table Schema](#projection-4-sql-table-schema)
    - [Projection 5: GraphQL Type](#projection-5-graphql-type)
  - [Evolution: Update Once, Regenerate Everywhere](#evolution-update-once-regenerate-everywhere)
    - [Step 1: Modify the Ontology](#step-1-modify-the-ontology)
    - [Step 2: Regenerate All Projections](#step-2-regenerate-all-projections)
    - [The Result](#the-result)
  - [How Projections Work Internally](#how-projections-work-internally)
  - [Projection Patterns and Best Practices](#projection-patterns-and-best-practices)
    - [Standard Pattern](#standard-pattern)
    - [Marketplace Pattern](#marketplace-pattern)
    - [Best Practices](#best-practices)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

# Semantic Projections

## The Core Concept

**Semantic projections** are the mechanism by which ggen transforms a single, language-agnostic RDF ontology into multiple language-specific code representations.

```
                   RDF Ontology (Semantic Model)
                            ↓
        ┌───────────────────┼───────────────────┐
        ↓                   ↓                   ↓
   Rust Structs      TypeScript Types      Python Classes
```

The key insight: **The domain model (ontology) is separate from its representation (projection).**

This separation enables:

- **Cross-language consistency**: Same business logic across codebases
- **Automatic synchronization**: Change ontology → regenerate all projections
- **Single source of truth**: One model, many representations
- **Evolution without drift**: Update once, deploy everywhere

## One Ontology, Many Languages

Traditional approach:

```
Product.java     → Manually kept in sync with
Product.ts       → Each requires separate updates
Product.py       → Easy to drift out of sync
product.sql      → Different conventions, same entity
```

**ggen approach:**

```turtle
# product_catalog.ttl (ONE source of truth)
pc:Product a rdfs:Class ;
    rdfs:label "Product" .

pc:name rdfs:domain pc:Product ; rdfs:range xsd:string .
pc:price rdfs:domain pc:Product ; rdfs:range xsd:decimal .
```

```bash
# Generate all projections from one ontology
ggen gen rust/models.tmpl --graph product_catalog.ttl
ggen gen typescript/types.tmpl --graph product_catalog.ttl
ggen gen python/models.tmpl --graph product_catalog.ttl
ggen gen sql/schema.tmpl --graph product_catalog.ttl
```

**Result:** Four language-specific implementations, guaranteed to be in sync.

## Type Mapping: Semantic to Language-Specific

ggen maps RDF datatypes (XSD schema types) to appropriate language-specific types.

### XSD Datatypes to Language Types

| XSD Type       | Rust       | TypeScript | Python    | SQL          |
|----------------|-----------|-----------|----------|-------------|
| `xsd:string`   | `String`  | `string`  | `str`    | `VARCHAR`   |
| `xsd:integer`  | `i64`     | `number`  | `int`    | `INTEGER`   |
| `xsd:decimal`  | `f64`     | `number`  | `float`  | `DECIMAL`   |
| `xsd:boolean`  | `bool`    | `boolean` | `bool`   | `BOOLEAN`   |
| `xsd:date`     | `NaiveDate` | `Date`  | `date`   | `DATE`      |
| `xsd:dateTime` | `DateTime` | `Date`  | `datetime` | `TIMESTAMP` |

These mappings are **configurable** via Handlebars helpers in templates.

### Example: Product Price Across Languages

**Ontology:**

```turtle
pc:price a rdf:Property ;
    rdfs:domain pc:Product ;
    rdfs:range xsd:decimal ;
    rdfs:label "price" .
```

**Projected to:**

| Language   | Field Declaration                |
|-----------|--------------------------------|
| Rust      | `pub price: f64`               |
| TypeScript | `price: number`               |
| Python    | `price: float`                 |
| SQL       | `price DECIMAL(10, 2)`         |
| GraphQL   | `price: Float!`                |

**The ontology never changes.** Only the projection templates differ.

## Relationship Mapping: Predicates to Methods

RDF relationships (object properties) project to different code patterns depending on the language.

### RDF Relationships

```turtle
# Product belongs to a Category
pc:category a rdf:Property ;
    rdfs:domain pc:Product ;
    rdfs:range pc:Category ;
    rdfs:label "category" .

# Product has a Supplier
pc:supplier a rdf:Property ;
    rdfs:domain pc:Product ;
    rdfs:range pc:Supplier ;
    rdfs:label "supplier" .
```

### Projected to Code

**Rust:**

```rust
pub struct Product {
    pub name: String,
    pub price: f64,
    pub category: Category,  // Foreign key relationship
    pub supplier: Supplier,
}

impl Product {
    /// Get the category for this product
    pub fn get_category(&self) -> &Category {
        &self.category
    }

    /// Get the supplier for this product
    pub fn get_supplier(&self) -> &Supplier {
        &self.supplier
    }
}
```

**TypeScript:**

```typescript
interface Product {
  name: string;
  price: number;
  category: Category;
  supplier: Supplier;
}

class ProductService {
  async getCategory(product: Product): Promise<Category> {
    return product.category;
  }

  async getSupplier(product: Product): Promise<Supplier> {
    return product.supplier;
  }
}
```

**SQL:**

```sql
CREATE TABLE products (
  id SERIAL PRIMARY KEY,
  name VARCHAR(255) NOT NULL,
  price DECIMAL(10, 2) NOT NULL,
  category_id INTEGER REFERENCES categories(id),
  supplier_id INTEGER REFERENCES suppliers(id)
);
```

**Same relationship, different representations.** The ontology defines the semantics, templates define the syntax.

## Complete Example: Product Catalog Projections

Let's see a full example of one ontology generating code in five languages.

### The Ontology (Language-Agnostic)

```turtle
@prefix pc: <http://example.org/product_catalog#> .
@prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .
@prefix xsd: <http://www.w3.org/2001/XMLSchema#> .

# Classes
pc:Product a rdfs:Class ;
    rdfs:label "Product" ;
    rdfs:comment "A product in the e-commerce catalog" .

pc:Category a rdfs:Class ;
    rdfs:label "Category" ;
    rdfs:comment "A product category" .

# Data properties (primitives)
pc:name rdfs:domain pc:Product ; rdfs:range xsd:string .
pc:price rdfs:domain pc:Product ; rdfs:range xsd:decimal .
pc:sku rdfs:domain pc:Product ; rdfs:range xsd:string .

# Object properties (relationships)
pc:category rdfs:domain pc:Product ; rdfs:range pc:Category .
```

### Projection 1: Rust Struct

**Template:** `rust/models.tmpl`

```handlebars
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Product {
{{#each properties}}
    pub {{ name }}: {{ rust_type datatype }},
{{/each}}
}
```

**Generated:** `src/models/product.rs`

```rust
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Product {
    pub name: String,
    pub price: f64,
    pub sku: String,
    pub category: Category,
}
```

### Projection 2: TypeScript Interface

**Template:** `typescript/types.tmpl`

```handlebars
export interface Product {
{{#each properties}}
  {{ name }}: {{ ts_type datatype }};
{{/each}}
}
```

**Generated:** `src/types/Product.ts`

```typescript
export interface Product {
  name: string;
  price: number;
  sku: string;
  category: Category;
}
```

### Projection 3: Python Dataclass

**Template:** `python/models.tmpl`

```handlebars
@dataclass
class Product:
{{#each properties}}
    {{ name }}: {{ python_type datatype }}
{{/each}}
```

**Generated:** `models/product.py`

```python
from dataclasses import dataclass

@dataclass
class Product:
    name: str
    price: float
    sku: str
    category: Category
```

### Projection 4: SQL Table Schema

**Template:** `sql/schema.tmpl`

```handlebars
CREATE TABLE products (
  id SERIAL PRIMARY KEY,
{{#each properties}}
  {{ name }} {{ sql_type datatype }}{{#if required}} NOT NULL{{/if}},
{{/each}}
);
```

**Generated:** `migrations/001_create_products.sql`

```sql
CREATE TABLE products (
  id SERIAL PRIMARY KEY,
  name VARCHAR(255) NOT NULL,
  price DECIMAL(10, 2) NOT NULL,
  sku VARCHAR(50) NOT NULL,
  category_id INTEGER REFERENCES categories(id)
);
```

### Projection 5: GraphQL Type

**Template:** `graphql/schema.tmpl`

```handlebars
type Product {
{{#each properties}}
  {{ name }}: {{ graphql_type datatype }}!
{{/each}}
}
```

**Generated:** `schema/product.graphql`

```graphql
type Product {
  name: String!
  price: Float!
  sku: String!
  category: Category!
}
```

**Five languages, one source of truth, complete consistency.**

## Evolution: Update Once, Regenerate Everywhere

The real power of semantic projections emerges when evolving your domain model.

### Step 1: Modify the Ontology

Add a `rating` field to Product:

```turtle
# Add to product_catalog.ttl
pc:rating a rdf:Property ;
    rdfs:domain pc:Product ;
    rdfs:range xsd:decimal ;
    rdfs:label "rating" ;
    rdfs:comment "Product rating from 0.0 to 5.0" .
```

### Step 2: Regenerate All Projections

```bash
# One command per projection
ggen gen rust/models.tmpl --graph product_catalog.ttl
ggen gen typescript/types.tmpl --graph product_catalog.ttl
ggen gen python/models.tmpl --graph product_catalog.ttl
ggen gen sql/schema.tmpl --graph product_catalog.ttl
ggen gen graphql/schema.tmpl --graph product_catalog.ttl
```

Or batch with a script:

```bash
# regenerate-all.sh
for template in templates/*.tmpl; do
  ggen gen "$template" --graph product_catalog.ttl
done
```

### The Result

**All five languages now have the `rating` field:**

```rust
// Rust
pub struct Product {
    pub name: String,
    pub price: f64,
    pub sku: String,
    pub rating: f64,  // ← NEW
    pub category: Category,
}
```

```typescript
// TypeScript
export interface Product {
  name: string;
  price: number;
  sku: string;
  rating: number;  // ← NEW
  category: Category;
}
```

```python
# Python
@dataclass
class Product:
    name: str
    price: float
    sku: str
    rating: float  # ← NEW
    category: Category
```

```sql
-- SQL
CREATE TABLE products (
  id SERIAL PRIMARY KEY,
  name VARCHAR(255) NOT NULL,
  price DECIMAL(10, 2) NOT NULL,
  sku VARCHAR(50) NOT NULL,
  rating DECIMAL(2, 1),  -- NEW
  category_id INTEGER REFERENCES categories(id)
);
```

```graphql
# GraphQL
type Product {
  name: String!
  price: Float!
  sku: String!
  rating: Float!  # ← NEW
  category: Category!
}
```

**No manual editing. No copy-paste. No drift. Guaranteed synchronization.**

## How Projections Work Internally

Under the hood, ggen performs these steps for each projection:

1. **Load ontology** into Oxigraph RDF store
2. **Execute SPARQL query** defined in template frontmatter
3. **Extract variables** from query results
4. **Map types** using Handlebars helpers (e.g., `{{ rust_type }}`)
5. **Render template** with mapped variables
6. **Write output** to specified file path

**Example template with SPARQL query:**

```yaml
---
to: src/models/{{ class_name }}.rs
vars:
  class_name: Product
sparql: |
  PREFIX pc: <http://example.org/product_catalog#>
  PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>

  SELECT ?property ?datatype ?label WHERE {
    ?property rdfs:domain pc:Product .
    ?property rdfs:range ?datatype .
    ?property rdfs:label ?label .
  }
  ORDER BY ?label
---
pub struct {{ class_name }} {
{{#each sparql_results}}
    pub {{ ?label }}: {{ rust_type ?datatype }},
{{/each}}
}
```

**The SPARQL query extracts data from the ontology.** The template renders it as Rust code.

## Projection Patterns and Best Practices

### Standard Pattern

1. **Define ontology** in language-agnostic RDF
2. **Create templates** for each target language
3. **Use SPARQL** to extract exactly what each template needs
4. **Map types** with Handlebars helpers
5. **Regenerate** whenever ontology changes

### Marketplace Pattern

For reusable projections, ggen supports the **marketplace gpack** pattern:

```bash
# Install projection templates from marketplace
ggen add io.ggen.rust.models
ggen add io.ggen.typescript.types
ggen add io.ggen.sql.schema

# Generate from marketplace templates
ggen gen io.ggen.rust.models:models.tmpl --graph product_catalog.ttl
ggen gen io.ggen.typescript.types:types.tmpl --graph product_catalog.ttl
```

**Benefits:**

- Pre-built, tested templates
- Consistent code style across projects
- Community-maintained type mappings
- Version-locked for determinism

### Best Practices

**1. Use semantic types in ontology:**

```turtle
# Good: Semantic precision
pc:createdAt rdfs:range xsd:dateTime .
pc:isActive rdfs:range xsd:boolean .

# Avoid: Generic types lose information
pc:createdAt rdfs:range xsd:string .  # ❌ Lost temporal semantics
```

**2. Leverage SPARQL for complex queries:**

```sparql
# Extract only required properties (not optional)
SELECT ?property ?datatype WHERE {
    ?property rdfs:domain ?class .
    ?property rdfs:range ?datatype .
    FILTER EXISTS { ?shape sh:property [ sh:path ?property ; sh:minCount 1 ] }
}
```

**3. Create custom type mappings:**

```handlebars
{{! Custom helper for domain-specific types }}
{{ custom_type datatype }}

{{! Where custom_type might map: }}
{{! xsd:string + pc:UUID → Uuid (Rust) or uuid.UUID (Python) }}
```

**4. Document projection conventions:**

```markdown
# Type Mapping Conventions

| Ontology Type | Rust Type | Notes |
|--------------|----------|-------|
| xsd:decimal + pc:Price | Decimal | Use rust_decimal crate for precision |
| xsd:string + pc:Email | String | Add validation in constructor |
```

**5. Automate regeneration in CI:**

```yaml
# .github/workflows/codegen.yml
- name: Regenerate projections
  run: |
    ./scripts/regenerate-all.sh
    git diff --exit-code || echo "::error::Projections out of sync"
```

This ensures ontology changes are caught before merging.

---

**Semantic projections are the bridge between abstract domain models and concrete implementations.** By separating semantics from syntax, ggen enables true cross-language consistency and effortless evolution.
