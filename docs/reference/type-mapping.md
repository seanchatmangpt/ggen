<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->
**Table of Contents**

- [Type Mapping Reference](#type-mapping-reference)
  - [Quick Reference Table](#quick-reference-table)
  - [Semantic Types (Optional)](#semantic-types-optional)
    - [Constraints](#constraints)
    - [Relationships](#relationships)
  - [Type Mapping Examples](#type-mapping-examples)
    - [Example 1: Simple Entity](#example-1-simple-entity)
    - [Example 2: Relationships](#example-2-relationships)
    - [Example 3: Complex Types](#example-3-complex-types)
  - [Null/Optional Handling](#nulloptional-handling)
  - [Collection Types](#collection-types)
  - [Validation Rules](#validation-rules)
  - [Custom Type Mappings](#custom-type-mappings)
  - [Common Patterns](#common-patterns)
    - [Financial Values](#financial-values)
    - [Timestamps](#timestamps)
    - [Identifiers](#identifiers)
    - [Enumerations](#enumerations)
  - [Troubleshooting](#troubleshooting)
  - [See Also](#see-also)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

# Type Mapping Reference

Complete reference for RDF type mappings to all supported languages.

## Quick Reference Table

| RDF Type | Rust | TypeScript | Python | Go | Java |
|----------|------|------------|--------|----|----|
| `xsd:string` | `String` | `string` | `str` | `string` | `String` |
| `xsd:integer` | `i32` | `number` | `int` | `int` | `int` |
| `xsd:long` | `i64` | `number` | `int` | `int64` | `long` |
| `xsd:decimal` | `f64` | `number` | `Decimal` | `float64` | `BigDecimal` |
| `xsd:float` | `f32` | `number` | `float` | `float32` | `float` |
| `xsd:boolean` | `bool` | `boolean` | `bool` | `bool` | `boolean` |
| `xsd:date` | `chrono::NaiveDate` | `Date` | `date` | `time.Time` | `LocalDate` |
| `xsd:dateTime` | `chrono::DateTime` | `Date` | `datetime` | `time.Time` | `LocalDateTime` |
| `xsd:duration` | `chrono::Duration` | `Duration` | `timedelta` | `time.Duration` | `Duration` |
| `xsd:anyURI` | `String` | `string` | `str` | `string` | `URI` |
| `xsd:base64Binary` | `Vec<u8>` | `Uint8Array` | `bytes` | `[]byte` | `byte[]` |
| `rdfs:Class` | `pub struct` | `interface` | `class` | `type` | `class` |
| `rdf:Property` (datatype) | `pub field` | `property` | `attribute` | `field` | `field` |
| `rdf:Property` (object) | `fn get_*()` | `get*()` | `def get_*()` | `Get*()` | `get*()` |

## Semantic Types (Optional)

Custom RDF namespaces for richer semantics:

### Constraints

| RDF Property | Usage | Example |
|--------------|-------|---------|
| `sh:minInclusive` | Minimum value | `sh:minInclusive 0 ;` → Validation added |
| `sh:maxInclusive` | Maximum value | `sh:maxInclusive 100 ;` → Range check |
| `sh:minLength` | String length | `sh:minLength 3 ;` → Length validation |
| `sh:maxLength` | String length | `sh:maxLength 50 ;` → Length check |
| `sh:pattern` | Regex pattern | `sh:pattern "[a-z]+";` → Regex validation |
| `rdfs:comment` | Documentation | `rdfs:comment "User's email"` → Doc comment |
| `rdfs:label` | Human-readable | `rdfs:label "Email Address"` → Property name |

### Relationships

| RDF Property | Usage | Generated Code |
|--------------|-------|-----------------|
| `rdf:Property` (object) | Reference to another class | Method or field returning related object |
| `rdf:type` of `rdfs:Class` | Class definition | `pub struct` / `interface` / `class` |
| `rdfs:domain` | Property ownership | Indicates which class owns property |
| `rdfs:range` | Property type | Determines generated type |

## Type Mapping Examples

### Example 1: Simple Entity

**RDF Ontology:**
```turtle
@prefix ex: <http://example.org/> .
@prefix xsd: <http://www.w3.org/2001/XMLSchema#> .
@prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .

ex:User a rdfs:Class ;
  rdfs:label "User" ;
  rdfs:comment "Platform user account" .

ex:email a rdf:Property ;
  rdfs:domain ex:User ;
  rdfs:range xsd:string ;
  rdfs:label "email" ;
  sh:pattern "[a-z0-9._%+-]+@[a-z0-9.-]+\\.[a-z]{2,}" .

ex:age a rdf:Property ;
  rdfs:domain ex:User ;
  rdfs:range xsd:integer ;
  rdfs:label "age" ;
  sh:minInclusive 0 ;
  sh:maxInclusive 150 .
```

**Generated Rust:**
```rust
pub struct User {
    pub email: String,  // Pattern validation from sh:pattern
    pub age: i32,       // Range validation from sh:minInclusive/maxInclusive
}
```

**Generated TypeScript:**
```typescript
interface User {
    email: string;      // Pattern validation
    age: number;        // Range validation
}
```

**Generated Python:**
```python
@dataclass
class User:
    email: str          # Pattern validation
    age: int            # Range validation
```

### Example 2: Relationships

**RDF Ontology:**
```turtle
ex:Order a rdfs:Class .
ex:User a rdfs:Class .

ex:orderedBy a rdf:Property ;
  rdfs:domain ex:Order ;
  rdfs:range ex:User ;
  rdfs:label "orderedBy" .

ex:totalAmount a rdf:Property ;
  rdfs:domain ex:Order ;
  rdfs:range xsd:decimal ;
  rdfs:label "totalAmount" ;
  sh:minInclusive 0.01 .
```

**Generated Rust:**
```rust
pub struct Order {
    pub total_amount: f64,
}

impl Order {
    pub fn get_ordered_by(&self) -> User {
        // Load User from relationship
    }
}
```

**Generated TypeScript:**
```typescript
interface Order {
    totalAmount: number;
}

function getOrderedBy(order: Order): User {
    // Load User from relationship
}
```

### Example 3: Complex Types

**RDF Ontology:**
```turtle
ex:Event a rdfs:Class .

ex:startDate a rdf:Property ;
  rdfs:domain ex:Event ;
  rdfs:range xsd:dateTime ;
  rdfs:label "startDate" .

ex:tags a rdf:Property ;
  rdfs:domain ex:Event ;
  rdfs:range xsd:string ;
  rdfs:label "tags" .
```

**Generated Rust:**
```rust
pub struct Event {
    pub start_date: chrono::DateTime<chrono::Utc>,
    pub tags: Vec<String>,
}
```

**Generated TypeScript:**
```typescript
interface Event {
    startDate: Date;
    tags: string[];
}
```

## Null/Optional Handling

Properties can be marked as optional:

```turtle
ex:middleName a rdf:Property ;
  rdfs:domain ex:User ;
  rdfs:range xsd:string ;
  rdfs:label "middleName" ;
  rdfs:comment "Optional middle name" .  {# Presence of comment with "Optional" triggers nullable #}
```

**Generated Rust:**
```rust
pub struct User {
    pub first_name: String,
    pub middle_name: Option<String>,  // Optional based on comment
    pub last_name: String,
}
```

**Generated TypeScript:**
```typescript
interface User {
    firstName: string;
    middleName?: string;  // Optional
    lastName: string;
}
```

## Collection Types

Arrays and collections are inferred from semantic patterns:

```turtle
ex:products a rdf:Property ;
  rdfs:domain ex:Order ;
  rdfs:range ex:Product ;
  rdfs:label "products" ;
  rdfs:comment "List of products in order" .  {# "List" triggers Vec/[] #}
```

**Generated Rust:**
```rust
pub struct Order {
    pub products: Vec<Product>,
}
```

**Generated TypeScript:**
```typescript
interface Order {
    products: Product[];
}
```

## Validation Rules

Validation constraints are extracted and applied:

| Constraint | Rust | TypeScript | Python |
|-----------|------|------------|--------|
| `sh:minInclusive` | `.min()` validation | Range check in setter | `value >= min` assertion |
| `sh:maxInclusive` | `.max()` validation | Range check in setter | `value <= max` assertion |
| `sh:minLength` | `.len() >=` | Length check | `len() >=` check |
| `sh:maxLength` | `.len() <=` | Length check | `len() <=` check |
| `sh:pattern` | Regex validation | Pattern test | `re.match()` check |

## Custom Type Mappings

Override default mappings by extending the template:

```tera
{% if prop.range == "xsd:decimal" %}
  {{- prop.rust_type = "rust_decimal::Decimal" -}}
{% endif %}
```

## Common Patterns

### Financial Values
Use `xsd:decimal` with `sh:minInclusive 0` for monetary amounts.

### Timestamps
Use `xsd:dateTime` for event times; ggen automatically adds timezone handling.

### Identifiers
Use `xsd:string` with `sh:pattern` for IDs (UUID, email, SKU).

### Enumerations
Use `rdfs:comment` with comma-separated values to generate enums:
```turtle
ex:status a rdf:Property ;
  rdfs:range xsd:string ;
  rdfs:comment "active, inactive, pending" .  {# Generates enum #}
```

## Troubleshooting

**Q: My property type isn't being recognized**
A: Ensure `rdfs:range` is set explicitly. ggen uses `rdfs:range` to determine type.

**Q: Validation isn't being added to generated code**
A: Add SHACL constraints (`sh:minInclusive`, `sh:pattern`, etc.) to your ontology.

**Q: Optional/nullable fields aren't being generated**
A: Add "Optional" or "nullable" in the `rdfs:comment` of the property.

## See Also

- [RDF/SPARQL Reference](rdf-sparql.md) - RDF syntax and SPARQL queries
- [Template Reference](templates.md) - Template syntax and directives
- [Ontology-Driven Development](../explanations/ontology-driven.md) - Conceptual overview
