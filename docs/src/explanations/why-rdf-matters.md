# Why RDF Matters for Code Generation

Understanding the power and limitations of RDF-based code generation.

## The Problem: Manual Synchronization

### Traditional Approach

You define types in multiple places:

**TypeScript**:
```typescript
export interface User {
  id: string;
  email: string;
  active: boolean;
}
```

**Python**:
```python
class User:
    def __init__(self, id: str, email: str, active: bool):
        self.id = id
        self.email = email
        self.active = active
```

**Rust**:
```rust
pub struct User {
    pub id: String,
    pub email: String,
    pub active: bool,
}
```

**Database Schema**:
```sql
CREATE TABLE users (
    id TEXT PRIMARY KEY,
    email TEXT NOT NULL,
    active BOOLEAN DEFAULT true
);
```

**The Problem**: When you add a field, you update 4+ places manually. Someone inevitably forgets a language. **Type drift** occurs.

## The RDF Solution

Define once in a **semantic ontology**:

```turtle
@prefix app: <http://example.org/app/> .
@prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .
@prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .
@prefix xsd: <http://www.w3.org/2001/XMLSchema#> .

app:User a rdfs:Class ;
  rdfs:label "User" ;
  rdfs:comment "A user account" .

app:id a rdf:Property ;
  rdfs:domain app:User ;
  rdfs:range xsd:string .

app:email a rdf:Property ;
  rdfs:domain app:User ;
  rdfs:range xsd:string .

app:active a rdf:Property ;
  rdfs:domain app:User ;
  rdfs:range xsd:boolean .
```

**Regenerate all languages**:

```bash
ggen ontology generate app.ttl --language typescript --output models.ts
ggen ontology generate app.ttl --language python --output models.py
ggen ontology generate app.ttl --language rust --output models.rs
```

**Result**: All languages in sync, zero drift.

## Why RDF Specifically?

### What is RDF?

**Resource Description Framework** is a standardized way to represent knowledge:
- **Standardized**: W3C standard, not proprietary
- **Machine-readable**: Computers understand the meaning
- **Semantic**: Includes metadata about meaning, not just structure
- **Interoperable**: Works with other systems using RDF

### RDF vs YAML/JSON/TOML

| Aspect | YAML/JSON | RDF |
|--------|-----------|-----|
| **Structure** | Nested, tree-like | Graph, any shape |
| **Semantics** | Document format | Meaning is encoded |
| **Relationships** | Parent-child | Any entity can relate to any |
| **Types** | Strings, numbers, booleans | Formal type system |
| **Querying** | Basic (XPath, JSONPath) | Powerful (SPARQL) |
| **Extensibility** | Add new fields | Add new relationships |
| **Interoperability** | Vendor-specific | Standard, linked data |

**Example**: With RDF, you can automatically find:
- All classes with a `price` property
- All relationships between `User` and `Product`
- Which properties are required vs optional
- Type compatibility rules

With JSON, you'd write code to search the structure manually.

### RDF enables Knowledge Graphs

A **knowledge graph** is a machine-understandable representation of knowledge:

```
User -- has --> Email
User -- has --> Name
Product -- costs --> Price
User -- purchases --> Product
```

Query examples that RDF enables:
```sparql
# Find all users who purchased products > $100
SELECT ?user WHERE {
  ?user purchases ?product .
  ?product costs ?price .
  FILTER (?price > 100)
}

# Find all properties of the User class
SELECT ?property WHERE {
  ?property rdfs:domain User .
}
```

## Use Cases Where RDF Excels

### ✅ Good for RDF

1. **Polyglot Systems**: Types must sync across Rust, TypeScript, Python
2. **Complex Relationships**: Many-to-many relationships, graphs
3. **Domain-Driven Design**: Modeling business domains semantically
4. **Federated Systems**: Linking data from multiple sources
5. **Semantic APIs**: APIs with machine-understandable contracts
6. **Knowledge Bases**: Building AI training data
7. **Compliance/Governance**: Tracking data lineage and rules

### ❌ Not Good for RDF

1. **Simple CRUD Apps**: Single language, single database, simple structure
2. **Configuration Files**: YAML/TOML better for humans
3. **Temporary Projects**: Overhead not justified
4. **Simple REST APIs**: OpenAPI/Swagger simpler
5. **When No Reuse Needed**: Code duplication acceptable

## Key Benefits of RDF for Code Generation

### 1. Single Source of Truth

One ontology is your system's contract:
```
Domain Knowledge (RDF) → All Code Derives From This
```

### 2. Automatic Synchronization

Change one field, regenerate all languages:
```bash
# Add "verified" field to User
# Edit app.ttl
app:verified a rdf:Property ;
  rdfs:domain app:User ;
  rdfs:range xsd:boolean .

# Regenerate - all languages updated automatically
ggen ontology generate app.ttl --language typescript --output ts-models.ts
ggen ontology generate app.ttl --language python --output py-models.py
ggen ontology generate app.ttl --language rust --output rs-models.rs
```

### 3. Machine-Readable Contracts

APIs become self-describing:

```sparql
# Find API endpoints and their parameters
SELECT ?endpoint ?parameter ?type WHERE {
  ?endpoint rdf:type api:Endpoint ;
    api:hasParameter ?parameter .
  ?parameter api:parameterType ?type .
}
```

### 4. Knowledge Reuse

The same ontology works for:
- Code generation
- Database schema
- API documentation
- Data validation
- Testing
- AI training

### 5. Extensibility Without Breakage

Add new properties without breaking existing code:

```turtle
# New requirement: phone number
app:phone a rdf:Property ;
  rdfs:domain app:User ;
  rdfs:range xsd:string ;
  rdf:comment "User phone number (optional)" .

# Regenerate - all languages add the field
# Existing code using User continues to work
```

## RDF Ontologies vs Traditional Schemas

| Aspect | Database Schema | SQL Schema | RDF Ontology |
|--------|-----------------|-----------|--------------|
| **Definition** | Visual diagrams | DDL statements | Semantic description |
| **Machine Readable** | No | Partially | Yes, fully |
| **Queryable** | Entity-Relationship | SQL | SPARQL |
| **Extensible** | Hard (schema migration) | Hard (ALTER TABLE) | Easy (new triples) |
| **Standardized** | Vendor-specific | SQL standard | W3C standard |
| **Portable** | Locked to DB | Portable | Fully portable |
| **Business Logic** | Separate | Constraints | Integrated |

## When to Adopt RDF

### Readiness Questions

✅ **Do you have...**
- Multiple implementations in different languages?
- Complex domain knowledge to model?
- Need for AI/ML integration?
- Strict type safety requirements?
- Multiple teams managing the same domain?

→ **RDF is right for you**

❌ **Avoid RDF if you have...**
- Single language, single database
- Simple CRUD operations
- Highly dynamic schema
- Deep aversion to semantic modeling
- Very tight performance constraints

## Limitations to Accept

### RDF Learning Curve

SPARQL queries take time to master:
```sparql
# Requires understanding:
# - Graph patterns
# - Variable binding
# - Triple structure
# - Namespace resolution
```

### Triple Storage Overhead

RDF is verbose for simple data:
```turtle
# 3 simple values = 9 lines
ex:User rdf:type rdfs:Class .
ex:name rdfs:domain ex:User .
ex:name rdfs:range xsd:string .
ex:email rdfs:domain ex:User .
ex:email rdfs:range xsd:string .
ex:active rdfs:domain ex:User .
ex:active rdfs:range xsd:boolean .
```

### Query Performance

SPARQL queries on large graphs need optimization:
```sparql
# This might be slow on 1M+ triples
SELECT ?class WHERE {
  ?class rdf:type rdfs:Class .
}

# Better: add more constraints
SELECT ?class WHERE {
  ?class rdf:type rdfs:Class .
  ?class rdfs:label ?label .
  FILTER (STRLEN(?label) > 0)
}
```

## Summary

RDF is powerful for:
- ✅ Polyglot systems (sync types across languages)
- ✅ Complex knowledge modeling
- ✅ Machine-readable domain contracts
- ✅ Automatic code generation
- ✅ Extensible architectures

RDF requires:
- ⚠️ Learning curve (SPARQL, graph thinking)
- ⚠️ Semantic modeling discipline
- ⚠️ Acceptance of verbosity for small data

**Best practice**: Use RDF where its benefits outweigh the overhead - polyglot projects, complex domains, knowledge-intensive systems.
