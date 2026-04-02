<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->
**Table of Contents**

- [Skill Transition Matrix: Traditional → RDF-First Development](#skill-transition-matrix-traditional-%E2%86%92-rdf-first-development)
  - [Purpose](#purpose)
  - [Skill Transition Matrix](#skill-transition-matrix)
    - [1. Query Languages: SQL → SPARQL](#1-query-languages-sql-%E2%86%92-sparql)
    - [2. Data Formats: JSON/YAML → Turtle (TTL)](#2-data-formats-jsonyaml-%E2%86%92-turtle-ttl)
    - [3. Type Systems: Static Typing → SHACL Constraints](#3-type-systems-static-typing-%E2%86%92-shacl-constraints)
    - [4. Architecture: Class Hierarchies → RDF Ontologies](#4-architecture-class-hierarchies-%E2%86%92-rdf-ontologies)
    - [5. Data Modeling: ER Diagrams → RDF Graphs](#5-data-modeling-er-diagrams-%E2%86%92-rdf-graphs)
    - [6. Templating: Jinja2/Mustache → Tera (SPARQL-aware)](#6-templating-jinja2mustache-%E2%86%92-tera-sparql-aware)
    - [7. Testing: Unit Tests → Property-Based Tests](#7-testing-unit-tests-%E2%86%92-property-based-tests)
    - [8. Error Handling: Exceptions → Result<T, E>](#8-error-handling-exceptions-%E2%86%92-resultt-e)
    - [9. Reasoning: Imperative Logic → Declarative Inference](#9-reasoning-imperative-logic-%E2%86%92-declarative-inference)
    - [10. Versioning: Git Commits → Knowledge Graph Evolution](#10-versioning-git-commits-%E2%86%92-knowledge-graph-evolution)
    - [11. Performance: O(n) Analysis → Graph Complexity](#11-performance-on-analysis-%E2%86%92-graph-complexity)
    - [12. API Design: REST → Hypermedia (HATEOAS)](#12-api-design-rest-%E2%86%92-hypermedia-hateoas)
    - [13. Code Generation: String Templates → RDF Projections](#13-code-generation-string-templates-%E2%86%92-rdf-projections)
    - [14. Debugging: Print Statements → RDF Introspection](#14-debugging-print-statements-%E2%86%92-rdf-introspection)
    - [15. Documentation: Markdown → RDF Schema + Generated Docs](#15-documentation-markdown-%E2%86%92-rdf-schema--generated-docs)
    - [16. Collaboration: Code Reviews → Ontology Reviews](#16-collaboration-code-reviews-%E2%86%92-ontology-reviews)
  - [Learning Paths by Background](#learning-paths-by-background)
    - [For SQL Developers](#for-sql-developers)
    - [For OOP Developers (Java/C&#035;)](#for-oop-developers-javac)
    - [For Frontend Developers (React/Vue)](#for-frontend-developers-reactvue)
    - [For DevOps Engineers](#for-devops-engineers)
  - [Self-Assessment Checklist](#self-assessment-checklist)
    - [RDF Fundamentals](#rdf-fundamentals)
    - [SPARQL Querying](#sparql-querying)
    - [SHACL Validation](#shacl-validation)
    - [Ontology Design](#ontology-design)
    - [Code Generation (ggen)](#code-generation-ggen)
    - [Rust Integration](#rust-integration)
  - [Common Anti-Patterns to Avoid](#common-anti-patterns-to-avoid)
    - [1. The "JSON in Turtle" Anti-Pattern](#1-the-json-in-turtle-anti-pattern)
    - [2. The "Database ID" Anti-Pattern](#2-the-database-id-anti-pattern)
    - [3. The "String Everything" Anti-Pattern](#3-the-string-everything-anti-pattern)
    - [4. The "Procedural SPARQL" Anti-Pattern](#4-the-procedural-sparql-anti-pattern)
    - [5. The "Hardcoded Schema" Anti-Pattern](#5-the-hardcoded-schema-anti-pattern)
  - [Next Steps](#next-steps)
    - [Immediate Actions (Today)](#immediate-actions-today)
    - [This Week](#this-week)
    - [This Month](#this-month)
    - [This Quarter](#this-quarter)
  - [Resources by Skill Level](#resources-by-skill-level)
    - [Beginner](#beginner)
    - [Intermediate](#intermediate)
    - [Advanced](#advanced)
    - [Expert](#expert)
  - [Conclusion](#conclusion)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

# Skill Transition Matrix: Traditional → RDF-First Development

**Version**: 1.0.0
**Last Updated**: 2026-01-24
**Audience**: Developers transitioning to RDF-first development with ggen

---

## Purpose

This document maps existing developer skills to the new skills required for RDF-first development. It provides learning paths, practice exercises, and self-assessment tools to accelerate your transition from traditional imperative development to declarative, ontology-driven code generation.

**Core Paradigm Shift**: Traditional development focuses on *how* (imperative instructions). RDF-first development focuses on *what* (declarative specifications). Code becomes a projection of truth encoded in RDF ontologies.

---

## Skill Transition Matrix

### 1. Query Languages: SQL → SPARQL

| Traditional Skill | Maps To | New Skill Required | Learning Time | Difficulty |
|-------------------|---------|-------------------|---------------|------------|
| SQL SELECT queries | Pattern matching | SPARQL SELECT with graph patterns | 2-4 hours | Low |
| SQL JOINs | Triple patterns | SPARQL graph traversal | 3-5 hours | Medium |
| SQL WHERE clauses | FILTER expressions | SPARQL FILTER with functions | 1-2 hours | Low |
| SQL aggregates (COUNT, SUM) | Aggregation | SPARQL aggregate queries | 2-3 hours | Low |
| Subqueries | Nested patterns | SPARQL subqueries and OPTIONAL | 4-6 hours | Medium |

**Key Insight**: SQL operates on tables (2D). SPARQL operates on graphs (N-dimensional). Think relationships, not tables.

**Learning Path**:
1. Understand triple patterns: `?subject ?predicate ?object`
2. Practice basic SELECT queries on small RDF graphs
3. Learn graph traversal with property paths
4. Master FILTER expressions for data constraints
5. Combine patterns with UNION, OPTIONAL, MINUS

**Practice Exercise**:
```sparql
# Traditional SQL mindset: SELECT name, email FROM users WHERE age > 18
# RDF-first SPARQL pattern:
PREFIX schema: <http://schema.org/>
SELECT ?name ?email WHERE {
  ?person a schema:Person ;
          schema:name ?name ;
          schema:email ?email ;
          schema:age ?age .
  FILTER(?age > 18)
}
```

**Common Mistakes**:
- Expecting tables instead of triples (RDF has no implicit schema)
- Forgetting to declare namespaces with PREFIX
- Using SQL-style string concatenation (use SPARQL CONCAT)
- Not leveraging property paths for multi-hop traversal

**Resources**:
- ggen examples: `/examples/sparql/basic-queries.rq`
- Official SPARQL 1.1 spec: https://www.w3.org/TR/sparql11-query/
- Interactive tutorial: https://www.w3.org/2009/Talks/0615-qbe/

---

### 2. Data Formats: JSON/YAML → Turtle (TTL)

| Traditional Skill | Maps To | New Skill Required | Learning Time | Difficulty |
|-------------------|---------|-------------------|---------------|------------|
| JSON object syntax | Subject-predicate-object | Turtle triple notation | 1-2 hours | Low |
| YAML hierarchies | Nested blank nodes | Turtle nested structures | 2-3 hours | Low |
| JSON arrays | RDF collections | Turtle lists (rdf:List) | 2-4 hours | Medium |
| JSON schema | RDF vocabularies | OWL/RDFS class definitions | 4-8 hours | Medium |
| Configuration files | Ontology graphs | Turtle instance data | 1-2 hours | Low |

**Key Insight**: JSON/YAML are trees. Turtle encodes graphs. Same data can have multiple valid serializations.

**Learning Path**:
1. Master basic triple syntax: `subject predicate object .`
2. Learn shorthand notations (`;` for same subject, `,` for same predicate)
3. Understand prefix declarations with `@prefix`
4. Practice nested blank nodes `[ ]`
5. Use collections for ordered lists `( item1 item2 )`

**Practice Exercise**:
```turtle
# Traditional JSON:
# {"name": "Alice", "age": 30, "friends": ["Bob", "Carol"]}

# RDF-first Turtle:
@prefix schema: <http://schema.org/> .
@prefix ex: <http://example.org/> .

ex:Alice a schema:Person ;
    schema:name "Alice" ;
    schema:age 30 ;
    schema:knows ex:Bob, ex:Carol .

ex:Bob a schema:Person .
ex:Carol a schema:Person .
```

**Common Mistakes**:
- Forgetting trailing `.` on final triple in block
- Mixing `;` and `,` incorrectly (`;` = same subject, `,` = same predicate)
- Not declaring prefixes before use
- Using URIs without angle brackets `<>`
- Confusing blank nodes `[]` with named nodes

**Resources**:
- ggen templates: `.specify/templates/rdf-helpers/*.ttl.template`
- Turtle primer: https://www.w3.org/TR/turtle/
- RDF validator: http://www.rdfvalidator.org/

---

### 3. Type Systems: Static Typing → SHACL Constraints

| Traditional Skill | Maps To | New Skill Required | Learning Time | Difficulty |
|-------------------|---------|-------------------|---------------|------------|
| Class definitions | RDF classes | OWL/RDFS class hierarchies | 3-5 hours | Medium |
| Type annotations | Property shapes | SHACL sh:property constraints | 4-8 hours | High |
| Interface contracts | Node shapes | SHACL sh:NodeShape | 4-6 hours | High |
| Enum types | sh:in constraints | SHACL closed value lists | 2-3 hours | Medium |
| Validation rules | SPARQL constraints | SHACL SPARQL-based validation | 6-10 hours | High |

**Key Insight**: Traditional types are compile-time. SHACL validates runtime data graphs. Both can coexist (RDF → Rust types via ggen).

**Learning Path**:
1. Understand SHACL shapes vs RDF classes (shapes validate, classes classify)
2. Learn property constraints: `sh:datatype`, `sh:minCount`, `sh:maxCount`
3. Practice cardinality constraints (required vs optional properties)
4. Master value constraints: `sh:in`, `sh:minInclusive`, `sh:pattern`
5. Combine shapes with `sh:and`, `sh:or`, `sh:not` for complex validation

**Practice Exercise**:
```turtle
# Traditional TypeScript:
# interface User { name: string; age: number; email?: string; }

# RDF-first SHACL:
@prefix sh: <http://www.w3.org/ns/shacl#> .
@prefix schema: <http://schema.org/> .
@prefix xsd: <http://www.w3.org/2001/XMLSchema#> .

:UserShape a sh:NodeShape ;
    sh:targetClass schema:Person ;
    sh:property [
        sh:path schema:name ;
        sh:datatype xsd:string ;
        sh:minCount 1 ;
        sh:maxCount 1 ;
    ] ;
    sh:property [
        sh:path schema:age ;
        sh:datatype xsd:integer ;
        sh:minCount 1 ;
        sh:minInclusive 0 ;
    ] ;
    sh:property [
        sh:path schema:email ;
        sh:datatype xsd:string ;
        sh:maxCount 1 ;
        sh:pattern "^[^@]+@[^@]+\\.[^@]+$" ;
    ] .
```

**Common Mistakes**:
- Confusing SHACL shapes with OWL classes (shapes validate, classes infer)
- Forgetting `sh:targetClass` or `sh:targetNode` (shapes need targets)
- Using `sh:minCount 0` (redundant, omit for optional properties)
- Not escaping regex patterns in `sh:pattern`
- Expecting closed-world assumption (SHACL is open-world)

**Resources**:
- ggen validation: `ggen validate` command (uses SHACL)
- SHACL spec: https://www.w3.org/TR/shacl/
- SHACL playground: https://shacl-playground.zazuko.com/

---

### 4. Architecture: Class Hierarchies → RDF Ontologies

| Traditional Skill | Maps To | New Skill Required | Learning Time | Difficulty |
|-------------------|---------|-------------------|---------------|------------|
| Inheritance (OOP) | rdfs:subClassOf | OWL class hierarchies | 3-5 hours | Medium |
| Composition | RDF properties | Object properties and data properties | 2-4 hours | Medium |
| Polymorphism | Multiple types | RDF multiple classification | 2-3 hours | Low |
| Abstract classes | OWL restrictions | OWL class expressions | 6-10 hours | High |
| Interfaces | Property domains/ranges | RDFS domain/range constraints | 3-5 hours | Medium |

**Key Insight**: OOP hierarchies are rigid trees. RDF ontologies are flexible graphs. Multiple inheritance is natural in RDF.

**Learning Path**:
1. Understand `rdf:type` vs `rdfs:subClassOf` (instance vs subclass)
2. Learn property hierarchies with `rdfs:subPropertyOf`
3. Practice domain/range constraints: `rdfs:domain`, `rdfs:range`
4. Master OWL equivalence: `owl:equivalentClass`, `owl:sameAs`
5. Combine restrictions: `owl:Restriction`, `owl:onProperty`, `owl:someValuesFrom`

**Practice Exercise**:
```turtle
# Traditional Java:
# abstract class Vehicle { String model; }
# class Car extends Vehicle { int doors; }

# RDF-first Ontology:
@prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .
@prefix owl: <http://www.w3.org/2002/07/owl#> .
@prefix ex: <http://example.org/> .

ex:Vehicle a owl:Class ;
    rdfs:label "Vehicle" ;
    rdfs:comment "Abstract base class for all vehicles" .

ex:Car a owl:Class ;
    rdfs:subClassOf ex:Vehicle ;
    rdfs:label "Car" .

ex:model a owl:DatatypeProperty ;
    rdfs:domain ex:Vehicle ;
    rdfs:range xsd:string .

ex:doors a owl:DatatypeProperty ;
    rdfs:domain ex:Car ;
    rdfs:range xsd:integer .

# Instance:
ex:MyCar a ex:Car ;
    ex:model "Tesla Model 3" ;
    ex:doors 4 .
```

**Common Mistakes**:
- Confusing instances with classes (use `rdf:type` for instances)
- Overusing `owl:equivalentClass` (semantic identity is strong claim)
- Forgetting to declare property types (`owl:DatatypeProperty` vs `owl:ObjectProperty`)
- Not leveraging inference (reasoners infer implicit triples)
- Creating circular subclass hierarchies

**Resources**:
- ggen ontologies: `.specify/*.ttl` (domain ontologies)
- OWL primer: https://www.w3.org/TR/owl2-primer/
- Protégé editor: https://protege.stanford.edu/

---

### 5. Data Modeling: ER Diagrams → RDF Graphs

| Traditional Skill | Maps To | New Skill Required | Learning Time | Difficulty |
|-------------------|---------|-------------------|---------------|------------|
| Entity-Relationship | Subject-Predicate-Object | Triple-based modeling | 2-4 hours | Medium |
| Primary keys | URIs | Unique resource identifiers | 1-2 hours | Low |
| Foreign keys | Object properties | RDF references | 2-3 hours | Low |
| Join tables | Reified statements | RDF reification or blank nodes | 4-6 hours | Medium |
| Normalization | Graph patterns | RDF graph design patterns | 6-10 hours | High |

**Key Insight**: ER models normalize to avoid redundancy. RDF graphs embrace redundancy for flexible querying.

**Learning Path**:
1. Map entities to RDF classes, attributes to properties
2. Design URI schemes for stable identifiers
3. Use object properties for relationships (not foreign keys)
4. Practice blank nodes for intermediate objects
5. Learn graph patterns: star, chain, tree, lattice

**Practice Exercise**:
```turtle
# Traditional ER: User (id, name) —(1:N)— Order (id, user_id, total)

# RDF-first Graph:
@prefix schema: <http://schema.org/> .
@prefix ex: <http://example.org/> .

ex:user123 a schema:Person ;
    schema:name "Alice" ;
    ex:hasOrder ex:order456, ex:order789 .

ex:order456 a schema:Order ;
    schema:orderNumber "456" ;
    schema:customer ex:user123 ;
    schema:totalPrice 99.99 .

ex:order789 a schema:Order ;
    schema:orderNumber "789" ;
    schema:customer ex:user123 ;
    schema:totalPrice 149.99 .
```

**Common Mistakes**:
- Using database IDs instead of URIs (IDs are local, URIs are global)
- Creating synthetic join entities (use direct properties instead)
- Not planning URI namespaces (inconsistent URIs cause fragmentation)
- Modeling everything as strings (use typed literals: xsd:integer, xsd:date)
- Over-normalizing graphs (denormalization is acceptable in RDF)

**Resources**:
- RDF data modeling: https://www.w3.org/TR/swbp-xsch-datatypes/
- Linked Data patterns: http://patterns.dataincubator.org/

---

### 6. Templating: Jinja2/Mustache → Tera (SPARQL-aware)

| Traditional Skill | Maps To | New Skill Required | Learning Time | Difficulty |
|-------------------|---------|-------------------|---------------|------------|
| Variable interpolation | Tera variables | `{{ variable }}` syntax | 1 hour | Low |
| Conditionals (if/else) | Tera control flow | `{% if %}` blocks | 1-2 hours | Low |
| Loops (for) | Tera iteration | `{% for %}` over SPARQL results | 2-3 hours | Medium |
| Filters (uppercase, etc.) | Tera filters | Built-in + custom filters | 2-4 hours | Medium |
| Template inheritance | Tera extends/blocks | `{% extends %}` and `{% block %}` | 3-5 hours | Medium |

**Key Insight**: Traditional templates iterate over JSON/arrays. Tera templates iterate over SPARQL query results (RDF graphs).

**Learning Path**:
1. Master basic Tera syntax: variables, filters, comments
2. Learn control flow: `if`, `for`, `match`
3. Practice SPARQL integration: query results as context
4. Use custom filters for code generation (snake_case, PascalCase)
5. Organize templates with inheritance and macros

**Practice Exercise**:
```tera
{# Traditional Jinja2: #}
{# {% for user in users %} #}
{#   <div>{{ user.name }}</div> #}
{# {% endfor %} #}

{# RDF-first Tera (SPARQL context): #}
{% for row in sparql_results %}
pub struct {{ row.className | pascal_case }} {
    {% for prop in row.properties %}
    pub {{ prop.name | snake_case }}: {{ prop.rust_type }},
    {% endfor %}
}
{% endfor %}
```

**Common Mistakes**:
- Not providing SPARQL results in context (template expects graph data)
- Forgetting to escape special characters in generated code
- Using generic variable names (prefer semantic names: `?className`, `?propertyName`)
- Not leveraging custom filters (ggen provides Rust-specific filters)
- Hardcoding logic in templates (push to SPARQL queries instead)

**Resources**:
- ggen templates: `/templates/*.tera`
- Tera documentation: https://tera.netlify.app/docs/
- Custom filters: `crates/ggen-core/src/template/filters.rs`

---

### 7. Testing: Unit Tests → Property-Based Tests

| Traditional Skill | Maps To | New Skill Required | Learning Time | Difficulty |
|-------------------|---------|-------------------|---------------|------------|
| Example-based tests | Property tests | Proptest strategies | 4-6 hours | High |
| Mocking dependencies | Real collaborators | Chicago TDD (no mocks) | 3-5 hours | Medium |
| Assertion libraries | Behavior verification | State-based assertions | 2-3 hours | Low |
| Test fixtures | Deterministic data | RDF fixture graphs | 3-5 hours | Medium |
| Coverage metrics | Mutation testing | Test quality analysis | 4-8 hours | High |

**Key Insight**: Traditional tests verify examples. Property tests verify laws (∀x: P(x) = true).

**Learning Path**:
1. Understand Chicago TDD: state-based, real collaborators, AAA pattern
2. Learn proptest strategies: `any()`, `prop_oneof!()`, custom strategies
3. Practice property definitions: idempotence, commutativity, round-trip
4. Use RDF fixtures for deterministic test data
5. Verify observable outputs (not implementation details)

**Practice Exercise**:
```rust
// Traditional example test:
#[test]
fn test_parse_user() {
    let json = r#"{"name": "Alice", "age": 30}"#;
    let user = parse_user(json).unwrap();
    assert_eq!(user.name, "Alice");
    assert_eq!(user.age, 30);
}

// RDF-first property test:
use proptest::prelude::*;

proptest! {
    #[test]
    fn test_rdf_round_trip(
        name in "[a-zA-Z]{1,20}",
        age in 0u8..120u8
    ) {
        // Arrange: Create RDF graph
        let ttl = format!(
            r#"@prefix ex: <http://example.org/> .
               ex:person ex:name "{}" ; ex:age {} ."#,
            name, age
        );

        // Act: Parse → Serialize → Parse
        let graph1 = parse_turtle(&ttl)?;
        let serialized = serialize_turtle(&graph1)?;
        let graph2 = parse_turtle(&serialized)?;

        // Assert: Graphs are isomorphic
        prop_assert!(graphs_isomorphic(&graph1, &graph2));
    }
}
```

**Common Mistakes**:
- Testing implementation details (test observable behavior)
- Using mocks when real objects suffice (Chicago TDD prefers real collaborators)
- Not shrinking failing inputs (proptest shrinks automatically)
- Writing properties that always pass (tautologies)
- Ignoring edge cases (property tests find them automatically)

**Resources**:
- ggen tests: `crates/*/tests/*.rs`
- Proptest book: https://altsysrq.github.io/proptest-book/
- Chicago TDD guide: `chicago-tdd-tools` crate

---

### 8. Error Handling: Exceptions → Result<T, E>

| Traditional Skill | Maps To | New Skill Required | Learning Time | Difficulty |
|-------------------|---------|-------------------|---------------|------------|
| Try/catch blocks | Result matching | `match result` or `?` operator | 2-3 hours | Medium |
| Custom exceptions | Error enums | `thiserror` derive macros | 3-5 hours | Medium |
| Error messages | Error context | `anyhow::Context` trait | 2-4 hours | Medium |
| Stack traces | Error chains | Nested Result types | 3-5 hours | Medium |
| Validation errors | SHACL violations | Structured validation reports | 4-6 hours | High |

**Key Insight**: Exceptions are invisible in signatures. Result<T, E> makes errors explicit in types.

**Learning Path**:
1. Master `Result<T, E>` pattern matching and `?` operator
2. Learn `thiserror` for custom error types
3. Practice error propagation with `map_err()`
4. Use `anyhow` for application-level error handling
5. Combine with SHACL validation for RDF errors

**Practice Exercise**:
```rust
// Traditional exception-based:
// try {
//     let user = parse_user(json);
//     save_user(user);
// } catch (ParseError e) {
//     log.error("Parse failed: " + e.message);
// }

// RDF-first Result-based:
use thiserror::Error;

#[derive(Error, Debug)]
pub enum RdfError {
    #[error("Invalid Turtle syntax at line {line}: {message}")]
    ParseError { line: usize, message: String },

    #[error("SHACL validation failed: {0}")]
    ValidationError(String),

    #[error("SPARQL query error: {0}")]
    QueryError(String),
}

fn process_rdf(ttl: &str) -> Result<Graph, RdfError> {
    let graph = parse_turtle(ttl)
        .map_err(|e| RdfError::ParseError {
            line: e.line,
            message: e.message,
        })?;

    validate_shacl(&graph)
        .map_err(|e| RdfError::ValidationError(e.to_string()))?;

    Ok(graph)
}

// Usage:
match process_rdf(input) {
    Ok(graph) => println!("Success: {} triples", graph.len()),
    Err(RdfError::ParseError { line, message }) => {
        eprintln!("Parse error at line {}: {}", line, message);
    }
    Err(e) => eprintln!("Error: {}", e),
}
```

**Common Mistakes**:
- Using `unwrap()` in production code (panic instead of error propagation)
- Not adding context to errors (`map_err()` should explain what failed)
- Catching all errors generically (match specific variants)
- Creating error types without `Error` trait (use `thiserror`)
- Ignoring SHACL validation reports (structured validation is precise)

**Resources**:
- Rust error handling: https://doc.rust-lang.org/book/ch09-00-error-handling.html
- thiserror: https://docs.rs/thiserror/
- ggen error types: `crates/ggen-utils/src/error.rs`

---

### 9. Reasoning: Imperative Logic → Declarative Inference

| Traditional Skill | Maps To | New Skill Required | Learning Time | Difficulty |
|-------------------|---------|-------------------|---------------|------------|
| If/else conditionals | SPARQL FILTER | Graph pattern matching | 3-5 hours | Medium |
| Loops (for/while) | SPARQL iteration | Pattern-based traversal | 4-6 hours | Medium |
| Recursive functions | Property paths | SPARQL property path expressions | 5-8 hours | High |
| Business rules | SHACL/SPARQL rules | Declarative constraints | 6-10 hours | High |
| Computed properties | SPARQL CONSTRUCT | Materialized inferences | 5-8 hours | High |

**Key Insight**: Imperative code tells *how* to compute. Declarative rules specify *what* is true. Reasoners infer the rest.

**Learning Path**:
1. Stop thinking "How do I loop?" Start thinking "What pattern matches?"
2. Learn SPARQL property paths: `+` (one or more), `*` (zero or more), `/` (sequence)
3. Practice CONSTRUCT queries (create new triples from existing patterns)
4. Use SHACL rules for validation and inference
5. Understand open-world assumption (absence ≠ falsehood)

**Practice Exercise**:
```rust
// Traditional imperative:
// fn find_ancestors(person_id: String) -> Vec<Person> {
//     let mut ancestors = Vec::new();
//     let mut current = get_person(person_id);
//     while let Some(parent) = current.parent {
//         ancestors.push(parent);
//         current = parent;
//     }
//     ancestors
// }

// RDF-first declarative (SPARQL):
PREFIX fam: <http://example.org/family#>
SELECT ?ancestor WHERE {
  :PersonX fam:hasParent+ ?ancestor .
}
# Property path `+` means "one or more hasParent relationships"
# No loops, no mutation, no iteration logic
```

**Common Mistakes**:
- Writing procedural SPARQL (chaining multiple queries instead of one pattern)
- Not leveraging property paths (manually traversing graphs)
- Expecting closed-world reasoning (RDF is open-world)
- Confusing validation (SHACL) with inference (OWL/RDFS)
- Materializing all inferences (query-time inference is often sufficient)

**Resources**:
- SPARQL property paths: https://www.w3.org/TR/sparql11-query/#propertypaths
- OWL reasoning: https://www.w3.org/TR/owl2-primer/#Reasoning
- ggen inference: `crates/ggen-core/src/rdf/inference.rs`

---

### 10. Versioning: Git Commits → Knowledge Graph Evolution

| Traditional Skill | Maps To | New Skill Required | Learning Time | Difficulty |
|-------------------|---------|-------------------|---------------|------------|
| Git commits | RDF snapshots | Temporal RDF versioning | 4-6 hours | High |
| Diffs | Graph diffs | RDF delta computation | 5-8 hours | High |
| Branching | Ontology forks | Parallel ontology versions | 3-5 hours | Medium |
| Merging | Graph merging | Conflict resolution in RDF | 6-10 hours | High |
| Tags | Named graphs | RDF dataset versioning | 3-5 hours | Medium |

**Key Insight**: Git tracks text. KGC-4D tracks graph evolution (4D = 3D graph + time dimension).

**Learning Path**:
1. Understand named graphs for versioning: `GRAPH <version> { ... }`
2. Learn RDF patch format for graph deltas
3. Practice graph isomorphism for change detection
4. Use SPARQL 1.1 Update for graph modifications
5. Design temporal URIs: `ex:Person/v1`, `ex:Person/v2`

**Practice Exercise**:
```turtle
# Traditional Git:
# commit 1: Add User class
# commit 2: Rename User to Person
# commit 3: Add age property

# RDF-first KGC-4D:
# Named graph for each version:
GRAPH <http://example.org/ontology/v1> {
    ex:User a owl:Class .
}

GRAPH <http://example.org/ontology/v2> {
    ex:Person a owl:Class .
    ex:User owl:equivalentClass ex:Person .  # Migration path
}

GRAPH <http://example.org/ontology/v3> {
    ex:Person a owl:Class ;
        rdfs:subClassOf [
            a owl:Restriction ;
            owl:onProperty ex:age ;
            owl:someValuesFrom xsd:integer
        ] .
}
```

**Common Mistakes**:
- Modifying ontology URIs instead of versioning (breaks stability)
- Not providing migration paths (`owl:equivalentClass`, `owl:deprecated`)
- Treating RDF like text diffs (graph isomorphism ≠ string equality)
- Forgetting named graphs for isolation
- Not documenting breaking changes in ontology evolution

**Resources**:
- Named graphs: https://www.w3.org/TR/rdf11-concepts/#section-dataset
- RDF patch: https://afs.github.io/rdf-patch/
- ggen KGC-4D: `crates/ggen-domain/src/kgc_4d.rs`

---

### 11. Performance: O(n) Analysis → Graph Complexity

| Traditional Skill | Maps To | New Skill Required | Learning Time | Difficulty |
|-------------------|---------|-------------------|---------------|------------|
| Big-O notation | SPARQL complexity | Graph traversal cost analysis | 4-6 hours | High |
| Indexing strategies | RDF indexing | Triple store optimization (SPO, POS, OSP) | 5-8 hours | High |
| Caching | Query result caching | SPARQL query optimization | 3-5 hours | Medium |
| Lazy evaluation | On-demand queries | Incremental graph loading | 4-6 hours | Medium |
| Profiling tools | SPARQL explain plans | Query performance analysis | 3-5 hours | Medium |

**Key Insight**: Algorithmic complexity applies to graphs. But RDF has different costs: triple lookup (O(1)), pattern matching (variable).

**Learning Path**:
1. Understand triple store indexing (SPO, POS, OSP, etc.)
2. Learn SPARQL query optimization (selective patterns first)
3. Practice query planning (minimize intermediate results)
4. Use EXPLAIN for query analysis
5. Benchmark with realistic graph sizes (10K, 100K, 1M triples)

**Practice Exercise**:
```rust
// Traditional: O(n²) nested loop
// for user in users {
//     for order in orders {
//         if order.user_id == user.id {
//             process(user, order);
//         }
//     }
// }

// RDF-first: O(n) graph traversal with index
// SPARQL joins are index-assisted (SPO index):
PREFIX schema: <http://schema.org/>
SELECT ?user ?order WHERE {
  ?user a schema:Person .           # Index lookup: (?, rdf:type, schema:Person)
  ?order a schema:Order ;           # Index lookup: (?, rdf:type, schema:Order)
         schema:customer ?user .    # Index join: (?, schema:customer, ?user)
}
# Cost: O(users + orders), not O(users × orders)
```

**Common Mistakes**:
- Not considering triple store indexes (assuming sequential scan)
- Placing expensive FILTER early (move to end of WHERE clause)
- Using unbounded property paths (`:parent*` can explode)
- Not limiting results with LIMIT (pagination is essential)
- Ignoring SPARQL query plans (EXPLAIN shows actual execution)

**Resources**:
- Oxigraph benchmarks: `benches/rdf_benchmarks.rs`
- SPARQL optimization: https://www.w3.org/2001/sw/DataAccess/tests/
- ggen SLOs: `Makefile.toml` (target: slo-check)

---

### 12. API Design: REST → Hypermedia (HATEOAS)

| Traditional Skill | Maps To | New Skill Required | Learning Time | Difficulty |
|-------------------|---------|-------------------|---------------|------------|
| RESTful endpoints | RDF resources | Dereferenceable URIs | 3-5 hours | Medium |
| JSON-LD responses | RDF serialization | Content negotiation (Turtle, JSON-LD) | 4-6 hours | Medium |
| HATEOAS links | RDF links | Hypermedia-driven navigation | 5-8 hours | High |
| Schema.org | Vocabularies | Reusing standard vocabularies | 2-4 hours | Medium |
| Pagination | SPARQL LIMIT/OFFSET | Cursor-based pagination | 3-5 hours | Medium |

**Key Insight**: REST APIs return opaque JSON. Linked Data APIs return self-describing RDF with semantic links.

**Learning Path**:
1. Design URI schemes for REST resources
2. Learn content negotiation (Accept: application/ld+json, text/turtle)
3. Embed links in RDF responses (rdfs:seeAlso, schema:url)
4. Use standard vocabularies (Schema.org, Dublin Core)
5. Implement SPARQL endpoints for querying

**Practice Exercise**:
```turtle
# Traditional REST: GET /users/123 → {"id": 123, "name": "Alice"}

# RDF-first Linked Data: GET /users/123 (Accept: text/turtle)
@prefix schema: <http://schema.org/> .
@prefix hydra: <http://www.w3.org/ns/hydra/core#> .

<http://example.org/users/123> a schema:Person ;
    schema:name "Alice" ;
    schema:email "alice@example.org" ;
    schema:url <http://example.org/users/123> ;
    hydra:collection <http://example.org/users> ;
    hydra:next <http://example.org/users/124> ;
    hydra:previous <http://example.org/users/122> .

# Self-describing: type, properties, links all in RDF
```

**Common Mistakes**:
- Not making URIs dereferenceable (URIs should return RDF when fetched)
- Using opaque IDs instead of URIs (http://example.org/user/123, not just "123")
- Inventing custom vocabularies (reuse Schema.org, Dublin Core, FOAF)
- Not supporting content negotiation (clients need Turtle, JSON-LD, RDF/XML)
- Exposing implementation details (URIs should be semantic, not database IDs)

**Resources**:
- Linked Data principles: https://www.w3.org/DesignIssues/LinkedData.html
- JSON-LD: https://json-ld.org/
- Schema.org: https://schema.org/

---

### 13. Code Generation: String Templates → RDF Projections

| Traditional Skill | Maps To | New Skill Required | Learning Time | Difficulty |
|-------------------|---------|-------------------|---------------|------------|
| String interpolation | SPARQL bindings | Query results → template context | 3-5 hours | Medium |
| AST manipulation | RDF graph transformation | SPARQL CONSTRUCT | 5-8 hours | High |
| Code scaffolding | Ontology-driven generation | ggen sync pipeline (μ₁-μ₅) | 6-10 hours | High |
| Meta-programming | Reflection from RDF | Type generation from SHACL shapes | 6-10 hours | High |
| IDE features (autocomplete) | Ontology introspection | SPARQL-based schema queries | 4-6 hours | Medium |

**Key Insight**: Traditional codegen manipulates strings. RDF-first codegen projects truth from ontologies (deterministic, traceable).

**Learning Path**:
1. Understand ggen pipeline: μ₁ (Normalize) → μ₅ (Receipt)
2. Write SPARQL queries to extract schema (classes, properties, constraints)
3. Design Tera templates for target language (Rust, TypeScript, etc.)
4. Practice deterministic formatting (canonical output)
5. Generate cryptographic receipts for auditability

**Practice Exercise**:
```sparql
# Extract Rust struct definitions from SHACL shapes:
PREFIX sh: <http://www.w3.org/ns/shacl#>
PREFIX schema: <http://schema.org/>
PREFIX xsd: <http://www.w3.org/2001/XMLSchema#>

SELECT ?className ?propName ?rustType WHERE {
  ?shape a sh:NodeShape ;
         sh:targetClass ?class ;
         sh:property ?propShape .

  ?propShape sh:path ?prop ;
             sh:datatype ?datatype .

  BIND(REPLACE(STR(?class), "^.*/", "") AS ?className)
  BIND(REPLACE(STR(?prop), "^.*/", "") AS ?propName)
  BIND(IF(?datatype = xsd:string, "String",
       IF(?datatype = xsd:integer, "i64",
       IF(?datatype = xsd:boolean, "bool", "Value"))) AS ?rustType)
}
```

**Tera template**:
```tera
{% for row in sparql_results %}
pub struct {{ row.className | pascal_case }} {
    pub {{ row.propName | snake_case }}: {{ row.rustType }},
}
{% endfor %}
```

**Common Mistakes**:
- Hardcoding schemas in templates (query RDF dynamically)
- Not validating SHACL before generation (garbage in → garbage out)
- Skipping canonicalization (non-deterministic output breaks reproducibility)
- Ignoring receipts (no audit trail for compliance)
- Generating code without tests (use property tests for generated code)

**Resources**:
- ggen pipeline: `V6_RELEASE_NOTES.md` (μ₁-μ₅ explanation)
- Templates: `/templates/*.tera`
- SPARQL + Tera: `crates/ggen-core/src/template/mod.rs`

---

### 14. Debugging: Print Statements → RDF Introspection

| Traditional Skill | Maps To | New Skill Required | Learning Time | Difficulty |
|-------------------|---------|-------------------|---------------|------------|
| Print debugging | SPARQL queries | Graph introspection queries | 2-3 hours | Low |
| Stack traces | RDF provenance | PROV-O tracing | 5-8 hours | High |
| Breakpoints | Graph snapshots | Named graph versioning | 3-5 hours | Medium |
| Logs | Audit trails | ggen receipt logs | 2-4 hours | Medium |
| Profilers | SPARQL EXPLAIN | Query performance analysis | 3-5 hours | Medium |

**Key Insight**: Traditional debugging steps through imperative code. RDF debugging queries the graph state at any point.

**Learning Path**:
1. Write diagnostic SPARQL queries (count triples, list classes, find orphans)
2. Use ASK queries for existence checks (returns true/false)
3. Inspect named graphs for versioning (GRAPH keyword)
4. Read ggen receipts for generation provenance
5. Enable SPARQL query logging for debugging

**Practice Exercise**:
```sparql
# Traditional: console.log(user.orders.length)

# RDF-first diagnostic queries:

# Count orders per user:
SELECT ?user (COUNT(?order) AS ?orderCount) WHERE {
  ?user a schema:Person .
  OPTIONAL { ?user schema:owns ?order . ?order a schema:Order . }
}
GROUP BY ?user

# Find users without orders (data quality check):
SELECT ?user WHERE {
  ?user a schema:Person .
  FILTER NOT EXISTS { ?user schema:owns ?order . }
}

# Check if specific property exists (ASK query):
ASK WHERE {
  ex:Alice schema:email ?email .
}
# Returns: true or false
```

**Common Mistakes**:
- Not using ASK for existence checks (SELECT returns empty vs false)
- Forgetting OPTIONAL in diagnostic queries (missing data → no results)
- Not logging SPARQL queries during debugging (opaque failures)
- Ignoring ggen receipts (contain full provenance chain)
- Using deprecated `log::debug!` in library code (use alert macros)

**Resources**:
- Diagnostic queries: `examples/sparql/diagnostics.rq`
- ggen receipts: `.ggen/receipts/*.json`
- SPARQL debugging: https://www.w3.org/TR/sparql11-query/#ask

---

### 15. Documentation: Markdown → RDF Schema + Generated Docs

| Traditional Skill | Maps To | New Skill Required | Learning Time | Difficulty |
|-------------------|---------|-------------------|---------------|------------|
| README files | RDFS/OWL documentation | rdfs:label, rdfs:comment | 2-3 hours | Low |
| API docs | SPARQL-generated docs | Template-driven documentation | 4-6 hours | Medium |
| Diagrams | RDF visualizations | Graph rendering tools | 3-5 hours | Medium |
| Changelog | Ontology evolution | Version-diffing with named graphs | 4-6 hours | Medium |
| Examples | RDF fixtures | Turtle instance data | 2-4 hours | Medium |

**Key Insight**: Traditional docs are manual and stale. RDF-first docs are generated from the source of truth (ontologies).

**Learning Path**:
1. Add `rdfs:label` and `rdfs:comment` to all RDF classes and properties
2. Use `skos:example` for usage examples in Turtle
3. Generate markdown from RDF using `cargo make speckit-render`
4. Visualize graphs with tools (Graphviz, yEd, WebVOWL)
5. Document ontology evolution in named graphs

**Practice Exercise**:
```turtle
# Traditional: Manually written README.md

# RDF-first: Self-documenting ontology
@prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .
@prefix skos: <http://www.w3.org/2004/02/skos/core#> .
@prefix ex: <http://example.org/> .

ex:Person a owl:Class ;
    rdfs:label "Person" ;
    rdfs:comment "Represents an individual human being with identity and attributes." ;
    skos:example """
        ex:Alice a ex:Person ;
            ex:name "Alice" ;
            ex:age 30 .
    """ .

ex:name a owl:DatatypeProperty ;
    rdfs:label "name" ;
    rdfs:comment "The full name of a person." ;
    rdfs:domain ex:Person ;
    rdfs:range xsd:string .

# Generate docs: cargo make speckit-render
# Output: .specify/specs/*/spec.md (auto-generated from TTL)
```

**Common Mistakes**:
- Skipping rdfs:label/comment (undocumented ontologies are unusable)
- Writing docs in separate files (keep truth in RDF)
- Not regenerating docs after ontology changes (stale docs)
- Using narrative docs instead of structured RDF (RDF is queryable)
- Forgetting examples (skos:example is essential for learning)

**Resources**:
- ggen docs generation: `cargo make speckit-render`
- RDFS vocabulary: https://www.w3.org/TR/rdf-schema/
- SKOS: https://www.w3.org/2004/02/skos/

---

### 16. Collaboration: Code Reviews → Ontology Reviews

| Traditional Skill | Maps To | New Skill Required | Learning Time | Difficulty |
|-------------------|---------|-------------------|---------------|------------|
| Pull requests | Ontology diffs | RDF graph comparison | 4-6 hours | Medium |
| Code comments | SHACL validation | Schema compliance checks | 3-5 hours | Medium |
| Linting (clippy) | SHACL validation | Automated ontology quality checks | 4-6 hours | Medium |
| Style guides | Ontology conventions | URI naming, namespace standards | 2-4 hours | Medium |
| Architectural reviews | Ontology design patterns | Graph structure analysis | 6-10 hours | High |

**Key Insight**: Traditional code reviews check syntax and logic. Ontology reviews check semantic correctness and design patterns.

**Learning Path**:
1. Review SHACL validation reports for errors
2. Check URI naming conventions (kebab-case, semantic paths)
3. Verify namespace declarations (all prefixes defined)
4. Analyze graph structure (star, chain, lattice patterns)
5. Ensure ontology evolution is backward-compatible

**Practice Exercise**:
```bash
# Traditional: git diff main...feature-branch | review

# RDF-first: Ontology review workflow
# 1. Validate SHACL compliance
ggen validate .specify/specs/*/feature.ttl

# 2. Check for breaking changes
sparql --data old.ttl --data new.ttl --query diff.rq

# 3. Visualize graph diff
# (use graph diff tool to see added/removed triples)

# 4. Verify examples
# Run SPARQL queries against example data

# 5. Generate docs to review
cargo make speckit-render
git diff .specify/specs/*/spec.md
```

**SPARQL diff query**:
```sparql
# Find triples added in new version:
SELECT ?s ?p ?o WHERE {
  GRAPH <new> { ?s ?p ?o }
  FILTER NOT EXISTS { GRAPH <old> { ?s ?p ?o } }
}
```

**Common Mistakes**:
- Reviewing generated code instead of RDF source (fix TTL, not Rust)
- Not validating SHACL before review (broken schemas waste time)
- Ignoring breaking changes (renaming URIs breaks consumers)
- Skipping documentation review (rdfs:comment is code)
- Not testing SPARQL queries (broken queries = broken codegen)

**Resources**:
- ggen validation: `ggen validate`
- Graph diff tools: RDF::Trine::Graph, Apache Jena
- Code review guide: `CONTRIBUTING.md`

---

## Learning Paths by Background

### For SQL Developers
**Fast Track** (10-15 hours):
1. SQL → SPARQL (3 hours)
2. JSON → Turtle (2 hours)
3. ER diagrams → RDF graphs (4 hours)
4. Schema validation → SHACL (6 hours)

**Practice Project**: Convert an existing SQL database schema to RDF ontology with SHACL constraints.

---

### For OOP Developers (Java/C#)
**Fast Track** (12-18 hours):
1. Class hierarchies → RDF ontologies (4 hours)
2. Type systems → SHACL (6 hours)
3. Exceptions → Result<T,E> (3 hours)
4. Unit tests → Property tests (5 hours)

**Practice Project**: Model a domain (e-commerce, social network) in RDF and generate type-safe Rust code with ggen.

---

### For Frontend Developers (React/Vue)
**Fast Track** (8-12 hours):
1. JSON/YAML → Turtle (2 hours)
2. REST APIs → Linked Data (5 hours)
3. Jinja2 → Tera templates (3 hours)
4. Component props → RDF properties (2 hours)

**Practice Project**: Build a hypermedia-driven UI that consumes RDF/JSON-LD APIs.

---

### For DevOps Engineers
**Fast Track** (10-15 hours):
1. YAML configs → Turtle (2 hours)
2. Infrastructure as Code → RDF-driven generation (6 hours)
3. CI/CD → ggen pipeline (4 hours)
4. Monitoring → RDF audit trails (3 hours)

**Practice Project**: Generate Kubernetes manifests from RDF infrastructure specifications.

---

## Self-Assessment Checklist

Rate yourself (1 = beginner, 5 = expert):

### RDF Fundamentals
- [ ] (1-5) I can read and write Turtle syntax fluently
- [ ] (1-5) I understand subject-predicate-object triples
- [ ] (1-5) I can design URI namespaces for ontologies
- [ ] (1-5) I can use blank nodes appropriately
- [ ] (1-5) I understand RDF vs XML/JSON differences

### SPARQL Querying
- [ ] (1-5) I can write SELECT queries with graph patterns
- [ ] (1-5) I can use FILTER for data constraints
- [ ] (1-5) I can traverse graphs with property paths
- [ ] (1-5) I can aggregate results (COUNT, SUM, GROUP BY)
- [ ] (1-5) I can optimize SPARQL query performance

### SHACL Validation
- [ ] (1-5) I can write property shapes for validation
- [ ] (1-5) I understand cardinality constraints (minCount, maxCount)
- [ ] (1-5) I can use value constraints (sh:in, sh:pattern)
- [ ] (1-5) I can combine shapes with sh:and, sh:or, sh:not
- [ ] (1-5) I can debug SHACL validation reports

### Ontology Design
- [ ] (1-5) I can design RDF class hierarchies (rdfs:subClassOf)
- [ ] (1-5) I can define properties with domain/range
- [ ] (1-5) I understand OWL restrictions and axioms
- [ ] (1-5) I can reuse standard vocabularies (Schema.org, Dublin Core)
- [ ] (1-5) I can version ontologies without breaking changes

### Code Generation (ggen)
- [ ] (1-5) I can write SPARQL queries for code generation
- [ ] (1-5) I can design Tera templates for target languages
- [ ] (1-5) I understand ggen pipeline (μ₁-μ₅)
- [ ] (1-5) I can generate deterministic, auditable code
- [ ] (1-5) I can debug codegen failures with receipts

### Rust Integration
- [ ] (1-5) I can map RDF types to Rust types
- [ ] (1-5) I use Result<T,E> for all fallible operations
- [ ] (1-5) I write Chicago TDD tests (state-based, no mocks)
- [ ] (1-5) I can profile RDF processing performance
- [ ] (1-5) I follow ggen conventions (cargo make, Andon signals)

**Scoring**:
- **0-30**: Beginner (start with fundamentals)
- **31-60**: Intermediate (focus on SPARQL and SHACL)
- **61-90**: Advanced (practice ontology design and codegen)
- **91-120**: Expert (contribute to ggen ontologies)

---

## Common Anti-Patterns to Avoid

### 1. The "JSON in Turtle" Anti-Pattern
**Wrong**:
```turtle
ex:data ex:hasJson "{\"name\": \"Alice\", \"age\": 30}" .
```

**Right**:
```turtle
ex:Alice a schema:Person ;
    schema:name "Alice" ;
    schema:age 30 .
```

**Why**: RDF is a graph, not a string format. Model data as triples, not serialized JSON.

---

### 2. The "Database ID" Anti-Pattern
**Wrong**:
```turtle
ex:123 a schema:Person ;
    schema:name "Alice" .
```

**Right**:
```turtle
ex:Alice a schema:Person ;
    ex:id "123" ;
    schema:name "Alice" .
```

**Why**: URIs should be semantic and stable. Database IDs are local and change.

---

### 3. The "String Everything" Anti-Pattern
**Wrong**:
```turtle
ex:Alice schema:age "30" ;
         schema:birthDate "2023-01-15" .
```

**Right**:
```turtle
ex:Alice schema:age 30 ;  # xsd:integer (implicit)
         schema:birthDate "2023-01-15"^^xsd:date .
```

**Why**: Typed literals enable validation, comparison, and inference.

---

### 4. The "Procedural SPARQL" Anti-Pattern
**Wrong**:
```sparql
# Query 1: Get user IDs
SELECT ?userId WHERE { ?user a schema:Person . BIND(?user AS ?userId) }

# Query 2: Get orders for each user (loop in application code)
SELECT ?order WHERE { ?order schema:customer <userId> }
```

**Right**:
```sparql
# Single query with pattern matching
SELECT ?user ?order WHERE {
  ?user a schema:Person .
  ?order a schema:Order ;
         schema:customer ?user .
}
```

**Why**: SPARQL is declarative. Let the triple store optimize joins, not your application.

---

### 5. The "Hardcoded Schema" Anti-Pattern
**Wrong**:
```tera
pub struct User {
    pub name: String,
    pub email: String,
}
```

**Right**:
```tera
{% for row in sparql_results %}
pub struct {{ row.className }} {
    {% for prop in row.properties %}
    pub {{ prop.name }}: {{ prop.rust_type }},
    {% endfor %}
}
{% endfor %}
```

**Why**: Templates should query RDF dynamically. Hardcoding defeats the purpose of ontology-driven generation.

---

## Next Steps

### Immediate Actions (Today)
1. Read 3 RDF examples in `.specify/specs/*/entities.ttl`
2. Run `ggen validate` on an existing TTL file
3. Write 1 SPARQL query to extract data from a spec
4. Complete the self-assessment checklist

### This Week
1. Complete your background's fast track (8-18 hours)
2. Implement the practice project
3. Contribute 1 improvement to ggen ontologies
4. Read 5 existing ggen templates in `/templates/*.tera`

### This Month
1. Master all 16 skill transitions (score 60+ on self-assessment)
2. Generate production code with `ggen sync`
3. Design your own domain ontology
4. Review 3 ontology-related pull requests

### This Quarter
1. Become an RDF-first expert (score 90+ on self-assessment)
2. Teach RDF-first concepts to your team
3. Contribute 5+ ontology improvements to ggen
4. Write advanced SPARQL queries and Tera templates

---

## Resources by Skill Level

### Beginner
- W3C RDF Primer: https://www.w3.org/TR/rdf11-primer/
- W3C Turtle Tutorial: https://www.w3.org/TR/turtle/
- ggen examples: `/examples/rdf/*.ttl`
- SPARQL by example: https://www.w3.org/2009/Talks/0615-qbe/

### Intermediate
- SPARQL 1.1 Query Language: https://www.w3.org/TR/sparql11-query/
- SHACL Specification: https://www.w3.org/TR/shacl/
- OWL 2 Primer: https://www.w3.org/TR/owl2-primer/
- ggen ontologies: `.specify/*.ttl`

### Advanced
- Semantic Web for the Working Ontologist (book)
- RDF 1.1 Semantics: https://www.w3.org/TR/rdf11-mt/
- SPARQL 1.1 Entailment Regimes: https://www.w3.org/TR/sparql11-entailment/
- ggen pipeline internals: `crates/ggen-core/src/`

### Expert
- Knowledge Graphs (book by Hogan et al.)
- SHACL Advanced Features: https://www.w3.org/TR/shacl-af/
- Research papers on RDF query optimization
- ggen contributions: https://github.com/seanchatmangpt/ggen

---

## Conclusion

Transitioning to RDF-first development requires unlearning imperative habits and embracing declarative thinking. This skill matrix provides a roadmap, but the real learning happens through practice.

**Remember**:
- **RDF is truth**: Ontologies are the source, code is the projection
- **SPARQL is your query language**: Stop writing loops, start writing patterns
- **SHACL validates**: Let schemas enforce correctness at runtime
- **ggen generates**: Your job is to model truth, not write boilerplate

Start with the fast track for your background, complete the practice project, and assess yourself weekly. Within a month, you'll think in graphs naturally.

**Welcome to the RDF-first paradigm. The future is declarative.**

---

**Document Version**: 1.0.0
**Maintainers**: ggen core team
**Last Reviewed**: 2026-01-24
**Feedback**: https://github.com/seanchatmangpt/ggen/issues
