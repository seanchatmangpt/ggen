# ggen v5: CONSTRUCT Queries & N3 Rules for Innovative Code Generation

**Date**: 2024-12-14
**Focus**: Moving beyond SELECT to CONSTRUCT and N3 for semantic code synthesis
**Philosophy**: Code is data, data is code - RDF enables both

---

## The Insight: Why CONSTRUCT & N3 Change Everything

### SELECT is Limited (v4 approach)
```sparql
SELECT ?class ?name ?fields WHERE {
    ?class a rdfs:Class ; rdfs:label ?name .
}
```
**Problem**: SELECT returns tabular data. You still need template logic to transform it into code.

### CONSTRUCT is Revolutionary (v5 approach)
```sparql
CONSTRUCT {
    ?struct a :RustStruct ;
            :structName ?name ;
            :derives (:Debug :Clone :Serialize) ;
            :fields ?fieldList .
}
WHERE {
    ?class a rdfs:Class ;
           rdfs:label ?name ;
           :fields ?fieldList .
}
```
**Result**: CONSTRUCT builds NEW RDF graphs that directly represent code structures. The output graph IS the code, just in RDF form.

### N3 is Meta-Programming (v5+ approach)
```n3
# N3 Rule: If struct has UUID field, add Serialize derive
{
    ?struct :fields ?fields .
    ?fields rdf:first [ :fieldType "Uuid" ] .
} => {
    ?struct :auto-derive (:Serialize :Deserialize) .
} .
```
**Result**: N3 rules enable **inference-driven code generation**. The reasoner figures out what code to generate based on semantic patterns.

---

## Part 1: CONSTRUCT-Based Code Generation

### Pattern 1: Domain Model → Rust Structs via CONSTRUCT

**Input Ontology (domain.ttl)**:
```turtle
:User a rdfs:Class ;
    rdfs:label "User" ;
    :fields (
        [ :name "id" ; :type "Uuid" ; :required true ]
        [ :name "email" ; :type "String" ; :required true ]
    ) .
```

**CONSTRUCT Query (generates intermediate code graph)**:
```sparql
PREFIX : <http://example.com/ggen#>
PREFIX code: <http://example.com/codegen#>

CONSTRUCT {
    ?rustStruct a code:RustStruct ;
        code:name ?structName ;
        code:visibility "pub" ;
        code:derives (code:Debug code:Clone code:Serialize code:Deserialize) ;
        code:fields ?fieldGraph .

    ?fieldGraph a code:FieldList ;
        code:field [
            code:fieldName ?fieldName ;
            code:fieldType ?fieldType ;
            code:visibility "pub" ;
        ] .
}
WHERE {
    ?class a rdfs:Class ;
           rdfs:label ?structName ;
           :fields ?fieldList .

    ?fieldList rdf:first ?field .
    ?field :name ?fieldName ;
           :type ?fieldType .

    BIND(IRI(CONCAT(str(?class), "_rust")) AS ?rustStruct)
    BIND(IRI(CONCAT(str(?class), "_fields")) AS ?fieldGraph)
}
```

**Output Graph (code as RDF)**:
```turtle
:User_rust a code:RustStruct ;
    code:name "User" ;
    code:visibility "pub" ;
    code:derives (code:Debug code:Clone code:Serialize code:Deserialize) ;
    code:fields :User_fields .

:User_fields a code:FieldList ;
    code:field [
        code:fieldName "id" ;
        code:fieldType "Uuid" ;
        code:visibility "pub" ;
    ] ;
    code:field [
        code:fieldName "email" ;
        code:fieldType "String" ;
        code:visibility "pub" ;
    ] .
```

**Final Template (trivial - just serializes the code graph)**:
```tera
{% for struct in code_structs %}
#[derive({{ struct.derives | join(", ") }})]
{{ struct.visibility }} struct {{ struct.name }} {
{% for field in struct.fields %}
    {{ field.visibility }} {{ field.name }}: {{ field.type }},
{% endfor %}
}
{% endfor %}
```

**Key Insight**: CONSTRUCT separates **what to generate** (domain model) from **how to represent code** (code ontology). Templates become trivial serializers.

---

### Pattern 2: Relationships → Rust Implementations via CONSTRUCT

**Input (relationships in ontology)**:
```turtle
:User :has_many :Order .
:Order :belongs_to :User .
```

**CONSTRUCT Query (generates impl blocks)**:
```sparql
PREFIX : <http://example.com/ggen#>
PREFIX code: <http://example.com/codegen#>

CONSTRUCT {
    ?implBlock a code:ImplBlock ;
        code:for ?ownerType ;
        code:method [
            code:name ?methodName ;
            code:returnType ?returnType ;
            code:body ?methodBody ;
        ] .
}
WHERE {
    ?owner :has_many ?related .

    BIND(LCASE(STR(?related)) AS ?relatedName)
    BIND(CONCAT("get_", ?relatedName, "s") AS ?methodName)
    BIND(CONCAT("Vec<", STR(?related), ">") AS ?returnType)
    BIND(CONCAT("self.", ?relatedName, "s.clone()") AS ?methodBody)

    BIND(IRI(CONCAT(str(?owner), "_impl")) AS ?implBlock)
    BIND(STR(?owner) AS ?ownerType)
}
```

**Output (code graph)**:
```turtle
:User_impl a code:ImplBlock ;
    code:for "User" ;
    code:method [
        code:name "get_orders" ;
        code:returnType "Vec<Order>" ;
        code:body "self.orders.clone()" ;
    ] .
```

**Generated Rust**:
```rust
impl User {
    pub fn get_orders(&self) -> Vec<Order> {
        self.orders.clone()
    }
}
```

---

### Pattern 3: CONSTRUCT for Code Safety Validation

**CONSTRUCT as Safety Check (poka-yoke)**:
```sparql
PREFIX : <http://example.com/ggen#>
PREFIX safety: <http://example.com/safety#>

CONSTRUCT {
    ?warning a safety:Warning ;
        safety:severity "high" ;
        safety:message ?msg ;
        safety:location ?class .
}
WHERE {
    ?class a rdfs:Class ;
           :codegen-as "struct" .

    # Check for fields without types
    ?class :fields ?fields .
    ?fields rdf:first ?field .
    FILTER NOT EXISTS { ?field :type ?t }

    BIND(CONCAT("Field in ", STR(?class), " missing type annotation") AS ?msg)
    BIND(IRI(CONCAT("warning:", STR(?class))) AS ?warning)
}
```

**If this CONSTRUCT returns non-empty graph → FAIL generation with semantic error**.

---

## Part 2: N3 Rules for Inference-Driven Code Generation

N3 rules enable **forward-chaining inference**: The reasoner discovers new facts, which trigger more rules, building up a complete code graph.

### Pattern 4: N3 Rule for Auto-Derive Inference

**Rules (codegen-rules.n3)**:
```n3
@prefix : <http://example.com/ggen#> .
@prefix code: <http://example.com/codegen#> .
@prefix xsd: <http://www.w3.org/2001/XMLSchema#> .

# Rule 1: Structs with Uuid fields should derive Serialize/Deserialize
{
    ?struct :codegen-as "struct" ;
            :fields ?fields .
    ?fields rdf:first* [ :type "Uuid" ] .
} => {
    ?struct :auto-derive code:Serialize, code:Deserialize .
} .

# Rule 2: Structs with DateTime fields need chrono import
{
    ?struct :codegen-as "struct" ;
            :fields ?fields .
    ?fields rdf:first* [ :type "DateTime<Utc>" ] .
} => {
    ?struct :requires-import "chrono::DateTime" .
    ?struct :requires-import "chrono::Utc" .
} .

# Rule 3: Traits with async methods need async_trait
{
    ?trait :codegen-as "trait" ;
           :methods ?methods .
    ?methods rdf:first* [ :methodAsync true ] .
} => {
    ?trait :requires-macro "async_trait" .
} .

# Rule 4: Repository traits get CRUD methods auto-generated
{
    ?repo rdfs:label ?name .
    ?name log:endsWith "Repository" .
    ?repo :for-entity ?entity .
} => {
    ?repo :auto-method [
        :methodName "create" ;
        :methodParams ( [ :paramType ?entity ] ) ;
        :methodReturns { ?entity } ;
    ] .
    ?repo :auto-method [
        :methodName "find_by_id" ;
        :methodParams ( [ :paramType "Uuid" ] ) ;
        :methodReturns { "Option<" ?entity ">" } ;
    ] .
} .

# Rule 5: Entities with :auditable get created_at/updated_at fields
{
    ?entity :auditable true ;
            :codegen-as "struct" .
} => {
    ?entity :auto-field [
        :name "created_at" ;
        :type "DateTime<Utc>" ;
        :required true ;
    ] .
    ?entity :auto-field [
        :name "updated_at" ;
        :type "DateTime<Utc>" ;
        :required true ;
    ] .
} .

# Rule 6: Soft-delete entities get deleted_at field
{
    ?entity :soft-delete true ;
            :codegen-as "struct" .
} => {
    ?entity :auto-field [
        :name "deleted_at" ;
        :type "Option<DateTime<Utc>>" ;
        :required false ;
    ] .
} .
```

### How N3 Reasoning Works for Code Generation

```
Input Ontology:
    :User a rdfs:Class ;
        :codegen-as "struct" ;
        :auditable true ;
        :soft-delete true ;
        :fields (
            [ :name "id" ; :type "Uuid" ]
            [ :name "email" ; :type "String" ]
        ) .

After N3 Reasoning (inferred facts):
    :User :auto-derive code:Serialize, code:Deserialize .
    :User :auto-field [ :name "created_at" ; :type "DateTime<Utc>" ] .
    :User :auto-field [ :name "updated_at" ; :type "DateTime<Utc>" ] .
    :User :auto-field [ :name "deleted_at" ; :type "Option<DateTime<Utc>>" ] .
    :User :requires-import "chrono::DateTime", "chrono::Utc" .

Generated Rust:
    use chrono::{DateTime, Utc};
    use serde::{Serialize, Deserialize};

    #[derive(Debug, Clone, Serialize, Deserialize)]
    pub struct User {
        pub id: Uuid,
        pub email: String,
        pub created_at: DateTime<Utc>,
        pub updated_at: DateTime<Utc>,
        pub deleted_at: Option<DateTime<Utc>>,
    }
```

**Key Insight**: The developer only specifies `:auditable true`. The N3 reasoner infers ALL the fields, imports, and derives automatically. This is semantic meta-programming.

---

## Part 3: Advanced Patterns

### Pattern 5: N3 for Design Pattern Generation

**Generate Builder Pattern from struct**:
```n3
{
    ?struct :codegen-as "struct" ;
            :builder-pattern true ;
            :fields ?fields .
} => {
    # Generate builder struct
    ?structBuilder a code:RustStruct ;
        code:name { ?struct rdfs:label ; "Builder" } ;
        code:fields [ code:wrapAll ?fields ; code:wrapType "Option" ] .

    # Generate build method
    ?structBuilder :method [
        code:name "build" ;
        code:returnType { "Result<" ?struct rdfs:label ", BuildError>" } ;
    ] .
} .
```

### Pattern 6: N3 for Error Type Generation

**Generate error enums from failure modes**:
```n3
{
    ?service :failure-modes ?failures .
    ?failures rdf:first ?mode .
    ?mode :name ?errorName ;
          :message ?errorMsg .
} => {
    ?service :error-enum [
        code:variant [
            code:name ?errorName ;
            code:message ?errorMsg ;
        ] ;
    ] .
} .
```

**Input**:
```turtle
:UserService :failure-modes (
    [ :name "NotFound" ; :message "User not found" ]
    [ :name "DuplicateEmail" ; :message "Email already exists" ]
    [ :name "InvalidPassword" ; :message "Password does not meet requirements" ]
) .
```

**Generated**:
```rust
#[derive(Debug, thiserror::Error)]
pub enum UserServiceError {
    #[error("User not found")]
    NotFound,
    #[error("Email already exists")]
    DuplicateEmail,
    #[error("Password does not meet requirements")]
    InvalidPassword,
}
```

### Pattern 7: CONSTRUCT for Test Generation

**Generate tests from SHACL constraints**:
```sparql
CONSTRUCT {
    ?test a code:RustTest ;
        code:testName ?testName ;
        code:testBody ?testBody .
}
WHERE {
    ?shape a sh:NodeShape ;
           sh:targetClass ?class ;
           sh:property ?prop .

    ?prop sh:path ?path ;
          sh:minLength ?minLen .

    BIND(CONCAT("test_", LCASE(STR(?class)), "_", STR(?path), "_min_length") AS ?testName)
    BIND(CONCAT(
        "let invalid = ", STR(?class), " { ", STR(?path), ": \"".repeat(?minLen - 1), "\" };\n",
        "assert!(invalid.validate().is_err());"
    ) AS ?testBody)
}
```

---

## Part 4: The ggen v5 Pipeline with CONSTRUCT & N3

```
┌─────────────────┐     ┌─────────────────┐     ┌─────────────────┐
│  domain.ttl     │     │  rules.n3       │     │  ggen.toml      │
│  (Domain Model) │     │  (Inference     │     │  (Manifest)     │
│                 │     │   Rules)        │     │                 │
└────────┬────────┘     └────────┬────────┘     └────────┬────────┘
         │                       │                       │
         └───────────────────────┼───────────────────────┘
                                 │
                                 ▼
                    ┌────────────────────────┐
                    │  oxigraph + N3 Reasoner │
                    │  (Forward Chaining)     │
                    └────────────┬───────────┘
                                 │
                                 ▼
                    ┌────────────────────────┐
                    │  Enriched RDF Graph    │
                    │  (Domain + Inferred    │
                    │   Code Patterns)       │
                    └────────────┬───────────┘
                                 │
                                 ▼
                    ┌────────────────────────┐
                    │  CONSTRUCT Queries     │
                    │  (Build Code Graph)    │
                    └────────────┬───────────┘
                                 │
                                 ▼
                    ┌────────────────────────┐
                    │  Code Ontology Graph   │
                    │  (RustStruct, ImplBlock│
                    │   RustTrait, etc.)     │
                    └────────────┬───────────┘
                                 │
                                 ▼
                    ┌────────────────────────┐
                    │  Tera Templates        │
                    │  (Trivial Serializers) │
                    └────────────┬───────────┘
                                 │
                                 ▼
                    ┌────────────────────────┐
                    │  Generated Rust Code   │
                    │  (models.rs, traits.rs │
                    │   tests.rs)            │
                    └────────────────────────┘
```

---

## Part 5: Innovative Use Cases

### Use Case 1: Agent-Driven Refactoring via CONSTRUCT

An AI agent can issue CONSTRUCT queries to refactor code semantically:

```sparql
# Agent: "Split large structs into smaller ones"
CONSTRUCT {
    ?newStruct1 a code:RustStruct ;
        code:name ?name1 ;
        code:fields ?fields1 .
    ?newStruct2 a code:RustStruct ;
        code:name ?name2 ;
        code:fields ?fields2 .
}
WHERE {
    ?struct :fields ?allFields .
    FILTER(COUNT(?allFields) > 10)

    # Split fields into two groups
    # ... logic to partition fields
}
```

### Use Case 2: Cross-Service API Generation via N3

```n3
# If service A depends on service B, generate client for B in A
{
    ?serviceA :depends-on ?serviceB .
    ?serviceB :exposes-api ?api .
} => {
    ?serviceA :generate-client [
        :for-api ?api ;
        :client-type "reqwest" ;
    ] .
} .
```

### Use Case 3: Schema Migration via CONSTRUCT

```sparql
# Generate migration from schema diff
CONSTRUCT {
    ?migration a code:Migration ;
        code:up ?upSql ;
        code:down ?downSql .
}
WHERE {
    ?oldSchema :version ?oldV .
    ?newSchema :version ?newV .
    FILTER(?newV > ?oldV)

    # Diff fields and generate ALTER statements
    # ...
}
```

### Use Case 4: Documentation Generation via N3

```n3
# Auto-generate OpenAPI spec from trait methods
{
    ?trait :codegen-as "trait" ;
           :http-api true ;
           :methods ?methods .
    ?methods rdf:first ?method .
    ?method :methodName ?name ;
            :httpVerb ?verb ;
            :httpPath ?path .
} => {
    ?trait :openapi-operation [
        :operationId ?name ;
        :method ?verb ;
        :path ?path ;
    ] .
} .
```

---

## Part 6: oxigraph N3 Integration

ggen v5 uses oxigraph's native N3 support:

```rust
use oxigraph::store::Store;
use oxigraph::model::*;
use oxigraph::sparql::QueryResults;

pub struct GgenEngine {
    store: Store,
}

impl GgenEngine {
    /// Load ontology and rules, then run N3 reasoning
    pub fn load_and_reason(&mut self, ontology: &str, rules: &str) -> Result<(), GgenError> {
        // Load base ontology
        self.store.load_from_read(
            RdfFormat::Turtle,
            std::io::Cursor::new(ontology),
            None,
        )?;

        // Load N3 rules (oxigraph supports N3)
        self.store.load_from_read(
            RdfFormat::N3,
            std::io::Cursor::new(rules),
            None,
        )?;

        // Run forward-chaining inference
        // oxigraph's N3 reasoner applies rules until fixpoint
        self.store.run_inference()?;

        Ok(())
    }

    /// Execute CONSTRUCT query and return new graph
    pub fn construct(&self, query: &str) -> Result<Graph, GgenError> {
        match self.store.query(query)? {
            QueryResults::Graph(quads) => {
                let graph = Graph::new();
                for quad in quads {
                    graph.insert(&quad?);
                }
                Ok(graph)
            }
            _ => Err(GgenError::SparqlError("Expected CONSTRUCT query".into())),
        }
    }
}
```

---

## Part 7: Performance Considerations

### N3 Reasoning Complexity
- **Forward chaining** can be expensive for large rule sets
- **Stratification**: Rules should be layered to avoid infinite loops
- **Termination**: ggen v5 enforces 5s timeout on reasoning

### CONSTRUCT Query Optimization
- **Materialized views**: Pre-compute common CONSTRUCT patterns
- **Incremental updates**: Only re-run CONSTRUCT for changed triples
- **Parallelization**: Independent CONSTRUCT queries run in parallel

### Target Performance
- **<100ms** for typical domain model (10-50 classes)
- **<500ms** for enterprise model (100-500 classes)
- **<2s** for massive models (1000+ classes)

---

## Summary: Why CONSTRUCT & N3 are Game-Changers

| Aspect | SELECT (v4) | CONSTRUCT + N3 (v5) |
|--------|-------------|---------------------|
| Output | Tabular data | New RDF graphs |
| Logic | In templates | In SPARQL/N3 |
| Inference | None | Forward-chaining |
| Meta-programming | Hard | Native |
| Code = Data | Separate | Unified |
| Agent-friendliness | Limited | Excellent |
| Composability | Low | High (chain CONSTRUCT) |
| Determinism | Depends on templates | Guaranteed by logic |

**The fundamental insight**: CONSTRUCT and N3 let you treat **code as data in an RDF graph**. Code generation becomes **graph transformation**, which is deterministic, composable, and semantically precise.

This is why ggen v5 is a **hyperfast SPARQL/N3 code generator** - not a template engine with RDF bolted on.
