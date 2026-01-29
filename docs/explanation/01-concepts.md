# Explanation: Core Concepts

**Deep dive into ggen architecture and design philosophy.**

## What is an Ontology?

An **ontology** is a machine-readable description of domain knowledge.

### Example: Simple Ontology

```turtle
@prefix : <https://example.org/> .

:Alice a :Person ;
  :name "Alice" ;
  :age 30 .

:Bob a :Person ;
  :name "Bob" ;
  :knows :Alice .
```

**What this says:**
- **Alice** is a **Person** with name "Alice" and age 30
- **Bob** is a **Person** with name "Bob"
- **Bob** knows **Alice**

### Why Ontologies Matter for Code Generation

| Without Ontology | With Ontology |
|------------------|---------------|
| Manually write REST handlers | Define data in RDF, generate handlers |
| Update database schema by hand | Update ontology, regenerate migrations |
| Manually keep docs in sync | Generate docs from ontology |
| Copy-paste boilerplate | Generate from single source of truth |

**Key advantage**: **Single source of truth** - edit ontology once, generate everywhere

---

## RDF: Resource Description Framework

**RDF** is a standard for describing things using triples.

### Triple Structure

Every fact is a **triple**: Subject - Predicate - Object

```
Subject     Predicate    Object
--------    ---------    ------
Alice       name         "Alice"
Alice       age          30
Bob         knows        Alice
```

In Turtle notation:
```turtle
:Alice :name "Alice" ;
       :age 30 ;
       :knows :Bob .
```

### Example: Building a REST API Ontology

```turtle
@prefix : <https://api.example.org/> .
@prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .

# Define User resource
:User a :ResourceType ;
  :name "User" ;
  :fields ( :id :name :email ) .

# Define GET /users endpoint
:GetUsers a :Endpoint ;
  :method :GET ;
  :path "/users" ;
  :returns :UserList .
```

**Advantage**: Machine-readable, queryable with SPARQL

---

## SPARQL: Query Language for RDF

**SPARQL** is like SQL but for RDF triples.

### Example: Finding All Endpoints

```sparql
PREFIX : <https://api.example.org/>

SELECT ?endpoint ?method ?path
WHERE {
  ?endpoint a :Endpoint ;
    :method ?method ;
    :path ?path .
}
ORDER BY ?path
```

**This finds:**
- All things of type `:Endpoint`
- Extract their `:method` and `:path`
- Sort by path

### How SPARQL Powers Code Generation

```
Ontology (.ttl)
    ↓
SPARQL Query
    ↓
Results (JSON/Bindings)
    ↓
Tera Template
    ↓
Generated Code
```

**Example flow:**

```sparql
-- Query: Find all REST endpoints
SELECT ?name ?method ?path
WHERE { ?name a :Endpoint ; :method ?method ; :path ?path . }
```

**Results:**
```json
[
  { "name": "GetUsers", "method": "GET", "path": "/users" },
  { "name": "CreateUser", "method": "POST", "path": "/users" },
  { "name": "GetUser", "method": "GET", "path": "/users/{id}" }
]
```

**Template:**
```tera
{% for endpoint in results %}
#[{{ endpoint.method | lower }}("{{ endpoint.path }}")]
pub async fn {{ endpoint.name }}() { }
{% endfor %}
```

**Generated:**
```rust
#[get("/users")]
pub async fn GetUsers() { }

#[post("/users")]
pub async fn CreateUser() { }

#[get("/users/{id}")]
pub async fn GetUser() { }
```

---

## Tera: Template Engine

**Tera** is a template language for rendering code.

### Basic Syntax

```tera
# Variables
{{ user.name }}

# Loops
{% for user in users %}
  User: {{ user.name }}
{% endfor %}

# Conditionals
{% if user.admin %}
  ADMIN: {{ user.name }}
{% endif %}

# Filters
{{ user.name | uppercase }}
{{ user.email | truncate(20) }}
```

### Tera in ggen Workflow

```
SPARQL Results          Tera Template           Generated Output
--------------          -----                   ----------------

[
  {
    "name": "Alice",
    "email": "alice@...",
    "admin": true
  },
  {
    "name": "Bob",
    "email": "bob@...",
    "admin": false
  }
]
         │
         ├──────────────→  {% for user in users %}  ────────────→  struct User {
                            pub {{ user.name }}: String;                name: String,
                            pub {{ user.email }}: String;               email: String,
                            pub admin: bool;                           admin: bool,
                          {% endfor %}                              }
```

---

## Five-Stage Pipeline (μ₁-μ₅)

ggen's core is a **deterministic pipeline** that transforms ontologies into code.

```
                Input                  Pipeline                 Output
                -----                  --------                 ------

            .specify/                μ₁ Normalize
            *.ttl files              • Load RDF
                ├──────────────────→  • SHACL validation
                                       • Schema checking
                                       • OWL inference
                                                    │
                                                    ↓
                                        μ₂ Extract
                                        • SPARQL queries
                                        • Rule engine
                                        • Data extraction
                                                    │
                                                    ↓
                                        μ₃ Emit
            templates/                 • Tera rendering
            *.tera ─────────────────→  • Multi-file generation
                                        • Code synthesis
                                                    │
                                                    ↓
                                        μ₄ Canonicalize
                                        • rustfmt/prettier
                                        • Syntax validation
                                        • Content hashing
                                                    │
                                                    ↓
                                        μ₅ Receipt
                                        • SHA-256 hashing
                                        • Execution ID
                                        • Audit trail
                                                    │
                                                    ↓
                                            src/
                                         *.rs, *.ts, *.py
                                        + .ggen/receipts/
                                            latest.json
```

### Stage Details

| Stage | Input | Process | Output |
|-------|-------|---------|--------|
| **μ₁ Normalize** | `.ttl` files | Load, validate, infer | RDF graph |
| **μ₂ Extract** | RDF graph | SPARQL queries | Structured data |
| **μ₃ Emit** | Data + templates | Render templates | Raw artifacts |
| **μ₄ Canonicalize** | Raw artifacts | Format, validate, hash | Formatted code |
| **μ₅ Receipt** | Everything | Hash, sign, log | Proof + audit trail |

**Key property**: **Deterministic** - same input always produces identical output

---

## Determinism: Why It Matters

**Deterministic = Reproducible**

### Example: Reproducibility

```bash
# Day 1: Generate code
ggen sync
# Receipt: abc123...

# Day 30: Regenerate same ontology
ggen sync
# Receipt: abc123... (IDENTICAL!)

# Someone else regenerates
ggen sync
# Receipt: abc123... (STILL IDENTICAL!)
```

**Guarantee**: Same ontology + same templates = identical output every time

### Why This Matters

| Scenario | Benefit |
|----------|---------|
| **Audit trail** | Prove what was generated and when |
| **Reproducibility** | Anyone can regenerate identical code |
| **Caching** | Cache results, reuse if input unchanged |
| **CI/CD** | Verify generated code matches proof |
| **Collaboration** | Team members generate identical code |

---

## Specification-First Development

**Traditional approach**: Code → Documentation → Requirements

**Ontology-first approach**: Specifications → Code → Documentation

```
Traditional            Ontology-First
-----------            --------

Code
  ↓
Tests
  ↓
Documentation
  ↓
(Requirements buried in code)

                       Specifications (.ttl)
                         ↓
                       Code (generated)
                         ↓
                       Tests
                         ↓
                       Documentation (generated)
                         ↓
                       (Requirements explicit and queryable)
```

**Advantage**: **Single source of truth** - requirements, code, docs, tests all derive from specifications

---

## SHACL: Shape Constraint Language

**SHACL** defines what valid RDF looks like.

### Example: User Shape

```turtle
@prefix : <https://example.org/> .
@prefix sh: <http://www.w3.org/ns/shacl#> .

:UserShape a sh:NodeShape ;
  sh:targetClass :User ;
  sh:property [
    sh:path :name ;
    sh:datatype xsd:string ;
    sh:minLength 1 ;
    sh:maxLength 255 ;
  ] ;
  sh:property [
    sh:path :email ;
    sh:datatype xsd:string ;
    sh:pattern "^[\\w\\.-]+@[\\w\\.-]+\\.\\w+$" ;  # Email regex
  ] .
```

**What this says:**
- Users MUST have a `:name` (string, 1-255 characters)
- Users MUST have an `:email` (must be valid email format)
- Violating these rules = validation error

**In ggen**: SHACL validates specifications before generation

---

## Comparison: Code Generation Approaches

### Approach 1: Manual Code

```
✗ Write handlers manually
✗ Write models manually
✗ Update docs manually
✗ Keep everything in sync manually

Result: High maintenance, easy to drift
```

### Approach 2: Code Generators (Old)

```
? Write code generator
? Learn generator syntax
? Maintain multiple generators (one per language)
? Brittle if requirements change

Result: Better than manual, but complex
```

### Approach 3: Ontology-First (ggen)

```
✓ Write specification once (RDF/Turtle)
✓ Generate any number of outputs (Rust, Go, TypeScript, SQL, docs)
✓ Single source of truth
✓ Easy to modify (edit .ttl, regenerate)
✓ Deterministic and reproducible

Result: Write once, generate everywhere
```

---

## Key Mental Models

### 1. RDF Triple = Programming Concept

| RDF | Programming |
|-----|-------------|
| Subject | Class, function, variable |
| Predicate | Property, attribute, method |
| Object | Value, type, implementation |

### 2. SPARQL Query = Data Extraction

SPARQL queries extract relevant data from ontology to feed into templates.

### 3. Template = Code Skeleton

Templates define HOW to render extracted data into code.

### 4. Pipeline = Transformation

Each stage transforms data into the next format:
- RDF → Structured data → Code → Formatted code → Proof

---

## Common Mistakes

### Mistake 1: Editing Generated Code

```bash
# ✗ WRONG
cat > src/handlers.rs <<'EOF'
// Manually edit generated file
#[get("/users")]
pub async fn list_users() { }
EOF

ggen sync  # Overwrites your manual edits!

# ✓ CORRECT
# Edit ontology
vim .specify/specs/001-users/users.ttl

# Regenerate
ggen sync
```

### Mistake 2: Complex Ontologies

```turtle
# ✗ WRONG - One massive .ttl file with 10,000 triples
@prefix : <https://example.org/> .
:Entity1 ...
:Entity2 ...
:Entity3 ...
# (many more)

# ✓ CORRECT - Split by domain
.specify/specs/001-users/users.ttl       # ~500 triples
.specify/specs/002-products/products.ttl # ~500 triples
.specify/specs/003-orders/orders.ttl     # ~500 triples
```

### Mistake 3: Missing Namespace Declarations

```turtle
# ✗ WRONG
:User a :ResourceType ;  # Namespace `:` not declared!
  :name "User" .

# ✓ CORRECT
@prefix : <https://example.org/> .
:User a :ResourceType ;
  :name "User" .
```

---

## Further Reading

- [ggen Architecture Docs](./02-sparql.md)
- [RDF/Turtle Specification](https://www.w3.org/TR/turtle/)
- [SPARQL 1.1 Query Language](https://www.w3.org/TR/sparql11-query/)
- [Tera Template Engine](https://keats.github.io/tera/)
- [SHACL Specification](https://www.w3.org/TR/shacl/)

---

**Next**: [How SPARQL Powers Generation](./02-sparql.md)
