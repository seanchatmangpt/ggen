# ggen v5 Design: RDF-First Code Generation

**Date**: 2024-12-14
**Focus**: Ontology/RDF as source of truth, CLI as thin wrapper
**Philosophy**: Semantic precision over generalization

---

## Core Paradigm Shift: ggen.toml → RDF Ontology → Generated Code

### v4 (Wrong)
```
CLI command → Project config → Code generation → Output
```

### v5 (Right)
```
ggen.toml (manifest) → Load RDF Ontology → SPARQL Queries → Tera Templates → Output Code
       ↓
   [Semantic Truth]
```

---

## ggen.toml Specification (Project Manifest)

The `ggen.toml` file lives in the project root and describes:
1. **What RDF ontology to load** (domain model)
2. **Which SPARQL queries to execute** (what to generate)
3. **Which templates to use** (how to render)
4. **Generation rules and constraints** (poka-yoke validation)

### Example: Basic ggen.toml

```toml
# ggen.toml - Declarative code generation manifest

[project]
name = "user-service"
version = "1.0.0"
description = "User management microservice"

[ontology]
# Primary RDF ontology file (can be .ttl, .rdf, .jsonld, .n3, .trig)
source = "domain/user-model.ttl"

# Alternative: inline N3 rules for code generation
rules_file = "domain/generation-rules.n3"

# SHACL constraints for validation (optional)
shapes_file = "domain/shapes.ttl"

[generation]
# Target language (v5.0 = Rust only)
language = "rust"

# Output directory
output_dir = "./src/generated"

# Template directories (ordered search path)
templates = [
    "templates/",           # Project-specific templates
    "~/.ggen/templates/",   # User-installed templates
    "/usr/share/ggen/",     # System templates
]

# Generation rules (what to generate)
[[generation.rules]]
name = "rust-structs"
query = """
    PREFIX : <http://example.com/myapp#>
    PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>

    SELECT ?class ?name ?fields
    WHERE {
        ?class a rdfs:Class ;
               :codegen-as "struct" ;
               rdfs:label ?name ;
               :fields ?fieldsList .
    }
"""
template = "rust-struct.tera"
output_file = "src/models.rs"

[[generation.rules]]
name = "rust-traits"
query = """
    PREFIX : <http://example.com/myapp#>

    SELECT ?trait ?methods
    WHERE {
        ?trait a rdfs:Class ;
               :codegen-as "trait" ;
               :methods ?methodsList .
    }
"""
template = "rust-trait.tera"
output_file = "src/traits.rs"

[validation]
# Poka-yoke safety settings
enforce_shacl = true              # Validate against SHACL shapes
check_cardinalities = true        # Verify cardinality constraints
max_sparql_timeout_ms = 5000      # Kill long-running queries
max_template_render_ms = 2000     # Kill slow template rendering
max_total_generation_ms = 10000   # Kill slow overall generation

# Generated code validation
validate_syntax = true            # Verify generated code is valid Rust
no_unsafe = true                  # Forbid unsafe code in generation
require_audit_trail = true        # Generate audit.json

[options]
# Determinism settings
determinism_seed = 42             # Seed for reproducible outputs
strict_mode = true                # Strict validation

# Agent safety (for autonomous code generation)
dry_run_default = false           # --dry-run enabled?
audit_trail = true                # Always generate audit.json
```

---

## RDF Ontology Structure (domain/user-model.ttl)

The ontology describes:
1. **Domain classes** (what entities exist)
2. **Code generation directives** (how to generate code from classes)
3. **Constraints** (cardinality, types, relationships)

### Example: User Service Ontology

```turtle
@prefix : <http://example.com/myapp#> .
@prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .
@prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .
@prefix xsd: <http://www.w3.org/2001/XMLSchema#> .
@prefix sh: <http://www.w3.org/ns/shacl#> .

# ============================================================================
# ONTOLOGY METADATA
# ============================================================================

<http://example.com/myapp> a <http://www.w3.org/2002/07/owl#Ontology> ;
    rdfs:label "User Service Domain Model" ;
    rdfs:comment "Semantic model for user management microservice" ;
    <http://www.w3.org/2002/07/owl#versionInfo> "1.0.0" .

# ============================================================================
# CODE GENERATION DIRECTIVES (ggen-specific properties)
# ============================================================================

# Property: codegen-as (what kind of code to generate)
:codegen-as a rdf:Property ;
    rdfs:label "Code Generation Type" ;
    rdfs:comment "Tells ggen how to generate code for this class (struct, trait, enum, etc.)" ;
    rdfs:range [ rdf:type rdfs:Datatype ;
                 rdf:value "struct|trait|enum|impl|module|test" ] .

# Property: codegen-derives (what derive macros to add)
:codegen-derives a rdf:Property ;
    rdfs:label "Derives" ;
    rdfs:comment "Derive macros to add to generated Rust code" ;
    rdfs:range rdf:List .

# Property: fields (struct fields definition)
:fields a rdf:Property ;
    rdfs:label "Fields" ;
    rdfs:comment "Ordered list of struct fields with types and attributes" ;
    rdfs:range rdf:List .

# Property: methods (trait/impl methods)
:methods a rdf:Property ;
    rdfs:label "Methods" ;
    rdfs:comment "Ordered list of methods with signatures" ;
    rdfs:range rdf:List .

# ============================================================================
# DOMAIN CLASSES (What to generate)
# ============================================================================

# User class with code generation directive
:User a rdfs:Class ;
    rdfs:label "User" ;
    rdfs:comment "A user in the system" ;
    :codegen-as "struct" ;
    :codegen-derives (
        <http://example.com/derives/Debug>
        <http://example.com/derives/Clone>
        <http://example.com/derives/Serialize>
        <http://example.com/derives/Deserialize>
    ) ;
    :fields (
        [
            :fieldName "id" ;
            :fieldType "Uuid" ;
            :fieldRequired true ;
            :fieldDocstring "Unique identifier for the user" ;
        ]
        [
            :fieldName "email" ;
            :fieldType "String" ;
            :fieldRequired true ;
            :fieldValidation "email_format" ;
            :fieldDocstring "User email address (must be valid email)" ;
        ]
        [
            :fieldName "name" ;
            :fieldType "String" ;
            :fieldRequired true ;
            :fieldDocstring "User's full name" ;
        ]
        [
            :fieldName "created_at" ;
            :fieldType "DateTime<Utc>" ;
            :fieldRequired true ;
            :fieldDocstring "Account creation timestamp" ;
        ]
        [
            :fieldName "updated_at" ;
            :fieldType "DateTime<Utc>" ;
            :fieldRequired true ;
            :fieldDocstring "Last update timestamp" ;
        ]
    ) .

# UserRepository trait with code generation
:UserRepository a rdfs:Class ;
    rdfs:label "User Repository" ;
    rdfs:comment "Data access trait for User entities" ;
    :codegen-as "trait" ;
    :methods (
        [
            :methodName "create" ;
            :methodAsync true ;
            :methodParams ( [ :paramName "user" ; :paramType "User" ] ) ;
            :methodReturns "Result<User, Error>" ;
            :methodDocstring "Create a new user" ;
        ]
        [
            :methodName "find_by_id" ;
            :methodAsync true ;
            :methodParams ( [ :paramName "id" ; :paramType "Uuid" ] ) ;
            :methodReturns "Result<Option<User>, Error>" ;
            :methodDocstring "Find user by ID" ;
        ]
        [
            :methodName "update" ;
            :methodAsync true ;
            :methodParams ( [ :paramName "user" ; :paramType "User" ] ) ;
            :methodReturns "Result<User, Error>" ;
            :methodDocstring "Update existing user" ;
        ]
        [
            :methodName "delete" ;
            :methodAsync true ;
            :methodParams ( [ :paramName "id" ; :paramType "Uuid" ] ) ;
            :methodReturns "Result<(), Error>" ;
            :methodDocstring "Delete user by ID" ;
        ]
    ) .

# ============================================================================
# CONSTRAINTS (SHACL)
# ============================================================================

# Validate User class shape
:UserShape a sh:NodeShape ;
    sh:targetClass :User ;
    sh:property [
        sh:path :email ;
        sh:minCount 1 ;
        sh:maxCount 1 ;
        sh:datatype xsd:string ;
        sh:pattern "^[a-zA-Z0-9._%+-]+@[a-zA-Z0-9.-]+\\.[a-zA-Z]{2,}$" ;
    ] ;
    sh:property [
        sh:path :name ;
        sh:minCount 1 ;
        sh:maxCount 1 ;
        sh:datatype xsd:string ;
        sh:minLength 1 ;
        sh:maxLength 256 ;
    ] .

# ============================================================================
# RELATIONSHIPS (Object Properties)
# ============================================================================

:manages a <http://www.w3.org/1999/02/22-rdf-syntax-ns#Property> ;
    rdfs:label "manages" ;
    rdfs:comment "User manages other users (admin relationship)" ;
    rdfs:domain :User ;
    rdfs:range :User .

:belongs_to_team a <http://www.w3.org/1999/02/22-rdf-syntax-ns#Property> ;
    rdfs:label "belongs to team" ;
    rdfs:comment "User is a member of a team" ;
    rdfs:domain :User ;
    rdfs:range :Team .
```

---

## Generation Rules in N3 (domain/generation-rules.n3)

N3 rules enable **logic-based code generation** - conditional patterns that depend on ontology facts.

```n3
@prefix : <http://example.com/myapp#> .
@prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .

# Rule: If a property is marked async, generate async trait method
{
    ?class :codegen-as "trait" ;
           :methods ?methodsList .
    ?methodsList rdf:first ?method ;
                 rdf:rest [ rdf:first ?nextMethod ] .
    ?method :methodAsync true .
} => {
    ?class :requires-async-runtime true .
} .

# Rule: If struct has > 5 fields, derive PartialEq
{
    ?class :codegen-as "struct" ;
           :fields ?fieldsList .
    ?fieldsList rdf:first ?f1 ;
               rdf:rest [ rdf:first ?f2 ;
                        rdf:rest [ rdf:first ?f3 ;
                                  rdf:rest [ rdf:first ?f4 ;
                                            rdf:rest [ rdf:first ?f5 ;
                                                      rdf:rest [ rdf:first ?f6 ] ] ] ] ] .
} => {
    ?class :auto-derive-partial-eq true .
} .
```

---

## Pipeline: ggen.toml → Generated Code

### Step 1: Parse ggen.toml
```rust
let manifest = GgenManifest::from_toml("ggen.toml")?;
println!("Project: {}", manifest.project.name);
println!("Ontology: {}", manifest.ontology.source);
```

### Step 2: Load RDF Ontology
```rust
let graph = RdfGraph::load(&manifest.ontology.source)?;
// Now graph contains all semantic facts
println!("Loaded {} triples", graph.triple_count());
```

### Step 3: Validate with SHACL
```rust
let shapes = RdfGraph::load(&manifest.validation.shapes_file)?;
let violations = validate_shacl(&graph, &shapes)?;
if !violations.is_empty() {
    return Err(GgenError::ValidationError("SHACL violations found"));
}
```

### Step 4: Execute SPARQL Queries
```rust
for rule in &manifest.generation.rules {
    let results = graph.execute_sparql(&rule.query)?;
    println!("Rule '{}' matched {} results", rule.name, results.len());
}
```

### Step 5: Render Templates
```rust
for rule in &manifest.generation.rules {
    let context = prepare_template_context(&rule, &results)?;
    let output = tera.render(&rule.template, &context)?;
    write_file(&rule.output_file, &output)?;
}
```

### Step 6: Generate Audit Trail
```json
{
    "input_ontology_hash": "sha256:abc123...",
    "sparql_queries": ["query1", "query2"],
    "templates_rendered": ["rust-struct.tera", "rust-trait.tera"],
    "output_files": ["src/models.rs", "src/traits.rs"],
    "validation_passed": true,
    "exit_code": 0,
    "duration_ms": 42
}
```

---

## Why v5 Works (vs v4)

| Aspect | v4 (CLI-first) | v5 (RDF-first) |
|--------|---|---|
| Source of Truth | CLI flags (messy) | RDF ontology (semantic) |
| Code generation | Ad-hoc templates | SPARQL → Templates |
| Reproducibility | Hard (many variations) | Perfect (same input = same output) |
| Agent safety | Limited (CLI unpredictable) | Excellent (audit trail + validation) |
| Scalability | O(n) commands | O(1) engine + multiple ontologies |
| Determinism | Binary success/fail | Semantic exit codes + detailed errors |

---

## Migration from v4 to v5

**v4 command:**
```bash
ggen project new --name myapp --template rust-service
```

**v5 equivalent:**
```bash
# 1. Create ggen.toml + domain/myapp.ttl
cat > ggen.toml << 'EOF'
[project]
name = "myapp"

[ontology]
source = "domain/myapp.ttl"

[generation.rules]
name = "rust-service"
template = "rust-service.tera"
EOF

# 2. Run code generation (single semantic command)
ggen sync
```

Key difference: **Manifest-first, not CLI-first**.

---

## Next: v5.0 Implementation Roadmap

1. ✅ **Design**: ggen.toml spec + RDF structure (THIS DOCUMENT)
2. ⏳ **Core**: Finish ggen-core RDF/SPARQL engine (80% done)
3. ⏳ **Parsing**: ggen.toml parser with strong typing
4. ⏳ **Pipeline**: RDF → SPARQL → Templates → Output
5. ⏳ **Validation**: SHACL + poka-yoke guards
6. ⏳ **CLI**: Single unified command (ggen sync, configured via ggen.toml)
7. ⏳ **Examples**: 3 complete example projects
8. ⏳ **Tests**: Chicago TDD suite (80%+ coverage)

---

**Key Insight**: The CLI is not the product. The product is the **semantic code generation engine powered by RDF/SPARQL**. The CLI is just how humans interact with it.
