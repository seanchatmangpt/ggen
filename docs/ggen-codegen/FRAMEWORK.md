# ggen-codegen Framework Guide

The ggen-codegen framework is a specification-driven code generation system that transforms RDF ontologies into working code through a deterministic five-stage pipeline (`A = μ(O)`).

## Overview

**Formula**: Code precipitates from RDF via five transformation stages:
- **μ₁ (Normalize)**: RDF validation, SHACL shapes, ontology loading
- **μ₂ (Extract)**: SPARQL CONSTRUCT queries extract code blueprints
- **μ₃ (Emit)**: Tera templates render code files
- **μ₄ (Canonicalize)**: Deterministic formatting ensures reproducibility
- **μ₅ (Receipt)**: Cryptographic proof generation for verification

## Core Architecture

### GenerationPipeline

The `GenerationPipeline` orchestrates the five-stage transformation:

```
┌─────────────────────────────────────────┐
│     Load Ontology (μ₁: Normalize)      │
│  ├─ Parse manifest
│  ├─ Load RDF graph
│  └─ Validate SHACL shapes
└────────────────┬────────────────────────┘
                 ↓
┌─────────────────────────────────────────┐
│    Execute Rules (μ₂: Extract)          │
│  ├─ Inference rules (CONSTRUCT)
│  ├─ Generate rules (SELECT)
│  └─ Build code graph
└────────────────┬────────────────────────┘
                 ↓
┌─────────────────────────────────────────┐
│    Render Templates (μ₃: Emit)          │
│  ├─ SELECT queries bind variables
│  ├─ Tera templating system
│  └─ Generate output files
└────────────────┬────────────────────────┘
                 ↓
┌─────────────────────────────────────────┐
│   Canonicalize (μ₄: Determinism)        │
│  ├─ Format consistently
│  ├─ Sort output
│  └─ Compute SHA256 hashes
└────────────────┬────────────────────────┘
                 ↓
┌─────────────────────────────────────────┐
│    Verify & Audit (μ₅: Receipt)         │
│  ├─ Validate generated code
│  ├─ Create audit trail
│  └─ Generate cryptographic proof
└─────────────────────────────────────────┘
```

## Key Traits

### Queryable Trait

**Purpose**: Abstracts RDF query execution allowing different backends (Oxigraph, SPARQL endpoints, etc.)

**What It Does**:
- Executes SPARQL SELECT and CONSTRUCT queries
- Binds variables for template rendering
- Materializes graph patterns
- Validates query results

**When to Use**:
- Custom RDF stores (graph databases, federated endpoints)
- Domain-specific query optimization
- Integration with external knowledge bases

**How to Implement**:

```rust
use ggen_core::rdf::Queryable;
use ggen_core::Graph;

pub struct MyCustomQueryable {
    // Your implementation fields
}

impl Queryable for MyCustomQueryable {
    /// Execute a SPARQL SELECT query
    fn select(&self, sparql: &str) -> Result<Vec<Binding>> {
        // 1. Parse the SPARQL query
        // 2. Execute against your backend
        // 3. Convert results to Binding format
        // 4. Return Ok(bindings) or Err(error)
    }

    /// Execute a SPARQL CONSTRUCT query
    fn construct(&self, sparql: &str) -> Result<Graph> {
        // 1. Parse the SPARQL query
        // 2. Execute against your backend
        // 3. Materialize results into a Graph
        // 4. Return Ok(graph) or Err(error)
    }

    /// Query variable bindings
    fn query_bindings(&self, sparql: &str) -> Result<Vec<Binding>> {
        // Similar to select but handles specific binding format
    }
}
```

**Example: Graph Database Backend**

```rust
use ggen_core::Graph;
use ggen_core::rdf::Queryable;

pub struct GraphDbBackend {
    connection_url: String,
    timeout_ms: u64,
}

impl Queryable for GraphDbBackend {
    fn select(&self, sparql: &str) -> Result<Vec<Binding>> {
        // HTTP POST to graph database with SPARQL query
        let client = reqwest::Client::new();
        let response = client
            .post(&self.connection_url)
            .json(&json!({ "query": sparql }))
            .timeout(Duration::from_millis(self.timeout_ms))
            .send()?;

        let results: Vec<Binding> = response.json()?;
        Ok(results)
    }

    fn construct(&self, sparql: &str) -> Result<Graph> {
        // Similar pattern, return materialized graph
        let triples = self.select(sparql)?;
        Graph::from_bindings(&triples)
    }
}
```

### Renderable Trait

**Purpose**: Abstracts template rendering allowing Tera, Handlebars, or custom template engines

**What It Does**:
- Loads templates from various sources (files, strings, archives)
- Renders templates with variable bindings
- Manages template inheritance and partials
- Handles custom template functions and filters

**When to Use**:
- Custom template engines (Handlebars, Liquid, etc.)
- Specialized code formatting requirements
- Template loading from non-filesystem sources (databases, URLs)

**How to Implement**:

```rust
use ggen_core::templates::Renderable;
use serde_json::Value;

pub struct MyTemplateEngine {
    templates: HashMap<String, String>,
}

impl Renderable for MyTemplateEngine {
    /// Load a template by name
    fn load(&mut self, name: &str, content: &str) -> Result<()> {
        // Store template for later rendering
        self.templates.insert(name.to_string(), content.to_string());
        Ok(())
    }

    /// Render a template with given context
    fn render(&self, name: &str, context: &Value) -> Result<String> {
        // 1. Look up template by name
        // 2. Replace {{variable}} patterns with context values
        // 3. Process control flow (if, for, etc.)
        // 4. Return rendered string
        let template = self.templates
            .get(name)
            .ok_or_else(|| Error::template_not_found(name))?;

        let rendered = my_render_fn(template, context)?;
        Ok(rendered)
    }

    /// Check if template exists
    fn has_template(&self, name: &str) -> bool {
        self.templates.contains_key(name)
    }
}
```

**Example: Handlebars Template Engine**

```rust
use handlebars::Handlebars;
use serde_json::Value;

pub struct HandlebarsRenderer {
    hb: Handlebars<'static>,
}

impl HandlebarsRenderer {
    pub fn new() -> Self {
        Self {
            hb: Handlebars::new(),
        }
    }
}

impl Renderable for HandlebarsRenderer {
    fn load(&mut self, name: &str, content: &str) -> Result<()> {
        self.hb.register_template_string(name, content)
            .map_err(|e| Error::template_error(e.to_string()))
    }

    fn render(&self, name: &str, context: &Value) -> Result<String> {
        self.hb.render(name, context)
            .map_err(|e| Error::template_error(e.to_string()))
    }

    fn has_template(&self, name: &str) -> bool {
        self.hb.has_template(name)
    }
}
```

## Rule<Q, T> Pattern

The `Rule<Q, T>` pattern is the core abstraction for generative rules where:
- **Q**: A Queryable backend (RDF source)
- **T**: A Renderable template engine

**Design Benefits**:
1. **Composability**: Rules compose independently
2. **Testability**: Mock Q and T for unit tests
3. **Flexibility**: Swap implementations without code changes
4. **Type Safety**: Compile-time verification of rule logic

### Rule Execution Model

```
┌────────────────────────────────────┐
│  Rule<Queryable, Renderable>       │
├────────────────────────────────────┤
│ SPARQL SELECT Query                │
│         ↓                          │
│ Variable Bindings                  │
│         ↓                          │
│ Template Rendering                 │
│         ↓                          │
│ Output Code                        │
└────────────────────────────────────┘
```

**Generic Rule Implementation**:

```rust
use ggen_core::codegen::Rule;
use ggen_core::rdf::Queryable;
use ggen_core::templates::Renderable;

pub struct GenerationRule<Q: Queryable, T: Renderable> {
    name: String,
    query: String,
    template: String,
    queryable: Q,
    renderer: T,
}

impl<Q: Queryable, T: Renderable> GenerationRule<Q, T> {
    pub fn new(
        name: &str,
        query: &str,
        template: &str,
        queryable: Q,
        renderer: T,
    ) -> Self {
        Self {
            name: name.to_string(),
            query: query.to_string(),
            template: template.to_string(),
            queryable,
            renderer,
        }
    }

    /// Execute the rule: Query → Render → Output
    pub fn execute(&self) -> Result<Vec<GeneratedFile>> {
        // μ₂: Extract - execute SELECT query
        let bindings = self.queryable.select(&self.query)?;

        // μ₃: Emit - render template for each binding
        let mut files = Vec::new();
        for binding in bindings {
            let context = binding_to_json(&binding);
            let output = self.renderer.render(&self.template, &context)?;
            files.push(GeneratedFile::new(&self.name, output));
        }

        Ok(files)
    }
}
```

## Error Handling Strategy

ggen-codegen follows **fail-fast** error handling with deterministic recovery:

### Error Categories

| Category | Handling | Recovery |
|----------|----------|----------|
| **Validation** | Reported at μ₁ | User fixes input |
| **Query** | SPARQL syntax errors | User corrects query |
| **Template** | Rendering failures | User fixes template |
| **IO** | File system errors | Retry with backoff |
| **State** | Inconsistent graph | Rebuild from scratch |

### Error Propagation Model

```rust
use ggen_utils::error::{Error, Result};

pub enum GgenError {
    /// RDF validation failed (user input error)
    ValidationError { rule: String, reason: String },

    /// SPARQL query malformed
    QueryError { query: String, cause: String },

    /// Template rendering failed
    TemplateError { template: String, cause: String },

    /// File I/O operation failed
    IoError { path: PathBuf, cause: String },

    /// Internal state inconsistency
    StateError { message: String },
}

impl From<GgenError> for Result<T> {
    fn from(err: GgenError) -> Self {
        Err(Error::from(err))
    }
}

// Usage pattern
pub fn execute_rule<Q, T>(rule: &Rule<Q, T>) -> Result<Vec<GeneratedFile>> {
    // Validate inputs first (fail-fast)
    rule.validate()
        .map_err(|e| Error::validation_error(&rule.name, &e.to_string()))?;

    // Execute with detailed error context
    rule.execute()
        .map_err(|e| match e {
            GgenError::QueryError { query, cause } => {
                Error::query_error(&query, &cause)
            }
            GgenError::TemplateError { template, cause } => {
                Error::template_error_with_context(&template, &cause)
            }
            other => Error::from(other),
        })
}
```

### Retry Logic for Transient Failures

```rust
use std::time::Duration;

pub async fn execute_with_retry<F, T>(
    mut f: F,
    max_attempts: u32,
) -> Result<T>
where
    F: FnMut() -> Result<T>,
{
    let mut attempt = 0;
    let mut backoff = Duration::from_millis(100);

    loop {
        attempt += 1;

        match f() {
            Ok(result) => return Ok(result),
            Err(e) if e.is_transient() && attempt < max_attempts => {
                eprintln!("Attempt {} failed, retrying in {:?}...",
                    attempt, backoff);
                tokio::time::sleep(backoff).await;
                backoff *= 2; // exponential backoff
            }
            Err(e) => return Err(e),
        }
    }
}
```

## Determinism and Content Hashing

**Goal**: Generate identical output for identical inputs (reproducible builds)

### μ₄ Canonicalization Pipeline

```
Input Graph
    ↓
1. Sort triples by (subject, predicate, object)
2. Normalize namespace prefixes
3. Format numbers consistently
4. Sort template variables alphabetically
5. Canonicalize whitespace
    ↓
Canonical Output
    ↓
SHA256(output) → Content Hash
```

### SHA256 Content Hash

Every generated file includes a deterministic hash:

```rust
use sha2::{Sha256, Digest};

pub fn hash_file_content(content: &[u8]) -> String {
    let mut hasher = Sha256::new();
    hasher.update(content);

    let result = hasher.finalize();
    format!("{:x}", result) // hex string
}

// Usage in pipeline
let output = renderer.render(&template, &context)?;
let hash = hash_file_content(output.as_bytes());

let audit = AuditEntry {
    file_path: path.clone(),
    content_hash: hash,
    timestamp: Instant::now(),
};
```

### Verifying Reproducibility

```bash
# Generate twice and compare hashes
ggen sync --audit true --output-dir build1/
ggen sync --audit true --output-dir build2/

# Extract hashes from audit trails
jq '.files[].content_hash' build1/.ggen-audit.json > hashes1.txt
jq '.files[].content_hash' build2/.ggen-audit.json > hashes2.txt

# Verify identical
diff hashes1.txt hashes2.txt
```

## Integration Example: Custom Domain Generator

Here's a complete example implementing a custom domain-specific code generator:

```rust
use ggen_core::rdf::Queryable;
use ggen_core::templates::Renderable;
use ggen_core::codegen::Rule;

// 1. Custom Queryable for domain-specific ontology
pub struct DomainOntology {
    graph: Graph,
}

impl Queryable for DomainOntology {
    fn select(&self, sparql: &str) -> Result<Vec<Binding>> {
        self.graph.query(sparql)
    }

    fn construct(&self, sparql: &str) -> Result<Graph> {
        self.graph.construct(sparql)
    }
}

// 2. Custom Renderable for domain language templates
pub struct DomainTemplateEngine {
    templates: HashMap<String, String>,
}

impl Renderable for DomainTemplateEngine {
    fn load(&mut self, name: &str, content: &str) -> Result<()> {
        self.templates.insert(name.to_string(), content.to_string());
        Ok(())
    }

    fn render(&self, name: &str, context: &Value) -> Result<String> {
        // Custom rendering logic for domain language
        Ok(format!("generated from {}", name))
    }
}

// 3. Domain-specific rule execution
pub fn generate_domain_code() -> Result<Vec<GeneratedFile>> {
    let ontology = DomainOntology::load("schema.ttl")?;
    let mut engine = DomainTemplateEngine::new();

    engine.load("model", include_str!("templates/model.hbs"))?;
    engine.load("api", include_str!("templates/api.hbs"))?;

    let rule = Rule::new("domain_models", EXTRACT_MODELS_QUERY, "model");
    let files = rule.execute(&ontology, &engine)?;

    Ok(files)
}
```

## See Also

- [ggen-yawl YAWL_RULES.md](../ggen-yawl/YAWL_RULES.md) - Specific YAWL transformation rules
- [ggen-yawl INTEGRATION.md](../ggen-yawl/INTEGRATION.md) - Integration with Spring Boot
- RDF Specification: https://www.w3.org/RDF/
- SPARQL: https://www.w3.org/TR/sparql11-query/
- Tera Templating: https://tera.netlify.app/

