# RDF & SPARQL Integration Guide

## Oxigraph Triple Store

ggen uses **Oxigraph 0.5** as its RDF engine for semantic graph operations.

### Store Initialization

```rust
use oxigraph::store::Store;
use std::path::Path;

// In-memory store
let store = Store::new().expect("Failed to create store");

// Persistent store (RocksDB backend)
let store = Store::open(Path::new("./graph.db"))
    .expect("Failed to open persistent store");
```

### Triple Structure

RDF triples consist of (subject, predicate, object):

```
subject (Entity)  predicate (Relationship)  object (Value/Entity)
─────────────────────────────────────────────────────────────
<http://example.com/User#123>  <http://xmlns.com/foaf/0.1/name>  "John Doe"
<http://example.com/Order#456> <http://vocab.example.com/contains> <http://example.com/Item#789>
```

### Graph Caching & Invalidation

ggen implements **epoch-based cache invalidation**:

```rust
pub struct GraphCache {
    epoch: AtomicUsize,
    cache: RwLock<HashMap<String, CachedQuery>>,
}

impl GraphCache {
    pub fn invalidate(&self) {
        self.epoch.fetch_add(1, Ordering::SeqCst);
        self.cache.write().clear();
    }

    pub fn query_cached(&self, sparql: &str) -> Result<Vec<Solution>> {
        let current_epoch = self.epoch.load(Ordering::SeqCst);

        if let Some(cached) = self.cache.read().get(sparql) {
            if cached.epoch == current_epoch {
                return Ok(cached.results.clone());
            }
        }

        // Cache miss or epoch mismatch - recompute
        let results = self.store.query(sparql)?;
        self.cache.write().insert(
            sparql.to_string(),
            CachedQuery { results: results.clone(), epoch: current_epoch },
        );
        Ok(results)
    }
}
```

**Invalidation triggers:**
- Template modification
- Ontology update
- Package registry change
- Explicit `invalidate()` call

## SPARQL Query Execution

### Query Pattern: SELECT with FILTER

```sparql
SELECT ?entity ?type ?property
WHERE {
  ?entity a ?type .
  ?entity ?property ?value .
  FILTER (?type = <http://vocab.example.com/Entity>)
  FILTER (STRLEN(?property) > 5)
}
ORDER BY ?entity
LIMIT 100
```

### Pattern: CONSTRUCT (Return RDF triples)

```sparql
CONSTRUCT {
  ?template rdfs:label ?label .
  ?template ggen:framework ?framework .
}
WHERE {
  ?template a ggen:Template .
  ?template rdfs:label ?label .
  ?template ggen:framework ?framework .
}
```

### Pattern: ASK (Boolean query)

```sparql
ASK {
  <http://marketplace.ggen.io/templates/rust-api> a ggen:Template .
  <http://marketplace.ggen.io/templates/rust-api> ggen:verified true .
}
```

### Safe Query Execution

Always use parameterized queries to prevent injection:

```rust
pub fn query_templates_by_framework(
    store: &Store,
    framework: &str,
) -> Result<Vec<Template>> {
    // WRONG (vulnerable to injection):
    // let query = format!("SELECT ?t WHERE {{ ?t ggen:framework \"{}\" }}", framework);

    // CORRECT (parameterized):
    let query = r#"
        SELECT ?id ?name ?description
        WHERE {
            ?id a ggen:Template .
            ?id rdfs:label ?name .
            ?id rdfs:comment ?description .
            ?id ggen:framework ?framework .
            FILTER (?framework = ?targetFramework)
        }
    "#;

    let target_framework = oxigraph::sparql::Variable::new("targetFramework");
    let solution = store.query_with_variables(query, &[(target_framework, framework)])?;

    Ok(solution.into_iter().map(Template::from_solution).collect())
}
```

## Template Integration with RDF

### RDF Metadata Frontmatter

Templates include RDF metadata as YAML frontmatter:

```yaml
---
rdf:
  id: http://marketplace.ggen.io/templates/rust-api
  type: ggen:Template
  framework: axum
  language: rust
  version: "1.0.0"
  tags:
    - api
    - rest
    - async
  dependencies:
    - template: base-rust
    - template: error-handling
metadata:
  author: ggen-team
  created: 2024-01-15
  updated: 2024-01-20
  popularity: 9.2
---

# Template content here
```

### SPARQL Queries in Templates

Templates can embed SPARQL to fetch dynamic data during rendering:

```liquid
{%- set templates = sparql(
    "SELECT ?t WHERE { ?t a ggen:Template . ?t ggen:framework ?fw . FILTER (?fw = 'rust') }",
    ontology=context.ontology
) -%}

Available Rust templates:
{% for template in templates %}
  - {{ template.name }} (v{{ template.version }})
{% endfor %}
```

### Tera Template Engine with RDF Context

```rust
pub struct TemplateContext {
    pub ontology: Store,
    pub project_name: String,
    pub package_name: String,
    pub rdf_query_cache: GraphCache,
}

pub fn render_template(
    template_str: &str,
    context: &TemplateContext,
) -> Result<String> {
    let mut tera = Tera::new("templates/**/*.tmpl")?;

    // Register RDF query function
    tera.register_function(
        "sparql",
        |args: &HashMap<String, serde_json::Value>| {
            let query = args.get("query")
                .and_then(|v| v.as_str())
                .ok_or("Query required")?;

            context.rdf_query_cache.query_cached(query)
                .map(|results| serde_json::to_value(results).unwrap())
                .map_err(|e| e.to_string())
        },
    );

    tera.render_str(template_str, context)
}
```

## Query Performance Optimization

### Index Strategy

Oxigraph provides automatic indexing on:
- Subject-predicate-object (SPO) index
- Subject-object-predicate (SOP) index
- Predicate-subject-object (PSO) index
- And 6 more permutations for optimal query planning

Use most selective filter first:

```sparql
-- FAST (filters by type first, then property)
SELECT ?entity WHERE {
  ?entity a <http://vocab.example.com/User> .
  ?entity <http://vocab.example.com/active> true .
}

-- SLOW (broader triples fetched first)
SELECT ?entity WHERE {
  ?entity <http://vocab.example.com/active> true .
  ?entity a <http://vocab.example.com/User> .
}
```

### Batch Operations

Efficient bulk loading:

```rust
pub fn bulk_insert_triples(
    store: &Store,
    triples: Vec<(String, String, String)>,
) -> Result<()> {
    let mut batch = Vec::new();

    for (subject, predicate, object) in triples {
        let s = Subject::NamedNode(NamedNode::new(subject)?);
        let p = NamedNode::new(predicate)?;
        let o = Term::Literal(Literal::new_simple_literal(object));
        batch.push((s, p, o));
    }

    store.insert_all(batch.iter())?;
    Ok(())
}
```

## Critical Rules

1. **ALWAYS use parameterized queries** - Never string interpolation
2. **ALWAYS cache SPARQL results** - Use GraphCache to avoid redundant queries
3. **INVALIDATE on mutations** - Call cache.invalidate() after any insert/delete
4. **ALWAYS validate RDF URIs** - Use NamedNode::new() with error checking
5. **LIMIT query results** - Use LIMIT clause to prevent huge result sets
6. **PREFER SELECT over CONSTRUCT** - For performance (fewer triple iterations)
7. **TEST with representative data** - Ontologies in tests should be realistic size

---

## Examples

### Marketplace Package Search

```rust
pub fn search_packages(
    store: &Store,
    query_text: &str,
    framework: Option<&str>,
) -> Result<Vec<Package>> {
    let mut sparql = r#"
        SELECT ?id ?name ?version ?framework
        WHERE {
            ?id a ggen:Package .
            ?id rdfs:label ?name .
            ?id dcat:version ?version .
            ?id ggen:framework ?fw .
            FILTER (CONTAINS(LCASE(?name), LCASE(?queryText)))
    "#.to_string();

    if framework.is_some() {
        sparql.push_str("FILTER (?fw = ?targetFramework)");
    }

    sparql.push_str("} LIMIT 100");

    store.query(&sparql)
        .map(|solutions| solutions.into_iter().map(Package::from_solution).collect())
}
```

### Extract Dependency Graph

```sparql
SELECT ?pkg ?dependency (COUNT(?dep) as ?dep_count)
WHERE {
  ?pkg a ggen:Package .
  ?pkg ggen:dependsOn ?dependency .
  ?dependency a ggen:Package .
}
GROUP BY ?pkg ?dependency
ORDER BY DESC(?dep_count)
```
