# Graph Querying API: Programmatic RDF Access in ggen

> **The RDF graph is the heart of ggen.** Learn to query, analyze, and manipulate it programmatically.

## Quick Start

```rust
use ggen_core::graph::Graph;
use ggen_core::query::SparqlQuery;

fn main() -> Result<()> {
    // Load ontology
    let mut graph = Graph::new();
    graph.load_file("ontology/domain.ttl")?;

    // Query: Find all classes
    let query = SparqlQuery::select(r#"
        PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
        SELECT ?class ?label
        WHERE {
            ?class a rdfs:Class .
            OPTIONAL { ?class rdfs:label ?label . }
        }
    "#)?;

    let results = graph.query(&query)?;

    for binding in results.bindings() {
        let class = binding.get("class")?;
        let label = binding.get("label")?;
        println!("Class: {} ({})", class, label);
    }

    Ok(())
}
```

---

## Module Overview

```
ggen_core::graph
├── core         // RDF triple storage and indexing
├── export       // Export to RDF/N-Triples/JSON-LD
├── query        // SPARQL SELECT/CONSTRUCT/ASK
└── update       // INSERT/DELETE/MODIFY triples
```

---

## Core Operations

### 1. Loading Ontologies

```rust
use ggen_core::graph::{Graph, GraphFormat};

// Load from Turtle file
let graph = Graph::load("ontology/domain.ttl")?;

// Load from RDF/XML
let graph = Graph::load_with_format(
    "ontology/schema.rdf",
    GraphFormat::RdfXml
)?;

// Load from JSON-LD
let graph = Graph::load_with_format(
    "ontology/schema.jsonld",
    GraphFormat::JsonLd
)?;

// Create empty graph
let mut graph = Graph::new();
```

### 2. Adding Triples Programmatically

```rust
use ggen_core::graph::{Graph, Triple, Term};

let mut graph = Graph::new();

// Add class definition
graph.add_triple(
    Term::iri("https://example.com/User"),
    Term::iri("http://www.w3.org/1999/02/22-rdf-syntax-ns#type"),
    Term::iri("http://www.w3.org/2000/01/rdf-schema#Class")
)?;

// Add label
graph.add_triple(
    Term::iri("https://example.com/User"),
    Term::iri("http://www.w3.org/2000/01/rdf-schema#label"),
    Term::literal("User")
)?;

// Helper: Add with Turtle-style prefixes
graph.add_triple_with_prefixes(
    "ex:User",
    "rdf:type",
    "rdfs:Class",
    &[("ex", "https://example.com/"), ("rdf", "http://www.w3.org/1999/02/22-rdf-syntax-ns#")]
)?;
```

### 3. SPARQL SELECT (Querying)

```rust
use ggen_core::query::SparqlQuery;

// Find all users and their email addresses
let query = SparqlQuery::select(r#"
    PREFIX ex: <https://example.com/>

    SELECT ?user ?email ?age
    WHERE {
        ?user a ex:User ;
              ex:email ?email .
        OPTIONAL { ?user ex:age ?age . }
    }
    ORDER BY ?user
    LIMIT 100
"#)?;

let results = graph.query(&query)?;

// Iterate results
for binding in results.bindings() {
    let user = binding.get("user")?;
    let email = binding.get("email")?;
    let age = binding.get_opt("age")?;

    println!("{}: {} (age: {:?})", user, email, age);
}
```

### 4. SPARQL CONSTRUCT (Graph Transformation)

```rust
// Transform ontology: materialize inheritance
let rule = SparqlQuery::construct(r#"
    PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>

    CONSTRUCT {
        ?sub :allSuperclasses ?super .
    }
    WHERE {
        ?sub rdfs:subClassOf+ ?super .
    }
"#)?;

// Apply rule
let enriched = graph.query(&rule)?;

// Write back
graph.merge_results(&enriched)?;
```

### 5. SPARQL ASK (Boolean Queries)

```rust
// Check if a specific class exists
let query = SparqlQuery::ask(r#"
    PREFIX ex: <https://example.com/>

    ASK {
        ex:User a <http://www.w3.org/2000/01/rdf-schema#Class> .
    }
"#)?;

let exists = graph.query_boolean(&query)?;
println!("User class exists: {}", exists);
```

### 6. Exporting Graphs

```rust
use ggen_core::graph::GraphFormat;

// Export to Turtle
graph.save("output.ttl")?;

// Export to RDF/XML
graph.save_with_format("output.rdf", GraphFormat::RdfXml)?;

// Export to JSON-LD
graph.save_with_format("output.jsonld", GraphFormat::JsonLd)?;

// Export to N-Triples (line-based)
graph.save_with_format("output.nt", GraphFormat::NTriples)?;
```

---

## Common Query Patterns

### Pattern A: Find All Entities of a Type

```rust
let query = SparqlQuery::select(r#"
    PREFIX ex: <https://example.com/>

    SELECT ?entity
    WHERE {
        ?entity a ex:Post .
    }
"#)?;

let posts = graph.query(&query)?;
```

### Pattern B: Get Entity Properties

```rust
let query = SparqlQuery::select(r#"
    PREFIX ex: <https://example.com/>

    SELECT ?property ?value
    WHERE {
        ex:User ?property ?value .
    }
"#)?;

let properties = graph.query(&query)?;
```

### Pattern C: Traverse Relationships

```rust
let query = SparqlQuery::select(r#"
    PREFIX ex: <https://example.com/>

    SELECT ?author ?post ?title
    WHERE {
        ?post ex:author ?author ;
              ex:title ?title .
    }
"#)?;

let posts_by_author = graph.query(&query)?;
```

### Pattern D: Aggregate Data

```rust
let query = SparqlQuery::select(r#"
    PREFIX ex: <https://example.com/>

    SELECT ?user (COUNT(?post) AS ?post_count)
    WHERE {
        ?post ex:author ?user .
    }
    GROUP BY ?user
    HAVING (COUNT(?post) > 5)
"#)?;

let prolific_users = graph.query(&query)?;
```

### Pattern E: Find Hierarchy

```rust
let query = SparqlQuery::select(r#"
    PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>

    SELECT ?class ?parent ?grandparent
    WHERE {
        ?class rdfs:subClassOf ?parent .
        ?parent rdfs:subClassOf ?grandparent .
    }
"#)?;

let hierarchies = graph.query(&query)?;
```

---

## Integration Examples

### Example 1: Build a CLI Command from Ontology

```rust
use ggen_core::graph::Graph;
use ggen_core::query::SparqlQuery;

// Generate `ggen introspect` output from ontology
fn introspect_capabilities(graph: &Graph) -> Result<String> {
    let query = SparqlQuery::select(r#"
        PREFIX ex: <https://example.com/>

        SELECT ?class ?capability ?arg_count
        WHERE {
            ?class :hasCapability ?capability .
            ?capability :argCount ?arg_count .
        }
    "#)?;

    let results = graph.query(&query)?;
    let mut output = String::new();

    for binding in results.bindings() {
        let class = binding.get("class")?;
        let capability = binding.get("capability")?;
        let args = binding.get("arg_count")?;
        output.push_str(&format!("{}.{} ({})\n", class, capability, args));
    }

    Ok(output)
}
```

### Example 2: Validate Ontology Completeness

```rust
// Check that every entity has a description
fn validate_documentation(graph: &Graph) -> Result<Vec<String>> {
    let query = SparqlQuery::select(r#"
        PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
        PREFIX ex: <https://example.com/>

        SELECT ?entity
        WHERE {
            ?entity a rdfs:Class .
            FILTER NOT EXISTS { ?entity rdfs:comment ?desc . }
        }
    "#)?;

    let results = graph.query(&query)?;
    let mut undocumented = Vec::new();

    for binding in results.bindings() {
        undocumented.push(binding.get("entity")?.to_string());
    }

    Ok(undocumented)
}
```

### Example 3: Detect Unused Definitions

```rust
// Find classes that are never used
fn find_dead_code(graph: &Graph) -> Result<Vec<String>> {
    let query = SparqlQuery::select(r#"
        PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>

        SELECT ?class
        WHERE {
            ?class a rdfs:Class .
            FILTER NOT EXISTS {
                ?instance a ?class .
            }
            FILTER NOT EXISTS {
                ?sub rdfs:subClassOf ?class .
            }
        }
    "#)?;

    let results = graph.query(&query)?;
    let mut unused = Vec::new();

    for binding in results.bindings() {
        unused.push(binding.get("class")?.to_string());
    }

    Ok(unused)
}
```

### Example 4: Generate Test Cases from Spec

```rust
// Extract test scenarios from ontology
fn generate_test_cases(graph: &Graph) -> Result<Vec<TestCase>> {
    let query = SparqlQuery::select(r#"
        PREFIX test: <http://test.example.com/>
        PREFIX ex: <https://example.com/>

        SELECT ?entity ?scenario ?expected
        WHERE {
            ?entity test:hasScenario ?scenario .
            ?scenario test:expectedResult ?expected .
        }
    "#)?;

    let results = graph.query(&query)?;
    let mut tests = Vec::new();

    for binding in results.bindings() {
        tests.push(TestCase {
            entity: binding.get("entity")?.to_string(),
            scenario: binding.get("scenario")?.to_string(),
            expected: binding.get("expected")?.to_string(),
        });
    }

    Ok(tests)
}

struct TestCase {
    entity: String,
    scenario: String,
    expected: String,
}
```

---

## Marketplace: Using the RDF-Backed Search

The marketplace uses SPARQL for intelligent package discovery:

```rust
use ggen_core::marketplace::{PackageRegistry, SearchQuery};

// Find packages that depend on JWT auth
let query = SearchQuery::sparql(r#"
    PREFIX pkg: <http://marketplace.ggen.io/package/>
    PREFIX dep: <http://marketplace.ggen.io/dependency/>

    SELECT ?package ?name ?version
    WHERE {
        ?package dep:requires ?requirement .
        ?requirement dep:package "jwt-auth" .
        ?requirement dep:minVersion ?version .
        ?package pkg:name ?name .
    }
"#);

let registry = PackageRegistry::load()?;
let matching = registry.search(&query)?;

for package in matching {
    println!("Found: {} v{}", package.name, package.version);
}
```

---

## Advanced: Graph Analysis

### Detect Circular Dependencies

```rust
fn find_circular_dependencies(graph: &Graph) -> Result<Vec<Vec<String>>> {
    // Query transitive closures and find cycles
    let query = SparqlQuery::select(r#"
        PREFIX ex: <https://example.com/>

        SELECT ?a ?b ?c ?d
        WHERE {
            ?a ex:dependsOn+ ?b .
            ?b ex:dependsOn+ ?c .
            ?c ex:dependsOn+ ?d .
            ?d ex:dependsOn+ ?a .
        }
    "#)?;

    // Process results to extract cycles
    // ... (implementation)

    Ok(Vec::new())
}
```

### Find Duplicate Patterns

```rust
fn find_duplicate_definitions(graph: &Graph) -> Result<Vec<(String, String)>> {
    // Find classes with identical properties
    let query = SparqlQuery::select(r#"
        PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>

        SELECT ?class1 ?class2 (COUNT(?prop) AS ?shared_props)
        WHERE {
            ?class1 ?prop ?value1 .
            ?class2 ?prop ?value2 .
            FILTER (?class1 < ?class2)
            FILTER (?value1 = ?value2)
        }
        GROUP BY ?class1 ?class2
        HAVING (COUNT(?prop) > 3)
    "#)?;

    // ... (process results)

    Ok(Vec::new())
}
```

---

## Error Handling

```rust
use ggen_core::graph::GraphError;

match graph.query(&query) {
    Ok(results) => {
        // Process results
    }
    Err(GraphError::InvalidQuery(msg)) => {
        eprintln!("Invalid SPARQL query: {}", msg);
    }
    Err(GraphError::ParseError(msg)) => {
        eprintln!("Ontology parse error: {}", msg);
    }
    Err(e) => {
        eprintln!("Query failed: {}", e);
    }
}
```

---

## Performance Tips

1. **Index frequently-queried properties**
   ```rust
   graph.create_index("http://example.com/author")?;
   ```

2. **Use FILTER for post-processing heavy queries**
   ```sparql
   FILTER (?age > 18)  // SPARQL handles this
   ```

3. **Limit result set**
   ```sparql
   LIMIT 1000  // Don't fetch millions
   ```

4. **Use OPTIONAL wisely**
   ```sparql
   OPTIONAL { ?x ex:email ?email . }  // Only if needed
   ```

5. **Profile slow queries**
   ```bash
   ggen sync --explain-sparql  # Shows query plan
   ```

---

## Next Steps

- Learn [SPARQL Inference Patterns](./SPARQL_INFERENCE_GUIDE.md)
- Explore [Marketplace Package Discovery](./MARKETPLACE_CREATION_GUIDE.md)
- Build an [Agentic System](./AI_AGI_INTEGRATION_GUIDE.md) with graph queries
- Integrate with your [CI/CD Pipeline](./ADVANCED_CONFIGURATION.md#ci-integration)

