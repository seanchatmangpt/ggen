# How-To Guide: Mapping Domain Models to RDF Triples

This guide explains how to convert application domain models into RDF triples and persist them using `oxigraph`, similar to the `RdfMapper` implemented in the `ggen-marketplace`.

## Problem

You have a domain model (e.g., `Package`) and you want to persist it in a semantic RDF graph so it can be queried using SPARQL. 

## Solution: Bidirectional RDF Mapping

### 1. Define Namespaces and Classes

First, ensure you have predefined your URIs in a structured way to avoid typos:

```rust
pub struct Namespaces;
impl Namespaces {
    pub const GGEN: &'static str = "https://ggen.io/marketplace/";
    pub const RDF: &'static str = "http://www.w3.org/1999/02/22-rdf-syntax-ns#";
}

pub struct Classes;
impl Classes {
    pub fn package() -> String { format!("{}Package", Namespaces::GGEN) }
}
```

### 2. Implement an RDF Mapper

Create a struct that holds an `Arc<Store>` (from `oxigraph`):

```rust
use oxigraph::store::Store;
use oxigraph::model::{NamedNode, QuadRef, Literal, GraphNameRef};
use std::sync::Arc;

pub struct RdfMapper {
    store: Arc<Store>,
}

impl RdfMapper {
    pub fn new(store: Arc<Store>) -> Self {
        Self { store }
    }
}
```

### 3. Converting Domain Entities to Triples

When converting an entity, you need to map its fields to RDF properties and insert them into the store.

```rust
impl RdfMapper {
    pub fn insert_entity(&self, id: &str, name: &str) -> Result<(), Box<dyn std::error::Error>> {
        let subject_uri = NamedNode::new(format!("{}packages/{}", Namespaces::GGEN, id))?;
        
        // Insert RDF Type
        let type_pred = NamedNode::new(format!("{}type", Namespaces::RDF))?;
        let package_class = NamedNode::new(Classes::package())?;
        
        self.store.insert(QuadRef::new(&subject_uri, &type_pred, &package_class, GraphNameRef::DefaultGraph))?;

        // Insert Literal properties
        let name_pred = NamedNode::new(format!("{}name", Namespaces::GGEN))?;
        let name_lit = Literal::new_simple_literal(name);
        
        self.store.insert(QuadRef::new(&subject_uri, &name_pred, &name_lit, GraphNameRef::DefaultGraph))?;

        Ok(())
    }
}
```

### 4. Reconstructing Entities using SPARQL

To rebuild the domain model, execute a SPARQL query against the store:

```rust
impl RdfMapper {
    pub fn get_entity_name(&self, id: &str) -> Result<String, Box<dyn std::error::Error>> {
        let query = format!(
            r"
            SELECT ?name WHERE {{
                <{}packages/{}> <{}name> ?name .
            }}
            ",
            Namespaces::GGEN, id, Namespaces::GGEN
        );

        if let oxigraph::sparql::QueryResults::Solutions(mut solutions) = self.store.query(&query)? {
            if let Some(Ok(solution)) = solutions.next() {
                if let Some(oxigraph::model::Term::Literal(lit)) = solution.get("name") {
                    return Ok(lit.value().to_string());
                }
            }
        }
        Err("Not found".into())
    }
}
```

## Best Practices
- Isolate property strings into constants or functions (e.g., `Properties::name()`).
- Always gracefully handle `OPTIONAL` fields during query extraction.
- Use typed literals (e.g., `xsd:dateTime` or `xsd:integer`) when storing non-string primitives.
