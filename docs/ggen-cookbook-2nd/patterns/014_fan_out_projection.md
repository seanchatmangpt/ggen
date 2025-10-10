<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->
**Table of Contents**

- [Pattern 014: Fan-Out Projection](#pattern-014-fan-out-projection)
  - [Intent](#intent)
  - [Motivation](#motivation)
  - [Applicability](#applicability)
  - [Structure](#structure)
  - [Implementation](#implementation)
    - [Step 1: Define Your Graph Data](#step-1-define-your-graph-data)
    - [Step 2: Create Fan-Out Template](#step-2-create-fan-out-template)
    - [Step 3: Execute Fan-Out Generation](#step-3-execute-fan-out-generation)
    - [Generated Output Examples](#generated-output-examples)
  - [Advanced Example: REST API Generation](#advanced-example-rest-api-generation)
  - [Benefits](#benefits)
  - [Related Patterns](#related-patterns)
  - [Known Uses](#known-uses)
  - [See Also](#see-also)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

# Pattern 014: Fan-Out Projection

## Intent
Generate multiple files from a single template by projecting across RDF graph nodes, enabling 1-to-N file generation.

## Motivation
Traditional templating creates one output file per template invocation. Fan-out projection allows a single template to generate multiple files by iterating over graph data, perfect for scaffolding APIs, models, or any repetitive structure.

## Applicability
- Generating CRUD endpoints for multiple entities
- Creating model classes from ontology definitions
- Scaffolding test files for multiple components
- Building documentation pages from metadata

## Structure

```yaml
---
ggen:projection:
  query: |
    PREFIX ex: <http://example.org/>
    SELECT ?entity ?name ?description
    WHERE {
      ?entity a ex:Model ;
              ex:name ?name ;
              ex:description ?description .
    }
  template: "src/models/{{name}}.rs"
---
```

## Implementation

### Step 1: Define Your Graph Data

**File: `entities.ttl`**
```turtle
@prefix ex: <http://example.org/> .
@prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .

ex:User a ex:Model ;
    ex:name "User" ;
    ex:description "User account model" ;
    ex:hasField ex:UserName, ex:UserEmail .

ex:UserName a ex:Field ;
    ex:fieldName "name" ;
    ex:fieldType "String" .

ex:UserEmail a ex:Field ;
    ex:fieldName "email" ;
    ex:fieldType "String" .

ex:Product a ex:Model ;
    ex:name "Product" ;
    ex:description "Product catalog model" ;
    ex:hasField ex:ProductTitle, ex:ProductPrice .

ex:ProductTitle a ex:Field ;
    ex:fieldName "title" ;
    ex:fieldType "String" .

ex:ProductPrice a ex:Field ;
    ex:fieldName "price" ;
    ex:fieldType "f64" .

ex:Order a ex:Model ;
    ex:name "Order" ;
    ex:description "Customer order model" ;
    ex:hasField ex:OrderId, ex:OrderTotal .

ex:OrderId a ex:Field ;
    ex:fieldName "id" ;
    ex:fieldType "u64" .

ex:OrderTotal a ex:Field ;
    ex:fieldName "total" ;
    ex:fieldType "f64" .
```

### Step 2: Create Fan-Out Template

**File: `model_generator.tmpl`**
```handlebars
---
ggen:projection:
  query: |
    PREFIX ex: <http://example.org/>
    SELECT ?entity ?name ?description
    WHERE {
      ?entity a ex:Model ;
              ex:name ?name ;
              ex:description ?description .
    }
  template: "src/models/{{name | snake_case}}.rs"
---
// {{description}}
// Generated from: {{entity}}

use serde::{Deserialize, Serialize};

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct {{name}} {
{{#sparql}}
    PREFIX ex: <http://example.org/>
    SELECT ?fieldName ?fieldType
    WHERE {
      <{{entity}}> ex:hasField ?field .
      ?field ex:fieldName ?fieldName ;
             ex:fieldType ?fieldType .
    }
{{/sparql}}
    pub {{fieldName}}: {{fieldType}},
{{/sparql}}
}

impl {{name}} {
    pub fn new(
{{#sparql}}
    PREFIX ex: <http://example.org/>
    SELECT ?fieldName ?fieldType
    WHERE {
      <{{entity}}> ex:hasField ?field .
      ?field ex:fieldName ?fieldName ;
             ex:fieldType ?fieldType .
    }
{{/sparql}}
        {{fieldName}}: {{fieldType}},
{{/sparql}}
    ) -> Self {
        Self {
{{#sparql}}
    PREFIX ex: <http://example.org/>
    SELECT ?fieldName
    WHERE {
      <{{entity}}> ex:hasField ?field .
      ?field ex:fieldName ?fieldName .
    }
{{/sparql}}
            {{fieldName}},
{{/sparql}}
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_{{name | snake_case}}_creation() {
        let instance = {{name}}::new(
{{#sparql}}
    PREFIX ex: <http://example.org/>
    SELECT ?fieldName ?fieldType
    WHERE {
      <{{entity}}> ex:hasField ?field .
      ?field ex:fieldName ?fieldName ;
             ex:fieldType ?fieldType .
    }
{{/sparql}}
            {{#if (eq fieldType "String")}}
            "test_{{fieldName}}".to_string(),
            {{else if (eq fieldType "u64")}}
            123,
            {{else if (eq fieldType "f64")}}
            99.99,
            {{/if}}
{{/sparql}}
        );
        assert!(format!("{:?}", instance).contains("{{name}}"));
    }
}
```

### Step 3: Execute Fan-Out Generation

```bash
# Load graph and generate files
ggen gen model_generator.tmpl --graph entities.ttl

# Output:
# Generated: src/models/user.rs
# Generated: src/models/product.rs
# Generated: src/models/order.rs
```

### Generated Output Examples

**File: `src/models/user.rs`**
```rust
// User account model
// Generated from: http://example.org/User

use serde::{Deserialize, Serialize};

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct User {
    pub name: String,
    pub email: String,
}

impl User {
    pub fn new(
        name: String,
        email: String,
    ) -> Self {
        Self {
            name,
            email,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_user_creation() {
        let instance = User::new(
            "test_name".to_string(),
            "test_email".to_string(),
        );
        assert!(format!("{:?}", instance).contains("User"));
    }
}
```

**File: `src/models/product.rs`**
```rust
// Product catalog model
// Generated from: http://example.org/Product

use serde::{Deserialize, Serialize};

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Product {
    pub title: String,
    pub price: f64,
}

impl Product {
    pub fn new(
        title: String,
        price: f64,
    ) -> Self {
        Self {
            title,
            price,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_product_creation() {
        let instance = Product::new(
            "test_title".to_string(),
            99.99,
        );
        assert!(format!("{:?}", instance).contains("Product"));
    }
}
```

## Advanced Example: REST API Generation

**File: `api_entities.ttl`**
```turtle
@prefix api: <http://api.example.org/> .

api:users a api:Endpoint ;
    api:resource "User" ;
    api:path "/users" ;
    api:methods "GET,POST,PUT,DELETE" ;
    api:auth "required" .

api:products a api:Endpoint ;
    api:resource "Product" ;
    api:path "/products" ;
    api:methods "GET,POST" ;
    api:auth "optional" .

api:orders a api:Endpoint ;
    api:resource "Order" ;
    api:path "/orders" ;
    api:methods "GET,POST" ;
    api:auth "required" .
```

**File: `api_generator.tmpl`**
```handlebars
---
ggen:projection:
  query: |
    PREFIX api: <http://api.example.org/>
    SELECT ?endpoint ?resource ?path ?methods ?auth
    WHERE {
      ?endpoint a api:Endpoint ;
                api:resource ?resource ;
                api:path ?path ;
                api:methods ?methods ;
                api:auth ?auth .
    }
  template: "src/api/{{resource | snake_case}}_routes.rs"
---
// API routes for {{resource}}
// Path: {{path}}
// Auth: {{auth}}

use axum::{
    routing::{get, post, put, delete},
    Router, Json,
};
use crate::models::{{resource | snake_case}}::{{resource}};
{{#if (eq auth "required")}}
use crate::middleware::auth::AuthGuard;
{{/if}}

pub fn routes() -> Router {
    Router::new()
{{#each (split methods ",")}}
    {{#if (eq this "GET")}}
        .route("{{../path}}", get(list_{{../resource | snake_case}}))
        .route("{{../path}}/:id", get(get_{{../resource | snake_case}}))
    {{/if}}
    {{#if (eq this "POST")}}
        .route("{{../path}}", post(create_{{../resource | snake_case}}))
    {{/if}}
    {{#if (eq this "PUT")}}
        .route("{{../path}}/:id", put(update_{{../resource | snake_case}}))
    {{/if}}
    {{#if (eq this "DELETE")}}
        .route("{{../path}}/:id", delete(delete_{{../resource | snake_case}}))
    {{/if}}
{{/each}}
{{#if (eq auth "required")}}
        .layer(AuthGuard)
{{/if}}
}

async fn list_{{resource | snake_case}}() -> Json<Vec<{{resource}}>> {
    // TODO: Implement list logic
    Json(vec![])
}

async fn get_{{resource | snake_case}}(id: String) -> Json<Option<{{resource}}>> {
    // TODO: Implement get logic
    Json(None)
}
{{#if (contains methods "POST")}}

async fn create_{{resource | snake_case}}(Json(payload): Json<{{resource}}>) -> Json<{{resource}}> {
    // TODO: Implement create logic
    Json(payload)
}
{{/if}}
{{#if (contains methods "PUT")}}

async fn update_{{resource | snake_case}}(id: String, Json(payload): Json<{{resource}}>) -> Json<{{resource}}> {
    // TODO: Implement update logic
    Json(payload)
}
{{/if}}
{{#if (contains methods "DELETE")}}

async fn delete_{{resource | snake_case}}(id: String) -> Json<bool> {
    // TODO: Implement delete logic
    Json(true)
}
{{/if}}
```

## Benefits

1. **Reduced Boilerplate**: Write once, generate many
2. **Consistency**: All generated files follow the same pattern
3. **Maintainability**: Update template to update all outputs
4. **Traceability**: Generated code links back to graph data
5. **Automation**: Perfect for CI/CD scaffolding pipelines

## Related Patterns

- **015: Immutability First** - Protect generated files from overwrites
- **017: Graph-Driven Paths** - Dynamic file paths from graph data
- **091: Idempotent Injection** - Safe updates to generated files

## Known Uses

- Generating CRUD APIs from entity models
- Creating GraphQL resolvers from schema
- Building database migrations from ontologies
- Scaffolding microservices from architecture graphs

## See Also

- GGen Projection Documentation
- SPARQL Query Patterns
- Template Path Interpolation
