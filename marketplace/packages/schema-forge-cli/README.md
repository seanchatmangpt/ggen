## schema-forge-cli

[![Crates.io](https://img.shields.io/crates/v/schema-forge-cli)](https://crates.io/crates/schema-forge-cli)
[![License](https://img.shields.io/badge/license-MIT%20OR%20Apache--2.0-blue)](LICENSE)
[![ggen Marketplace](https://img.shields.io/badge/ggen-marketplace-success)](https://github.com/seanchatmangpt/ggen)

Data modeling and schema generation CLI built with `clap-noun-verb` pattern. Design models with ontological validation and generate code for multiple targets.

## Features

- **Model Building**: Create data models with fields and constraints
- **Validation**: Validate data, consistency, and constraints
- **Multi-Target Generation**: JSON Schema, SQL DDL, GraphQL, OpenAPI, Rust
- **Ontological Validation**: Type hierarchies and constraint checking
- **JSON Output**: Structured results for all commands

## Installation

### From ggen Marketplace
```bash
ggen market install schema-forge-cli
ggen template generate schema-forge-cli:cli.tmpl
cargo build --release
```

### From crates.io
```bash
cargo install schema-forge-cli
```

## Quick Start

```bash
# Create a model
sf-cli model create User --description "User model"

# Add fields
sf-cli model add-field User email string --constraints '{"format": "email"}'
sf-cli model add-field User age integer --constraints '{"minimum": 0, "maximum": 150}'
sf-cli model add-field User active boolean

# Show model
sf-cli model show User

# Validate data
sf-cli validate data users.json User

# Generate JSON Schema
sf-cli generate json-schema User --output user.schema.json

# Generate SQL DDL
sf-cli generate sql User --output schema.sql --dialect postgresql

# Generate GraphQL SDL
sf-cli generate graphql User --output schema.graphql

# Generate Rust structs
sf-cli generate rust User --output models.rs
```

## Command Reference

### Model Noun
```bash
# Create model
sf-cli model create <NAME> [--description <DESC>]

# List models
sf-cli model list

# Show model
sf-cli model show <NAME>

# Add field
sf-cli model add-field <MODEL> <FIELD> <TYPE> [--constraints <JSON>]

# Remove field
sf-cli model remove-field <MODEL> <FIELD>
```

### Validate Noun
```bash
# Validate data file
sf-cli validate data <FILE> <MODEL>

# Check model consistency
sf-cli validate consistency

# Validate constraints
sf-cli validate constraints <MODEL>
```

### Generate Noun
```bash
# JSON Schema
sf-cli generate json-schema <MODEL> --output <FILE>

# SQL DDL
sf-cli generate sql <MODEL> --output <FILE> [--dialect postgresql]

# GraphQL SDL
sf-cli generate graphql <MODEL> --output <FILE>

# OpenAPI
sf-cli generate openapi <MODEL> --output <FILE>

# Rust structs
sf-cli generate rust <MODEL> --output <FILE>
```

## Examples

### E-Commerce Models
```bash
# Product model
sf-cli model create Product --description "Product catalog"
sf-cli model add-field Product sku string --constraints '{"pattern": "^[A-Z]{3}-\\d{6}$"}'
sf-cli model add-field Product name string --constraints '{"minLength": 1, "maxLength": 200}'
sf-cli model add-field Product price number --constraints '{"minimum": 0.01}'
sf-cli model add-field Product inStock boolean

# Order model
sf-cli model create Order
sf-cli model add-field Order orderId string
sf-cli model add-field Order customerId string
sf-cli model add-field Order items array --constraints '{"minItems": 1}'
sf-cli model add-field Order total number --constraints '{"minimum": 0}'

# Generate all targets
sf-cli generate json-schema Product --output product.schema.json
sf-cli generate sql Product --output product.sql
sf-cli generate graphql Product --output product.graphql
sf-cli generate rust Product --output product.rs
```

### Validation Example
```json
// data.json
{
  "sku": "ABC-123456",
  "name": "Widget",
  "price": 19.99,
  "inStock": true
}
```

```bash
$ sf-cli validate data data.json Product
{
  "valid": true,
  "errors": []
}

# Invalid data
$ echo '{"sku": "invalid", "price": -5}' | sf-cli validate data - Product
{
  "valid": false,
  "errors": [
    "sku: pattern mismatch",
    "price: must be >= 0.01",
    "name: required field missing"
  ]
}
```

### Generated JSON Schema
```bash
$ sf-cli generate json-schema Product --output product.schema.json
```

```json
{
  "type": "object",
  "properties": {
    "sku": {
      "type": "string",
      "pattern": "^[A-Z]{3}-\\d{6}$"
    },
    "name": {
      "type": "string",
      "minLength": 1,
      "maxLength": 200
    },
    "price": {
      "type": "number",
      "minimum": 0.01
    },
    "inStock": {
      "type": "boolean"
    }
  },
  "required": ["sku", "name", "price"]
}
```

### Generated SQL
```bash
$ sf-cli generate sql Product --output product.sql --dialect postgresql
```

```sql
CREATE TABLE products (
    sku VARCHAR(11) NOT NULL CHECK (sku ~ '^[A-Z]{3}-\d{6}$'),
    name VARCHAR(200) NOT NULL CHECK (LENGTH(name) >= 1),
    price DECIMAL(10,2) NOT NULL CHECK (price >= 0.01),
    in_stock BOOLEAN,
    PRIMARY KEY (sku)
);
```

### Generated GraphQL
```bash
$ sf-cli generate graphql Product --output product.graphql
```

```graphql
type Product {
  sku: String!
  name: String!
  price: Float!
  inStock: Boolean
}

input ProductInput {
  sku: String!
  name: String!
  price: Float!
  inStock: Boolean
}

type Query {
  product(sku: String!): Product
  products: [Product!]!
}

type Mutation {
  createProduct(input: ProductInput!): Product
  updateProduct(sku: String!, input: ProductInput!): Product
  deleteProduct(sku: String!): Boolean
}
```

### Generated Rust
```bash
$ sf-cli generate rust Product --output product.rs
```

```rust
use serde::{Deserialize, Serialize};

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Product {
    pub sku: String,
    pub name: String,
    pub price: f64,
    pub in_stock: Option<bool>,
}

impl Product {
    pub fn validate(&self) -> Result<(), Vec<String>> {
        let mut errors = Vec::new();

        if !self.sku.chars().all(|c| c.is_ascii_alphanumeric() || c == '-') {
            errors.push("sku: invalid format".to_string());
        }

        if self.name.is_empty() || self.name.len() > 200 {
            errors.push("name: length must be 1-200".to_string());
        }

        if self.price < 0.01 {
            errors.push("price: must be >= 0.01".to_string());
        }

        if errors.is_empty() {
            Ok(())
        } else {
            Err(errors)
        }
    }
}
```

## ggen Integration

### Generate from Ontology
```bash
ggen graph load marketplace/packages/schema-forge-cli/rdf/ontology.ttl

ggen graph query --sparql "
  SELECT ?model ?field WHERE {
    ?field a <http://ggen.io/ontology/clap-noun-verb#Field> .
  }
"

ggen template generate schema-forge-cli:cli.tmpl
```

### Extend with Custom Generators
Edit `rdf/ontology.ttl`:
```turtle
sf:typescript a clap:Verb ;
    rdfs:label "typescript" ;
    clap:belongsToNoun sf:Generate ;
    clap:hasArgument sf:modelArg, sf:outputArg ;
    clap:returns sf:GenerateResult .
```

Regenerate:
```bash
ggen template generate schema-forge-cli:cli.tmpl
# New command: sf-cli generate typescript Product --output models.ts
```

## Supported Types

- **string**: Text data
- **integer**: Whole numbers
- **number**: Decimals
- **boolean**: true/false
- **array**: Lists
- **object**: Nested structures

## Constraints

- **minLength, maxLength**: String length
- **minimum, maximum**: Numeric ranges
- **pattern**: Regex validation
- **format**: email, uri, date, etc.
- **minItems, maxItems**: Array size
- **required**: Required fields
- **unique**: Uniqueness constraints

## License

MIT OR Apache-2.0
