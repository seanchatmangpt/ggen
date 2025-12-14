<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->
**Table of Contents**

- [Polyglot Code Generation Guide](#polyglot-code-generation-guide)
  - [Language Support](#language-support)
  - [Framework Support](#framework-support)
    - [Rust Frameworks](#rust-frameworks)
    - [TypeScript Frameworks](#typescript-frameworks)
    - [Python Frameworks](#python-frameworks)
  - [Type Mapping (xsd:* → Language Types)](#type-mapping-xsd-%E2%86%92-language-types)
    - [Type Mapping Implementation](#type-mapping-implementation)
  - [Zero-Drift Architecture](#zero-drift-architecture)
    - [Zero-Drift Validation](#zero-drift-validation)
  - [Rust Code Generation Example](#rust-code-generation-example)
  - [TypeScript Code Generation Example](#typescript-code-generation-example)
  - [Python Code Generation Example](#python-code-generation-example)
  - [Generating All Languages](#generating-all-languages)
  - [Validation Testing](#validation-testing)
  - [Critical Rules](#critical-rules)
  - [Example: End-to-End Generation](#example-end-to-end-generation)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

# Polyglot Code Generation Guide

## Language Support

ggen generates code in **Rust, TypeScript, and Python** from a single RDF ontology source:

- **Rust**: Type-safe, compiled, zero-cost abstractions
- **TypeScript**: Type-checked JavaScript for Node.js and browsers
- **Python**: Dynamic typing, rapid development, ML integration

## Framework Support

### Rust Frameworks

```
axum       - Async web framework with tower middleware
actix      - Actor-based async framework
rocket     - Developer-friendly web framework
warp       - Composable web framework
```

### TypeScript Frameworks

```
express    - Minimal, unopinionated Node.js HTTP framework
fastify    - High-performance Node.js framework
nestjs     - Full-featured, dependency injection framework
nextjs     - Full-stack React framework with API routes
```

### Python Frameworks

```
fastapi    - Modern, async Python web framework
flask      - Lightweight Python web framework
django     - Full-featured batteries-included framework
starlette  - Lightweight ASGI framework
```

## Type Mapping (xsd:* → Language Types)

RDF defines data types with XSD namespace. ggen maps to native types:

```
RDF/XSD Type              Rust              TypeScript         Python
─────────────────────────────────────────────────────────────────────────
xsd:string                String            string             str
xsd:integer               i64               number             int
xsd:float                 f64               number             float
xsd:boolean               bool              boolean            bool
xsd:dateTime              DateTime<Utc>     Date               datetime
xsd:date                  NaiveDate         Date               date
xsd:uuid                  Uuid              string             UUID
xsd:decimal               Decimal           Decimal            Decimal

Ontology Example:
─────────────────
<User>
  <id> rdf:type xsd:uuid
  <name> rdf:type xsd:string
  <age> rdf:type xsd:integer
  <active> rdf:type xsd:boolean
  <created> rdf:type xsd:dateTime
```

### Type Mapping Implementation

```rust
pub fn xsd_to_rust_type(xsd_type: &str) -> String {
    match xsd_type {
        "string" => "String".to_string(),
        "integer" => "i64".to_string(),
        "float" => "f64".to_string(),
        "boolean" => "bool".to_string(),
        "dateTime" => "DateTime<Utc>".to_string(),
        "date" => "NaiveDate".to_string(),
        "uuid" => "Uuid".to_string(),
        "decimal" => "Decimal".to_string(),
        _ => "String".to_string(),  // Fallback
    }
}

pub fn xsd_to_typescript_type(xsd_type: &str) -> String {
    match xsd_type {
        "string" => "string".to_string(),
        "integer" => "number".to_string(),
        "float" => "number".to_string(),
        "boolean" => "boolean".to_string(),
        "dateTime" => "Date".to_string(),
        "date" => "Date".to_string(),
        "uuid" => "string".to_string(),
        "decimal" => "number".to_string(),
        _ => "string".to_string(),
    }
}

pub fn xsd_to_python_type(xsd_type: &str) -> String {
    match xsd_type {
        "string" => "str".to_string(),
        "integer" => "int".to_string(),
        "float" => "float".to_string(),
        "boolean" => "bool".to_string(),
        "dateTime" => "datetime".to_string(),
        "date" => "date".to_string(),
        "uuid" => "UUID".to_string(),
        "decimal" => "Decimal".to_string(),
        _ => "str".to_string(),
    }
}
```

## Zero-Drift Architecture

Single ontology, multiple language implementations:

```
domain.ttl (Single Source of Truth)
    ↓
Ontology Parser (RDF)
    ├─→ Rust Generator      → crate/src/models.rs
    ├─→ TypeScript Generator → src/types.ts
    └─→ Python Generator    → models.py

Result: Three language implementations that:
  ✓ Have identical structure
  ✓ Share same semantics
  ✓ Stay in sync automatically
  ✓ No manual sync-up needed
```

### Zero-Drift Validation

```rust
pub fn validate_cross_language_consistency(
    rust_module: &RustModule,
    typescript_module: &TypeScriptModule,
    python_module: &PythonModule,
) -> Result<()> {
    // All three must have same entity count
    if rust_module.entities.len()
        != typescript_module.entities.len()
        || typescript_module.entities.len() != python_module.entities.len() {
        return Err("Entity count mismatch across languages".into());
    }

    // For each entity, verify fields match
    for entity_name in &rust_module.entities {
        let rust_fields: HashSet<_> =
            rust_module.get_entity(entity_name)?.fields.iter()
                .map(|f| f.name.clone()).collect();

        let ts_fields: HashSet<_> =
            typescript_module.get_entity(entity_name)?.fields.iter()
                .map(|f| f.name.clone()).collect();

        let py_fields: HashSet<_> =
            python_module.get_entity(entity_name)?.fields.iter()
                .map(|f| f.name.clone()).collect();

        if rust_fields != ts_fields || ts_fields != py_fields {
            return Err(format!(
                "Field mismatch for entity {}: Rust={:?}, TS={:?}, Py={:?}",
                entity_name, rust_fields, ts_fields, py_fields
            ));
        }
    }

    Ok(())
}
```

## Rust Code Generation Example

```rust
// Template: rust-api/src/entities/{{ entity_name }}.rs.tmpl
use serde::{Deserialize, Serialize};
use uuid::Uuid;
use chrono::{DateTime, Utc};

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct {{ entity_name | capitalize }} {
    {% for property in entity.properties %}
    pub {{ property.name }}: {{ property.rust_type }},
    {% endfor %}
}

impl {{ entity_name | capitalize }} {
    pub fn new({% for property in required_properties %}{{ property.name }}: {{ property.rust_type }}{% if not loop.last %}, {% endif %}{% endfor %}) -> Self {
        Self {
            {% for property in entity.properties %}
            {% if property.required %}
            {{ property.name }},
            {% else %}
            {{ property.name }}: Default::default(),
            {% endif %}
            {% endfor %}
        }
    }
}

// Generated output (for User entity):
/*
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct User {
    pub id: Uuid,
    pub name: String,
    pub email: String,
    pub age: i64,
    pub created_at: DateTime<Utc>,
}

impl User {
    pub fn new(id: Uuid, name: String, email: String) -> Self {
        Self {
            id,
            name,
            email,
            age: 0,
            created_at: Utc::now(),
        }
    }
}
*/
```

## TypeScript Code Generation Example

```typescript
// Template: typescript-models/src/entities/{{ entity_name }}.ts.tmpl
import { v4 as uuidv4 } from 'uuid';

export interface {{ entity_name | capitalize }} {
    {% for property in entity.properties %}
    {{ property.name }}: {{ property.typescript_type }};
    {% endfor %}
}

export class {{ entity_name | capitalize }}Impl implements {{ entity_name | capitalize }} {
    {% for property in entity.properties %}
    {{ property.name }}: {{ property.typescript_type }};
    {% endfor %}

    constructor({% for property in required_properties %}{{ property.name }}: {{ property.typescript_type }}{% if not loop.last %}, {% endif %}{% endfor %}) {
        {% for property in required_properties %}
        this.{{ property.name }} = {{ property.name }};
        {% endfor %}
        {% for property in optional_properties %}
        this.{{ property.name }} = undefined;
        {% endfor %}
    }
}

// Generated output (for User entity):
/*
export interface User {
    id: string;
    name: string;
    email: string;
    age: number;
    created_at: Date;
}

export class UserImpl implements User {
    id: string;
    name: string;
    email: string;
    age: number;
    created_at: Date;

    constructor(id: string, name: string, email: string) {
        this.id = id;
        this.name = name;
        this.email = email;
        this.age = undefined;
        this.created_at = undefined;
    }
}
*/
```

## Python Code Generation Example

```python
# Template: python-models/{{ entity_name | lower }}.py.tmpl
from pydantic import BaseModel, Field
from typing import Optional
from uuid import UUID
from datetime import datetime

class {{ entity_name | capitalize }}(BaseModel):
    {% for property in entity.properties %}
    {{ property.name }}: {{ property.python_type }}{% if property.required %}{% else %} = None{% endif %}
    {% endfor %}

    class Config:
        frozen = True

# Generated output (for User entity):
"""
from pydantic import BaseModel
from uuid import UUID
from datetime import datetime

class User(BaseModel):
    id: UUID
    name: str
    email: str
    age: int = None
    created_at: datetime = None

    class Config:
        frozen = True
"""
```

## Generating All Languages

```bash
# Generate Rust models
ggen template generate-rdf \
    --ontology domain.ttl \
    --template rust-api \
    --output ./rust-api/src/models

# Generate TypeScript models
ggen template generate-rdf \
    --ontology domain.ttl \
    --template typescript-models \
    --output ./typescript-api/src/types

# Generate Python models
ggen template generate-rdf \
    --ontology domain.ttl \
    --template python-models \
    --output ./python-api/models

# Verify zero-drift
ggen validate consistency \
    --ontology domain.ttl \
    --artifacts ./rust-api,./typescript-api,./python-api
```

## Validation Testing

Test generated code in all languages:

```rust
#[test]
fn test_zero_drift_consistency() {
    // Load ontology
    let ontology = load_ontology("domain.ttl").unwrap();

    // Generate in all languages
    let rust_code = generate_rust(&ontology).unwrap();
    let ts_code = generate_typescript(&ontology).unwrap();
    let py_code = generate_python(&ontology).unwrap();

    // Verify all have same structure
    assert_eq!(
        extract_entity_names(&rust_code),
        extract_entity_names(&ts_code)
    );
    assert_eq!(
        extract_entity_names(&ts_code),
        extract_entity_names(&py_code)
    );
}

#[test]
fn test_type_mapping_correctness() {
    let mappings = vec![
        ("xsd:string", "String", "string", "str"),
        ("xsd:integer", "i64", "number", "int"),
        ("xsd:boolean", "bool", "boolean", "bool"),
        ("xsd:dateTime", "DateTime<Utc>", "Date", "datetime"),
    ];

    for (xsd, rust_ty, ts_ty, py_ty) in mappings {
        assert_eq!(xsd_to_rust_type(xsd), rust_ty);
        assert_eq!(xsd_to_typescript_type(xsd), ts_ty);
        assert_eq!(xsd_to_python_type(xsd), py_ty);
    }
}
```

## Critical Rules

1. **MAINTAIN single source of truth** - One ontology, N implementations
2. **TEST in all languages** - Consistency across Rust, TS, Python
3. **USE idiomatic code** - Not direct translations
4. **VALIDATE type mappings** - Ensure correctness across languages
5. **DOCUMENT differences** - When semantic differences are required
6. **SYNC versions** - All implementations same ontology version
7. **TEST cross-language interop** - If systems communicate

---

## Example: End-to-End Generation

Ontology:
```ttl
@prefix : <http://example.com/api/> .

:User a :Entity ;
    :property [ :name "id" ; :type xsd:uuid ; :required true ] ;
    :property [ :name "name" ; :type xsd:string ; :required true ] ;
    :property [ :name "email" ; :type xsd:string ; :required true ] ;
    :property [ :name "age" ; :type xsd:integer ] ;
    :property [ :name "active" ; :type xsd:boolean ] .
```

Generated Rust:
```rust
pub struct User {
    pub id: Uuid,
    pub name: String,
    pub email: String,
    pub age: Option<i64>,
    pub active: Option<bool>,
}
```

Generated TypeScript:
```typescript
export interface User {
    id: string;
    name: string;
    email: string;
    age?: number;
    active?: boolean;
}
```

Generated Python:
```python
class User(BaseModel):
    id: UUID
    name: str
    email: str
    age: Optional[int] = None
    active: Optional[bool] = None
```

All three are **identical in structure** and **automatically in sync**!
