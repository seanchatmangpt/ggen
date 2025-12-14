<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->
**Table of Contents**

- [File Formats Reference](#file-formats-reference)
  - [RDF Ontology Format (.ttl)](#rdf-ontology-format-ttl)
    - [Basic Structure](#basic-structure)
    - [Triple Format](#triple-format)
    - [Common Predicates](#common-predicates)
    - [Data Types (xsd)](#data-types-xsd)
    - [Example Complete Ontology](#example-complete-ontology)
  - [ggen.toml Configuration](#ggentoml-configuration)
    - [Minimal Configuration](#minimal-configuration)
    - [Full Configuration](#full-configuration)
  - [Lockfile Format (ggen.lock)](#lockfile-format-ggenlock)
  - [Template Format (.liquid)](#template-format-liquid)
    - [Basic Template](#basic-template)
    - [Template Functions](#template-functions)
  - [Generated Output Formats](#generated-output-formats)
    - [TypeScript Output](#typescript-output)
    - [Python Output](#python-output)
    - [Rust Output](#rust-output)
  - [JSON Schema Export](#json-schema-export)
  - [Serialization Formats](#serialization-formats)
    - [JSON Serialization](#json-serialization)
    - [YAML Serialization](#yaml-serialization)
    - [XML Serialization](#xml-serialization)
  - [Environment Variables](#environment-variables)
  - [File Size Guidelines](#file-size-guidelines)
  - [Summary](#summary)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

# File Formats Reference

Complete specification of ggen configuration and ontology file formats.

## RDF Ontology Format (.ttl)

**Format**: Turtle RDF (W3C standard)

**File extension**: `.ttl`

### Basic Structure

```turtle
# Prefixes (namespace definitions)
@prefix ex: <http://example.org/> .
@prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .
@prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .
@prefix xsd: <http://www.w3.org/2001/XMLSchema#> .

# Class definition
ex:User a rdfs:Class ;
  rdfs:label "User" ;
  rdfs:comment "A user account" .

# Property definition
ex:email a rdf:Property ;
  rdfs:domain ex:User ;
  rdfs:range xsd:string ;
  rdfs:label "Email" ;
  rdfs:comment "User email address" .
```

### Triple Format

Every statement is a **triple**: subject-predicate-object

```
subject predicate object .
```

Examples:
```turtle
ex:User rdf:type rdfs:Class .       # ex:User is a Class
ex:email rdfs:domain ex:User .      # email domain is User
```

### Common Predicates

| Predicate | Meaning | Example |
|-----------|---------|---------|
| `rdf:type` | "is a" | `ex:User rdf:type rdfs:Class` |
| `rdfs:label` | Display name | `ex:User rdfs:label "User"` |
| `rdfs:comment` | Description | `ex:User rdfs:comment "A user"` |
| `rdfs:domain` | Applies to classes | `ex:email rdfs:domain ex:User` |
| `rdfs:range` | Value type | `ex:email rdfs:range xsd:string` |
| `rdfs:subClassOf` | Inheritance | `ex:Admin rdfs:subClassOf ex:User` |

### Data Types (xsd)

```turtle
ex:age rdfs:range xsd:integer .         # Integer
ex:weight rdfs:range xsd:decimal .      # Decimal number
ex:active rdfs:range xsd:boolean .      # Boolean
ex:name rdfs:range xsd:string .         # String text
ex:createdAt rdfs:range xsd:dateTime .  # Timestamp
ex:birthDate rdfs:range xsd:date .      # Date only
```

### Example Complete Ontology

```turtle
@prefix shop: <http://example.org/ecommerce/> .
@prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .
@prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .
@prefix xsd: <http://www.w3.org/2001/XMLSchema#> .

# Classes
shop:Product a rdfs:Class ;
  rdfs:label "Product" ;
  rdfs:comment "An e-commerce product" .

shop:Category a rdfs:Class ;
  rdfs:label "Category" ;
  rdfs:comment "Product category" .

# Properties
shop:id a rdf:Property ;
  rdfs:domain shop:Product, shop:Category ;
  rdfs:range xsd:string ;
  rdfs:label "ID" .

shop:name a rdf:Property ;
  rdfs:domain shop:Product, shop:Category ;
  rdfs:range xsd:string ;
  rdfs:label "Name" .

shop:price a rdf:Property ;
  rdfs:domain shop:Product ;
  rdfs:range xsd:decimal ;
  rdfs:label "Price" .

shop:inStock a rdf:Property ;
  rdfs:domain shop:Product ;
  rdfs:range xsd:boolean ;
  rdfs:label "In Stock" ;
  rdfs:comment "Whether product is available" .

shop:category a rdf:Property ;
  rdfs:domain shop:Product ;
  rdfs:range shop:Category ;
  rdfs:label "Category" ;
  rdfs:comment "Product's category" .
```

## ggen.toml Configuration

**Format**: TOML (Tom's Obvious Minimal Language)

**File location**: Project root

### Minimal Configuration

```toml
[project]
name = "my-api"
version = "0.1.0"
description = "My API domain models"

[generation]
ontology = "domain.ttl"
languages = ["typescript", "python", "rust"]

[output]
typescript = "src/models/index.ts"
python = "models.py"
rust = "src/models.rs"
```

### Full Configuration

```toml
[project]
name = "ecommerce-api"
version = "1.0.0"
description = "E-commerce domain models"
author = "Your Name <email@example.com>"
license = "MIT"

[generation]
ontology = "schema.ttl"
languages = ["typescript", "python", "rust"]

# Namespace for generated code
namespace = "ecommerce"

# Template configuration
template = "custom-api-template"
template_vars = { include_validation = true, include_docs = true }

[output]
typescript = "src/generated/models.ts"
python = "app/models.py"
rust = "src/models.rs"

[validation]
# Fail on missing documentation
require_documentation = true

# Fail on circular dependencies
allow_circular = false

# Check for untyped properties
require_types = true

[publication]
# For marketplace packages
registry = "https://marketplace.example.org"
keywords = ["api", "ecommerce", "models"]
```

## Lockfile Format (ggen.lock)

**Format**: TOML

**Purpose**: Record exact versions for reproducibility

```toml
[metadata]
version = "1"
generated = "2024-01-15T10:30:00Z"
ggen_version = "0.1.0"

[ontology]
path = "schema.ttl"
hash = "sha256:abc123..."
last_modified = "2024-01-15T10:20:00Z"

[templates]
"custom-api-template" = { version = "1.2.3", hash = "sha256:def456..." }

[outputs]
"typescript" = { file = "src/models.ts", hash = "sha256:ghi789..." }
"python" = { file = "models.py", hash = "sha256:jkl012..." }
"rust" = { file = "src/models.rs", hash = "sha256:mno345..." }
```

## Template Format (.liquid)

**Format**: Liquid template syntax

**File extension**: `.liquid` or `.tmpl`

### Basic Template

```liquid
// Generated {{ class | capitalize }} model

{% for class in classes %}
export interface {{ class.name | capitalize }} {
  {% for property in class.properties %}
  {{ property.name }}: {{ property.type }};  // {{ property.comment }}
  {% endfor %}
}
{% endfor %}
```

### Template Functions

```liquid
{# Capitalize first letter #}
{{ "user" | capitalize }}  -> "User"

{# Lowercase #}
{{ "User" | downcase }}  -> "user"

{# Replace characters #}
{{ "user_name" | replace: "_", "-" }}  -> "user-name"

{# Size/length #}
{{ class.properties | size }}  -> number of properties

{# Filter by condition #}
{% assign string_props = class.properties | where: "type", "string" %}

{# Join array #}
{{ properties | map: "name" | join: ", " }}
```

## Generated Output Formats

### TypeScript Output

```typescript
export interface User {
  id: string;
  email: string;
  active: boolean;
  createdAt: Date;
}
```

### Python Output

```python
from dataclasses import dataclass
from datetime import datetime

@dataclass
class User:
    id: str
    email: str
    active: bool
    created_at: datetime
```

### Rust Output

```rust
#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct User {
    pub id: String,
    pub email: String,
    pub active: bool,
    pub created_at: DateTime<Utc>,
}
```

## JSON Schema Export

**Format**: JSON Schema (Draft 7)

```json
{
  "$schema": "http://json-schema.org/draft-07/schema#",
  "title": "User",
  "type": "object",
  "properties": {
    "id": { "type": "string" },
    "email": { "type": "string", "format": "email" },
    "active": { "type": "boolean" },
    "createdAt": { "type": "string", "format": "date-time" }
  },
  "required": ["id", "email", "active", "createdAt"]
}
```

## Serialization Formats

### JSON Serialization

```json
{
  "id": "user-123",
  "email": "alice@example.com",
  "active": true,
  "createdAt": "2024-01-15T10:30:00Z"
}
```

### YAML Serialization

```yaml
id: user-123
email: alice@example.com
active: true
createdAt: 2024-01-15T10:30:00Z
```

### XML Serialization

```xml
<User>
  <id>user-123</id>
  <email>alice@example.com</email>
  <active>true</active>
  <createdAt>2024-01-15T10:30:00Z</createdAt>
</User>
```

## Environment Variables

Configuration via environment:

```bash
# Ontology file
export GGEN_ONTOLOGY="schema.ttl"

# Output languages
export GGEN_LANGUAGES="typescript,python,rust"

# Template path
export GGEN_TEMPLATE="custom-template/"

# Output directory
export GGEN_OUTPUT="generated/"

# Namespace
export GGEN_NAMESPACE="myapp"

# Verbose output
export GGEN_VERBOSE="true"

# Debug mode
export GGEN_DEBUG="true"
```

## File Size Guidelines

| File Type | Max Size | Recommended | Notes |
|-----------|----------|-------------|-------|
| `.ttl` | 10MB | < 500KB | Ontology files |
| `.liquid` | 1MB | < 100KB | Template files |
| `ggen.toml` | 100KB | < 10KB | Config file |
| Generated code | 10MB | < 1MB | Output per language |

## Summary

You now understand:
- ✅ RDF Turtle format for ontologies
- ✅ ggen.toml configuration
- ✅ Lockfile format
- ✅ Liquid template syntax
- ✅ Generated code formats
- ✅ Serialization options
