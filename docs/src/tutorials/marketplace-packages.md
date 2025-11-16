<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->
**Table of Contents**

- [Creating Marketplace Packages](#creating-marketplace-packages)
  - [Part 1: Understanding Packages](#part-1-understanding-packages)
    - [What is a Package?](#what-is-a-package)
    - [Package Structure](#package-structure)
  - [Part 2: Building a Package](#part-2-building-a-package)
    - [Step 1: Initialize Package](#step-1-initialize-package)
    - [Step 2: Create `ggen.toml` Manifest](#step-2-create-ggentoml-manifest)
    - [Step 3: Create Manifest README](#step-3-create-manifest-readme)
  - [Example](#example)
  - [Installation](#installation)
  - [Configuration](#configuration)
  - [Requirements](#requirements)
  - [License](#license)
    - [Step 5: Create SPARQL Queries](#step-5-create-sparql-queries)
  - [Part 3: Testing Your Package](#part-3-testing-your-package)
    - [Step 6: Create Golden File Tests](#step-6-create-golden-file-tests)
    - [Step 7: Run Tests](#step-7-run-tests)
  - [Part 4: Documentation & Examples](#part-4-documentation--examples)
    - [Step 8: Create Examples](#step-8-create-examples)
  - [Generation](#generation)
  - [Usage](#usage)
    - [Step 10: Publish to Marketplace](#step-10-publish-to-marketplace)
    - [Step 11: Share Your Package](#step-11-share-your-package)
  - [Part 6: Maintenance](#part-6-maintenance)
    - [Version Updates](#version-updates)
    - [Getting Help](#getting-help)
  - [Best Practices Checklist](#best-practices-checklist)
  - [Common Issues](#common-issues)
  - [Next Steps](#next-steps)
  - [Resources](#resources)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

# Creating Marketplace Packages

**Goal:** Build and publish a reusable template package to the ggen marketplace.

**What you'll learn:** How to structure, test, and publish packages that others can discover and install.

**Time:** 60 minutes | **Difficulty:** Intermediate

---

## Part 1: Understanding Packages

### What is a Package?

A package is a reusable unit of code generation containing:
1. **Templates** - Tera files that generate code
2. **Manifest** - `ggen.toml` describing the package
3. **Dependencies** - Required libraries/tools
4. **Metadata** - Version, author, license
5. **Tests** - Golden file tests proving correctness
6. **Documentation** - README and examples

### Package Structure

```
my-template-package/
├── ggen.toml                      # Package manifest (required)
├── README.md                      # Usage documentation
├── LICENSE                        # License file
├── templates/
│   └── rust-models.tmpl          # Template files
├── queries/
│   ├── extract-classes.rq        # SPARQL queries
│   └── extract-properties.rq
├── tests/
│   ├── fixtures/
│   │   └── sample-ontology.ttl   # Test input
│   └── golden/
│       └── expected-output.rs    # Expected output
└── examples/
    └── usage.md                  # Usage examples
```

---

## Part 2: Building a Package

### Step 1: Initialize Package

```bash
# Create package directory
mkdir rust-models-package
cd rust-models-package

# Initialize git (optional but recommended)
git init
```

### Step 2: Create `ggen.toml` Manifest

```toml
[package]
# Basic information
name = "rust-models"
version = "1.0.0"
authors = ["Your Name <you@example.com>"]
description = "Generate Rust model structs from RDF ontologies with serde support"
license = "MIT"
repository = "https://github.com/you/rust-models"
documentation = "https://docs.rs/rust-models"

# Package configuration
[package.metadata]
language = "rust"
framework = "serde"
category = ["code-generation", "rust", "serialization"]
tags = ["models", "structs", "serde", "schema"]

# Template files mapping
[[package.templates]]
name = "rust-models"
path = "templates/rust-models.tmpl"
language = "rust"
framework = "serde"
description = "Generate Rust structs with serde support"
minimum_ontology_size = 1
maximum_ontology_size = 10000

# Dependencies this package requires
[package.dependencies]
serde = "1.0"
serde_json = "1.0"
uuid = { version = "1.0", features = ["v4", "serde"] }
tokio = { version = "1.0", optional = true }

# Optional features
[package.features]
async = ["tokio"]
full = ["async"]

# Runtime requirements
[package.runtime]
minimum_rust_version = "1.70.0"
ontology_prefixes = ["rdfs", "rdf"]  # Required prefixes in ontology
```

### Step 3: Create Manifest README

```markdown
# Rust Models Template

Generate Rust model structs from RDF ontologies with automatic serde integration.

## Features
- Automatic struct generation from rdfs:Class
- Property extraction from rdf:Property
- Serde serialization support
- Type mapping from XSD to Rust types
- Configurable derive attributes

## Usage

```bash
ggen template generate-rdf \
  --ontology domain.ttl \
  --template rust-models \
  --output src/
```

## Example

Given this ontology:

```turtle
@prefix ex: <http://example.com/> .
@prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .

ex:User a rdfs:Class ;
    rdfs:label "User" .

ex:name a rdf:Property ;
    rdfs:domain ex:User ;
    rdfs:range xsd:string ;
    rdfs:label "name" .

ex:email a rdf:Property ;
    rdfs:domain ex:User ;
    rdfs:range xsd:string ;
    rdfs:label "email" .
```

Generates:

```rust
use serde::{Deserialize, Serialize};

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct User {
    pub name: String,
    pub email: String,
}
```

## Installation

```bash
ggen marketplace install rust-models
```

## Configuration

Via frontmatter in template:

```yaml
---
to: "src/models.rs"
vars:
  derive:
    default: ["Debug", "Clone", "Serialize", "Deserialize"]
---
```

## Requirements
- Rust 1.70+
- Ontology with rdfs:Class and rdf:Property

## License
MIT
```

### Step 4: Create Templates

Create `templates/rust-models.tmpl`:

```tera
---
to: "src/models.rs"
vars:
  derive:
    type: "array"
    default: ["Debug", "Clone", "Serialize", "Deserialize"]
    description: "Derive macro attributes"

  visibility:
    type: "string"
    default: "pub"
    description: "Struct visibility"

rdf:
  - domain.ttl

sparql:
  prefixes:
    ex: "http://example.com/"
    xsd: "http://www.w3.org/2001/XMLSchema#"

frozen:
  - "// MANUAL: "

validation:
  required_prefixes: ["rdfs", "rdf"]
  min_classes: 1
---
{% query "queries/extract-classes.rq" as classes %}
{% query "queries/extract-properties.rq" as all_properties %}
use serde::{Deserialize, Serialize};

{% set type_map = {
    "xsd:string": "String",
    "xsd:integer": "i64",
    "xsd:int": "i32",
    "xsd:decimal": "f64",
    "xsd:float": "f32",
    "xsd:boolean": "bool",
    "xsd:dateTime": "chrono::DateTime<chrono::Utc>",
} %}

{% for class in classes %}
/// Auto-generated from RDF ontology
#[derive({{ derive | join(", ") }})]
{{ visibility }} struct {{ class.name }} {
    {% for prop in class.properties %}
    pub {{ prop.name }}: {{ type_map.get(prop.type, "String") }},
    {% endfor %}
}

// MANUAL: Add custom implementation below
impl {{ class.name }} {
    // Add your methods here
}
// MANUAL END

{% endfor %}
```

### Step 5: Create SPARQL Queries

Create `queries/extract-classes.rq`:

```sparql
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>

SELECT ?className (GROUP_CONCAT(?propName; separator="|") as ?properties)
       (GROUP_CONCAT(?propType; separator="|") as ?types)
WHERE {
    ?class a rdfs:Class ;
           rdfs:label ?className .

    OPTIONAL {
        ?prop rdfs:domain ?class ;
              rdfs:label ?propName ;
              rdfs:range ?propType .
    }
}
GROUP BY ?className
ORDER BY ?className
```

Create `queries/extract-properties.rq`:

```sparql
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>

SELECT ?className ?propName ?propType
WHERE {
    ?class a rdfs:Class ;
           rdfs:label ?className .

    ?prop rdfs:domain ?class ;
          rdfs:label ?propName ;
          rdfs:range ?propType .
}
ORDER BY ?className ?propName
```

---

## Part 3: Testing Your Package

### Step 6: Create Golden File Tests

Create `tests/fixtures/sample-ontology.ttl`:

```turtle
@prefix ex: <http://example.com/> .
@prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .
@prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .
@prefix xsd: <http://www.w3.org/2001/XMLSchema#> .

ex:User a rdfs:Class ;
    rdfs:label "User" .

ex:name a rdf:Property ;
    rdfs:domain ex:User ;
    rdfs:range xsd:string ;
    rdfs:label "name" .

ex:email a rdf:Property ;
    rdfs:domain ex:User ;
    rdfs:range xsd:string ;
    rdfs:label "email" .

ex:Post a rdfs:Class ;
    rdfs:label "Post" .

ex:title a rdf:Property ;
    rdfs:domain ex:Post ;
    rdfs:range xsd:string ;
    rdfs:label "title" .

ex:content a rdf:Property ;
    rdfs:domain ex:Post ;
    rdfs:range xsd:string ;
    rdfs:label "content" .
```

Create `tests/golden/expected-output.rs`:

```rust
use serde::{Deserialize, Serialize};

/// Auto-generated from RDF ontology
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct User {
    pub name: String,
    pub email: String,
}

// MANUAL: Add custom implementation below
impl User {
    // Add your methods here
}
// MANUAL END

/// Auto-generated from RDF ontology
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Post {
    pub title: String,
    pub content: String,
}

// MANUAL: Add custom implementation below
impl Post {
    // Add your methods here
}
// MANUAL END
```

### Step 7: Run Tests

```bash
# Test template rendering
ggen template test \
  --template . \
  --ontology tests/fixtures/sample-ontology.ttl \
  --golden tests/golden/expected-output.rs

# Update golden files if template changed intentionally
ggen template test \
  --template . \
  --ontology tests/fixtures/sample-ontology.ttl \
  --update-golden
```

---

## Part 4: Documentation & Examples

### Step 8: Create Examples

Create `examples/simple-blog.md`:

```markdown
# Simple Blog Example

Generate models for a blog system.

## Ontology (blog.ttl)

```turtle
@prefix ex: <http://example.com/blog/> .
@prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .
@prefix xsd: <http://www.w3.org/2001/XMLSchema#> .

ex:User a rdfs:Class ;
    rdfs:label "User" .

ex:username a rdf:Property ;
    rdfs:domain ex:User ;
    rdfs:range xsd:string ;
    rdfs:label "username" .

ex:Post a rdfs:Class ;
    rdfs:label "Post" .

ex:title a rdf:Property ;
    rdfs:domain ex:Post ;
    rdfs:range xsd:string ;
    rdfs:label "title" .
```

## Generation

```bash
ggen template generate-rdf \
  --ontology blog.ttl \
  --template rust-models \
  --output src/
```

## Usage

```rust
use serde_json::json;

let user = User {
    username: "alice".to_string(),
};

println!("{}", serde_json::to_string_pretty(&user)?);
```
```

---

## Part 5: Publishing

### Step 9: Prepare for Publishing

1. **Create LICENSE file:**

```
MIT License

Copyright (c) 2024 Your Name

Permission is hereby granted, free of charge...
[Full MIT license text]
```

2. **Create .gitignore:**

```
/target/
Cargo.lock
.DS_Store
*.swp
```

3. **Commit to git:**

```bash
git add .
git commit -m "Initial release v1.0.0"
git tag v1.0.0
git push origin main --tags
```

### Step 10: Publish to Marketplace

```bash
# Build and package
ggen marketplace package \
  --path . \
  --output my-template.ggen

# Publish
ggen marketplace publish \
  --package my-template.ggen \
  --visibility public

# Verify
ggen marketplace search rust-models
```

### Step 11: Share Your Package

```bash
# Users can now install your package
ggen marketplace install rust-models

# Or pin a specific version
ggen marketplace install rust-models --version 1.0.0

# Use in projects
ggen template generate-rdf \
  --template rust-models \
  --ontology domain.ttl \
  --output src/
```

---

## Part 6: Maintenance

### Version Updates

When you make changes:

1. **Update version in ggen.toml:**

```toml
[package]
version = "1.1.0"  # Change from 1.0.0
```

2. **Update CHANGELOG:**

```markdown
## 1.1.0 (2024-01-15)

### Added
- Support for optional properties via SHACL

### Fixed
- Improved handling of missing rdf:range declarations

### Changed
- Better type mapping for complex types
```

3. **Commit and publish:**

```bash
git add .
git commit -m "Release v1.1.0"
git tag v1.1.0
git push origin main --tags
ggen marketplace publish --path .
```

### Getting Help

If your package isn't installing:

```bash
# Check package manifest
ggen marketplace validate --path .

# See package details
ggen marketplace info rust-models

# Check installation logs
ggen marketplace install rust-models --verbose
```

---

## Best Practices Checklist

- ✅ Clear, descriptive `ggen.toml`
- ✅ Comprehensive README with examples
- ✅ SPARQL queries that handle edge cases
- ✅ Golden file tests proving correctness
- ✅ Frozen sections for manual code
- ✅ Type mapping for target language
- ✅ Documentation of all variables
- ✅ License file (preferably MIT or Apache-2.0)
- ✅ Example ontologies in tests/fixtures
- ✅ Version tags in git
- ✅ Semantic versioning (MAJOR.MINOR.PATCH)
- ✅ CHANGELOG tracking changes

---

## Common Issues

**Q: My package fails to render certain ontologies**

A: Check your SPARQL queries handle OPTIONAL properties:
```sparql
OPTIONAL {
    ?prop rdfs:domain ?class ;
          rdfs:label ?propName .
}
```

**Q: Users report missing types in generated code**

A: Expand your type mapping in the template:
```tera
{% set type_map = {
    "xsd:string": "String",
    "xsd:integer": "i64",
    # Add more types as needed
} %}
```

**Q: Frozen sections aren't being preserved**

A: Ensure markers are consistent:
```tera
// MANUAL: Start of manual code
// ... custom implementation ...
// MANUAL END
```

**Q: Package won't publish**

A: Validate with:
```bash
ggen marketplace validate --path .
```

---

## Next Steps

1. **Publish your first package** - Share with community
2. **Get feedback** - Improve based on users
3. **Add more templates** - Grow your package
4. **Create variants** - TypeScript, Python versions
5. **Join ecosystem** - Help build ggen community

## Resources

- **Testing Guide:** [Testing Strategy](../how-to-guides/testing-strategy.md)
- **Template Creation:** [Create Templates](../how-to-guides/create-templates.md)
- **Marketplace:** `ggen marketplace --help`
- **Examples:** `examples/` directory in ggen repo
