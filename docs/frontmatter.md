<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->
**Table of Contents**

- [Frontmatter Schema (v1)](#frontmatter-schema-v1)
  - [Schema Overview](#schema-overview)
  - [Field Reference](#field-reference)
    - [`to` (Required)](#to-required)
    - [`vars` (Optional)](#vars-optional)
    - [`rdf` (Optional)](#rdf-optional)
    - [`shape` (Optional)](#shape-optional)
    - [`sparql` (Optional)](#sparql-optional)
      - [`vars` Subfield](#vars-subfield)
      - [`matrix` Subfield](#matrix-subfield)
    - [`determinism` (Optional)](#determinism-optional)
      - [`sort` Subfield](#sort-subfield)
      - [`seed` Subfield](#seed-subfield)
  - [Complete Example](#complete-example)
  - [Validation](#validation)
  - [Best Practices](#best-practices)
    - [File Organization](#file-organization)
    - [Query Design](#query-design)
    - [Template Variables](#template-variables)
    - [Determinism](#determinism)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

# Frontmatter Schema (v1)

Template frontmatter defines metadata and configuration for ggen projections. It specifies data sources, variable bindings, validation rules, and determinism controls for reproducible code generation.

## Schema Overview

Frontmatter is defined in YAML format at the beginning of template files:

```yaml
---
to: "output/{{ name }}.rs"
vars:
  seed: "my-project"
rdf:
  - "data/ontology.ttl"
  - "data/instances.jsonld"
shape:
  - "validation/shapes.ttl"
sparql:
  vars:
    - name: title
      query: "SELECT ?title WHERE { ?s <urn:ex#title> ?title } LIMIT 1"
  matrix:
    query: "SELECT ?id ?name WHERE { ?s <urn:ex#id> ?id ; <urn:ex#name> ?name }"
    bind: { id: "?id", name: "?name" }
determinism:
  sort: [id, name]
  seed: "{{ seed }}"
---
```

## Field Reference

### `to` (Required)
**Type**: String
**Description**: Output file path template with variable interpolation

**Syntax**: `"path/to/{{ variable }}/file.{{ extension }}"`

**Examples**:
```yaml
to: "src/{{ module }}.rs"                    # Simple variable
to: "tests/{{ module }}/mod.rs"             # Nested directories
to: "{{ lang }}/{{ name }}.{{ ext }}"       # Multiple variables
```

### `vars` (Optional)
**Type**: Object
**Description**: Static variables available during template rendering

**Usage**:
```yaml
vars:
  author: "John Doe"
  version: "1.0.0"
  license: "MIT"
```

### `rdf` (Optional)
**Type**: Array of strings
**Description**: RDF data sources to load for this projection

**Supported formats**:
- `.ttl` - Turtle files
- `.nt` - N-Triples files
- `.jsonld` - JSON-LD files

**Examples**:
```yaml
rdf:
  - "ontology/core.ttl"           # Single file
  - "data/people.jsonld"          # JSON-LD data
  - "schemas/validation.ttl"      # Validation schemas
```

### `shape` (Optional)
**Type**: Array of strings
**Description**: SHACL shape files for data validation

**Usage**:
```yaml
shape:
  - "validation/person-shapes.ttl"
  - "validation/company-shapes.ttl"
```

### `sparql` (Optional)
**Type**: Object
**Description**: SPARQL queries for data extraction and transformation

#### `vars` Subfield
**Type**: Array of variable query objects
**Description**: Extract single values for template variables

**Structure**:
```yaml
sparql:
  vars:
    - name: variable_name        # Variable name in template
      query: "SPARQL query"      # Query returning single value
      default: "fallback_value"  # Optional default if query fails
```

**Example**:
```yaml
sparql:
  vars:
    - name: project_name
      query: "SELECT ?name WHERE { ?s <urn:ex#name> ?name } LIMIT 1"
    - name: author
      query: "SELECT ?author WHERE { ?s <urn:ex#author> ?author } LIMIT 1"
      default: "Unknown Author"
```

#### `matrix` Subfield
**Type**: Object
**Description**: Generate projection matrices for batch processing

**Required fields**:
- `query`: SPARQL SELECT query (must include ORDER BY)
- `bind`: Variable binding mapping

**Structure**:
```yaml
sparql:
  matrix:
    query: "SELECT ?col1 ?col2 WHERE { ... } ORDER BY ?col1"
    bind:
      template_var: "?sparql_var"
```

**Example**:
```yaml
sparql:
  matrix:
    query: |
      SELECT ?id ?name ?email
      WHERE {
        ?person <urn:ex#id> ?id ;
                <urn:ex#name> ?name ;
                <urn:ex#email> ?email .
      } ORDER BY ?id
    bind:
      id: "?id"
      name: "?name"
      email: "?email"
```

### `determinism` (Optional)
**Type**: Object
**Description**: Control deterministic processing and output ordering

#### `sort` Subfield
**Type**: String or Array of strings
**Description**: Sort matrix results by specified columns

**Examples**:
```yaml
determinism:
  sort: id                    # Single column
  sort: [name, id]           # Multiple columns
```

#### `seed` Subfield
**Type**: String
**Description**: Random seed for deterministic processing

**Usage**:
```yaml
determinism:
  seed: "{{ project_name }}"  # Use template variable as seed
  seed: "fixed-seed-123"     # Fixed seed for reproducible builds
```

## Complete Example

```yaml
---
# Output: src/api/v1/users.rs
to: "src/api/{{ api_version }}/users.rs"

# Static variables
vars:
  api_version: "v1"
  author: "Team API"
  generated_date: "2025-01-01"

# RDF data sources
rdf:
  - "ontology/api.ttl"
  - "data/user-endpoints.jsonld"
  - "validation/api-shapes.ttl"

# SHACL validation
shape:
  - "validation/user-shapes.ttl"
  - "validation/endpoint-shapes.ttl"

# SPARQL queries
sparql:
  # Variable extraction
  vars:
    - name: base_path
      query: "SELECT ?path WHERE { <urn:api#base> <urn:api#path> ?path } LIMIT 1"
      default: "/api"
    - name: port
      query: "SELECT ?port WHERE { <urn:api#config> <urn:api#port> ?port } LIMIT 1"
      default: "8080"

  # Matrix projection for user endpoints
  matrix:
    query: |
      SELECT ?endpoint ?method ?path ?handler
      WHERE {
        ?ep <urn:ex#endpoint> ?endpoint ;
            <urn:ex#method> ?method ;
            <urn:ex#path> ?path ;
            <urn:ex#handler> ?handler .
      } ORDER BY ?endpoint
    bind:
      endpoint: "?endpoint"
      method: "?method"
      path: "?path"
      handler: "?handler"

# Deterministic processing
determinism:
  sort: [endpoint, method]
  seed: "{{ api_version }}-{{ generated_date }}"
---
```

## Validation

Frontmatter is validated against JSON Schema:
- **Schema file**: `schema/frontmatter.schema.json`
- **Validation**: Automatic validation during template loading
- **Error reporting**: Detailed error messages with field context

## Best Practices

### File Organization
1. **Group related RDF files** by domain or purpose
2. **Separate shapes from data** for clarity
3. **Use consistent naming** across related files

### Query Design
1. **Always use ORDER BY** in matrix queries for determinism
2. **Use LIMIT 1** for single-value variable queries
3. **Include default values** for optional variables
4. **Test queries** against sample data before deployment

### Template Variables
1. **Use descriptive names** for template variables
2. **Document variable sources** in comments
3. **Handle missing variables** gracefully in templates
4. **Use consistent naming conventions** across templates

### Determinism
1. **Always specify seeds** for reproducible builds
2. **Sort matrix results** consistently
3. **Avoid random operations** in templates
4. **Test determinism** with identical inputs
