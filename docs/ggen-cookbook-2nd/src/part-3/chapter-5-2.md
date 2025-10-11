<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->
**Table of Contents**

- [Frontmatter & Metadata](#frontmatter--metadata)
  - [YAML Structure](#yaml-structure)
  - [Required vs Optional Fields](#required-vs-optional-fields)
    - [Required Fields](#required-fields)
    - [Optional Fields](#optional-fields)
  - [Variable Sources](#variable-sources)
    - [1. Static Variables (`vars`)](#1-static-variables-vars)
    - [2. SPARQL Variables (`sparql.vars`)](#2-sparql-variables-sparqlvars)
    - [3. Built-in Variables](#3-built-in-variables)
    - [4. Environment Variables](#4-environment-variables)
  - [Data Source Configuration](#data-source-configuration)
    - [RDF Sources (`rdf`)](#rdf-sources-rdf)
    - [Validation Sources (`shape`)](#validation-sources-shape)
  - [Validation Rules](#validation-rules)
    - [Schema Validation](#schema-validation)
    - [Runtime Validation](#runtime-validation)
  - [Output Specification](#output-specification)
    - [Single File Output](#single-file-output)
    - [Multiple File Output](#multiple-file-output)
    - [Dynamic Paths](#dynamic-paths)
    - [Path Helpers](#path-helpers)
  - [Determinism Controls](#determinism-controls)
    - [Sorting Control](#sorting-control)
    - [Seed Management](#seed-management)
  - [Advanced Configuration](#advanced-configuration)
    - [Conditional Fields](#conditional-fields)
    - [Environment-Specific Configuration](#environment-specific-configuration)
  - [Best Practices](#best-practices)
    - [Organization](#organization)
    - [Performance](#performance)
    - [Maintainability](#maintainability)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

# Frontmatter & Metadata

Frontmatter is the YAML metadata section at the beginning of every `.tmpl` file. It configures the template's behavior, specifies data sources, defines variables, and controls output generation.

## YAML Structure

Frontmatter is enclosed between `---` markers and follows standard YAML syntax:

```yaml
---
# Required fields
to: "output/{{ name }}.rs"
vars:
  author: "GGen Team"

# Optional fields
rdf:
  - "data/ontology.ttl"
shape:
  - "validation/rules.ttl"
sparql:
  vars:
    - name: title
      query: "SELECT ?title WHERE { ?s <urn:ex#title> ?title } LIMIT 1"
determinism:
  sort: [name, id]
  seed: "my-project-2025"
---
```

## Required vs Optional Fields

### Required Fields

Every template must specify:

- **`to`**: Output file path (supports variable interpolation)
- **`vars`**: Template variables (can be empty object `{}`)

### Optional Fields

These enhance template functionality:

- **`rdf`**: RDF data files to load
- **`shape`**: SHACL validation files
- **`sparql`**: SPARQL queries for data extraction
- **`determinism`**: Controls for reproducible generation

## Variable Sources

Variables available in templates come from multiple sources:

### 1. Static Variables (`vars`)

Defined directly in frontmatter:

```yaml
vars:
  project_name: "My API"
  version: "1.0.0"
  author: "Development Team"
  base_url: "https://api.example.com"
```

### 2. SPARQL Variables (`sparql.vars`)

Extracted from RDF data using SPARQL queries:

```yaml
sparql:
  vars:
    - name: entity_name
      query: "SELECT ?name WHERE { <urn:ex#entity> <urn:ex#name> ?name }"
      default: "Unknown Entity"
    - name: description
      query: "SELECT ?desc WHERE { <urn:ex#entity> <urn:ex#description> ?desc }"
```

### 3. Built-in Variables

Automatically available in all templates:

- `{{ file }}` - Current template filename
- `{{ dir }}` - Template directory path
- `{{ timestamp }}` - Generation timestamp (ISO format)
- `{{ seed }}` - Determinism seed value

### 4. Environment Variables

System environment variables accessible as `{{ env.VARIABLE_NAME }}`:

```yaml
vars:
  api_key: "{{ env.OPENAI_API_KEY }}"
  database_url: "{{ env.DATABASE_URL }}"
```

## Data Source Configuration

### RDF Sources (`rdf`)

Specify RDF files containing semantic data:

```yaml
rdf:
  - "ontology/core.ttl"              # Turtle ontology
  - "data/instances.jsonld"          # JSON-LD instances
  - "validation/schemas.ttl"         # Validation schemas
  - "https://example.com/data.ttl"   # Remote RDF source
```

**Supported formats:**
- `.ttl` - Turtle
- `.nt` - N-Triples
- `.jsonld` - JSON-LD
- `.rdf` - RDF/XML
- HTTP(S) URLs for remote sources

### Validation Sources (`shape`)

SHACL shape files for data validation:

```yaml
shape:
  - "validation/person-shapes.ttl"
  - "validation/company-shapes.ttl"
  - "validation/api-shapes.ttl"
```

## Validation Rules

Frontmatter validation ensures template correctness:

### Schema Validation

All frontmatter is validated against a JSON Schema:

```json
{
  "$schema": "http://json-schema.org/draft-07/schema#",
  "type": "object",
  "properties": {
    "to": { "type": "string" },
    "vars": { "type": "object" },
    "rdf": { "type": "array", "items": { "type": "string" } },
    "shape": { "type": "array", "items": { "type": "string" } }
  },
  "required": ["to", "vars"]
}
```

### Runtime Validation

- **Path validation**: Ensure `to` field contains valid path syntax
- **Query validation**: Verify SPARQL syntax in `sparql` fields
- **File existence**: Check that referenced RDF/shape files exist
- **Variable consistency**: Ensure referenced variables are defined

## Output Specification

The `to` field defines where and how files are generated:

### Single File Output

```yaml
to: "src/{{ module }}/mod.rs"
```

### Multiple File Output

```yaml
to:
  - "src/{{ name }}.rs"
  - "tests/{{ name }}_test.rs"
  - "docs/{{ name }}.md"
```

### Dynamic Paths

Use variable interpolation for dynamic paths:

```yaml
to: "{{ language }}/{{ framework }}/src/main.{{ extension }}"
```

### Path Helpers

Built-in functions for path manipulation:

```yaml
to: "{{ join 'src' module 'mod.rs' }}"    # Join path segments
to: "{{ dirname file }}/{{ name }}.rs"    # Use template directory
```

## Determinism Controls

Ensure reproducible generation across environments:

### Sorting Control

Specify sort order for matrix queries:

```yaml
determinism:
  sort: [name, id]           # Sort by name, then id
  sort: "priority"           # Sort by single field
```

### Seed Management

Control randomness for deterministic output:

```yaml
determinism:
  seed: "{{ project_name }}-{{ version }}"    # Use variables as seed
  seed: "fixed-seed-2025-01-01"              # Fixed seed for reproducibility
```

## Advanced Configuration

### Conditional Fields

Use variables for conditional configuration:

```yaml
rdf:
  {{#if use_validation}}
  - "validation/shapes.ttl"
  {{/if}}
  - "data/core.ttl"
```

### Environment-Specific Configuration

Different settings for different environments:

```yaml
{{#if env.CI}}
determinism:
  seed: "ci-build-{{ env.BUILD_NUMBER }}"
{{else}}
determinism:
  seed: "{{ project_name }}-dev"
{{/if}}
```

## Best Practices

### Organization

1. **Group related variables** logically in `vars` section
2. **Use descriptive names** for all variables and fields
3. **Document complex queries** with comments
4. **Keep RDF sources minimal** - only include necessary data

### Performance

1. **Cache expensive queries** using static variables when possible
2. **Use specific file paths** instead of wildcard patterns
3. **Validate early** - catch configuration errors before generation
4. **Test with sample data** - verify queries work with real RDF data

### Maintainability

1. **Version your frontmatter** - track changes in template metadata
2. **Use consistent naming** across related templates
3. **Document assumptions** - explain why specific data sources are needed
4. **Test edge cases** - ensure templates work with missing or malformed data

Frontmatter is the control center of your template - invest time in getting it right, and your templates will be more maintainable, reliable, and reusable.
