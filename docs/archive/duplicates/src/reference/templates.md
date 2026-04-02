<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->
**Table of Contents**

- [Template Reference](#template-reference)
  - [Template Structure](#template-structure)
    - [Basic Template](#basic-template)
  - [Template Syntax](#template-syntax)
    - [Variables](#variables)
    - [Control Flow](#control-flow)
    - [SPARQL Queries](#sparql-queries)
  - [Template Metadata (Frontmatter)](#template-metadata-frontmatter)
    - [Frontmatter Keys](#frontmatter-keys)
  - [Built-in Functions](#built-in-functions)
    - [String Functions](#string-functions)
    - [Type Conversion](#type-conversion)
    - [Formatting](#formatting)
  - [SPARQL Query Results](#sparql-query-results)
    - [Result Structure](#result-structure)
  - [Type Mapping](#type-mapping)
  - [Template Includes](#template-includes)
  - [Filters](#filters)
    - [Custom Filters](#custom-filters)
  - [Error Handling](#error-handling)
  - [Best Practices](#best-practices)
  - [See Also](#see-also)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

# Template Reference

Complete reference for ggen template syntax, structure, and features.

## Template Structure

Templates use Tera syntax with SPARQL query integration.

### Basic Template

```tera
{% query "queries/extract-classes.rq" as classes %}
{% for class in classes %}
pub struct {{ class.name }} {
    {% for prop in class.properties %}
    pub {{ prop.name }}: {{ prop.type }},
    {% endfor %}
}
{% endfor %}
```

## Template Syntax

### Variables

```tera
{{ variable_name }}
{{ object.property }}
{{ array[0] }}
```

### Control Flow

**If/Else:**
```tera
{% if condition %}
  {{ content }}
{% else %}
  {{ alternative }}
{% endif %}
```

**Loops:**
```tera
{% for item in items %}
  {{ item.name }}
{% endfor %}
```

**Loop Variables:**
```tera
{% for item in items %}
  {{ loop.index }}      {# 1, 2, 3... #}
  {{ loop.index0 }}     {# 0, 1, 2... #}
  {{ loop.first }}      {# true on first iteration #}
  {{ loop.last }}       {# true on last iteration #}
{% endfor %}
```

### SPARQL Queries

**Query from file:**
```tera
{% query "queries/extract-classes.rq" as results %}
```

**Inline query:**
```tera
{% query "SELECT ?s ?p ?o WHERE { ?s ?p ?o }" as results %}
```

**Query with parameters:**
```tera
{% query "queries/find-class.rq" with {"class_name": "User"} as results %}
```

## Template Metadata (Frontmatter)

Templates can include YAML frontmatter:

```yaml
---
to: src/models.rs
vars:
  - class_name
  - output_dir
rdf:
  - domain.ttl
sparql:
  - queries/extract-classes.rq
determinism:
  seed: 42
---
```

### Frontmatter Keys

- `to:` - Output file path
- `vars:` - Required template variables
- `rdf:` - RDF files to load
- `sparql:` - SPARQL query files
- `determinism:` - Determinism settings

## Built-in Functions

### String Functions

```tera
{{ name | upper }}
{{ name | lower }}
{{ name | title }}
{{ name | trim }}
```

### Type Conversion

```tera
{{ value | int }}
{{ value | float }}
{{ value | string }}
```

### Formatting

```tera
{{ value | format(format="0.2f") }}
{{ date | date(format="%Y-%m-%d") }}
```

## SPARQL Query Results

SPARQL queries return result sets accessible in templates:

```tera
{% query "queries/extract-classes.rq" as classes %}
{% for class in classes %}
  Class: {{ class.name }}
  Properties: {{ class.properties | length }}
{% endfor %}
```

### Result Structure

Results are arrays of objects with properties matching SPARQL SELECT variables:

```sparql
SELECT ?name ?type
WHERE { ... }
```

Results:
```json
[
  {"name": "User", "type": "rdfs:Class"},
  {"name": "Product", "type": "rdfs:Class"}
]
```

## Type Mapping

Map RDF types to target language types:

```tera
{% set type_map = {
    "xsd:string": "String",
    "xsd:integer": "i32",
    "xsd:decimal": "f64",
    "xsd:boolean": "bool",
    "xsd:dateTime": "DateTime<Utc>"
} %}

{{ type_map.get(property.type, "String") }}
```

## Template Includes

Include other templates:

```tera
{% include "macros/common.tera" %}
```

## Filters

### Custom Filters

```tera
{{ value | rust_type }}
{{ value | camel_case }}
{{ value | snake_case }}
```

## Error Handling

Templates validate syntax and SPARQL queries:

```bash
# Lint template
ggen template lint templates/rust-models/models.rs.tmpl
```

## Best Practices

1. **Use SPARQL for extraction:** Don't hardcode structure
2. **Parameterize templates:** Use variables for customization
3. **Validate inputs:** Check ontology structure
4. **Document queries:** Add comments to SPARQL files
5. **Test thoroughly:** Use multiple ontologies

## See Also

- [Create Templates Guide](../how-to-guides/create-templates.md)
- [RDF/SPARQL Reference](rdf-sparql.md)
- [Getting Started Tutorial](../tutorials/getting-started.md)

