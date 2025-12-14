# Quickstart: Template Generation with RDF/SPARQL

**Branch**: `005-fix-rendering-engine`
**Date**: 2025-12-13

## Overview

After this fix, `ggen template generate` will automatically detect and process SPARQL queries in template frontmatter.

## Basic Usage

### 1. Create a Template with SPARQL

```yaml
# template.tmpl
---
to: "output.txt"
rdf: "data.ttl"
prefixes:
  ex: "http://example.org/"
sparql:
  people: |
    SELECT ?name WHERE {
      ?person a ex:Person ;
              ex:name ?name .
    }
---
# People Found

{% for person in sparql_results.people %}
- {{ person.name }}
{% endfor %}

Total: {{ sparql_results.people | length }}
```

### 2. Create RDF Data

```turtle
# data.ttl
@prefix ex: <http://example.org/> .

ex:alice a ex:Person ;
    ex:name "Alice" .

ex:bob a ex:Person ;
    ex:name "Bob" .
```

### 3. Generate Output

```bash
ggen template generate --template template.tmpl --output output.txt
```

### 4. Result

```markdown
# People Found

- Alice
- Bob

Total: 2
```

## Helper Functions

### sparql_first

Get the first value from a column:

```jinja2
{{ sparql_first(results=sparql_results.people, column="name") }}
```

### sparql_values

Get all values from a column as an array:

```jinja2
{% for name in sparql_values(results=sparql_results.people, column="name") %}
  - {{ name }}
{% endfor %}
```

### sparql_count

Count results:

```jinja2
Found {{ sparql_count(results=sparql_results.people) }} people
```

### sparql_empty

Check if results are empty:

```jinja2
{% if sparql_empty(results=sparql_results.people) %}
  No people found
{% endif %}
```

## Multi-File Output

Use `{# FILE: path #}` markers to generate multiple files:

```yaml
---
rdf: "cli.ttl"
sparql:
  commands: "SELECT ?name ?desc WHERE { ?cmd a :Command ; :name ?name }"
---
{# FILE: Cargo.toml #}
[package]
name = "my-cli"

{# FILE: src/main.rs #}
fn main() {
    {% for cmd in sparql_results.commands %}
    // Command: {{ cmd.name }}
    {% endfor %}
}
```

## Verification

After generation, check the output JSON for metrics:

```json
{
  "output_path": "/path/to/output",
  "bytes_written": 1234,
  "rdf_files_loaded": 1,
  "sparql_queries_executed": 2,
  "files_created": 3
}
```

Key metrics:
- `sparql_queries_executed > 0` confirms SPARQL ran
- `rdf_files_loaded > 0` confirms RDF was loaded
- `files_created` shows multi-file output count
