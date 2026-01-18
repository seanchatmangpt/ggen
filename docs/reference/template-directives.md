# Template Directives Reference

Complete reference for template metadata directives and frontmatter.

## Frontmatter Structure

Templates use YAML frontmatter to declare metadata:

```yaml
---
output: src/models.rs
vars:
  - class_name
  - output_dir
when: production
query:
  - name: extract-classes
    file: queries/extract-classes.rq
determinism:
  seed: 42
---
```

## Core Directives

### `output` (Required)

Specifies the output file path. Supports variables and expressions.

**Syntax:**
```yaml
output: <file_path>
```

**Examples:**

Simple path:
```yaml
output: src/models.rs
```

Variable interpolation:
```yaml
output: src/{{ class_name }}/models.rs
```

Conditional path:
```yaml
output: {% if lang == "rust" %}src/models.rs{% else %}src/models.ts{% endif %}
```

Dynamic directory:
```yaml
output: {{ output_dir }}/generated.rs
```

### `vars` (Optional)

Lists template variables that must be provided at generation time.

**Syntax:**
```yaml
vars:
  - variable_name
  - another_var
```

**Example:**
```yaml
vars:
  - class_name
  - package_name
  - author_name
```

**Usage:**
When generating code:
```bash
ggen template generate-rdf \
  --ontology domain.ttl \
  --template my-template \
  --var class_name=User \
  --var package_name=myapp
```

In template:
```tera
package {{ package_name }};

public class {{ class_name }} {
  // ...
}
```

### `when` (Optional)

Conditionally applies template based on environment or context.

**Syntax:**
```yaml
when: <condition>
```

**Built-in conditions:**
- `development` - Only apply in dev environments
- `production` - Only apply in prod environments
- `testing` - Only apply in test environments

**Example:**
```yaml
when: production
# This template only generates in production environment
```

Multiple conditions:
```yaml
when: development || testing
# Applies in dev or testing, not production
```

Custom conditions via context:
```yaml
when: {{ generate_migrations }}
# Apply if the context variable generate_migrations is truthy
```

### `query` (Optional)

Declares SPARQL queries that populate template variables.

**Syntax:**
```yaml
query:
  - name: <variable_name>
    file: <path_to_sparql>
```

**Example:**
```yaml
query:
  - name: classes
    file: queries/extract-classes.rq
  - name: properties
    file: queries/extract-properties.rq
```

**In template:**
```tera
{% for class in classes %}
pub struct {{ class.name }} {
  {% for prop in properties %}
    pub {{ prop.name }}: {{ prop.type }},
  {% endfor %}
}
{% endfor %}
```

**Inline queries:**
```yaml
query:
  - name: classes
    inline: |
      SELECT ?name WHERE {
        ?class a rdfs:Class ;
        rdfs:label ?name .
      }
```

**Query parameters:**
```yaml
query:
  - name: user_class
    file: queries/find-class.rq
    params:
      class_name: "User"
```

Query file with parameters:
```sparql
{# queries/find-class.rq #}
SELECT ?class WHERE {
  ?class a rdfs:Class ;
         rdfs:label "{{ class_name }}" .
}
```

### `determinism` (Optional)

Ensures byte-identical output for reproducible builds.

**Syntax:**
```yaml
determinism:
  seed: <number>
  order: <field_name>
```

**Example:**
```yaml
determinism:
  seed: 42
  order: name
```

**Ensures:**
- Randomization uses consistent seed
- Results sorted by specified field
- Byte-identical output across runs

### `rdf` (Optional)

Explicitly lists RDF files to load for this template.

**Syntax:**
```yaml
rdf:
  - domain.ttl
  - additional.rdf
```

**Example:**
```yaml
rdf:
  - domain.ttl
  - types.ttl
  - constraints.ttl
```

### `sparql` (Optional)

Declares SPARQL query files used in template.

**Syntax:**
```yaml
sparql:
  - queries/extract-classes.rq
  - queries/extract-properties.rq
```

Used for validation and documentation.

## Advanced Directives

### `validate` (Optional)

Adds validation rules for generated output.

**Syntax:**
```yaml
validate:
  - rule: <validation_rule>
    message: <error_message>
```

**Example:**
```yaml
validate:
  - rule: "file_size < 1000000"
    message: "Generated file too large"
  - rule: "has_content"
    message: "Template generated empty file"
```

### `postprocess` (Optional)

Applies transformations after template rendering.

**Syntax:**
```yaml
postprocess:
  - command: <shell_command>
```

**Example:**
```yaml
postprocess:
  - command: "rustfmt --edition 2021"
  - command: "eslint --fix"
```

### `metadata` (Optional)

Custom metadata for marketplace or documentation.

**Syntax:**
```yaml
metadata:
  author: <author_name>
  version: <version>
  description: <description>
  tags:
    - tag1
    - tag2
```

**Example:**
```yaml
metadata:
  author: Sean Chatman
  version: 1.0.0
  description: "Rust model generation from RDF ontologies"
  tags:
    - rust
    - models
    - rdf
```

## Complete Example

```yaml
---
output: src/models/{{ class_name }}.rs
vars:
  - class_name
  - visibility
  - features

when: development || production

query:
  - name: classes
    file: queries/find-class.rq
    params:
      class_name: "{{ class_name }}"
  - name: properties
    file: queries/extract-properties.rq

rdf:
  - domain.ttl
  - constraints.ttl

sparql:
  - queries/find-class.rq
  - queries/extract-properties.rq

determinism:
  seed: 42
  order: name

validate:
  - rule: "file_size < 100000"
    message: "Generated file too large"

postprocess:
  - command: "rustfmt --edition 2021"

metadata:
  author: Development Team
  version: 2.0.0
  description: "Rust struct generation from RDF classes"
  tags:
    - rust
    - models
    - structs
---

// Template content starts after ---
{% for class in classes %}
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct {{ class.name }} {
    {% for prop in properties %}
    pub {{ prop.name }}: {{ prop.type }},
    {% endfor %}
}
{% endfor %}
```

## Template File Organization

**Structure:**
```
templates/
└── my-template/
    ├── template.md        # Template documentation
    ├── models.rs.tmpl     # Template with frontmatter
    ├── api.rs.tmpl        # Another template in set
    └── queries/
        ├── extract-classes.rq
        └── extract-properties.rq
```

## Context Variables

Templates have access to these built-in variables:

| Variable | Type | Description |
|----------|------|-------------|
| `template_name` | string | Name of template being rendered |
| `timestamp` | datetime | Current timestamp |
| `seed` | number | Determinism seed |
| `environment` | string | Current environment (dev/prod/test) |

**Example:**
```tera
// Generated by {{ template_name }} at {{ timestamp }}
// Environment: {{ environment }}
```

## Common Patterns

### Multi-File Generation

Generate multiple files from single ontology:

```yaml
---
output: src/models/{{ class_name }}.rs
when: production
query:
  - name: classes
    file: queries/extract-classes.rq
---
{# template content #}
```

Then create another template file for another output.

### Conditional Features

Apply features based on context:

```yaml
---
vars:
  - enable_serde
query:
  - name: classes
    file: queries/extract-classes.rq
---
{% if enable_serde %}
use serde::{Serialize, Deserialize};

#[derive(Serialize, Deserialize)]
{% endif %}
pub struct {{ class.name }} {
  // ...
}
```

### Parameterized Output Paths

Generate different files based on classification:

```yaml
---
output: {% if class.entity_type == "value_object" %}
  src/domain/values/{{ class.name }}.rs
{% else %}
  src/domain/entities/{{ class.name }}.rs
{% endif %}
---
```

## Troubleshooting

**Q: Template not finding SPARQL file**
A: Ensure `query.file` is relative to template directory. Use `queries/filename.rq`.

**Q: Variables not being interpolated in output path**
A: Variables in `output` must be declared in `vars`. Example:
```yaml
vars:
  - class_name
output: src/{{ class_name }}.rs
```

**Q: `when` condition not working**
A: Use `development`, `production`, or `testing` (lowercase). Custom conditions must be provided in context.

**Q: Determinism seed not taking effect**
A: Ensure SPARQL queries use `ORDER BY` for consistent sorting.

## See Also

- [Template Reference](templates.md) - Tera syntax and template language
- [RDF/SPARQL Reference](rdf-sparql.md) - SPARQL query syntax
- [How to Create Templates](../how-to-guides/create-templates.md) - Step-by-step guide
