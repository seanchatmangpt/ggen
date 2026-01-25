<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->
**Table of Contents**

- [7. TEMPLATE RENDERING **](#7-template-rendering-)
  - [Context](#context)
  - [Connections](#connections)
  - [Implementation](#implementation)
    - [Tera Syntax](#tera-syntax)
    - [Variable Access](#variable-access)
    - [Built-in Filters](#built-in-filters)
    - [Custom Filters](#custom-filters)
    - [Control Flow](#control-flow)
    - [Default Values](#default-values)
    - [Whitespace Control](#whitespace-control)
  - [Template Organization](#template-organization)
    - [File-Based Templates](#file-based-templates)
    - [Inline Templates](#inline-templates)
    - [Template Includes](#template-includes)
    - [Macros](#macros)
  - [The All-Results Pattern](#the-all-results-pattern)
  - [Error Handling](#error-handling)
  - [The Deeper Pattern](#the-deeper-pattern)
  - [Template Best Practices](#template-best-practices)
  - [When This Pattern Breaks](#when-this-pattern-breaks)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

# 7. TEMPLATE RENDERING **

*A template is a shape waiting to be filled.*

---

## Context

**[GENERATION RULES](06-generation-rules.md)** extract data from the graph. But data is not code. You need a way to transform variable bindings into actual source files—with proper syntax, formatting, and structure.

You could embed code generation in Rust, but that would mean:
- Changing templates requires recompiling ggen
- Templates are hidden in application code
- Non-programmers cannot modify output formats

You need a templating language that is:
- Expressive enough for code generation
- Separate from the application
- Accessible to domain experts

---

❖ ❖ ❖

**When code generation is embedded in the tool, the tool controls the output. When code generation is in templates, the user controls the output.**

The forces:
- Templates must express code structure (conditionals, loops)
- Templates must transform data (filters, formatting)
- Templates must be readable by non-experts
- Templates must produce valid code reliably

Embedding generation in code leads to:
- Opaque output (what will be generated?)
- Rigid formats (can't customize without modifying ggen)
- Maintenance burden (template changes require releases)

**Therefore:**

**Use a template language (Tera) that supports control flow, filters, and composition. Store templates as separate files. Make templates the authority on output format.**

Templates should:
- Live in the project alongside ontologies
- Use Jinja2-style syntax for familiarity
- Support filters for data transformation
- Support macros for reuse
- Produce output that matches the project's style

---

❖ ❖ ❖

## Connections

This pattern is invoked by **[GENERATION RULES](06-generation-rules.md)** for each result row.

- **[MANIFEST AS TRUTH](02-manifest-as-truth.md)** declares template sources
- **[DETERMINISTIC OUTPUT](13-deterministic-output.md)** requires templates to be pure
- **[ERROR SIGNALS](12-error-signals.md)** reports template failures (exit code 4)

---

## Implementation

### Tera Syntax

ggen uses [Tera](https://tera.netlify.app/), a Rust template engine with Jinja2-style syntax:

```jinja2
{# This is a comment #}

{# Variables #}
pub struct {{ name | pascal_case }} {

{# Conditionals #}
{% if is_required == "true" %}
    pub {{ field_name }}: {{ field_type }},
{% else %}
    pub {{ field_name }}: Option<{{ field_type }}>,
{% endif %}

{# Loops #}
{% for field in fields %}
    pub {{ field.name }}: {{ field.type }},
{% endfor %}

}
```

### Variable Access

Variables come from SPARQL query results:

```sparql
SELECT ?name ?description ?version
WHERE { ... }
```

In template:

```jinja2
/// {{ description }}
/// Version: {{ version }}
pub struct {{ name }} { }
```

### Built-in Filters

Tera provides filters for data transformation:

| Filter | Input | Output |
|--------|-------|--------|
| `upper` | `hello` | `HELLO` |
| `lower` | `Hello` | `hello` |
| `capitalize` | `hello` | `Hello` |
| `title` | `hello world` | `Hello World` |
| `trim` | ` hello ` | `hello` |
| `length` | `[1,2,3]` | `3` |
| `first` | `[1,2,3]` | `1` |
| `last` | `[1,2,3]` | `3` |
| `join(sep)` | `["a","b"]` | `a, b` |

### Custom Filters

ggen adds code-generation specific filters:

| Filter | Input | Output |
|--------|-------|--------|
| `snake_case` | `UserProfile` | `user_profile` |
| `pascal_case` | `user_profile` | `UserProfile` |
| `camel_case` | `user_profile` | `userProfile` |
| `screaming_snake` | `userProfile` | `USER_PROFILE` |
| `kebab_case` | `UserProfile` | `user-profile` |

Usage:

```jinja2
pub mod {{ name | snake_case }};  {# user_profile #}
pub struct {{ name | pascal_case }};  {# UserProfile #}
const {{ name | screaming_snake }}: &str = "...";  {# USER_PROFILE #}
```

### Control Flow

**Conditionals:**

```jinja2
{% if is_public == "true" %}
pub struct {{ name }} {
{% else %}
struct {{ name }} {
{% endif %}
```

**Loops:**

```jinja2
{% for field in fields %}
    pub {{ field.name }}: {{ field.type }},
{% endfor %}
```

**Loop Variables:**

```jinja2
{% for field in fields %}
    {% if loop.first %}// First field{% endif %}
    pub {{ field.name }}: {{ field.type }},
    {% if loop.last %}// Last field{% endif %}
{% endfor %}
```

| Variable | Meaning |
|----------|---------|
| `loop.index` | 1-based index |
| `loop.index0` | 0-based index |
| `loop.first` | True if first iteration |
| `loop.last` | True if last iteration |

### Default Values

Handle missing variables gracefully:

```jinja2
{{ description | default(value="No description") }}
```

### Whitespace Control

Control whitespace around tags:

```jinja2
{%- if condition -%}   {# Trim whitespace on both sides #}
    content
{%- endif -%}
```

---

## Template Organization

### File-Based Templates

For complex templates, use separate files:

```
templates/
├── struct.tera          # Main struct template
├── enum.tera            # Enum template
├── field.tera           # Reusable field template
└── includes/
    ├── header.tera      # Common header
    └── derives.tera     # Derive macros
```

Reference in manifest:

```toml
template = { file = "templates/struct.tera" }
```

### Inline Templates

For simple cases, embed in manifest:

```toml
template = { inline = "pub mod {{ name | snake_case }};" }
```

### Template Includes

Break large templates into parts:

```jinja2
{# struct.tera #}
{% include "includes/header.tera" %}

{% include "includes/derives.tera" %}
pub struct {{ name | pascal_case }} {
{% for field in fields %}
    {% include "field.tera" %}
{% endfor %}
}
```

### Macros

Define reusable template fragments:

```jinja2
{% macro field_declaration(name, type, required) %}
{% if required %}
    pub {{ name }}: {{ type }},
{% else %}
    pub {{ name }}: Option<{{ type }}>,
{% endif %}
{% endmacro %}

{# Usage #}
{{ self::field_declaration(name="id", type="String", required=true) }}
```

---

## The All-Results Pattern

For templates that need the full query results:

```jinja2
{# All entities are available as sparql_results #}
{% for entity in sparql_results %}
pub mod {{ entity.name | snake_case }};
{% endfor %}
```

This is useful for:
- Module files that list all generated items
- Index files that aggregate information
- Summary reports

---

## Error Handling

Template errors produce clear messages:

```
error[E0004]: Template render error in rule 'structs':
    Variable 'naem' not found in context
    Available variables: ["name", "type", "fields"]
```

Common template errors:

| Error | Cause |
|-------|-------|
| Variable not found | Typo in variable name or missing in query |
| Filter not found | Unknown filter name |
| Syntax error | Malformed template syntax |
| Include not found | Missing include file |

---

## The Deeper Pattern

TEMPLATE RENDERING is about **separating content from presentation**.

The SPARQL query defines *what* data exists.
The template defines *how* that data appears as code.

This separation enables:
- Different templates for different languages (Rust, TypeScript, Python)
- Different templates for different styles (verbose, terse)
- Template reuse across projects
- Template evolution without ontology changes

The template is a **contract**: given these variables, produce this code. As long as the query provides the variables, the template does its job.

---

## Template Best Practices

**1. Name variables clearly:**

```jinja2
{# Good #}
pub {{ field_name }}: {{ field_type }}

{# Avoid #}
pub {{ n }}: {{ t }}
```

**2. Use filters for transformations:**

```jinja2
{# Good #}
pub struct {{ name | pascal_case }}

{# Avoid - doing transformation in query #}
```

**3. Handle edge cases:**

```jinja2
{% if fields | length > 0 %}
{% for field in fields %}...{% endfor %}
{% else %}
    // No fields defined
{% endif %}
```

**4. Comment non-obvious logic:**

```jinja2
{# Required fields come first per style guide #}
{% for field in required_fields %}...{% endfor %}
{% for field in optional_fields %}...{% endfor %}
```

---

## When This Pattern Breaks

TEMPLATE RENDERING struggles when:

- Output requires complex logic (templates become spaghetti)
- Output depends on external state (templates are pure)
- Multiple passes are needed (templates render once)

ggen addresses these partially:

- Macros and includes reduce template complexity
- Inference rules can pre-compute complex values
- Multiple rules can generate intermediate artifacts

For very complex output, consider:
- Post-processing generated files
- Breaking into simpler generation rules
- Moving logic to inference (derive before render)

The pattern remains: templates transform data into text, purely and declaratively.
