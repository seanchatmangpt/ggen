# Creating Custom Code Generation Templates

Learn how to build custom Liquid/Jinja2 templates that generate code tailored to your specific needs.

## Goal

Create a custom template that generates API documentation from your ontology, demonstrating template engine fundamentals.

## Prerequisites

- ggen installed
- Completed [Your First CLI Command](05-first-cli-command.md)
- Basic familiarity with Liquid/Jinja2 template syntax

## Understanding Templates

ggen uses **Liquid templates** (with Tera engine) to transform RDF data into code. Templates receive:
- **Classes**: Entity types from your ontology
- **Properties**: Fields and relationships
- **Metadata**: Documentation, types, constraints

## Step 1: Create a Template Project

Create a template directory structure:

```bash
mkdir -p my-api-docs-template/src
cd my-api-docs-template
```

## Step 2: Define Template Metadata

Create `template.toml`:

```toml
[template]
name = "api-docs-generator"
version = "0.1.0"
description = "Generates API documentation from ontologies"
author = "Your Name"

[inputs]
# What this template expects
format = "rdf"
required_properties = ["name", "description"]

[outputs]
# What this template produces
format = "markdown"
extension = ".md"

[variables]
# Template parameters
title = { type = "string", default = "API Reference" }
include_examples = { type = "boolean", default = true }
```

## Step 3: Create Your Template

Create `src/api-reference.liquid`:

```liquid
# {{ title }}

Generated API documentation from {{ ontology_name }} ontology.

{% for class in classes %}
## {{ class.name | capitalize }}

{{ class.documentation }}

### Properties

| Property | Type | Description |
|----------|------|-------------|
{% for property in class.properties %}
| {{ property.name }} | {{ property.range }} | {{ property.documentation }} |
{% endfor %}

### Example Usage

```json
{
  {% for property in class.properties limit: 3 %}
  "{{ property.name }}": "{{ property.example }}"{% unless forloop.last %},{% endunless %}
  {% endfor %}
}
```

{% endfor %}

---

*Documentation generated with ggen*
```

## Step 4: Add Template Filters

Create `src/filters.rs` for custom Liquid filters:

```rust
use liquid::Object;
use liquid::ValueView;

pub fn titlecase(v: &dyn ValueView, _args: &[Value]) -> liquid::Result<Value> {
    match v.as_str() {
        Some(s) => {
            let titlecased = s
                .split('_')
                .map(|word| {
                    let mut chars = word.chars();
                    match chars.next() {
                        None => String::new(),
                        Some(first) => {
                            first.to_uppercase().collect::<String>() + chars.as_str()
                        }
                    }
                })
                .collect::<Vec<_>>()
                .join(" ");
            Ok(Value::scalar(titlecased))
        }
        None => Ok(v.to_value()),
    }
}
```

## Step 5: Register Template

Create `Cargo.toml`:

```toml
[package]
name = "ggen-api-docs-template"
version = "0.1.0"
edition = "2021"

[dependencies]
ggen-core = "0.1"
liquid = "0.26"
serde = { version = "1.0", features = ["derive"] }
serde_json = "1.0"
```

## Step 6: Test Your Template

Test with your ontology from the first tutorial:

```bash
ggen template generate-rdf \
  --ontology ../project.ttl \
  --template . \
  --output api-docs.md
```

The generated `api-docs.md` should show:

```markdown
# API Reference

Generated API documentation from project ontology.

## Project

A project with tasks

### Properties

| Property | Type | Description |
|----------|------|-------------|
| name | string | Project name |
| description | string | Project description |
| hasTask | Task | Tasks in project |

### Example Usage

```json
{
  "name": "Example Project",
  "description": "An example",
  "hasTask": []
}
```
```

## Step 7: Iterate and Improve

Enhance your template with more features:

**Add conditional sections**:
```liquid
{% if include_examples %}
### Example Code

Here's how to use {{ class.name }}...
{% endif %}
```

**Add loops over relationships**:
```liquid
{% for relationship in class.relationships %}
- `{{ relationship.name }}` → `{{ relationship.target }}`
{% endfor %}
```

**Use filters for formatting**:
```liquid
{{ class.name | downcase | replace: "_", "-" }}
```

## Step 8: Publish Your Template

Package and share your template:

```bash
ggen template lint
ggen template publish \
  --template . \
  --name my-api-docs \
  --description "Generate API docs from ontologies"
```

## Template Best Practices

**1. Organize logically**: Separate classes, properties, examples into sections

**2. Use clear variable names**: Makes templates self-documenting

```liquid
{% assign total_properties = class.properties | size %}
You have {{ total_properties }} properties to document.
```

**3. Provide sensible defaults**: Handle missing data gracefully

```liquid
{{ property.documentation | default: "No description provided" }}
```

**4. Comment complex logic**:
```liquid
{# Generate examples only for the first 3 properties #}
{% for property in class.properties limit: 3 %}
...
{% endfor %}
```

## Common Template Patterns

**Enumerating with indices**:
```liquid
{% for item in items %}
  {{ forloop.index }}: {{ item }}
{% endfor %}
```

**Grouping by type**:
```liquid
{% for class in classes %}
  {% assign props = class.properties | where: "range", "string" %}
  String properties: {{ props | map: "name" | join: ", " }}
{% endfor %}
```

**Nested loops**:
```liquid
{% for class in classes %}
  {% for property in class.properties %}
    - {{ class.name }}.{{ property.name }}
  {% endfor %}
{% endfor %}
```

## Next Steps

- Create more specialized templates (REST APIs, GraphQL schemas, database migrations)
- Combine multiple templates in one project
- [Multi-Language Project](07-multi-language-project.md) - Generate across languages
- [Advanced Template Patterns](../reference/templates.md) - Full template reference

## Summary

You've learned:
- ✅ Template structure and metadata definition
- ✅ Liquid template syntax and filters
- ✅ Template registration and testing
- ✅ Publishing templates for reuse
- ✅ Best practices for maintainable templates

Custom templates unlock ggen's full power!
