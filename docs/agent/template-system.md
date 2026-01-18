# Template System Guide

## Tera Template Engine

ggen uses **Tera** (Jinja2-compatible template engine) for code generation with RDF integration.

### Template Loading & Rendering

```rust
use tera::Tera;
use std::collections::HashMap;

pub struct TemplateRenderer {
    tera: Tera,
    ontology: Store,
}

impl TemplateRenderer {
    pub fn new(template_dir: &Path, ontology: Store) -> Result<Self> {
        let tera = Tera::new(&format!("{}/**/*.tmpl", template_dir.display()))?;
        Ok(Self { tera, ontology })
    }

    pub fn render(&self, template_name: &str, context: &TemplateContext) -> Result<String> {
        self.tera.render(template_name, context)
    }
}
```

### Variable Substitution

```liquid
Project: {{ project_name }}
Author: {{ author }}
Framework: {{ framework }}
Created: {{ created_at | date(format="%Y-%m-%d") }}
```

### Conditional Rendering

```liquid
{%- if framework == "axum" -%}
# Async Rust Web Framework
{%- elif framework == "express" -%}
# JavaScript Web Framework
{%- else -%}
# Unknown Framework
{%- endif -%}
```

### Looping

```liquid
{%- for dependency in dependencies -%}
- {{ dependency.name }}: ^{{ dependency.version }}
{%- endfor -%}
```

### Filters & Functions

```liquid
{{ package_name | lower | replace(from="-", to="_") }}
{% include "common/license-header.tmpl" with payload %}
{% set total = items | length %}
```

## SPARQL Queries in Templates

Templates embed SPARQL to dynamically fetch ontology data:

```liquid
{%- set all_entities = sparql(
    "SELECT ?entity ?type WHERE { ?entity a ?type }",
    graph=ontology
) -%}

{%- if all_entities -%}
Discovered entities:
{%- for entity in all_entities -%}
  - {{ entity.entity }} ({{ entity.type }})
{%- endfor -%}
{%- endif -%}
```

### Dynamic Template Selection

```liquid
{%- set template_id = framework | lower -%}
{%- set has_template = sparql(
    "ASK { <" ~ marketplace_base ~ "templates/" ~ template_id ~ "> a ggen:Template }",
    graph=marketplace_ontology
) -%}

{%- if has_template -%}
{%- include "frameworks/" ~ framework | lower ~ "/model.tmpl" -%}
{%- else -%}
No framework template available for {{ framework }}
{%- endif -%}
```

### Query Results in Loops

```liquid
{%- set packages = sparql(
    "SELECT ?pkg ?name ?version WHERE {
       ?pkg a ggen:Package .
       ?pkg rdfs:label ?name .
       ?pkg dcat:version ?version .
       FILTER (?pkg IN (?dependency1, ?dependency2))
     }
     ORDER BY ?name",
    graph=marketplace
) -%}

Dependencies:
{%- for package in packages %}
  - {{ package.name }}: {{ package.version }}
{%- endfor %}
```

## File Injection Patterns

### Copy-and-Inject Pattern

**Input:** Template with placeholders
**Output:** Generated code with injected content

```rust
pub fn inject_into_template(
    template_str: &str,
    injections: HashMap<String, String>,
) -> Result<String> {
    let mut result = template_str.to_string();

    for (key, value) in injections {
        let placeholder = format!("{{{{ INJECT_{} }}}}", key);
        result = result.replace(&placeholder, &value);
    }

    Ok(result)
}
```

Template file:

```rust
// {{{ INJECT_IMPORTS }}}

pub struct {{ entity_name }} {
    // {{{ INJECT_FIELDS }}}
}

impl {{ entity_name }} {
    // {{{ INJECT_METHODS }}}
}
```

Generated with:

```rust
let injections = hashmap![
    "IMPORTS" => "use chrono::DateTime;\nuse uuid::Uuid;",
    "FIELDS" => "pub id: Uuid,\npub created_at: DateTime<Utc>,",
    "METHODS" => "pub fn new() -> Self { Self { id: Uuid::new_v4(), created_at: Utc::now() } }",
];
```

### File Tree Generation

ggen supports **directory tree templating** where folder structure encodes semantics:

```
templates/rust-api/
├── Cargo.toml                    # Cargo manifest (not templated)
├── src/
│   ├── main.rs.tmpl            # Rendered to main.rs
│   ├── {{ entity }}/
│   │   ├── mod.rs.tmpl
│   │   ├── model.rs.tmpl        # One per entity in ontology
│   │   └── handler.rs.tmpl
│   └── lib.rs.tmpl
├── tests/
│   └── integration_test.rs.tmpl
└── docs/
    └── API.md.tmpl
```

Files/directories with `{{ variable }}` syntax are expanded with ontology data.

### Conditional File Inclusion

```liquid
{%- if framework == "async" -%}
# Include async-specific files
{%- endif -%}

{%- for entity in entities -%}
# Generate entity-specific files
{%- endfor -%}
```

## Liquid Template Syntax for Rust Contexts

### Render Context Structure

```rust
use serde::Serialize;

#[derive(Serialize)]
pub struct RenderContext {
    pub project_name: String,
    pub package_name: String,
    pub entities: Vec<Entity>,
    pub config: ProjectConfig,
    pub ontology_uri: String,
}

#[derive(Serialize)]
pub struct Entity {
    pub name: String,
    pub id: String,
    pub properties: Vec<Property>,
}

#[derive(Serialize)]
pub struct Property {
    pub name: String,
    pub rust_type: String,
    pub required: bool,
}
```

### Passing Context to Tera

```rust
pub fn render_entity_file(
    entity: &Entity,
    context: &RenderContext,
) -> Result<String> {
    let mut tera = Tera::default();
    tera.add_raw_template("entity.rs", include_str!("../templates/entity.rs.tmpl"))?;

    let mut map = serde_json::to_value(context)?
        .as_object_mut()
        .unwrap()
        .clone();

    map.insert("current_entity".to_string(), serde_json::to_value(entity)?);

    tera.render("entity.rs", &map)
}
```

## Template Metadata

Templates declare RDF metadata for discovery:

```yaml
---
rdf:
  id: http://marketplace.ggen.io/templates/rust-api
  type: ggen:Template
  framework: axum
  language: rust
  version: "1.0.0"
metadata:
  author: ggen-team
  description: Full-stack Rust REST API with Axum and PostgreSQL
  tags: [api, rest, async, database]
  supports:
    - entities: true
    - relationships: true
    - validation: true
    - database: postgresql
    - auth: jwt
---
```

## Performance Characteristics

- **Template parsing:** <50ms per template
- **Rendering 100 files:** <500ms
- **SPARQL queries in templates:** <100ms each (with caching)
- **File tree expansion:** O(n) where n = files in tree

## Testing Template Rendering

```rust
#[test]
fn test_entity_generation() {
    // Arrange
    let context = RenderContext {
        project_name: "test-project".to_string(),
        entities: vec![
            Entity {
                name: "User".to_string(),
                id: "user".to_string(),
                properties: vec![
                    Property {
                        name: "id".to_string(),
                        rust_type: "Uuid".to_string(),
                        required: true,
                    },
                ],
            },
        ],
    };

    // Act
    let rendered = render_entity_file(&context.entities[0], &context).unwrap();

    // Assert
    assert!(rendered.contains("pub struct User"));
    assert!(rendered.contains("pub id: Uuid"));
    assert!(!rendered.is_empty());
}
```

## Critical Rules

1. **ALWAYS escape user input in templates** - Use `{{ var | escape }}`
2. **AVOID unbounded loops** - Use LIMIT in SPARQL queries
3. **CACHE template objects** - Don't recompile templates repeatedly
4. **VALIDATE ontology before rendering** - Check graph consistency first
5. **PREFER filters over logic** - Use Tera filters instead of complex conditions
6. **TEST with real ontologies** - Templates tested against representative RDF data
7. **DOCUMENT template variables** - Each template file should document required context

---

## Examples

### Rust Model Generation

```liquid
{% for entity in entities %}
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct {{ entity.name | capitalize }} {
    {% for prop in entity.properties %}
    pub {{ prop.name }}: {{ prop.rust_type }},
    {% endfor %}
}
{% endfor %}
```

### TypeScript Interface Generation

```liquid
{% for entity in entities %}
export interface {{ entity.name | capitalize }} {
    {% for prop in entity.properties %}
    {{ prop.name }}: {{ prop.typescript_type }};
    {% endfor %}
}
{% endfor %}
```

### Dynamic Imports

```liquid
{% for dependency in dependencies %}
import { {{ dependency.exported_name }} } from '{{ dependency.package }}';
{% endfor %}
```
