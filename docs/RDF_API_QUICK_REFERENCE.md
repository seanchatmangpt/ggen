# RDF API Quick Reference (v2.0)

## TL;DR

**v2.0 Change**: RDF files are now loaded via API/CLI instead of frontmatter.

### Before (v1.x)
```yaml
---
rdf:
  - domain.ttl
---
```

### After (v2.0)
```rust
template.render_with_rdf(vec![PathBuf::from("domain.ttl")], ...)
```

## API Usage

### Basic Example

```rust
use ggen_core::{Graph, Template};
use std::path::{Path, PathBuf};
use tera::Context;

// 1. Parse template
let template_str = r#"---
prefixes:
  ex: "http://example.org/"
sparql:
  people: "SELECT ?name WHERE { ?p a ex:Person ; ex:name ?name }"
---
Found: {{ sparql_count(results=sparql_results.people) }} people
"#;

let mut template = Template::parse(template_str)?;

// 2. Setup rendering context
let mut graph = Graph::new()?;
let mut tera = mk_tera();
let vars = Context::new();

// 3. Render with RDF files (NEW v2.0 API)
let rendered = template.render_with_rdf(
    vec![PathBuf::from("domain.ttl")],  // RDF files
    &mut graph,
    &mut tera,
    &vars,
    Path::new("template.tmpl"),
)?;

println!("{}", rendered);
```

### Multiple RDF Files

```rust
let rendered = template.render_with_rdf(
    vec![
        PathBuf::from("schema.ttl"),
        PathBuf::from("data.ttl"),
        PathBuf::from("constraints.ttl"),
    ],
    &mut graph,
    &mut tera,
    &vars,
    Path::new("template.tmpl"),
)?;
```

### With Template Variables

```rust
let mut vars = Context::new();
vars.insert("entity_type", "User");
vars.insert("namespace", "myapp");

let rendered = template.render_with_rdf(
    vec![PathBuf::from("domain.ttl")],
    &mut graph,
    &mut tera,
    &vars,
    Path::new("template.tmpl"),
)?;
```

## Template Format

### Minimal Template

```yaml
---
prefixes:
  ex: "http://example.org/"
sparql:
  query: "SELECT ?s WHERE { ?s a ex:Thing }"
---
Count: {{ sparql_results.query | length }}
```

### Complete Template

```yaml
---
to: "output/{{name}}.rs"
prefixes:
  ex: "http://example.org/"
  rdf: "http://www.w3.org/1999/02/22-rdf-syntax-ns#"
base: "http://example.org/base/"
rdf_inline:
  - "ex:{{entity}} a ex:Entity ."
sparql:
  entities: "SELECT ?name WHERE { ?e a ex:Entity ; ex:name ?name }"
  count: "SELECT (COUNT(?e) as ?cnt) WHERE { ?e a ex:Entity }"
---
// Generated code for {{name}}
pub struct {{ sparql_first(results=sparql_results.entities, column="name") }} {
    // {{ sparql_count(results=sparql_results.entities) }} entities
}
```

## SPARQL Helpers

### sparql_count()
```tera
{{ sparql_count(results=sparql_results.query_name) }}
```

### sparql_first()
```tera
{{ sparql_first(results=sparql_results.query_name, column="column_name") }}
```

### sparql_values()
```tera
{{ sparql_values(results=sparql_results.query_name, column="column_name") }}
```

### sparql_empty()
```tera
{% if sparql_empty(results=sparql_results.query_name) %}
  No results found
{% endif %}
```

### Direct Array Access
```tera
{% for row in sparql_results.query_name %}
  Name: {{ row.name }}  {# Note: column names have ? prefix in keys #}
{% endfor %}
```

## RDF File Formats

Supported formats:
- `.ttl`, `.turtle` - Turtle
- `.nt`, `.ntriples` - N-Triples
- `.rdf`, `.xml` - RDF/XML

Example:
```turtle
@prefix ex: <http://example.org/> .
@prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .

ex:User rdf:type ex:Entity ;
        ex:name "User" ;
        ex:description "User entity" .
```

## Error Handling

```rust
match template.render_with_rdf(rdf_files, &mut graph, &mut tera, &vars, &path) {
    Ok(rendered) => println!("{}", rendered),
    Err(e) => {
        if e.to_string().contains("Failed to read RDF file") {
            eprintln!("RDF file not found: {}", e);
        } else if e.to_string().contains("parse") {
            eprintln!("Invalid RDF syntax: {}", e);
        } else {
            eprintln!("Template error: {}", e);
        }
    }
}
```

## Migration from v1.x

### Step 1: Remove `rdf:` from frontmatter

```diff
---
-rdf:
-  - domain.ttl
-  - data.ttl
prefixes:
  ex: "http://example.org/"
sparql:
  query: "SELECT ?s WHERE { ?s a ex:Thing }"
---
```

### Step 2: Use render_with_rdf()

```diff
-let rendered = template.render(&mut tera, &vars)?;
+let rendered = template.render_with_rdf(
+    vec![PathBuf::from("domain.ttl"), PathBuf::from("data.ttl")],
+    &mut graph,
+    &mut tera,
+    &vars,
+    Path::new("template.tmpl"),
+)?;
```

### Step 3: Keep inline RDF (optional)

`rdf_inline:` field still works for template-embedded triples:

```yaml
---
rdf_inline:
  - "ex:{{entity}} a ex:Thing ."
---
```

## CLI Usage (Future)

```bash
# Basic usage
ggen new component Widget --rdf domain.ttl

# Multiple RDF files
ggen new service UserService \
  --rdf schema.ttl \
  --rdf data.ttl \
  --rdf constraints.ttl

# With variables
ggen new model User \
  --rdf domain.ttl \
  --var entity_type=User \
  --var namespace=myapp
```

## Best Practices

### 1. Organize RDF Files

```
project/
├── domain/
│   ├── schema.ttl     # Class definitions
│   ├── properties.ttl # Property definitions
│   └── constraints.ttl # Validation rules
└── data/
    ├── users.ttl      # User data
    └── roles.ttl      # Role data
```

### 2. Use Prefixes

Always define prefixes in frontmatter for cleaner SPARQL:

```yaml
prefixes:
  ex: "http://example.org/"
  schema: "http://schema.org/"
  rdf: "http://www.w3.org/1999/02/22-rdf-syntax-ns#"
```

### 3. Name SPARQL Queries

Use descriptive names for queries:

```yaml
sparql:
  entity_list: "SELECT ..."
  field_definitions: "SELECT ..."
  relationship_count: "SELECT (COUNT(?r) as ?cnt) ..."
```

### 4. Handle Empty Results

```tera
{% if sparql_empty(results=sparql_results.query) %}
  // No data found
{% else %}
  {% for row in sparql_results.query %}
    // Process {{ row.name }}
  {% endfor %}
{% endif %}
```

### 5. Use Template Variables

Make templates reusable:

```yaml
sparql:
  filtered: "SELECT ?name WHERE { ?e ex:type '{{entity_type}}' ; ex:name ?name }"
```

## Performance Tips

1. **Batch RDF files**: Load all RDF in one call instead of multiple calls
2. **Use SPARQL caching**: Graph caches query plans and results
3. **Minimize queries**: Combine data into fewer, broader queries
4. **Order results**: Use `ORDER BY` in SPARQL for consistent output

## Testing

### Unit Test Example

```rust
#[test]
fn test_template_rendering() -> Result<()> {
    let template_str = r#"---
prefixes:
  ex: "http://example.org/"
sparql:
  query: "SELECT ?s WHERE { ?s a ex:Thing }"
---
Count: {{ sparql_count(results=sparql_results.query) }}
"#;

    let mut template = Template::parse(template_str)?;
    let mut graph = Graph::new()?;
    let mut tera = mk_tera();
    let vars = Context::new();

    let rdf_file = create_test_rdf()?;

    let rendered = template.render_with_rdf(
        vec![rdf_file.path().to_path_buf()],
        &mut graph,
        &mut tera,
        &vars,
        Path::new("test.tmpl"),
    )?;

    assert!(rendered.contains("Count: 1"));
    Ok(())
}
```

## Troubleshooting

### Issue: "Failed to read RDF file"
**Solution**: Check file path is absolute or relative to current directory

### Issue: "Variable `column_name` not found"
**Solution**: SPARQL column names have `?` prefix - use helper functions:
```tera
{# WRONG #}
{{ row.name }}

{# RIGHT #}
{{ sparql_first(results=sparql_results.query, column="name") }}
```

### Issue: Empty SPARQL results
**Solution**: Check:
1. RDF file loaded correctly
2. Prefixes match between RDF and SPARQL
3. SPARQL query syntax
4. RDF data actually contains expected triples

### Issue: Template rendering fails
**Solution**: Verify:
1. Template syntax is valid Tera
2. All SPARQL queries in frontmatter are valid
3. Variables used in template are defined in context

## Resources

- **Test Suite**: `ggen-core/tests/rdf_rendering_e2e.rs`
- **Examples**: `ggen-core/tests/template_rdf_api_tests.rs`
- **Migration Guide**: `docs/MIGRATION_V1_TO_V2.md`
- **Validation Report**: `docs/RDF_E2E_VALIDATION_REPORT.md`

## Summary

```rust
// v2.0 API in 5 lines
let mut template = Template::parse(template_str)?;
let mut graph = Graph::new()?;
let mut tera = mk_tera();
let vars = Context::new();
let rendered = template.render_with_rdf(rdf_files, &mut graph, &mut tera, &vars, &path)?;
```

**Key Change**: RDF files from CLI/API, not frontmatter. Everything else unchanged.
