<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->
**Table of Contents**

- [The .tmpl Format](#the-tmpl-format)
  - [File Structure](#file-structure)
  - [Frontmatter](#frontmatter)
  - [Template Body](#template-body)
  - [Output Specification](#output-specification)
  - [Variable Interpolation](#variable-interpolation)
  - [Control Structures](#control-structures)
  - [Functions and Helpers](#functions-and-helpers)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

# The .tmpl Format

The `.tmpl` format is GGen's native template language, designed to be both human-readable and machine-processable. It combines YAML frontmatter for metadata with a flexible templating syntax for content generation.

## File Structure

A typical `.tmpl` file has three main sections:

1. **Frontmatter** (YAML metadata at the top)
2. **Template Body** (the actual generation logic)
3. **Comments and Documentation** (inline explanations)

Here's the basic structure:

```tmpl
---
# Frontmatter (YAML)
to: "output/{{ name }}.rs"
vars:
  author: "GGen Team"
rdf:
  - "data/ontology.ttl"
---

// Template body starts here
// This is the actual code that gets generated

{{#if module}}
pub mod {{ module }};
{{/if}}

struct {{ name }} {
    // Generated fields will go here
}
```

## Frontmatter

The frontmatter is YAML metadata that configures the template's behavior:

### Required Fields

- **`to`**: Output file path template (supports variable interpolation)
- **`vars`**: Static variables available during rendering

### Optional Fields

- **`rdf`**: RDF data sources to load
- **`shape`**: SHACL validation files
- **`sparql`**: SPARQL queries for data extraction
- **`determinism`**: Deterministic processing controls

### Example Frontmatter

```yaml
---
to: "src/api/{{ version }}/mod.rs"
vars:
  version: "v1"
  author: "API Team"
rdf:
  - "ontology/api.ttl"
  - "data/endpoints.jsonld"
shape:
  - "validation/api-shapes.ttl"
sparql:
  vars:
    - name: base_path
      query: "SELECT ?path WHERE { <urn:api#config> <urn:api#basePath> ?path }"
      default: "/api/v1"
determinism:
  sort: [endpoint, method]
  seed: "{{ version }}-{{ author }}"
---
```

## Template Body

The template body uses a syntax similar to Handlebars.js but optimized for code generation:

### Variable Interpolation

Use `{{ variable }}` to insert values:

```rust
// Generated code
struct {{ name }} {
    id: {{ type }},
    name: String,
}
```

### Conditional Blocks

Use `{{#if}}` and `{{/if}}` for conditional generation:

```rust
{{#if has_validation}}
#[derive(Validate)]
{{/if}}
struct {{ name }} {
    // fields...
}
```

### Loops

Use `{{#each}}` to iterate over collections:

```rust
{{#each fields}}
pub {{ name }}: {{ type }},
{{/each}}
```

### Comments

Use `//` for template comments (not included in output):

```rust
// This comment explains the template logic
// {{ variable }} will be replaced with actual values
```

## Output Specification

The `to` field specifies where and how to write the generated file:

### Path Templates

Support for complex path structures:

```yaml
to: "src/{{ module }}/tests/{{ name }}_test.rs"
to: "{{ lang }}/{{ framework }}/src/main.{{ ext }}"
```

### Multiple Outputs

Some templates can generate multiple files:

```yaml
to:
  - "src/{{ name }}.rs"
  - "tests/{{ name }}_test.rs"
  - "docs/{{ name }}.md"
```

## Variable Interpolation

Variables come from multiple sources:

1. **Static vars** in frontmatter
2. **SPARQL queries** extracting data from RDF
3. **Template context** (file paths, timestamps, etc.)
4. **System environment** variables

### Interpolation Syntax

- `{{ variable }}` - Simple variable
- `{{ variable.subfield }}` - Nested object access
- `{{ variable | upper }}` - Apply filters

### Built-in Variables

- `{{ file }}` - Current template filename
- `{{ dir }}` - Template directory
- `{{ timestamp }}` - Generation timestamp
- `{{ seed }}` - Determinism seed

## Control Structures

### Conditionals

```rust
{{#if condition}}
// This code only generates if condition is true
{{/if}}

{{#unless disabled}}
// This code generates unless disabled is true
{{/unless}}
```

### Loops

```rust
{{#each items}}
// This repeats for each item in the items array
pub fn {{ name }}() -> {{ type }} {
    // implementation
}
{{/each}}
```

### With Blocks

```rust
{{#with user as u}}
// u is now an alias for user
Hello {{ u.name }}!
{{/with}}
```

## Functions and Helpers

GGen provides built-in helper functions:

### String Helpers

- `{{ upper "text" }}` - Convert to uppercase
- `{{ lower "TEXT" }}` - Convert to lowercase
- `{{ camel "snake_case" }}` - Convert to CamelCase
- `{{ snake "CamelCase" }}` - Convert to snake_case

### Path Helpers

- `{{ join "path" "segments" }}` - Join path segments
- `{{ dirname "path/file" }}` - Get directory part
- `{{ basename "path/file" }}` - Get filename part
- `{{ extname "file.txt" }}` - Get file extension

### Collection Helpers

- `{{ first array }}` - Get first item
- `{{ last array }}` - Get last item
- `{{ length array }}` - Get array length
- `{{ sort array }}` - Sort array

### Example Usage

```rust
// Using helpers in templates
const {{ upper name }}_VERSION: &str = "{{ version }}";

{{#each endpoints}}
#[route("{{ join base_path path }}", method = "{{ method }}")]
pub async fn {{ snake name }}() -> Result<Json<{{ response_type }}>> {
    // handler implementation
}
{{/each}}
```

## Best Practices

### Template Organization

1. **Keep templates focused** - One template per logical unit
2. **Use descriptive variable names** - Make intent clear
3. **Document complex logic** - Use comments liberally
4. **Test template output** - Verify generated code compiles

### Performance Considerations

1. **Minimize complex expressions** in loops
2. **Use efficient SPARQL queries** - Include ORDER BY for matrix queries
3. **Cache expensive operations** - Use static variables when possible
4. **Profile template rendering** - Identify bottlenecks

This `.tmpl` format provides the foundation for all GGen code generation, balancing expressiveness with predictability.
