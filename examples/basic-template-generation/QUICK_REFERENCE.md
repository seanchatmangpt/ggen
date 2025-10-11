# ggen Template Quick Reference

Quick reference guide for ggen template syntax and features.

## Template Structure

```yaml
---
# YAML Frontmatter (configuration)
to: path/to/{{ name }}.rs
vars:
  name: string
  count: number
---
{# Tera Template Body (code) #}
{{ name }} will be substituted here
```

## Frontmatter Fields

| Field | Type | Description |
|-------|------|-------------|
| `to` | String | Output file path (supports `{{ var }}` substitution) |
| `from` | String | Read template body from external file |
| `vars` | Map | Variable definitions with types |
| `force` | Boolean | Overwrite existing files if they exist |
| `unless_exists` | Boolean | Skip generation if file already exists |
| `inject` | Boolean | Enable injection into existing files |
| `before` | String | Inject before this marker |
| `after` | String | Inject after this marker |
| `prepend` | Boolean | Inject at file start |
| `append` | Boolean | Inject at file end |
| `at_line` | Number | Inject at specific line number |
| `skip_if` | String | Skip if this pattern is found in file |
| `base` | String | RDF base IRI (for graph features) |
| `prefixes` | Map | RDF namespace prefixes |
| `rdf_inline` | Array | Inline Turtle RDF triples |
| `rdf` | Array | Paths to RDF files to load |
| `sparql` | Map | Named SPARQL queries to execute |

## Variable Substitution

### Basic Syntax

```
{{ variable_name }}
```

Substitutes the variable value directly.

### With Filters

```
{{ variable_name | filter_name }}
```

Applies a transformation filter to the variable.

## Common Filters

| Filter | Example Input | Example Output | Description |
|--------|--------------|----------------|-------------|
| `snake_case` | "UserService" | "user_service" | Converts to snake_case |
| `pascal_case` | "user_service" | "UserService" | Converts to PascalCase |
| `camel_case` | "user_service" | "userService" | Converts to camelCase |
| `upper` | "hello" | "HELLO" | Converts to UPPERCASE |
| `lower` | "HELLO" | "hello" | Converts to lowercase |
| `capitalize` | "hello world" | "Hello world" | Capitalizes first letter |
| `trim` | "  hello  " | "hello" | Removes whitespace |
| `length` | [1,2,3] | 3 | Returns length |
| `first` | [1,2,3] | 1 | Returns first item |
| `last` | [1,2,3] | 3 | Returns last item |

## Control Structures

### Conditionals

```tera
{% if condition %}
  Code when true
{% endif %}
```

```tera
{% if condition %}
  Code when true
{% else %}
  Code when false
{% endif %}
```

```tera
{% if condition1 %}
  First case
{% elif condition2 %}
  Second case
{% else %}
  Default case
{% endif %}
```

### Loops

```tera
{% for item in items %}
  {{ item }}
{% endfor %}
```

#### Loop Variables

Inside a loop, you have access to:

| Variable | Description |
|----------|-------------|
| `loop.index` | Current iteration (1-indexed) |
| `loop.index0` | Current iteration (0-indexed) |
| `loop.first` | True if first iteration |
| `loop.last` | True if last iteration |
| `loop.length` | Total number of iterations |

```tera
{% for field in fields %}
  {{ field.name }}: {{ field.type }}{% if not loop.last %},{% endif %}
{% endfor %}
```

### Logical Operators

```tera
{% if x and y %}          {# Both must be true #}
{% if x or y %}           {# Either must be true #}
{% if not x %}            {# Must be false #}
{% if x == y %}           {# Equality #}
{% if x != y %}           {# Inequality #}
{% if x > y %}            {# Greater than #}
{% if x >= y %}           {# Greater or equal #}
{% if x < y %}            {# Less than #}
{% if x <= y %}           {# Less or equal #}
```

## Comments

```tera
{# This is a comment - won't appear in output #}

{#
  Multi-line comment
  Also won't appear in output
#}
```

## Variable Types

### String

```yaml
vars:
  name: string
  description: string
```

Usage:
```tera
{{ name }}
{{ description }}
```

### Boolean

```yaml
vars:
  has_id: boolean
  is_active: boolean
```

Usage:
```tera
{% if has_id %}
  pub id: u64,
{% endif %}
```

### Number

```yaml
vars:
  count: number
  max_items: number
```

Usage:
```tera
const MAX: usize = {{ max_items }};
```

### Array

```yaml
vars:
  fields: array
  items: array
```

Usage:
```tera
{% for field in fields %}
  {{ field }}
{% endfor %}
```

### Object

```yaml
vars:
  config: object
  settings: object
```

Usage:
```tera
{{ config.key }}
{{ settings.value }}
```

## Common Patterns

### Generating Rust Modules

```yaml
---
to: src/{{ name | snake_case }}.rs
vars:
  name: string
  description: string
---
//! {{ description }}

pub mod {{ name | snake_case }} {
    // Module code here
}
```

### Generating Structs with Optional Fields

```yaml
---
to: src/models/{{ name | snake_case }}.rs
vars:
  name: string
  has_id: boolean
  fields: array
---
#[derive(Debug, Clone)]
pub struct {{ name | pascal_case }} {
    {% if has_id %}
    pub id: u64,
    {% endif %}
    {% for field in fields %}
    pub {{ field.name | snake_case }}: {{ field.type }},
    {% endfor %}
}
```

### Generating with Timestamps

```yaml
---
to: src/{{ name | snake_case }}.rs
vars:
  name: string
  has_timestamps: boolean
---
{% if has_timestamps %}
use std::time::SystemTime;
{% endif %}

pub struct {{ name | pascal_case }} {
    {% if has_timestamps %}
    pub created_at: SystemTime,
    pub updated_at: SystemTime,
    {% endif %}
}
```

### Generating Tests

```yaml
---
to: tests/{{ name | snake_case }}_test.rs
vars:
  name: string
  functions: array
---
#[cfg(test)]
mod tests {
    use super::*;

    {% for func in functions %}
    #[test]
    fn test_{{ func | snake_case }}() {
        // Test {{ func }}
    }
    {% endfor %}
}
```

## Usage Examples

### In Rust Code

```rust
use ggen_core::template::Template;
use tera::{Context, Tera};

// Parse template
let input = std::fs::read_to_string("template.tmpl")?;
let mut template = Template::parse(&input)?;

// Set up variables
let mut tera = Tera::default();
let mut vars = Context::new();
vars.insert("name", "MyModule");
vars.insert("description", "Example module");

// Render frontmatter first
template.render_frontmatter(&mut tera, &vars)?;

// Then render body
let output = template.render(&mut tera, &vars)?;

// Get output path from frontmatter
let output_path = template.front.to.unwrap();
std::fs::write(output_path, output)?;
```

### With Complex Variables

```rust
use serde_json::json;

let mut vars = Context::new();
vars.insert("name", "User");
vars.insert("has_id", &true);
vars.insert("has_timestamps", &true);
vars.insert("fields", &json!([
    {
        "name": "username",
        "type": "String",
        "doc": "User login name"
    },
    {
        "name": "email",
        "type": "String",
        "doc": "Email address"
    }
]));
```

## Best Practices

1. **Always define variables in frontmatter** - Makes templates self-documenting
2. **Use appropriate filters** - `snake_case` for Rust, `camelCase` for JavaScript
3. **Add comments** - Explain complex logic with `{# comments #}`
4. **Handle edge cases** - Use conditionals for optional features
5. **Generate valid code** - Test templates with different variable values
6. **Include documentation** - Generate doc comments with `///`
7. **Keep templates focused** - One file per template usually works best
8. **Use descriptive variable names** - `has_id` is better than `has_i`

## Troubleshooting

### Template Parse Errors

**Error**: `Failed to parse frontmatter`
- Check YAML syntax between `---` markers
- Ensure proper indentation (use spaces, not tabs)
- Verify all quotes are balanced

### Variable Not Found

**Error**: `Variable 'x' not found`
- Ensure variable is defined in `vars:` section
- Check variable name spelling
- Verify variable is provided when rendering

### Filter Errors

**Error**: `Filter 'xyz' not found`
- Use only built-in Tera filters
- Check filter name spelling
- Refer to filter list above for available filters

### Syntax Errors in Generated Code

- Test template with sample variables
- Check conditional logic
- Verify loop syntax
- Ensure filters produce valid output for your target language

## Additional Resources

- [Tera Template Documentation](https://keats.github.io/tera/)
- ggen Documentation: See main README.md
- Example Templates: Browse `/examples` directory
- Template Tests: See `ggen-core/src/template.rs` tests
