<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->
**Table of Contents**

- [Body & Templating Syntax](#body--templating-syntax)
  - [Basic Syntax Elements](#basic-syntax-elements)
    - [Variables](#variables)
    - [Expressions](#expressions)
  - [Variable Interpolation](#variable-interpolation)
    - [String Variables](#string-variables)
    - [Boolean Variables](#boolean-variables)
    - [Array/Collection Variables](#arraycollection-variables)
    - [Object Variables](#object-variables)
  - [Control Flow](#control-flow)
    - [If Statements](#if-statements)
    - [Unless Statements](#unless-statements)
    - [Else Conditions](#else-conditions)
  - [Iteration](#iteration)
    - [Each Loops](#each-loops)
    - [Range Loops](#range-loops)
    - [Object Iteration](#object-iteration)
  - [Conditionals](#conditionals)
    - [Comparison Operators](#comparison-operators)
    - [Logical Operators](#logical-operators)
    - [Nested Conditions](#nested-conditions)
  - [Comments and Whitespace](#comments-and-whitespace)
    - [Single-line Comments](#single-line-comments)
    - [Multi-line Comments](#multi-line-comments)
    - [Template Comments](#template-comments)
  - [Escaping and Raw Text](#escaping-and-raw-text)
    - [Escaping Special Characters](#escaping-special-characters)
    - [Raw Blocks](#raw-blocks)
  - [Nested Structures](#nested-structures)
    - [Nested Functions](#nested-functions)
    - [Nested Modules](#nested-modules)
  - [Error Handling](#error-handling)
    - [Template Validation](#template-validation)
    - [Graceful Degradation](#graceful-degradation)
    - [Error Context](#error-context)
  - [Advanced Syntax](#advanced-syntax)
    - [Helper Functions](#helper-functions)
    - [Partials and Includes](#partials-and-includes)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

# Body & Templating Syntax

The template body is where the actual code generation happens. GGen uses a powerful yet intuitive syntax that balances expressiveness with readability. This chapter covers the core templating constructs that bring your templates to life.

## Basic Syntax Elements

Template syntax uses `{{` and `}}` delimiters for dynamic content:

### Variables
```rust
// Simple variable
const VERSION: &str = "{{ version }}";

// Object property access
const AUTHOR: &str = "{{ author.name }}";

// Array index
const FIRST_ITEM: &str = "{{ items[0] }}";
```

### Expressions
```rust
// Function calls
const UPPER_NAME: &str = "{{ upper name }}";

// Method chaining
const SAFE_NAME: &str = "{{ name | lower | replace ' ' '_' }}";

// Arithmetic
const TOTAL: usize = {{ count * 2 + 10 }};
```

## Variable Interpolation

Variables are the foundation of dynamic content generation:

### String Variables
```rust
struct {{ struct_name }} {
    name: String,
    value: {{ value_type }},
}
```

### Boolean Variables
```rust
{{#if debug_enabled}}
println!("Debug: {:?}", value);
{{/if}}
```

### Array/Collection Variables
```rust
{{#each fields}}
pub {{ name }}: {{ type }},
{{/each}}
```

### Object Variables
```rust
impl {{ struct_name }} {
    pub fn new() -> Self {
        Self {
            name: "{{ default_name }}".to_string(),
            config: {{ config_type }}::{{ config_method }}(),
        }
    }
}
```

## Control Flow

Control flow structures enable conditional and iterative generation:

### If Statements
```rust
{{#if condition}}
// This code generates only if condition is true
#[derive(Debug)]
{{/if}}
struct MyStruct {
    // fields...
}
```

### Unless Statements
```rust
{{#unless production}}
// This code generates only if NOT in production
#[cfg(test)]
{{/unless}}
```

### Else Conditions
```rust
{{#if has_validation}}
#[derive(Validate)]
{{else}}
#[derive(Debug)]
{{/if}}
```

## Iteration

Iteration is essential for generating repetitive code patterns:

### Each Loops
```rust
{{#each endpoints}}
#[route("{{ path }}", method = "{{ method }}")]
pub async fn {{ snake name }}() -> Result<Json<{{ response_type }}>> {
    // handler implementation
}
{{/each}}
```

### Range Loops
```rust
{{#each (range 1 10) as |i|}}
pub fn method_{{ i }}() -> Result<()> {
    // method {{ i }}
    Ok(())
}
{{/each}}
```

### Object Iteration
```rust
{{#each config as |key value|}}
println!("{}: {}", key, value);
{{/each}}
```

## Conditionals

Complex conditional logic for sophisticated generation:

### Comparison Operators
```rust
{{#if (eq type "string")}}
// String-specific code
{{/if}}

{{#if (gt count 5)}}
{{#each (range count) as |i|}}
// Generate {{ count }} items
{{/each}}
{{/if}}
```

### Logical Operators
```rust
{{#if (and has_validation (not production))}}
// Validation code for non-production
{{/if}}

{{#if (or debug_enabled test_mode)}}
println!("Debug info: {:?}", data);
{{/if}}
```

### Nested Conditions
```rust
{{#if framework}}
{{#if (eq framework "axum")}}
use axum::{routing::get, Router};
{{else if (eq framework "warp")}}
use warp::Filter;
{{/if}}
{{/if}}
```

## Comments and Whitespace

Template comments help document generation logic:

### Single-line Comments
```rust
// This struct handles {{ purpose }}
// Generated fields: {{ field_count }}
struct {{ name }} {
    {{#each fields}}
    // {{ description }}
    pub {{ name }}: {{ type }},
    {{/each}}
}
```

### Multi-line Comments
```rust
/*
 * {{ struct_name }} struct
 * Purpose: {{ purpose }}
 * Generated: {{ timestamp }}
 */
```

### Template Comments
```rust
{{!--
  This entire block is a comment
  and won't appear in generated code
--}}
```

## Escaping and Raw Text

Sometimes you need literal template syntax in output:

### Escaping Special Characters
```rust
// Output: {{ variable }}
const TEMPLATE_SYNTAX: &str = "{{ '{{' }} variable {{ '}}' }}";
```

### Raw Blocks
```rust
{{{ raw }
// This is literal text that won't be processed
// {{ variable }} stays as {{ variable }}
{{{ /raw }}}
```

## Nested Structures

Templates can generate complex nested code:

### Nested Functions
```rust
impl {{ struct_name }} {
    {{#each methods}}
    pub fn {{ name }}(&self{{#if params}}, {{/if}}{{#each params}}{{ name }}: {{ type }}{{^last}}, {{/last}}{{/each}}) -> {{ return_type }} {
        {{#if body}}
        {{ body }}
        {{/if}}
        {{#if return_expr}}
        {{ return_expr }}
        {{/if}}
    }
    {{/each}}
}
```

### Nested Modules
```rust
{{#if module}}
pub mod {{ module }} {
    {{#each submodules}}
    pub mod {{ name }} {
        {{#each items}}
        pub struct {{ name }} {
            {{#each fields}}
            pub {{ name }}: {{ type }},
            {{/each}}
        }
        {{/each}}
    }
    {{/each}}
}
{{/if}}
```

## Error Handling

Robust error handling ensures template reliability:

### Template Validation
```rust
{{!-- Validate required variables --}}
{{#if (not name)}}
{{!-- Error: name variable is required --}}
{{/if}}

{{!-- Validate data types --}}
{{#if (and fields (not (is_array fields)))}}
{{!-- Error: fields must be an array --}}
{{/if}}
```

### Graceful Degradation
```rust
{{#if optional_feature}}
// Optional feature code
{{else}}
// Fallback code when feature unavailable
{{/if}}
```

### Error Context
```rust
{{!--
  Template: {{ file }}
  Line: {{ line }}
  Error: Missing required variable
--}}
```

## Advanced Syntax

### Helper Functions
```rust
// Built-in helpers
const SAFE_NAME: &str = "{{ name | lower | replace ' ' '_' }}";
const FORMATTED_COUNT: String = "{{ count | format '{} items' }}";

// Custom helpers (defined in template context)
const PROCESSED_DATA: String = "{{ process_data data }}";
```

### Partials and Includes
```rust
// Include common patterns
{{> common_headers }}

struct {{ name }} {
    {{#each fields}}
    {{> field_definition }}
    {{/each}}
}

// Common field pattern
{{!-- partials/field_definition --}}
pub {{ name }}: {{ type }}{{^if optional}}?{{/if}},
```

This syntax provides the expressive power needed for complex code generation while maintaining clarity and maintainability.
