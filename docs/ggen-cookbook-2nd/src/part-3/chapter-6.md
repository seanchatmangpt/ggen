<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->
**Table of Contents**

- [Core Patterns](#core-patterns)
  - [Pattern 1: Single File Generator](#pattern-1-single-file-generator)
    - [Problem](#problem)
    - [Solution](#solution)
    - [Use Cases](#use-cases)
  - [Pattern 2: Multi-File Project](#pattern-2-multi-file-project)
    - [Problem](#problem-1)
    - [Solution](#solution-1)
    - [Use Cases](#use-cases-1)
  - [Pattern 3: Conditional Generation](#pattern-3-conditional-generation)
    - [Problem](#problem-2)
    - [Solution](#solution-2)
    - [Use Cases](#use-cases-2)
  - [Pattern 4: Template Inheritance](#pattern-4-template-inheritance)
    - [Problem](#problem-3)
    - [Solution](#solution-3)
    - [Use Cases](#use-cases-3)
  - [Pattern 5: Dynamic Variables](#pattern-5-dynamic-variables)
    - [Problem](#problem-4)
    - [Solution](#solution-4)
    - [Use Cases](#use-cases-4)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->
**Table of Contents**

- [Core Patterns](#core-patterns)
  - [Pattern 1: Single File Generator](#pattern-1-single-file-generator)
  - [Pattern 2: Multi-File Project](#pattern-2-multi-file-project)
  - [Pattern 3: Conditional Generation](#pattern-3-conditional-generation)
  - [Pattern 4: Template Inheritance](#pattern-4-template-inheritance)
  - [Pattern 5: Dynamic Variables](#pattern-5-dynamic-variables)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

# Core Patterns

Core patterns represent the fundamental building blocks of GGen template development. These patterns solve the most common code generation problems and serve as the foundation for more complex patterns.

## Pattern 1: Single File Generator

The most basic pattern - generating a single file from data.

### Problem
Generate a single source file (struct, function, module) from input data.

### Solution
```tmpl
---
to: "src/{{ name | lower }}.rs"
vars:
  name: "User"
  fields:
    - name: "id"
      type: "u64"
    - name: "username"
      type: "String"
    - name: "email"
      type: "String"
---

{{#if derive_debug}}
#[derive(Debug)]
{{/if}}
pub struct {{ name }} {
    {{#each fields}}
    pub {{ name }}: {{ type }},
    {{/each}}
}

impl {{ name }} {
    pub fn new({{#each fields}}{{ name }}: {{ type }}{{^last}}, {{/last}}{{/each}}) -> Self {
        Self {
            {{#each fields}}
            {{ name }},
            {{/each}}
        }
    }
}
```

### Use Cases
- Data structures (structs, enums)
- Configuration files
- Simple utility functions
- Documentation files

## Pattern 2: Multi-File Project

Generate multiple related files that work together.

### Problem
Generate a complete module with implementation, tests, and documentation.

### Solution
```yaml
# templates/user_module.tmpl
---
to:
  - "src/user.rs"
  - "src/user_tests.rs"
  - "README.md"
vars:
  module_name: "user"
  fields: [...]
---

// src/user.rs
pub mod {{ module_name }} {
    {{#each fields}}
    pub {{ name }}: {{ type }},
    {{/each}}
}

// src/user_tests.rs
#[cfg(test)]
mod tests {
    // Test implementation
}

// README.md
# {{ module_name }} Module
// Documentation content
```

### Use Cases
- Complete modules with tests
- API endpoints with handlers
- Database models with migrations
- Service implementations

## Pattern 3: Conditional Generation

Generate code based on conditions or feature flags.

### Problem
Generate different code based on configuration or environment.

### Solution
```tmpl
---
to: "src/config.rs"
vars:
  features:
    database: true
    authentication: true
    logging: false
---

pub struct Config {
    {{#if features.database}}
    pub database_url: String,
    {{/if}}
    {{#if features.authentication}}
    pub auth_token: String,
    {{/if}}
    {{#if features.logging}}
    pub log_level: String,
    {{/if}}
}

impl Config {
    {{#if features.database}}
    pub fn database_enabled(&self) -> bool {
        !self.database_url.is_empty()
    }
    {{/if}}
}
```

### Use Cases
- Feature flags
- Environment-specific code
- Optional dependencies
- Configuration variants

## Pattern 4: Template Inheritance

Extend existing templates with additional functionality.

### Problem
Create variations of existing templates without duplication.

### Solution
```tmpl
---
# Base template
to: "src/base_{{ name }}.rs"
extends: "templates/base_struct.tmpl"
vars:
  name: "Entity"
  base_fields: [...]
---

{{!-- Override the fields block --}}
{{ block "fields" }}
{{#each base_fields}}
pub {{ name }}: {{ type }},
{{/each}}
{{#each additional_fields}}
pub {{ name }}: {{ type }},
{{/each}}
{{ /block }}

{{!-- Add new methods --}}
{{ block "methods" }}
{{ super }}  // Include parent methods

pub fn custom_method(&self) -> String {
    "custom implementation".to_string()
}
{{ /block }}
```

### Use Cases
- Base classes with extensions
- Framework-specific implementations
- Version-specific code
- Customization layers

## Pattern 5: Dynamic Variables

Extract data from external sources to drive generation.

### Problem
Generate code based on external data sources or computed values.

### Solution
```tmpl
---
to: "src/generated_{{ entity_name | lower }}.rs"
vars:
  entity_name: "Product"
sparql:
  vars:
    - name: field_count
      query: "SELECT (COUNT(?field) as ?count) WHERE { ?s <urn:ex#field> ?field }"
    - name: table_name
      query: "SELECT ?name WHERE { <urn:ex#entity> <urn:ex#tableName> ?name }"
rdf:
  - "data/schema.ttl"
---

use sqlx::FromRow;

#[derive(FromRow)]
pub struct {{ entity_name }} {
    {{#each (range 1 field_count)}}
    pub field_{{ this }}: String,
    {{/each}}
}

impl {{ entity_name }} {
    pub async fn find_by_id(id: i32) -> Result<Self, sqlx::Error> {
        sqlx::query_as!(
            Self,
            "SELECT * FROM {{ table_name }} WHERE id = $1",
            id
        )
        .fetch_one(&pool)
        .await
    }
}
```

### Use Cases
- Database schema generation
- API client generation
- Configuration file generation
- Documentation from code

These core patterns provide the foundation for all GGen template development. Master these patterns, and you'll be able to solve the majority of code generation problems you encounter.
