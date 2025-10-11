<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->
**Table of Contents**

- [Template Composition](#template-composition)
  - [Composition Strategies](#composition-strategies)
  - [Template Inheritance](#template-inheritance)
  - [Partial Templates](#partial-templates)
  - [Template Chaining](#template-chaining)
  - [Modular Design](#modular-design)
  - [Cross-Template Dependencies](#cross-template-dependencies)
  - [Composition Patterns](#composition-patterns)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

# Template Composition

Template composition allows you to build complex generation systems from smaller, reusable components. This chapter explores strategies for combining templates effectively to create maintainable and scalable code generation solutions.

## Composition Strategies

Templates can be composed in several ways:

### 1. Sequential Composition
Templates run in sequence, each contributing to the final output:

```yaml
# config.ggen.toml
templates:
  - "templates/base-setup.tmpl"
  - "templates/api-endpoints.tmpl"
  - "templates/tests.tmpl"
  - "templates/documentation.tmpl"
```

### 2. Hierarchical Composition
Templates organized in dependency hierarchies:

```yaml
# Project structure
templates/
├── base/
│   ├── project-setup.tmpl
│   └── module-structure.tmpl
├── features/
│   ├── api-endpoints.tmpl
│   └── database-models.tmpl
└── final/
    ├── tests.tmpl
    └── docs.tmpl
```

### 3. Functional Composition
Templates as composable functions:

```rust
// Template as a function
fn generate_api_endpoint(endpoint: ApiEndpoint) -> String {
    // Template logic here
}

// Compose multiple templates
let result = generate_base_struct() +
             generate_api_endpoints(endpoints) +
             generate_tests(endpoints);
```

## Template Inheritance

Create template hierarchies where child templates extend parent functionality:

### Base Template
```tmpl
---
to: "src/{{ module }}/mod.rs"
vars:
  base_imports: ["std::collections::HashMap"]
---

{{#each base_imports}}
use {{ this }};
{{/each}}

{{!-- Placeholder for child template content --}}
{{ block "content" }}
// Base implementation
{{ /block }}

{{ block "tests" }}
#[cfg(test)]
mod tests {
    // Base tests
}
{{ /block }}
```

### Child Template
```tmpl
---
to: "src/api/mod.rs"
extends: "templates/base-module.tmpl"
vars:
  module: "api"
  api_imports: ["serde::{Deserialize, Serialize}"]
---

{{!-- Override content block --}}
{{ block "content" }}
{{#each api_imports}}
use {{ this }};
{{/each}}

pub mod handlers;
pub mod models;
pub mod middleware;
{{ /block }}

{{!-- Extend tests block --}}
{{ block "tests" }}
{{ super }}  // Include parent tests

// API-specific tests
#[test]
fn test_api_endpoints() {
    // Test implementation
}
{{ /block }}
```

## Partial Templates

Break complex templates into smaller, reusable partials:

### Directory Structure
```bash
templates/
├── api/
│   ├── _header.tmpl      # Common imports and setup
│   ├── _endpoint.tmpl    # Individual endpoint pattern
│   └── endpoints.tmpl    # Main template using partials
```

### Header Partial
```tmpl
{{!-- templates/api/_header.tmpl --}}
use axum::{
    extract::Path,
    http::StatusCode,
    response::Json,
    routing::{get, post},
    Router,
};
use serde::{Deserialize, Serialize};
use std::collections::HashMap;
```

### Endpoint Partial
```tmpl
{{!-- templates/api/_endpoint.tmpl --}}
{{!--
  Generate a single API endpoint
  Variables: name, path, method, request_type, response_type
--}}
#[derive(Debug, Serialize, Deserialize)]
pub struct {{ request_type }} {
    {{#each fields}}
    pub {{ name }}: {{ type }},
    {{/each}}
}

#[derive(Debug, Serialize)]
pub struct {{ response_type }} {
    {{#each response_fields}}
    pub {{ name }}: {{ type }},
    {{/each}}
}

pub async fn {{ snake name }}_handler(
    {{#if request_type}}
    Json(payload): Json<{{ request_type }}>
    {{/if}}
) -> Result<Json<{{ response_type }}>, StatusCode> {
    // Handler implementation
    let response = {{ response_type }} {
        {{#each response_fields}}
        {{ name }}: {{ default_value }},
        {{/each}}
    };
    Ok(Json(response))
}
```

### Main Template
```tmpl
---
to: "src/api/handlers.rs"
vars:
  endpoints:
    - name: "CreateUser"
      path: "/users"
      method: "POST"
      request_type: "CreateUserRequest"
      response_type: "User"
    - name: "GetUser"
      path: "/users/:id"
      method: "GET"
      response_type: "User"
---

{{> api/_header }}

{{#each endpoints}}
{{> api/_endpoint }}
{{/each}}

pub fn create_router() -> Router {
    Router::new()
    {{#each endpoints}}
    .route("{{ path }}", {{ method | lower }}({{ snake name }}_handler))
    {{/each}}
}
```

## Template Chaining

Chain templates to create complex generation pipelines:

### Chain Configuration
```yaml
chains:
  api-project:
    - "templates/project-setup.tmpl"
    - "templates/dependencies.tmpl"
    - "templates/api-structure.tmpl"
    - "templates/endpoints.tmpl"
    - "templates/tests.tmpl"
```

### Template with Chain Logic
```tmpl
---
to: "src/lib.rs"
chain: "api-project"
vars:
  project_name: "My API"
  endpoints: [...]
---

// Generated by chain: api-project
// This file combines output from multiple templates

{{!-- Include results from previous templates --}}
{{ chain_output "project-setup" }}
{{ chain_output "dependencies" }}
{{ chain_output "api-structure" }}
```

## Modular Design

Design templates as composable modules:

### Module Interface
```rust
pub trait TemplateModule {
    fn generate(&self, context: &TemplateContext) -> Result<String>;
    fn dependencies(&self) -> Vec<String>;
    fn provides(&self) -> Vec<String>;
}
```

### Module Implementation
```rust
pub struct ApiModule {
    endpoints: Vec<ApiEndpoint>,
}

impl TemplateModule for ApiModule {
    fn generate(&self, context: &TemplateContext) -> Result<String> {
        // Generate API code
    }

    fn dependencies(&self) -> Vec<String> {
        vec!["base", "serde", "validation"]
    }

    fn provides(&self) -> Vec<String> {
        vec!["api_handlers", "api_models", "api_tests"]
    }
}
```

## Cross-Template Dependencies

Manage dependencies between templates:

### Dependency Declaration
```yaml
---
template: "api-handlers"
depends_on:
  - "models"      # Must run after models template
  - "validation"  # Must run after validation template
provides:
  - "handlers"    # Provides handler functions
---
```

### Dependency Resolution
```rust
fn resolve_template_order(templates: Vec<Template>) -> Vec<Template> {
    // Topological sort based on dependencies
    let mut graph = DependencyGraph::new();
    for template in &templates {
        graph.add_dependencies(template.id(), template.dependencies());
    }
    graph.topological_sort()
}
```

## Composition Patterns

Common patterns for template composition:

### Layered Architecture Pattern
```yaml
layers:
  base:
    - "templates/core-types.tmpl"
    - "templates/interfaces.tmpl"
  business:
    - "templates/domain-logic.tmpl"
    - "templates/use-cases.tmpl"
  api:
    - "templates/controllers.tmpl"
    - "templates/serialization.tmpl"
```

### Feature Flag Pattern
```yaml
features:
  authentication:
    templates: ["auth-middleware.tmpl", "user-model.tmpl"]
    enabled: "{{ features.auth }}"
  database:
    templates: ["db-connection.tmpl", "migrations.tmpl"]
    enabled: "{{ features.database }}"
```

### Environment-Specific Composition
```yaml
environments:
  development:
    templates: ["dev-setup.tmpl", "debug-logging.tmpl"]
    variables:
      debug: true
      logging_level: "debug"
  production:
    templates: ["prod-config.tmpl", "monitoring.tmpl"]
    variables:
      debug: false
      logging_level: "warn"
```

### Incremental Composition
```yaml
incremental:
  base:
    templates: ["project-structure.tmpl"]
  feature-a:
    templates: ["feature-a-models.tmpl", "feature-a-handlers.tmpl"]
    depends_on: ["base"]
  feature-b:
    templates: ["feature-b-models.tmpl", "feature-b-handlers.tmpl"]
    depends_on: ["base"]
```

## Best Practices

### Design Principles

1. **Single Responsibility** - Each template should have one clear purpose
2. **Loose Coupling** - Minimize dependencies between templates
3. **High Cohesion** - Related functionality should be grouped together
4. **Interface Segregation** - Templates should depend only on necessary interfaces

### Implementation Guidelines

1. **Document Dependencies** - Clearly specify what each template requires and provides
2. **Version Compatibility** - Ensure templates work across different versions
3. **Test Composition** - Verify that composed templates work together correctly
4. **Performance Awareness** - Consider the performance impact of template chains

### Maintenance Considerations

1. **Backward Compatibility** - Changes should not break existing compositions
2. **Gradual Migration** - Allow for incremental updates to template compositions
3. **Error Isolation** - Failures in one template shouldn't cascade to others
4. **Debuggability** - Make it easy to trace issues in composed templates

Template composition transforms individual templates into powerful generation systems, enabling complex software projects to be built from reusable, maintainable components.
