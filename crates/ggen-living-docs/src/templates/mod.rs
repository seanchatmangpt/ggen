//! Template management for narrative generation

use std::collections::HashMap;

/// Template registry for documentation narratives
pub struct TemplateRegistry {
    templates: HashMap<String, String>,
}

impl TemplateRegistry {
    /// Create a new template registry with default templates
    pub fn new() -> Self {
        let mut registry = Self {
            templates: HashMap::new(),
        };

        // Register default templates
        registry.register_defaults();
        registry
    }

    /// Register default templates
    fn register_defaults(&mut self) {
        self.register("changelog", Self::changelog_template());
        self.register("api_diff", Self::api_diff_template());
        self.register("migration_guide", Self::migration_guide_template());
        self.register("architecture", Self::architecture_template());
    }

    /// Register a template
    pub fn register(&mut self, name: &str, template: String) {
        self.templates.insert(name.to_string(), template);
    }

    /// Get a template by name
    pub fn get(&self, name: &str) -> Option<&String> {
        self.templates.get(name)
    }

    // Template definitions

    fn changelog_template() -> String {
        r#"# Changelog

## {{ version }} - {{ date }}

### Added
{% for item in added %}
- {{ item }}
{% endfor %}

### Changed
{% for item in changed %}
- {{ item }}
{% endfor %}

### Deprecated
{% for item in deprecated %}
- {{ item }}
{% endfor %}

### Removed
{% for item in removed %}
- {{ item }}
{% endfor %}

### Fixed
{% for item in fixed %}
- {{ item }}
{% endfor %}
"#.to_string()
    }

    fn api_diff_template() -> String {
        r#"# API Changes

## Summary

{{ summary }}

## Breaking Changes

{% for change in breaking_changes %}
### {{ change.name }}

**Before:**
```rust
{{ change.before }}
```

**After:**
```rust
{{ change.after }}
```

**Migration:** {{ change.migration }}
{% endfor %}

## New APIs

{% for api in new_apis %}
- `{{ api.name }}` - {{ api.description }}
{% endfor %}
"#.to_string()
    }

    fn migration_guide_template() -> String {
        r#"# Migration Guide: {{ from_version }} → {{ to_version }}

## Overview

{{ overview }}

## Steps

{% for step in steps %}
### {{ step.number }}. {{ step.title }}

{{ step.description }}

```rust
{{ step.code_example }}
```
{% endfor %}

## Breaking Changes

{% for change in breaking_changes %}
- **{{ change.item }}**: {{ change.reason }}
  - Migration: {{ change.migration }}
{% endfor %}
"#.to_string()
    }

    fn architecture_template() -> String {
        r#"# Architecture Overview

## System Architecture

{{ description }}

## Components

{% for component in components %}
### {{ component.name }}

{{ component.description }}

**Dependencies:**
{% for dep in component.dependencies %}
- {{ dep }}
{% endfor %}

**Responsibilities:**
{% for resp in component.responsibilities %}
- {{ resp }}
{% endfor %}
{% endfor %}

## Data Flow

```
{{ data_flow_diagram }}
```
"#.to_string()
    }
}

impl Default for TemplateRegistry {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_template_registry() {
        let registry = TemplateRegistry::new();
        assert!(registry.get("changelog").is_some());
        assert!(registry.get("api_diff").is_some());
        assert!(registry.get("migration_guide").is_some());
    }
}
