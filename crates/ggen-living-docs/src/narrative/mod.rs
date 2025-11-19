//! Automated narrative generation from code ontology
//!
//! Generates human-readable documentation narratives from semantic code ontology.

use crate::{Error, Result, config::NarrativeConfig, ontology::CodeOntology};
use tera::{Tera, Context};
use std::collections::HashMap;
use tracing::{debug, info, instrument};

/// Narrative generator that creates human-readable documentation
pub struct NarrativeGenerator {
    config: NarrativeConfig,
    templates: Tera,
}

impl NarrativeGenerator {
    /// Create a new narrative generator
    #[instrument(skip(config))]
    pub fn new(config: &NarrativeConfig) -> Result<Self> {
        info!("Initializing narrative generator");

        // Initialize Tera with embedded templates
        let mut tera = Tera::default();

        // Add default templates
        tera.add_raw_templates(vec![
            ("function", Self::function_template()),
            ("struct", Self::struct_template()),
            ("module", Self::module_template()),
            ("trait", Self::trait_template()),
            ("enum", Self::enum_template()),
            ("overview", Self::overview_template()),
        ]).map_err(|e| Error::Template(e))?;

        Ok(Self {
            config: config.clone(),
            templates: tera,
        })
    }

    /// Generate narratives from ontology
    #[instrument(skip(self, ontology))]
    pub async fn generate_from_ontology(&self, ontology: &CodeOntology) -> Result<Vec<String>> {
        info!("Generating narratives from ontology");

        let mut narratives = Vec::new();

        // Get all entities from ontology
        let validation = ontology.validate_completeness().await?;

        debug!("Generating overview narrative");
        let overview = self.generate_overview(&validation)?;
        narratives.push(overview);

        // Generate entity-specific narratives
        // In a full implementation, iterate through all entities and generate narratives

        info!("Generated {} narratives", narratives.len());
        Ok(narratives)
    }

    /// Generate overview narrative
    fn generate_overview(&self, validation: &crate::ValidationReport) -> Result<String> {
        let mut context = Context::new();
        context.insert("total_entities", &validation.total_entities);
        context.insert("documented_entities", &validation.documented_entities);
        context.insert("coverage_percentage", &validation.coverage_percentage);

        self.templates.render("overview", &context)
            .map_err(Error::Template)
    }

    /// Generate narrative for a function
    pub fn generate_function_narrative(&self, name: &str, doc: Option<&str>, params: &[String], returns: Option<&str>) -> Result<String> {
        let mut context = Context::new();
        context.insert("name", name);
        context.insert("documentation", &doc.unwrap_or(""));
        context.insert("parameters", params);
        context.insert("returns", &returns.unwrap_or("()"));

        self.templates.render("function", &context)
            .map_err(Error::Template)
    }

    /// Generate narrative for a struct
    pub fn generate_struct_narrative(&self, name: &str, doc: Option<&str>, fields: &[String]) -> Result<String> {
        let mut context = Context::new();
        context.insert("name", name);
        context.insert("documentation", &doc.unwrap_or(""));
        context.insert("fields", fields);

        self.templates.render("struct", &context)
            .map_err(Error::Template)
    }

    /// Generate narrative for a module
    pub fn generate_module_narrative(&self, name: &str, doc: Option<&str>, contents: &[String]) -> Result<String> {
        let mut context = Context::new();
        context.insert("name", name);
        context.insert("documentation", &doc.unwrap_or(""));
        context.insert("contents", contents);

        self.templates.render("module", &context)
            .map_err(Error::Template)
    }

    // Template definitions

    fn function_template() -> &'static str {
        r#"# Function: {{ name }}

{% if documentation %}
## Description

{{ documentation }}
{% endif %}

## Signature

```rust
fn {{ name }}({% for param in parameters %}{{ param }}{% if not loop.last %}, {% endif %}{% endfor %}) -> {{ returns }}
```

{% if parameters %}
## Parameters

{% for param in parameters %}
- `{{ param }}`
{% endfor %}
{% endif %}

## Returns

`{{ returns }}`
"#
    }

    fn struct_template() -> &'static str {
        r#"# Struct: {{ name }}

{% if documentation %}
## Description

{{ documentation }}
{% endif %}

## Fields

{% if fields %}
{% for field in fields %}
- `{{ field }}`
{% endfor %}
{% else %}
_No fields documented_
{% endif %}
"#
    }

    fn module_template() -> &'static str {
        r#"# Module: {{ name }}

{% if documentation %}
## Overview

{{ documentation }}
{% endif %}

## Contents

{% if contents %}
{% for item in contents %}
- {{ item }}
{% endfor %}
{% else %}
_Module contents not yet documented_
{% endif %}
"#
    }

    fn trait_template() -> &'static str {
        r#"# Trait: {{ name }}

{% if documentation %}
## Description

{{ documentation }}
{% endif %}

## Methods

_Methods will be documented here_
"#
    }

    fn enum_template() -> &'static str {
        r#"# Enum: {{ name }}

{% if documentation %}
## Description

{{ documentation }}
{% endif %}

## Variants

_Variants will be documented here_
"#
    }

    fn overview_template() -> &'static str {
        r#"# Codebase Documentation Overview

## Statistics

- **Total Entities**: {{ total_entities }}
- **Documented Entities**: {{ documented_entities }}
- **Documentation Coverage**: {{ coverage_percentage | round(precision=2) }}%

## Summary

This documentation was automatically generated from the codebase semantic ontology.
It provides a comprehensive view of the code structure, relationships, and documentation status.
"#
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_narrative_generator_creation() {
        let config = NarrativeConfig::default();
        let generator = NarrativeGenerator::new(&config);
        assert!(generator.is_ok());
    }

    #[test]
    fn test_function_narrative() {
        let config = NarrativeConfig::default();
        let generator = NarrativeGenerator::new(&config).unwrap();

        let params = vec!["x: i32".to_string(), "y: i32".to_string()];
        let narrative = generator.generate_function_narrative(
            "add",
            Some("Adds two numbers"),
            &params,
            Some("i32")
        );

        assert!(narrative.is_ok());
        let text = narrative.unwrap();
        assert!(text.contains("add"));
        assert!(text.contains("Adds two numbers"));
    }
}
