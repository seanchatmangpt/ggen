//! Basic ggen library usage example
//!
//! This example demonstrates the fundamental operations with ggen-core:
//! - Loading templates from files and strings
//! - Creating generation context
//! - Rendering templates with variables
//! - Error handling best practices

use anyhow::{Context, Result};
use ggen_core::{GenContext, Generator, Pipeline, Template};
use std::collections::BTreeMap;
use std::path::PathBuf;
use tracing::{info, Level};
use tracing_subscriber::FmtSubscriber;

#[tokio::main]
async fn main() -> Result<()> {
    // Initialize logging
    let subscriber = FmtSubscriber::builder()
        .with_max_level(Level::INFO)
        .finish();
    tracing::subscriber::set_global_default(subscriber)?;

    info!("Starting ggen basic usage example");

    // Example 1: Create template from string
    example_template_from_string()?;

    // Example 2: Load template from file (if available)
    if let Err(e) = example_template_from_file().await {
        info!("Template from file example skipped: {}", e);
    }

    // Example 3: Use Generator with context
    example_generator_with_context().await?;

    // Example 4: Multiple variable substitution
    example_multiple_variables().await?;

    // Example 5: Conditional rendering
    example_conditional_rendering().await?;

    info!("All examples completed successfully!");
    Ok(())
}

/// Example 1: Create and render a simple template from a string
fn example_template_from_string() -> Result<()> {
    info!("=== Example 1: Template from String ===");

    let template_content = r#"---
to: "output.txt"
---
Hello, {{ name }}!
Welcome to {{ project }}.
"#;

    let template = Template::parse(template_content)
        .context("Failed to parse template from string")?;

    info!("Template body length: {}", template.body.len());
    info!("Template has frontmatter: {}", template.front.to.is_some());

    Ok(())
}

/// Example 2: Load template from a file
async fn example_template_from_file() -> Result<()> {
    info!("=== Example 2: Template from File ===");

    // Note: This requires a template file to exist
    // Create a sample template for demonstration
    let sample_path = "/tmp/ggen-example-template.md";
    std::fs::write(
        sample_path,
        r#"---
to: "output.md"
---
# {{ title }}

This is a template loaded from: {{ filename }}
"#,
    )
    .context("Failed to create sample template file")?;

    // Read file and parse
    let content = std::fs::read_to_string(sample_path)
        .context("Failed to read template file")?;
    let template = Template::parse(&content)
        .context("Failed to parse template from file")?;

    info!("Loaded template with body length: {}", template.body.len());

    // Clean up
    std::fs::remove_file(sample_path).ok();

    Ok(())
}

/// Example 3: Use Generator with GenContext
async fn example_generator_with_context() -> Result<()> {
    info!("=== Example 3: Generator with Context ===");

    // Create a temporary template file
    let temp_dir = std::env::temp_dir();
    let template_path = temp_dir.join("user-greeting.tmpl");
    std::fs::write(
        &template_path,
        r#"---
to: "greeting.txt"
---
Hello {{ user_name }}!
Your role is: {{ user_role }}
Email: {{ email }}
"#,
    )?;

    // Create pipeline and context
    let pipeline = Pipeline::new()?;
    let mut vars = BTreeMap::new();
    vars.insert("user_name".to_string(), "Alice".to_string());
    vars.insert("user_role".to_string(), "Developer".to_string());
    vars.insert("email".to_string(), "alice@example.com".to_string());

    let ctx = GenContext::new(
        template_path.clone(),
        temp_dir.clone()
    ).with_vars(vars);

    // Create generator
    let mut generator = Generator::new(pipeline, ctx);

    // Generate output
    let output_path = generator.generate()
        .context("Failed to generate output")?;

    info!("Generated output at: {:?}", output_path);

    // Clean up
    std::fs::remove_file(&template_path).ok();
    if output_path.exists() {
        std::fs::remove_file(&output_path).ok();
    }

    Ok(())
}

/// Example 4: Template with multiple variables and formatting
async fn example_multiple_variables() -> Result<()> {
    info!("=== Example 4: Multiple Variables ===");

    let temp_dir = std::env::temp_dir();
    let template_path = temp_dir.join("project-structure.tmpl");
    std::fs::write(
        &template_path,
        r#"---
to: "project.md"
---
# {{ project_name }}

## Project Information
- Name: {{ project_name }}
- Version: {{ version }}
- Author: {{ author }}
- License: {{ license }}

## Description
{{ description }}
"#,
    )?;

    let pipeline = Pipeline::new()?;
    let mut vars = BTreeMap::new();
    vars.insert("project_name".to_string(), "awesome-app".to_string());
    vars.insert("version".to_string(), "1.0.0".to_string());
    vars.insert("author".to_string(), "The Team".to_string());
    vars.insert("license".to_string(), "MIT".to_string());
    vars.insert("description".to_string(), "An awesome application built with ggen".to_string());

    let ctx = GenContext::new(
        template_path.clone(),
        temp_dir.clone()
    ).with_vars(vars);

    let mut generator = Generator::new(pipeline, ctx);
    let output_path = generator.generate()?;

    info!("Generated output at: {:?}", output_path);

    // Clean up
    std::fs::remove_file(&template_path).ok();
    if output_path.exists() {
        std::fs::remove_file(&output_path).ok();
    }

    Ok(())
}

/// Example 5: Conditional rendering in templates
async fn example_conditional_rendering() -> Result<()> {
    info!("=== Example 5: Conditional Rendering ===");

    let temp_dir = std::env::temp_dir();
    let template_path = temp_dir.join("conditional-template.tmpl");
    std::fs::write(
        &template_path,
        r#"---
to: "project-info.md"
---
# {{ project_name }}

{% if is_public %}
This is a public project.
Repository: {{ repo_url }}
{% else %}
This is a private project.
{% endif %}

{% if has_tests %}
✓ Tests included
{% endif %}

{% if has_docs %}
✓ Documentation included
{% endif %}
"#,
    )?;

    let pipeline = Pipeline::new()?;
    let mut vars = BTreeMap::new();
    vars.insert("project_name".to_string(), "my-library".to_string());
    vars.insert("is_public".to_string(), "true".to_string());
    vars.insert("repo_url".to_string(), "https://github.com/user/my-library".to_string());
    vars.insert("has_tests".to_string(), "true".to_string());
    vars.insert("has_docs".to_string(), "true".to_string());

    let ctx = GenContext::new(
        template_path.clone(),
        temp_dir.clone()
    ).with_vars(vars);

    let mut generator = Generator::new(pipeline, ctx);
    let output_path = generator.generate()?;

    info!("Generated output at: {:?}", output_path);

    // Clean up
    std::fs::remove_file(&template_path).ok();
    if output_path.exists() {
        std::fs::remove_file(&output_path).ok();
    }

    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_template_from_string() {
        let result = example_template_from_string();
        assert!(result.is_ok());
    }

    #[tokio::test]
    async fn test_generator_with_context() {
        let result = example_generator_with_context().await;
        assert!(result.is_ok());
    }

    #[tokio::test]
    async fn test_error_handling() {
        // Test invalid template
        let invalid_template = "---\nno closing frontmatter";
        let result = Template::parse(invalid_template);
        assert!(result.is_err());
    }
}
