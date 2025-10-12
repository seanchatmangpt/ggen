//! Basic ggen library usage example
//!
//! This example demonstrates the fundamental operations with ggen-core:
//! - Loading templates from files and strings
//! - Creating generation context
//! - Rendering templates with variables
//! - Error handling best practices

use anyhow::{Context, Result};
use ggen_core::{GenContext, Generator, Template};
use std::collections::HashMap;
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
name: simple-template
description: A simple hello world template
version: 1.0.0
---
Hello, {{ name }}!
Welcome to {{ project }}.
"#;

    let template = Template::from_str(template_content)
        .context("Failed to parse template from string")?;

    info!("Template name: {}", template.metadata.name);
    info!("Template description: {:?}", template.metadata.description);

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
name: file-template
description: Template loaded from file
version: 1.0.0
---
# {{ title }}

This is a template loaded from: {{ filename }}
"#,
    )
    .context("Failed to create sample template file")?;

    let template = Template::from_file(sample_path)
        .context("Failed to load template from file")?;

    info!("Loaded template: {}", template.metadata.name);

    // Clean up
    std::fs::remove_file(sample_path).ok();

    Ok(())
}

/// Example 3: Use Generator with GenContext
async fn example_generator_with_context() -> Result<()> {
    info!("=== Example 3: Generator with Context ===");

    let template_content = r#"---
name: user-greeting
description: User greeting template
---
Hello {{ user_name }}!
Your role is: {{ user_role }}
Email: {{ email }}
"#;

    let template = Template::from_str(template_content)?;

    // Create generation context
    let mut context = GenContext::new();
    context.insert("user_name", "Alice");
    context.insert("user_role", "Developer");
    context.insert("email", "alice@example.com");

    // Create generator
    let generator = Generator::new(vec![], HashMap::new())
        .context("Failed to create generator")?;

    // Generate output
    let output = generator
        .generate(&template, &context)
        .await
        .context("Failed to generate output")?;

    info!("Generated output:\n{}", output);

    Ok(())
}

/// Example 4: Template with multiple variables and formatting
async fn example_multiple_variables() -> Result<()> {
    info!("=== Example 4: Multiple Variables ===");

    let template_content = r#"---
name: project-structure
description: Project structure template
---
# {{ project_name }}

## Project Information
- Name: {{ project_name }}
- Version: {{ version }}
- Author: {{ author }}
- License: {{ license }}

## Description
{{ description }}

## Dependencies
{% for dep in dependencies -%}
- {{ dep }}
{% endfor %}
"#;

    let template = Template::from_str(template_content)?;

    let mut context = GenContext::new();
    context.insert("project_name", "awesome-app");
    context.insert("version", "1.0.0");
    context.insert("author", "The Team");
    context.insert("license", "MIT");
    context.insert("description", "An awesome application built with ggen");

    // Note: For array/list support, you'd need to use the template engine's
    // native context. This is a simplified example.
    let generator = Generator::new(vec![], HashMap::new())?;
    let output = generator.generate(&template, &context).await?;

    info!("Generated output:\n{}", output);

    Ok(())
}

/// Example 5: Conditional rendering in templates
async fn example_conditional_rendering() -> Result<()> {
    info!("=== Example 5: Conditional Rendering ===");

    let template_content = r#"---
name: conditional-template
description: Template with conditional logic
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
"#;

    let template = Template::from_str(template_content)?;

    let mut context = GenContext::new();
    context.insert("project_name", "my-library");
    context.insert("is_public", "true");
    context.insert("repo_url", "https://github.com/user/my-library");
    context.insert("has_tests", "true");
    context.insert("has_docs", "true");

    let generator = Generator::new(vec![], HashMap::new())?;
    let output = generator.generate(&template, &context).await?;

    info!("Generated output:\n{}", output);

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
        let result = Template::from_str(invalid_template);
        assert!(result.is_err());
    }
}
