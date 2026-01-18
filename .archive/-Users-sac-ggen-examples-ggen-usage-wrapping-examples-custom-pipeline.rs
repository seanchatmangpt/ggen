//! Custom pipeline creation example
//!
//! This example demonstrates advanced pipeline operations:
//! - Building multi-stage pipelines
//! - Chaining template transformations
//! - Pipeline error recovery
//! - Output organization and post-processing

use anyhow::{Context, Result};
use ggen_core::{GenContext, Generator, Pipeline, PipelineBuilder, Template};
use std::collections::HashMap;
use std::path::PathBuf;
use tracing::{info, warn, Level};
use tracing_subscriber::FmtSubscriber;

#[tokio::main]
async fn main() -> Result<()> {
    // Initialize logging
    let subscriber = FmtSubscriber::builder()
        .with_max_level(Level::INFO)
        .finish();
    tracing::subscriber::set_global_default(subscriber)?;

    info!("Starting custom pipeline example");

    // Create temporary workspace
    let workspace = setup_workspace()?;

    // Example 1: Simple linear pipeline
    example_linear_pipeline(&workspace).await?;

    // Example 2: Multi-stage pipeline with dependencies
    example_multistage_pipeline(&workspace).await?;

    // Example 3: Conditional pipeline execution
    example_conditional_pipeline(&workspace).await?;

    // Example 4: Pipeline with custom processors
    example_custom_processors(&workspace).await?;

    // Example 5: Error handling and recovery
    example_error_handling(&workspace).await?;

    // Cleanup
    cleanup_workspace(&workspace)?;

    info!("All pipeline examples completed successfully!");
    Ok(())
}

/// Example 1: Simple linear pipeline
async fn example_linear_pipeline(workspace: &PathBuf) -> Result<()> {
    info!("=== Example 1: Linear Pipeline ===");

    // Create template files
    let template1_path = workspace.join("template1.md");
    std::fs::write(
        &template1_path,
        r#"---
name: project-setup
description: Project initialization
---
# {{ project_name }}

## Setup
Project initialized with name: {{ project_name }}
"#,
    )?;

    let template2_path = workspace.join("template2.md");
    std::fs::write(
        &template2_path,
        r#"---
name: project-readme
description: README generation
---
# {{ project_name }}

## Description
{{ description }}

## Installation
npm install {{ project_name }}
"#,
    )?;

    // Build pipeline
    let pipeline = PipelineBuilder::new()
        .with_template(template1_path.to_str().unwrap())
        .with_template(template2_path.to_str().unwrap())
        .with_output_dir(workspace.join("output").to_str().unwrap())
        .build()
        .context("Failed to build linear pipeline")?;

    // Create context
    let mut context = GenContext::new();
    context.insert("project_name", "awesome-lib");
    context.insert("description", "An awesome library created with ggen");

    // Execute pipeline
    info!("Executing linear pipeline...");
    pipeline
        .execute(&context)
        .await
        .context("Pipeline execution failed")?;

    info!("Linear pipeline completed successfully");
    Ok(())
}

/// Example 2: Multi-stage pipeline with dependencies
async fn example_multistage_pipeline(workspace: &PathBuf) -> Result<()> {
    info!("=== Example 2: Multi-stage Pipeline ===");

    // Stage 1: Generate core files
    let core_template = workspace.join("core.md");
    std::fs::write(
        &core_template,
        r#"---
name: core-module
description: Core module template
---
// Core module: {{ module_name }}
export const VERSION = '{{ version }}';
"#,
    )?;

    // Stage 2: Generate API layer
    let api_template = workspace.join("api.md");
    std::fs::write(
        &api_template,
        r#"---
name: api-module
description: API module template
---
// API module for {{ module_name }}
import { VERSION } from './core';

export const API_VERSION = VERSION;
export const BASE_URL = '{{ base_url }}';
"#,
    )?;

    // Stage 3: Generate tests
    let test_template = workspace.join("test.md");
    std::fs::write(
        &test_template,
        r#"---
name: test-module
description: Test module template
---
// Tests for {{ module_name }}
describe('{{ module_name }}', () => {
  it('should export VERSION', () => {
    expect(VERSION).toBeDefined();
  });
});
"#,
    )?;

    // Build multi-stage pipeline
    let output_dir = workspace.join("multi-output");
    std::fs::create_dir_all(&output_dir)?;

    let pipeline = PipelineBuilder::new()
        .with_template(core_template.to_str().unwrap())
        .with_template(api_template.to_str().unwrap())
        .with_template(test_template.to_str().unwrap())
        .with_output_dir(output_dir.to_str().unwrap())
        .build()?;

    let mut context = GenContext::new();
    context.insert("module_name", "user-service");
    context.insert("version", "1.0.0");
    context.insert("base_url", "https://api.example.com");

    info!("Executing multi-stage pipeline...");
    pipeline.execute(&context).await?;

    info!("Multi-stage pipeline completed");
    Ok(())
}

/// Example 3: Conditional pipeline execution
async fn example_conditional_pipeline(workspace: &PathBuf) -> Result<()> {
    info!("=== Example 3: Conditional Pipeline ===");

    // Create templates with conditions
    let conditional_template = workspace.join("conditional.md");
    std::fs::write(
        &conditional_template,
        r#"---
name: conditional-template
description: Template with conditional logic
---
# {{ project_name }}

{% if include_tests %}
## Testing
Tests are included in this project.
Run: npm test
{% endif %}

{% if include_docs %}
## Documentation
Documentation is available in /docs
{% endif %}

{% if is_public %}
## License
This project is open source under {{ license }}.
{% endif %}
"#,
    )?;

    let pipeline = PipelineBuilder::new()
        .with_template(conditional_template.to_str().unwrap())
        .with_output_dir(workspace.join("conditional-output").to_str().unwrap())
        .build()?;

    // Scenario 1: Full features
    let mut context1 = GenContext::new();
    context1.insert("project_name", "full-featured-app");
    context1.insert("include_tests", "true");
    context1.insert("include_docs", "true");
    context1.insert("is_public", "true");
    context1.insert("license", "MIT");

    info!("Scenario 1: Full featured application");
    pipeline.execute(&context1).await?;

    // Scenario 2: Minimal features
    let mut context2 = GenContext::new();
    context2.insert("project_name", "minimal-app");
    context2.insert("include_tests", "false");
    context2.insert("include_docs", "false");
    context2.insert("is_public", "false");

    info!("Scenario 2: Minimal application");
    pipeline.execute(&context2).await?;

    info!("Conditional pipeline completed");
    Ok(())
}

/// Example 4: Pipeline with custom processors
async fn example_custom_processors(workspace: &PathBuf) -> Result<()> {
    info!("=== Example 4: Custom Processors ===");

    // Create a template
    let template_path = workspace.join("processor.md");
    std::fs::write(
        &template_path,
        r#"---
name: processor-template
description: Template for custom processing
---
# {{ title | upper }}

Content: {{ content }}
Timestamp: {{ timestamp }}
"#,
    )?;

    // In a real implementation, you would add custom Tera filters/functions
    // For demonstration, we'll show the concept
    info!("Custom processors can be added to the Tera environment");
    info!("Examples: upper, lower, capitalize, date formatting, etc.");

    let template = Template::from_file(&template_path)?;
    let mut context = GenContext::new();
    context.insert("title", "custom processing example");
    context.insert("content", "This content will be processed");
    context.insert("timestamp", "2024-01-01T12:00:00Z");

    let generator = Generator::new(vec![], HashMap::new())?;
    let output = generator.generate(&template, &context).await?;

    info!("Processed output:\n{}", output);

    Ok(())
}

/// Example 5: Error handling and recovery
async fn example_error_handling(workspace: &PathBuf) -> Result<()> {
    info!("=== Example 5: Error Handling ===");

    // Create a template with potential error
    let error_template = workspace.join("error.md");
    std::fs::write(
        &error_template,
        r#"---
name: error-template
description: Template that might fail
---
# {{ required_field }}

This requires: {{ another_required_field }}
"#,
    )?;

    // Attempt 1: Missing required fields (should fail gracefully)
    info!("Attempt 1: Missing required fields");
    let template = Template::from_file(&error_template)?;
    let context = GenContext::new(); // Empty context

    let generator = Generator::new(vec![], HashMap::new())?;

    match generator.generate(&template, &context).await {
        Ok(output) => {
            warn!("Generated with missing fields:\n{}", output);
        }
        Err(e) => {
            warn!("Expected error: {}", e);
        }
    }

    // Attempt 2: With all required fields
    info!("Attempt 2: With all required fields");
    let mut valid_context = GenContext::new();
    valid_context.insert("required_field", "Value 1");
    valid_context.insert("another_required_field", "Value 2");

    let output = generator.generate(&template, &valid_context).await?;
    info!("Successfully generated:\n{}", output);

    // Attempt 3: Invalid template syntax
    info!("Attempt 3: Invalid template syntax");
    let invalid_template = r#"---
name: invalid
---
{% if unclosed
"#;

    match Template::from_str(invalid_template) {
        Ok(_) => warn!("Unexpectedly parsed invalid template"),
        Err(e) => info!("Correctly caught template error: {}", e),
    }

    Ok(())
}

/// Setup temporary workspace for examples
fn setup_workspace() -> Result<PathBuf> {
    let workspace = PathBuf::from("/tmp/ggen-pipeline-examples");
    std::fs::create_dir_all(&workspace).context("Failed to create workspace")?;
    info!("Created workspace: {}", workspace.display());
    Ok(workspace)
}

/// Cleanup temporary workspace
fn cleanup_workspace(workspace: &PathBuf) -> Result<()> {
    if workspace.exists() {
        std::fs::remove_dir_all(workspace).context("Failed to cleanup workspace")?;
        info!("Cleaned up workspace");
    }
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[tokio::test]
    async fn test_linear_pipeline() {
        let workspace = setup_workspace().unwrap();
        let result = example_linear_pipeline(&workspace).await;
        cleanup_workspace(&workspace).ok();
        assert!(result.is_ok());
    }

    #[tokio::test]
    async fn test_error_handling() {
        let workspace = setup_workspace().unwrap();
        let result = example_error_handling(&workspace).await;
        cleanup_workspace(&workspace).ok();
        assert!(result.is_ok());
    }
}
