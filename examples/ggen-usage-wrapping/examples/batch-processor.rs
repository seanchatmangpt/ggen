//! Batch processing example
//!
//! This example demonstrates parallel template processing:
//! - Processing multiple templates concurrently with Tokio
//! - Progress tracking and reporting
//! - Result aggregation and error handling
//! - Performance optimization strategies

use anyhow::{Context, Result};
use ggen_core::{GenContext, Generator, Template};
use std::collections::HashMap;
use std::sync::Arc;
use std::time::Instant;
use tokio::sync::Semaphore;
use tokio::task;
use tracing::{info, warn, Level};
use tracing_subscriber::FmtSubscriber;

#[tokio::main]
async fn main() -> Result<()> {
    // Initialize logging
    let subscriber = FmtSubscriber::builder()
        .with_max_level(Level::INFO)
        .finish();
    tracing::subscriber::set_global_default(subscriber)?;

    info!("Starting batch processing example");

    // Example 1: Simple parallel processing
    example_simple_parallel().await?;

    // Example 2: Concurrent processing with semaphore (rate limiting)
    example_rate_limited_processing().await?;

    // Example 3: Batch processing with progress tracking
    example_progress_tracking().await?;

    // Example 4: Error-tolerant batch processing
    example_error_tolerant_batch().await?;

    // Example 5: Processing with shared resources
    example_shared_resources().await?;

    info!("All batch processing examples completed!");
    Ok(())
}

/// Example 1: Simple parallel processing
async fn example_simple_parallel() -> Result<()> {
    info!("=== Example 1: Simple Parallel Processing ===");

    // Create multiple templates
    let templates = create_sample_templates(5);

    // Create context
    let context = create_sample_context();

    // Process templates in parallel
    let start = Instant::now();

    let tasks: Vec<_> = templates
        .into_iter()
        .enumerate()
        .map(|(i, template)| {
            let ctx = context.clone();
            task::spawn(async move {
                info!("Processing template {}", i);
                process_template(template, ctx).await
            })
        })
        .collect();

    let results = futures::future::join_all(tasks).await;

    let duration = start.elapsed();

    // Collect successful results
    let successful: Vec<_> = results
        .into_iter()
        .filter_map(|r| r.ok().and_then(|inner| inner.ok()))
        .collect();

    info!(
        "Processed {} templates in {:?}",
        successful.len(),
        duration
    );

    Ok(())
}

/// Example 2: Rate-limited concurrent processing
async fn example_rate_limited_processing() -> Result<()> {
    info!("=== Example 2: Rate-Limited Processing ===");

    let templates = create_sample_templates(20);
    let context = create_sample_context();

    // Limit to 5 concurrent operations
    let semaphore = Arc::new(Semaphore::new(5));
    let start = Instant::now();

    let tasks: Vec<_> = templates
        .into_iter()
        .enumerate()
        .map(|(i, template)| {
            let sem = Arc::clone(&semaphore);
            let ctx = context.clone();

            task::spawn(async move {
                let _permit = sem.acquire().await.unwrap();
                info!("Processing template {} (rate limited)", i);
                process_template(template, ctx).await
            })
        })
        .collect();

    let results = futures::future::join_all(tasks).await;
    let duration = start.elapsed();

    let successful: Vec<_> = results
        .into_iter()
        .filter_map(|r| r.ok().and_then(|inner| inner.ok()))
        .collect();

    info!(
        "Processed {} templates with rate limiting in {:?}",
        successful.len(),
        duration
    );

    Ok(())
}

/// Example 3: Progress tracking
async fn example_progress_tracking() -> Result<()> {
    info!("=== Example 3: Progress Tracking ===");

    let templates = create_sample_templates(10);
    let total = templates.len();
    let context = create_sample_context();

    let completed = Arc::new(std::sync::atomic::AtomicUsize::new(0));

    let tasks: Vec<_> = templates
        .into_iter()
        .map(|template| {
            let ctx = context.clone();
            let completed_counter = Arc::clone(&completed);

            task::spawn(async move {
                let result = process_template(template, ctx).await;

                // Update progress
                let count = completed_counter.fetch_add(1, std::sync::atomic::Ordering::SeqCst) + 1;
                let progress = (count as f64 / total as f64) * 100.0;
                info!("Progress: {:.1}% ({}/{})", progress, count, total);

                result
            })
        })
        .collect();

    let results = futures::future::join_all(tasks).await;

    let successful: Vec<_> = results
        .into_iter()
        .filter_map(|r| r.ok().and_then(|inner| inner.ok()))
        .collect();

    info!("Completed: {}/{} templates", successful.len(), total);

    Ok(())
}

/// Example 4: Error-tolerant batch processing
async fn example_error_tolerant_batch() -> Result<()> {
    info!("=== Example 4: Error-Tolerant Batch Processing ===");

    // Mix of valid and invalid templates
    let mut templates = create_sample_templates(5);

    // Add an intentionally broken template
    templates.push(Template::from_str(
        r#"---
name: broken
---
{{ undefined_variable | non_existent_filter }}
"#,
    )?);

    let context = create_sample_context();

    let tasks: Vec<_> = templates
        .into_iter()
        .enumerate()
        .map(|(i, template)| {
            let ctx = context.clone();
            task::spawn(async move {
                match process_template(template, ctx).await {
                    Ok(output) => {
                        info!("✓ Template {} succeeded", i);
                        Ok(output)
                    }
                    Err(e) => {
                        warn!("✗ Template {} failed: {}", i, e);
                        Err(e)
                    }
                }
            })
        })
        .collect();

    let results = futures::future::join_all(tasks).await;

    let (successes, failures): (Vec<_>, Vec<_>) = results
        .into_iter()
        .filter_map(|r| r.ok())
        .partition(|r| r.is_ok());

    info!(
        "Batch complete: {} succeeded, {} failed",
        successes.len(),
        failures.len()
    );

    Ok(())
}

/// Example 5: Processing with shared resources
async fn example_shared_resources() -> Result<()> {
    info!("=== Example 5: Shared Resources ===");

    let templates = create_sample_templates(8);
    let context = create_sample_context();

    // Shared generator (wrapped in Arc for thread safety)
    let generator = Arc::new(Generator::new(vec![], HashMap::new())?);

    let tasks: Vec<_> = templates
        .into_iter()
        .enumerate()
        .map(|(i, template)| {
            let gen = Arc::clone(&generator);
            let ctx = context.clone();

            task::spawn(async move {
                info!("Processing with shared generator: template {}", i);
                gen.generate(&template, &ctx).await
            })
        })
        .collect();

    let results = futures::future::join_all(tasks).await;

    let successful: Vec<_> = results
        .into_iter()
        .filter_map(|r| r.ok().and_then(|inner| inner.ok()))
        .collect();

    info!("Processed {} templates with shared generator", successful.len());

    Ok(())
}

/// Helper function to process a single template
async fn process_template(template: Template, context: GenContext) -> Result<String> {
    let generator = Generator::new(vec![], HashMap::new())?;

    // Simulate some processing time
    tokio::time::sleep(tokio::time::Duration::from_millis(100)).await;

    generator
        .generate(&template, &context)
        .await
        .context("Template processing failed")
}

/// Create sample templates for testing
fn create_sample_templates(count: usize) -> Vec<Template> {
    (0..count)
        .filter_map(|i| {
            Template::from_str(&format!(
                r#"---
name: template-{}
description: Sample template {}
---
# Template {}

Project: {{{{ project_name }}}}
Version: {{{{ version }}}}

This is template number {}.
"#,
                i, i, i, i
            ))
            .ok()
        })
        .collect()
}

/// Create sample context
fn create_sample_context() -> GenContext {
    let mut context = GenContext::new();
    context.insert("project_name", "batch-example");
    context.insert("version", "1.0.0");
    context
}

/// Batch processor with configurable concurrency
struct BatchProcessor {
    max_concurrency: usize,
}

impl BatchProcessor {
    fn new(max_concurrency: usize) -> Self {
        Self { max_concurrency }
    }

    async fn process_batch(
        &self,
        templates: Vec<Template>,
        context: GenContext,
    ) -> Result<Vec<String>> {
        let semaphore = Arc::new(Semaphore::new(self.max_concurrency));

        let tasks: Vec<_> = templates
            .into_iter()
            .map(|template| {
                let sem = Arc::clone(&semaphore);
                let ctx = context.clone();

                task::spawn(async move {
                    let _permit = sem.acquire().await.unwrap();
                    process_template(template, ctx).await
                })
            })
            .collect();

        let results = futures::future::join_all(tasks).await;

        results
            .into_iter()
            .filter_map(|r| r.ok().and_then(|inner| inner.ok()))
            .collect::<Vec<_>>()
            .into()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[tokio::test]
    async fn test_simple_parallel() {
        let result = example_simple_parallel().await;
        assert!(result.is_ok());
    }

    #[tokio::test]
    async fn test_batch_processor() {
        let processor = BatchProcessor::new(3);
        let templates = create_sample_templates(5);
        let context = create_sample_context();

        let result = processor.process_batch(templates, context).await;
        assert!(result.is_ok());
        assert_eq!(result.unwrap().len(), 5);
    }

    #[tokio::test]
    async fn test_error_tolerant() {
        let result = example_error_tolerant_batch().await;
        assert!(result.is_ok());
    }
}
