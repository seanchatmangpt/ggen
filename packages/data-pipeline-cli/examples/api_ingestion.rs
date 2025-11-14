//! Example: REST API data ingestion pipeline
//!
//! This example demonstrates how to:
//! 1. Ingest data from a REST API
//! 2. Transform and enrich the data
//! 3. Write to multiple sinks (RDF and JSON)

use anyhow::Result;
use data_pipeline_cli::{Pipeline, Source, Transform, Sink};

#[tokio::main]
async fn main() -> Result<()> {
    println!("API Ingestion Pipeline Example");
    println!("===============================\n");

    // Create pipeline with API source
    let pipeline = Pipeline::builder()
        .name("github-users-ingestion")
        .description("Ingest GitHub user data via API")
        .source(
            Source::api("https://api.github.com/users")
                .with_pagination("page", 1, 10)
                .with_rate_limit(5000, std::time::Duration::from_secs(3600))
                .with_auth_token("ghp_example_token")?
        )
        .transform(Transform::map(vec![
            ("login", "http://schema.org/identifier"),
            ("name", "http://xmlns.com/foaf/0.1/name"),
            ("email", "http://xmlns.com/foaf/0.1/mbox"),
            ("bio", "http://schema.org/description"),
            ("location", "http://schema.org/location"),
        ])?)
        .transform(Transform::filter(
            "email IS NOT NULL AND location IS NOT NULL"
        )?)
        .transform(Transform::validate(vec![
            "email matches '^[^@]+@[^@]+\\.[^@]+$'",
        ])?)
        .sink(Sink::rdf("oxigraph://output/github_users.db")?)
        .sink(Sink::json("output/github_users.jsonl")?)
        .batch_size(100)
        .parallelism(2)
        .build()?;

    println!("Pipeline Configuration:");
    println!("  Name: {}", pipeline.name());
    println!("  Source: GitHub API");
    println!("  Transforms: Map, Filter, Validate");
    println!("  Sinks: RDF store + JSON file");
    println!("  Batch size: 100");
    println!("  Rate limit: 5000 requests/hour\n");

    // Execute pipeline
    println!("Executing pipeline...");
    let result = pipeline.run().await?;

    println!("\nPipeline Results:");
    println!("  Total API calls: {}", result.api_calls);
    println!("  Total records: {}", result.total_records);
    println!("  Successful: {}", result.successful_records);
    println!("  Filtered: {}", result.filtered_records);
    println!("  Validation errors: {}", result.validation_errors);
    println!("  Duration: {:.2}s", result.duration_seconds);
    println!("  Throughput: {:.0} records/sec", result.throughput());

    Ok(())
}
