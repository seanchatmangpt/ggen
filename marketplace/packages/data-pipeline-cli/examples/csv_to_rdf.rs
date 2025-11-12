//! Example: CSV to RDF conversion pipeline
//!
//! This example demonstrates how to:
//! 1. Read data from a CSV file
//! 2. Map CSV fields to RDF properties
//! 3. Write RDF triples to an Oxigraph store

use anyhow::Result;
use data_pipeline_cli::{Pipeline, Source, Transform, Sink};

#[tokio::main]
async fn main() -> Result<()> {
    println!("CSV to RDF Pipeline Example");
    println!("============================\n");

    // Create pipeline
    let pipeline = Pipeline::builder()
        .name("csv-to-rdf-example")
        .description("Convert user CSV data to RDF triples")
        .source(Source::csv("examples/data/users.csv")?)
        .transform(Transform::map(vec![
            ("firstName", "http://xmlns.com/foaf/0.1/givenName"),
            ("lastName", "http://xmlns.com/foaf/0.1/familyName"),
            ("email", "http://xmlns.com/foaf/0.1/mbox"),
            ("age", "http://xmlns.com/foaf/0.1/age"),
        ])?)
        .transform(Transform::filter(
            "age >= 18 AND email IS NOT NULL"
        )?)
        .sink(Sink::rdf("oxigraph://output/users.db")?)
        .batch_size(1000)
        .parallelism(4)
        .enable_checkpointing(true)
        .build()?;

    println!("Pipeline Configuration:");
    println!("  Name: {}", pipeline.name());
    println!("  Source: CSV file");
    println!("  Transforms: Map fields, Filter adults");
    println!("  Sink: Oxigraph RDF store");
    println!("  Batch size: 1000");
    println!("  Parallelism: 4 workers\n");

    // Execute pipeline
    println!("Executing pipeline...");
    let result = pipeline.run().await?;

    println!("\nPipeline Results:");
    println!("  Total records processed: {}", result.total_records);
    println!("  Successful: {}", result.successful_records);
    println!("  Filtered: {}", result.filtered_records);
    println!("  Errors: {}", result.error_count);
    println!("  Duration: {:.2}s", result.duration_seconds);
    println!("  Throughput: {:.0} records/sec", result.throughput());

    Ok(())
}
