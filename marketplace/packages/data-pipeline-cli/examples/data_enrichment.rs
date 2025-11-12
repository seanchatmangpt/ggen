//! Example: Data enrichment pipeline with multiple sources
//!
//! This example demonstrates how to:
//! 1. Load customer data from CSV
//! 2. Enrich with geocoding API data
//! 3. Join with transaction database
//! 4. Aggregate and write to RDF store

use anyhow::Result;
use data_pipeline_cli::{Pipeline, Source, Transform, Sink};

#[tokio::main]
async fn main() -> Result<()> {
    println!("Data Enrichment Pipeline Example");
    println!("=================================\n");

    // Create multi-source pipeline
    let pipeline = Pipeline::builder()
        .name("customer-enrichment")
        .description("Enrich customer data with location and transactions")

        // Primary source: Customer CSV
        .source(
            Source::csv("examples/data/customers.csv")?
                .with_name("customers")
        )

        // Secondary source: Geocoding API
        .source(
            Source::api("https://geocode.example.com/api")?
                .with_name("geocoding")
        )

        // Tertiary source: Transaction database
        .source(
            Source::sql("postgresql://localhost/sales")?
                .with_name("transactions")
                .with_query("SELECT * FROM transactions WHERE date >= NOW() - INTERVAL '1 year'")
        )

        // Transformation 1: Map customer fields
        .transform(Transform::map(vec![
            ("customer_id", "http://schema.org/identifier"),
            ("name", "http://xmlns.com/foaf/0.1/name"),
            ("email", "http://xmlns.com/foaf/0.1/mbox"),
            ("address", "http://schema.org/address"),
        ])?)

        // Transformation 2: Enrich with geocoding
        .transform(Transform::enrich(
            "geocoding",
            vec![
                ("address", "query"),
            ],
            vec![
                ("lat", "http://www.w3.org/2003/01/geo/wgs84_pos#lat"),
                ("lon", "http://www.w3.org/2003/01/geo/wgs84_pos#long"),
            ]
        )?)

        // Transformation 3: Join with transactions
        .transform(Transform::join(
            "customers",
            "transactions",
            "inner",
            "customers.customer_id = transactions.customer_id"
        )?)

        // Transformation 4: Aggregate transaction data
        .transform(Transform::aggregate(
            vec!["customer_id", "name", "email"],
            vec![
                "SUM(amount) as total_spent",
                "COUNT(*) as transaction_count",
                "AVG(amount) as avg_transaction",
                "MAX(amount) as largest_purchase",
            ]
        )?)

        // Transformation 5: Validate enriched data
        .transform(Transform::validate(vec![
            "total_spent > 0",
            "lat >= -90 AND lat <= 90",
            "lon >= -180 AND lon <= 180",
        ])?)

        // Sink: Write to RDF store
        .sink(Sink::rdf("oxigraph://output/enriched_customers.db")?)

        // Sink: Write summary to CSV
        .sink(Sink::csv("output/customer_summary.csv")?)

        .batch_size(500)
        .parallelism(4)
        .enable_checkpointing(true)
        .build()?;

    println!("Pipeline Configuration:");
    println!("  Name: {}", pipeline.name());
    println!("  Sources: CSV + API + SQL");
    println!("  Transforms: Map, Enrich, Join, Aggregate, Validate");
    println!("  Sinks: RDF store + CSV file");
    println!("  Batch size: 500");
    println!("  Parallelism: 4 workers\n");

    // Execute pipeline
    println!("Executing pipeline...");
    let result = pipeline.run().await?;

    println!("\nPipeline Results:");
    println!("  Customers processed: {}", result.total_records);
    println!("  Geocoding API calls: {}", result.api_calls);
    println!("  Transaction records joined: {}", result.joined_records);
    println!("  Successfully enriched: {}", result.successful_records);
    println!("  Validation failures: {}", result.validation_errors);
    println!("  Duration: {:.2}s", result.duration_seconds);
    println!("  Throughput: {:.0} records/sec", result.throughput());

    // Display sample enriched customer
    if let Some(sample) = result.sample_record {
        println!("\nSample Enriched Customer:");
        println!("{:#?}", sample);
    }

    Ok(())
}
