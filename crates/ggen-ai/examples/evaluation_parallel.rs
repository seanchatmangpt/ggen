//! Example: Parallel Evaluation with Multi-Threading
//!
//! Demonstrates parallel evaluation for performance optimization.
//!
//! Run with: cargo run --example evaluation_parallel
//!
//! Shows:
//! - Multi-threaded evaluation
//! - Performance comparison (sequential vs parallel)
//! - Progress tracking
//! - Export to CSV/JSON

use ggen_ai::dspy::{
    evaluation::{exact_match, Evaluate},
    Example, InputField, Module, ModuleError, OutputField, Signature,
};
use serde_json::json;
use std::collections::HashMap;
use std::time::Instant;
use tempfile::TempDir;

// Fast mock module for performance testing
struct FastMockQA {
    sig: Signature,
}

#[async_trait::async_trait]
impl Module for FastMockQA {
    fn signature(&self) -> &Signature {
        &self.sig
    }

    async fn forward(
        &self,
        inputs: HashMap<String, serde_json::Value>,
    ) -> Result<HashMap<String, serde_json::Value>, ModuleError> {
        // Simulate some processing time
        tokio::time::sleep(tokio::time::Duration::from_millis(10)).await;

        let mut outputs = HashMap::new();
        if let Some(q) = inputs.get("question").and_then(|v| v.as_str()) {
            // Simple pattern matching
            let answer = if q.contains("2+2") {
                "4"
            } else if q.contains("capital") {
                "Paris"
            } else {
                "Unknown"
            };
            outputs.insert("answer".to_string(), json!(answer));
        }
        Ok(outputs)
    }
}

fn create_large_dataset(size: usize) -> Vec<Example> {
    (0..size)
        .map(|i| {
            Example::new(
                [(
                    "question".to_string(),
                    json!(format!("What is {}+{}?", i, i)),
                )]
                .into(),
                [("answer".to_string(), json!(format!("{}", i * 2)))].into(),
            )
        })
        .collect()
}

#[tokio::main]
async fn main() -> Result<(), Box<dyn std::error::Error>> {
    tracing_subscriber::fmt()
        .with_max_level(tracing::Level::WARN)
        .init();

    println!("=== Parallel Evaluation Example ===\n");

    let signature = Signature::new("QA", "Answer questions")
        .with_input(InputField::new("question", "Question", "String"))
        .with_output(OutputField::new("answer", "Answer", "String"));

    let program = FastMockQA { sig: signature };
    let metric = exact_match("answer");

    // Test with different dataset sizes
    for size in [50, 100, 200] {
        println!("=== Dataset Size: {} examples ===", size);
        let devset = create_large_dataset(size);

        // Sequential evaluation (1 thread)
        println!("\n1. Sequential Evaluation (1 thread)");
        let evaluator = Evaluate::new(devset.clone()).with_metric(metric.clone()).with_num_threads(1);

        let start = Instant::now();
        let result = evaluator.evaluate(&program, None).await?;
        let seq_time = start.elapsed();

        println!("   Time: {:.2}s", seq_time.as_secs_f64());
        println!("   Score: {:.2}%", result.score);

        // Parallel evaluation (4 threads)
        println!("\n2. Parallel Evaluation (4 threads)");
        let evaluator = Evaluate::new(devset.clone())
            .with_metric(metric.clone())
            .with_num_threads(4)
            .with_display_progress(true);

        let start = Instant::now();
        let result = evaluator.evaluate(&program, None).await?;
        let par4_time = start.elapsed();

        println!("   Time: {:.2}s", par4_time.as_secs_f64());
        println!("   Score: {:.2}%", result.score);
        println!(
            "   Speedup: {:.2}x",
            seq_time.as_secs_f64() / par4_time.as_secs_f64()
        );

        // Parallel evaluation (8 threads)
        println!("\n3. Parallel Evaluation (8 threads)");
        let evaluator = Evaluate::new(devset.clone())
            .with_metric(metric.clone())
            .with_num_threads(8)
            .with_display_progress(true);

        let start = Instant::now();
        let result = evaluator.evaluate(&program, None).await?;
        let par8_time = start.elapsed();

        println!("   Time: {:.2}s", par8_time.as_secs_f64());
        println!("   Score: {:.2}%", result.score);
        println!(
            "   Speedup: {:.2}x",
            seq_time.as_secs_f64() / par8_time.as_secs_f64()
        );

        println!();
    }

    // Export demonstration
    println!("=== Export Demonstration ===\n");
    let temp_dir = TempDir::new()?;
    let csv_path = temp_dir.path().join("results.csv");
    let json_path = temp_dir.path().join("results.json");

    let devset = create_large_dataset(20);
    let evaluator = Evaluate::new(devset)
        .with_metric(metric)
        .with_num_threads(4)
        .save_as_csv(&csv_path)
        .save_as_json(&json_path);

    let result = evaluator.evaluate(&program, None).await?;

    println!("Exported results to:");
    println!("  CSV:  {}", csv_path.display());
    println!("  JSON: {}", json_path.display());
    println!("\nScore: {:.2}%", result.score);

    println!("\n=== Example Complete ===");
    println!("\nKey Takeaways:");
    println!("- Parallel evaluation provides significant speedup for I/O-bound operations");
    println!("- Optimal thread count depends on workload characteristics");
    println!("- Progress tracking helps monitor long-running evaluations");
    println!("- Export capabilities enable post-evaluation analysis");

    Ok(())
}
