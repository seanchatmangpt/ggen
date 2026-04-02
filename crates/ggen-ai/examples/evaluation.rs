//! Example: Comprehensive Evaluation Patterns
//!
//! Demonstrates various evaluation strategies:
//! 1. Basic accuracy evaluation
//! 2. Detailed metrics (latency, error rate)
//! 3. Cross-validation
//! 4. Custom metric functions
//!
//! Run with: cargo run --example evaluation
//!
//! Prerequisites:
//! - Set GGEN_LLM_MODEL environment variable
//! - Set appropriate API key

use ggen_ai::dspy::{
    field::{InputField, OutputField},
    module::Module,
    optimizer::{BootstrapFewShot, Example, MetricFn},
    predictor::Predictor,
    signature::Signature,
};
use serde_json::{json, Value};
use std::collections::HashMap;
use std::sync::Arc;
use std::time::Instant;

/// Detailed evaluation metrics
#[derive(Default, Debug)]
struct EvaluationMetrics {
    total: usize,
    correct: usize,
    total_latency_ms: u64,
    errors: Vec<String>,
}

impl EvaluationMetrics {
    fn accuracy(&self) -> f64 {
        if self.total == 0 {
            0.0
        } else {
            self.correct as f64 / self.total as f64
        }
    }

    fn avg_latency_ms(&self) -> u64 {
        if self.total == 0 {
            0
        } else {
            self.total_latency_ms / self.total as u64
        }
    }

    fn error_rate(&self) -> f64 {
        if self.total == 0 {
            0.0
        } else {
            self.errors.len() as f64 / self.total as f64
        }
    }
}

/// Basic evaluation function
async fn evaluate(
    module: &dyn Module, dataset: &[Example], metric: &MetricFn,
) -> Result<f64, Box<dyn std::error::Error>> {
    let mut total = 0.0;

    for example in dataset {
        let output = module.forward(example.inputs.clone()).await?;
        if metric(example, &output)? {
            total += 1.0;
        }
    }

    Ok(total / dataset.len() as f64)
}

/// Detailed evaluation with metrics
async fn detailed_evaluate(
    module: &dyn Module, dataset: &[Example], metric: &MetricFn,
) -> Result<EvaluationMetrics, Box<dyn std::error::Error>> {
    let mut metrics = EvaluationMetrics::default();

    for example in dataset {
        metrics.total += 1;

        let start = Instant::now();
        let result = module.forward(example.inputs.clone()).await;
        let elapsed = start.elapsed().as_millis() as u64;

        metrics.total_latency_ms += elapsed;

        match result {
            Ok(output) => {
                if metric(example, &output)? {
                    metrics.correct += 1;
                }
            }
            Err(e) => {
                metrics.errors.push(e.to_string());
            }
        }
    }

    Ok(metrics)
}

/// Cross-validation helper
fn split_dataset(dataset: &[Example], k: usize) -> Vec<Vec<Example>> {
    let chunk_size = (dataset.len() + k - 1) / k;
    dataset
        .chunks(chunk_size)
        .map(|chunk| chunk.to_vec())
        .collect()
}

/// K-fold cross-validation
async fn cross_validate(
    signature: Signature, dataset: &[Example], metric: MetricFn, k: usize,
) -> Result<f64, Box<dyn std::error::Error>> {
    let folds = split_dataset(dataset, k);
    let mut scores = Vec::new();

    println!("Running {}-fold cross-validation...\n", k);

    for i in 0..k {
        // Create train/val split
        let val_set = &folds[i];
        let train_set: Vec<_> = folds
            .iter()
            .enumerate()
            .filter(|(j, _)| *j != i)
            .flat_map(|(_, fold)| fold.clone())
            .collect();

        println!(
            "Fold {}/{}: train={}, val={}",
            i + 1,
            k,
            train_set.len(),
            val_set.len()
        );

        // Train and evaluate
        let student = Predictor::new(signature.clone());
        let optimizer = BootstrapFewShot::new(metric.clone()).with_max_bootstrapped_demos(3);

        let optimized = optimizer.compile(&student, &train_set).await?;
        let score = evaluate(&optimized, val_set, &metric).await?;
        scores.push(score);

        println!("  Accuracy: {:.1}%\n", score * 100.0);
    }

    let avg = scores.iter().sum::<f64>() / scores.len() as f64;
    let std_dev = {
        let variance = scores.iter().map(|s| (s - avg).powi(2)).sum::<f64>() / scores.len() as f64;
        variance.sqrt()
    };

    println!("Cross-validation results:");
    println!("  Average: {:.1}%", avg * 100.0);
    println!("  Std Dev: {:.1}%", std_dev * 100.0);

    Ok(avg)
}

#[tokio::main]
async fn main() -> Result<(), Box<dyn std::error::Error>> {
    tracing_subscriber::fmt()
        .with_max_level(tracing::Level::INFO)
        .init();

    println!("=== Evaluation Patterns Example ===\n");

    // Define signature
    let signature = Signature::new("MathClassification", "Classify math operations")
        .with_input(InputField::new("expression", "Math expression", "String"))
        .with_output(OutputField::new(
            "operation",
            "Operation type (addition/subtraction/multiplication/division)",
            "String",
        ))
        .with_instructions(
            "Identify the primary mathematical operation in the expression. \
         Respond with exactly one word: addition, subtraction, multiplication, or division.",
        );

    // Create dataset
    let dataset = vec![
        Example::new(
            [("expression".to_string(), json!("5 + 3"))].into(),
            [("operation".to_string(), json!("addition"))].into(),
        ),
        Example::new(
            [("expression".to_string(), json!("10 - 4"))].into(),
            [("operation".to_string(), json!("subtraction"))].into(),
        ),
        Example::new(
            [("expression".to_string(), json!("6 * 7"))].into(),
            [("operation".to_string(), json!("multiplication"))].into(),
        ),
        Example::new(
            [("expression".to_string(), json!("20 / 5"))].into(),
            [("operation".to_string(), json!("division"))].into(),
        ),
        Example::new(
            [("expression".to_string(), json!("15 + 8"))].into(),
            [("operation".to_string(), json!("addition"))].into(),
        ),
        Example::new(
            [("expression".to_string(), json!("12 - 9"))].into(),
            [("operation".to_string(), json!("subtraction"))].into(),
        ),
    ];

    println!("Dataset: {} examples\n", dataset.len());

    // Define metrics

    // 1. Exact match metric
    let exact_match = Arc::new(|example: &Example, output: &HashMap<String, Value>| {
        let expected = example
            .outputs
            .get("operation")
            .and_then(|v| v.as_str())
            .map(|s| s.to_lowercase());

        let actual = output
            .get("operation")
            .and_then(|v| v.as_str())
            .map(|s| s.to_lowercase());

        Ok(expected == actual)
    });

    // Part 1: Basic Evaluation
    println!("--- Part 1: Basic Evaluation ---\n");

    let predictor = Predictor::new(signature.clone());

    match evaluate(&predictor, &dataset, &exact_match).await {
        Ok(accuracy) => {
            println!("Basic accuracy: {:.1}%\n", accuracy * 100.0);
        }
        Err(e) => {
            eprintln!("Error: {}", e);
            if std::env::var("GGEN_LLM_MODEL").is_err() {
                eprintln!("\nNote: Set GGEN_LLM_MODEL and appropriate API key");
                return Ok(());
            }
        }
    }

    // Part 2: Detailed Evaluation
    println!("--- Part 2: Detailed Evaluation ---\n");

    match detailed_evaluate(&predictor, &dataset, &exact_match).await {
        Ok(metrics) => {
            println!("Detailed Metrics:");
            println!("  Total examples: {}", metrics.total);
            println!("  Correct: {}", metrics.correct);
            println!("  Accuracy: {:.1}%", metrics.accuracy() * 100.0);
            println!("  Avg latency: {}ms", metrics.avg_latency_ms());
            println!("  Error rate: {:.1}%", metrics.error_rate() * 100.0);
            if !metrics.errors.is_empty() {
                println!("  Errors: {:?}", metrics.errors);
            }
            println!();
        }
        Err(e) => {
            eprintln!("Error: {}\n", e);
        }
    }

    // Part 3: Cross-Validation
    println!("--- Part 3: Cross-Validation ---\n");

    match cross_validate(signature.clone(), &dataset, exact_match.clone(), 3).await {
        Ok(avg_score) => {
            println!("\nCross-validation average: {:.1}%\n", avg_score * 100.0);
        }
        Err(e) => {
            eprintln!("Error during cross-validation: {}\n", e);
        }
    }

    // Part 4: Custom Metrics
    println!("--- Part 4: Custom Metrics ---\n");

    // Multi-criteria metric
    let multi_criteria = Arc::new(|example: &Example, output: &HashMap<String, Value>| {
        // Check if output exists
        let has_output = output.contains_key("operation");

        // Check if matches expected
        let expected = example
            .outputs
            .get("operation")
            .and_then(|v| v.as_str())
            .map(|s| s.to_lowercase());

        let actual = output
            .get("operation")
            .and_then(|v| v.as_str())
            .map(|s| s.to_lowercase());

        let is_correct = expected == actual;

        // Check if response is one of the valid operations
        let valid_ops = ["addition", "subtraction", "multiplication", "division"];
        let is_valid = actual
            .as_ref()
            .map(|s| valid_ops.contains(&s.as_str()))
            .unwrap_or(false);

        Ok(has_output && is_correct && is_valid)
    });

    match evaluate(&predictor, &dataset, &multi_criteria).await {
        Ok(accuracy) => {
            println!("Multi-criteria accuracy: {:.1}%", accuracy * 100.0);
            println!("Criteria: has_output AND is_correct AND is_valid_operation\n");
        }
        Err(e) => {
            eprintln!("Error: {}\n", e);
        }
    }

    println!("=== Example Complete ===");
    println!(
        "\nEvaluation Patterns Summary:\n\
         1. Basic Evaluation: Simple accuracy calculation\n\
         2. Detailed Metrics: Latency, error rates, comprehensive stats\n\
         3. Cross-Validation: K-fold validation for robust estimates\n\
         4. Custom Metrics: Multi-criteria validation, domain-specific checks"
    );

    Ok(())
}
