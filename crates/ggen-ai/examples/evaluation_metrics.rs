//! Example: Custom Metrics and Metric Composition
//!
//! Demonstrates creating and using custom metrics:
//! - Simple metrics
//! - Dual-mode metrics (bootstrapping vs evaluation)
//! - Composite metrics
//! - Built-in metrics
//!
//! Run with: cargo run --example evaluation_metrics

use ggen_ai::dspy::{
    evaluation::{
        composite, exact_match, exact_match_ci, f1_score, length_within_range, passage_match,
        substring_match, token_overlap, Evaluate, ExactMatchMetric, F1Metric, MetricError,
        PassageMatchMetric,
    },
    Example, InputField, Module, ModuleError, OutputField, Signature,
};
use serde_json::json;
use std::collections::HashMap;
use std::sync::Arc;

struct MockQA {
    sig: Signature,
}

#[async_trait::async_trait]
impl Module for MockQA {
    fn signature(&self) -> &Signature {
        &self.sig
    }

    async fn forward(
        &self, inputs: HashMap<String, serde_json::Value>,
    ) -> Result<HashMap<String, serde_json::Value>, ModuleError> {
        let mut outputs = HashMap::new();

        if let Some(question) = inputs.get("question").and_then(|v| v.as_str()) {
            let answer = if question.contains("capital") {
                "paris is the capital of france"
            } else {
                "42"
            };

            outputs.insert("answer".to_string(), json!(answer));
            outputs.insert(
                "context".to_string(),
                json!(["Paris is a city", "France is a country"]),
            );
        }

        Ok(outputs)
    }
}

#[tokio::main]
async fn main() -> Result<(), Box<dyn std::error::Error>> {
    println!("=== Custom Metrics Example ===\n");

    let signature = Signature::new("QA", "Answer questions")
        .with_input(InputField::new("question", "Question", "String"))
        .with_output(OutputField::new("answer", "Answer", "String"))
        .with_output(OutputField::new("context", "Context passages", "Array"));

    let program = MockQA { sig: signature };

    let devset = vec![
        Example::new(
            [("question".to_string(), json!("What is the capital?"))].into(),
            [("answer".to_string(), json!("Paris"))].into(),
        ),
        Example::new(
            [("question".to_string(), json!("What is 2+2?"))].into(),
            [("answer".to_string(), json!("4"))].into(),
        ),
    ];

    // 1. Built-in Metrics
    println!("=== Part 1: Built-in Metrics ===\n");

    let metrics = vec![
        ("Exact Match", exact_match("answer")),
        ("Case-Insensitive Match", exact_match_ci("answer")),
        ("Substring Match", substring_match("answer")),
        ("Token Overlap", token_overlap("answer")),
    ];

    for (name, metric) in metrics {
        let evaluator = Evaluate::new(devset.clone()).with_metric(metric);
        let result = evaluator.evaluate(&program, None).await?;
        println!("{}: {:.2}%", name, result.score);
    }

    // 2. Custom Metric Functions
    println!("\n=== Part 2: Custom Metrics ===\n");

    // Custom metric 1: Length check
    let length_check = Arc::new(
        |_example: &Example, pred: &HashMap<String, serde_json::Value>| {
            let answer = pred
                .get("answer")
                .and_then(|v| v.as_str())
                .ok_or_else(|| MetricError::MissingField("answer".to_string()))?;

            let word_count = answer.split_whitespace().count();
            Ok(if word_count >= 1 && word_count <= 20 {
                1.0
            } else {
                0.0
            })
        },
    );

    let evaluator = Evaluate::new(devset.clone()).with_metric(length_check);
    let result = evaluator.evaluate(&program, None).await?;
    println!("Length Check (1-20 words): {:.2}%", result.score);

    // Custom metric 2: Multi-property check
    let multi_property = Arc::new(
        |example: &Example, pred: &HashMap<String, serde_json::Value>| {
            // Check 1: Has answer
            let has_answer = pred.contains_key("answer");

            // Check 2: Answer is string
            let is_string = pred.get("answer").and_then(|v| v.as_str()).is_some();

            // Check 3: Non-empty
            let is_nonempty = pred
                .get("answer")
                .and_then(|v| v.as_str())
                .map(|s| !s.trim().is_empty())
                .unwrap_or(false);

            // Check 4: Matches expected
            let expected = example.outputs.get("answer");
            let actual = pred.get("answer");
            let matches = expected.is_some() && expected == actual;

            // All checks must pass
            let all_pass = has_answer && is_string && is_nonempty && matches;

            Ok(if all_pass { 1.0 } else { 0.0 })
        },
    );

    let evaluator = Evaluate::new(devset.clone()).with_metric(multi_property);
    let result = evaluator.evaluate(&program, None).await?;
    println!("Multi-Property Check: {:.2}%", result.score);

    // 3. Metric Objects
    println!("\n=== Part 3: Metric Objects ===\n");

    let exact_metric = ExactMatchMetric::new("answer").case_sensitive(false);
    let evaluator = Evaluate::new(devset.clone()).with_metric(exact_metric.as_metric());
    let result = evaluator.evaluate(&program, None).await?;
    println!("ExactMatchMetric (case-insensitive): {:.2}%", result.score);

    let f1_metric = F1Metric::new("answer", "answer");
    let evaluator = Evaluate::new(devset.clone()).with_metric(f1_metric.as_metric());
    let result = evaluator.evaluate(&program, None).await?;
    println!("F1Metric: {:.2}%", result.score);

    // 4. Composite Metrics
    println!("\n=== Part 4: Composite Metrics ===\n");

    let composite_metric = composite(vec![
        (exact_match_ci("answer"), 0.5),             // 50% weight
        (substring_match("answer"), 0.3),            // 30% weight
        (length_within_range("answer", 1, 50), 0.2), // 20% weight
    ]);

    let evaluator = Evaluate::new(devset.clone()).with_metric(composite_metric);
    let result = evaluator.evaluate(&program, None).await?;
    println!("Composite Metric (weighted average): {:.2}%", result.score);

    // 5. F1 Score Calculation
    println!("\n=== Part 5: F1 Score ===\n");

    let precision = 0.8;
    let recall = 0.9;
    let f1 = f1_score(precision, recall);
    println!(
        "Precision: {:.2}, Recall: {:.2}, F1: {:.2}",
        precision, recall, f1
    );

    println!("\n=== Example Complete ===");
    println!("\nMetric Design Principles:");
    println!("1. Start with simple exact match");
    println!("2. Add flexibility based on failure analysis");
    println!("3. Combine multiple metrics for comprehensive evaluation");
    println!("4. Use built-in metrics when available");
    println!("5. Create custom metrics for domain-specific requirements");

    Ok(())
}
