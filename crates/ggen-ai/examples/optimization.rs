//! Example: Optimizer Comparison - BootstrapFewShot
//!
//! Demonstrates optimization workflow:
//! 1. Create baseline predictor
//! 2. Prepare training data
//! 3. Define metric function
//! 4. Optimize with BootstrapFewShot
//! 5. Compare baseline vs. optimized performance
//!
//! Run with: cargo run --example optimization
//!
//! Prerequisites:
//! - Set GGEN_LLM_MODEL environment variable
//! - Set appropriate API key

use ggen_ai::dspy::{
    field::{InputField, OutputField},
    module::Module,
    optimizer::{BootstrapFewShot, Example},
    predictor::Predictor,
    signature::Signature,
};
use serde_json::{json, Value};
use std::collections::HashMap;
use std::sync::Arc;

/// Evaluation function
async fn evaluate(
    module: &dyn Module, dataset: &[Example],
    metric: &Arc<
        dyn Fn(&Example, &HashMap<String, Value>) -> Result<bool, ggen_ai::dspy::ModuleError>
            + Send
            + Sync,
    >,
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

#[tokio::main]
async fn main() -> Result<(), Box<dyn std::error::Error>> {
    tracing_subscriber::fmt()
        .with_max_level(tracing::Level::INFO)
        .init();

    println!("=== Optimizer Comparison Example ===\n");

    // Step 1: Define signature
    let signature = Signature::new(
        "SentimentClassification",
        "Classify text sentiment as positive, negative, or neutral",
    )
    .with_input(InputField::new("text", "Text to classify", "String"))
    .with_output(OutputField::new(
        "sentiment",
        "Sentiment (positive/negative/neutral)",
        "String",
    ))
    .with_instructions(
        "Classify the sentiment of the given text. \
         Respond with exactly one word: positive, negative, or neutral.",
    );

    println!("Task: Sentiment Classification\n");

    // Step 2: Create training dataset
    let trainset = vec![
        Example::new(
            {
                let mut m = HashMap::new();
                m.insert(
                    "text".to_string(),
                    json!("I absolutely love this product! It's amazing!"),
                );
                m
            },
            {
                let mut m = HashMap::new();
                m.insert("sentiment".to_string(), json!("positive"));
                m
            },
        ),
        Example::new(
            {
                let mut m = HashMap::new();
                m.insert(
                    "text".to_string(),
                    json!("This is the worst experience I've ever had."),
                );
                m
            },
            {
                let mut m = HashMap::new();
                m.insert("sentiment".to_string(), json!("negative"));
                m
            },
        ),
        Example::new(
            {
                let mut m = HashMap::new();
                m.insert(
                    "text".to_string(),
                    json!("The product is okay, nothing special."),
                );
                m
            },
            {
                let mut m = HashMap::new();
                m.insert("sentiment".to_string(), json!("neutral"));
                m
            },
        ),
        Example::new(
            {
                let mut m = HashMap::new();
                m.insert(
                    "text".to_string(),
                    json!("Fantastic service, highly recommend!"),
                );
                m
            },
            {
                let mut m = HashMap::new();
                m.insert("sentiment".to_string(), json!("positive"));
                m
            },
        ),
        Example::new(
            {
                let mut m = HashMap::new();
                m.insert(
                    "text".to_string(),
                    json!("Terrible quality, waste of money."),
                );
                m
            },
            {
                let mut m = HashMap::new();
                m.insert("sentiment".to_string(), json!("negative"));
                m
            },
        ),
    ];

    println!("Training set: {} examples", trainset.len());

    // Step 3: Create validation/test dataset
    let testset = vec![
        Example::new(
            {
                let mut m = HashMap::new();
                m.insert("text".to_string(), json!("Great product, very satisfied!"));
                m
            },
            {
                let mut m = HashMap::new();
                m.insert("sentiment".to_string(), json!("positive"));
                m
            },
        ),
        Example::new(
            {
                let mut m = HashMap::new();
                m.insert("text".to_string(), json!("Disappointing and overpriced."));
                m
            },
            {
                let mut m = HashMap::new();
                m.insert("sentiment".to_string(), json!("negative"));
                m
            },
        ),
        Example::new(
            {
                let mut m = HashMap::new();
                m.insert("text".to_string(), json!("It's fine, does the job."));
                m
            },
            {
                let mut m = HashMap::new();
                m.insert("sentiment".to_string(), json!("neutral"));
                m
            },
        ),
    ];

    println!("Test set: {} examples\n", testset.len());

    // Step 4: Define metric function
    let metric = Arc::new(|example: &Example, output: &HashMap<String, Value>| {
        let expected = example
            .outputs
            .get("sentiment")
            .and_then(|v| v.as_str())
            .map(|s| s.to_lowercase());

        let actual = output
            .get("sentiment")
            .and_then(|v| v.as_str())
            .map(|s| s.to_lowercase());

        Ok(expected == actual)
    });

    println!("Defined metric: Exact match (case-insensitive)\n");

    // Step 5: Evaluate baseline (no optimization)
    println!("--- Baseline Performance ---\n");

    let baseline = Predictor::new(signature.clone());

    match evaluate(&baseline, &testset, &metric).await {
        Ok(baseline_score) => {
            println!("Baseline accuracy: {:.1}%\n", baseline_score * 100.0);

            // Step 6: Optimize with BootstrapFewShot
            println!("--- Optimization Phase ---\n");
            println!("Running BootstrapFewShot optimizer...");

            let optimizer = BootstrapFewShot::new(metric.clone()).with_max_bootstrapped_demos(3);

            match optimizer.compile(&baseline, &trainset).await {
                Ok(optimized) => {
                    println!(
                        "Optimization complete: {} demonstrations collected\n",
                        optimized.demonstration_count()
                    );

                    // Step 7: Evaluate optimized predictor
                    println!("--- Optimized Performance ---\n");

                    match evaluate(&optimized, &testset, &metric).await {
                        Ok(optimized_score) => {
                            println!("Optimized accuracy: {:.1}%", optimized_score * 100.0);
                            let improvement = (optimized_score - baseline_score) * 100.0;
                            println!("Improvement: {:+.1}%\n", improvement);

                            // Step 8: Demonstrate on new examples
                            println!("--- Testing on New Examples ---\n");

                            let new_examples = vec![
                                "This exceeded all my expectations!",
                                "Complete waste of time and money.",
                                "It's average, nothing to complain about.",
                            ];

                            for text in new_examples {
                                let mut inputs = HashMap::new();
                                inputs.insert("text".to_string(), json!(text));

                                println!("Text: {}", text);

                                match optimized.forward(inputs).await {
                                    Ok(result) => {
                                        let sentiment = result
                                            .get("sentiment")
                                            .and_then(|v| v.as_str())
                                            .unwrap_or("unknown");
                                        println!("Sentiment: {}\n", sentiment);
                                    }
                                    Err(e) => {
                                        eprintln!("Error: {}\n", e);
                                    }
                                }
                            }
                        }
                        Err(e) => {
                            eprintln!("Error evaluating optimized model: {}", e);
                        }
                    }
                }
                Err(e) => {
                    eprintln!("Error during optimization: {}", e);
                    eprintln!("\nNote: Set GGEN_LLM_MODEL and appropriate API key");
                }
            }
        }
        Err(e) => {
            eprintln!("Error evaluating baseline: {}", e);
            if std::env::var("GGEN_LLM_MODEL").is_err() {
                eprintln!("\nNote: Set GGEN_LLM_MODEL environment variable");
                eprintln!("Example: export GGEN_LLM_MODEL=gpt-4");
                eprintln!("         export OPENAI_API_KEY=sk-...");
            }
        }
    }

    println!("=== Example Complete ===");
    println!(
        "\nOptimization Summary:\n\
         - BootstrapFewShot collects successful demonstrations from training data\n\
         - Demonstrations are included as few-shot examples in the prompt\n\
         - Typically improves accuracy by 10-20%\n\
         - Works best with 10-50 training examples"
    );

    Ok(())
}
