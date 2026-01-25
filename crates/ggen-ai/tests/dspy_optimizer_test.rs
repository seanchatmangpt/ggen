//! Integration test for DSPy BootstrapFewShot Optimizer
//!
//! Tests the optimizer API and basic functionality.

use ggen_ai::dspy::{
    field::{InputField, OutputField},
    module::{Module, ModuleError},
    optimizer::{BootstrapFewShot, Demonstration, Example},
    signature::Signature,
};
use serde_json::{json, Value};
use std::collections::HashMap;
use std::sync::Arc;

/// Mock predictor for testing
struct MockPredictor {
    sig: Signature,
    responses: Vec<HashMap<String, Value>>,
    call_count: std::sync::Arc<std::sync::Mutex<usize>>,
}

impl MockPredictor {
    fn new(sig: Signature, responses: Vec<HashMap<String, Value>>) -> Self {
        Self {
            sig,
            responses,
            call_count: Arc::new(std::sync::Mutex::new(0)),
        }
    }
}

#[async_trait::async_trait]
impl Module for MockPredictor {
    fn signature(&self) -> &Signature {
        &self.sig
    }

    async fn forward(
        &self, _inputs: HashMap<String, Value>,
    ) -> Result<HashMap<String, Value>, ModuleError> {
        let mut count = self.call_count.lock().unwrap();
        let idx = *count % self.responses.len();
        *count += 1;

        Ok(self.responses[idx].clone())
    }
}

fn create_qa_signature() -> Signature {
    Signature::new("QA", "Answer questions")
        .with_input(InputField::new("question", "Question to answer", "String"))
        .with_output(OutputField::new(
            "answer",
            "Answer to the question",
            "String",
        ))
}

#[test]
fn test_example_creation() {
    let mut inputs = HashMap::new();
    inputs.insert("question".to_string(), json!("What is Rust?"));

    let mut outputs = HashMap::new();
    outputs.insert(
        "answer".to_string(),
        json!("A systems programming language"),
    );

    let example = Example::new(inputs, outputs);

    assert_eq!(example.inputs.len(), 1);
    assert_eq!(example.outputs.len(), 1);
}

#[test]
fn test_demonstration_format() {
    let sig = create_qa_signature();

    let mut inputs = HashMap::new();
    inputs.insert("question".to_string(), json!("What is 2+2?"));

    let mut outputs = HashMap::new();
    outputs.insert("answer".to_string(), json!("4"));

    let demo = Demonstration::new(inputs, outputs);
    let formatted = demo.format(&sig);

    assert!(formatted.contains("question:"));
    assert!(formatted.contains("2+2"));
    assert!(formatted.contains("answer:"));
    assert!(formatted.contains("4"));
}

#[tokio::test]
async fn test_bootstrap_fewshot_basic() {
    // Create metric that accepts all examples
    let metric = Arc::new(|_example: &Example, _output: &HashMap<String, Value>| {
        Ok(true) // Accept all
    });

    let optimizer = BootstrapFewShot::new(metric).with_max_bootstrapped_demos(2);

    // Create training examples
    let mut ex1_inputs = HashMap::new();
    ex1_inputs.insert("question".to_string(), json!("What is Rust?"));
    let mut ex1_outputs = HashMap::new();
    ex1_outputs.insert("answer".to_string(), json!("A programming language"));

    let mut ex2_inputs = HashMap::new();
    ex2_inputs.insert("question".to_string(), json!("What is DSPy?"));
    let mut ex2_outputs = HashMap::new();
    ex2_outputs.insert("answer".to_string(), json!("A framework"));

    let trainset = vec![
        Example::new(ex1_inputs.clone(), ex1_outputs.clone()),
        Example::new(ex2_inputs.clone(), ex2_outputs.clone()),
    ];

    // Create mock student
    let sig = create_qa_signature();
    let responses = vec![ex1_outputs.clone(), ex2_outputs.clone()];
    let student = MockPredictor::new(sig, responses);

    // Compile optimizer
    let optimized = optimizer.compile(&student, &trainset).await.unwrap();

    assert_eq!(optimized.demonstration_count(), 2);
}

#[tokio::test]
async fn test_bootstrap_with_selective_metric() {
    // Metric that only accepts specific answers
    let metric = Arc::new(|example: &Example, output: &HashMap<String, Value>| {
        if let (Some(expected), Some(actual)) =
            (example.outputs.get("answer"), output.get("answer"))
        {
            Ok(expected == actual)
        } else {
            Ok(false)
        }
    });

    let optimizer = BootstrapFewShot::new(metric).with_max_bootstrapped_demos(5);

    // Create training examples
    let mut correct_inputs = HashMap::new();
    correct_inputs.insert("question".to_string(), json!("Correct question"));
    let mut correct_outputs = HashMap::new();
    correct_outputs.insert("answer".to_string(), json!("Correct answer"));

    let trainset = vec![Example::new(
        correct_inputs.clone(),
        correct_outputs.clone(),
    )];

    // Create mock student that returns correct answer
    let sig = create_qa_signature();
    let responses = vec![correct_outputs.clone()];
    let student = MockPredictor::new(sig, responses);

    // Compile optimizer
    let optimized = optimizer.compile(&student, &trainset).await.unwrap();

    // Should have 1 demonstration (the one that passed the metric)
    assert_eq!(optimized.demonstration_count(), 1);
}

#[tokio::test]
async fn test_bootstrap_empty_trainset_error() {
    let metric = Arc::new(|_: &Example, _: &HashMap<String, Value>| Ok(true));
    let optimizer = BootstrapFewShot::new(metric);

    let sig = create_qa_signature();
    let student = MockPredictor::new(sig, vec![]);

    let result = optimizer.compile(&student, &[]).await;

    assert!(result.is_err());
    assert!(result.unwrap_err().to_string().contains("empty"));
}

#[test]
fn test_bootstrap_builder_pattern() {
    let metric = Arc::new(|_: &Example, _: &HashMap<String, Value>| Ok(true));

    let optimizer = BootstrapFewShot::new(metric)
        .with_max_bootstrapped_demos(10)
        .with_max_labeled_demos(50);

    // Check that builder worked (fields are private, so we just verify it compiles)
    drop(optimizer);
}
