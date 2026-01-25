//! Unit Tests for DSPy Testing Infrastructure
//!
//! Comprehensive tests for DummyLM, ExampleBuilder, and testing utilities.

use ggen_ai::dspy::testing::*;
use ggen_ai::dspy::{Module, ModuleError};
use serde_json::json;
use std::collections::HashMap;

// ============================================================================
// DummyLM Unit Tests
// ============================================================================

#[tokio::test]
async fn test_dummy_lm_sequential_basic() {
    let responses = vec![
        HashMap::from([("answer".to_string(), json!("Response 1"))]),
        HashMap::from([("answer".to_string(), json!("Response 2"))]),
    ];

    let dummy = DummyLM::sequential(responses);

    let inputs = HashMap::from([("prompt".to_string(), json!("Question 1"))]);
    let result = dummy.forward(inputs).await.unwrap();
    assert_eq!(result.get("answer").unwrap(), &json!("Response 1"));

    let inputs = HashMap::from([("prompt".to_string(), json!("Question 2"))]);
    let result = dummy.forward(inputs).await.unwrap();
    assert_eq!(result.get("answer").unwrap(), &json!("Response 2"));

    assert_eq!(dummy.call_count(), 2);
}

#[tokio::test]
async fn test_dummy_lm_sequential_cycling() {
    let responses = vec![HashMap::from([("answer".to_string(), json!("A"))])];

    let dummy = DummyLM::sequential(responses);

    // First 3 calls should all return "A" (cycling)
    for i in 1..=3 {
        let inputs = HashMap::from([("prompt".to_string(), json!(format!("Q{}", i)))]);
        let result = dummy.forward(inputs).await.unwrap();
        assert_eq!(result.get("answer").unwrap(), &json!("A"));
    }

    assert_eq!(dummy.call_count(), 3);
}

#[tokio::test]
async fn test_dummy_lm_query_based() {
    let mut query_map = HashMap::new();
    query_map.insert(
        "Rust".to_string(),
        HashMap::from([(
            "answer".to_string(),
            json!("A systems programming language"),
        )]),
    );
    query_map.insert(
        "Python".to_string(),
        HashMap::from([("answer".to_string(), json!("A high-level language"))]),
    );

    let dummy = DummyLM::query_based(query_map);

    // Test Rust query
    let inputs = HashMap::from([("prompt".to_string(), json!("What is Rust?"))]);
    let result = dummy.forward(inputs).await.unwrap();
    assert_eq!(
        result.get("answer").unwrap(),
        &json!("A systems programming language")
    );

    // Test Python query
    let inputs = HashMap::from([("prompt".to_string(), json!("Tell me about Python"))]);
    let result = dummy.forward(inputs).await.unwrap();
    assert_eq!(
        result.get("answer").unwrap(),
        &json!("A high-level language")
    );
}

#[tokio::test]
async fn test_dummy_lm_query_based_default() {
    let dummy = DummyLM::query_based(HashMap::new());

    let inputs = HashMap::from([("prompt".to_string(), json!("Unknown query"))]);
    let result = dummy.forward(inputs).await.unwrap();

    // Should return default response
    assert_eq!(result.get("response").unwrap(), &json!("default response"));
}

#[tokio::test]
async fn test_dummy_lm_example_following() {
    let demonstrations = vec![
        (
            HashMap::from([("question".to_string(), json!("What is 2+2?"))]),
            HashMap::from([("answer".to_string(), json!("4"))]),
        ),
        (
            HashMap::from([("question".to_string(), json!("What is 3+3?"))]),
            HashMap::from([("answer".to_string(), json!("6"))]),
        ),
    ];

    let dummy = DummyLM::example_following(demonstrations);

    // Match first example
    let inputs = HashMap::from([("prompt".to_string(), json!("question: What is 2+2?"))]);
    let result = dummy.forward(inputs).await.unwrap();
    assert_eq!(result.get("answer").unwrap(), &json!("4"));

    // Match second example
    let inputs = HashMap::from([("prompt".to_string(), json!("question: What is 3+3?"))]);
    let result = dummy.forward(inputs).await.unwrap();
    assert_eq!(result.get("answer").unwrap(), &json!("6"));
}

#[tokio::test]
async fn test_dummy_lm_call_history() {
    let responses = vec![HashMap::from([("answer".to_string(), json!("Response"))])];
    let dummy = DummyLM::sequential(responses);

    // Make several calls
    for i in 1..=3 {
        let inputs = HashMap::from([("prompt".to_string(), json!(format!("Prompt {}", i)))]);
        dummy.forward(inputs).await.unwrap();
    }

    let history = dummy.history();
    assert_eq!(history.len(), 3);
    assert!(history[0].prompt.contains("Prompt 1"));
    assert!(history[1].prompt.contains("Prompt 2"));
    assert!(history[2].prompt.contains("Prompt 3"));
}

#[tokio::test]
async fn test_dummy_lm_reset() {
    let responses = vec![HashMap::from([("answer".to_string(), json!("A"))])];
    let dummy = DummyLM::sequential(responses);

    let inputs = HashMap::from([("prompt".to_string(), json!("Q"))]);
    dummy.forward(inputs).await.unwrap();
    assert_eq!(dummy.call_count(), 1);

    dummy.reset();
    assert_eq!(dummy.call_count(), 0);
    assert_eq!(dummy.history().len(), 0);
}

#[tokio::test]
async fn test_dummy_lm_last_n_calls() {
    let responses = vec![HashMap::from([("answer".to_string(), json!("A"))])];
    let dummy = DummyLM::sequential(responses);

    // Make 5 calls
    for i in 1..=5 {
        let inputs = HashMap::from([("prompt".to_string(), json!(format!("Q{}", i)))]);
        dummy.forward(inputs).await.unwrap();
    }

    let last_3 = dummy.last_n_calls(3);
    assert_eq!(last_3.len(), 3);
    assert!(last_3[0].prompt.contains("Q5"));
    assert!(last_3[1].prompt.contains("Q4"));
    assert!(last_3[2].prompt.contains("Q3"));
}

#[tokio::test]
async fn test_dummy_lm_empty_responses_error() {
    let dummy = DummyLM::sequential(vec![]);

    let inputs = HashMap::from([("prompt".to_string(), json!("Test"))]);
    let result = dummy.forward(inputs).await;

    assert!(result.is_err());
}

// ============================================================================
// ExampleBuilder Unit Tests
// ============================================================================

#[test]
fn test_example_builder_basic() {
    let example = ExampleBuilder::new()
        .input("question", "What is Rust?")
        .output("answer", "A programming language")
        .build();

    assert_eq!(example.inputs.len(), 1);
    assert_eq!(example.outputs.len(), 1);
}

#[test]
fn test_example_builder_multiple_fields() {
    let example = ExampleBuilder::new()
        .input("q1", "Question 1")
        .input("q2", "Question 2")
        .output("a1", "Answer 1")
        .output("a2", "Answer 2")
        .build();

    assert_eq!(example.inputs.len(), 2);
    assert_eq!(example.outputs.len(), 2);
}

#[test]
fn test_example_builder_different_types() {
    let example = ExampleBuilder::new()
        .input("string", "text")
        .input("number", 42)
        .input("bool", true)
        .input("array", json!(["a", "b"]))
        .output("object", json!({"key": "value"}))
        .build();

    assert_eq!(example.inputs.len(), 4);
    assert_eq!(example.outputs.len(), 1);
}

#[test]
fn test_example_builder_inputs_method() {
    let example = ExampleBuilder::new()
        .inputs(vec![("q1", "A1"), ("q2", "A2")])
        .output("summary", "Combined")
        .build();

    assert_eq!(example.inputs.len(), 2);
}

#[test]
fn test_example_builder_outputs_method() {
    let example = ExampleBuilder::new()
        .input("query", "Test")
        .outputs(vec![("a1", "R1"), ("a2", "R2")])
        .build();

    assert_eq!(example.outputs.len(), 2);
}

#[test]
fn test_example_builder_inputs_only() {
    let example = ExampleBuilder::new()
        .input("question", "What is DSPy?")
        .build_inputs_only();

    assert_eq!(example.inputs.len(), 1);
    assert_eq!(example.outputs.len(), 0);
}

// ============================================================================
// Factory Function Tests
// ============================================================================

#[test]
fn test_qa_example_factory() {
    let example = qa_example("Question", "Answer");

    assert!(example.inputs.contains_key("question"));
    assert!(example.outputs.contains_key("answer"));
}

#[test]
fn test_classification_example_factory() {
    let example = classification_example("Text", "Label");

    assert!(example.inputs.contains_key("text"));
    assert!(example.outputs.contains_key("label"));
}

#[test]
fn test_create_qa_trainset() {
    let trainset = create_qa_trainset();

    assert!(trainset.len() >= 5);
    assert!(trainset
        .iter()
        .all(|ex| !ex.inputs.is_empty() && !ex.outputs.is_empty()));
}

#[test]
fn test_create_classification_trainset() {
    let trainset = create_classification_trainset();

    assert!(trainset.len() >= 5);
}

#[test]
fn test_create_test_signature() {
    let sig = create_test_signature("TestSig", "Test description");

    assert_eq!(sig.name, "TestSig");
    assert_eq!(sig.description, "Test description");
    assert_eq!(sig.inputs.len(), 1);
    assert_eq!(sig.outputs.len(), 1);
}

#[test]
fn test_create_large_trainset() {
    let trainset = create_large_trainset(100);

    assert_eq!(trainset.len(), 100);
}

#[test]
fn test_create_complexity_trainset() {
    let trainset = create_complexity_trainset();

    assert_eq!(trainset.len(), 3);

    // Verify varying complexity
    let lengths: Vec<usize> = trainset
        .iter()
        .map(|ex| {
            ex.inputs
                .values()
                .next()
                .and_then(|v| v.as_str())
                .map(|s| s.len())
                .unwrap_or(0)
        })
        .collect();

    assert!(lengths[0] < lengths[1]);
    assert!(lengths[1] < lengths[2]);
}

// ============================================================================
// Assertion Helper Tests
// ============================================================================

#[test]
fn test_assert_example_valid() {
    let example = qa_example("Q", "A");
    assert_example_valid(&example, &["question"], &["answer"]);
}

#[test]
#[should_panic]
fn test_assert_example_valid_missing_field() {
    let example = qa_example("Q", "A");
    assert_example_valid(&example, &["missing"], &["answer"]);
}

#[test]
fn test_assert_dataset_valid() {
    let examples = create_qa_trainset();
    assert_dataset_valid(&examples, &["question"], &["answer"]);
}

#[test]
#[should_panic]
fn test_assert_dataset_valid_empty() {
    assert_dataset_valid(&[], &["question"], &["answer"]);
}

// ============================================================================
// Integration Tests - DummyLM with Optimizer
// ============================================================================

use ggen_ai::dspy::optimizer::{BootstrapFewShot, Example};
use std::sync::Arc;

#[tokio::test]
async fn test_dummy_lm_with_optimizer_sequential() {
    // Create metric that accepts all
    let metric = Arc::new(|_: &Example, _: &HashMap<_, _>| Ok(true));

    let optimizer = BootstrapFewShot::new(metric).with_max_bootstrapped_demos(2);

    let trainset = create_qa_trainset();
    let trainset_ref: Vec<Example> = trainset.into_iter().take(2).collect();

    // Create DummyLM as student
    let responses = vec![
        HashMap::from([("answer".to_string(), json!("Response 1"))]),
        HashMap::from([("answer".to_string(), json!("Response 2"))]),
    ];
    let dummy = DummyLM::sequential(responses);

    // Compile optimizer with DummyLM
    let optimized = optimizer.compile(&dummy, &trainset_ref).await.unwrap();

    assert_eq!(optimized.demonstration_count(), 2);
    assert_eq!(dummy.call_count(), 2);
}

#[tokio::test]
async fn test_dummy_lm_with_selective_metric() {
    // Metric that only accepts specific answer
    let metric = Arc::new(|example: &Example, output: &HashMap<String, _>| {
        if let (Some(expected), Some(actual)) =
            (example.outputs.get("answer"), output.get("answer"))
        {
            Ok(expected == actual)
        } else {
            Ok(false)
        }
    });

    let optimizer = BootstrapFewShot::new(metric).with_max_bootstrapped_demos(5);

    // Create trainset with known answers
    let mut ex1_inputs = HashMap::new();
    ex1_inputs.insert("question".to_string(), json!("Q1"));
    let mut ex1_outputs = HashMap::new();
    ex1_outputs.insert("answer".to_string(), json!("Correct answer"));

    let trainset = vec![Example::new(ex1_inputs.clone(), ex1_outputs.clone())];

    // DummyLM that returns the correct answer
    let responses = vec![HashMap::from([(
        "answer".to_string(),
        json!("Correct answer"),
    )])];
    let dummy = DummyLM::sequential(responses);

    let optimized = optimizer.compile(&dummy, &trainset).await.unwrap();

    // Should have 1 demonstration (passed metric)
    assert_eq!(optimized.demonstration_count(), 1);
}

// ============================================================================
// Performance Tests
// ============================================================================

#[tokio::test]
async fn test_dummy_lm_performance_sequential() {
    use std::time::Instant;

    let responses = vec![HashMap::from([("answer".to_string(), json!("Response"))])];
    let dummy = DummyLM::sequential(responses);

    let start = Instant::now();

    // Make 1000 calls
    for i in 0..1000 {
        let inputs = HashMap::from([("prompt".to_string(), json!(format!("Q{}", i)))]);
        dummy.forward(inputs).await.unwrap();
    }

    let duration = start.elapsed();

    assert_eq!(dummy.call_count(), 1000);
    assert!(duration.as_millis() < 1000, "Should complete in < 1s");
}

#[test]
fn test_example_builder_performance() {
    use std::time::Instant;

    let start = Instant::now();

    // Build 10000 examples
    for i in 0..10000 {
        ExampleBuilder::new()
            .input("question", format!("Q{}", i))
            .output("answer", format!("A{}", i))
            .build();
    }

    let duration = start.elapsed();
    assert!(duration.as_millis() < 500, "Should complete in < 500ms");
}
