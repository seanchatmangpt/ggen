//! Integration Tests for DSPy Testing Infrastructure
//!
//! Tests complete workflows with DummyLM, optimizers, and pipelines.

use ggen_ai::dspy::testing::*;
use ggen_ai::dspy::{Module, ModuleError, optimizer::{BootstrapFewShot, Example, Demonstration}};
use serde_json::json;
use std::collections::HashMap;
use std::sync::Arc;

// ============================================================================
// Complete Workflow Tests
// ============================================================================

#[tokio::test]
async fn test_complete_optimization_workflow() {
    // Arrange: Create training set
    let trainset = create_qa_trainset();

    // Create metric (exact match)
    let metric = Arc::new(|example: &Example, output: &HashMap<String, _>| {
        if let (Some(expected), Some(actual)) =
            (example.outputs.get("answer"), output.get("answer"))
        {
            Ok(expected == actual)
        } else {
            Ok(false)
        }
    });

    // Create optimizer
    let optimizer = BootstrapFewShot::new(metric).with_max_bootstrapped_demos(3);

    // Create DummyLM that returns correct answers
    let responses: Vec<HashMap<String, _>> = trainset
        .iter()
        .take(3)
        .map(|ex| ex.outputs.clone())
        .collect();

    let dummy = DummyLM::sequential(responses);

    // Act: Compile optimizer
    let optimized = optimizer.compile(&dummy, &trainset[..3]).await.unwrap();

    // Assert: Verify demonstrations collected
    assert!(optimized.demonstration_count() > 0);
    assert!(optimized.demonstration_count() <= 3);
}

#[tokio::test]
async fn test_multi_stage_pipeline() {
    // Stage 1: Create examples
    let examples = vec![
        ExampleBuilder::new()
            .input("text", "Great product!")
            .output("label", "positive")
            .build(),
        ExampleBuilder::new()
            .input("text", "Terrible quality")
            .output("label", "negative")
            .build(),
    ];

    // Stage 2: Create DummyLM
    let responses = vec![
        HashMap::from([("label".to_string(), json!("positive"))]),
        HashMap::from([("label".to_string(), json!("negative"))]),
    ];
    let dummy = DummyLM::sequential(responses);

    // Stage 3: Run through optimizer
    let metric = Arc::new(|example: &Example, output: &HashMap<String, _>| {
        Ok(example.outputs.get("label") == output.get("label"))
    });

    let optimizer = BootstrapFewShot::new(metric).with_max_bootstrapped_demos(2);

    let optimized = optimizer.compile(&dummy, &examples).await.unwrap();

    // Stage 4: Verify results
    assert_eq!(optimized.demonstration_count(), 2);
    assert_eq!(dummy.call_count(), 2);

    let history = dummy.history();
    assert_eq!(history.len(), 2);
}

#[tokio::test]
async fn test_query_based_workflow() {
    // Create query-response mapping
    let mut query_map = HashMap::new();
    query_map.insert(
        "capital of France".to_string(),
        HashMap::from([("answer".to_string(), json!("Paris"))]),
    );
    query_map.insert(
        "capital of UK".to_string(),
        HashMap::from([("answer".to_string(), json!("London"))]),
    );
    query_map.insert(
        "capital of Germany".to_string(),
        HashMap::from([("answer".to_string(), json!("Berlin"))]),
    );

    let dummy = DummyLM::query_based(query_map);

    // Test multiple queries
    let queries = vec![
        "What is the capital of France?",
        "Tell me the capital of UK",
        "capital of Germany please",
    ];

    let expected_answers = vec!["Paris", "London", "Berlin"];

    for (query, expected) in queries.iter().zip(expected_answers.iter()) {
        let inputs = HashMap::from([("prompt".to_string(), json!(query))]);
        let result = dummy.forward(inputs).await.unwrap();

        assert_eq!(result.get("answer").unwrap(), &json!(expected));
    }

    assert_eq!(dummy.call_count(), 3);
}

#[tokio::test]
async fn test_example_following_workflow() {
    // Create demonstrations
    let demonstrations = vec![
        (
            HashMap::from([("input".to_string(), json!("translate: hello"))]),
            HashMap::from([("output".to_string(), json!("bonjour"))]),
        ),
        (
            HashMap::from([("input".to_string(), json!("translate: goodbye"))]),
            HashMap::from([("output".to_string(), json!("au revoir"))]),
        ),
    ];

    let dummy = DummyLM::example_following(demonstrations);

    // Test matching
    let inputs = HashMap::from([("prompt".to_string(), json!("input: translate: hello"))]);
    let result = dummy.forward(inputs).await.unwrap();
    assert_eq!(result.get("output").unwrap(), &json!("bonjour"));

    let inputs = HashMap::from([("prompt".to_string(), json!("input: translate: goodbye"))]);
    let result = dummy.forward(inputs).await.unwrap();
    assert_eq!(result.get("output").unwrap(), &json!("au revoir"));
}

// ============================================================================
// Error Handling Tests
// ============================================================================

#[tokio::test]
async fn test_optimizer_empty_trainset_error() {
    let metric = Arc::new(|_: &Example, _: &HashMap<String, _>| Ok(true));
    let optimizer = BootstrapFewShot::new(metric);

    let dummy = DummyLM::sequential(vec![]);
    let result = optimizer.compile(&dummy, &[]).await;

    assert!(result.is_err());
    assert!(result.unwrap_err().to_string().contains("empty"));
}

#[tokio::test]
async fn test_metric_failure_handling() {
    // Metric that always returns error
    let metric = Arc::new(|_: &Example, _: &HashMap<String, _>| {
        Err(ModuleError::Other("Metric error".to_string()))
    });

    let optimizer = BootstrapFewShot::new(metric).with_max_bootstrapped_demos(2);

    let trainset = create_qa_trainset();
    let responses = vec![HashMap::from([("answer".to_string(), json!("Response"))])];
    let dummy = DummyLM::sequential(responses);

    // Should handle metric errors gracefully
    let optimized = optimizer.compile(&dummy, &trainset[..2]).await.unwrap();

    // No demonstrations should pass metric
    assert_eq!(optimized.demonstration_count(), 0);
}

#[tokio::test]
async fn test_partial_metric_success() {
    // Metric that only accepts answers containing "programming"
    let metric = Arc::new(|_example: &Example, output: &HashMap<String, _>| {
        if let Some(answer) = output.get("answer").and_then(|v| v.as_str()) {
            Ok(answer.contains("programming"))
        } else {
            Ok(false)
        }
    });

    let optimizer = BootstrapFewShot::new(metric).with_max_bootstrapped_demos(5);

    let trainset = create_qa_trainset();

    // Mixed responses - some pass, some fail
    let responses = vec![
        HashMap::from([("answer".to_string(), json!("A programming language"))]),
        HashMap::from([("answer".to_string(), json!("A framework"))]),
        HashMap::from([("answer".to_string(), json!("programming tool"))]),
        HashMap::from([("answer".to_string(), json!("4"))]),
        HashMap::from([("answer".to_string(), json!("Paris"))]),
    ];

    let dummy = DummyLM::sequential(responses);

    let optimized = optimizer.compile(&dummy, &trainset).await.unwrap();

    // Should have 2 demonstrations (those containing "programming")
    assert_eq!(optimized.demonstration_count(), 2);
}

// ============================================================================
// Stress Tests
// ============================================================================

#[tokio::test]
async fn test_large_trainset_optimization() {
    let trainset = create_large_trainset(50);

    let metric = Arc::new(|_: &Example, _: &HashMap<String, _>| Ok(true));
    let optimizer = BootstrapFewShot::new(metric).with_max_bootstrapped_demos(10);

    let responses = vec![HashMap::from([("answer".to_string(), json!("Response"))])];
    let dummy = DummyLM::sequential(responses);

    let optimized = optimizer.compile(&dummy, &trainset).await.unwrap();

    // Should collect exactly max_bootstrapped_demos
    assert_eq!(optimized.demonstration_count(), 10);
}

#[tokio::test]
async fn test_concurrent_dummy_lm_calls() {
    use tokio::task;

    let responses = vec![HashMap::from([("answer".to_string(), json!("Response"))])];
    let dummy = Arc::new(DummyLM::sequential(responses));

    // Spawn 10 concurrent tasks
    let mut handles = vec![];
    for i in 0..10 {
        let dummy_clone = dummy.clone();
        let handle = task::spawn(async move {
            let inputs = HashMap::from([("prompt".to_string(), json!(format!("Q{}", i)))]);
            dummy_clone.forward(inputs).await
        });
        handles.push(handle);
    }

    // Wait for all to complete
    for handle in handles {
        assert!(handle.await.unwrap().is_ok());
    }

    assert_eq!(dummy.call_count(), 10);
}

// ============================================================================
// Golden Test Integration
// ============================================================================

#[test]
fn test_golden_test_workflow() {
    use tempfile::TempDir;
    use ggen_ai::dspy::testing::GoldenTest;

    let temp_dir = TempDir::new().unwrap();

    // Create and save golden test
    let inputs = HashMap::from([("question".to_string(), json!("What is Rust?"))]);
    let outputs = HashMap::from([("answer".to_string(), json!("A programming language"))]);

    let test = GoldenTest::new("test_rust_question", inputs.clone(), outputs.clone());
    let path = temp_dir.path().join("test.json");
    test.save(&path).unwrap();

    // Load and verify
    let loaded = GoldenTest::load(&path).unwrap();
    assert_eq!(loaded.name, "test_rust_question");
    assert_eq!(loaded.inputs, inputs);
    assert_eq!(loaded.expected_outputs, outputs);

    // Compare with matching outputs
    use ggen_ai::dspy::testing::compare_with_golden;
    let result = compare_with_golden(&path, &outputs).unwrap();
    assert!(result.is_match());

    // Compare with different outputs
    let different = HashMap::from([("answer".to_string(), json!("Different answer"))]);
    let result = compare_with_golden(&path, &different).unwrap();
    assert!(!result.is_match());
}

// ============================================================================
// Demonstration Quality Tests
// ============================================================================

#[tokio::test]
async fn test_demonstrations_are_diverse() {
    let metric = Arc::new(|_: &Example, _: &HashMap<String, _>| Ok(true));
    let optimizer = BootstrapFewShot::new(metric).with_max_bootstrapped_demos(5);

    // Create diverse training set
    let trainset = create_complexity_trainset();

    let responses: Vec<HashMap<String, _>> = trainset
        .iter()
        .map(|ex| ex.outputs.clone())
        .collect();

    let dummy = DummyLM::sequential(responses);

    let optimized = optimizer.compile(&dummy, &trainset).await.unwrap();

    // Check demonstrations are diverse
    use ggen_ai::dspy::testing::assert_demonstrations_diverse;
    assert_demonstrations_diverse(optimized.demonstrations());
}

#[tokio::test]
async fn test_demonstrations_match_signature() {
    use ggen_ai::dspy::testing::assert_demonstration_valid;

    let sig = create_test_signature("QA", "Question answering");

    let metric = Arc::new(|_: &Example, _: &HashMap<String, _>| Ok(true));
    let optimizer = BootstrapFewShot::new(metric).with_max_bootstrapped_demos(3);

    let trainset = create_qa_trainset();
    let responses: Vec<HashMap<String, _>> = trainset
        .iter()
        .take(3)
        .map(|ex| ex.outputs.clone())
        .collect();

    let dummy = DummyLM::sequential(responses).with_signature(sig.clone());

    let optimized = optimizer.compile(&dummy, &trainset[..3]).await.unwrap();

    // Verify each demonstration matches signature
    for demo in optimized.demonstrations() {
        assert_demonstration_valid(demo, &sig);
    }
}

// ============================================================================
// End-to-End Pipeline Tests
// ============================================================================

#[tokio::test]
async fn test_e2e_classification_pipeline() {
    // 1. Create training data
    let trainset = create_classification_trainset();

    // 2. Setup DummyLM with query-based responses
    let mut query_map = HashMap::new();
    query_map.insert(
        "Great".to_string(),
        HashMap::from([("label".to_string(), json!("positive"))]),
    );
    query_map.insert(
        "Terrible".to_string(),
        HashMap::from([("label".to_string(), json!("negative"))]),
    );
    query_map.insert(
        "okay".to_string(),
        HashMap::from([("label".to_string(), json!("neutral"))]),
    );
    query_map.insert(
        "love".to_string(),
        HashMap::from([("label".to_string(), json!("positive"))]),
    );
    query_map.insert(
        "Disappointed".to_string(),
        HashMap::from([("label".to_string(), json!("negative"))]),
    );

    let dummy = DummyLM::query_based(query_map);

    // 3. Setup optimizer
    let metric = Arc::new(|example: &Example, output: &HashMap<String, _>| {
        Ok(example.outputs.get("label") == output.get("label"))
    });

    let optimizer = BootstrapFewShot::new(metric).with_max_bootstrapped_demos(3);

    // 4. Compile
    let optimized = optimizer.compile(&dummy, &trainset).await.unwrap();

    // 5. Verify
    assert!(optimized.demonstration_count() >= 3);
    assert_eq!(dummy.call_count(), trainset.len());

    // 6. Check diversity
    use ggen_ai::dspy::testing::assert_demonstrations_diverse;
    assert_demonstrations_diverse(optimized.demonstrations());
}

#[tokio::test]
async fn test_e2e_qa_pipeline_with_reset() {
    let trainset = create_qa_trainset();

    let responses: Vec<HashMap<String, _>> = trainset
        .iter()
        .take(3)
        .map(|ex| ex.outputs.clone())
        .collect();

    let dummy = DummyLM::sequential(responses);

    // First run
    let metric = Arc::new(|_: &Example, _: &HashMap<String, _>| Ok(true));
    let optimizer = BootstrapFewShot::new(metric.clone()).with_max_bootstrapped_demos(3);

    let optimized1 = optimizer.compile(&dummy, &trainset[..3]).await.unwrap();
    assert_eq!(optimized1.demonstration_count(), 3);

    let call_count_after_first = dummy.call_count();
    assert!(call_count_after_first >= 3);

    // Reset and run again
    dummy.reset();
    assert_eq!(dummy.call_count(), 0);

    let optimizer2 = BootstrapFewShot::new(metric).with_max_bootstrapped_demos(3);
    let optimized2 = optimizer2.compile(&dummy, &trainset[..3]).await.unwrap();

    assert_eq!(optimized2.demonstration_count(), 3);
    assert!(dummy.call_count() >= 3);
}
