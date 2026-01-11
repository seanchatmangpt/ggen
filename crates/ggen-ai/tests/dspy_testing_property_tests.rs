//! Property-Based Tests for DSPy Testing Infrastructure
//!
//! Uses proptest to verify invariants hold across random inputs.

use ggen_ai::dspy::testing::*;
use ggen_ai::dspy::Module;
use proptest::prelude::*;
use serde_json::{json, Value};
use std::collections::HashMap;

// ============================================================================
// Property Test Strategies
// ============================================================================

/// Strategy for generating field names
fn field_name_strategy() -> impl Strategy<Value = String> {
    prop::string::string_regex("[a-z_][a-z0-9_]{0,20}").unwrap()
}

/// Strategy for generating string values
fn string_value_strategy() -> impl Strategy<Value = String> {
    prop_oneof![
        prop::string::string_regex("[A-Za-z ]{5,50}").unwrap(),
        Just("".to_string()),
        Just("Unicode: 你好 🌍".to_string()),
    ]
}

/// Strategy for generating JSON values
fn json_value_strategy() -> impl Strategy<Value = Value> {
    prop_oneof![
        string_value_strategy().prop_map(|s| json!(s)),
        any::<i32>().prop_map(|n| json!(n)),
        any::<bool>().prop_map(|b| json!(b)),
        prop::collection::vec(string_value_strategy(), 0..5).prop_map(|v| json!(v)),
    ]
}

/// Strategy for generating example inputs/outputs
fn field_map_strategy() -> impl Strategy<Value = HashMap<String, Value>> {
    prop::collection::hash_map(field_name_strategy(), json_value_strategy(), 1..5)
}

/// Strategy for generating Examples
fn example_strategy() -> impl Strategy<Value = ggen_ai::dspy::optimizer::Example> {
    (field_map_strategy(), field_map_strategy()).prop_map(|(inputs, outputs)| {
        ggen_ai::dspy::optimizer::Example::new(inputs, outputs)
    })
}

// ============================================================================
// DummyLM Property Tests
// ============================================================================

proptest! {
    #[test]
    fn test_dummy_lm_sequential_cycles_correctly(
        responses in prop::collection::vec(field_map_strategy(), 1..10),
        call_count in 1usize..100,
    ) {
        let rt = tokio::runtime::Runtime::new().unwrap();
        rt.block_on(async {
            let dummy = DummyLM::sequential(responses.clone());

            for i in 0..call_count {
                let inputs = HashMap::from([("prompt".to_string(), json!(format!("Q{}", i)))]);
                let result = dummy.forward(inputs).await;

                // Should always succeed
                prop_assert!(result.is_ok());

                // Should cycle through responses
                let expected_idx = i % responses.len();
                let expected = &responses[expected_idx];

                let actual = result.unwrap();

                // At least one field should match
                let has_match = expected.iter().any(|(k, v)| actual.get(k) == Some(v));
                prop_assert!(has_match);
            }

            prop_assert_eq!(dummy.call_count(), call_count);
            Ok(())
        })?;
    }

    #[test]
    fn test_dummy_lm_history_tracks_all_calls(
        responses in prop::collection::vec(field_map_strategy(), 1..5),
        call_count in 1usize..20,
    ) {
        let rt = tokio::runtime::Runtime::new().unwrap();
        rt.block_on(async {
            let dummy = DummyLM::sequential(responses);

            for i in 0..call_count {
                let inputs = HashMap::from([("prompt".to_string(), json!(format!("Prompt {}", i)))]);
                let _ = dummy.forward(inputs).await;
            }

            let history = dummy.history();
            prop_assert_eq!(history.len(), call_count);

            // Check history is in order
            for (i, entry) in history.iter().enumerate() {
                prop_assert!(entry.prompt.contains(&format!("Prompt {}", i)));
            }

            Ok(())
        })?;
    }

    #[test]
    fn test_dummy_lm_reset_clears_state(
        responses in prop::collection::vec(field_map_strategy(), 1..5),
        initial_calls in 1usize..10,
    ) {
        let rt = tokio::runtime::Runtime::new().unwrap();
        rt.block_on(async {
            let dummy = DummyLM::sequential(responses);

            // Make some calls
            for i in 0..initial_calls {
                let inputs = HashMap::from([("prompt".to_string(), json!(format!("Q{}", i)))]);
                let _ = dummy.forward(inputs).await;
            }

            prop_assert_eq!(dummy.call_count(), initial_calls);

            // Reset
            dummy.reset();

            prop_assert_eq!(dummy.call_count(), 0);
            prop_assert_eq!(dummy.history().len(), 0);

            Ok(())
        })?;
    }
}

// ============================================================================
// ExampleBuilder Property Tests
// ============================================================================

proptest! {
    #[test]
    fn test_example_builder_preserves_all_fields(
        inputs in field_map_strategy(),
        outputs in field_map_strategy(),
    ) {
        let mut builder = ExampleBuilder::new();

        for (key, value) in &inputs {
            builder = builder.input(key, value.clone());
        }

        for (key, value) in &outputs {
            builder = builder.output(key, value.clone());
        }

        let example = builder.build();

        prop_assert_eq!(example.inputs.len(), inputs.len());
        prop_assert_eq!(example.outputs.len(), outputs.len());

        for (key, value) in &inputs {
            prop_assert_eq!(example.inputs.get(key), Some(value));
        }

        for (key, value) in &outputs {
            prop_assert_eq!(example.outputs.get(key), Some(value));
        }
    }

    #[test]
    fn test_example_builder_inputs_method(
        fields in prop::collection::hash_map(
            field_name_strategy(),
            string_value_strategy(),
            1..10
        ),
    ) {
        let items: Vec<_> = fields.iter()
            .map(|(k, v)| (k.clone(), v.clone()))
            .collect();

        let example = ExampleBuilder::new()
            .inputs(items)
            .build();

        prop_assert_eq!(example.inputs.len(), fields.len());

        for (key, value) in &fields {
            prop_assert_eq!(
                example.inputs.get(key),
                Some(&json!(value))
            );
        }
    }

    #[test]
    fn test_example_builder_chaining_order_irrelevant(
        key1 in field_name_strategy(),
        key2 in field_name_strategy(),
        key3 in field_name_strategy(),
        val1 in string_value_strategy(),
        val2 in string_value_strategy(),
        val3 in string_value_strategy(),
    ) {
        prop_assume!(key1 != key2 && key2 != key3 && key1 != key3);

        // Build in one order
        let example1 = ExampleBuilder::new()
            .input(&key1, val1.clone())
            .output(&key2, val2.clone())
            .input(&key3, val3.clone())
            .build();

        // Build in different order
        let example2 = ExampleBuilder::new()
            .output(&key2, val2.clone())
            .input(&key3, val3.clone())
            .input(&key1, val1.clone())
            .build();

        prop_assert_eq!(example1.inputs, example2.inputs);
        prop_assert_eq!(example1.outputs, example2.outputs);
    }
}

// ============================================================================
// Example Validity Property Tests
// ============================================================================

proptest! {
    #[test]
    fn test_any_qa_example_is_valid(
        question in string_value_strategy(),
        answer in string_value_strategy(),
    ) {
        let example = qa_example(&question, &answer);

        prop_assert!(example.inputs.contains_key("question"));
        prop_assert!(example.outputs.contains_key("answer"));
        prop_assert_eq!(example.inputs.get("question"), Some(&json!(question)));
        prop_assert_eq!(example.outputs.get("answer"), Some(&json!(answer)));
    }

    #[test]
    fn test_any_classification_example_is_valid(
        text in string_value_strategy(),
        label in prop::sample::select(vec!["positive", "negative", "neutral"]),
    ) {
        let example = classification_example(&text, label);

        prop_assert!(example.inputs.contains_key("text"));
        prop_assert!(example.outputs.contains_key("label"));
        prop_assert_eq!(example.inputs.get("text"), Some(&json!(text)));
        prop_assert_eq!(example.outputs.get("label"), Some(&json!(label)));
    }

    #[test]
    fn test_large_trainset_size_correct(size in 1usize..200) {
        let trainset = create_large_trainset(size);

        prop_assert_eq!(trainset.len(), size);

        // All examples should be valid
        for example in trainset {
            prop_assert!(!example.inputs.is_empty());
            prop_assert!(!example.outputs.is_empty());
        }
    }
}

// ============================================================================
// Assertion Helper Property Tests
// ============================================================================

proptest! {
    #[test]
    fn test_assert_example_valid_never_panics_on_valid_examples(
        example in example_strategy(),
    ) {
        // Extract actual keys from example
        let input_keys: Vec<_> = example.inputs.keys()
            .map(|s| s.as_str())
            .collect();
        let output_keys: Vec<_> = example.outputs.keys()
            .map(|s| s.as_str())
            .collect();

        // Should not panic when keys match
        assert_example_valid(&example, &input_keys, &output_keys);
    }

    #[test]
    fn test_assert_dataset_valid_with_consistent_examples(
        size in 1usize..20,
        question in string_value_strategy(),
        answer in string_value_strategy(),
    ) {
        let examples: Vec<_> = (0..size)
            .map(|i| qa_example(&format!("{} {}", question, i), &format!("{} {}", answer, i)))
            .collect();

        // Should not panic - all examples have same structure
        assert_dataset_valid(&examples, &["question"], &["answer"]);
    }
}

// ============================================================================
// Golden Test Property Tests
// ============================================================================

proptest! {
    #[test]
    fn test_golden_test_save_load_roundtrip(
        name in prop::string::string_regex("[a-zA-Z0-9_-]{1,50}").unwrap(),
        inputs in field_map_strategy(),
        outputs in field_map_strategy(),
    ) {
        use tempfile::TempDir;
        use ggen_ai::dspy::testing::GoldenTest;

        let temp_dir = TempDir::new().unwrap();
        let path = temp_dir.path().join("test.json");

        let test = GoldenTest::new(name.clone(), inputs.clone(), outputs.clone());
        test.save(&path).unwrap();

        let loaded = GoldenTest::load(&path).unwrap();

        prop_assert_eq!(loaded.name, name);
        prop_assert_eq!(loaded.inputs, inputs);
        prop_assert_eq!(loaded.expected_outputs, outputs);
    }

    #[test]
    fn test_golden_comparison_reflexive(
        outputs in field_map_strategy(),
    ) {
        use ggen_ai::dspy::testing::GoldenTest;
        use tempfile::TempDir;

        let temp_dir = TempDir::new().unwrap();
        let path = temp_dir.path().join("test.json");

        let test = GoldenTest::new("test", HashMap::new(), outputs.clone());
        test.save(&path).unwrap();

        // Comparing with itself should always match
        use ggen_ai::dspy::testing::compare_with_golden;
        let result = compare_with_golden(&path, &outputs).unwrap();

        prop_assert!(result.is_match());
    }
}

// ============================================================================
// Complex Integration Property Tests
// ============================================================================

proptest! {
    #[test]
    fn test_dummy_lm_with_random_responses(
        responses in prop::collection::vec(field_map_strategy(), 1..10),
        query_count in 1usize..50,
    ) {
        let rt = tokio::runtime::Runtime::new().unwrap();
        rt.block_on(async {
            let dummy = DummyLM::sequential(responses.clone());

            for i in 0..query_count {
                let inputs = HashMap::from([
                    ("prompt".to_string(), json!(format!("Query {}", i))),
                ]);

                let result = dummy.forward(inputs).await;

                // Should always succeed
                prop_assert!(result.is_ok());

                // Result should be one of the configured responses
                let response = result.unwrap();
                let expected_idx = i % responses.len();
                let expected = &responses[expected_idx];

                // At least one field should match expected response
                let has_matching_field = expected.iter().any(|(k, v)| {
                    response.get(k) == Some(v)
                });

                prop_assert!(has_matching_field);
            }

            prop_assert_eq!(dummy.call_count(), query_count);

            Ok(())
        })?;
    }

    #[test]
    fn test_history_last_n_calls_invariants(
        responses in prop::collection::vec(field_map_strategy(), 1..5),
        total_calls in 5usize..30,
        n in 1usize..10,
    ) {
        let rt = tokio::runtime::Runtime::new().unwrap();
        rt.block_on(async {
            let dummy = DummyLM::sequential(responses);

            for i in 0..total_calls {
                let inputs = HashMap::from([("prompt".to_string(), json!(format!("Q{}", i)))]);
                let _ = dummy.forward(inputs).await;
            }

            let last_n = dummy.last_n_calls(n);

            // Should not exceed requested count or total calls
            prop_assert!(last_n.len() <= n);
            prop_assert!(last_n.len() <= total_calls);

            // If we have enough calls, should return exactly n
            if total_calls >= n {
                prop_assert_eq!(last_n.len(), n);
            }

            // Last entry should be most recent
            if !last_n.is_empty() {
                let most_recent = &last_n[0];
                prop_assert!(most_recent.prompt.contains(&format!("Q{}", total_calls - 1)));
            }

            Ok(())
        })?;
    }
}

// ============================================================================
// Edge Case Property Tests
// ============================================================================

proptest! {
    #[test]
    fn test_example_builder_handles_empty_strings(
        empty_count in 0usize..5,
        nonempty_count in 1usize..5,
    ) {
        let mut builder = ExampleBuilder::new();

        for i in 0..empty_count {
            builder = builder.input(&format!("empty_{}", i), "");
        }

        for i in 0..nonempty_count {
            builder = builder.input(&format!("nonempty_{}", i), format!("value_{}", i));
        }

        let example = builder.build();

        prop_assert_eq!(example.inputs.len(), empty_count + nonempty_count);
    }

    #[test]
    fn test_dummy_lm_query_based_handles_missing_keys(
        configured_keys in prop::collection::hash_set(field_name_strategy(), 1..5),
        query_keys in prop::collection::hash_set(field_name_strategy(), 1..5),
    ) {
        let rt = tokio::runtime::Runtime::new().unwrap();
        rt.block_on(async {
            // Create query map with configured keys
            let mut query_map = HashMap::new();
            for key in &configured_keys {
                query_map.insert(
                    key.clone(),
                    HashMap::from([("response".to_string(), json!(format!("Response for {}", key)))]),
                );
            }

            let dummy = DummyLM::query_based(query_map);

            // Try queries with different keys
            for key in &query_keys {
                let inputs = HashMap::from([("prompt".to_string(), json!(key))]);
                let result = dummy.forward(inputs).await;

                // Should always return something (default if not found)
                prop_assert!(result.is_ok());
            }

            Ok(())
        })?;
    }
}

// ============================================================================
// Invariant Tests
// ============================================================================

proptest! {
    #[test]
    fn test_call_count_monotonically_increases(
        responses in prop::collection::vec(field_map_strategy(), 1..5),
        call_sequence in prop::collection::vec(any::<bool>(), 1..20),
    ) {
        let rt = tokio::runtime::Runtime::new().unwrap();
        rt.block_on(async {
            let dummy = DummyLM::sequential(responses);
            let mut last_count = 0;

            for (i, should_call) in call_sequence.iter().enumerate() {
                if *should_call {
                    let inputs = HashMap::from([("prompt".to_string(), json!(format!("Q{}", i)))]);
                    let _ = dummy.forward(inputs).await;

                    let current_count = dummy.call_count();
                    prop_assert!(current_count > last_count);
                    last_count = current_count;
                }
            }

            Ok(())
        })?;
    }

    #[test]
    fn test_history_never_loses_entries(
        responses in prop::collection::vec(field_map_strategy(), 1..5),
        calls in 1usize..30,
    ) {
        let rt = tokio::runtime::Runtime::new().unwrap();
        rt.block_on(async {
            let dummy = DummyLM::sequential(responses);

            for i in 0..calls {
                let inputs = HashMap::from([("prompt".to_string(), json!(format!("Q{}", i)))]);
                let _ = dummy.forward(inputs).await;

                let history = dummy.history();

                // History should grow with each call
                prop_assert_eq!(history.len(), i + 1);
            }

            Ok(())
        })?;
    }
}
