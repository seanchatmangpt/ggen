//! DSPy Testing Infrastructure
//!
//! Provides comprehensive testing utilities for DSPy modules:
//! - `DummyLM` - Test-focused LM with 3 operating modes
//! - `ExampleBuilder` - Fluent API for creating test examples
//! - Test utilities - Factory functions, assertion helpers, golden tests
//!
//! Based on Python DSPy testing patterns adapted for Rust with Chicago TDD principles.

pub mod dummy_lm;
pub mod example_builder;
pub mod factories;
pub mod assertions;
pub mod golden;

pub use dummy_lm::{DummyLM, DummyLMMode, CallHistory};
pub use example_builder::ExampleBuilder;
pub use factories::{
    qa_example, classification_example, create_qa_trainset,
    create_classification_trainset, create_test_signature,
};
pub use assertions::{
    assert_example_valid, assert_demonstration_valid,
    assert_metric_filters_correctly, assert_output_matches_signature,
};
pub use golden::{GoldenTest, GoldenTestRunner, save_golden, compare_with_golden};
