//! DSPy Testing Infrastructure
//!
//! Provides comprehensive testing utilities for DSPy modules:
//! - `DummyLM` - Test-focused LM with 3 operating modes
//! - `ExampleBuilder` - Fluent API for creating test examples
//! - Test utilities - Factory functions, assertion helpers, golden tests
//!
//! Based on Python DSPy testing patterns adapted for Rust with Chicago TDD principles.

pub mod assertions;
pub mod dummy_lm;
pub mod example_builder;
pub mod factories;
pub mod golden;

pub use assertions::{
    assert_demonstration_valid, assert_example_valid, assert_metric_filters_correctly,
    assert_output_matches_signature,
};
pub use dummy_lm::{CallHistory, DummyLM, DummyLMMode};
pub use example_builder::ExampleBuilder;
pub use factories::{
    classification_example, create_classification_trainset, create_qa_trainset,
    create_test_signature, qa_example,
};
pub use golden::{compare_with_golden, save_golden, GoldenTest, GoldenTestRunner};
