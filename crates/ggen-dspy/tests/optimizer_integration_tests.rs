//! Optimizer integration tests — BootstrapFewShot and MiproOptimizer compile() pipelines
//!
//! These tests exercise the full `compile()` loop against a `MockModule` without
//! hitting a live LLM.  Each test follows the Chicago TDD Arrange-Act-Assert pattern.

use ggen_dspy::testing::{test_output, MockModule};
use ggen_dspy::{
    BootstrapFewShot, MiproOptimizer, Module, Optimizer, OptimizerConfig, TrainExample,
};

// ============================================================================
// Test 1 — BootstrapFewShot collects demos when module output matches trainset
// ============================================================================

/// A `MockModule` that returns a fixed output matching the trainset expected output
/// triggers `BootstrapFewShot::compile()` to collect at least one demonstration.
/// The returned module's name must contain "+Bootstrap".
#[tokio::test]
async fn test_bootstrap_compile_collects_demos() {
    // Arrange
    let output = test_output(&[("answer", "42")]);
    let module = MockModule::new("quiz").with_output(output);

    let trainset = vec![TrainExample::from_pairs(
        &[("question", "What is 6 * 7?")],
        &[("answer", "42")],
    )];

    let optimizer = BootstrapFewShot::new(OptimizerConfig::default().with_num_examples(5));

    // Act
    let compiled = optimizer
        .compile(Box::new(module), &trainset)
        .await
        .expect("compile must succeed");

    // Assert — demonstrations were collected so the wrapper has "+Bootstrap" in its name
    assert!(
        compiled.name().contains("+Bootstrap"),
        "compiled module name '{}' does not contain '+Bootstrap'",
        compiled.name()
    );
}

// ============================================================================
// Test 2 — BootstrapFewShot returns module unchanged for an empty trainset
// ============================================================================

/// When the trainset is empty `BootstrapFewShot::compile()` must return the
/// original module without wrapping it.
#[tokio::test]
async fn test_bootstrap_compile_empty_trainset() {
    // Arrange
    let module = MockModule::new("base-module");
    let original_name = module.name().to_string();
    let optimizer = BootstrapFewShot::default_config();

    // Act
    let compiled = optimizer
        .compile(Box::new(module), &[])
        .await
        .expect("compile must succeed even with empty trainset");

    // Assert — module is returned unchanged (same name, no "+Bootstrap" wrapper)
    assert_eq!(
        compiled.name(),
        original_name,
        "module should be unchanged when trainset is empty"
    );
    assert!(
        !compiled.name().contains("+Bootstrap"),
        "module must not be wrapped when trainset is empty"
    );
}

// ============================================================================
// Test 3 — MiproOptimizer returns module unchanged when no model is configured
// ============================================================================

/// `MiproOptimizer` must return the module unchanged and not panic when neither
/// `GGEN_LLM_MODEL`/`DEFAULT_MODEL` env vars nor `.with_model()` are set.
#[tokio::test]
async fn test_mipro_compile_no_model_returns_unchanged() {
    // Arrange — ensure no model env vars are set
    // (we use a unique module name so we can verify identity)
    let module = MockModule::new("no-model-module");
    let original_name = module.name().to_string();

    // Temporarily remove model env vars (they may not be set in CI anyway)
    let saved_model = std::env::var("GGEN_LLM_MODEL").ok();
    let saved_default = std::env::var("DEFAULT_MODEL").ok();
    std::env::remove_var("GGEN_LLM_MODEL");
    std::env::remove_var("DEFAULT_MODEL");

    let optimizer = MiproOptimizer::default_config();

    // Act
    let compiled = optimizer
        .compile(Box::new(module), &[])
        .await
        .expect("compile must not return Err even with no model");

    // Restore env vars
    if let Some(v) = saved_model {
        std::env::set_var("GGEN_LLM_MODEL", v);
    }
    if let Some(v) = saved_default {
        std::env::set_var("DEFAULT_MODEL", v);
    }

    // Assert — module returned unchanged
    assert_eq!(
        compiled.name(),
        original_name,
        "module should be unchanged when no model is configured"
    );
    assert!(
        !compiled.name().contains("+MIPRO"),
        "module must not be wrapped when no model is configured"
    );
}

// ============================================================================
// Test 4 — MiproOptimizer wraps module in InstructedModule when given a model
// ============================================================================

/// When a model is set and instruction candidates are injected via
/// `with_preset_candidates` (test-only bypass), `compile()` must wrap the
/// module in an `InstructedModule` whose name contains "+MIPRO".
#[tokio::test]
async fn test_mipro_compile_with_model_wraps_in_instructed_module() {
    // Arrange
    let output = test_output(&[("answer", "yes")]);
    let module = MockModule::new("mipro-subject").with_output(output);

    let trainset = vec![TrainExample::from_pairs(
        &[("question", "Is Rust fast?")],
        &[("answer", "yes")],
    )];

    // Inject a preset candidate so we bypass the live LLM proposal step.
    let optimizer = MiproOptimizer::default_config()
        .with_model("test-model-placeholder")
        .with_preset_candidates(vec!["Be concise and accurate.".to_string()]);

    // Act
    let compiled = optimizer
        .compile(Box::new(module), &trainset)
        .await
        .expect("compile must succeed with preset candidates");

    // Assert — module is wrapped in InstructedModule ("+MIPRO" suffix)
    assert!(
        compiled.name().contains("+MIPRO"),
        "compiled module name '{}' does not contain '+MIPRO'",
        compiled.name()
    );
}
