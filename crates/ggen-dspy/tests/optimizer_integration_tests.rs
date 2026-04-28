//! Optimizer integration tests — BootstrapFewShot and MiproOptimizer compile() pipelines
//!
//! NOTE: These tests are disabled pending feature stabilization.
//! The testing module requires live-llm-tests feature to be enabled.

#![cfg(any(test, feature = "live-llm-tests"))]

// Disabled optimizer tests - require feature stabilization
// See: crates/ggen-dspy/src/optimizers/ for actual implementations
// The MockModule and test_output fixtures require the live-llm-tests feature
// which is not enabled in the standard test build.