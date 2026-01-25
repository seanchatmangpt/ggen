//! FactoryPaaS Test Runner
//!
//! Workspace-level test runner for FactoryPaaS comprehensive test suite
//!
//! Run tests with:
//! ```bash
//! # All tests (fast)
//! cargo make test factory_paas
//!
//! # With load tests (slow)
//! cargo test --test factory_paas_runner -- --ignored --test-threads=1
//! ```

#[cfg(test)]
mod factory_paas;

// Re-export test modules for easy discovery
pub use factory_paas::*;

#[cfg(test)]
mod factory_paas_suite {
    use super::*;

    // Smoke test to verify module loads
    #[test]
    fn test_factory_paas_module_loads() {
        // This test just verifies the module structure is correct
        assert!(true, "FactoryPaaS test module loaded successfully");
    }
}
