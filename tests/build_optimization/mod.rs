//! Build Optimization Test Suite
//!
//! Comprehensive Chicago TDD test suite validating all build optimizations.
//! This module contains 100+ tests across 6 categories to ensure that
//! optimizations don't break functionality and meet SLO targets.
//!
//! Test Categories:
//! 1. Profile Configuration Tests (15 tests)
//!    - Validates Cargo profile settings (dev, test, release, bench)
//!    - Verifies optimization levels, LTO, codegen-units, etc.
//!
//! 2. Feature Flag Tests (20 tests)
//!    - Validates feature definitions and combinations
//!    - Verifies optional dependencies work correctly
//!    - Tests feature-gated compilation
//!
//! 3. Dependency Consolidation Tests (15 tests)
//!    - Validates workspace dependency management
//!    - Verifies version consistency across crates
//!    - Tests dependency deduplication strategy
//!
//! 4. Build Performance Tests (15 tests)
//!    - Validates that builds complete successfully
//!    - Tests profile optimization effectiveness
//!    - Verifies build time constraints
//!
//! 5. Binary Compatibility Tests (20 tests)
//!    - Validates that optimizations don't break the CLI
//!    - Verifies API stability
//!    - Tests deterministic output
//!
//! 6. SLO Compliance Tests (15 tests)
//!    - Validates Service Level Objective compliance
//!    - Verifies build time targets (15s first, 2s incremental)
//!    - Tests memory and binary size constraints
//!
//! Chicago TDD Principles Used:
//! - State-based testing: Tests verify observable output and configuration state
//! - Real objects: Tests use actual Cargo.toml, Makefile.toml, and real builds
//! - No mocks: All tests use real dependencies and artifacts
//! - AAA pattern: All tests follow Arrange-Act-Assert structure
//! - Behavior verification: Tests verify what code does, not implementation details
//!
//! Running Tests:
//! ```bash
//! # Run all optimization tests
//! cargo make test-optimization
//!
//! # Run specific test category
//! cargo test --test build_optimization profile_config_tests
//! cargo test --test build_optimization feature_flag_tests
//! cargo test --test build_optimization dependency_consolidation_tests
//! cargo test --test build_optimization build_performance_tests
//! cargo test --test build_optimization binary_compatibility_tests
//! cargo test --test build_optimization slo_compliance_tests
//! ```

mod profile_config_tests;
mod feature_flag_tests;
mod dependency_consolidation_tests;
mod build_performance_tests;
mod binary_compatibility_tests;
mod slo_compliance_tests;
