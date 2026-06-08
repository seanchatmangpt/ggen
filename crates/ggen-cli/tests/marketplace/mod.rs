#![allow(
    clippy::unwrap_used,
    clippy::expect_used,
    clippy::panic,
    clippy::needless_raw_string_hashes,
    clippy::duration_suboptimal_units,
    clippy::branches_sharing_code,
    clippy::used_underscore_binding,
    clippy::single_char_pattern,
    clippy::ignore_without_reason,
    clippy::cloned_ref_to_slice_refs,
    clippy::doc_overindented_list_items,
    clippy::match_wildcard_for_single_variants,
    clippy::ignored_unit_patterns,
    clippy::needless_collect,
    clippy::unnecessary_map_or,
    clippy::manual_flatten,
    clippy::manual_strip,
    clippy::future_not_send,
    clippy::unnested_or_patterns,
    clippy::no_effect_underscore_binding,
    clippy::literal_string_with_formatting_args
)]
//! Comprehensive test suite for marketplace commands
//!
//! This test suite follows London School TDD principles:
//! - **Unit Tests**: Test individual components in isolation
//! - **Integration Tests**: Test complete workflows end-to-end
//! - **Performance Tests**: Verify scalability and speed
//! - **Security Tests**: Validate input handling and safety
//!
//! ## Test Organization
//!
//! ```text
//! marketplace/
//! ├── unit/               # Fast, isolated component tests
//! │   ├── maturity_scoring_test.rs
//! │   ├── search_ranking_test.rs
//! │   └── package_filtering_test.rs
//! ├── integration/        # CLI command integration tests
//! │   ├── cli_commands_test.rs
//! │   └── edge_cases_test.rs
//! ├── performance/        # Benchmarks and load tests
//! │   └── benchmark_test.rs
//! ├── security/           # Security validation tests
//! │   └── validation_test.rs
//! └── fixtures/           # Test data and helpers
//!     └── mod.rs
//! ```
//!
//! ## Running Tests
//!
//! ```bash
//! # Run all marketplace tests
//! cargo test --test marketplace
//!
//! # Run only unit tests
//! cargo test --test marketplace unit::
//!
//! # Run only integration tests
//! cargo test --test marketplace integration::
//!
//! # Run specific test category
//! cargo test --test marketplace performance::
//! cargo test --test marketplace security::
//! ```
//!
//! ## Test Coverage
//!
//! The test suite covers:
//!
//! ### Unit Tests (80+ tests)
//! - Maturity scoring algorithm (6 dimensions)
//! - Search ranking (relevance, popularity, quality, recency)
//! - Package filtering (by level, score, dimensions, use case)
//!
//! ### Integration Tests (50+ tests)
//! - All CLI commands (list, search, maturity, validate, export, compare, recommend)
//! - Edge cases (empty input, special characters, extreme values)
//! - Error handling and recovery
//!
//! ### Performance Tests (15+ tests)
//! - Search performance with 100+ packages
//! - Batch assessment benchmarks
//! - Report generation speed
//! - Memory efficiency
//!
//! ### Security Tests (20+ tests)
//! - Input validation and sanitization
//! - Injection prevention (SQL, XSS, path traversal)
//! - Score overflow protection
//! - Vulnerability impact assessment

// NOTE: Marketplace tests have been disabled pending migration to ggen-marketplace-v2 only.
// The v1 marketplace crate has been removed from the workspace.
// These tests should be reimplemented to use v2 RDF-backed marketplace.

// Existing Chicago TDD tests (v1 - DISABLED)
// mod install_tests;
// mod registry_tests;

// New comprehensive test suite (v1 - DISABLED)
// pub mod fixtures;
// pub mod integration;
// pub mod performance;
// pub mod security;
// pub mod unit;
