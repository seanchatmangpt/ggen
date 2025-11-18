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

// Existing Chicago TDD tests
mod install_tests;
mod registry_tests;

// New comprehensive test suite
pub mod fixtures;
pub mod integration;
pub mod performance;
pub mod security;
pub mod unit;
