//! Clap Integration Test Suite
//!
//! Comprehensive validation tests for clap-noun-verb CLI integration.
//! Organized following the 80/20 principle to focus on critical paths.
//!
//! ## Test Modules
//!
//! - `io_validation_tests` - File operations, path safety, permissions
//! - `noun_verb_validation_tests` - Command structure, circular deps, ordering
//! - `ggen_toml_integration_tests` - Config loading, overrides, validation
//! - `security_tests` - Path traversal, symlinks, injection prevention
//!
//! ## Running Tests
//!
//! ```bash
//! # Run all clap integration tests
//! cargo test --test 'clap/*'
//!
//! # Run specific test module
//! cargo test --test clap/io_validation_tests
//! cargo test --test clap/noun_verb_validation_tests
//! cargo test --test clap/ggen_toml_integration_tests
//! cargo test --test clap/security_tests
//! ```

mod io_validation_tests;
mod noun_verb_validation_tests;
mod ggen_toml_integration_tests;
mod security_tests;
