//! Security Integration Test Suite
//!
//! End-to-end security tests for ggen v6.0.0
//!
//! Test Categories:
//! 1. Path Traversal Prevention - File system security
//! 2. SPARQL Injection Prevention - Query injection attacks
//! 3. Rate Limiting - DoS prevention with Redis
//! 4. Input Validation - Malicious input handling
//! 5. Secrets Protection - Credential leakage prevention
//!
//! All tests follow Chicago TDD principles:
//! - AAA pattern (Arrange/Act/Assert)
//! - Real collaborators (actual file system, RDF stores, Redis)
//! - State-based verification (observable outputs)
//! - No mocks for core functionality

mod path_traversal_tests;
mod sparql_injection_tests;
mod rate_limit_integration_tests;
mod input_validation_tests;
mod secrets_protection_tests;
