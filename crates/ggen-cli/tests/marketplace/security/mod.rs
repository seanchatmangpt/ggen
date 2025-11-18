//! Security tests for marketplace functionality
//!
//! This module contains security-focused tests:
//! - Input validation and sanitization
//! - Injection prevention
//! - Authentication/authorization
//! - Cryptographic verification
//! - Vulnerability impact assessment

// Existing tests
mod validation_test;

// V2 security tests
mod ed25519_signature_test;
