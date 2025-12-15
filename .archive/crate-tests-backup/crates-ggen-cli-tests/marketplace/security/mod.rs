//! Security tests for marketplace functionality
//!
//! This module contains consolidated security-focused tests:
//! - Input validation and sanitization
//! - Injection prevention (SQL, path traversal, XSS)
//! - Authentication/authorization
//! - Cryptographic verification (Ed25519 signatures)
//! - Vulnerability impact assessment
//! - Score validation and overflow prevention
//! - Tamper detection

mod consolidated_security;
