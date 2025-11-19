//! Security module for input validation, command execution safety, and error sanitization
//!
//! This module provides comprehensive security mechanisms to prevent:
//! - Command injection attacks
//! - Path traversal vulnerabilities
//! - Information disclosure via error messages
//! - Malicious input exploitation
//!
//! ## Week 4 Security Hardening
//!
//! Target: 82% → 85% security health improvement
//!
//! Fixed issues:
//! 1. Panic in library code → Result-based error handling
//! 2. Unwrap() usage → Proper error propagation
//! 3. Command injection → Safe command execution
//! 4. Input validation → Comprehensive validation functions
//! 5. Error message leakage → Sanitized error messages

pub mod command;
pub mod error;
pub mod validation;

pub use command::{SafeCommand, CommandExecutor, CommandError};
pub use error::{SanitizedError, ErrorSanitizer};
pub use validation::{PathValidator, EnvVarValidator, InputValidator, ValidationError};

#[cfg(test)]
mod tests;
