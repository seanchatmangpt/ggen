//! # ggen-cli-validation
//!
//! IO validation and security for ggen CLI operations.
//!
//! Provides validation for:
//! - File read/write operations
//! - Permission enforcement
//! - Path traversal prevention
//! - Command structure validation

pub mod error;
pub mod io_validator;
pub mod noun_verb_validator;
pub mod security;

pub use error::ValidationError;
pub use io_validator::IoValidator;
pub use noun_verb_validator::NounVerbValidator;
pub use security::{Permission, PermissionModel};
