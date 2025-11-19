//! # ggen-cli-validation
//!
//! IO validation and security for ggen CLI operations.
//!
//! Provides validation for:
//! - File read/write operations
//! - Permission enforcement
//! - Path traversal prevention
//! - Command structure validation

pub mod io_validator;
pub mod security;
pub mod noun_verb_validator;
pub mod error;

pub use io_validator::IoValidator;
pub use security::{PermissionModel, Permission};
pub use noun_verb_validator::NounVerbValidator;
pub use error::ValidationError;
