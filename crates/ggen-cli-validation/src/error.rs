//! Validation error types

use thiserror::Error;

#[derive(Error, Debug)]
pub enum ValidationError {
    #[error("File not found: {path}")]
    FileNotFound { path: String },

    #[error("Permission denied: {operation} on {path}")]
    PermissionDenied { operation: String, path: String },

    #[error("Path traversal attempt detected: {path}")]
    PathTraversal { path: String },

    #[error("Invalid path: {path} - {reason}")]
    InvalidPath { path: String, reason: String },

    #[error("Write operation failed: {path} - {reason}")]
    WriteFailed { path: String, reason: String },

    #[error("Read operation failed: {path} - {reason}")]
    ReadFailed { path: String, reason: String },

    #[error("Circular dependency detected in command chain")]
    CircularDependency,

    #[error("Invalid command structure: {reason}")]
    InvalidCommandStructure { reason: String },

    #[error("Sandbox violation: {reason}")]
    SandboxViolation { reason: String },

    #[error("IO error: {0}")]
    Io(#[from] std::io::Error),
}

pub type Result<T> = std::result::Result<T, ValidationError>;
