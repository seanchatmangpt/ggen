//! Comprehensive error types for E2E testing operations
//!
//! Provides type-safe error handling with context for all E2E operations.

use std::io;
use std::path::PathBuf;
use std::time::Duration;
use thiserror::Error;

/// Top-level E2E error type
#[derive(Debug, Error)]
pub enum E2EError {
    #[error("Platform error: {0}")]
    Platform(#[from] PlatformError),

    #[error("Fixture error: {0}")]
    Fixture(#[from] FixtureError),

    #[error("Golden file error: {0}")]
    Golden(#[from] GoldenError),

    #[error("Container error: {0}")]
    Container(#[from] ContainerError),

    #[error("Runner error: {0}")]
    Runner(#[from] RunnerError),

    #[error("Test timed out after {0:?}")]
    Timeout(Duration),

    #[error("Comparison error: {0}")]
    Comparison(String),

    #[error("IO error: {0}")]
    Io(#[from] io::Error),
}

/// Platform detection and capability errors
#[derive(Debug, Error)]
pub enum PlatformError {
    #[error("Unsupported OS: {0}")]
    UnsupportedOs(String),

    #[error("Unsupported architecture: {0}")]
    UnsupportedArch(String),

    #[error("Docker is not available on this platform")]
    DockerUnavailable,

    #[error("Failed to detect current platform: {0}")]
    DetectionFailed(String),
}

/// Test fixture loading and validation errors
#[derive(Debug, Error)]
pub enum FixtureError {
    #[error("Fixture not found: {0}")]
    NotFound(PathBuf),

    #[error("Invalid ggen.toml: {0}")]
    InvalidManifest(String),

    #[error("Missing required file: {0}")]
    MissingFile(PathBuf),

    #[error("Failed to copy fixture to temp directory: {0}")]
    CopyFailed(String),

    #[error("IO error: {0}")]
    Io(#[from] io::Error),

    #[error("Fixture configuration error: {0}")]
    Configuration(String),
}

/// Golden file comparison and validation errors
#[derive(Debug, Error)]
pub enum GoldenError {
    #[error("Golden file not found: {0}")]
    NotFound(PathBuf),

    #[error("Golden file mismatch for {file}: {reason}")]
    Mismatch { file: PathBuf, reason: String },

    #[error("Checksum mismatch: expected {expected}, got {actual}")]
    ChecksumMismatch { expected: String, actual: String },

    #[error("Failed to read golden file: {0}")]
    ReadFailed(#[from] io::Error),

    #[error("Failed to write golden file: {0}")]
    WriteFailed(String),

    #[error("Line ending normalization failed: {0}")]
    LineEndingNormalization(String),
}

/// Container lifecycle and execution errors
#[derive(Debug, Error)]
pub enum ContainerError {
    #[error("Failed to start container: {0}")]
    StartFailed(String),

    #[error("Container exited with non-zero code {0}: {1}")]
    ExitCode(i32, String),

    #[error("Failed to mount volume from {host} to {container}: {reason}")]
    VolumeMountFailed {
        host: PathBuf,
        container: PathBuf,
        reason: String,
    },

    #[error("Failed to inject ggen binary into container: {0}")]
    BinaryInjectionFailed(String),

    #[error("Container cleanup failed: {0}")]
    CleanupFailed(String),

    #[error("Failed to capture container logs: {0}")]
    LogCaptureFailed(String),

    #[error("Container operation timed out after {0:?}")]
    Timeout(Duration),

    #[error("Container configuration error: {0}")]
    Configuration(String),
}

/// Test runner and execution errors
#[derive(Debug, Error)]
pub enum RunnerError {
    #[error("ggen sync failed: {0}")]
    SyncFailed(String),

    #[error("Container error: {0}")]
    Container(#[from] ContainerError),

    #[error("Failed to locate ggen binary: {0}")]
    BinaryNotFound(String),

    #[error("Test execution error: {0}")]
    ExecutionFailed(String),

    #[error("Output capture failed: {0}")]
    OutputCaptureFailed(String),

    #[error("IO error: {0}")]
    Io(#[from] io::Error),

    #[error("Platform error: {0}")]
    Platform(#[from] PlatformError),

    #[error("Test runner configuration error: {0}")]
    Configuration(String),
}

/// Result type for E2E operations
pub type Result<T> = std::result::Result<T, E2EError>;

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_platform_error_display() {
        let err = PlatformError::UnsupportedOs("FreeBSD".to_string());
        assert_eq!(err.to_string(), "Unsupported OS: FreeBSD");
    }

    #[test]
    fn test_fixture_error_not_found() {
        let path = PathBuf::from("/nonexistent/fixture");
        let err = FixtureError::NotFound(path.clone());
        assert!(err.to_string().contains("Fixture not found"));
    }

    #[test]
    fn test_error_conversion() {
        let io_err = io::Error::new(io::ErrorKind::NotFound, "file not found");
        let fixture_err: FixtureError = io_err.into();
        assert!(fixture_err.to_string().contains("IO error"));
    }
}
