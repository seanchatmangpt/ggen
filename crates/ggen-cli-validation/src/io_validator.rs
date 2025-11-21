//! IO validation for file operations
//!
//! Validates read and write operations before execution,
//! preventing common errors and security issues.

use crate::error::{Result, ValidationError};
use crate::security::{Permission, PermissionModel};
use std::fs;
use std::path::Path;

/// IO validator for file operations
#[derive(Debug)]
pub struct IoValidator {
    /// Permission model for security checks
    permission_model: PermissionModel,
}

impl Default for IoValidator {
    fn default() -> Self {
        Self::new()
    }
}

impl IoValidator {
    /// Create a new IO validator with default permission model
    #[must_use]
    pub fn new() -> Self {
        Self {
            permission_model: PermissionModel::new(),
        }
    }

    /// Create an IO validator with custom permission model
    #[must_use]
    pub fn with_permissions(permission_model: PermissionModel) -> Self {
        Self { permission_model }
    }

    /// Validate a read operation
    ///
    /// Checks:
    /// - File exists
    /// - Read permissions
    /// - Path traversal prevention
    pub fn validate_read(&self, path: &Path) -> Result<()> {
        // Check permissions first
        self.permission_model
            .check_permission(path, Permission::Read)?;

        // Check file exists
        if !path.exists() {
            return Err(ValidationError::FileNotFound {
                path: path.display().to_string(),
            });
        }

        // Check it's actually a file (not a directory)
        if !path.is_file() {
            return Err(ValidationError::InvalidPath {
                path: path.display().to_string(),
                reason: "Path is not a file".to_string(),
            });
        }

        // Try to open for reading
        fs::File::open(path).map_err(|e| ValidationError::ReadFailed {
            path: path.display().to_string(),
            reason: e.to_string(),
        })?;

        Ok(())
    }

    /// Validate a write operation
    ///
    /// Checks:
    /// - Parent directory exists
    /// - Write permissions
    /// - Path traversal prevention
    pub fn validate_write(&self, path: &Path) -> Result<()> {
        // Check permissions first
        self.permission_model
            .check_permission(path, Permission::Write)?;

        // Check parent directory exists
        if let Some(parent) = path.parent() {
            if !parent.exists() {
                return Err(ValidationError::InvalidPath {
                    path: path.display().to_string(),
                    reason: format!("Parent directory {} does not exist", parent.display()),
                });
            }

            // Check parent is writable
            if let Ok(metadata) = fs::metadata(parent) {
                if metadata.permissions().readonly() {
                    return Err(ValidationError::WriteFailed {
                        path: path.display().to_string(),
                        reason: "Parent directory is read-only".to_string(),
                    });
                }
            }
        }

        Ok(())
    }

    /// Validate multiple read operations
    pub fn validate_reads(&self, paths: &[&Path]) -> Result<Vec<PathValidation>> {
        Ok(paths
            .iter()
            .map(|path| {
                let result = self.validate_read(path);
                PathValidation {
                    path: path.to_path_buf(),
                    valid: result.is_ok(),
                    error: result.err(),
                }
            })
            .collect())
    }

    /// Validate multiple write operations
    pub fn validate_writes(&self, paths: &[&Path]) -> Result<Vec<PathValidation>> {
        Ok(paths
            .iter()
            .map(|path| {
                let result = self.validate_write(path);
                PathValidation {
                    path: path.to_path_buf(),
                    valid: result.is_ok(),
                    error: result.err(),
                }
            })
            .collect())
    }
}

/// Result of path validation
#[derive(Debug)]
pub struct PathValidation {
    /// The path being validated
    pub path: std::path::PathBuf,
    /// Whether the path is valid
    pub valid: bool,
    /// Error if validation failed
    pub error: Option<ValidationError>,
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::fs::File;
    use std::io::Write;
    use tempfile::tempdir;

    #[allow(clippy::expect_used)]
    #[test]
    fn test_validate_read_existing_file() {
        let dir = tempdir().expect("Failed to create temp dir");
        let file_path = dir.path().join("test.txt");
        let mut file = File::create(&file_path).expect("Failed to create file");
        writeln!(file, "test content").expect("Failed to write");

        let validator = IoValidator::new();
        assert!(validator.validate_read(&file_path).is_ok());
    }

    #[allow(clippy::expect_used)]
    #[test]
    fn test_validate_read_missing_file() {
        let validator = IoValidator::new();
        let result = validator.validate_read(Path::new("/nonexistent/file.txt"));
        assert!(result.is_err());
        assert!(matches!(result, Err(ValidationError::FileNotFound { .. })));
    }

    #[allow(clippy::expect_used)]
    #[test]
    fn test_validate_write_existing_directory() {
        let dir = tempdir().expect("Failed to create temp dir");
        let file_path = dir.path().join("output.txt");

        let validator = IoValidator::new();
        assert!(validator.validate_write(&file_path).is_ok());
    }

    #[allow(clippy::expect_used)]
    #[test]
    fn test_validate_write_missing_parent() {
        let validator = IoValidator::new();
        let result = validator.validate_write(Path::new("/nonexistent/dir/file.txt"));
        assert!(result.is_err());
    }

    #[allow(clippy::expect_used)]
    #[test]
    fn test_batch_read_validation() {
        let dir = tempdir().expect("Failed to create temp dir");
        let file1 = dir.path().join("file1.txt");
        let file2 = dir.path().join("file2.txt");

        File::create(&file1).expect("Failed to create file1");
        File::create(&file2).expect("Failed to create file2");

        let validator = IoValidator::new();
        let paths = vec![file1.as_path(), file2.as_path()];
        let results = validator.validate_reads(&paths);

        assert!(results.is_ok());
        let validations = results.expect("Validation should succeed");
        assert_eq!(validations.len(), 2);
        assert!(validations.iter().all(|v| v.valid));
    }
}
