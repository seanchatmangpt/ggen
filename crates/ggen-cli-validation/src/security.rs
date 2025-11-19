//! Security and permission model for CLI operations
//!
//! Implements a permission-based security model for validating
//! read, write, and execute operations.

use crate::error::{Result, ValidationError};
use std::path::{Path, PathBuf};

/// Permission types for operations
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Permission {
    /// Read permission
    Read,
    /// Write permission
    Write,
    /// Execute permission (for running commands)
    Execute,
}

/// Permission model for validating operations
#[derive(Debug, Clone)]
pub struct PermissionModel {
    /// Allowed read paths (patterns)
    allowed_read_paths: Vec<PathBuf>,
    /// Allowed write paths (patterns)
    allowed_write_paths: Vec<PathBuf>,
    /// Sandbox root (if enabled)
    sandbox_root: Option<PathBuf>,
    /// Environment variable restrictions
    restricted_env_vars: Vec<String>,
}

impl Default for PermissionModel {
    fn default() -> Self {
        Self::new()
    }
}

impl PermissionModel {
    /// Create a new permission model with permissive default settings
    /// Allows all operations unless explicitly restricted by sandbox
    #[must_use]
    pub fn new() -> Self {
        Self {
            allowed_read_paths: vec![],  // Empty means allow all
            allowed_write_paths: vec![], // Empty means allow all
            sandbox_root: None,
            restricted_env_vars: vec!["PATH".to_string(), "HOME".to_string(), "USER".to_string()],
        }
    }

    /// Enable sandbox mode with a root directory
    #[must_use]
    pub fn with_sandbox(mut self, root: PathBuf) -> Self {
        self.sandbox_root = Some(root);
        self
    }

    /// Add allowed read path
    #[must_use]
    pub fn allow_read(mut self, path: PathBuf) -> Self {
        self.allowed_read_paths.push(path);
        self
    }

    /// Add allowed write path
    #[must_use]
    pub fn allow_write(mut self, path: PathBuf) -> Self {
        self.allowed_write_paths.push(path);
        self
    }

    /// Check if a path is allowed for the given permission
    pub fn check_permission(&self, path: &Path, permission: Permission) -> Result<()> {
        // First check path traversal
        self.check_path_traversal(path)?;

        // Check sandbox constraints
        if let Some(sandbox_root) = &self.sandbox_root {
            self.check_sandbox(path, sandbox_root)?;
        }

        // Check specific permission
        match permission {
            Permission::Read => self.check_read_permission(path),
            Permission::Write => self.check_write_permission(path),
            Permission::Execute => Ok(()), // Execute permissions handled separately
        }
    }

    /// Check for path traversal attempts
    fn check_path_traversal(&self, path: &Path) -> Result<()> {
        let path_str = path.to_string_lossy();

        // Check for dangerous path traversal patterns (../ going up directories)
        if path_str.contains("../") || path_str.starts_with("..") {
            return Err(ValidationError::PathTraversal {
                path: path_str.to_string(),
            });
        }

        Ok(())
    }

    /// Check sandbox constraints
    fn check_sandbox(&self, path: &Path, sandbox_root: &Path) -> Result<()> {
        let canonical_path = path.canonicalize().or_else(|_| {
            // If path doesn't exist yet, check parent
            path.parent()
                .and_then(|p| p.canonicalize().ok())
                .ok_or(ValidationError::InvalidPath {
                    path: path.display().to_string(),
                    reason: "Cannot canonicalize path".to_string(),
                })
        })?;

        let canonical_root =
            sandbox_root
                .canonicalize()
                .map_err(|e| ValidationError::InvalidPath {
                    path: sandbox_root.display().to_string(),
                    reason: format!("Cannot canonicalize sandbox root: {e}"),
                })?;

        if !canonical_path.starts_with(&canonical_root) {
            return Err(ValidationError::SandboxViolation {
                reason: format!(
                    "Path {} is outside sandbox {}",
                    canonical_path.display(),
                    canonical_root.display()
                ),
            });
        }

        Ok(())
    }

    /// Check read permission for path
    fn check_read_permission(&self, path: &Path) -> Result<()> {
        if self.is_path_allowed(path, &self.allowed_read_paths) {
            Ok(())
        } else {
            Err(ValidationError::PermissionDenied {
                operation: "read".to_string(),
                path: path.display().to_string(),
            })
        }
    }

    /// Check write permission for path
    fn check_write_permission(&self, path: &Path) -> Result<()> {
        if self.is_path_allowed(path, &self.allowed_write_paths) {
            Ok(())
        } else {
            Err(ValidationError::PermissionDenied {
                operation: "write".to_string(),
                path: path.display().to_string(),
            })
        }
    }

    /// Check if path matches any allowed patterns
    /// Empty allowed_paths means allow all (permissive default)
    fn is_path_allowed(&self, path: &Path, allowed_paths: &[PathBuf]) -> bool {
        if allowed_paths.is_empty() {
            return true; // Permissive default
        }
        allowed_paths.iter().any(|allowed| {
            // Simple prefix matching for now
            path.starts_with(allowed)
        })
    }

    /// Check if environment variable is restricted
    #[must_use]
    pub fn is_env_var_restricted(&self, var: &str) -> bool {
        self.restricted_env_vars.contains(&var.to_string())
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::env;

    #[test]
    fn test_default_permission_model() {
        let model = PermissionModel::new();
        assert!(model.sandbox_root.is_none());
        // Permissive default - empty paths mean allow all
        assert!(model.allowed_read_paths.is_empty());
    }

    #[test]
    fn test_path_traversal_detection() {
        let model = PermissionModel::new();
        let traversal_path = Path::new("../../../etc/passwd");

        let result = model.check_path_traversal(traversal_path);
        assert!(result.is_err());
    }

    #[test]
    fn test_sandbox_enforcement() {
        let temp_dir = env::temp_dir();
        let model = PermissionModel::new().with_sandbox(temp_dir.clone());

        // Path inside sandbox should be allowed
        let inside_path = temp_dir.join("test.txt");
        assert!(model.check_sandbox(&inside_path, &temp_dir).is_ok());

        // Path outside sandbox should be denied
        let outside_path = Path::new("/etc/passwd");
        assert!(model.check_sandbox(outside_path, &temp_dir).is_err());
    }

    #[test]
    fn test_read_write_permissions() {
        // Create restrictive model with specific allowed paths
        let mut model = PermissionModel::new();
        model.allowed_read_paths = vec![PathBuf::from("./src")];
        model.allowed_write_paths = vec![PathBuf::from("./target")];

        // Read permission - allowed
        assert!(model
            .check_permission(Path::new("./src/lib.rs"), Permission::Read)
            .is_ok());

        // Write permission - allowed
        assert!(model
            .check_permission(Path::new("./target/output"), Permission::Write)
            .is_ok());

        // Read permission - denied (not in allowed paths)
        assert!(model
            .check_permission(Path::new("./target/lib.rs"), Permission::Read)
            .is_err());

        // Write permission - denied (not in allowed paths)
        assert!(model
            .check_permission(Path::new("./src/output"), Permission::Write)
            .is_err());
    }

    #[test]
    fn test_env_var_restrictions() {
        let model = PermissionModel::new();
        assert!(model.is_env_var_restricted("PATH"));
        assert!(model.is_env_var_restricted("HOME"));
        assert!(!model.is_env_var_restricted("MY_CUSTOM_VAR"));
    }
}
