//! Filesystem determinism control
//!
//! Provides filesystem isolation and read-only rootfs simulation for deterministic testing.

use crate::error::{CleanroomError, Result};
use std::collections::HashMap;
use std::path::{Path, PathBuf};

/// Controller for filesystem determinism
#[derive(Debug, Clone)]
pub struct FsController {
    profile: crate::policy::FsProfile,
    base_dir: PathBuf,
    mounts: HashMap<PathBuf, PathBuf>,
    read_only: bool,
}

impl FsController {
    /// Create a new filesystem controller with the given profile
    ///
    /// # Errors
    ///
    /// Returns an error if the base directory doesn't exist or is not valid.
    pub fn new(profile: crate::policy::FsProfile, base_dir: PathBuf) -> Result<Self> {
        // READY: Filesystem isolation with validation and security checks

        // Validate base directory exists and is a directory
        if !base_dir.exists() {
            return Err(CleanroomError::Io(std::io::Error::new(
                std::io::ErrorKind::NotFound,
                format!("Base directory does not exist: {}", base_dir.display()),
            )));
        }

        if !base_dir.is_dir() {
            return Err(CleanroomError::Io(std::io::Error::new(
                std::io::ErrorKind::InvalidInput,
                format!("Base path is not a directory: {}", base_dir.display()),
            )));
        }

        // Canonicalize base directory to resolve symlinks and get absolute path
        let base_dir = base_dir.canonicalize().map_err(CleanroomError::Io)?;

        let read_only = match profile {
            crate::policy::FsProfile::ReadOnly { .. } => true,
            crate::policy::FsProfile::Writable { .. } => false,
        };

        Ok(Self {
            profile,
            base_dir,
            mounts: HashMap::new(),
            read_only,
        })
    }

    /// Create a temporary directory for the workdir
    pub fn create_workdir(&self) -> Result<PathBuf> {
        let workdir = self.base_dir.join("workdir");
        std::fs::create_dir_all(&workdir)?;
        Ok(workdir)
    }

    /// Mount a host path to a container path
    pub fn mount(&mut self, host_path: PathBuf, container_path: PathBuf) -> Result<()> {
        if self.read_only {
            return Err(CleanroomError::Policy(
                crate::error::PolicyError::SecurityViolation(
                    "Cannot mount in read-only filesystem".into(),
                ),
            ));
        }

        self.mounts.insert(container_path, host_path);
        Ok(())
    }

    /// Check if a path is allowed for access
    pub fn is_allowed(&self, path: &Path) -> Result<bool> {
        match &self.profile {
            crate::policy::FsProfile::ReadOnly { workdir } => {
                // In read-only mode, only workdir and mounted paths are allowed
                let workdir_path = self.base_dir.join(workdir);
                let in_workdir = path.starts_with(&workdir_path);
                let in_mount = self
                    .mounts
                    .keys()
                    .any(|mount_path| path.starts_with(mount_path));
                Ok(in_workdir || in_mount)
            }
            crate::policy::FsProfile::Writable { workdir } => {
                // In writable mode, workdir is allowed for writing
                let workdir_path = self.base_dir.join(workdir);
                let in_workdir = path.starts_with(&workdir_path);
                Ok(in_workdir || path.starts_with(&self.base_dir))
            }
        }
    }

    /// Read a file (checking permissions)
    pub fn read_file(&self, path: &Path) -> Result<String> {
        if !self.is_allowed(path)? {
            return Err(CleanroomError::Policy(
                crate::error::PolicyError::SecurityViolation(format!(
                    "Access denied to path: {}",
                    path.display()
                )),
            ));
        }

        std::fs::read_to_string(path).map_err(CleanroomError::Io)
    }

    /// Write a file (checking permissions)
    pub fn write_file(&self, path: &Path, content: &str) -> Result<()> {
        if self.read_only && !self.is_allowed(path)? {
            return Err(CleanroomError::Policy(
                crate::error::PolicyError::SecurityViolation(format!(
                    "Write access denied to path: {}",
                    path.display()
                )),
            ));
        }

        // Ensure parent directory exists
        if let Some(parent) = path.parent() {
            std::fs::create_dir_all(parent)?;
        }

        std::fs::write(path, content).map_err(CleanroomError::Io)
    }

    /// List directory contents (checking permissions)
    pub fn list_dir(&self, path: &Path) -> Result<Vec<PathBuf>> {
        if !self.is_allowed(path)? {
            return Err(CleanroomError::Policy(
                crate::error::PolicyError::SecurityViolation(format!(
                    "Access denied to path: {}",
                    path.display()
                )),
            ));
        }

        let mut entries = Vec::new();
        for entry in std::fs::read_dir(path)? {
            let entry = entry?;
            entries.push(entry.path());
        }
        Ok(entries)
    }

    /// Get the current profile
    pub fn profile(&self) -> &crate::policy::FsProfile {
        &self.profile
    }

    /// Check if filesystem is read-only
    pub fn is_read_only(&self) -> bool {
        self.read_only
    }

    /// Get all mounts
    pub fn mounts(&self) -> &HashMap<PathBuf, PathBuf> {
        &self.mounts
    }

    /// Sanitize and validate a path to prevent directory traversal attacks
    ///
    /// Ensures the path is within the allowed boundaries and doesn't contain
    /// dangerous patterns like `..` or symlinks that escape the container.
    fn sanitize_path(&self, path: &Path) -> Result<PathBuf> {
        // Canonicalize the path to resolve symlinks and get absolute path
        let canonical = if path.exists() {
            path.canonicalize().map_err(CleanroomError::Io)?
        } else {
            // If path doesn't exist, we need to check its parent
            if let Some(parent) = path.parent() {
                if parent.exists() {
                    let canon_parent = parent.canonicalize().map_err(CleanroomError::Io)?;
                    if let Some(file_name) = path.file_name() {
                        canon_parent.join(file_name)
                    } else {
                        return Err(CleanroomError::Policy(
                            crate::error::PolicyError::SecurityViolation(
                                "Invalid path: missing file name".into(),
                            ),
                        ));
                    }
                } else {
                    return Err(CleanroomError::Io(std::io::Error::new(
                        std::io::ErrorKind::NotFound,
                        format!("Parent directory does not exist: {}", parent.display()),
                    )));
                }
            } else {
                return Err(CleanroomError::Policy(
                    crate::error::PolicyError::SecurityViolation(
                        "Invalid path: no parent directory".into(),
                    ),
                ));
            }
        };

        // Check if the canonical path is within the base directory
        if !canonical.starts_with(&self.base_dir) {
            return Err(CleanroomError::Policy(
                crate::error::PolicyError::SecurityViolation(format!(
                    "Path escape detected: {} is outside base directory {}",
                    canonical.display(),
                    self.base_dir.display()
                )),
            ));
        }

        Ok(canonical)
    }

    /// Check if a path contains dangerous patterns
    fn has_dangerous_patterns(&self, path: &Path) -> bool {
        let path_str = path.to_string_lossy();
        path_str.contains("..") || path_str.contains("~")
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::fs;
    use tempfile::TempDir;

    #[test]
    fn test_fs_controller_new_valid() {
        let temp_dir = TempDir::new().expect("failed to create temp dir");
        let profile = crate::policy::FsProfile::ReadOnly {
            workdir: "workdir".to_string(),
        };

        let controller =
            FsController::new(profile, temp_dir.path().to_path_buf()).expect("should succeed");
        assert!(controller.is_read_only());
    }

    #[test]
    fn test_fs_controller_new_nonexistent_dir() {
        let profile = crate::policy::FsProfile::ReadOnly {
            workdir: "workdir".to_string(),
        };

        let result = FsController::new(profile, PathBuf::from("/nonexistent/directory"));
        assert!(result.is_err());
    }

    #[test]
    fn test_fs_controller_new_file_instead_of_dir() {
        let temp_dir = TempDir::new().expect("failed to create temp dir");
        let file_path = temp_dir.path().join("file.txt");
        fs::write(&file_path, "test").expect("failed to write file");

        let profile = crate::policy::FsProfile::ReadOnly {
            workdir: "workdir".to_string(),
        };

        let result = FsController::new(profile, file_path);
        assert!(result.is_err());
    }

    #[test]
    fn test_fs_controller_create_workdir() {
        let temp_dir = TempDir::new().expect("failed to create temp dir");
        let profile = crate::policy::FsProfile::ReadOnly {
            workdir: "workdir".to_string(),
        };

        let controller =
            FsController::new(profile, temp_dir.path().to_path_buf()).expect("should succeed");
        let workdir = controller.create_workdir().expect("should create workdir");

        assert!(workdir.exists());
        assert!(workdir.is_dir());
    }

    #[test]
    fn test_fs_controller_mount_readonly_fails() {
        let temp_dir = TempDir::new().expect("failed to create temp dir");
        let profile = crate::policy::FsProfile::ReadOnly {
            workdir: "workdir".to_string(),
        };

        let mut controller =
            FsController::new(profile, temp_dir.path().to_path_buf()).expect("should succeed");

        let result = controller.mount(PathBuf::from("/host"), PathBuf::from("/container"));
        assert!(result.is_err());
    }

    #[test]
    fn test_fs_controller_mount_writable_succeeds() {
        let temp_dir = TempDir::new().expect("failed to create temp dir");
        let profile = crate::policy::FsProfile::Writable {
            workdir: "workdir".to_string(),
        };

        let mut controller =
            FsController::new(profile, temp_dir.path().to_path_buf()).expect("should succeed");

        let result = controller.mount(PathBuf::from("/host"), PathBuf::from("/container"));
        assert!(result.is_ok());
        assert_eq!(controller.mounts().len(), 1);
    }

    #[test]
    fn test_fs_controller_is_allowed_readonly() {
        let temp_dir = TempDir::new().expect("failed to create temp dir");
        let profile = crate::policy::FsProfile::ReadOnly {
            workdir: "workdir".to_string(),
        };

        let controller =
            FsController::new(profile, temp_dir.path().to_path_buf()).expect("should succeed");

        let workdir_path = temp_dir.path().join("workdir");
        fs::create_dir_all(&workdir_path).expect("failed to create workdir");

        // Workdir should be allowed
        assert!(controller
            .is_allowed(&workdir_path)
            .expect("should succeed"));

        // Path outside workdir should not be allowed
        let outside_path = temp_dir.path().join("outside");
        assert!(!controller
            .is_allowed(&outside_path)
            .expect("should succeed"));
    }

    #[test]
    fn test_fs_controller_read_file() {
        let temp_dir = TempDir::new().expect("failed to create temp dir");
        let profile = crate::policy::FsProfile::ReadOnly {
            workdir: "workdir".to_string(),
        };

        let controller =
            FsController::new(profile, temp_dir.path().to_path_buf()).expect("should succeed");

        // Create a file in workdir
        let workdir_path = temp_dir.path().join("workdir");
        fs::create_dir_all(&workdir_path).expect("failed to create workdir");
        let file_path = workdir_path.join("test.txt");
        fs::write(&file_path, "test content").expect("failed to write file");

        // Should be able to read from workdir
        let content = controller.read_file(&file_path).expect("should read file");
        assert_eq!(content, "test content");
    }

    #[test]
    fn test_fs_controller_write_file_readonly_fails() {
        let temp_dir = TempDir::new().expect("failed to create temp dir");
        let profile = crate::policy::FsProfile::ReadOnly {
            workdir: "workdir".to_string(),
        };

        let controller =
            FsController::new(profile, temp_dir.path().to_path_buf()).expect("should succeed");

        let outside_path = temp_dir.path().join("outside.txt");
        let result = controller.write_file(&outside_path, "test");
        assert!(result.is_err());
    }

    #[test]
    fn test_fs_controller_write_file_writable_succeeds() {
        let temp_dir = TempDir::new().expect("failed to create temp dir");
        let profile = crate::policy::FsProfile::Writable {
            workdir: "workdir".to_string(),
        };

        let controller =
            FsController::new(profile, temp_dir.path().to_path_buf()).expect("should succeed");

        let workdir_path = temp_dir.path().join("workdir");
        fs::create_dir_all(&workdir_path).expect("failed to create workdir");
        let file_path = workdir_path.join("test.txt");

        controller
            .write_file(&file_path, "test content")
            .expect("should write file");

        assert!(file_path.exists());
        let content = fs::read_to_string(&file_path).expect("should read file");
        assert_eq!(content, "test content");
    }

    #[test]
    fn test_fs_controller_list_dir() {
        let temp_dir = TempDir::new().expect("failed to create temp dir");
        let profile = crate::policy::FsProfile::ReadOnly {
            workdir: "workdir".to_string(),
        };

        let controller =
            FsController::new(profile, temp_dir.path().to_path_buf()).expect("should succeed");

        // Create workdir with files
        let workdir_path = temp_dir.path().join("workdir");
        fs::create_dir_all(&workdir_path).expect("failed to create workdir");
        fs::write(workdir_path.join("file1.txt"), "test1").expect("failed to write");
        fs::write(workdir_path.join("file2.txt"), "test2").expect("failed to write");

        let entries = controller.list_dir(&workdir_path).expect("should list dir");
        assert_eq!(entries.len(), 2);
    }

    #[test]
    fn test_fs_controller_sanitize_path_prevents_escape() {
        let temp_dir = TempDir::new().expect("failed to create temp dir");
        let profile = crate::policy::FsProfile::ReadOnly {
            workdir: "workdir".to_string(),
        };

        let controller =
            FsController::new(profile, temp_dir.path().to_path_buf()).expect("should succeed");

        // Create workdir
        let workdir_path = temp_dir.path().join("workdir");
        fs::create_dir_all(&workdir_path).expect("failed to create workdir");

        // Try to escape using ..
        let escape_path = workdir_path
            .join("..")
            .join("..")
            .join("etc")
            .join("passwd");
        let result = controller.sanitize_path(&escape_path);

        // Should fail if path escapes base directory
        if let Ok(sanitized) = result {
            assert!(
                sanitized.starts_with(&controller.base_dir),
                "Sanitized path should be within base dir"
            );
        } else {
            // Or it should return an error
            assert!(result.is_err());
        }
    }

    #[test]
    fn test_fs_controller_has_dangerous_patterns() {
        let temp_dir = TempDir::new().expect("failed to create temp dir");
        let profile = crate::policy::FsProfile::ReadOnly {
            workdir: "workdir".to_string(),
        };

        let controller =
            FsController::new(profile, temp_dir.path().to_path_buf()).expect("should succeed");

        assert!(controller.has_dangerous_patterns(&PathBuf::from("../etc/passwd")));
        assert!(controller.has_dangerous_patterns(&PathBuf::from("~/secret")));
        assert!(!controller.has_dangerous_patterns(&PathBuf::from("workdir/file.txt")));
    }
}
