use ggen_utils::error::{Error, Result};
///! File system transaction support for atomic writes and rollback
///!
///! Provides bulletproof file operations with automatic cleanup on failure.
///! Constitutional Rule: No partial state - either all changes succeed or all are rolled back.
use std::collections::HashMap;
use std::fs;
use std::path::{Path, PathBuf};
use tempfile::NamedTempFile;

/// File operation tracking for rollback
#[derive(Debug, Clone)]
enum FileOperation {
    /// File was created (didn't exist before)
    Created { path: PathBuf },
    /// File was modified (backup saved at location)
    Modified { path: PathBuf, backup: PathBuf },
}

/// Transaction manager for atomic file operations
#[derive(Debug)]
pub struct FileTransaction {
    operations: Vec<FileOperation>,
    backup_dir: Option<PathBuf>,
    committed: bool,
}

impl FileTransaction {
    /// Create a new transaction
    pub fn new() -> Result<Self> {
        Ok(Self {
            operations: Vec::new(),
            backup_dir: None,
            committed: false,
        })
    }

    /// Create a new transaction with backup directory
    pub fn with_backup_dir(backup_dir: impl AsRef<Path>) -> Result<Self> {
        let backup_path = backup_dir.as_ref().to_path_buf();
        fs::create_dir_all(&backup_path).map_err(|e| {
            Error::new(&format!(
                "Failed to create backup directory {}: {}",
                backup_path.display(),
                e
            ))
        })?;

        Ok(Self {
            operations: Vec::new(),
            backup_dir: Some(backup_path),
            committed: false,
        })
    }

    /// Write file atomically - either succeeds completely or has no effect
    ///
    /// Uses temp file + rename for atomic operation:
    /// 1. Write to temporary file
    /// 2. Atomically rename to target (OS-level atomic operation)
    /// 3. Track operation for potential rollback
    pub fn write_file(&mut self, path: impl AsRef<Path>, content: &str) -> Result<()> {
        let path = path.as_ref();

        // Check if file exists (for rollback tracking)
        let existed = path.exists();

        // Create parent directories if needed
        if let Some(parent) = path.parent() {
            fs::create_dir_all(parent).map_err(|e| {
                Error::new(&format!(
                    "Failed to create parent directory {}: {}",
                    parent.display(),
                    e
                ))
            })?;
        }

        // Create backup if file exists
        let backup_path = if existed {
            let backup = self.create_backup(path)?;
            Some(backup)
        } else {
            None
        };

        // Write to temporary file in same directory (ensures same filesystem)
        let temp_dir = path.parent().unwrap_or_else(|| Path::new("."));
        let mut temp_file = NamedTempFile::new_in(temp_dir).map_err(|e| {
            Error::new(&format!(
                "Failed to create temporary file in {}: {}",
                temp_dir.display(),
                e
            ))
        })?;

        use std::io::Write;
        temp_file
            .write_all(content.as_bytes())
            .map_err(|e| Error::new(&format!("Failed to write to temporary file: {}", e)))?;

        // Atomic rename (this is the critical atomic operation)
        temp_file.persist(path).map_err(|e| {
            Error::new(&format!(
                "Failed to atomically write to {}: {}",
                path.display(),
                e
            ))
        })?;

        // Track operation for potential rollback
        let operation = if let Some(backup) = backup_path {
            FileOperation::Modified {
                path: path.to_path_buf(),
                backup,
            }
        } else {
            FileOperation::Created {
                path: path.to_path_buf(),
            }
        };
        self.operations.push(operation);

        Ok(())
    }

    /// Create backup of existing file
    fn create_backup(&self, path: &Path) -> Result<PathBuf> {
        let backup_path = if let Some(backup_dir) = &self.backup_dir {
            // Use dedicated backup directory
            let filename = path
                .file_name()
                .ok_or_else(|| Error::new(&format!("Invalid path: {}", path.display())))?;
            backup_dir.join(format!(
                "{}.backup.{}",
                filename.to_string_lossy(),
                chrono::Utc::now().timestamp()
            ))
        } else {
            // Use .backup suffix in same directory
            path.with_extension(format!(
                "{}.backup",
                path.extension().and_then(|e| e.to_str()).unwrap_or("txt")
            ))
        };

        fs::copy(path, &backup_path).map_err(|e| {
            Error::new(&format!(
                "Failed to create backup of {} to {}: {}",
                path.display(),
                backup_path.display(),
                e
            ))
        })?;

        Ok(backup_path)
    }

    /// Commit transaction - mark as successful
    ///
    /// After commit, backups are kept but rollback is disabled
    pub fn commit(mut self) -> Result<TransactionReceipt> {
        self.committed = true;

        let receipt = TransactionReceipt {
            files_created: self
                .operations
                .iter()
                .filter_map(|op| match op {
                    FileOperation::Created { path } => Some(path.clone()),
                    _ => None,
                })
                .collect(),
            files_modified: self
                .operations
                .iter()
                .filter_map(|op| match op {
                    FileOperation::Modified { path, .. } => Some(path.clone()),
                    _ => None,
                })
                .collect(),
            backups: self
                .operations
                .iter()
                .filter_map(|op| match op {
                    FileOperation::Modified { path, backup } => {
                        Some((path.clone(), backup.clone()))
                    }
                    _ => None,
                })
                .collect(),
        };

        Ok(receipt)
    }

    /// Rollback all operations on drop (if not committed)
    fn rollback(&mut self) {
        if self.committed {
            return;
        }

        // Rollback in reverse order
        for operation in self.operations.iter().rev() {
            match operation {
                FileOperation::Created { path } => {
                    // Remove created file
                    if let Err(e) = fs::remove_file(path) {
                        eprintln!(
                            "Warning: Failed to remove {} during rollback: {}",
                            path.display(),
                            e
                        );
                    }
                }
                FileOperation::Modified { path, backup } => {
                    // Restore from backup
                    if let Err(e) = fs::copy(backup, path) {
                        eprintln!(
                            "Warning: Failed to restore {} from backup during rollback: {}",
                            path.display(),
                            e
                        );
                    }
                    // Clean up backup
                    let _ = fs::remove_file(backup);
                }
            }
        }
    }
}

impl Drop for FileTransaction {
    fn drop(&mut self) {
        if !self.committed {
            self.rollback();
        }
    }
}

/// Receipt of completed transaction
#[derive(Debug, Clone)]
pub struct TransactionReceipt {
    pub files_created: Vec<PathBuf>,
    pub files_modified: Vec<PathBuf>,
    pub backups: HashMap<PathBuf, PathBuf>,
}

impl TransactionReceipt {
    /// Clean up backups after successful operation
    pub fn clean_backups(&self) -> Result<()> {
        for backup in self.backups.values() {
            if let Err(e) = fs::remove_file(backup) {
                eprintln!(
                    "Warning: Failed to remove backup {}: {}",
                    backup.display(),
                    e
                );
            }
        }
        Ok(())
    }

    /// Total files affected
    pub fn total_files(&self) -> usize {
        self.files_created.len() + self.files_modified.len()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::fs;
    use tempfile::tempdir;

    #[test]
    fn test_atomic_write_new_file() {
        let dir = tempdir().unwrap();
        let file_path = dir.path().join("test.txt");

        let mut tx = FileTransaction::new().unwrap();
        tx.write_file(&file_path, "test content").unwrap();

        assert!(file_path.exists());
        assert_eq!(fs::read_to_string(&file_path).unwrap(), "test content");

        let receipt = tx.commit().unwrap();
        assert_eq!(receipt.files_created.len(), 1);
        assert_eq!(receipt.files_modified.len(), 0);
    }

    #[test]
    fn test_atomic_write_existing_file() {
        let dir = tempdir().unwrap();
        let file_path = dir.path().join("test.txt");

        // Create initial file
        fs::write(&file_path, "original").unwrap();

        let mut tx = FileTransaction::new().unwrap();
        tx.write_file(&file_path, "modified").unwrap();

        assert_eq!(fs::read_to_string(&file_path).unwrap(), "modified");

        let receipt = tx.commit().unwrap();
        assert_eq!(receipt.files_created.len(), 0);
        assert_eq!(receipt.files_modified.len(), 1);
        assert_eq!(receipt.backups.len(), 1);
    }

    #[test]
    fn test_rollback_on_drop() {
        let dir = tempdir().unwrap();
        let file_path = dir.path().join("test.txt");

        {
            let mut tx = FileTransaction::new().unwrap();
            tx.write_file(&file_path, "test content").unwrap();
            assert!(file_path.exists());
            // Drop without commit triggers rollback
        }

        // File should be removed
        assert!(!file_path.exists());
    }

    #[test]
    fn test_rollback_restores_original() {
        let dir = tempdir().unwrap();
        let file_path = dir.path().join("test.txt");

        // Create initial file
        fs::write(&file_path, "original").unwrap();

        {
            let mut tx = FileTransaction::new().unwrap();
            tx.write_file(&file_path, "modified").unwrap();
            assert_eq!(fs::read_to_string(&file_path).unwrap(), "modified");
            // Drop without commit triggers rollback
        }

        // File should be restored
        assert_eq!(fs::read_to_string(&file_path).unwrap(), "original");
    }

    #[test]
    fn test_multiple_operations_rollback() {
        let dir = tempdir().unwrap();
        let file1 = dir.path().join("file1.txt");
        let file2 = dir.path().join("file2.txt");

        {
            let mut tx = FileTransaction::new().unwrap();
            tx.write_file(&file1, "content1").unwrap();
            tx.write_file(&file2, "content2").unwrap();
            assert!(file1.exists());
            assert!(file2.exists());
            // Drop without commit
        }

        // Both files should be removed
        assert!(!file1.exists());
        assert!(!file2.exists());
    }
}
