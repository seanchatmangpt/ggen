//! Dry-run mode for preview before execution.
//!
//! Allows users to preview destructive operations before committing.

use ggen_utils::error::Result;
use crate::poka_yoke::ValidatedPath;

/// Operation to be performed.
#[derive(Debug, Clone)]
pub enum Operation {
    /// Create a file.
    FileCreate {
        path: ValidatedPath,
        size: u64,
    },
    /// Write to a file.
    FileWrite {
        path: ValidatedPath,
        size: u64,
    },
    /// Delete a file.
    FileDelete {
        path: ValidatedPath,
    },
    /// Execute a command.
    CommandExec {
        command: String,
        args: Vec<String>,
    },
    /// Create a directory.
    DirCreate {
        path: ValidatedPath,
    },
    /// Delete a directory.
    DirDelete {
        path: ValidatedPath,
    },
}

/// Dry-run mode for previewing operations.
///
/// # Workflow
///
/// 1. Collect operations
/// 2. Preview operations to user
/// 3. Confirm with user
/// 4. Execute operations (with rollback on failure)
///
/// # Example
///
/// ```no_run
/// use ggen_core::poka_yoke::{DryRunMode, Operation, ValidatedPath};
///
/// let mut dry_run = DryRunMode::new();
/// dry_run.add_operation(Operation::FileCreate {
///     path: ValidatedPath::new("output.txt")?,
///     size: 1024,
/// });
///
/// dry_run.preview();
/// if dry_run.confirm()? {
///     dry_run.execute()?;
/// }
/// # Ok::<(), ggen_core::error::Error>(())
/// ```
pub struct DryRunMode {
    operations: Vec<Operation>,
    executed: bool,
}

impl DryRunMode {
    /// Creates a new dry-run mode.
    pub fn new() -> Self {
        Self {
            operations: Vec::new(),
            executed: false,
        }
    }

    /// Adds an operation to the queue.
    pub fn add_operation(&mut self, operation: Operation) {
        self.operations.push(operation);
    }

    /// Previews operations to be performed.
    pub fn preview(&self) {
        println!("📋 Dry Run Preview:");
        println!("  {} operations planned:", self.operations.len());
        println!();

        for (i, op) in self.operations.iter().enumerate() {
            match op {
                Operation::FileCreate { path, size } => {
                    println!("  {}. CREATE {} ({} bytes)", i + 1, path, size);
                }
                Operation::FileWrite { path, size } => {
                    println!("  {}. WRITE {} ({} bytes)", i + 1, path, size);
                }
                Operation::FileDelete { path } => {
                    println!("  {}. DELETE {}", i + 1, path);
                }
                Operation::CommandExec { command, args } => {
                    println!("  {}. EXEC {} {}", i + 1, command, args.join(" "));
                }
                Operation::DirCreate { path } => {
                    println!("  {}. MKDIR {}", i + 1, path);
                }
                Operation::DirDelete { path } => {
                    println!("  {}. RMDIR {}", i + 1, path);
                }
            }
        }
    }

    /// Confirms with user.
    ///
    /// # Errors
    ///
    /// Returns error if stdin read fails.
    pub fn confirm(&self) -> Result<bool> {
        print!("\nProceed with these operations? [y/N]: ");
        use std::io::Write;
        std::io::stdout().flush().map_err(|e| {
            ggen_utils::error::Error::io_error(&format!("Failed to flush stdout: {}", e))
        })?;

        let mut input = String::new();
        std::io::stdin().read_line(&mut input).map_err(|e| {
            ggen_utils::error::Error::io_error(&format!("Failed to read stdin: {}", e))
        })?;

        Ok(input.trim().to_lowercase().starts_with('y'))
    }

    /// Executes all operations.
    ///
    /// Validates all operations first (fail-fast), then executes with rollback on failure.
    ///
    /// # Errors
    ///
    /// Returns error if any operation fails. Attempts rollback.
    pub fn execute(&mut self) -> Result<()> {
        if self.executed {
            return Err(ggen_utils::error::Error::new("Operations already executed"));
        }

        // Validate all operations first (fail-fast)
        for op in &self.operations {
            self.validate_operation(op)?;
        }

        // Execute with rollback on failure
        let mut completed = Vec::new();
        for op in &self.operations {
            match self.execute_operation(op) {
                Ok(_) => completed.push(op.clone()),
                Err(e) => {
                    log::error!("Operation failed: {}", e);
                    self.rollback(&completed)?;
                    return Err(e);
                }
            }
        }

        self.executed = true;
        Ok(())
    }

    /// Validates an operation.
    fn validate_operation(&self, op: &Operation) -> Result<()> {
        match op {
            Operation::FileCreate { path, .. } | Operation::FileWrite { path, .. } => {
                // Check parent directory exists
                if let Some(parent) = path.as_path().parent() {
                    if !parent.exists() {
                        return Err(ggen_utils::error::Error::invalid_input(&format!(
                            "Parent directory does not exist: {}",
                            parent.display()
                        )));
                    }
                }
            }
            Operation::FileDelete { path } | Operation::DirDelete { path } => {
                // Check path exists
                if !path.as_path().exists() {
                    return Err(ggen_utils::error::Error::invalid_input(&format!(
                        "Path does not exist: {}",
                        path
                    )));
                }
            }
            Operation::CommandExec { .. } | Operation::DirCreate { .. } => {
                // Basic validation (commands are inherently risky)
            }
        }
        Ok(())
    }

    /// Executes a single operation.
    fn execute_operation(&self, op: &Operation) -> Result<()> {
        match op {
            Operation::FileCreate { path, .. } | Operation::FileWrite { path, .. } => {
                std::fs::write(path.as_path(), b"").map_err(|e| {
                    ggen_utils::error::Error::io_error(&format!("Failed to write file: {}", e))
                })?;
            }
            Operation::FileDelete { path } => {
                std::fs::remove_file(path.as_path()).map_err(|e| {
                    ggen_utils::error::Error::io_error(&format!("Failed to delete file: {}", e))
                })?;
            }
            Operation::CommandExec { command, args } => {
                let status = std::process::Command::new(command)
                    .args(args)
                    .status()
                    .map_err(|e| {
                        ggen_utils::error::Error::new(&format!("Failed to execute command: {}", e))
                    })?;

                if !status.success() {
                    return Err(ggen_utils::error::Error::new(&format!(
                        "Command failed with exit code: {:?}",
                        status.code()
                    )));
                }
            }
            Operation::DirCreate { path } => {
                std::fs::create_dir_all(path.as_path()).map_err(|e| {
                    ggen_utils::error::Error::io_error(&format!("Failed to create directory: {}", e))
                })?;
            }
            Operation::DirDelete { path } => {
                std::fs::remove_dir_all(path.as_path()).map_err(|e| {
                    ggen_utils::error::Error::io_error(&format!("Failed to delete directory: {}", e))
                })?;
            }
        }
        Ok(())
    }

    /// Rolls back completed operations.
    fn rollback(&self, completed: &[Operation]) -> Result<()> {
        println!("\n⚠️  Rolling back {} operations...", completed.len());

        for op in completed.iter().rev() {
            match self.rollback_operation(op) {
                Ok(_) => log::info!("Rolled back: {:?}", op),
                Err(e) => log::error!("Rollback failed: {}", e),
            }
        }

        Ok(())
    }

    /// Rolls back a single operation.
    fn rollback_operation(&self, op: &Operation) -> Result<()> {
        match op {
            Operation::FileCreate { path, .. } | Operation::FileWrite { path, .. } => {
                // Rollback: Delete created file
                let _ = std::fs::remove_file(path.as_path());
            }
            Operation::FileDelete { .. } => {
                // Cannot rollback file deletion (backup required)
                log::warn!("Cannot rollback file deletion");
            }
            Operation::CommandExec { .. } => {
                // Cannot rollback command execution
                log::warn!("Cannot rollback command execution");
            }
            Operation::DirCreate { path } => {
                // Rollback: Delete created directory
                let _ = std::fs::remove_dir_all(path.as_path());
            }
            Operation::DirDelete { .. } => {
                // Cannot rollback directory deletion
                log::warn!("Cannot rollback directory deletion");
            }
        }
        Ok(())
    }
}

impl Default for DryRunMode {
    fn default() -> Self {
        Self::new()
    }
}
