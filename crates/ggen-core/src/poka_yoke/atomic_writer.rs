//! Atomic file writer with type-state pattern.
//!
//! Prevents partial writes and ensures atomicity using temp file + rename.

use std::fs::{self, File, OpenOptions};
use std::io::Write;
use std::marker::PhantomData;
use std::path::{Path, PathBuf};

use ggen_utils::error::{Error, Result};

/// Minimum free disk space required (10 MB).
const MIN_FREE_SPACE: u64 = 10 * 1024 * 1024;

/// Atomic file writer with type-state pattern.
///
/// Ensures atomic file writes using temp file + rename strategy:
/// 1. Create temp file in same directory (same filesystem)
/// 2. Write data to temp file
/// 3. Sync to disk
/// 4. Atomic rename (POSIX guarantees)
///
/// # Type States
///
/// - [`Uncommitted`]: Data written but not committed
/// - [`Committed`]: Data committed via atomic rename
///
/// # Example
///
/// ```no_run
/// use ggen_core::poka_yoke::{AtomicFileWriter, Uncommitted};
///
/// let mut writer = AtomicFileWriter::new("/path/to/file.txt")?;
/// writer.write_all(b"Hello, world!")?;
/// let _committed = writer.commit()?; // Atomic rename
/// # Ok::<(), ggen_core::error::Error>(())
/// ```
pub struct AtomicFileWriter<State = Uncommitted> {
    target_path: PathBuf,
    temp_path: PathBuf,
    file: File,
    committed: bool, // Track if file was committed (for Drop)
    _state: PhantomData<State>,
}

/// Uncommitted state (data written to temp file).
#[derive(Debug)]
pub struct Uncommitted;

/// Committed state (temp file renamed to target).
#[derive(Debug)]
pub struct Committed;

impl AtomicFileWriter<Uncommitted> {
    /// Creates a new atomic file writer.
    ///
    /// # Pre-flight Checks
    ///
    /// - Disk space >= MIN_FREE_SPACE
    /// - Parent directory exists and is writable
    ///
    /// # Errors
    ///
    /// Returns error if pre-flight checks fail or temp file creation fails.
    pub fn new(target: impl AsRef<Path>) -> Result<Self> {
        let target_path = target.as_ref().to_path_buf();

        // Pre-flight check: disk space
        Self::check_disk_space(&target_path)?;

        // Pre-flight check: parent directory exists
        if let Some(parent) = target_path.parent() {
            if !parent.exists() {
                return Err(Error::invalid_input(format!(
                    "Parent directory does not exist: {}",
                    parent.display()
                )));
            }
        }

        // Create temp file in same directory (ensures same filesystem)
        let temp_path = Self::temp_path(&target_path);
        let file = OpenOptions::new()
            .create(true)
            .write(true)
            .truncate(true)
            .open(&temp_path)
            .map_err(|e| Error::io_error(format!("Failed to create temp file: {}", e)))?;

        Ok(Self {
            target_path,
            temp_path,
            file,
            committed: false,
            _state: PhantomData,
        })
    }

    /// Writes all data to the temp file.
    ///
    /// # Errors
    ///
    /// Returns error if write or sync fails.
    pub fn write_all(&mut self, data: &[u8]) -> Result<()> {
        self.file
            .write_all(data)
            .map_err(|e| Error::io_error(&format!("Failed to write data: {}", e)))?;

        // Force to disk (prevents data loss on crash)
        self.file
            .sync_all()
            .map_err(|e| Error::io_error(&format!("Failed to sync data: {}", e)))?;

        Ok(())
    }

    /// Commits the write by atomically renaming temp file to target.
    ///
    /// # Type-Level State Transition
    ///
    /// Uncommitted → Committed (enforced at compile time)
    ///
    /// # Errors
    ///
    /// Returns error if rename fails.
    pub fn commit(mut self) -> Result<AtomicFileWriter<Committed>> {
        // Atomic rename (POSIX guarantees atomicity)
        fs::rename(&self.temp_path, &self.target_path).map_err(|e| {
            Error::io_error(&format!("Failed to commit file (rename failed): {}", e))
        })?;

        // Mark as committed to prevent cleanup in Drop
        self.committed = true;

        Ok(AtomicFileWriter {
            target_path: self.target_path.clone(),
            temp_path: self.temp_path.clone(),
            file: self
                .file
                .try_clone()
                .map_err(|e| Error::io_error(&format!("Failed to clone file handle: {}", e)))?,
            committed: true,
            _state: PhantomData,
        })
    }

    /// Generates temp file path.
    fn temp_path(target: &Path) -> PathBuf {
        let mut temp = target.as_os_str().to_os_string();
        temp.push(".tmp");
        PathBuf::from(temp)
    }

    /// Checks available disk space.
    fn check_disk_space(path: &Path) -> Result<()> {
        // Get filesystem stats (platform-specific)
        #[cfg(unix)]
        {
            use std::os::unix::fs::MetadataExt;
            if let Some(parent) = path.parent() {
                if parent.exists() {
                    if let Ok(metadata) = fs::metadata(parent) {
                        let block_size = metadata.blksize();
                        let blocks_avail = metadata.blocks();
                        let available = block_size * blocks_avail;

                        if available < MIN_FREE_SPACE {
                            return Err(Error::io_error(&format!(
                                "Insufficient disk space: {} bytes available, {} required",
                                available, MIN_FREE_SPACE
                            )));
                        }
                    }
                }
            }
        }

        // Windows/other platforms: Skip disk space check (not critical)
        #[cfg(not(unix))]
        {
            let _ = path; // Suppress unused warning
        }

        Ok(())
    }
}

// RAII: Automatic cleanup when writer is dropped without commit
impl<State> Drop for AtomicFileWriter<State> {
    fn drop(&mut self) {
        // Only clean up temp file if not committed
        if !self.committed {
            let _ = fs::remove_file(&self.temp_path);
        }
    }
}
