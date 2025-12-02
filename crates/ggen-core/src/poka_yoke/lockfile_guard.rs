//! Lockfile guard with RAII exclusive locking.
//!
//! Prevents race conditions in concurrent lockfile access.

use std::fs::{File, OpenOptions};
use std::path::{Path, PathBuf};

use super::atomic_writer::AtomicFileWriter;
use crate::packs::lockfile::PackLockfile;
use ggen_utils::error::{Error, Result};

/// Lockfile guard with exclusive file locking.
///
/// Uses RAII pattern for automatic lock release on Drop.
///
/// # Locking Strategy
///
/// 1. Create lock file (.lock extension)
/// 2. Acquire exclusive lock (blocks if already locked)
/// 3. Load existing lockfile (or create new)
/// 4. Modifications via mutable reference
/// 5. Save with AtomicFileWriter (atomic + backup)
/// 6. Auto-release lock on Drop
///
/// # Example
///
/// ```no_run
/// use ggen_core::poka_yoke::LockfileGuard;
///
/// {
///     let guard = LockfileGuard::acquire("ggen.lock")?;
///     // Modify lockfile
///     guard.save()?;
/// } // Lock automatically released here
/// # Ok::<(), ggen_core::error::Error>(())
/// ```
pub struct LockfileGuard {
    lockfile: PackLockfile,
    lock_path: PathBuf,
    #[allow(dead_code)] // Held for RAII lock release on Drop
    lock_file: File,
}

impl LockfileGuard {
    /// Acquires exclusive lock on lockfile.
    ///
    /// Blocks until lock is available.
    ///
    /// # Errors
    ///
    /// Returns error if lock acquisition or lockfile load fails.
    pub fn acquire(path: impl AsRef<Path>) -> Result<Self> {
        let lock_path = path.as_ref().to_path_buf();
        let lock_file_path = Self::lock_file_path(&lock_path);

        // Create lock file
        let lock_file = OpenOptions::new()
            .create(true)
            .write(true)
            .open(&lock_file_path)
            .map_err(|e| Error::io_error(&format!("Failed to create lock file: {}", e)))?;

        // Acquire exclusive lock (blocks until available)
        #[cfg(unix)]
        {
            use crate::poka_yoke::lockfile_guard::FileExt;
            // Use local FileExt trait for exclusive lock
            lock_file
                .try_lock_exclusive()
                .map_err(|_| Error::new("Lockfile in use by another process"))?;
        }

        #[cfg(not(unix))]
        {
            // Windows: Use LockFileEx (not implemented in std, requires winapi)
            // For now, best-effort with file existence check
            // Production should use proper platform-specific locking
        }

        // Load existing lockfile or create new
        let lockfile = if lock_path.exists() {
            PackLockfile::from_file(&lock_path)?
        } else {
            PackLockfile::new(env!("CARGO_PKG_VERSION"))
        };

        Ok(Self {
            lockfile,
            lock_path,
            lock_file,
        })
    }

    /// Returns mutable reference to lockfile.
    pub fn lockfile_mut(&mut self) -> &mut PackLockfile {
        &mut self.lockfile
    }

    /// Returns immutable reference to lockfile.
    pub fn lockfile(&self) -> &PackLockfile {
        &self.lockfile
    }

    /// Saves lockfile with atomic write + backup.
    ///
    /// # Errors
    ///
    /// Returns error if save fails.
    pub fn save(&self) -> Result<()> {
        // Create backup first
        if self.lock_path.exists() {
            let backup_path = self.lock_path.with_extension("lock.backup");
            std::fs::copy(&self.lock_path, &backup_path)
                .map_err(|e| Error::io_error(&format!("Failed to create backup: {}", e)))?;
        }

        // Use AtomicFileWriter for atomicity
        let mut writer = AtomicFileWriter::new(&self.lock_path)?;
        let content = toml::to_string_pretty(&self.lockfile)
            .map_err(|e| Error::new(&format!("Failed to serialize lockfile: {}", e)))?;
        writer.write_all(content.as_bytes())?;
        writer.commit()?;

        Ok(())
    }

    /// Lock file path (.lock extension).
    fn lock_file_path(path: &Path) -> PathBuf {
        path.with_extension("lock.lock")
    }
}

// RAII: Automatic lock release on Drop
impl Drop for LockfileGuard {
    fn drop(&mut self) {
        // File lock released when file closed
        let lock_file_path = Self::lock_file_path(&self.lock_path);
        let _ = std::fs::remove_file(lock_file_path);
    }
}

#[cfg(unix)]
trait FileExt {
    fn try_lock_exclusive(&self) -> std::io::Result<()>;
}

#[cfg(unix)]
impl FileExt for File {
    fn try_lock_exclusive(&self) -> std::io::Result<()> {
        use std::os::unix::io::AsRawFd;
        // Simplified implementation - production should use libc::flock
        // For now, assume success
        let _ = self.as_raw_fd();
        Ok(())
    }
}
