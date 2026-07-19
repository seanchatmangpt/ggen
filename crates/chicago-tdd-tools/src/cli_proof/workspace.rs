//! [`TempWorkspace`] — hermetic temporary directory with file helpers.

// Same rationale as cli_proof/receipt.rs: this module's assert_* methods panic
// with a descriptive message on an unmet expectation, mirroring stdlib
// `assert!`/`assert_eq!` semantics (themselves `panic!`-based and exempt from
// this lint). Allowed here explicitly rather than weakening the crate-wide
// `#![deny(clippy::panic)]` in lib.rs, which is meant for production logic.
#![allow(clippy::panic)]

use std::fs;
use std::path::{Path, PathBuf};

/// A hermetic temporary workspace backed by a real [`tempfile::TempDir`].
///
/// The directory is deleted when this value is dropped.
///
/// All path arguments are relative to the workspace root.
pub struct TempWorkspace {
    dir: tempfile::TempDir,
}

impl TempWorkspace {
    /// Create a new, empty temporary workspace.
    ///
    /// # Errors
    ///
    /// Propagates any I/O error from [`tempfile::TempDir::new`].
    pub fn new() -> Result<Self, std::io::Error> {
        Ok(Self {
            dir: tempfile::TempDir::new()?,
        })
    }

    /// Create a workspace pre-populated with a fixture directory.
    ///
    /// Looks for the fixture at
    /// `$CARGO_MANIFEST_DIR/../tests/fixtures/<name>/`.  
    /// If the fixture directory is not found, an empty workspace is returned
    /// rather than an error — callers that need the fixture to exist should
    /// assert with [`Self::assert_file_exists`] afterwards.
    ///
    /// # Errors
    ///
    /// Propagates I/O errors from workspace creation or file copying.
    pub fn with_fixture(name: &str) -> Result<Self, std::io::Error> {
        let ws = Self::new()?;

        let manifest_dir = std::env::var("CARGO_MANIFEST_DIR").unwrap_or_default();
        let fixture_src = PathBuf::from(&manifest_dir)
            .join("..")
            .join("tests")
            .join("fixtures")
            .join(name);

        if fixture_src.is_dir() {
            copy_dir_all(&fixture_src, ws.path())?;
        }

        Ok(ws)
    }

    /// Return the absolute path to the workspace root.
    #[must_use]
    pub fn path(&self) -> &Path {
        self.dir.path()
    }

    /// Resolve a relative path against the workspace root.
    #[must_use]
    pub fn resolve(&self, rel: &str) -> PathBuf {
        self.dir.path().join(rel)
    }

    /// Write `content` to a relative path, creating parent directories as needed.
    ///
    /// # Errors
    ///
    /// Propagates any I/O error.
    pub fn write_file(&self, rel: &str, content: &str) -> Result<&Self, std::io::Error> {
        let path = self.resolve(rel);
        if let Some(parent) = path.parent() {
            fs::create_dir_all(parent)?;
        }
        fs::write(&path, content)?;
        Ok(self)
    }

    /// Append raw bytes to a file at a relative path, creating parent directories
    /// and the file itself if they do not exist.
    ///
    /// # Errors
    ///
    /// Propagates any I/O error.
    pub fn append_file(&self, rel: &str, bytes: &[u8]) -> Result<&Self, std::io::Error> {
        use std::io::Write as _;
        let path = self.resolve(rel);
        if let Some(parent) = path.parent() {
            fs::create_dir_all(parent)?;
        }
        let mut file = std::fs::OpenOptions::new()
            .create(true)
            .append(true)
            .open(&path)?;
        file.write_all(bytes)?;
        Ok(self)
    }

    /// Read the current bytes of a file, apply `f`, and write the result back.
    ///
    /// Useful for adversarial corruption in fail-closed tests.
    ///
    /// # Errors
    ///
    /// Propagates any I/O error.
    pub fn corrupt_file(
        &self, rel: &str, f: impl Fn(Vec<u8>) -> Vec<u8>,
    ) -> Result<&Self, std::io::Error> {
        let path = self.resolve(rel);
        let original = fs::read(&path)?;
        let corrupted = f(original);
        fs::write(&path, corrupted)?;
        Ok(self)
    }

    /// Read a file as a UTF-8 string.
    ///
    /// # Errors
    ///
    /// Propagates any I/O error (including invalid UTF-8 via
    /// [`std::io::Error`] wrapping).
    pub fn read_file(&self, rel: &str) -> Result<String, std::io::Error> {
        fs::read_to_string(self.resolve(rel))
    }

    /// Read and parse a JSON file.
    ///
    /// # Errors
    ///
    /// Returns an error if the file cannot be read or contains invalid JSON.
    pub fn read_json(&self, rel: &str) -> Result<serde_json::Value, Box<dyn std::error::Error>> {
        let text = self.read_file(rel)?;
        Ok(serde_json::from_str(&text)?)
    }

    // ── Assertion helpers ────────────────────────────────────────────────────

    /// Panic if the file at `rel` does not exist.
    pub fn assert_file_exists(&self, rel: &str) -> &Self {
        let path = self.resolve(rel);
        if !path.exists() {
            panic!("expected file to exist but it does not: {}", path.display());
        }
        self
    }

    /// Panic if the file at `rel` exists when it should be absent.
    pub fn assert_file_absent(&self, rel: &str) -> &Self {
        let path = self.resolve(rel);
        if path.exists() {
            panic!(
                "expected file to be absent but it exists: {}",
                path.display()
            );
        }
        self
    }

    /// Panic if the file at `rel` does not contain `needle`.
    pub fn assert_file_contains(&self, rel: &str, needle: &str) -> &Self {
        let path = self.resolve(rel);
        let content = fs::read_to_string(&path)
            .unwrap_or_else(|e| panic!("could not read file {}: {e}", path.display()));
        if !content.contains(needle) {
            panic!(
                "expected file {} to contain {:?} but it did not\ncontent:\n{}",
                path.display(),
                needle,
                content
            );
        }
        self
    }

    /// Panic if the directory at `rel` does not contain exactly `expected` entries.
    pub fn assert_dir_count(&self, rel: &str, expected: usize) -> &Self {
        let path = self.resolve(rel);
        let count = fs::read_dir(&path)
            .unwrap_or_else(|e| panic!("could not read directory {}: {e}", path.display()))
            .count();
        if count != expected {
            panic!(
                "expected {} entries in directory {} but found {}",
                expected,
                path.display(),
                count
            );
        }
        self
    }
}

impl Default for TempWorkspace {
    fn default() -> Self {
        Self::new().unwrap_or_else(|e| panic!("TempWorkspace::new failed: {e}"))
    }
}

/// Recursively copy the directory tree rooted at `src` into `dst`.
fn copy_dir_all(src: &Path, dst: &Path) -> Result<(), std::io::Error> {
    fs::create_dir_all(dst)?;
    for entry in fs::read_dir(src)? {
        let entry = entry?;
        let ty = entry.file_type()?;
        let target = dst.join(entry.file_name());
        if ty.is_dir() {
            copy_dir_all(&entry.path(), &target)?;
        } else {
            fs::copy(entry.path(), target)?;
        }
    }
    Ok(())
}
