//! [`SabotageFixture`] — adversarial file corruption helpers for fail-closed testing.
//!
//! All methods write real corrupted bytes to real disk paths — no mocks, no stubs.

use std::fs;
use std::path::Path;

/// Adversarial file corruption helpers for fail-closed testing.
///
/// Every method writes real corrupted bytes to a real on-disk path so that
/// error-handling paths in the system under test are exercised with genuine
/// bad inputs.
pub struct SabotageFixture;

impl SabotageFixture {
    /// Overwrite the file with raw garbage bytes, making it unparseable.
    ///
    /// # Errors
    ///
    /// Propagates any I/O error from [`fs::write`].
    pub fn write_garbage(path: impl AsRef<Path>) -> Result<(), std::io::Error> {
        fs::write(path, b"XXXX CORRUPT GARBAGE {{{ not valid }}} \x00\x01\x02")
    }

    /// Flip every bit of the first byte of the file.
    ///
    /// If the file is empty this is a no-op (the file stays empty).
    ///
    /// # Errors
    ///
    /// Propagates any I/O error.
    pub fn bit_flip(path: impl AsRef<Path>) -> Result<(), std::io::Error> {
        let mut bytes = fs::read(path.as_ref())?;
        if let Some(b) = bytes.first_mut() {
            *b ^= 0xFF;
        }
        fs::write(path, bytes)
    }

    /// Truncate the file to zero bytes (empty file, still exists on disk).
    ///
    /// # Errors
    ///
    /// Propagates any I/O error.
    pub fn truncate(path: impl AsRef<Path>) -> Result<(), std::io::Error> {
        fs::write(path, b"")
    }

    /// Remove the file from disk entirely.
    ///
    /// # Errors
    ///
    /// Propagates any I/O error from [`fs::remove_file`].
    pub fn remove(path: impl AsRef<Path>) -> Result<(), std::io::Error> {
        fs::remove_file(path)
    }

    /// Read a JSON file, apply `f` to its parsed [`serde_json::Value`], then
    /// write the result back as pretty-printed JSON.
    ///
    /// # Errors
    ///
    /// Returns an error if the file cannot be read, contains invalid JSON,
    /// or cannot be re-serialised / written.
    pub fn corrupt_json(
        path: impl AsRef<Path>, f: impl Fn(serde_json::Value) -> serde_json::Value,
    ) -> Result<(), Box<dyn std::error::Error>> {
        let content = fs::read_to_string(path.as_ref())?;
        let value: serde_json::Value = serde_json::from_str(&content)?;
        let corrupted = f(value);
        fs::write(path, serde_json::to_string_pretty(&corrupted)?)?;
        Ok(())
    }
}
