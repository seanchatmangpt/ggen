//! Golden file management and comparison
//!
//! Handles loading, comparing, and updating golden (expected) output files.
//! Supports LF line ending normalization for cross-platform determinism.

use crate::error::{GoldenError, Result};
use sha2::{Digest, Sha256};
use std::fs;
use std::path::{Path, PathBuf};

/// A golden (expected) output file
#[derive(Debug, Clone)]
pub struct GoldenFile {
    /// Path relative to the golden directory
    pub relative_path: PathBuf,
    /// File content
    pub content: String,
    /// SHA256 checksum of content
    pub checksum: String,
}

/// Mismatch between expected and actual output
#[derive(Debug, Clone)]
pub struct GoldenMismatch {
    /// Path to the file
    pub file: PathBuf,
    /// Expected content
    pub expected: String,
    /// Actual content
    pub actual: String,
    /// Unified diff of the files
    pub diff: String,
}

impl GoldenFile {
    /// Load a golden file from disk
    pub fn load(golden_dir: &Path, relative_path: &Path) -> Result<Self> {
        let full_path = golden_dir.join(relative_path);

        if !full_path.exists() {
            return Err(GoldenError::NotFound(full_path).into());
        }

        let content = fs::read_to_string(&full_path).map_err(|e| GoldenError::ReadFailed(e))?;

        let normalized = normalize_line_endings(&content);
        let checksum = compute_checksum(&normalized);

        Ok(GoldenFile {
            relative_path: relative_path.to_path_buf(),
            content: normalized,
            checksum,
        })
    }

    /// Compare this golden file with actual content
    pub fn compare(&self, actual: &str) -> std::result::Result<(), GoldenMismatch> {
        let normalized_actual = normalize_line_endings(actual);

        if self.content == normalized_actual {
            return Ok(());
        }

        let diff = create_unified_diff(&self.content, &normalized_actual);

        Err(GoldenMismatch {
            file: self.relative_path.clone(),
            expected: self.content.clone(),
            actual: normalized_actual,
            diff,
        })
    }

    /// Update the golden file with new content
    pub fn update(&mut self, actual: &str) -> Result<()> {
        let normalized = normalize_line_endings(actual);
        self.content = normalized.clone();
        self.checksum = compute_checksum(&normalized);
        Ok(())
    }

    /// Get the checksum of this file
    pub fn checksum(&self) -> &str {
        &self.checksum
    }

    /// Write this golden file to disk
    pub fn write(&self, golden_dir: &Path) -> Result<()> {
        let full_path = golden_dir.join(&self.relative_path);

        // Create parent directories if needed
        if let Some(parent) = full_path.parent() {
            fs::create_dir_all(parent).map_err(|e| GoldenError::WriteFailed(e.to_string()))?;
        }

        // Write with LF line endings
        fs::write(&full_path, &self.content)
            .map_err(|e| GoldenError::WriteFailed(e.to_string()))?;

        Ok(())
    }
}

impl GoldenMismatch {
    /// Display the mismatch in a user-friendly format
    pub fn display(&self) -> String {
        format!(
            "Golden file mismatch: {}\n\n{}",
            self.file.display(),
            self.diff
        )
    }
}

/// Normalize line endings to LF (Unix style)
///
/// Converts CRLF (Windows) and CR (old Mac) to LF for cross-platform determinism.
fn normalize_line_endings(content: &str) -> String {
    content
        .replace("\r\n", "\n") // Windows CRLF -> LF
        .replace('\r', "\n") // Old Mac CR -> LF
}

/// Compute SHA256 checksum of content
fn compute_checksum(content: &str) -> String {
    let mut hasher = Sha256::new();
    hasher.update(content.as_bytes());
    hex::encode(hasher.finalize())
}

/// Create a unified diff between expected and actual
fn create_unified_diff(expected: &str, actual: &str) -> String {
    let expected_lines: Vec<&str> = expected.lines().collect();
    let actual_lines: Vec<&str> = actual.lines().collect();

    let mut diff = String::new();
    diff.push_str("--- expected\n");
    diff.push_str("+++ actual\n");

    // Simple line-by-line diff (for more advanced diff, could use external crate)
    let min_len = expected_lines.len().min(actual_lines.len());
    let _max_len = expected_lines.len().max(actual_lines.len());

    for i in 0..min_len {
        if expected_lines[i] != actual_lines[i] {
            diff.push_str(&format!("-{}\n", expected_lines[i]));
            diff.push_str(&format!("+{}\n", actual_lines[i]));
        }
    }

    // Handle lines that only exist in one file
    if expected_lines.len() > actual_lines.len() {
        for i in min_len..expected_lines.len() {
            diff.push_str(&format!("-{}\n", expected_lines[i]));
        }
    } else if actual_lines.len() > expected_lines.len() {
        for i in min_len..actual_lines.len() {
            diff.push_str(&format!("+{}\n", actual_lines[i]));
        }
    }

    diff
}

#[cfg(test)]
mod tests {
    use super::*;
    use tempfile::TempDir;

    #[test]
    fn test_normalize_line_endings() {
        assert_eq!(normalize_line_endings("a\r\nb\r\nc"), "a\nb\nc");
        assert_eq!(normalize_line_endings("a\rb\rc"), "a\nb\nc");
        assert_eq!(normalize_line_endings("a\nb\nc"), "a\nb\nc");
    }

    #[test]
    fn test_compute_checksum() {
        let checksum1 = compute_checksum("hello");
        let checksum2 = compute_checksum("hello");
        assert_eq!(checksum1, checksum2);

        let checksum3 = compute_checksum("world");
        assert_ne!(checksum1, checksum3);
    }

    #[test]
    fn test_golden_file_comparison_match() {
        let golden = GoldenFile {
            relative_path: PathBuf::from("output.txt"),
            content: "line1\nline2\n".to_string(),
            checksum: compute_checksum("line1\nline2\n"),
        };

        let result = golden.compare("line1\nline2\n");
        assert!(result.is_ok());
    }

    #[test]
    fn test_golden_file_comparison_mismatch() {
        let golden = GoldenFile {
            relative_path: PathBuf::from("output.txt"),
            content: "line1\nline2\n".to_string(),
            checksum: compute_checksum("line1\nline2\n"),
        };

        match golden.compare("line1\nmodified\n") {
            Ok(_) => panic!("Expected mismatch but got Ok"),
            Err(mismatch) => {
                assert!(mismatch.diff.contains("line2"));
                assert!(mismatch.diff.contains("modified"));
            }
        }
    }

    #[test]
    fn test_golden_file_write_and_load() {
        let temp_dir = TempDir::new().unwrap();

        let golden = GoldenFile {
            relative_path: PathBuf::from("subdir/output.txt"),
            content: "test content\n".to_string(),
            checksum: compute_checksum("test content\n"),
        };

        golden.write(temp_dir.path()).unwrap();

        let loaded = GoldenFile::load(temp_dir.path(), Path::new("subdir/output.txt"))
            .expect("Failed to load");

        assert_eq!(loaded.content, golden.content);
        assert_eq!(loaded.checksum, golden.checksum);
    }

    #[test]
    fn test_golden_file_update() {
        let mut golden = GoldenFile {
            relative_path: PathBuf::from("output.txt"),
            content: "old content\n".to_string(),
            checksum: compute_checksum("old content\n"),
        };

        let old_checksum = golden.checksum.clone();

        golden.update("new content\n").unwrap();

        assert_ne!(golden.content, "old content\n");
        assert_eq!(golden.content, "new content\n");
        assert_ne!(golden.checksum, old_checksum);
    }
}
