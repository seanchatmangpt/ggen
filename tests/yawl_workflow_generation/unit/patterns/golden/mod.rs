//! Golden file utilities for workflow pattern tests.
//!
//! Golden files store expected YAWL XML output for each workflow pattern.
//! Tests compare generated output against these known-good references.

use std::path::Path;

/// Path to the golden files directory
pub fn golden_dir() -> &'static str {
    concat!(env!("CARGO_MANIFEST_DIR"), "/tests/yawl_workflow_generation/unit/patterns/golden/")
}

/// Get the path to a golden file for a specific pattern
pub fn golden_file(pattern_name: &str) -> String {
    format!("{}{}.yawl", golden_dir(), pattern_name)
}

/// Get the path to a failed comparison file for debugging
pub fn failed_file(pattern_name: &str) -> String {
    format!("{}{}.yawl.failed", golden_dir(), pattern_name)
}

/// Read a golden file, returning None if it doesn't exist
pub fn read_golden(pattern_name: &str) -> Option<String> {
    let path = golden_file(pattern_name);
    std::fs::read_to_string(&path).ok()
}

/// Write a golden file (typically run once to create initial golden files)
pub fn write_golden(pattern_name: &str, content: &str) -> Result<(), std::io::Error> {
    let path = golden_file(pattern_name);
    std::fs::write(&path, content)
}

/// Compare generated output against golden file
///
/// Returns Ok(()) if they match, Err with details if they differ
pub fn compare_with_golden(
    pattern_name: &str,
    generated: &str,
) -> Result<(), GoldenComparisonError> {
    let golden_path = golden_file(pattern_name);

    // If golden doesn't exist, create it and return Ok
    if !Path::new(&golden_path).exists() {
        write_golden(pattern_name, generated)
            .map_err(GoldenComparisonError::IoError)?;
        return Ok(());
    }

    let expected = std::fs::read_to_string(&golden_path)
        .map_err(GoldenComparisonError::IoError)?;

    if generated != expected {
        // Write failed file for debugging
        let failed_path = failed_file(pattern_name);
        std::fs::write(&failed_path, generated)
            .map_err(GoldenComparisonError::IoError)?;

        return Err(GoldenComparisonError::Mismatch {
            golden_path,
            failed_path,
        });
    }

    Ok(())
}

/// Errors that can occur during golden file comparison
#[derive(Debug)]
pub enum GoldenComparisonError {
    /// IO error reading or writing files
    IoError(std::io::Error),
    /// Generated output differs from golden file
    Mismatch {
        golden_path: String,
        failed_path: String,
    },
}

impl std::fmt::Display for GoldenComparisonError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::IoError(e) => write!(f, "IO error: {}", e),
            Self::Mismatch { golden_path, failed_path } => {
                write!(
                    f,
                    "Generated output differs from golden file. Golden: {}, Failed comparison: {}",
                    golden_path, failed_path
                )
            }
        }
    }
}

impl std::error::Error for GoldenComparisonError {}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_golden_dir_is_valid() {
        let dir = golden_dir();
        // Just verify the path is constructed correctly
        assert!(dir.contains("golden"));
        assert!(dir.ends_with('/'));
    }

    #[test]
    fn test_golden_file_construction() {
        let path = golden_file("wp01_sequence");
        assert!(path.contains("wp01_sequence"));
        assert!(path.ends_with(".yawl"));
    }

    #[test]
    fn test_read_missing_golden_returns_none() {
        let result = read_golden("nonexistent_pattern");
        assert!(result.is_none());
    }
}
