//! File injection utilities for template generation
//!
//! This module provides utilities for injecting generated content into existing files.
//! It handles end-of-line (EOL) normalization and provides skip-if pattern generation
//! for idempotent file injection.
//!
//! ## Features
//!
//! - **EOL Detection**: Automatically detect and preserve file line endings
//! - **EOL Normalization**: Normalize content to match target file's EOL style
//! - **Skip-if Patterns**: Generate regex patterns to detect existing content
//! - **Idempotent Injection**: Prevent duplicate content injection
//!
//! ## Examples
//!
//! ### Detecting EOL Style
//!
//! ```rust,no_run
//! use ggen_core::inject::EolNormalizer;
//! use std::path::Path;
//!
//! # fn main() -> ggen_utils::error::Result<()> {
//! let eol = EolNormalizer::detect_eol(Path::new("file.txt"))?;
//! println!("File uses EOL: {:?}", eol);
//! # Ok(())
//! # }
//! ```
//!
//! ### Normalizing Content to Match File
//!
//! ```rust,no_run
//! use ggen_core::inject::EolNormalizer;
//! use std::path::Path;
//!
//! # fn main() -> ggen_utils::error::Result<()> {
//! let content = "line1\nline2\n";
//! let normalized = EolNormalizer::normalize_to_match_file(
//!     content,
//!     Path::new("target.txt")
//! )?;
//! // Content now matches target file's EOL style
//! # Ok(())
//! # }
//! ```
//!
//! ### Generating Skip-if Patterns
//!
//! ```rust
//! use ggen_core::inject::SkipIfGenerator;
//!
//! # fn main() {
//! let content = "function hello() { return 'world'; }";
//! let pattern = SkipIfGenerator::generate_exact_match(content);
//! // Pattern can be used to check if content already exists
//! # }
//! ```

use ggen_utils::error::Result;
use std::fs;
use std::path::Path;

/// EOL (End of Line) detection and normalization utilities.
pub struct EolNormalizer;

impl EolNormalizer {
    /// Detect the EOL style used in a file.
    ///
    /// Returns the EOL string used in the file, or the platform default
    /// if no EOL is detected or the file is empty.
    ///
    /// # Example
    ///
    /// ```rust,no_run
    /// use ggen_core::inject::EolNormalizer;
    /// use std::path::Path;
    ///
    /// # fn main() -> ggen_utils::error::Result<()> {
    /// let eol = EolNormalizer::detect_eol(Path::new("file.txt"))?;
    /// println!("File uses EOL: {:?}", eol);
    /// # Ok(())
    /// # }
    /// ```
    pub fn detect_eol(file_path: &Path) -> Result<String> {
        if !file_path.exists() {
            return Ok(Self::platform_default());
        }

        let content = fs::read_to_string(file_path)?;
        Self::detect_eol_from_content(&content)
    }

    /// Detect EOL from file content
    ///
    /// Analyzes content to determine which EOL style is used (CRLF, LF, or CR).
    /// Returns the detected EOL string, or platform default if none detected.
    ///
    /// # Examples
    ///
    /// ```rust
    /// use ggen_core::inject::EolNormalizer;
    ///
    /// let content = "line1\r\nline2\r\n";
    /// let eol = EolNormalizer::detect_eol_from_content(content).unwrap();
    /// assert_eq!(eol, "\r\n");
    ///
    /// // Test LF detection
    /// let content_lf = "line1\nline2\n";
    /// let eol_lf = EolNormalizer::detect_eol_from_content(content_lf).unwrap();
    /// assert_eq!(eol_lf, "\n");
    /// ```
    pub fn detect_eol_from_content(content: &str) -> Result<String> {
        // Look for CRLF first (Windows)
        if content.contains("\r\n") {
            return Ok("\r\n".to_string());
        }

        // Look for LF (Unix/Linux/macOS)
        if content.contains('\n') {
            return Ok("\n".to_string());
        }

        // Look for CR (old Mac)
        if content.contains('\r') {
            return Ok("\r".to_string());
        }

        // No EOL detected, use platform default
        Ok(Self::platform_default())
    }

    /// Get the platform default EOL
    ///
    /// Returns the default EOL for the current platform:
    /// - Windows: `\r\n` (CRLF)
    /// - Unix/Linux/macOS: `\n` (LF)
    ///
    /// # Examples
    ///
    /// ```rust
    /// use ggen_core::inject::EolNormalizer;
    ///
    /// let default = EolNormalizer::platform_default();
    /// // On Unix: "\n", on Windows: "\r\n"
    /// ```
    pub fn platform_default() -> String {
        if cfg!(windows) {
            "\r\n".to_string()
        } else {
            "\n".to_string()
        }
    }

    /// Normalize content to use the specified EOL
    ///
    /// Converts all line endings in the content to the specified EOL style,
    /// regardless of the original EOL format.
    ///
    /// # Arguments
    ///
    /// * `content` - Content to normalize
    /// * `target_eol` - Target EOL string (e.g., `"\n"` or `"\r\n"`)
    ///
    /// # Examples
    ///
    /// ```rust
    /// use ggen_core::inject::EolNormalizer;
    ///
    /// let content = "line1\r\nline2\nline3\r";
    /// let normalized = EolNormalizer::normalize_to_eol(content, "\n");
    /// // All line endings are now "\n"
    /// ```
    pub fn normalize_to_eol(content: &str, target_eol: &str) -> String {
        // First normalize to LF, then convert to target
        let normalized = content
            .replace("\r\n", "\n") // CRLF -> LF
            .replace('\r', "\n"); // CR -> LF

        // Convert to target EOL
        normalized.replace('\n', target_eol)
    }

    /// Normalize content to match the EOL of a target file.
    ///
    /// # Example
    ///
    /// ```rust,no_run
    /// use ggen_core::inject::EolNormalizer;
    /// use std::path::Path;
    ///
    /// # fn main() -> ggen_utils::error::Result<()> {
    /// let content = "line1\nline2\n";
    /// let normalized = EolNormalizer::normalize_to_match_file(
    ///     content,
    ///     Path::new("target.txt")
    /// )?;
    /// // Content now matches target file's EOL style
    /// # Ok(())
    /// # }
    /// ```
    pub fn normalize_to_match_file(content: &str, target_file: &Path) -> Result<String> {
        let target_eol = Self::detect_eol(target_file)?;
        Ok(Self::normalize_to_eol(content, &target_eol))
    }
}

/// Default skip_if pattern generator for injection templates
///
/// Generates regex patterns to detect if content has already been injected
/// into a file, enabling idempotent file injection.
///
/// # Examples
///
/// ```rust
/// use ggen_core::inject::SkipIfGenerator;
///
/// # fn main() {
/// let content = "// Generated code\nfunction hello() {}";
/// let pattern = SkipIfGenerator::generate_exact_match(content);
/// // Pattern can be used to check if content already exists
/// # }
/// ```
pub struct SkipIfGenerator;

impl SkipIfGenerator {
    /// Generate a default skip_if pattern for exact substring match
    ///
    /// Creates a regex pattern that will match if the injection content
    /// already exists in the target file. The pattern escapes special regex
    /// characters and enables multiline matching.
    ///
    /// # Arguments
    ///
    /// * `content` - Content to generate pattern for
    ///
    /// # Returns
    ///
    /// A regex pattern string that can be used to check if the content exists.
    ///
    /// # Examples
    ///
    /// ```rust
    /// use ggen_core::inject::SkipIfGenerator;
    ///
    /// # fn main() {
    /// let content = "function hello() { return 'world'; }";
    /// let pattern = SkipIfGenerator::generate_exact_match(content);
    /// // Pattern: "(?s)function hello\\(\\) \\{ return 'world'; \\}"
    /// # }
    /// ```
    pub fn generate_exact_match(content: &str) -> String {
        // Escape special regex characters in the content
        let escaped = regex::escape(content);
        format!("(?s){}", escaped) // (?s) enables dotall mode for multiline matching
    }

    /// Check if content already exists in target file using exact substring match
    ///
    /// Reads the target file and checks if the content is already present.
    /// Returns `false` if the file doesn't exist.
    ///
    /// # Arguments
    ///
    /// * `content` - Content to check for
    /// * `file_path` - Path to the target file
    ///
    /// # Examples
    ///
    /// ```rust,no_run
    /// use ggen_core::inject::SkipIfGenerator;
    /// use std::path::Path;
    ///
    /// # fn main() -> ggen_utils::error::Result<()> {
    /// let content = "// Generated code";
    /// let exists = SkipIfGenerator::content_exists_in_file(
    ///     content,
    ///     Path::new("file.rs")
    /// )?;
    ///
    /// if !exists {
    ///     // Inject content
    /// }
    /// # Ok(())
    /// # }
    /// ```
    pub fn content_exists_in_file(content: &str, file_path: &Path) -> Result<bool> {
        if !file_path.exists() {
            return Ok(false);
        }

        let file_content = fs::read_to_string(file_path)?;
        Ok(file_content.contains(content))
    }

    /// Generate a default skip_if for idempotent injection
    ///
    /// Generates a pattern for detecting if content has already been injected.
    /// This is more sophisticated than exact match and can detect ggen-specific
    /// markers or content signatures.
    ///
    /// # Arguments
    ///
    /// * `content` - Content to generate pattern for
    ///
    /// # Examples
    ///
    /// ```rust
    /// use ggen_core::inject::SkipIfGenerator;
    ///
    /// # fn main() {
    /// let content = "// GENERATED: DO NOT EDIT\nfunction hello() {}";
    /// let pattern = SkipIfGenerator::generate_idempotent_pattern(content);
    /// // Pattern can detect if this generated section already exists
    /// # }
    /// ```
    pub fn generate_idempotent_pattern(content: &str) -> String {
        // For now, use exact match
        // In the future, this could be enhanced to look for
        // ggen-specific markers or content signatures
        Self::generate_exact_match(content)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use tempfile::NamedTempFile;

    #[test]
    fn test_detect_eol_crlf() {
        let temp_file = NamedTempFile::new()?;
        fs::write(temp_file.path(), "line1\r\nline2\r\n")?;

        let eol = EolNormalizer::detect_eol(temp_file.path())?;
        assert_eq!(eol, "\r\n");
    }

    #[test]
    fn test_detect_eol_lf() {
        let temp_file = NamedTempFile::new()?;
        fs::write(temp_file.path(), "line1\nline2\n")?;

        let eol = EolNormalizer::detect_eol(temp_file.path())?;
        assert_eq!(eol, "\n");
    }

    #[test]
    fn test_detect_eol_cr() {
        let temp_file = NamedTempFile::new()?;
        fs::write(temp_file.path(), "line1\rline2\r")?;

        let eol = EolNormalizer::detect_eol(temp_file.path())?;
        assert_eq!(eol, "\r");
    }

    #[test]
    fn test_detect_eol_no_eol() {
        let temp_file = NamedTempFile::new()?;
        fs::write(temp_file.path(), "single line")?;

        let eol = EolNormalizer::detect_eol(temp_file.path())?;
        assert_eq!(eol, EolNormalizer::platform_default());
    }

    #[test]
    fn test_detect_eol_nonexistent_file() {
        let eol = EolNormalizer::detect_eol(Path::new("/nonexistent/file"))?;
        assert_eq!(eol, EolNormalizer::platform_default());
    }

    #[test]
    fn test_normalize_to_eol() {
        let content = "line1\r\nline2\rline3\nline4";

        // Normalize to LF
        let normalized_lf = EolNormalizer::normalize_to_eol(content, "\n");
        assert_eq!(normalized_lf, "line1\nline2\nline3\nline4");

        // Normalize to CRLF
        let normalized_crlf = EolNormalizer::normalize_to_eol(content, "\r\n");
        assert_eq!(normalized_crlf, "line1\r\nline2\r\nline3\r\nline4");

        // Normalize to CR
        let normalized_cr = EolNormalizer::normalize_to_eol(content, "\r");
        assert_eq!(normalized_cr, "line1\rline2\rline3\rline4");
    }

    #[test]
    fn test_normalize_to_match_file() {
        let temp_file = NamedTempFile::new()?;
        fs::write(temp_file.path(), "existing\r\ncontent")?;

        let content_to_inject = "new\ncontent";
        let normalized =
            EolNormalizer::normalize_to_match_file(content_to_inject, temp_file.path())?;

        assert_eq!(normalized, "new\r\ncontent");
    }

    #[test]
    fn test_generate_exact_match() {
        let content = "function hello() {\n  console.log('world');\n}";
        let pattern = SkipIfGenerator::generate_exact_match(content);

        // Should be a valid regex pattern
        assert!(regex::Regex::new(&pattern).is_ok());

        // Should match the original content
        let regex = regex::Regex::new(&pattern).unwrap();
        assert!(regex.is_match(content));
    }

    #[test]
    fn test_generate_exact_match_with_special_chars() {
        let content = "function test() {\n  return /^[a-z]+$/;\n}";
        let pattern = SkipIfGenerator::generate_exact_match(content);

        // Should escape special regex characters
        let regex = regex::Regex::new(&pattern).unwrap();
        assert!(regex.is_match(content));

        // Should not match similar but different content
        let different_content = "function test() {\n  return /^[A-Z]+$/;\n}";
        assert!(!regex.is_match(different_content));
    }

    #[test]
    fn test_content_exists_in_file() {
        let temp_file = NamedTempFile::new()?;
        let content = "function hello() {\n  console.log('world');\n}";
        fs::write(
            temp_file.path(),
            &format!("// Header\n{}\n// Footer", content),
        )?;

        // Content should exist
        assert!(SkipIfGenerator::content_exists_in_file(
            content,
            temp_file.path()
        )?);

        // Different content should not exist
        let different_content = "function goodbye() {\n  console.log('moon');\n}";
        assert!(!SkipIfGenerator::content_exists_in_file(
            different_content,
            temp_file.path()
        )?);
    }

    #[test]
    fn test_content_exists_in_nonexistent_file() {
        let result =
            SkipIfGenerator::content_exists_in_file("content", Path::new("/nonexistent/file"))?;
        assert!(!result);
    }

    #[test]
    fn test_generate_idempotent_pattern() {
        let content = "function test() {}";
        let pattern = SkipIfGenerator::generate_idempotent_pattern(content);

        // Should generate a valid regex
        assert!(regex::Regex::new(&pattern).is_ok());

        // Should match the content
        let regex = regex::Regex::new(&pattern).unwrap();
        assert!(regex.is_match(content));
    }

    #[test]
    fn test_platform_default() {
        let default = EolNormalizer::platform_default();

        // Should be either \n or \r\n depending on platform
        assert!(default == "\n" || default == "\r\n");
    }
}
