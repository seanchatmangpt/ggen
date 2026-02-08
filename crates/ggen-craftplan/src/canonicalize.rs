//! μ₄ (Canonicalize): Deterministic formatting, content hashing
//!
//! This stage ensures deterministic output by normalizing formatting
//! and computing cryptographic hashes for reproducibility verification.

use crate::error::{CraftplanError, Result};
use sha2::{Digest, Sha256};
use tracing::{debug, info};

/// Canonicalizer for generated Elixir code
///
/// Ensures deterministic formatting and computes hashes for
/// reproducibility verification.
pub struct Canonicalizer;

impl Canonicalizer {
    /// Create a new canonicalizer
    pub fn new() -> Self {
        Self
    }

    /// Canonicalize generated files
    ///
    /// # Arguments
    /// * `files` - Paths to generated files
    ///
    /// # Returns
    /// * `Ok(Vec<String>)` - SHA-256 hashes of each file
    /// * `Err(CraftplanError)` - Canonicalization failed
    pub fn canonicalize(&self, files: &[String]) -> Result<Vec<String>> {
        info!("Canonicalizing {} generated files", files.len());

        let mut hashes = Vec::new();

        for file_path in files {
            let hash = self.canonicalize_file(file_path)?;
            hashes.push(hash);
        }

        debug!(
            "Canonicalization complete: {} hashes computed",
            hashes.len()
        );
        Ok(hashes)
    }

    /// Canonicalize a single file
    fn canonicalize_file(&self, file_path: &str) -> Result<String> {
        debug!("Canonicalizing file: {}", file_path);

        // Read file
        let content = std::fs::read_to_string(file_path).map_err(|e| CraftplanError::Io {
            path: file_path.into(),
            source: e,
        })?;

        // Normalize whitespace (Elixir formatter will handle this)
        let normalized = self.normalize_whitespace(&content);

        // Write back (for now, just compute hash)
        let hash = self.compute_hash(&normalized);

        debug!("File hash: {} = {}", file_path, hash);
        Ok(hash)
    }

    /// Normalize whitespace in content
    fn normalize_whitespace(&self, content: &str) -> String {
        // Basic normalization: trim trailing whitespace
        content
            .lines()
            .map(|line| line.trim_end().to_string())
            .collect::<Vec<_>>()
            .join("\n")
    }

    /// Compute SHA-256 hash of content
    fn compute_hash(&self, content: &str) -> String {
        let mut hasher = Sha256::new();
        hasher.update(content.as_bytes());
        format!("{:x}", hasher.finalize())
    }
}

impl Default for Canonicalizer {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_canonicalizer_creation() {
        let canonicalizer = Canonicalizer::new();
        // Just verify it exists
    }

    #[test]
    fn test_compute_hash() {
        let canonicalizer = Canonicalizer::new();

        let hash1 = canonicalizer.compute_hash("test content");
        let hash2 = canonicalizer.compute_hash("test content");
        let hash3 = canonicalizer.compute_hash("different content");

        assert_eq!(hash1, hash2, "Same content should produce same hash");
        assert_ne!(
            hash1, hash3,
            "Different content should produce different hash"
        );
    }

    #[test]
    fn test_normalize_whitespace() {
        let canonicalizer = Canonicalizer::new();

        let input = "line 1  \nline 2   \nline 3";
        let normalized = canonicalizer.normalize_whitespace(input);

        assert!(
            !normalized.contains("  "),
            "Trailing spaces should be removed"
        );
        assert!(normalized.contains("\n"), "Newlines should be preserved");
    }
}
