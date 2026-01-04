//! Package format (.gpack) handling and serialization
//!
//! This module defines the gpack archive format, including:
//! - Archive structure and headers
//! - Compression and decompression
//! - Signature verification
//! - Format versioning

use serde::{Deserialize, Serialize};

use super::error::{GpackError, GpackResult};

/// Magic bytes identifying a gpack archive
pub const GPACK_MAGIC: &[u8; 4] = b"GPCK";

/// Current gpack format version
pub const GPACK_VERSION: u8 = 1;

/// The gpack archive format specification
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct GpackFormat {
    /// Format version number
    pub version: u8,
    /// Compression algorithm used
    pub compression: CompressionType,
    /// Whether the archive is signed
    pub signed: bool,
}

impl Default for GpackFormat {
    fn default() -> Self {
        Self {
            version: GPACK_VERSION,
            compression: CompressionType::Zstd,
            signed: true,
        }
    }
}

/// Supported compression algorithms
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum CompressionType {
    /// No compression
    None,
    /// Gzip compression
    Gzip,
    /// Zstandard compression (default, best ratio/speed)
    Zstd,
}

/// A gpack package archive
#[derive(Debug)]
pub struct PackageArchive {
    /// Archive format specification
    pub format: GpackFormat,
    /// Package manifest bytes
    pub manifest: Vec<u8>,
    /// Package content bytes (compressed)
    pub content: Vec<u8>,
    /// Optional Ed25519 signature
    pub signature: Option<Vec<u8>>,
    /// SHA-256 checksum of content
    pub checksum: String,
}

impl PackageArchive {
    /// Create a new package archive
    pub fn new(manifest: Vec<u8>, content: Vec<u8>) -> Self {
        Self {
            format: GpackFormat::default(),
            manifest,
            content,
            signature: None,
            checksum: String::new(),
        }
    }

    /// Parse a gpack archive from bytes
    pub fn from_bytes(_bytes: &[u8]) -> GpackResult<Self> {
        // TODO: Implement archive parsing
        // - Verify magic bytes
        // - Read format header
        // - Decompress content
        // - Verify checksum
        Err(GpackError::InvalidFormat(
            "Not yet implemented".to_string(),
        ))
    }

    /// Serialize the archive to bytes
    pub fn to_bytes(&self) -> GpackResult<Vec<u8>> {
        // TODO: Implement archive serialization
        // - Write magic bytes
        // - Write format header
        // - Compress content
        // - Calculate and write checksum
        Err(GpackError::InvalidFormat(
            "Not yet implemented".to_string(),
        ))
    }

    /// Verify the archive signature
    pub fn verify_signature(&self, _public_key: &[u8]) -> GpackResult<bool> {
        // TODO: Implement Ed25519 signature verification
        Ok(false)
    }

    /// Calculate SHA-256 checksum
    pub fn calculate_checksum(&self) -> String {
        use sha2::{Digest, Sha256};
        let mut hasher = Sha256::new();
        hasher.update(&self.content);
        hex::encode(hasher.finalize())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_default_format() {
        let format = GpackFormat::default();
        assert_eq!(format.version, GPACK_VERSION);
        assert_eq!(format.compression, CompressionType::Zstd);
        assert!(format.signed);
    }

    #[test]
    fn test_archive_creation() {
        let manifest = b"[package]\nname = \"test\"".to_vec();
        let content = b"test content".to_vec();
        let archive = PackageArchive::new(manifest.clone(), content.clone());

        assert_eq!(archive.manifest, manifest);
        assert_eq!(archive.content, content);
        assert!(archive.signature.is_none());
    }

    #[test]
    fn test_checksum_calculation() {
        let archive = PackageArchive::new(vec![], b"test content".to_vec());
        let checksum = archive.calculate_checksum();

        // SHA-256 of "test content"
        assert!(!checksum.is_empty());
        assert_eq!(checksum.len(), 64); // 32 bytes = 64 hex chars
    }
}
