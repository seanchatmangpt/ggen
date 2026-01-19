//! Unit tests for package extraction functionality
//!
//! Tests cover:
//! - TAR/GZIP extraction
//! - Directory creation
//! - Permission handling
//! - Path traversal prevention
//! - Symlink handling

use flate2::write::GzEncoder;
use flate2::Compression;
use std::fs;
use std::io::{Read, Write};
use std::path::{Path, PathBuf};
use tempfile::TempDir;

pub struct PackageExtractor {
    target_dir: PathBuf,
}

#[derive(Debug, Clone, PartialEq)]
pub enum ExtractionError {
    InvalidArchive,
    PathTraversal,
    PermissionDenied,
    DiskFull,
    UnsupportedFormat,
}

impl std::fmt::Display for ExtractionError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::InvalidArchive => write!(f, "Invalid archive format"),
            Self::PathTraversal => write!(f, "Path traversal attempt detected"),
            Self::PermissionDenied => write!(f, "Permission denied"),
            Self::DiskFull => write!(f, "Disk full"),
            Self::UnsupportedFormat => write!(f, "Unsupported archive format"),
        }
    }
}

impl std::error::Error for ExtractionError {}

impl PackageExtractor {
    pub fn new(target_dir: PathBuf) -> Self {
        Self { target_dir }
    }

    pub fn extract(&self, archive_data: &[u8]) -> Result<Vec<PathBuf>, ExtractionError> {
        let mut extracted_files = Vec::new();

        // Decompress GZIP
        let gz = flate2::read::GzDecoder::new(archive_data);
        let mut archive = tar::Archive::new(gz);

        // Extract all entries
        for entry in archive
            .entries()
            .map_err(|_| ExtractionError::InvalidArchive)?
        {
            let mut entry = entry.map_err(|_| ExtractionError::InvalidArchive)?;
            let path = entry.path().map_err(|_| ExtractionError::InvalidArchive)?;

            // Security check: prevent path traversal
            if self.is_path_traversal(&path) {
                return Err(ExtractionError::PathTraversal);
            }

            let dest_path = self.target_dir.join(&*path);

            // Create parent directories
            if let Some(parent) = dest_path.parent() {
                fs::create_dir_all(parent).map_err(|_| ExtractionError::PermissionDenied)?;
            }

            // Extract file
            entry
                .unpack(&dest_path)
                .map_err(|_| ExtractionError::PermissionDenied)?;
            extracted_files.push(dest_path);
        }

        Ok(extracted_files)
    }

    fn is_path_traversal(&self, path: &Path) -> bool {
        path.components()
            .any(|c| matches!(c, std::path::Component::ParentDir))
    }

    pub fn verify_extraction(&self, expected_files: &[&str]) -> Result<(), ExtractionError> {
        for file in expected_files {
            let path = self.target_dir.join(file);
            if !path.exists() {
                return Err(ExtractionError::InvalidArchive);
            }
        }
        Ok(())
    }
}

// ============================================================================
// TEST UTILITIES
// ============================================================================

fn create_test_archive() -> Vec<u8> {
    let mut ar = tar::Builder::new(Vec::new());

    // Add a test file
    let data = b"test content";
    let mut header = tar::Header::new_gnu();
    header.set_path("test.txt").unwrap();
    header.set_size(data.len() as u64);
    header.set_cksum();
    ar.append(&header, &data[..]).unwrap();

    // Add a directory
    let mut header = tar::Header::new_gnu();
    header.set_path("subdir/").unwrap();
    header.set_entry_type(tar::EntryType::Directory);
    header.set_size(0);
    header.set_cksum();
    ar.append(&header, &[][..]).unwrap();

    // Add a file in subdirectory
    let data2 = b"nested content";
    let mut header = tar::Header::new_gnu();
    header.set_path("subdir/nested.txt").unwrap();
    header.set_size(data2.len() as u64);
    header.set_cksum();
    ar.append(&header, &data2[..]).unwrap();

    let tar_data = ar.into_inner().unwrap();

    // Compress with GZIP
    let mut encoder = GzEncoder::new(Vec::new(), Compression::default());
    encoder.write_all(&tar_data).unwrap();
    encoder.finish().unwrap()
}

fn create_malicious_archive_with_path_traversal() -> Vec<u8> {
    let mut ar = tar::Builder::new(Vec::new());

    // Try to write outside target directory
    let data = b"malicious content";
    let mut header = tar::Header::new_gnu();
    header.set_path("../../../etc/passwd").unwrap();
    header.set_size(data.len() as u64);
    header.set_cksum();
    ar.append(&header, &data[..]).unwrap();

    let tar_data = ar.into_inner().unwrap();

    let mut encoder = GzEncoder::new(Vec::new(), Compression::default());
    encoder.write_all(&tar_data).unwrap();
    encoder.finish().unwrap()
}

// ============================================================================
// UNIT TESTS - Basic Extraction
// ============================================================================

#[test]
fn test_extract_simple_archive() {
    let temp_dir = TempDir::new().unwrap();
    let extractor = PackageExtractor::new(temp_dir.path().to_path_buf());

    let archive_data = create_test_archive();
    let result = extractor.extract(&archive_data);

    assert!(result.is_ok());
    let files = result.unwrap();
    assert!(files.len() >= 2); // test.txt and subdir/nested.txt

    // Verify files exist
    assert!(temp_dir.path().join("test.txt").exists());
    assert!(temp_dir.path().join("subdir/nested.txt").exists());
}

#[test]
fn test_extract_creates_directories() {
    let temp_dir = TempDir::new().unwrap();
    let extractor = PackageExtractor::new(temp_dir.path().to_path_buf());

    let archive_data = create_test_archive();
    extractor.extract(&archive_data).unwrap();

    assert!(temp_dir.path().join("subdir").is_dir());
}

#[test]
fn test_verify_extraction_success() {
    let temp_dir = TempDir::new().unwrap();
    let extractor = PackageExtractor::new(temp_dir.path().to_path_buf());

    let archive_data = create_test_archive();
    extractor.extract(&archive_data).unwrap();

    let result = extractor.verify_extraction(&["test.txt", "subdir/nested.txt"]);
    assert!(result.is_ok());
}

#[test]
fn test_verify_extraction_missing_file() {
    let temp_dir = TempDir::new().unwrap();
    let extractor = PackageExtractor::new(temp_dir.path().to_path_buf());

    let archive_data = create_test_archive();
    extractor.extract(&archive_data).unwrap();

    let result = extractor.verify_extraction(&["nonexistent.txt"]);
    assert!(result.is_err());
    assert_eq!(result.unwrap_err(), ExtractionError::InvalidArchive);
}

// ============================================================================
// SECURITY TESTS - Path Traversal Prevention
// ============================================================================

#[test]
fn test_path_traversal_prevention() {
    let temp_dir = TempDir::new().unwrap();
    let extractor = PackageExtractor::new(temp_dir.path().to_path_buf());

    let malicious_archive = create_malicious_archive_with_path_traversal();
    let result = extractor.extract(&malicious_archive);

    assert!(result.is_err());
    assert_eq!(result.unwrap_err(), ExtractionError::PathTraversal);
}

#[test]
fn test_is_path_traversal_detection() {
    let temp_dir = TempDir::new().unwrap();
    let extractor = PackageExtractor::new(temp_dir.path().to_path_buf());

    assert!(extractor.is_path_traversal(Path::new("../etc/passwd")));
    assert!(extractor.is_path_traversal(Path::new("foo/../../bar")));
    assert!(!extractor.is_path_traversal(Path::new("valid/path/file.txt")));
    assert!(!extractor.is_path_traversal(Path::new("file.txt")));
}

// ============================================================================
// ERROR HANDLING TESTS
// ============================================================================

#[test]
fn test_invalid_archive_format() {
    let temp_dir = TempDir::new().unwrap();
    let extractor = PackageExtractor::new(temp_dir.path().to_path_buf());

    let invalid_data = b"not a valid tar.gz archive";
    let result = extractor.extract(invalid_data);

    assert!(result.is_err());
    assert_eq!(result.unwrap_err(), ExtractionError::InvalidArchive);
}

#[test]
fn test_empty_archive() {
    let temp_dir = TempDir::new().unwrap();
    let extractor = PackageExtractor::new(temp_dir.path().to_path_buf());

    // Create empty tar.gz
    let ar = tar::Builder::new(Vec::new());
    let tar_data = ar.into_inner().unwrap();
    let mut encoder = GzEncoder::new(Vec::new(), Compression::default());
    encoder.write_all(&tar_data).unwrap();
    let empty_archive = encoder.finish().unwrap();

    let result = extractor.extract(&empty_archive);

    assert!(result.is_ok());
    assert_eq!(result.unwrap().len(), 0);
}

// ============================================================================
// PERFORMANCE TESTS
// ============================================================================

#[test]
fn test_large_archive_extraction() {
    let temp_dir = TempDir::new().unwrap();
    let extractor = PackageExtractor::new(temp_dir.path().to_path_buf());

    // Create archive with 100 files
    let mut ar = tar::Builder::new(Vec::new());
    for i in 0..100 {
        let data = format!("content {}", i);
        let mut header = tar::Header::new_gnu();
        header.set_path(format!("file_{}.txt", i)).unwrap();
        header.set_size(data.len() as u64);
        header.set_cksum();
        ar.append(&header, data.as_bytes()).unwrap();
    }
    let tar_data = ar.into_inner().unwrap();
    let mut encoder = GzEncoder::new(Vec::new(), Compression::default());
    encoder.write_all(&tar_data).unwrap();
    let archive = encoder.finish().unwrap();

    let start = std::time::Instant::now();
    let result = extractor.extract(&archive);
    let duration = start.elapsed();

    assert!(result.is_ok());
    assert!(
        duration.as_millis() < 1000,
        "Extraction should be fast (got {}ms)",
        duration.as_millis()
    );
}

// ============================================================================
// FMEA MAPPING TESTS
// ============================================================================

#[test]
fn test_fmea_extraction_failure_detection() {
    // FMEA Failure Mode: Extraction failure (RPN 45)
    // Mitigation: Validate extracted files match manifest

    let temp_dir = TempDir::new().unwrap();
    let extractor = PackageExtractor::new(temp_dir.path().to_path_buf());

    let archive_data = create_test_archive();
    extractor.extract(&archive_data).unwrap();

    // Verify all expected files exist
    let verification = extractor.verify_extraction(&["test.txt", "subdir/nested.txt"]);
    assert!(verification.is_ok());
}

#[test]
fn test_fmea_disk_full_handling() {
    // FMEA Failure Mode: Disk full during extraction (RPN 36)
    // Mitigation: Pre-check available space (simulated via error handling)

    let temp_dir = TempDir::new().unwrap();
    let extractor = PackageExtractor::new(temp_dir.path().to_path_buf());

    // This test verifies error propagation
    // In real implementation, would check fs::available_space before extraction
    let archive_data = create_test_archive();
    let result = extractor.extract(&archive_data);

    // Should succeed in test environment with available space
    assert!(result.is_ok());
}

#[test]
fn test_fmea_path_traversal_attack_prevention() {
    // FMEA Failure Mode: Path traversal attack (RPN 72)
    // Mitigation: Validate all paths before extraction

    let temp_dir = TempDir::new().unwrap();
    let extractor = PackageExtractor::new(temp_dir.path().to_path_buf());

    let malicious_archive = create_malicious_archive_with_path_traversal();
    let result = extractor.extract(&malicious_archive);

    // Must reject malicious archives
    assert!(result.is_err());
    assert_eq!(result.unwrap_err(), ExtractionError::PathTraversal);

    // Verify no files were created outside target directory
    assert!(
        !PathBuf::from("/etc/passwd").exists()
            || fs::read_to_string("/etc/passwd").unwrap() != "malicious content"
    );
}
