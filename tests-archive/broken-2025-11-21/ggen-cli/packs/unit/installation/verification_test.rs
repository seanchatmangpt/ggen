//! Unit tests for package verification
//!
//! Tests cover:
//! - Signature verification
//! - Checksum validation
//! - Manifest validation
//! - Metadata verification
//! - Version compatibility

use sha2::{Digest, Sha256};
use std::collections::HashMap;

#[derive(Debug, Clone)]
pub struct PackageManifest {
    pub name: String,
    pub version: String,
    pub files: HashMap<String, String>, // filename -> checksum
    pub ggen_compat: String,
}

#[derive(Debug, Clone, PartialEq)]
pub enum VerificationError {
    SignatureMismatch,
    ChecksumMismatch(String),
    MissingFile(String),
    InvalidManifest,
    IncompatibleVersion,
}

impl std::fmt::Display for VerificationError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::SignatureMismatch => write!(f, "Signature verification failed"),
            Self::ChecksumMismatch(file) => write!(f, "Checksum mismatch for file: {}", file),
            Self::MissingFile(file) => write!(f, "Missing file: {}", file),
            Self::InvalidManifest => write!(f, "Invalid manifest format"),
            Self::IncompatibleVersion => write!(f, "Incompatible ggen version"),
        }
    }
}

impl std::error::Error for VerificationError {}

pub struct PackageVerifier {
    public_key: Option<Vec<u8>>,
}

impl PackageVerifier {
    pub fn new() -> Self {
        Self { public_key: None }
    }

    pub fn with_public_key(mut self, key: Vec<u8>) -> Self {
        self.public_key = Some(key);
        self
    }

    pub fn verify_signature(&self, data: &[u8], signature: &[u8]) -> Result<(), VerificationError> {
        // Simplified signature verification (in real implementation would use ed25519)
        if let Some(ref _key) = self.public_key {
            // Mock verification: signature must be SHA256 of data + key
            let mut hasher = Sha256::new();
            hasher.update(data);
            hasher.update(_key);
            let expected = hasher.finalize();

            if signature == expected.as_slice() {
                Ok(())
            } else {
                Err(VerificationError::SignatureMismatch)
            }
        } else {
            Ok(()) // Skip verification if no key provided
        }
    }

    pub fn verify_checksum(
        &self, file_data: &[u8], expected_checksum: &str,
    ) -> Result<(), VerificationError> {
        let mut hasher = Sha256::new();
        hasher.update(file_data);
        let actual = format!("{:x}", hasher.finalize());

        if actual == expected_checksum {
            Ok(())
        } else {
            Err(VerificationError::ChecksumMismatch(
                expected_checksum.to_string(),
            ))
        }
    }

    pub fn verify_manifest(
        &self, manifest: &PackageManifest, files: &HashMap<String, Vec<u8>>,
    ) -> Result<(), VerificationError> {
        // Check all manifest files exist
        for (filename, expected_checksum) in &manifest.files {
            let file_data = files
                .get(filename)
                .ok_or_else(|| VerificationError::MissingFile(filename.clone()))?;

            self.verify_checksum(file_data, expected_checksum)?;
        }

        Ok(())
    }

    pub fn verify_version_compatibility(
        &self, manifest: &PackageManifest, current_version: &str,
    ) -> Result<(), VerificationError> {
        // Simplified version check
        if manifest.ggen_compat.contains(current_version) || manifest.ggen_compat == "*" {
            Ok(())
        } else {
            Err(VerificationError::IncompatibleVersion)
        }
    }

    pub fn verify_all(
        &self, manifest: &PackageManifest, files: &HashMap<String, Vec<u8>>, current_version: &str,
    ) -> Result<(), VerificationError> {
        self.verify_manifest(manifest, files)?;
        self.verify_version_compatibility(manifest, current_version)?;
        Ok(())
    }
}

// ============================================================================
// UNIT TESTS - Signature Verification
// ============================================================================

#[test]
fn test_signature_verification_success() {
    let key = b"test_public_key".to_vec();
    let verifier = PackageVerifier::new().with_public_key(key.clone());

    let data = b"package data";

    // Create expected signature
    let mut hasher = Sha256::new();
    hasher.update(data);
    hasher.update(&key);
    let signature = hasher.finalize().to_vec();

    let result = verifier.verify_signature(data, &signature);
    assert!(result.is_ok());
}

#[test]
fn test_signature_verification_failure() {
    let key = b"test_public_key".to_vec();
    let verifier = PackageVerifier::new().with_public_key(key);

    let data = b"package data";
    let wrong_signature = b"invalid signature";

    let result = verifier.verify_signature(data, wrong_signature);
    assert!(result.is_err());
    assert_eq!(result.unwrap_err(), VerificationError::SignatureMismatch);
}

#[test]
fn test_signature_verification_no_key() {
    let verifier = PackageVerifier::new();

    let data = b"package data";
    let signature = b"any signature";

    let result = verifier.verify_signature(data, signature);
    assert!(result.is_ok()); // Should skip verification if no key
}

// ============================================================================
// UNIT TESTS - Checksum Verification
// ============================================================================

#[test]
fn test_checksum_verification_success() {
    let verifier = PackageVerifier::new();
    let data = b"test file content";

    // Calculate expected checksum
    let mut hasher = Sha256::new();
    hasher.update(data);
    let expected = format!("{:x}", hasher.finalize());

    let result = verifier.verify_checksum(data, &expected);
    assert!(result.is_ok());
}

#[test]
fn test_checksum_verification_failure() {
    let verifier = PackageVerifier::new();
    let data = b"test file content";
    let wrong_checksum = "0000000000000000000000000000000000000000000000000000000000000000";

    let result = verifier.verify_checksum(data, wrong_checksum);
    assert!(result.is_err());
    match result.unwrap_err() {
        VerificationError::ChecksumMismatch(_) => (),
        _ => panic!("Expected ChecksumMismatch error"),
    }
}

// ============================================================================
// UNIT TESTS - Manifest Verification
// ============================================================================

#[test]
fn test_manifest_verification_success() {
    let verifier = PackageVerifier::new();

    let file1_data = b"content of file 1";
    let file2_data = b"content of file 2";

    let mut hasher = Sha256::new();
    hasher.update(file1_data);
    let file1_checksum = format!("{:x}", hasher.finalize());

    let mut hasher = Sha256::new();
    hasher.update(file2_data);
    let file2_checksum = format!("{:x}", hasher.finalize());

    let mut manifest_files = HashMap::new();
    manifest_files.insert("file1.txt".to_string(), file1_checksum);
    manifest_files.insert("file2.txt".to_string(), file2_checksum);

    let manifest = PackageManifest {
        name: "test-package".to_string(),
        version: "1.0.0".to_string(),
        files: manifest_files,
        ggen_compat: "*".to_string(),
    };

    let mut actual_files = HashMap::new();
    actual_files.insert("file1.txt".to_string(), file1_data.to_vec());
    actual_files.insert("file2.txt".to_string(), file2_data.to_vec());

    let result = verifier.verify_manifest(&manifest, &actual_files);
    assert!(result.is_ok());
}

#[test]
fn test_manifest_verification_missing_file() {
    let verifier = PackageVerifier::new();

    let mut manifest_files = HashMap::new();
    manifest_files.insert("file1.txt".to_string(), "abc123".to_string());
    manifest_files.insert("file2.txt".to_string(), "def456".to_string());

    let manifest = PackageManifest {
        name: "test-package".to_string(),
        version: "1.0.0".to_string(),
        files: manifest_files,
        ggen_compat: "*".to_string(),
    };

    let mut actual_files = HashMap::new();
    actual_files.insert("file1.txt".to_string(), b"data".to_vec());
    // file2.txt is missing

    let result = verifier.verify_manifest(&manifest, &actual_files);
    assert!(result.is_err());
    match result.unwrap_err() {
        VerificationError::MissingFile(name) => assert_eq!(name, "file2.txt"),
        _ => panic!("Expected MissingFile error"),
    }
}

#[test]
fn test_manifest_verification_checksum_mismatch() {
    let verifier = PackageVerifier::new();

    let mut manifest_files = HashMap::new();
    manifest_files.insert("file1.txt".to_string(), "wrong_checksum".to_string());

    let manifest = PackageManifest {
        name: "test-package".to_string(),
        version: "1.0.0".to_string(),
        files: manifest_files,
        ggen_compat: "*".to_string(),
    };

    let mut actual_files = HashMap::new();
    actual_files.insert("file1.txt".to_string(), b"some data".to_vec());

    let result = verifier.verify_manifest(&manifest, &actual_files);
    assert!(result.is_err());
    match result.unwrap_err() {
        VerificationError::ChecksumMismatch(_) => (),
        _ => panic!("Expected ChecksumMismatch error"),
    }
}

// ============================================================================
// UNIT TESTS - Version Compatibility
// ============================================================================

#[test]
fn test_version_compatibility_match() {
    let verifier = PackageVerifier::new();

    let manifest = PackageManifest {
        name: "test".to_string(),
        version: "1.0.0".to_string(),
        files: HashMap::new(),
        ggen_compat: "0.1.0".to_string(),
    };

    let result = verifier.verify_version_compatibility(&manifest, "0.1.0");
    assert!(result.is_ok());
}

#[test]
fn test_version_compatibility_wildcard() {
    let verifier = PackageVerifier::new();

    let manifest = PackageManifest {
        name: "test".to_string(),
        version: "1.0.0".to_string(),
        files: HashMap::new(),
        ggen_compat: "*".to_string(),
    };

    let result = verifier.verify_version_compatibility(&manifest, "99.99.99");
    assert!(result.is_ok());
}

#[test]
fn test_version_compatibility_mismatch() {
    let verifier = PackageVerifier::new();

    let manifest = PackageManifest {
        name: "test".to_string(),
        version: "1.0.0".to_string(),
        files: HashMap::new(),
        ggen_compat: "0.1.0".to_string(),
    };

    let result = verifier.verify_version_compatibility(&manifest, "0.2.0");
    assert!(result.is_err());
    assert_eq!(result.unwrap_err(), VerificationError::IncompatibleVersion);
}

// ============================================================================
// INTEGRATION TESTS - Complete Verification
// ============================================================================

#[test]
fn test_verify_all_success() {
    let verifier = PackageVerifier::new();

    let file_data = b"test content";
    let mut hasher = Sha256::new();
    hasher.update(file_data);
    let checksum = format!("{:x}", hasher.finalize());

    let mut manifest_files = HashMap::new();
    manifest_files.insert("test.txt".to_string(), checksum);

    let manifest = PackageManifest {
        name: "test-package".to_string(),
        version: "1.0.0".to_string(),
        files: manifest_files,
        ggen_compat: "0.1.0".to_string(),
    };

    let mut files = HashMap::new();
    files.insert("test.txt".to_string(), file_data.to_vec());

    let result = verifier.verify_all(&manifest, &files, "0.1.0");
    assert!(result.is_ok());
}

#[test]
fn test_verify_all_version_mismatch() {
    let verifier = PackageVerifier::new();

    let manifest = PackageManifest {
        name: "test".to_string(),
        version: "1.0.0".to_string(),
        files: HashMap::new(),
        ggen_compat: "0.1.0".to_string(),
    };

    let files = HashMap::new();

    let result = verifier.verify_all(&manifest, &files, "0.2.0");
    assert!(result.is_err());
}

// ============================================================================
// FMEA MAPPING TESTS
// ============================================================================

#[test]
fn test_fmea_tampered_package_detection() {
    // FMEA Failure Mode: Tampered package (RPN 80)
    // Mitigation: Signature verification + checksum validation

    let key = b"trusted_key".to_vec();
    let verifier = PackageVerifier::new().with_public_key(key.clone());

    let original_data = b"original package";
    let tampered_data = b"tampered package";

    let mut hasher = Sha256::new();
    hasher.update(original_data);
    hasher.update(&key);
    let signature = hasher.finalize().to_vec();

    // Verification should fail for tampered data
    let result = verifier.verify_signature(tampered_data, &signature);
    assert!(result.is_err());
    assert_eq!(result.unwrap_err(), VerificationError::SignatureMismatch);
}

#[test]
fn test_fmea_incomplete_package_detection() {
    // FMEA Failure Mode: Incomplete package (RPN 56)
    // Mitigation: Manifest verification

    let verifier = PackageVerifier::new();

    let mut manifest_files = HashMap::new();
    manifest_files.insert("required1.txt".to_string(), "hash1".to_string());
    manifest_files.insert("required2.txt".to_string(), "hash2".to_string());
    manifest_files.insert("required3.txt".to_string(), "hash3".to_string());

    let manifest = PackageManifest {
        name: "test".to_string(),
        version: "1.0.0".to_string(),
        files: manifest_files,
        ggen_compat: "*".to_string(),
    };

    let mut files = HashMap::new();
    files.insert("required1.txt".to_string(), b"data".to_vec());
    files.insert("required2.txt".to_string(), b"data".to_vec());
    // required3.txt is missing

    let result = verifier.verify_manifest(&manifest, &files);
    assert!(result.is_err());
    match result.unwrap_err() {
        VerificationError::MissingFile(name) => assert_eq!(name, "required3.txt"),
        _ => panic!("Expected MissingFile error"),
    }
}

#[test]
fn test_fmea_version_incompatibility_detection() {
    // FMEA Failure Mode: Version incompatibility (RPN 48)
    // Mitigation: ggen version compatibility check

    let verifier = PackageVerifier::new();

    let manifest = PackageManifest {
        name: "modern-package".to_string(),
        version: "2.0.0".to_string(),
        files: HashMap::new(),
        ggen_compat: ">=2.0.0".to_string(),
    };

    // Old version should be rejected
    let result = verifier.verify_version_compatibility(&manifest, "1.0.0");
    assert!(result.is_err());
    assert_eq!(result.unwrap_err(), VerificationError::IncompatibleVersion);
}
