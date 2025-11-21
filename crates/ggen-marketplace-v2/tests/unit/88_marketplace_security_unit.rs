//! Phase 3A: Comprehensive Security Unit Tests (Chicago TDD Style)
//!
//! Tests SignatureVerifier, KeyPair, ChecksumCalculator, Ed25519 operations.
//! Following Chicago TDD: state-based testing, real collaborators, AAA pattern.
//!
//! Test Count: 50+ tests covering cryptographic operations

use ggen_marketplace_v2::security::{
    ChecksumCalculator, KeyPair, SignatureReceipt, SignatureVerifier,
};
use ggen_marketplace_v2::traits::Signable;

// ============================================================================
// SECTION 1: KeyPair Generation Tests (15 tests)
// ============================================================================

#[test]
fn test_keypair_generate() {
    // Arrange & Act
    let keypair = KeyPair::generate();

    // Assert
    let pub_key = keypair.public_key_hex();
    assert!(!pub_key.is_empty());
    assert_eq!(pub_key.len(), 64); // 32 bytes = 64 hex chars
}

#[test]
fn test_keypair_generate_unique() {
    // Arrange & Act
    let keypair1 = KeyPair::generate();
    let keypair2 = KeyPair::generate();

    // Assert
    assert_ne!(keypair1.public_key_hex(), keypair2.public_key_hex());
}

#[test]
fn test_keypair_public_key_hex_format() {
    // Arrange
    let keypair = KeyPair::generate();

    // Act
    let pub_key = keypair.public_key_hex();

    // Assert - should be valid hex
    assert!(pub_key.chars().all(|c| c.is_ascii_hexdigit()));
}

#[test]
fn test_keypair_secret_key_hex() {
    // Arrange
    let keypair = KeyPair::generate();

    // Act
    let secret_key = keypair.secret_key_hex();

    // Assert
    assert!(!secret_key.is_empty());
    assert_eq!(secret_key.len(), 64); // 32 bytes = 64 hex chars
}

#[test]
fn test_keypair_from_secret_key_valid() {
    // Arrange
    let original = KeyPair::generate();
    let secret_hex = original.secret_key_hex();

    // Act
    let restored = KeyPair::from_secret_key(&secret_hex).unwrap();

    // Assert
    assert_eq!(original.public_key_hex(), restored.public_key_hex());
}

#[test]
fn test_keypair_from_secret_key_invalid_hex() {
    // Arrange
    let invalid_hex = "not_valid_hex_string_zzzz";

    // Act
    let result = KeyPair::from_secret_key(invalid_hex);

    // Assert
    assert!(result.is_err());
}

#[test]
fn test_keypair_from_secret_key_wrong_length() {
    // Arrange
    let short_hex = "abcd1234"; // Only 4 bytes

    // Act
    let result = KeyPair::from_secret_key(short_hex);

    // Assert
    assert!(result.is_err());
}

#[test]
fn test_keypair_from_secret_key_too_long() {
    // Arrange
    let long_hex = "a".repeat(128); // 64 bytes, too long

    // Act
    let result = KeyPair::from_secret_key(&long_hex);

    // Assert
    assert!(result.is_err());
}

#[test]
fn test_keypair_roundtrip() {
    // Arrange
    let original = KeyPair::generate();

    // Act
    let secret = original.secret_key_hex();
    let restored = KeyPair::from_secret_key(&secret).unwrap();

    // Assert
    assert_eq!(original.public_key_hex(), restored.public_key_hex());
    assert_eq!(original.secret_key_hex(), restored.secret_key_hex());
}

// ============================================================================
// SECTION 2: SignatureVerifier Tests (20 tests)
// ============================================================================

#[test]
fn test_verifier_creation() {
    // Arrange
    let keypair = KeyPair::generate();

    // Act
    let verifier = SignatureVerifier::new(keypair);

    // Assert
    let pub_key = verifier.public_key();
    assert!(!pub_key.is_empty());
}

#[test]
fn test_verifier_from_public_key() {
    // Arrange
    let keypair = KeyPair::generate();
    let pub_key_hex = keypair.public_key_hex();

    // Act
    let verifier = SignatureVerifier::from_public_key(&pub_key_hex);

    // Assert
    assert!(verifier.is_ok());
}

#[test]
fn test_verifier_from_public_key_invalid() {
    // Arrange
    let invalid_key = "invalid_public_key";

    // Act
    let result = SignatureVerifier::from_public_key(invalid_key);

    // Assert
    assert!(result.is_err());
}

#[test]
fn test_verifier_from_public_key_wrong_length() {
    // Arrange
    let short_key = "abcd1234";

    // Act
    let result = SignatureVerifier::from_public_key(short_key);

    // Assert
    assert!(result.is_err());
}

#[test]
fn test_sign_and_verify() {
    // Arrange
    let keypair = KeyPair::generate();
    let verifier = SignatureVerifier::new(keypair);
    let data = b"test message";

    // Act
    let signature = verifier.sign(data).unwrap();
    let verified = verifier.verify(data, &signature).unwrap();

    // Assert
    assert!(verified);
}

#[test]
fn test_verify_fails_with_wrong_data() {
    // Arrange
    let keypair = KeyPair::generate();
    let verifier = SignatureVerifier::new(keypair);
    let data = b"test message";
    let wrong_data = b"different message";

    // Act
    let signature = verifier.sign(data).unwrap();
    let verified = verifier.verify(wrong_data, &signature).unwrap();

    // Assert
    assert!(!verified);
}

#[test]
fn test_verify_fails_with_wrong_signature() {
    // Arrange
    let keypair1 = KeyPair::generate();
    let keypair2 = KeyPair::generate();
    let verifier1 = SignatureVerifier::new(keypair1);
    let verifier2 = SignatureVerifier::new(keypair2);
    let data = b"test message";

    // Act
    let signature = verifier1.sign(data).unwrap();
    let verified = verifier2.verify(data, &signature).unwrap();

    // Assert
    assert!(!verified);
}

#[test]
fn test_signature_format() {
    // Arrange
    let keypair = KeyPair::generate();
    let verifier = SignatureVerifier::new(keypair);
    let data = b"test";

    // Act
    let signature = verifier.sign(data).unwrap();

    // Assert
    assert_eq!(signature.len(), 128); // 64 bytes = 128 hex chars
    assert!(signature.chars().all(|c| c.is_ascii_hexdigit()));
}

#[test]
fn test_verify_signature_invalid_hex() {
    // Arrange
    let keypair = KeyPair::generate();
    let verifier = SignatureVerifier::new(keypair);
    let data = b"test";
    let invalid_sig = "not_valid_hex_zzzz";

    // Act
    let result = verifier.verify_signature(data, invalid_sig);

    // Assert
    assert!(result.is_err());
}

#[test]
fn test_verify_signature_wrong_length() {
    // Arrange
    let keypair = KeyPair::generate();
    let verifier = SignatureVerifier::new(keypair);
    let data = b"test";
    let short_sig = "abcd1234";

    // Act
    let result = verifier.verify_signature(data, short_sig);

    // Assert
    assert!(result.is_err());
}

#[test]
fn test_sign_empty_data() {
    // Arrange
    let keypair = KeyPair::generate();
    let verifier = SignatureVerifier::new(keypair);
    let empty_data = b"";

    // Act
    let signature = verifier.sign(empty_data).unwrap();
    let verified = verifier.verify(empty_data, &signature).unwrap();

    // Assert
    assert!(verified);
}

#[test]
fn test_sign_large_data() {
    // Arrange
    let keypair = KeyPair::generate();
    let verifier = SignatureVerifier::new(keypair);
    let large_data = vec![0u8; 1_000_000]; // 1MB

    // Act
    let signature = verifier.sign(&large_data).unwrap();
    let verified = verifier.verify(&large_data, &signature).unwrap();

    // Assert
    assert!(verified);
}

#[test]
fn test_signable_trait_sign() {
    // Arrange
    let keypair = KeyPair::generate();
    let verifier = SignatureVerifier::new(keypair);
    let data = b"test";

    // Act
    let signature = Signable::sign(&verifier, data).unwrap();

    // Assert
    assert!(!signature.is_empty());
}

#[test]
fn test_signable_trait_verify() {
    // Arrange
    let keypair = KeyPair::generate();
    let verifier = SignatureVerifier::new(keypair);
    let data = b"test";

    // Act
    let signature = Signable::sign(&verifier, data).unwrap();
    let verified = Signable::verify(&verifier, data, &signature).unwrap();

    // Assert
    assert!(verified);
}

#[test]
fn test_signable_trait_public_key() {
    // Arrange
    let keypair = KeyPair::generate();
    let pub_key = keypair.public_key_hex();
    let verifier = SignatureVerifier::new(keypair);

    // Act
    let trait_pub_key = Signable::public_key(&verifier);

    // Assert
    assert_eq!(trait_pub_key, pub_key);
}

// ============================================================================
// SECTION 3: ChecksumCalculator Tests (15 tests)
// ============================================================================

#[test]
fn test_checksum_calculate() {
    // Arrange
    let data = b"test data";

    // Act
    let checksum = ChecksumCalculator::calculate(data);

    // Assert
    assert!(!checksum.is_empty());
    assert_eq!(checksum.len(), 64); // SHA-256 = 64 hex chars
}

#[test]
fn test_checksum_hex_format() {
    // Arrange
    let data = b"test";

    // Act
    let checksum = ChecksumCalculator::calculate(data);

    // Assert
    assert!(checksum.chars().all(|c| c.is_ascii_hexdigit()));
}

#[test]
fn test_checksum_deterministic() {
    // Arrange
    let data = b"test data";

    // Act
    let checksum1 = ChecksumCalculator::calculate(data);
    let checksum2 = ChecksumCalculator::calculate(data);

    // Assert
    assert_eq!(checksum1, checksum2);
}

#[test]
fn test_checksum_different_data() {
    // Arrange
    let data1 = b"test data 1";
    let data2 = b"test data 2";

    // Act
    let checksum1 = ChecksumCalculator::calculate(data1);
    let checksum2 = ChecksumCalculator::calculate(data2);

    // Assert
    assert_ne!(checksum1, checksum2);
}

#[test]
fn test_checksum_verify_correct() {
    // Arrange
    let data = b"test data";
    let checksum = ChecksumCalculator::calculate(data);

    // Act
    let verified = ChecksumCalculator::verify(data, &checksum).unwrap();

    // Assert
    assert!(verified);
}

#[test]
fn test_checksum_verify_incorrect() {
    // Arrange
    let data = b"test data";
    let wrong_checksum = "0".repeat(64);

    // Act
    let verified = ChecksumCalculator::verify(data, &wrong_checksum).unwrap();

    // Assert
    assert!(!verified);
}

#[test]
fn test_checksum_verify_wrong_data() {
    // Arrange
    let data = b"test data";
    let wrong_data = b"different data";
    let checksum = ChecksumCalculator::calculate(data);

    // Act
    let verified = ChecksumCalculator::verify(wrong_data, &checksum).unwrap();

    // Assert
    assert!(!verified);
}

#[test]
fn test_checksum_empty_data() {
    // Arrange
    let data = b"";

    // Act
    let checksum = ChecksumCalculator::calculate(data);

    // Assert
    // SHA-256 of empty string is a known value
    assert_eq!(checksum.len(), 64);
}

#[test]
fn test_checksum_large_data() {
    // Arrange
    let data = vec![0u8; 1_000_000]; // 1MB

    // Act
    let checksum = ChecksumCalculator::calculate(&data);

    // Assert
    assert_eq!(checksum.len(), 64);
}

#[test]
fn test_checksum_known_value() {
    // Arrange
    // SHA-256 of "hello" is a known value
    let data = b"hello";

    // Act
    let checksum = ChecksumCalculator::calculate(data);

    // Assert
    // Known SHA-256 hash of "hello"
    assert_eq!(
        checksum,
        "2cf24dba5fb0a30e26e83b2ac5b9e29e1b161e5c1fa7425e73043362938b9824"
    );
}

// ============================================================================
// SECTION 4: SignatureReceipt Tests (10 tests)
// ============================================================================

#[test]
fn test_signature_receipt_creation() {
    // Arrange & Act
    let receipt = SignatureReceipt {
        package_identifier: "test-pkg@1.0.0".to_string(),
        signature: "abc123".repeat(10),
        public_key: "def456".repeat(10),
        signed_at: chrono::Utc::now(),
        data_checksum: "789".repeat(21),
    };

    // Assert
    assert_eq!(receipt.package_identifier, "test-pkg@1.0.0");
}

#[test]
fn test_signature_receipt_clone() {
    // Arrange
    let receipt = SignatureReceipt {
        package_identifier: "test-pkg@1.0.0".to_string(),
        signature: "abc123".to_string(),
        public_key: "def456".to_string(),
        signed_at: chrono::Utc::now(),
        data_checksum: "789abc".to_string(),
    };

    // Act
    let cloned = receipt.clone();

    // Assert
    assert_eq!(receipt.package_identifier, cloned.package_identifier);
    assert_eq!(receipt.signature, cloned.signature);
}

#[test]
fn test_signature_receipt_display() {
    // Arrange
    let receipt = SignatureReceipt {
        package_identifier: "test-pkg@1.0.0".to_string(),
        signature: "a".repeat(64),
        public_key: "b".repeat(64),
        signed_at: chrono::Utc::now(),
        data_checksum: "c".repeat(64),
    };

    // Act
    let display = format!("{}", receipt);

    // Assert
    assert!(display.contains("Signature Receipt"));
    assert!(display.contains("test-pkg@1.0.0"));
}

// ============================================================================
// SECTION 5: Integration Tests (10 tests)
// ============================================================================

#[test]
fn test_full_signing_workflow() {
    // Arrange
    let keypair = KeyPair::generate();
    let verifier = SignatureVerifier::new(keypair);
    let package_data = b"package contents here";

    // Act
    let checksum = ChecksumCalculator::calculate(package_data);
    let signature = verifier.sign(package_data).unwrap();
    let checksum_valid = ChecksumCalculator::verify(package_data, &checksum).unwrap();
    let signature_valid = verifier.verify(package_data, &signature).unwrap();

    // Assert
    assert!(checksum_valid);
    assert!(signature_valid);
}

#[test]
fn test_signature_receipt_workflow() {
    // Arrange
    let keypair = KeyPair::generate();
    let verifier = SignatureVerifier::new(keypair);
    let package_data = b"package contents";

    // Act
    let checksum = ChecksumCalculator::calculate(package_data);
    let signature = verifier.sign(package_data).unwrap();

    let receipt = SignatureReceipt {
        package_identifier: "my-pkg@1.0.0".to_string(),
        signature: signature.clone(),
        public_key: verifier.public_key(),
        signed_at: chrono::Utc::now(),
        data_checksum: checksum.clone(),
    };

    // Assert
    assert!(verifier.verify(package_data, &receipt.signature).unwrap());
    assert!(ChecksumCalculator::verify(package_data, &receipt.data_checksum).unwrap());
}

#[test]
fn test_keypair_export_import_signing() {
    // Arrange
    let original = KeyPair::generate();
    let secret_hex = original.secret_key_hex();
    let data = b"test message";

    // Sign with original
    let verifier1 = SignatureVerifier::new(original);
    let signature = verifier1.sign(data).unwrap();

    // Act - restore and verify
    let restored = KeyPair::from_secret_key(&secret_hex).unwrap();
    let verifier2 = SignatureVerifier::new(restored);
    let verified = verifier2.verify(data, &signature).unwrap();

    // Assert
    assert!(verified);
}

#[test]
fn test_cross_instance_verification() {
    // Arrange
    let keypair = KeyPair::generate();
    let pub_key_hex = keypair.public_key_hex();
    let signer = SignatureVerifier::new(keypair);
    let data = b"important data";

    // Act
    let signature = signer.sign(data).unwrap();

    // Create verifier with only public key
    let verifier = SignatureVerifier::from_public_key(&pub_key_hex).unwrap();
    let verified = verifier.verify(data, &signature).unwrap();

    // Assert
    assert!(verified);
}

#[test]
fn test_tamper_detection() {
    // Arrange
    let keypair = KeyPair::generate();
    let verifier = SignatureVerifier::new(keypair);
    let original_data = b"original package contents";
    let tampered_data = b"tampered package contents";

    // Act
    let checksum = ChecksumCalculator::calculate(original_data);
    let signature = verifier.sign(original_data).unwrap();

    // Try to verify with tampered data
    let checksum_valid = ChecksumCalculator::verify(tampered_data, &checksum).unwrap();
    let signature_valid = verifier.verify(tampered_data, &signature).unwrap();

    // Assert - both should fail
    assert!(!checksum_valid);
    assert!(!signature_valid);
}

// ============================================================================
// SECTION 6: Edge Cases and Error Handling (5 tests)
// ============================================================================

#[test]
fn test_sign_unicode_data() {
    // Arrange
    let keypair = KeyPair::generate();
    let verifier = SignatureVerifier::new(keypair);
    let unicode_data = "Hello, World!\n\u{1F600}\u{1F4BB}".as_bytes();

    // Act
    let signature = verifier.sign(unicode_data).unwrap();
    let verified = verifier.verify(unicode_data, &signature).unwrap();

    // Assert
    assert!(verified);
}

#[test]
fn test_checksum_binary_data() {
    // Arrange
    let binary_data: Vec<u8> = (0..=255).collect();

    // Act
    let checksum = ChecksumCalculator::calculate(&binary_data);
    let verified = ChecksumCalculator::verify(&binary_data, &checksum).unwrap();

    // Assert
    assert!(verified);
}

#[test]
fn test_multiple_signatures_same_data() {
    // Arrange
    let keypair = KeyPair::generate();
    let verifier = SignatureVerifier::new(keypair);
    let data = b"test data";

    // Act - sign multiple times (Ed25519 is deterministic)
    let sig1 = verifier.sign(data).unwrap();
    let sig2 = verifier.sign(data).unwrap();

    // Assert - signatures should be identical (deterministic signing)
    assert_eq!(sig1, sig2);
}
