//! Comprehensive security tests for ggen-marketplace cryptographic operations.
//!
//! Chicago TDD: Real cryptographic operations, no mocks.
//! Tests cover:
//! - MarketplaceSignature signing
//! - MarketplaceVerifier verification
//! - Invalid signature rejection
//! - Tampered data detection
//! - Public key loading and validation
//! - Checksum calculation and verification

use ggen_marketplace::security::{
    generate_marketplace_keypair, ChecksumCalculator, MarketplaceSignature, MarketplaceVerifier,
};
use ggen_marketplace::traits::Signable;
use std::fs;
use tempfile::TempDir;

#[test]
fn test_marketplace_signature_creation() {
    let (signing_key, _) = generate_marketplace_keypair();

    let data = b"test pack data for signing";
    let signature = MarketplaceSignature::sign(&signing_key, data).expect("signing failed");

    // Signature should be non-empty
    assert!(
        !signature.signature.is_empty(),
        "Signature must not be empty"
    );

    // Ed25519 signature is 64 bytes = 128 hex chars
    assert_eq!(
        signature.signature.len(),
        128,
        "Ed25519 signature must be 128 hex chars"
    );

    // Checksum should be SHA-256 (64 hex chars)
    assert_eq!(
        signature.checksum.len(),
        64,
        "SHA-256 checksum must be 64 hex chars"
    );

    // Public key should be 32 bytes = 64 hex chars
    assert_eq!(
        signature.public_key.len(),
        64,
        "Ed25519 public key must be 64 hex chars"
    );
}

#[test]
fn test_marketplace_signature_checksum_matches_ggen_receipt() {
    let (signing_key, _) = generate_marketplace_keypair();

    let data = b"test pack data";
    let signature = MarketplaceSignature::sign(&signing_key, data).expect("signing failed");

    // Checksum should match ggen_receipt::hash_data
    let expected_checksum = ggen_receipt::hash_data(data);
    assert_eq!(
        signature.checksum(),
        expected_checksum,
        "Checksum must match ggen_receipt"
    );
}

#[test]
fn test_marketplace_verifier_validates_correct_signature() {
    let (signing_key, verifying_key) = generate_marketplace_keypair();

    let data = b"test pack data";
    let signature = MarketplaceSignature::sign(&signing_key, data).expect("signing failed");

    let verifier = MarketplaceVerifier::new(verifying_key);
    let verified = verifier
        .verify(data, &signature)
        .expect("verification failed");

    assert!(verified, "Valid signature must verify");
}

#[test]
fn test_marketplace_verifier_rejects_wrong_signature() {
    let (signing_key, verifying_key) = generate_marketplace_keypair();

    let data = b"test pack data";
    let signature = MarketplaceSignature::sign(&signing_key, data).expect("signing failed");

    // Tamper with the data
    let wrong_data = b"wrong pack data";

    let verifier = MarketplaceVerifier::new(verifying_key);
    let verified = verifier
        .verify(wrong_data, &signature)
        .expect("verification failed");

    assert!(!verified, "Wrong data must not verify");
}

#[test]
fn test_marketplace_verifier_rejects_tampered_signature() {
    let (signing_key, verifying_key) = generate_marketplace_keypair();

    let data = b"test pack data";
    let mut signature = MarketplaceSignature::sign(&signing_key, data).expect("signing failed");

    // Tamper with signature (flip one character)
    let mut sig_bytes = signature.signature.clone();
    unsafe {
        let bytes = sig_bytes.as_bytes_mut();
        bytes[0] = if bytes[0] == b'0' { b'1' } else { b'0' };
    }
    signature.signature = sig_bytes;

    let verifier = MarketplaceVerifier::new(verifying_key);
    let verified = verifier
        .verify(data, &signature)
        .expect("verification failed");

    assert!(!verified, "Tampered signature must not verify");
}

#[test]
fn test_marketplace_verifier_from_public_key_hex() {
    let (signing_key, verifying_key) = generate_marketplace_keypair();
    let public_key_hex = hex::encode(verifying_key.to_bytes());

    let verifier = MarketplaceVerifier::from_public_key_hex(&public_key_hex)
        .expect("failed to create verifier");

    // Public key should round-trip correctly
    assert_eq!(verifier.public_key_hex(), public_key_hex);

    // Verifier should work correctly
    let data = b"test data";
    let signature = MarketplaceSignature::sign(&signing_key, data).expect("signing failed");

    let verified = verifier
        .verify(data, &signature)
        .expect("verification failed");
    assert!(verified);
}

#[test]
fn test_marketplace_verifier_rejects_invalid_public_key_hex() {
    // Invalid hex
    let result = MarketplaceVerifier::from_public_key_hex("not-valid-hex!!");
    assert!(result.is_err(), "Invalid hex should be rejected");

    // Wrong length (not 32 bytes)
    let short_hex = "abcd1234";
    let result = MarketplaceVerifier::from_public_key_hex(short_hex);
    assert!(
        result.is_err(),
        "Wrong length public key should be rejected"
    );
}

#[test]
fn test_marketplace_verifier_rejects_invalid_public_key_bytes() {
    // 32 bytes of zeros (not a valid Ed25519 key)
    let invalid_key = "0".repeat(64);

    let result = MarketplaceVerifier::from_public_key_hex(&invalid_key);
    assert!(result.is_err(), "Invalid Ed25519 key should be rejected");
}

#[test]
fn test_checksum_calculator_sha256() {
    let data = b"test pack data for checksum";

    let checksum = ChecksumCalculator::calculate(data);

    // SHA-256 produces 64 hex characters
    assert_eq!(checksum.len(), 64, "SHA-256 must be 64 hex chars");

    // Should be deterministic
    let checksum2 = ChecksumCalculator::calculate(data);
    assert_eq!(checksum, checksum2, "Checksum must be deterministic");
}

#[test]
fn test_checksum_calculator_avalanche_effect() {
    let data1 = b"test pack data";
    let data2 = b"test pack datf"; // One character difference

    let checksum1 = ChecksumCalculator::calculate(data1);
    let checksum2 = ChecksumCalculator::calculate(data2);

    // Checksums should be completely different
    assert_ne!(
        checksum1, checksum2,
        "Different data must produce different checksums"
    );

    // Check avalanche effect (at least 100 bits different out of 256)
    let bytes1 = hex::decode(&checksum1).expect("Invalid hex");
    let bytes2 = hex::decode(&checksum2).expect("Invalid hex");

    let mut differing_bits = 0;
    for (b1, b2) in bytes1.iter().zip(bytes2.iter()) {
        differing_bits += (b1 ^ b2).count_ones();
    }

    assert!(
        differing_bits >= 100,
        "SHA-256 must have good avalanche effect"
    );
}

#[test]
fn test_checksum_verification_valid() {
    let data = b"test pack data";
    let checksum = ChecksumCalculator::calculate(data);

    let verified = ChecksumCalculator::verify(data, &checksum).expect("verification failed");

    assert!(verified, "Valid checksum must verify");
}

#[test]
fn test_checksum_verification_invalid() {
    let data = b"test pack data";
    let wrong_checksum = "0".repeat(64);

    let verified = ChecksumCalculator::verify(data, &wrong_checksum).expect("verification failed");

    assert!(!verified, "Wrong checksum must not verify");
}

#[test]
fn test_checksum_verification_empty_input() {
    let data = b"";
    let checksum = ChecksumCalculator::calculate(data);

    let verified = ChecksumCalculator::verify(data, &checksum).expect("verification failed");

    assert!(verified, "Empty input checksum must verify");

    // Known SHA-256 hash of empty input
    assert_eq!(
        checksum,
        "e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855"
    );
}

#[test]
fn test_signature_persistence_to_file() {
    let temp_dir = TempDir::new().expect("Failed to create temp dir");
    let sig_path = temp_dir.path().join("signature.json");

    let (signing_key, _) = generate_marketplace_keypair();
    let data = b"test data";

    let signature = MarketplaceSignature::sign(&signing_key, data).expect("signing failed");

    // Serialize and write to file
    let json = serde_json::to_string_pretty(&signature).expect("serialization failed");
    fs::write(&sig_path, json).expect("write failed");

    // Read from file
    let read_json = fs::read_to_string(&sig_path).expect("read failed");
    let loaded: MarketplaceSignature =
        serde_json::from_str(&read_json).expect("deserialization failed");

    // Signature should be preserved
    assert_eq!(signature.signature, loaded.signature);
    assert_eq!(signature.checksum, loaded.checksum);
    assert_eq!(signature.public_key, loaded.public_key);
}

#[test]
fn test_verifier_public_key_hex_roundtrip() {
    let (_, verifying_key) = generate_marketplace_keypair();

    let verifier = MarketplaceVerifier::new(verifying_key);
    let pub_key_hex = verifier.public_key_hex();

    // Create new verifier from hex
    let verifier2 =
        MarketplaceVerifier::from_public_key_hex(&pub_key_hex).expect("failed to create verifier");

    // Public keys should match
    assert_eq!(verifier.public_key_hex(), verifier2.public_key_hex());
}

#[test]
fn test_multiple_signatures_different_data() {
    let (signing_key, verifying_key) = generate_marketplace_keypair();

    let data1 = b"first pack data";
    let data2 = b"second pack data";

    let sig1 = MarketplaceSignature::sign(&signing_key, data1).expect("signing failed");
    let sig2 = MarketplaceSignature::sign(&signing_key, data2).expect("signing failed");

    // Signatures must be different
    assert_ne!(
        sig1.signature, sig2.signature,
        "Different data must produce different signatures"
    );

    // Checksums must be different
    assert_ne!(
        sig1.checksum, sig2.checksum,
        "Different data must produce different checksums"
    );

    // Both should verify
    let verifier = MarketplaceVerifier::new(verifying_key);
    assert!(verifier.verify(data1, &sig1).expect("verification failed"));
    assert!(verifier.verify(data2, &sig2).expect("verification failed"));
}

#[test]
fn test_signature_includes_checksum() {
    let (signing_key, _) = generate_marketplace_keypair();

    let data = b"test pack data";
    let signature = MarketplaceSignature::sign(&signing_key, data).expect("signing failed");

    // Checksum should match direct calculation
    let expected_checksum = ggen_receipt::hash_data(data);
    assert_eq!(signature.checksum, expected_checksum);

    // Accessor methods should work
    assert_eq!(signature.as_hex(), signature.signature);
    assert_eq!(signature.checksum(), expected_checksum);
    assert_eq!(signature.public_key(), signature.public_key);
}

#[test]
fn test_large_data_signature() {
    let (signing_key, verifying_key) = generate_marketplace_keypair();

    // Create large data (1 MB)
    let large_data = vec![0u8; 1024 * 1024];

    let signature = MarketplaceSignature::sign(&signing_key, &large_data).expect("signing failed");

    // Should verify correctly
    let verifier = MarketplaceVerifier::new(verifying_key);
    let verified = verifier
        .verify(&large_data, &signature)
        .expect("verification failed");

    assert!(verified, "Large data signature must verify");
}

#[test]
fn test_empty_data_signature() {
    let (signing_key, verifying_key) = generate_marketplace_keypair();

    let empty_data = b"";

    let signature = MarketplaceSignature::sign(&signing_key, empty_data).expect("signing failed");

    // Should verify correctly
    let verifier = MarketplaceVerifier::new(verifying_key);
    let verified = verifier
        .verify(empty_data, &signature)
        .expect("verification failed");

    assert!(verified, "Empty data signature must verify");
}

#[test]
fn test_verifier_signable_trait_cannot_sign() {
    let (_, verifying_key) = generate_marketplace_keypair();

    let verifier = MarketplaceVerifier::new(verifying_key);

    // Verifier should not be able to sign (via Signable trait)
    let result = Signable::sign(&verifier, b"test data");
    assert!(result.is_err(), "Verifier should not be able to sign");

    let err = result.unwrap_err();
    assert!(
        err.to_string().contains("cannot sign"),
        "Error should indicate cannot sign"
    );
}

#[test]
fn test_verifier_signable_trait_verify() {
    let (signing_key, verifying_key) = generate_marketplace_keypair();

    let data = b"test data";
    let signature = MarketplaceSignature::sign(&signing_key, data).expect("signing failed");

    // Verify using Signable trait (different signature than inherent verify)
    let verifier = MarketplaceVerifier::new(verifying_key);
    let verified =
        Signable::verify(&verifier, data, signature.as_hex()).expect("verification failed");

    assert!(verified, "Signable trait verify should work");
}

#[test]
fn test_verifier_signable_trait_public_key() {
    let (_, verifying_key) = generate_marketplace_keypair();

    let verifier = MarketplaceVerifier::new(verifying_key);

    // Get public key via Signable trait
    let pub_key = Signable::public_key(&verifier);

    assert_eq!(pub_key, verifier.public_key_hex());
    assert_eq!(pub_key.len(), 64, "Public key must be 64 hex chars");
}
