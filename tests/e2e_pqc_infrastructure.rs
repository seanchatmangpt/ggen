use anyhow::Result;
use ggen_core::pqc::{calculate_sha256, PqcSigner, PqcVerifier};

/// E2E tests for PQC infrastructure
///
/// Tests the post-quantum cryptography signing and verification
/// capabilities implemented in v1.0.0.

#[test]
#[ignore]
fn test_pqc_signer_creates_valid_signatures() -> Result<()> {
    // Create a new signer (generates keypair)
    let signer = PqcSigner::new();

    // Sign some test data
    let pack_id = "io.ggen.test.pack";
    let version = "1.0.0";
    let sha256 = "abcdef1234567890";

    let signature = signer.sign_pack(pack_id, version, sha256);

    // Signature should be base64-encoded and non-empty
    assert!(!signature.is_empty());
    assert!(signature.len() > 100); // ML-DSA signatures are large (~4KB base64)

    // Get public key
    let pubkey = signer.public_key_base64();
    assert!(!pubkey.is_empty());
    assert!(pubkey.len() > 100); // Public keys are also large

    // Create verifier from public key
    let verifier = PqcVerifier::from_base64(&pubkey)?;

    // Verify the signature
    let is_valid = verifier.verify_pack(pack_id, version, sha256, &signature)?;
    assert!(is_valid, "Valid signature should verify successfully");

    Ok(())
}

#[test]
#[ignore]
fn test_pqc_signature_base64_encoding() -> Result<()> {
    // Test that base64 encoding/decoding round-trip works
    let signer = PqcSigner::new();
    let pubkey = signer.public_key_base64();

    // Should be able to create verifier from base64 public key
    let verifier = PqcVerifier::from_base64(&pubkey)?;

    // Sign and verify to confirm round-trip works
    let signature = signer.sign_pack("test", "1.0.0", "hash123");
    let is_valid = verifier.verify_pack("test", "1.0.0", "hash123", &signature)?;
    assert!(is_valid);

    Ok(())
}

#[test]
#[ignore]
fn test_pqc_verify_detects_tampering() -> Result<()> {
    // Create signer and sign data
    let signer = PqcSigner::new();
    let pack_id = "io.ggen.test.pack";
    let version = "1.0.0";
    let sha256 = "original_hash";

    let signature = signer.sign_pack(pack_id, version, sha256);
    let pubkey = signer.public_key_base64();
    let verifier = PqcVerifier::from_base64(&pubkey)?;

    // Valid signature should verify
    let is_valid = verifier.verify_pack(pack_id, version, sha256, &signature)?;
    assert!(is_valid, "Original signature should be valid");

    // Tampered data should fail verification
    let is_valid_tampered = verifier.verify_pack(pack_id, version, "tampered_hash", &signature)?;
    assert!(
        !is_valid_tampered,
        "Signature with tampered data should be invalid"
    );

    // Different pack ID should fail
    let is_valid_different = verifier.verify_pack("different.pack", version, sha256, &signature)?;
    assert!(
        !is_valid_different,
        "Signature with different pack ID should be invalid"
    );

    // Different version should fail
    let is_valid_version = verifier.verify_pack(pack_id, "2.0.0", sha256, &signature)?;
    assert!(
        !is_valid_version,
        "Signature with different version should be invalid"
    );

    Ok(())
}

#[test]
#[ignore]
fn test_pqc_signature_serialization() -> Result<()> {
    // Test that PQC signatures can be serialized and deserialized
    let signer = PqcSigner::new();
    let signature = signer.sign_pack("io.ggen.test.pack", "1.0.0", "abc123");
    let pubkey = signer.public_key_base64();

    // Verify signature and pubkey are non-empty and properly sized
    assert!(!signature.is_empty());
    assert!(signature.len() > 100); // ML-DSA signatures are large (~4KB base64)
    assert!(!pubkey.is_empty());
    assert!(pubkey.len() > 100); // Public keys are also large

    // Verify verifier can be recreated from serialized pubkey
    let verifier = PqcVerifier::from_base64(&pubkey)?;
    let is_valid = verifier.verify_pack("io.ggen.test.pack", "1.0.0", "abc123", &signature)?;
    assert!(is_valid);

    Ok(())
}

#[test]
#[ignore]
fn test_sha256_calculation_utility() -> Result<()> {
    // Test the calculate_sha256 helper function
    let test_data = b"Hello, World!";
    let hash = calculate_sha256(test_data);

    // SHA256 should be 64 hex characters
    assert_eq!(hash.len(), 64);

    // Should be deterministic
    let hash2 = calculate_sha256(test_data);
    assert_eq!(hash, hash2);

    // Different data should produce different hash
    let different_data = b"Different data";
    let different_hash = calculate_sha256(different_data);
    assert_ne!(hash, different_hash);

    // Known test vector (echo -n "Hello, World!" | sha256sum)
    assert_eq!(
        hash,
        "dffd6021bb2bd5b0af676290809ec3a53191dd81c7f70a4b28688a362182986f"
    );

    Ok(())
}

#[test]
#[ignore]
fn test_pqc_different_signers_produce_different_keys() -> Result<()> {
    // Each signer should generate unique keypairs
    let signer1 = PqcSigner::new();
    let signer2 = PqcSigner::new();

    let pubkey1 = signer1.public_key_base64();
    let pubkey2 = signer2.public_key_base64();

    // Public keys should be different
    assert_ne!(pubkey1, pubkey2);

    // Signatures from different signers should be different
    let sig1 = signer1.sign_pack("test", "1.0.0", "hash");
    let sig2 = signer2.sign_pack("test", "1.0.0", "hash");
    assert_ne!(sig1, sig2);

    // Verifier 1 should not verify signature from signer 2
    let verifier1 = PqcVerifier::from_base64(&pubkey1)?;
    let is_valid = verifier1.verify_pack("test", "1.0.0", "hash", &sig2)?;
    assert!(
        !is_valid,
        "Verifier should reject signature from different key"
    );

    Ok(())
}
