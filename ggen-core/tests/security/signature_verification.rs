//! Security tests for signature verification

use ggen_core::pqc::{PqcSigner, PqcVerifier, calculate_sha256};
use anyhow::Result;

#[test]
fn test_valid_signature_verification() -> Result<()> {
    let signer = PqcSigner::new()?;
    let message = b"test message for signing";

    let signature = signer.sign(message)?;

    // Should verify successfully
    let verifier = PqcVerifier::from_public_key(&signer.public_key())?;
    let is_valid = verifier.verify(message, &signature)?;

    assert!(is_valid);
    Ok(())
}

#[test]
fn test_tampered_message_verification_fails() -> Result<()> {
    let signer = PqcSigner::new()?;
    let message = b"original message";

    let signature = signer.sign(message)?;

    // Tamper with the message
    let tampered_message = b"tampered message";

    let verifier = PqcVerifier::from_public_key(&signer.public_key())?;
    let is_valid = verifier.verify(tampered_message, &signature)?;

    // Verification should fail
    assert!(!is_valid);
    Ok(())
}

#[test]
fn test_tampered_signature_verification_fails() -> Result<()> {
    let signer = PqcSigner::new()?;
    let message = b"test message";

    let mut signature = signer.sign(message)?;

    // Tamper with the signature
    if !signature.is_empty() {
        signature[0] ^= 0xFF;
    }

    let verifier = PqcVerifier::from_public_key(&signer.public_key())?;
    let is_valid = verifier.verify(message, &signature)?;

    // Verification should fail
    assert!(!is_valid);
    Ok(())
}

#[test]
fn test_wrong_public_key_verification_fails() -> Result<()> {
    let signer1 = PqcSigner::new()?;
    let signer2 = PqcSigner::new()?;

    let message = b"test message";
    let signature = signer1.sign(message)?;

    // Try to verify with wrong public key
    let verifier = PqcVerifier::from_public_key(&signer2.public_key())?;
    let is_valid = verifier.verify(message, &signature)?;

    // Verification should fail
    assert!(!is_valid);
    Ok(())
}

#[test]
fn test_empty_message_signature() -> Result<()> {
    let signer = PqcSigner::new()?;
    let message = b"";

    let signature = signer.sign(message)?;

    let verifier = PqcVerifier::from_public_key(&signer.public_key())?;
    let is_valid = verifier.verify(message, &signature)?;

    assert!(is_valid);
    Ok(())
}

#[test]
fn test_large_message_signature() -> Result<()> {
    let signer = PqcSigner::new()?;
    let message = vec![0u8; 1024 * 1024]; // 1MB message

    let signature = signer.sign(&message)?;

    let verifier = PqcVerifier::from_public_key(&signer.public_key())?;
    let is_valid = verifier.verify(&message, &signature)?;

    assert!(is_valid);
    Ok(())
}

#[test]
fn test_sha256_hash_consistency() {
    let data = b"test data for hashing";

    let hash1 = calculate_sha256(data);
    let hash2 = calculate_sha256(data);

    // Hash should be deterministic
    assert_eq!(hash1, hash2);
}

#[test]
fn test_sha256_different_inputs() {
    let data1 = b"input 1";
    let data2 = b"input 2";

    let hash1 = calculate_sha256(data1);
    let hash2 = calculate_sha256(data2);

    // Different inputs should produce different hashes
    assert_ne!(hash1, hash2);
}

#[test]
fn test_sha256_hash_length() {
    let data = b"test data";

    let hash = calculate_sha256(data);

    // SHA256 produces 32-byte (64 hex character) hash
    assert_eq!(hash.len(), 64);
}

#[test]
fn test_sha256_empty_input() {
    let data = b"";

    let hash = calculate_sha256(data);

    // Even empty input should produce valid hash
    assert_eq!(hash.len(), 64);
    // Known SHA256 of empty string
    assert_eq!(hash, "e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855");
}

#[test]
fn test_multiple_signers_independence() -> Result<()> {
    let signer1 = PqcSigner::new()?;
    let signer2 = PqcSigner::new()?;

    let message = b"test message";

    let signature1 = signer1.sign(message)?;
    let signature2 = signer2.sign(message)?;

    // Different signers should produce different signatures
    assert_ne!(signature1, signature2);

    // Each signature should verify with its own key
    let verifier1 = PqcVerifier::from_public_key(&signer1.public_key())?;
    let verifier2 = PqcVerifier::from_public_key(&signer2.public_key())?;

    assert!(verifier1.verify(message, &signature1)?);
    assert!(verifier2.verify(message, &signature2)?);

    // Cross-verification should fail
    assert!(!verifier1.verify(message, &signature2)?);
    assert!(!verifier2.verify(message, &signature1)?);

    Ok(())
}

#[test]
fn test_signature_replay_protection() -> Result<()> {
    let signer = PqcSigner::new()?;
    let message = b"test message";

    let signature = signer.sign(message)?;

    let verifier = PqcVerifier::from_public_key(&signer.public_key())?;

    // Signature should be valid multiple times (no replay protection at signature level)
    assert!(verifier.verify(message, &signature)?);
    assert!(verifier.verify(message, &signature)?);

    // Note: Replay protection should be implemented at protocol level,
    // not in signature verification
    Ok(())
}
