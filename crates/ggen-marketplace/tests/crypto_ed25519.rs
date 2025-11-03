// London School TDD - Tests FIRST
// These tests define the behavior we expect from Ed25519Verifier

use ggen_marketplace::crypto::Ed25519Verifier;
use ggen_marketplace::models::signature::{PublicKey, Signature, SignatureAlgorithm};
use ggen_marketplace::traits::CryptoVerifier;

#[test]
fn test_generate_keypair_produces_valid_keys() {
    // GIVEN: A fresh Ed25519 verifier
    let verifier = Ed25519Verifier::new();

    // WHEN: We generate a keypair
    let result = verifier.generate_keypair();

    // THEN: It should succeed
    assert!(result.is_ok(), "Keypair generation should succeed");

    let keypair = result.unwrap();

    // AND: Public key should be 32 bytes (Ed25519 standard)
    assert_eq!(
        keypair.public_key.key_data.len(),
        32,
        "Ed25519 public key should be 32 bytes"
    );

    // AND: Private key should be 32 bytes (Ed25519 standard)
    assert_eq!(
        keypair.private_key_bytes().len(),
        32,
        "Ed25519 private key should be 32 bytes"
    );

    // AND: Algorithm should be Ed25519
    assert_eq!(
        keypair.public_key.algorithm,
        SignatureAlgorithm::Ed25519,
        "Algorithm should be Ed25519"
    );
}

#[test]
fn test_sign_creates_valid_signature() {
    // GIVEN: A verifier with a keypair
    let verifier = Ed25519Verifier::new();
    let keypair = verifier
        .generate_keypair()
        .expect("Should generate keypair");

    // AND: Some content to sign
    let content = b"Hello, Ggen Marketplace!";

    // WHEN: We sign the content
    // Note: We need to store the keypair in the verifier first
    let verifier_with_key = Ed25519Verifier::with_keypair(keypair);
    let result = verifier_with_key.sign(content);

    // THEN: It should succeed
    assert!(result.is_ok(), "Signing should succeed");

    let signature = result.unwrap();

    // AND: Signature value should be 64 bytes (Ed25519 standard)
    assert_eq!(
        signature.value.len(),
        64,
        "Ed25519 signature should be 64 bytes"
    );

    // AND: Algorithm should be Ed25519
    assert_eq!(
        signature.algorithm,
        SignatureAlgorithm::Ed25519,
        "Signature algorithm should be Ed25519"
    );
}

#[test]
fn test_verify_valid_signature_returns_true() {
    // GIVEN: A verifier with a keypair
    let verifier = Ed25519Verifier::new();
    let keypair = verifier
        .generate_keypair()
        .expect("Should generate keypair");

    // AND: Signed content
    let content = b"Verify this content";
    let verifier_with_key = Ed25519Verifier::with_keypair(keypair);
    let signature = verifier_with_key
        .sign(content)
        .expect("Should sign content");

    // WHEN: We verify the signature
    let result = verifier_with_key.verify(content, &signature);

    // THEN: It should succeed
    assert!(result.is_ok(), "Verification should succeed");

    // AND: Should return true for valid signature
    assert!(result.unwrap(), "Valid signature should verify as true");
}

#[test]
fn test_verify_invalid_signature_returns_false() {
    // GIVEN: A verifier with a keypair
    let verifier = Ed25519Verifier::new();
    let keypair = verifier
        .generate_keypair()
        .expect("Should generate keypair");

    // AND: Signed content
    let content = b"Original content";
    let verifier_with_key = Ed25519Verifier::with_keypair(keypair.clone());
    let signature = verifier_with_key
        .sign(content)
        .expect("Should sign content");

    // WHEN: We verify with different content
    let different_content = b"Different content";
    let result = verifier_with_key.verify(different_content, &signature);

    // THEN: It should succeed (no error)
    assert!(result.is_ok(), "Verification should not error");

    // AND: Should return false for invalid signature
    assert!(!result.unwrap(), "Invalid signature should verify as false");
}

#[test]
fn test_verify_tampered_signature_fails() {
    // GIVEN: A verifier with a keypair and signed content
    let verifier = Ed25519Verifier::new();
    let keypair = verifier
        .generate_keypair()
        .expect("Should generate keypair");

    let content = b"Tamper-proof content";
    let verifier_with_key = Ed25519Verifier::with_keypair(keypair);
    let mut signature = verifier_with_key
        .sign(content)
        .expect("Should sign content");

    // WHEN: We tamper with the signature
    signature.value[0] ^= 0xFF; // Flip all bits in first byte

    // AND: Verify the tampered signature
    let result = verifier_with_key.verify(content, &signature);

    // THEN: Should return false
    assert!(result.is_ok(), "Verification should not error");
    assert!(
        !result.unwrap(),
        "Tampered signature should fail verification"
    );
}

#[test]
fn test_sign_empty_data_succeeds() {
    // GIVEN: A verifier with a keypair
    let verifier = Ed25519Verifier::new();
    let keypair = verifier
        .generate_keypair()
        .expect("Should generate keypair");

    // AND: Empty content
    let empty_content = b"";

    // WHEN: We sign empty content
    let verifier_with_key = Ed25519Verifier::with_keypair(keypair);
    let result = verifier_with_key.sign(empty_content);

    // THEN: It should succeed
    assert!(result.is_ok(), "Signing empty data should succeed");

    // AND: Signature should still be 64 bytes
    let signature = result.unwrap();
    assert_eq!(
        signature.value.len(),
        64,
        "Empty data signature should still be 64 bytes"
    );
}

#[test]
fn test_verify_empty_data_with_valid_signature() {
    // GIVEN: A verifier with a keypair and signed empty content
    let verifier = Ed25519Verifier::new();
    let keypair = verifier
        .generate_keypair()
        .expect("Should generate keypair");

    let empty_content = b"";
    let verifier_with_key = Ed25519Verifier::with_keypair(keypair);
    let signature = verifier_with_key
        .sign(empty_content)
        .expect("Should sign empty content");

    // WHEN: We verify the empty content signature
    let result = verifier_with_key.verify(empty_content, &signature);

    // THEN: It should verify successfully
    assert!(result.is_ok(), "Empty content verification should succeed");
    assert!(
        result.unwrap(),
        "Empty content with valid signature should verify"
    );
}

#[test]
fn test_different_keypairs_produce_different_signatures() {
    // GIVEN: Two different keypairs
    let verifier = Ed25519Verifier::new();
    let keypair1 = verifier
        .generate_keypair()
        .expect("Should generate first keypair");
    let keypair2 = verifier
        .generate_keypair()
        .expect("Should generate second keypair");

    // AND: Same content
    let content = b"Same content";

    // WHEN: We sign with both keypairs
    let verifier1 = Ed25519Verifier::with_keypair(keypair1);
    let verifier2 = Ed25519Verifier::with_keypair(keypair2);

    let sig1 = verifier1.sign(content).expect("Should sign with key 1");
    let sig2 = verifier2.sign(content).expect("Should sign with key 2");

    // THEN: Signatures should be different
    assert_ne!(
        sig1.value, sig2.value,
        "Different keys should produce different signatures"
    );
}

#[test]
fn test_signature_verification_with_wrong_public_key_fails() {
    // GIVEN: Two different keypairs
    let verifier = Ed25519Verifier::new();
    let keypair1 = verifier
        .generate_keypair()
        .expect("Should generate first keypair");
    let keypair2 = verifier
        .generate_keypair()
        .expect("Should generate second keypair");

    // AND: Content signed with keypair1
    let content = b"Authenticated content";
    let verifier1 = Ed25519Verifier::with_keypair(keypair1);
    let mut signature = verifier1.sign(content).expect("Should sign with key 1");

    // WHEN: We replace the public key with keypair2's public key
    signature.public_key = keypair2.public_key;

    // AND: Try to verify with the wrong public key
    let verifier2 = Ed25519Verifier::with_keypair(keypair2);
    let result = verifier2.verify(content, &signature);

    // THEN: Verification should fail
    assert!(result.is_ok(), "Verification should not error");
    assert!(
        !result.unwrap(),
        "Signature with wrong public key should fail verification"
    );
}

#[test]
fn test_export_and_import_public_key() {
    // GIVEN: A verifier with a keypair
    let verifier = Ed25519Verifier::new();
    let keypair = verifier
        .generate_keypair()
        .expect("Should generate keypair");

    let original_public_key = &keypair.public_key;

    // WHEN: We export the public key to PEM
    let pem = verifier
        .export_public_key(original_public_key)
        .expect("Should export public key");

    // THEN: PEM should contain standard markers
    assert!(pem.contains("-----BEGIN PUBLIC KEY-----"));
    assert!(pem.contains("-----END PUBLIC KEY-----"));

    // WHEN: We import the PEM back
    let imported_key = verifier
        .import_public_key(&pem)
        .expect("Should import public key");

    // THEN: Keys should match
    assert_eq!(
        imported_key.key_data, original_public_key.key_data,
        "Imported key should match original"
    );
    assert_eq!(
        imported_key.algorithm, original_public_key.algorithm,
        "Algorithm should be preserved"
    );
}

#[test]
fn test_deterministic_signatures() {
    // GIVEN: A verifier with a keypair
    let verifier = Ed25519Verifier::new();
    let keypair = verifier
        .generate_keypair()
        .expect("Should generate keypair");

    // AND: Same content
    let content = b"Deterministic test";

    // WHEN: We sign the same content twice
    let verifier_with_key = Ed25519Verifier::with_keypair(keypair);
    let sig1 = verifier_with_key
        .sign(content)
        .expect("Should sign first time");
    let sig2 = verifier_with_key
        .sign(content)
        .expect("Should sign second time");

    // THEN: Signatures should be identical (Ed25519 is deterministic)
    assert_eq!(
        sig1.value, sig2.value,
        "Ed25519 should produce deterministic signatures"
    );
}

#[test]
fn test_large_content_signing() {
    // GIVEN: A verifier with a keypair
    let verifier = Ed25519Verifier::new();
    let keypair = verifier
        .generate_keypair()
        .expect("Should generate keypair");

    // AND: Large content (1 MB)
    let large_content = vec![0x42u8; 1024 * 1024];

    // WHEN: We sign and verify large content
    let verifier_with_key = Ed25519Verifier::with_keypair(keypair);
    let signature = verifier_with_key
        .sign(&large_content)
        .expect("Should sign large content");

    let verified = verifier_with_key
        .verify(&large_content, &signature)
        .expect("Should verify large content");

    // THEN: Verification should succeed
    assert!(verified, "Large content should verify successfully");
}
