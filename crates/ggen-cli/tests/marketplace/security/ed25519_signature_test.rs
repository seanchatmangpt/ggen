//! Security tests for Ed25519 signature generation and verification
//!
//! Validates package signing, signature verification, tamper detection,
//! and key rotation handling.

#[cfg(test)]
#[cfg(feature = "marketplace-v2")]
mod ed25519_security_tests {
    use ggen_marketplace_v2::security::SignatureManager;

    #[test]
    fn test_keypair_generation() {
        let manager = SignatureManager::new();
        let (public_key, private_key) = manager.generate_keypair();

        // Ed25519 keys should be 32 bytes
        assert_eq!(public_key.len(), 32, "Public key should be 32 bytes");
        assert_eq!(private_key.len(), 64, "Private key should be 64 bytes");
    }

    #[test]
    fn test_signature_generation() {
        let manager = SignatureManager::new();
        let (_public_key, private_key) = manager.generate_keypair();

        let message = b"Package: test-package v1.0.0";
        let signature = manager.sign(message, &private_key);

        // Ed25519 signature should be 64 bytes
        assert_eq!(signature.len(), 64, "Signature should be 64 bytes");
    }

    #[test]
    fn test_valid_signature_verification() {
        let manager = SignatureManager::new();
        let (public_key, private_key) = manager.generate_keypair();

        let message = b"Valid package data";
        let signature = manager.sign(message, &private_key);

        // Verify signature
        let is_valid = manager.verify(message, &signature, &public_key);
        assert!(is_valid, "Valid signature should verify successfully");
    }

    #[test]
    fn test_invalid_signature_detection() {
        let manager = SignatureManager::new();
        let (public_key, private_key) = manager.generate_keypair();

        let message = b"Original message";
        let signature = manager.sign(message, &private_key);

        // Create invalid signature (flipped bits)
        let mut invalid_signature = signature.clone();
        invalid_signature[0] ^= 0xFF;

        // Verify should fail
        let is_valid = manager.verify(message, &invalid_signature, &public_key);
        assert!(!is_valid, "Invalid signature should fail verification");
    }

    #[test]
    fn test_tampered_message_detection() {
        let manager = SignatureManager::new();
        let (public_key, private_key) = manager.generate_keypair();

        let original_message = b"Package: test-package v1.0.0";
        let signature = manager.sign(original_message, &private_key);

        let tampered_message = b"Package: test-package v2.0.0"; // Version changed

        // Verify should fail with tampered message
        let is_valid = manager.verify(tampered_message, &signature, &public_key);
        assert!(!is_valid, "Tampered message should fail verification");
    }

    #[test]
    fn test_wrong_public_key_detection() {
        let manager = SignatureManager::new();
        let (_public_key1, private_key1) = manager.generate_keypair();
        let (public_key2, _private_key2) = manager.generate_keypair();

        let message = b"Test message";
        let signature = manager.sign(message, &private_key1);

        // Verify with wrong public key should fail
        let is_valid = manager.verify(message, &signature, &public_key2);
        assert!(!is_valid, "Wrong public key should fail verification");
    }

    #[test]
    fn test_signature_determinism() {
        let manager = SignatureManager::new();
        let (_public_key, private_key) = manager.generate_keypair();

        let message = b"Deterministic test";

        let signature1 = manager.sign(message, &private_key);
        let signature2 = manager.sign(message, &private_key);

        // Same message + same key should produce same signature
        assert_eq!(signature1, signature2, "Signatures should be deterministic");
    }

    #[test]
    fn test_different_messages_different_signatures() {
        let manager = SignatureManager::new();
        let (_public_key, private_key) = manager.generate_keypair();

        let message1 = b"Message 1";
        let message2 = b"Message 2";

        let signature1 = manager.sign(message1, &private_key);
        let signature2 = manager.sign(message2, &private_key);

        // Different messages should produce different signatures
        assert_ne!(
            signature1, signature2,
            "Different messages should have different signatures"
        );
    }

    #[test]
    fn test_empty_message_signing() {
        let manager = SignatureManager::new();
        let (public_key, private_key) = manager.generate_keypair();

        let empty_message = b"";
        let signature = manager.sign(empty_message, &private_key);

        // Should be able to sign and verify empty message
        let is_valid = manager.verify(empty_message, &signature, &public_key);
        assert!(is_valid, "Empty message signature should verify");
    }

    #[test]
    fn test_large_message_signing() {
        let manager = SignatureManager::new();
        let (public_key, private_key) = manager.generate_keypair();

        // 1MB message
        let large_message = vec![0u8; 1024 * 1024];
        let signature = manager.sign(&large_message, &private_key);

        // Should handle large messages
        let is_valid = manager.verify(&large_message, &signature, &public_key);
        assert!(is_valid, "Large message signature should verify");
    }

    #[test]
    fn test_unicode_message_signing() {
        let manager = SignatureManager::new();
        let (public_key, private_key) = manager.generate_keypair();

        let unicode_message = "Package: 测试包 🚀 v1.0.0".as_bytes();
        let signature = manager.sign(unicode_message, &private_key);

        // Should handle unicode correctly
        let is_valid = manager.verify(unicode_message, &signature, &public_key);
        assert!(is_valid, "Unicode message signature should verify");
    }

    #[test]
    fn test_multiple_signatures_same_key() {
        let manager = SignatureManager::new();
        let (public_key, private_key) = manager.generate_keypair();

        // Sign 10 different messages with same key
        for i in 0..10 {
            let message = format!("Message {}", i);
            let signature = manager.sign(message.as_bytes(), &private_key);

            let is_valid = manager.verify(message.as_bytes(), &signature, &public_key);
            assert!(is_valid, "Message {} signature should verify", i);
        }
    }

    #[test]
    fn test_key_rotation_scenario() {
        let manager = SignatureManager::new();

        // Old keypair
        let (old_public_key, old_private_key) = manager.generate_keypair();

        // New keypair (rotated)
        let (new_public_key, new_private_key) = manager.generate_keypair();

        let message = b"Package after key rotation";

        // Sign with old key
        let old_signature = manager.sign(message, &old_private_key);

        // Sign with new key
        let new_signature = manager.sign(message, &new_private_key);

        // Old signature verifies with old public key
        assert!(manager.verify(message, &old_signature, &old_public_key));

        // New signature verifies with new public key
        assert!(manager.verify(message, &new_signature, &new_public_key));

        // Old signature should NOT verify with new public key
        assert!(!manager.verify(message, &old_signature, &new_public_key));

        // New signature should NOT verify with old public key
        assert!(!manager.verify(message, &new_signature, &old_public_key));
    }

    #[test]
    fn test_concurrent_signature_operations() {
        use std::thread;

        let manager = SignatureManager::new();
        let (public_key, private_key) = manager.generate_keypair();

        // Clone keys for thread safety (in real code, use Arc<Mutex<>>)
        let public_key_clone = public_key.clone();
        let private_key_clone = private_key.clone();

        let handles: Vec<_> = (0..10)
            .map(|i| {
                let pk = private_key_clone.clone();
                let pub_k = public_key_clone.clone();
                let mgr = SignatureManager::new();

                thread::spawn(move || {
                    let message = format!("Concurrent message {}", i);
                    let signature = mgr.sign(message.as_bytes(), &pk);
                    mgr.verify(message.as_bytes(), &signature, &pub_k)
                })
            })
            .collect();

        // All signatures should verify
        for handle in handles {
            let is_valid = handle.join().unwrap();
            assert!(is_valid, "Concurrent signature should verify");
        }
    }

    #[test]
    fn test_signature_performance() {
        use std::time::Instant;

        let manager = SignatureManager::new();
        let (_public_key, private_key) = manager.generate_keypair();

        let message = b"Performance test message";

        let start = Instant::now();

        // 1000 signature operations
        for _ in 0..1000 {
            let _ = manager.sign(message, &private_key);
        }

        let duration = start.elapsed();

        println!("1000 signatures in {:?}", duration);

        // Should complete in <1s
        assert!(
            duration.as_secs() < 1,
            "Signature generation too slow: {:?}",
            duration
        );
    }

    #[test]
    fn test_verification_performance() {
        use std::time::Instant;

        let manager = SignatureManager::new();
        let (public_key, private_key) = manager.generate_keypair();

        let message = b"Performance test message";
        let signature = manager.sign(message, &private_key);

        let start = Instant::now();

        // 1000 verification operations
        for _ in 0..1000 {
            let _ = manager.verify(message, &signature, &public_key);
        }

        let duration = start.elapsed();

        println!("1000 verifications in {:?}", duration);

        // Should complete in <1s
        assert!(
            duration.as_secs() < 1,
            "Signature verification too slow: {:?}",
            duration
        );
    }

    #[test]
    fn test_signature_serialization() {
        let manager = SignatureManager::new();
        let (public_key, private_key) = manager.generate_keypair();

        let message = b"Serialization test";
        let signature = manager.sign(message, &private_key);

        // Serialize signature (hex encoding)
        let signature_hex = hex::encode(&signature);

        // Deserialize signature
        let signature_decoded = hex::decode(&signature_hex).unwrap();

        // Should verify with deserialized signature
        let is_valid = manager.verify(message, &signature_decoded, &public_key);
        assert!(is_valid, "Deserialized signature should verify");
    }

    #[test]
    fn test_public_key_serialization() {
        let manager = SignatureManager::new();
        let (public_key, private_key) = manager.generate_keypair();

        let message = b"Key serialization test";
        let signature = manager.sign(message, &private_key);

        // Serialize public key
        let public_key_hex = hex::encode(&public_key);

        // Deserialize public key
        let public_key_decoded = hex::decode(&public_key_hex).unwrap();

        // Should verify with deserialized public key
        let is_valid = manager.verify(message, &signature, &public_key_decoded);
        assert!(
            is_valid,
            "Signature should verify with deserialized public key"
        );
    }

    #[test]
    fn test_supply_chain_attack_prevention() {
        let manager = SignatureManager::new();
        let (legitimate_public_key, legitimate_private_key) = manager.generate_keypair();
        let (_attacker_public_key, attacker_private_key) = manager.generate_keypair();

        let legitimate_message = b"Package: legitimate-package v1.0.0";
        let legitimate_signature = manager.sign(legitimate_message, &legitimate_private_key);

        let malicious_message = b"Package: legitimate-package v1.0.0 + backdoor";

        // Attacker tries to sign malicious package with their key
        let malicious_signature = manager.sign(malicious_message, &attacker_private_key);

        // Legitimate signature should NOT verify malicious message
        assert!(!manager.verify(
            malicious_message,
            &legitimate_signature,
            &legitimate_public_key
        ));

        // Malicious signature should NOT verify with legitimate public key
        assert!(!manager.verify(
            malicious_message,
            &malicious_signature,
            &legitimate_public_key
        ));
    }
}
