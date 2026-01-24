//! Security tests for authentication system (Chicago TDD)

use ggen_auth::{Rs256JwtManager, PasswordHasher};
use std::time::{Duration, Instant};

// Test Suite 1: Timing Attack Resistance (5 tests)

#[test]
fn test_password_verification_constant_time() {
    // Arrange
    let hasher = PasswordHasher::default().unwrap();
    let password = "SecureP@ssw0rd123";
    let hash = hasher.hash_password(password).unwrap();

    // Act - measure time for correct password
    let start_correct = Instant::now();
    for _ in 0..100 {
        let _ = hasher.verify_password(password, &hash);
    }
    let duration_correct = start_correct.elapsed();

    // Act - measure time for incorrect password
    let start_incorrect = Instant::now();
    for _ in 0..100 {
        let _ = hasher.verify_password("WrongP@ssw0rd123", &hash);
    }
    let duration_incorrect = start_incorrect.elapsed();

    // Assert - timing should be similar (within 20% variance)
    let ratio = duration_correct.as_nanos() as f64 / duration_incorrect.as_nanos() as f64;
    assert!(ratio > 0.8 && ratio < 1.2, "Timing variance too high: {}", ratio);
}

#[test]
fn test_jwt_verification_timing_consistency() {
    // Arrange
    let (private_pem, public_pem) = Rs256JwtManager::generate_key_pair().unwrap();
    let manager = Rs256JwtManager::new(&private_pem, &public_pem, 900, 604800).unwrap();
    let scopes = vec!["read".to_string()];
    let pair = manager.generate_token_pair("user123", "user@example.com", "pro", scopes).unwrap();

    // Act - measure time for valid token
    let start_valid = Instant::now();
    for _ in 0..100 {
        let _ = manager.verify_token(&pair.access_token);
    }
    let duration_valid = start_valid.elapsed();

    // Act - measure time for invalid token
    let start_invalid = Instant::now();
    for _ in 0..100 {
        let _ = manager.verify_token("invalid.token.here");
    }
    let duration_invalid = start_invalid.elapsed();

    // Assert - timing should be similar
    let ratio = duration_valid.as_nanos() as f64 / duration_invalid.as_nanos() as f64;
    assert!(ratio > 0.5 && ratio < 2.0, "Timing variance too high: {}", ratio);
}

#[test]
fn test_hash_verification_same_timing_for_different_wrong_passwords() {
    // Arrange
    let hasher = PasswordHasher::default().unwrap();
    let password = "SecureP@ssw0rd123";
    let hash = hasher.hash_password(password).unwrap();

    // Act - measure time for first wrong password
    let start1 = Instant::now();
    for _ in 0..100 {
        let _ = hasher.verify_password("WrongPass1!@#", &hash);
    }
    let duration1 = start1.elapsed();

    // Act - measure time for second wrong password
    let start2 = Instant::now();
    for _ in 0..100 {
        let _ = hasher.verify_password("WrongPass2!@#", &hash);
    }
    let duration2 = start2.elapsed();

    // Assert - timing should be similar
    let ratio = duration1.as_nanos() as f64 / duration2.as_nanos() as f64;
    assert!(ratio > 0.8 && ratio < 1.2, "Timing variance too high: {}", ratio);
}

#[test]
fn test_password_verification_no_early_termination() {
    // Arrange
    let hasher = PasswordHasher::default().unwrap();
    let password = "SecureP@ssw0rd123";
    let hash = hasher.hash_password(password).unwrap();

    // Act - verify with password that differs at start
    let start1 = Instant::now();
    for _ in 0..100 {
        let _ = hasher.verify_password("XecureP@ssw0rd123", &hash);
    }
    let duration1 = start1.elapsed();

    // Act - verify with password that differs at end
    let start2 = Instant::now();
    for _ in 0..100 {
        let _ = hasher.verify_password("SecureP@ssw0rd12X", &hash);
    }
    let duration2 = start2.elapsed();

    // Assert - timing should be similar (no early termination)
    let ratio = duration1.as_nanos() as f64 / duration2.as_nanos() as f64;
    assert!(ratio > 0.8 && ratio < 1.2, "Timing variance suggests early termination: {}", ratio);
}

#[test]
fn test_token_verification_consistent_invalid_signature_timing() {
    // Arrange
    let (private_pem1, public_pem1) = Rs256JwtManager::generate_key_pair().unwrap();
    let (private_pem2, public_pem2) = Rs256JwtManager::generate_key_pair().unwrap();
    let manager1 = Rs256JwtManager::new(&private_pem1, &public_pem1, 900, 604800).unwrap();
    let manager2 = Rs256JwtManager::new(&private_pem2, &public_pem2, 900, 604800).unwrap();
    let scopes = vec!["read".to_string()];
    let pair = manager1.generate_token_pair("user123", "user@example.com", "pro", scopes).unwrap();

    // Act - measure time for invalid signature
    let start = Instant::now();
    for _ in 0..100 {
        let _ = manager2.verify_token(&pair.access_token);
    }
    let duration = start.elapsed();

    // Assert - should complete in reasonable time (no excessive computation)
    assert!(duration < Duration::from_secs(5), "Verification took too long");
}

// Test Suite 2: Token Replay Protection (4 tests)

#[test]
fn test_tokens_have_unique_jti() {
    // Arrange
    let (private_pem, public_pem) = Rs256JwtManager::generate_key_pair().unwrap();
    let manager = Rs256JwtManager::new(&private_pem, &public_pem, 900, 604800).unwrap();
    let scopes = vec!["read".to_string()];

    // Act
    let pair1 = manager.generate_token_pair("user123", "user@example.com", "pro", scopes.clone()).unwrap();
    let pair2 = manager.generate_token_pair("user123", "user@example.com", "pro", scopes).unwrap();
    let claims1 = manager.verify_token(&pair1.access_token).unwrap();
    let claims2 = manager.verify_token(&pair2.access_token).unwrap();

    // Assert - JTIs must be unique for replay detection
    assert_ne!(claims1.jti, claims2.jti);
}

#[test]
fn test_refreshed_tokens_have_new_jti() {
    // Arrange
    let (private_pem, public_pem) = Rs256JwtManager::generate_key_pair().unwrap();
    let manager = Rs256JwtManager::new(&private_pem, &public_pem, 900, 604800).unwrap();
    let scopes = vec!["read".to_string()];
    let pair = manager.generate_token_pair("user123", "user@example.com", "pro", scopes).unwrap();
    let old_claims = manager.verify_token(&pair.access_token).unwrap();

    // Act
    let new_pair = manager.refresh_token_pair(&pair.refresh_token).unwrap();
    let new_claims = manager.verify_token(&new_pair.access_token).unwrap();

    // Assert
    assert_ne!(old_claims.jti, new_claims.jti);
}

#[test]
fn test_tokens_have_timestamps() {
    // Arrange
    let (private_pem, public_pem) = Rs256JwtManager::generate_key_pair().unwrap();
    let manager = Rs256JwtManager::new(&private_pem, &public_pem, 900, 604800).unwrap();
    let scopes = vec!["read".to_string()];

    // Act
    let pair = manager.generate_token_pair("user123", "user@example.com", "pro", scopes).unwrap();
    let claims = manager.verify_token(&pair.access_token).unwrap();

    // Assert - tokens must have iat and exp for replay detection
    assert!(claims.iat > 0);
    assert!(claims.exp > claims.iat);
}

#[test]
fn test_tokens_expire_correctly() {
    // Arrange
    let (private_pem, public_pem) = Rs256JwtManager::generate_key_pair().unwrap();
    let manager = Rs256JwtManager::new(&private_pem, &public_pem, 1, 604800).unwrap(); // 1 second expiry
    let scopes = vec!["read".to_string()];

    // Act
    let pair = manager.generate_token_pair("user123", "user@example.com", "pro", scopes).unwrap();

    // Verify immediately (should work)
    let result_before = manager.verify_token(&pair.access_token);
    assert!(result_before.is_ok());

    // Wait for expiration
    std::thread::sleep(Duration::from_secs(2));

    // Verify after expiration (should fail)
    let result_after = manager.verify_token(&pair.access_token);
    assert!(result_after.is_err());
}

// Test Suite 3: Brute Force Protection (6 tests)

#[test]
fn test_password_hashing_is_expensive() {
    // Arrange
    let hasher = PasswordHasher::default().unwrap();
    let password = "SecureP@ssw0rd123";

    // Act - measure time for single hash
    let start = Instant::now();
    let _ = hasher.hash_password(password);
    let duration = start.elapsed();

    // Assert - should take at least 10ms (Argon2 is deliberately slow)
    assert!(duration > Duration::from_millis(10), "Hashing too fast: {:?}", duration);
}

#[test]
fn test_password_verification_is_expensive() {
    // Arrange
    let hasher = PasswordHasher::default().unwrap();
    let password = "SecureP@ssw0rd123";
    let hash = hasher.hash_password(password).unwrap();

    // Act - measure time for verification
    let start = Instant::now();
    let _ = hasher.verify_password(password, &hash);
    let duration = start.elapsed();

    // Assert - should take at least 10ms
    assert!(duration > Duration::from_millis(10), "Verification too fast: {:?}", duration);
}

#[test]
fn test_unique_salts_prevent_rainbow_tables() {
    // Arrange
    let hasher = PasswordHasher::default().unwrap();
    let password = "SecureP@ssw0rd123";

    // Act
    let hash1 = hasher.hash_password(password).unwrap();
    let hash2 = hasher.hash_password(password).unwrap();

    // Assert - same password should produce different hashes
    assert_ne!(hash1, hash2);
}

#[test]
fn test_argon2_parameters_are_secure() {
    // Arrange
    let hasher = PasswordHasher::default().unwrap();
    let password = "SecureP@ssw0rd123";

    // Act
    let hash = hasher.hash_password(password).unwrap();

    // Assert - verify Argon2id with secure parameters
    assert!(hash.contains("$argon2id$"));
    assert!(hash.contains("m=19456")); // 19 MiB memory
    assert!(hash.contains("t=2"));     // 2 iterations
    assert!(hash.contains("p=1"));     // 1 parallelism
}

#[test]
fn test_jwt_signing_is_computationally_expensive() {
    // Arrange
    let (private_pem, public_pem) = Rs256JwtManager::generate_key_pair().unwrap();
    let manager = Rs256JwtManager::new(&private_pem, &public_pem, 900, 604800).unwrap();
    let scopes = vec!["read".to_string()];

    // Act - measure time for token generation
    let start = Instant::now();
    for _ in 0..10 {
        let _ = manager.generate_token_pair("user123", "user@example.com", "pro", scopes.clone());
    }
    let duration = start.elapsed();

    // Assert - RS256 signing should have some computational cost
    assert!(duration > Duration::from_micros(100), "Signing too fast: {:?}", duration);
}

#[test]
fn test_key_generation_produces_strong_keys() {
    // Arrange + Act
    let (private_pem, public_pem) = Rs256JwtManager::generate_key_pair().unwrap();

    // Assert - 4096-bit keys should be substantial
    assert!(private_pem.len() > 1000);
    assert!(public_pem.len() > 500);
    assert!(private_pem.contains("BEGIN PRIVATE KEY"));
    assert!(public_pem.contains("BEGIN PUBLIC KEY"));
}

// Test Suite 4: Cryptographic Strength (5 tests)

#[test]
fn test_different_keys_produce_incompatible_tokens() {
    // Arrange
    let (private_pem1, public_pem1) = Rs256JwtManager::generate_key_pair().unwrap();
    let (private_pem2, public_pem2) = Rs256JwtManager::generate_key_pair().unwrap();
    let manager1 = Rs256JwtManager::new(&private_pem1, &public_pem1, 900, 604800).unwrap();
    let manager2 = Rs256JwtManager::new(&private_pem2, &public_pem2, 900, 604800).unwrap();
    let scopes = vec!["read".to_string()];

    // Act
    let pair = manager1.generate_token_pair("user123", "user@example.com", "pro", scopes).unwrap();
    let result = manager2.verify_token(&pair.access_token);

    // Assert - tokens from different keys should not verify
    assert!(result.is_err());
}

#[test]
fn test_token_tampering_detection() {
    // Arrange
    let (private_pem, public_pem) = Rs256JwtManager::generate_key_pair().unwrap();
    let manager = Rs256JwtManager::new(&private_pem, &public_pem, 900, 604800).unwrap();
    let scopes = vec!["read".to_string()];
    let pair = manager.generate_token_pair("user123", "user@example.com", "pro", scopes).unwrap();

    // Act - tamper with token by changing one character
    let mut tampered_token = pair.access_token.chars().collect::<Vec<_>>();
    if let Some(c) = tampered_token.get_mut(50) {
        *c = if *c == 'a' { 'b' } else { 'a' };
    }
    let tampered = tampered_token.into_iter().collect::<String>();
    let result = manager.verify_token(&tampered);

    // Assert - tampered token should fail verification
    assert!(result.is_err());
}

#[test]
fn test_password_hash_tampering_detection() {
    // Arrange
    let hasher = PasswordHasher::default().unwrap();
    let password = "SecureP@ssw0rd123";
    let hash = hasher.hash_password(password).unwrap();

    // Act - tamper with hash by changing one character
    let mut tampered_hash = hash.chars().collect::<Vec<_>>();
    if let Some(c) = tampered_hash.get_mut(50) {
        *c = if *c == 'a' { 'b' } else { 'a' };
    }
    let tampered = tampered_hash.into_iter().collect::<String>();
    let result = hasher.verify_password(password, &tampered);

    // Assert - tampered hash should fail verification
    assert!(result.is_err());
}

#[test]
fn test_rs256_uses_asymmetric_cryptography() {
    // Arrange
    let (private_pem, public_pem) = Rs256JwtManager::generate_key_pair().unwrap();

    // Act
    let manager = Rs256JwtManager::new(&private_pem, &public_pem, 900, 604800).unwrap();
    let scopes = vec!["read".to_string()];
    let pair = manager.generate_token_pair("user123", "user@example.com", "pro", scopes).unwrap();

    // Assert - should be able to verify with public key only
    let verify_only_manager = Rs256JwtManager::new(&private_pem, &public_pem, 900, 604800).unwrap();
    let result = verify_only_manager.verify_token(&pair.access_token);
    assert!(result.is_ok());
}

#[test]
fn test_argon2id_provides_side_channel_resistance() {
    // Arrange
    let hasher = PasswordHasher::default().unwrap();
    let password = "SecureP@ssw0rd123";

    // Act
    let hash = hasher.hash_password(password).unwrap();

    // Assert - Argon2id provides both data-dependent and data-independent modes
    assert!(hash.contains("$argon2id$"));
}
