//! Comprehensive tests for RS256 JWT implementation (Chicago TDD)

use ggen_auth::{Rs256JwtManager, Rs256TokenClaims, TokenType};

// Helper function to create test manager
fn create_test_manager() -> Rs256JwtManager {
    let (private_pem, public_pem) = Rs256JwtManager::generate_key_pair().unwrap();
    Rs256JwtManager::new(&private_pem, &public_pem, 900, 604800).unwrap()
}

// Test Suite 1: Key Generation (5 tests)

#[test]
fn test_generate_key_pair_creates_valid_keys() {
    // Arrange + Act
    let result = Rs256JwtManager::generate_key_pair();

    // Assert
    assert!(result.is_ok());
    let (private_pem, public_pem) = result.unwrap();
    assert!(private_pem.contains("BEGIN PRIVATE KEY"));
    assert!(public_pem.contains("BEGIN PUBLIC KEY"));
    assert!(private_pem.len() > 1000); // 4096-bit key should be substantial
    assert!(public_pem.len() > 500);
}

#[test]
fn test_generate_key_pair_produces_unique_keys() {
    // Arrange + Act
    let (private1, public1) = Rs256JwtManager::generate_key_pair().unwrap();
    let (private2, public2) = Rs256JwtManager::generate_key_pair().unwrap();

    // Assert - keys should be unique due to randomness
    assert_ne!(private1, private2);
    assert_ne!(public1, public2);
}

#[test]
fn test_manager_creation_with_valid_keys() {
    // Arrange
    let (private_pem, public_pem) = Rs256JwtManager::generate_key_pair().unwrap();

    // Act
    let result = Rs256JwtManager::new(&private_pem, &public_pem, 900, 604800);

    // Assert
    assert!(result.is_ok());
}

#[test]
fn test_manager_creation_fails_with_invalid_private_key() {
    // Arrange
    let (_, public_pem) = Rs256JwtManager::generate_key_pair().unwrap();
    let invalid_private = "-----BEGIN PRIVATE KEY-----\ninvalid\n-----END PRIVATE KEY-----";

    // Act
    let result = Rs256JwtManager::new(invalid_private, &public_pem, 900, 604800);

    // Assert
    assert!(result.is_err());
}

#[test]
fn test_manager_creation_fails_with_invalid_public_key() {
    // Arrange
    let (private_pem, _) = Rs256JwtManager::generate_key_pair().unwrap();
    let invalid_public = "-----BEGIN PUBLIC KEY-----\ninvalid\n-----END PUBLIC KEY-----";

    // Act
    let result = Rs256JwtManager::new(&private_pem, invalid_public, 900, 604800);

    // Assert
    assert!(result.is_err());
}

// Test Suite 2: Token Generation (8 tests)

#[test]
fn test_generate_token_pair_creates_both_tokens() {
    // Arrange
    let manager = create_test_manager();
    let scopes = vec!["read".to_string(), "write".to_string()];

    // Act
    let result = manager.generate_token_pair("user123", "user@example.com", "pro", scopes);

    // Assert
    assert!(result.is_ok());
    let pair = result.unwrap();
    assert!(!pair.access_token.is_empty());
    assert!(!pair.refresh_token.is_empty());
    assert_ne!(pair.access_token, pair.refresh_token);
}

#[test]
fn test_token_pair_has_correct_expiration_times() {
    // Arrange
    let manager = create_test_manager();
    let scopes = vec!["read".to_string()];

    // Act
    let pair = manager
        .generate_token_pair("user123", "user@example.com", "free", scopes)
        .unwrap();

    // Assert
    assert_eq!(pair.access_expires_in, 900); // 15 minutes
    assert_eq!(pair.refresh_expires_in, 604800); // 7 days
}

#[test]
fn test_generated_tokens_are_unique() {
    // Arrange
    let manager = create_test_manager();
    let scopes = vec!["read".to_string()];

    // Act
    let pair1 = manager
        .generate_token_pair("user123", "user@example.com", "pro", scopes.clone())
        .unwrap();
    let pair2 = manager
        .generate_token_pair("user123", "user@example.com", "pro", scopes)
        .unwrap();

    // Assert - even for same user, tokens should be unique (different JTI)
    assert_ne!(pair1.access_token, pair2.access_token);
    assert_ne!(pair1.refresh_token, pair2.refresh_token);
}

#[test]
fn test_token_contains_correct_user_data() {
    // Arrange
    let manager = create_test_manager();
    let scopes = vec!["read".to_string(), "write".to_string()];

    // Act
    let pair = manager
        .generate_token_pair("user456", "test@example.com", "enterprise", scopes.clone())
        .unwrap();
    let claims = manager.verify_token(&pair.access_token).unwrap();

    // Assert
    assert_eq!(claims.sub, "user456");
    assert_eq!(claims.email, "test@example.com");
    assert_eq!(claims.tier, "enterprise");
    assert_eq!(claims.scopes, scopes);
}

#[test]
fn test_access_token_has_correct_type() {
    // Arrange
    let manager = create_test_manager();
    let scopes = vec!["read".to_string()];

    // Act
    let pair = manager
        .generate_token_pair("user123", "user@example.com", "free", scopes)
        .unwrap();
    let claims = manager.verify_token(&pair.access_token).unwrap();

    // Assert
    assert_eq!(claims.token_type, "access");
}

#[test]
fn test_refresh_token_has_correct_type() {
    // Arrange
    let manager = create_test_manager();
    let scopes = vec!["read".to_string()];

    // Act
    let pair = manager
        .generate_token_pair("user123", "user@example.com", "free", scopes)
        .unwrap();
    let claims = manager.verify_token(&pair.refresh_token).unwrap();

    // Assert
    assert_eq!(claims.token_type, "refresh");
}

#[test]
fn test_token_has_valid_timestamps() {
    // Arrange
    let manager = create_test_manager();
    let scopes = vec!["read".to_string()];

    // Act
    let pair = manager
        .generate_token_pair("user123", "user@example.com", "pro", scopes)
        .unwrap();
    let claims = manager.verify_token(&pair.access_token).unwrap();

    // Assert
    assert!(claims.iat > 0);
    assert!(claims.exp > claims.iat);
    assert_eq!(claims.exp - claims.iat, 900); // 15 minutes
}

#[test]
fn test_token_has_unique_jti() {
    // Arrange
    let manager = create_test_manager();
    let scopes = vec!["read".to_string()];

    // Act
    let pair1 = manager
        .generate_token_pair("user123", "user@example.com", "pro", scopes.clone())
        .unwrap();
    let pair2 = manager
        .generate_token_pair("user123", "user@example.com", "pro", scopes)
        .unwrap();
    let claims1 = manager.verify_token(&pair1.access_token).unwrap();
    let claims2 = manager.verify_token(&pair2.access_token).unwrap();

    // Assert
    assert_ne!(claims1.jti, claims2.jti);
}

// Test Suite 3: Token Verification (7 tests)

#[test]
fn test_verify_valid_access_token() {
    // Arrange
    let manager = create_test_manager();
    let scopes = vec!["read".to_string(), "write".to_string()];
    let pair = manager
        .generate_token_pair("user123", "user@example.com", "pro", scopes)
        .unwrap();

    // Act
    let result = manager.verify_token(&pair.access_token);

    // Assert
    assert!(result.is_ok());
    let claims = result.unwrap();
    assert_eq!(claims.sub, "user123");
    assert_eq!(claims.email, "user@example.com");
}

#[test]
fn test_verify_valid_refresh_token() {
    // Arrange
    let manager = create_test_manager();
    let scopes = vec!["read".to_string()];
    let pair = manager
        .generate_token_pair("user123", "user@example.com", "free", scopes)
        .unwrap();

    // Act
    let result = manager.verify_token(&pair.refresh_token);

    // Assert
    assert!(result.is_ok());
    let claims = result.unwrap();
    assert_eq!(claims.token_type, "refresh");
}

#[test]
fn test_verify_rejects_invalid_token() {
    // Arrange
    let manager = create_test_manager();

    // Act
    let result = manager.verify_token("invalid.token.here");

    // Assert
    assert!(result.is_err());
}

#[test]
fn test_verify_rejects_malformed_token() {
    // Arrange
    let manager = create_test_manager();

    // Act
    let result = manager.verify_token("not.even.jwt");

    // Assert
    assert!(result.is_err());
}

#[test]
fn test_verify_rejects_token_from_different_key() {
    // Arrange
    let manager1 = create_test_manager();
    let manager2 = create_test_manager();
    let scopes = vec!["read".to_string()];
    let pair = manager1
        .generate_token_pair("user123", "user@example.com", "pro", scopes)
        .unwrap();

    // Act - try to verify with different manager (different keys)
    let result = manager2.verify_token(&pair.access_token);

    // Assert
    assert!(result.is_err());
}

#[test]
fn test_verify_rejects_empty_token() {
    // Arrange
    let manager = create_test_manager();

    // Act
    let result = manager.verify_token("");

    // Assert
    assert!(result.is_err());
}

#[test]
fn test_verify_preserves_all_claims() {
    // Arrange
    let manager = create_test_manager();
    let scopes = vec!["admin".to_string(), "read".to_string(), "write".to_string()];
    let pair = manager
        .generate_token_pair("user789", "admin@example.com", "enterprise", scopes.clone())
        .unwrap();

    // Act
    let claims = manager.verify_token(&pair.access_token).unwrap();

    // Assert
    assert_eq!(claims.sub, "user789");
    assert_eq!(claims.email, "admin@example.com");
    assert_eq!(claims.tier, "enterprise");
    assert_eq!(claims.scopes, scopes);
    assert_eq!(claims.token_type, "access");
    assert!(!claims.jti.is_empty());
}

// Test Suite 4: Token Rotation (5 tests)

#[test]
fn test_refresh_token_pair_generates_new_tokens() {
    // Arrange
    let manager = create_test_manager();
    let scopes = vec!["read".to_string()];
    let pair = manager
        .generate_token_pair("user123", "user@example.com", "pro", scopes)
        .unwrap();

    // Act
    let result = manager.refresh_token_pair(&pair.refresh_token);

    // Assert
    assert!(result.is_ok());
    let new_pair = result.unwrap();
    assert_ne!(new_pair.access_token, pair.access_token);
    assert_ne!(new_pair.refresh_token, pair.refresh_token);
}

#[test]
fn test_refresh_preserves_user_data() {
    // Arrange
    let manager = create_test_manager();
    let scopes = vec!["read".to_string(), "write".to_string()];
    let pair = manager
        .generate_token_pair("user456", "test@example.com", "enterprise", scopes.clone())
        .unwrap();

    // Act
    let new_pair = manager.refresh_token_pair(&pair.refresh_token).unwrap();
    let new_claims = manager.verify_token(&new_pair.access_token).unwrap();

    // Assert
    assert_eq!(new_claims.sub, "user456");
    assert_eq!(new_claims.email, "test@example.com");
    assert_eq!(new_claims.tier, "enterprise");
    assert_eq!(new_claims.scopes, scopes);
}

#[test]
fn test_cannot_refresh_with_access_token() {
    // Arrange
    let manager = create_test_manager();
    let scopes = vec!["read".to_string()];
    let pair = manager
        .generate_token_pair("user123", "user@example.com", "pro", scopes)
        .unwrap();

    // Act
    let result = manager.refresh_token_pair(&pair.access_token);

    // Assert
    assert!(result.is_err());
}

#[test]
fn test_cannot_refresh_with_invalid_token() {
    // Arrange
    let manager = create_test_manager();

    // Act
    let result = manager.refresh_token_pair("invalid.refresh.token");

    // Assert
    assert!(result.is_err());
}

#[test]
fn test_refresh_generates_new_jti() {
    // Arrange
    let manager = create_test_manager();
    let scopes = vec!["read".to_string()];
    let pair = manager
        .generate_token_pair("user123", "user@example.com", "pro", scopes)
        .unwrap();
    let old_access_claims = manager.verify_token(&pair.access_token).unwrap();

    // Act
    let new_pair = manager.refresh_token_pair(&pair.refresh_token).unwrap();
    let new_access_claims = manager.verify_token(&new_pair.access_token).unwrap();
    let new_refresh_claims = manager.verify_token(&new_pair.refresh_token).unwrap();

    // Assert - all JTIs should be unique
    assert_ne!(old_access_claims.jti, new_access_claims.jti);
    assert_ne!(old_access_claims.jti, new_refresh_claims.jti);
    assert_ne!(new_access_claims.jti, new_refresh_claims.jti);
}
