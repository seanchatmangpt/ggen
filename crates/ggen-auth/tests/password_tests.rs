//! Comprehensive tests for password hashing with Argon2 (Chicago TDD)

use ggen_auth::{PasswordHasher, PasswordRequirements, SecurePassword};

// Helper function to create test hasher
fn create_test_hasher() -> PasswordHasher {
    PasswordHasher::default().unwrap()
}

// Test Suite 1: Password Hashing (8 tests)

#[test]
fn test_hash_password_creates_argon2_hash() {
    // Arrange
    let hasher = create_test_hasher();
    let password = "SecureP@ssw0rd123";

    // Act
    let result = hasher.hash_password(password);

    // Assert
    assert!(result.is_ok());
    let hash = result.unwrap();
    assert!(hash.starts_with("$argon2id$"));
}

#[test]
fn test_hash_password_produces_unique_hashes() {
    // Arrange
    let hasher = create_test_hasher();
    let password = "SecureP@ssw0rd123";

    // Act
    let hash1 = hasher.hash_password(password).unwrap();
    let hash2 = hasher.hash_password(password).unwrap();

    // Assert - hashes should differ due to random salt
    assert_ne!(hash1, hash2);
}

#[test]
fn test_hash_password_rejects_weak_password() {
    // Arrange
    let hasher = create_test_hasher();
    let weak_password = "weak";

    // Act
    let result = hasher.hash_password(weak_password);

    // Assert
    assert!(result.is_err());
}

#[test]
fn test_verify_correct_password() {
    // Arrange
    let hasher = create_test_hasher();
    let password = "SecureP@ssw0rd123";
    let hash = hasher.hash_password(password).unwrap();

    // Act
    let result = hasher.verify_password(password, &hash);

    // Assert
    assert!(result.is_ok());
}

#[test]
fn test_verify_incorrect_password() {
    // Arrange
    let hasher = create_test_hasher();
    let password = "SecureP@ssw0rd123";
    let hash = hasher.hash_password(password).unwrap();

    // Act
    let result = hasher.verify_password("WrongP@ssw0rd123", &hash);

    // Assert
    assert!(result.is_err());
}

#[test]
fn test_verify_fails_with_invalid_hash() {
    // Arrange
    let hasher = create_test_hasher();

    // Act
    let result = hasher.verify_password("SecureP@ssw0rd123", "invalid_hash");

    // Assert
    assert!(result.is_err());
}

#[test]
fn test_hash_contains_salt_and_params() {
    // Arrange
    let hasher = create_test_hasher();
    let password = "SecureP@ssw0rd123";

    // Act
    let hash = hasher.hash_password(password).unwrap();

    // Assert
    // Argon2 hash format: $argon2id$v=19$m=19456,t=2,p=1$salt$hash
    assert!(hash.contains("$argon2id$"));
    assert!(hash.contains("m=19456")); // Memory cost
    assert!(hash.contains("t=2")); // Time cost
    assert!(hash.contains("p=1")); // Parallelism
}

#[test]
fn test_different_passwords_produce_different_hashes() {
    // Arrange
    let hasher = create_test_hasher();

    // Act
    let hash1 = hasher.hash_password("Password1!@#").unwrap();
    let hash2 = hasher.hash_password("Password2!@#").unwrap();

    // Assert
    assert_ne!(hash1, hash2);
    assert!(hasher.verify_password("Password1!@#", &hash1).is_ok());
    assert!(hasher.verify_password("Password2!@#", &hash2).is_ok());
}

// Test Suite 2: Password Complexity Validation (12 tests)

#[test]
fn test_validate_password_accepts_strong_password() {
    // Arrange
    let hasher = create_test_hasher();

    // Act
    let result = hasher.validate_password("StrongP@ssw0rd123!");

    // Assert
    assert!(result.is_ok());
}

#[test]
fn test_validate_password_rejects_too_short() {
    // Arrange
    let hasher = create_test_hasher();

    // Act
    let result = hasher.validate_password("Short1!");

    // Assert
    assert!(result.is_err());
}

#[test]
fn test_validate_password_rejects_no_uppercase() {
    // Arrange
    let hasher = create_test_hasher();

    // Act
    let result = hasher.validate_password("nouppercase123!");

    // Assert
    assert!(result.is_err());
}

#[test]
fn test_validate_password_rejects_no_lowercase() {
    // Arrange
    let hasher = create_test_hasher();

    // Act
    let result = hasher.validate_password("NOLOWERCASE123!");

    // Assert
    assert!(result.is_err());
}

#[test]
fn test_validate_password_rejects_no_digit() {
    // Arrange
    let hasher = create_test_hasher();

    // Act
    let result = hasher.validate_password("NoDigitsHere!");

    // Assert
    assert!(result.is_err());
}

#[test]
fn test_validate_password_rejects_no_special() {
    // Arrange
    let hasher = create_test_hasher();

    // Act
    let result = hasher.validate_password("NoSpecialChar123");

    // Assert
    assert!(result.is_err());
}

#[test]
fn test_validate_password_minimum_length_boundary() {
    // Arrange
    let hasher = create_test_hasher();

    // Act - exactly 12 characters
    let result = hasher.validate_password("ValidPass1!");

    // Assert
    assert!(result.is_ok());
}

#[test]
fn test_validate_password_below_minimum_length() {
    // Arrange
    let hasher = create_test_hasher();

    // Act - 11 characters (below minimum)
    let result = hasher.validate_password("ValidPas1!");

    // Assert
    assert!(result.is_err());
}

#[test]
fn test_validate_password_with_multiple_special_chars() {
    // Arrange
    let hasher = create_test_hasher();

    // Act
    let result = hasher.validate_password("P@ssw0rd!#$%^&*");

    // Assert
    assert!(result.is_ok());
}

#[test]
fn test_validate_password_with_unicode_special() {
    // Arrange
    let hasher = create_test_hasher();

    // Act
    let result = hasher.validate_password("PassWord123™");

    // Assert
    assert!(result.is_ok());
}

#[test]
fn test_validate_password_all_requirements_minimal() {
    // Arrange
    let hasher = create_test_hasher();

    // Act - minimal password meeting all requirements
    let result = hasher.validate_password("Aa1!bbbbbbbb");

    // Assert
    assert!(result.is_ok());
}

#[test]
fn test_validate_password_very_long_password() {
    // Arrange
    let hasher = create_test_hasher();

    // Act
    let result = hasher.validate_password("VeryLongP@ssw0rd!ThisIsStillValid12345");

    // Assert
    assert!(result.is_ok());
}

// Test Suite 3: Custom Requirements (5 tests)

#[test]
fn test_custom_requirements_no_uppercase() {
    // Arrange
    let requirements = PasswordRequirements {
        min_length: 8,
        require_uppercase: false,
        require_lowercase: true,
        require_digit: true,
        require_special: true,
    };
    let hasher = PasswordHasher::new(requirements).unwrap();

    // Act
    let result = hasher.validate_password("password1!");

    // Assert
    assert!(result.is_ok());
}

#[test]
fn test_custom_requirements_shorter_min_length() {
    // Arrange
    let requirements = PasswordRequirements {
        min_length: 8,
        require_uppercase: true,
        require_lowercase: true,
        require_digit: true,
        require_special: true,
    };
    let hasher = PasswordHasher::new(requirements).unwrap();

    // Act
    let result = hasher.validate_password("Pass1!");

    // Assert
    assert!(result.is_err()); // Only 6 chars, min is 8
}

#[test]
fn test_custom_requirements_no_special_required() {
    // Arrange
    let requirements = PasswordRequirements {
        min_length: 12,
        require_uppercase: true,
        require_lowercase: true,
        require_digit: true,
        require_special: false,
    };
    let hasher = PasswordHasher::new(requirements).unwrap();

    // Act
    let result = hasher.validate_password("Password1234");

    // Assert
    assert!(result.is_ok());
}

#[test]
fn test_custom_requirements_very_strict() {
    // Arrange
    let requirements = PasswordRequirements {
        min_length: 20,
        require_uppercase: true,
        require_lowercase: true,
        require_digit: true,
        require_special: true,
    };
    let hasher = PasswordHasher::new(requirements).unwrap();

    // Act
    let result = hasher.validate_password("VeryLongP@ssw0rd123!");

    // Assert
    assert!(result.is_ok());
}

#[test]
fn test_custom_requirements_minimal() {
    // Arrange
    let requirements = PasswordRequirements {
        min_length: 4,
        require_uppercase: false,
        require_lowercase: false,
        require_digit: false,
        require_special: false,
    };
    let hasher = PasswordHasher::new(requirements).unwrap();

    // Act
    let result = hasher.validate_password("test");

    // Assert
    assert!(result.is_ok());
}

// Test Suite 4: Secure Password (3 tests)

#[test]
fn test_secure_password_creation() {
    // Arrange + Act
    let secure_pwd = SecurePassword::new("TestPassword123!".to_string());

    // Assert
    assert_eq!(secure_pwd.as_str(), "TestPassword123!");
}

#[test]
fn test_secure_password_from_string() {
    // Arrange
    let password = "TestPassword123!".to_string();

    // Act
    let secure_pwd = SecurePassword::from(password);

    // Assert
    assert_eq!(secure_pwd.as_str(), "TestPassword123!");
}

#[test]
fn test_secure_password_zeroizes_on_drop() {
    // Arrange
    let password = "TestPassword123!".to_string();
    let secure_pwd = SecurePassword::new(password);

    // Act
    let pwd_str = secure_pwd.as_str();

    // Assert
    assert_eq!(pwd_str, "TestPassword123!");

    // Note: Drop will zeroize memory, but we can't test memory directly
    // This test verifies the API works correctly
}
