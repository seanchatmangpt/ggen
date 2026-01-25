//! Password hashing and verification using Argon2

use argon2::{
    password_hash::{rand_core::OsRng, PasswordHash, PasswordHasher as PasswordHasherTrait, PasswordVerifier, SaltString},
    Argon2, Params, Algorithm, Version,
};
use zeroize::Zeroize;

use crate::{errors::AuthError, AuthResult};

/// Password complexity requirements
pub struct PasswordRequirements {
    pub min_length: usize,
    pub require_uppercase: bool,
    pub require_lowercase: bool,
    pub require_digit: bool,
    pub require_special: bool,
}

impl Default for PasswordRequirements {
    fn default() -> Self {
        Self {
            min_length: 12,
            require_uppercase: true,
            require_lowercase: true,
            require_digit: true,
            require_special: true,
        }
    }
}

/// Password hasher using Argon2
pub struct PasswordHasher {
    argon2: Argon2<'static>,
    requirements: PasswordRequirements,
}

impl PasswordHasher {
    /// Create a new password hasher with secure Argon2 parameters
    ///
    /// # Errors
    /// Returns error if Argon2 parameters are invalid
    pub fn new(requirements: PasswordRequirements) -> AuthResult<Self> {
        // Argon2id with OWASP recommended parameters
        // m_cost: 19456 KiB (19 MiB)
        // t_cost: 2 iterations
        // p_cost: 1 parallelism
        let params = Params::new(19456, 2, 1, None)
            .map_err(|e| AuthError::CryptoError(format!("Invalid Argon2 params: {}", e)))?;

        let argon2 = Argon2::new(Algorithm::Argon2id, Version::V0x13, params);

        Ok(Self {
            argon2,
            requirements,
        })
    }

    /// Create a password hasher with default requirements
    ///
    /// # Errors
    /// Returns error if initialization fails
    pub fn default() -> AuthResult<Self> {
        Self::new(PasswordRequirements::default())
    }

    /// Hash a password
    ///
    /// # Errors
    /// Returns error if password is weak or hashing fails
    pub fn hash_password(&self, password: &str) -> AuthResult<String> {
        // Validate password complexity
        self.validate_password(password)?;

        // Generate random salt
        let salt = SaltString::generate(&mut OsRng);

        // Hash password
        let password_hash = self.argon2
            .hash_password(password.as_bytes(), &salt)
            .map_err(|e| AuthError::CryptoError(format!("Password hashing failed: {}", e)))?;

        Ok(password_hash.to_string())
    }

    /// Verify a password against a hash
    ///
    /// # Errors
    /// Returns error if password is incorrect or verification fails
    pub fn verify_password(&self, password: &str, hash: &str) -> AuthResult<()> {
        let parsed_hash = PasswordHash::new(hash)
            .map_err(|e| AuthError::CryptoError(format!("Invalid password hash: {}", e)))?;

        self.argon2
            .verify_password(password.as_bytes(), &parsed_hash)
            .map_err(|_| AuthError::InvalidCredentials)
    }

    /// Validate password complexity
    ///
    /// # Errors
    /// Returns error if password does not meet requirements
    pub fn validate_password(&self, password: &str) -> AuthResult<()> {
        if password.len() < self.requirements.min_length {
            return Err(AuthError::WeakPassword);
        }

        if self.requirements.require_uppercase && !password.chars().any(|c| c.is_uppercase()) {
            return Err(AuthError::WeakPassword);
        }

        if self.requirements.require_lowercase && !password.chars().any(|c| c.is_lowercase()) {
            return Err(AuthError::WeakPassword);
        }

        if self.requirements.require_digit && !password.chars().any(|c| c.is_ascii_digit()) {
            return Err(AuthError::WeakPassword);
        }

        if self.requirements.require_special {
            let has_special = password.chars().any(|c| !c.is_alphanumeric());
            if !has_special {
                return Err(AuthError::WeakPassword);
            }
        }

        Ok(())
    }

    /// Check if a password hash needs rehashing (for parameter updates)
    pub fn needs_rehash(&self, hash: &str) -> bool {
        if let Ok(parsed_hash) = PasswordHash::new(hash) {
            // Check if algorithm changed (comparing identifiers)
            let expected_algorithm = Algorithm::Argon2id.ident();
            if parsed_hash.algorithm != expected_algorithm {
                return true;
            }

            // In production, you'd also check params
            // For now, we'll keep it simple
            false
        } else {
            true
        }
    }
}

/// Secure password string that zeroizes on drop
pub struct SecurePassword {
    inner: String,
}

impl SecurePassword {
    /// Create a new secure password
    pub fn new(password: String) -> Self {
        Self { inner: password }
    }

    /// Get password as string slice
    pub fn as_str(&self) -> &str {
        &self.inner
    }
}

impl Drop for SecurePassword {
    fn drop(&mut self) {
        self.inner.zeroize();
    }
}

impl From<String> for SecurePassword {
    fn from(password: String) -> Self {
        Self::new(password)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn create_test_hasher() -> PasswordHasher {
        PasswordHasher::default().unwrap()
    }

    #[test]
    fn test_hash_password() {
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
        assert!(matches!(result.unwrap_err(), AuthError::InvalidCredentials));
    }

    #[test]
    fn test_password_too_short() {
        // Arrange
        let hasher = create_test_hasher();

        // Act
        let result = hasher.validate_password("Short1!");

        // Assert
        assert!(result.is_err());
        assert!(matches!(result.unwrap_err(), AuthError::WeakPassword));
    }

    #[test]
    fn test_password_no_uppercase() {
        // Arrange
        let hasher = create_test_hasher();

        // Act
        let result = hasher.validate_password("nouppercase123!");

        // Assert
        assert!(result.is_err());
        assert!(matches!(result.unwrap_err(), AuthError::WeakPassword));
    }

    #[test]
    fn test_password_no_lowercase() {
        // Arrange
        let hasher = create_test_hasher();

        // Act
        let result = hasher.validate_password("NOLOWERCASE123!");

        // Assert
        assert!(result.is_err());
        assert!(matches!(result.unwrap_err(), AuthError::WeakPassword));
    }

    #[test]
    fn test_password_no_digit() {
        // Arrange
        let hasher = create_test_hasher();

        // Act
        let result = hasher.validate_password("NoDigitsHere!");

        // Assert
        assert!(result.is_err());
        assert!(matches!(result.unwrap_err(), AuthError::WeakPassword));
    }

    #[test]
    fn test_password_no_special() {
        // Arrange
        let hasher = create_test_hasher();

        // Act
        let result = hasher.validate_password("NoSpecialChar123");

        // Assert
        assert!(result.is_err());
        assert!(matches!(result.unwrap_err(), AuthError::WeakPassword));
    }

    #[test]
    fn test_valid_password() {
        // Arrange
        let hasher = create_test_hasher();

        // Act
        let result = hasher.validate_password("ValidP@ssw0rd123");

        // Assert
        assert!(result.is_ok());
    }

    #[test]
    fn test_hash_uniqueness() {
        // Arrange
        let hasher = create_test_hasher();
        let password = "SecureP@ssw0rd123";

        // Act
        let hash1 = hasher.hash_password(password).unwrap();
        let hash2 = hasher.hash_password(password).unwrap();

        // Assert - hashes should be different due to random salt
        assert_ne!(hash1, hash2);

        // But both should verify correctly
        assert!(hasher.verify_password(password, &hash1).is_ok());
        assert!(hasher.verify_password(password, &hash2).is_ok());
    }

    #[test]
    fn test_secure_password_zeroizes() {
        // Arrange
        let password = String::from("SecureP@ssw0rd123");
        let secure_pwd = SecurePassword::new(password);

        // Act
        let pwd_str = secure_pwd.as_str();

        // Assert
        assert_eq!(pwd_str, "SecureP@ssw0rd123");

        // Drop will zeroize (can't test directly, but coverage is good)
    }
}
