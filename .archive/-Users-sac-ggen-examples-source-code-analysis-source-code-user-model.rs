// Database model with serialization and validation
// Demonstrates common patterns for data models

use serde::{Deserialize, Serialize};
use std::time::SystemTime;

/// User model representing a user in the system
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
pub struct User {
    /// Unique user identifier
    pub id: i64,

    /// Username (unique, 3-50 characters)
    pub username: String,

    /// Email address (unique, valid format)
    pub email: String,

    /// Hashed password
    #[serde(skip_serializing)]
    pub password_hash: String,

    /// User's full name
    pub full_name: Option<String>,

    /// User's biography
    pub bio: Option<String>,

    /// Account creation timestamp
    pub created_at: SystemTime,

    /// Last update timestamp
    pub updated_at: SystemTime,

    /// Whether the user account is active
    pub is_active: bool,

    /// Whether the user is an administrator
    pub is_admin: bool,

    /// Email verification status
    pub email_verified: bool,
}

impl User {
    /// Create a new user with required fields
    pub fn new(
        id: i64,
        username: String,
        email: String,
        password_hash: String,
    ) -> Result<Self, ValidationError> {
        let now = SystemTime::now();

        let user = Self {
            id,
            username,
            email,
            password_hash,
            full_name: None,
            bio: None,
            created_at: now,
            updated_at: now,
            is_active: true,
            is_admin: false,
            email_verified: false,
        };

        user.validate()?;
        Ok(user)
    }

    /// Validate user data
    pub fn validate(&self) -> Result<(), ValidationError> {
        // Validate username
        if self.username.len() < 3 {
            return Err(ValidationError::UsernameTooShort);
        }
        if self.username.len() > 50 {
            return Err(ValidationError::UsernameTooLong);
        }
        if !self.username.chars().all(|c| c.is_alphanumeric() || c == '_' || c == '-') {
            return Err(ValidationError::InvalidUsernameFormat);
        }

        // Validate email
        if !self.email.contains('@') {
            return Err(ValidationError::InvalidEmailFormat);
        }
        if self.email.len() > 255 {
            return Err(ValidationError::EmailTooLong);
        }

        // Validate password hash
        if self.password_hash.is_empty() {
            return Err(ValidationError::PasswordHashRequired);
        }

        // Validate bio length if present
        if let Some(bio) = &self.bio {
            if bio.len() > 500 {
                return Err(ValidationError::BioTooLong);
            }
        }

        Ok(())
    }

    /// Update user's full name
    pub fn set_full_name(&mut self, name: String) {
        self.full_name = Some(name);
        self.updated_at = SystemTime::now();
    }

    /// Update user's bio
    pub fn set_bio(&mut self, bio: String) -> Result<(), ValidationError> {
        if bio.len() > 500 {
            return Err(ValidationError::BioTooLong);
        }
        self.bio = Some(bio);
        self.updated_at = SystemTime::now();
        Ok(())
    }

    /// Mark email as verified
    pub fn verify_email(&mut self) {
        self.email_verified = true;
        self.updated_at = SystemTime::now();
    }

    /// Deactivate user account
    pub fn deactivate(&mut self) {
        self.is_active = false;
        self.updated_at = SystemTime::now();
    }

    /// Activate user account
    pub fn activate(&mut self) {
        self.is_active = true;
        self.updated_at = SystemTime::now();
    }

    /// Convert to public user representation (without sensitive data)
    pub fn to_public(&self) -> PublicUser {
        PublicUser {
            id: self.id,
            username: self.username.clone(),
            full_name: self.full_name.clone(),
            bio: self.bio.clone(),
            created_at: self.created_at,
            is_active: self.is_active,
        }
    }
}

/// Public user representation without sensitive data
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct PublicUser {
    pub id: i64,
    pub username: String,
    pub full_name: Option<String>,
    pub bio: Option<String>,
    pub created_at: SystemTime,
    pub is_active: bool,
}

/// User creation request
#[derive(Debug, Deserialize)]
pub struct CreateUserRequest {
    pub username: String,
    pub email: String,
    pub password: String,
    pub full_name: Option<String>,
}

/// User update request
#[derive(Debug, Deserialize)]
pub struct UpdateUserRequest {
    pub full_name: Option<String>,
    pub bio: Option<String>,
}

/// Validation errors for user data
#[derive(Debug, Clone, PartialEq)]
pub enum ValidationError {
    UsernameTooShort,
    UsernameTooLong,
    InvalidUsernameFormat,
    InvalidEmailFormat,
    EmailTooLong,
    PasswordHashRequired,
    BioTooLong,
}

impl std::fmt::Display for ValidationError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::UsernameTooShort => write!(f, "Username must be at least 3 characters"),
            Self::UsernameTooLong => write!(f, "Username must be at most 50 characters"),
            Self::InvalidUsernameFormat => write!(f, "Username can only contain alphanumeric characters, underscores, and hyphens"),
            Self::InvalidEmailFormat => write!(f, "Invalid email format"),
            Self::EmailTooLong => write!(f, "Email must be at most 255 characters"),
            Self::PasswordHashRequired => write!(f, "Password hash is required"),
            Self::BioTooLong => write!(f, "Bio must be at most 500 characters"),
        }
    }
}

impl std::error::Error for ValidationError {}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_create_user() {
        let user = User::new(
            1,
            "testuser".to_string(),
            "test@example.com".to_string(),
            "hashed_password".to_string(),
        ).unwrap();

        assert_eq!(user.username, "testuser");
        assert_eq!(user.email, "test@example.com");
        assert!(user.is_active);
        assert!(!user.is_admin);
    }

    #[test]
    fn test_username_validation() {
        let result = User::new(
            1,
            "ab".to_string(), // Too short
            "test@example.com".to_string(),
            "hashed_password".to_string(),
        );
        assert!(result.is_err());
    }

    #[test]
    fn test_email_verification() {
        let mut user = User::new(
            1,
            "testuser".to_string(),
            "test@example.com".to_string(),
            "hashed_password".to_string(),
        ).unwrap();

        assert!(!user.email_verified);
        user.verify_email();
        assert!(user.email_verified);
    }

    #[test]
    fn test_public_user_conversion() {
        let user = User::new(
            1,
            "testuser".to_string(),
            "test@example.com".to_string(),
            "hashed_password".to_string(),
        ).unwrap();

        let public = user.to_public();
        assert_eq!(public.username, "testuser");
    }
}
