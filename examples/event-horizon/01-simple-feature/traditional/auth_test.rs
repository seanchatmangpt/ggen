// Traditional Approach: Hand-written tests
// File: auth_test.rs (98 LOC)

#[cfg(test)]
mod tests {
    use super::super::{AuthService, AuthError};

    #[test]
    fn test_register_success() {
        let mut service = AuthService::new();
        let result = service.register(
            "test@example.com".to_string(),
            "password123".to_string()
        );

        assert!(result.is_ok());
        let user = result.unwrap();
        assert_eq!(user.email, "test@example.com");
        assert_ne!(user.password_hash, "password123"); // Should be hashed
    }

    #[test]
    fn test_register_invalid_email() {
        let mut service = AuthService::new();
        let result = service.register(
            "invalid-email".to_string(),
            "password123".to_string()
        );

        assert!(result.is_err());
        assert!(matches!(result.unwrap_err(), AuthError::InvalidEmail(_)));
    }

    #[test]
    fn test_register_weak_password() {
        let mut service = AuthService::new();
        let result = service.register(
            "test@example.com".to_string(),
            "weak".to_string()
        );

        assert!(result.is_err());
        assert!(matches!(result.unwrap_err(), AuthError::WeakPassword(_)));
    }

    #[test]
    fn test_register_duplicate_user() {
        let mut service = AuthService::new();
        service.register(
            "test@example.com".to_string(),
            "password123".to_string()
        ).unwrap();

        let result = service.register(
            "test@example.com".to_string(),
            "password456".to_string()
        );

        assert!(result.is_err());
        assert!(matches!(result.unwrap_err(), AuthError::UserExists(_)));
    }

    #[test]
    fn test_login_success() {
        let mut service = AuthService::new();
        service.register(
            "test@example.com".to_string(),
            "password123".to_string()
        ).unwrap();

        let result = service.login(
            "test@example.com".to_string(),
            "password123".to_string()
        );

        assert!(result.is_ok());
        let session = result.unwrap();
        assert!(session.is_valid());
    }

    #[test]
    fn test_login_wrong_password() {
        let mut service = AuthService::new();
        service.register(
            "test@example.com".to_string(),
            "password123".to_string()
        ).unwrap();

        let result = service.login(
            "test@example.com".to_string(),
            "wrong_password".to_string()
        );

        assert!(result.is_err());
        assert!(matches!(result.unwrap_err(), AuthError::InvalidCredentials));
    }

    #[test]
    fn test_login_user_not_found() {
        let mut service = AuthService::new();
        let result = service.login(
            "nonexistent@example.com".to_string(),
            "password123".to_string()
        );

        assert!(result.is_err());
        assert!(matches!(result.unwrap_err(), AuthError::UserNotFound(_)));
    }

    #[test]
    fn test_validate_session_success() {
        let mut service = AuthService::new();
        service.register(
            "test@example.com".to_string(),
            "password123".to_string()
        ).unwrap();

        let session = service.login(
            "test@example.com".to_string(),
            "password123".to_string()
        ).unwrap();

        let result = service.validate_session(&session.token);
        assert!(result.is_ok());
        assert_eq!(result.unwrap().email, "test@example.com");
    }

    #[test]
    fn test_validate_session_invalid_token() {
        let service = AuthService::new();
        let result = service.validate_session("invalid_token");

        assert!(result.is_err());
        assert!(matches!(result.unwrap_err(), AuthError::InvalidSession(_)));
    }

    #[test]
    fn test_logout_success() {
        let mut service = AuthService::new();
        service.register(
            "test@example.com".to_string(),
            "password123".to_string()
        ).unwrap();

        let session = service.login(
            "test@example.com".to_string(),
            "password123".to_string()
        ).unwrap();

        let result = service.logout(&session.token);
        assert!(result.is_ok());

        // Session should be invalid after logout
        let validate_result = service.validate_session(&session.token);
        assert!(validate_result.is_err());
    }

    #[test]
    fn test_active_sessions_count() {
        let mut service = AuthService::new();
        service.register(
            "test@example.com".to_string(),
            "password123".to_string()
        ).unwrap();

        assert_eq!(service.active_sessions(), 0);

        service.login(
            "test@example.com".to_string(),
            "password123".to_string()
        ).unwrap();

        assert_eq!(service.active_sessions(), 1);
    }
}
