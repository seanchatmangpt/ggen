//! Integration tests for runtime validators.

use ggen_poka_yoke::validators::*;

#[test]
fn test_email_validation_valid() {
    let email = Email::new("user@example.com").unwrap();
    assert_eq!(email.as_str(), "user@example.com");

    let email = Email::new("test.user@domain.co.uk").unwrap();
    assert_eq!(email.as_str(), "test.user@domain.co.uk");

    let email = Email::new("admin+tag@service.com").unwrap();
    assert_eq!(email.as_str(), "admin+tag@service.com");
}

#[test]
fn test_email_validation_invalid() {
    assert!(Email::new("").is_err());
    assert!(Email::new("notanemail").is_err());
    assert!(Email::new("@example.com").is_err());
    assert!(Email::new("user@").is_err());
    assert!(Email::new("user").is_err());
}

#[test]
fn test_email_display() {
    let email = Email::new("display@test.com").unwrap();
    assert_eq!(format!("{}", email), "display@test.com");
}

#[test]
fn test_email_into_string() {
    let email = Email::new("convert@test.com").unwrap();
    let s = email.into_string();
    assert_eq!(s, "convert@test.com");
}

#[test]
fn test_url_validation_valid() {
    let url = Url::new("http://example.com").unwrap();
    assert_eq!(url.as_str(), "http://example.com");
    assert!(!url.is_https());

    let url = Url::new("https://secure.example.com").unwrap();
    assert_eq!(url.as_str(), "https://secure.example.com");
    assert!(url.is_https());

    let url = Url::new("https://example.com/path?query=value").unwrap();
    assert!(url.is_https());
}

#[test]
fn test_url_validation_invalid() {
    assert!(Url::new("").is_err());
    assert!(Url::new("example.com").is_err());
    assert!(Url::new("ftp://example.com").is_err());
    assert!(Url::new("not-a-url").is_err());
}

#[test]
fn test_url_display() {
    let url = Url::new("https://display.test").unwrap();
    assert_eq!(format!("{}", url), "https://display.test");
}

#[test]
fn test_range_validation() {
    assert!(validate_range(50, 0, 100).is_ok());
    assert!(validate_range(0, 0, 100).is_ok());
    assert!(validate_range(100, 0, 100).is_ok());
    assert!(validate_range(-50, -100, 100).is_ok());

    assert!(validate_range(-1, 0, 100).is_err());
    assert!(validate_range(101, 0, 100).is_err());
    assert!(validate_range(-101, -100, 100).is_err());
}

#[test]
fn test_length_validation() {
    assert!(validate_length("hello", 1, 10).is_ok());
    assert!(validate_length("hello", 5, 5).is_ok());
    assert!(validate_length("a", 1, 1).is_ok());

    let result = validate_length("hi", 3, 10);
    assert!(result.is_err());
    match result {
        Err(ValidationError::StringTooShort { len, min }) => {
            assert_eq!(len, 2);
            assert_eq!(min, 3);
        }
        _ => panic!("expected StringTooShort error"),
    }

    let result = validate_length("hello world!", 1, 10);
    assert!(result.is_err());
    match result {
        Err(ValidationError::StringTooLong { len, max }) => {
            assert_eq!(len, 12);
            assert_eq!(max, 10);
        }
        _ => panic!("expected StringTooLong error"),
    }
}

#[test]
fn test_pattern_validation() {
    assert!(validate_pattern("test", "test").is_ok());
    assert!(validate_pattern("hello_world", "hello*").is_ok());
    assert!(validate_pattern("hello_world", "*world").is_ok());
    assert!(validate_pattern("hello_world", "hello*world").is_ok());

    assert!(validate_pattern("test", "other").is_err());
    assert!(validate_pattern("hello", "world*").is_err());
    assert!(validate_pattern("hello", "*world").is_err());
}

#[test]
fn test_username_validation_valid() {
    let username = Username::new("user123").unwrap();
    assert_eq!(username.as_str(), "user123");

    let username = Username::new("test_user").unwrap();
    assert_eq!(username.as_str(), "test_user");

    let username = Username::new("user-name").unwrap();
    assert_eq!(username.as_str(), "user-name");

    let username = Username::new("abc").unwrap();
    assert_eq!(username.as_str(), "abc");
}

#[test]
fn test_username_validation_invalid() {
    assert!(Username::new("ab").is_err()); // Too short
    assert!(Username::new("a".repeat(33)).is_err()); // Too long
    assert!(Username::new("user@name").is_err()); // Invalid character
    assert!(Username::new("user name").is_err()); // Space not allowed
    assert!(Username::new("user!").is_err()); // Special char not allowed
}

#[test]
fn test_username_display() {
    let username = Username::new("display_user").unwrap();
    assert_eq!(format!("{}", username), "display_user");
}

#[test]
fn test_password_validation_valid() {
    let password = Password::new("password123!").unwrap();
    assert_eq!(password.as_str(), "password123!");

    let password = Password::new("SecureP@ss1").unwrap();
    assert_eq!(password.as_str(), "SecureP@ss1");

    let password = Password::new("C0mpl3x!Pass").unwrap();
    assert_eq!(password.as_str(), "C0mpl3x!Pass");
}

#[test]
fn test_password_validation_invalid() {
    assert!(Password::new("short1!").is_err()); // Too short
    assert!(Password::new("nodigits!").is_err()); // No digit
    assert!(Password::new("nospecial1").is_err()); // No special char
    assert!(Password::new("NoDigit!").is_err()); // No digit
    assert!(Password::new("1234567890").is_err()); // No special char
}

#[test]
fn test_password_display_masked() {
    let password = Password::new("secret123!").unwrap();
    assert_eq!(format!("{}", password), "********");
}

#[test]
fn test_validation_error_display() {
    let err = ValidationError::EmptyString;
    assert_eq!(format!("{}", err), "empty string not allowed");

    let err = ValidationError::InvalidEmail("bad".to_string());
    assert_eq!(format!("{}", err), "invalid email format: bad");

    let err = ValidationError::OutOfRange {
        value: 150,
        min: 0,
        max: 100,
    };
    assert_eq!(format!("{}", err), "value 150 out of range [0, 100]");
}

#[test]
fn test_validators_serialization() {
    let email = Email::new("test@example.com").unwrap();
    let json = serde_json::to_string(&email).unwrap();
    let deserialized: Email = serde_json::from_str(&json).unwrap();
    assert_eq!(email.as_str(), deserialized.as_str());

    let url = Url::new("https://example.com").unwrap();
    let json = serde_json::to_string(&url).unwrap();
    let deserialized: Url = serde_json::from_str(&json).unwrap();
    assert_eq!(url.as_str(), deserialized.as_str());

    let username = Username::new("testuser").unwrap();
    let json = serde_json::to_string(&username).unwrap();
    let deserialized: Username = serde_json::from_str(&json).unwrap();
    assert_eq!(username.as_str(), deserialized.as_str());
}
