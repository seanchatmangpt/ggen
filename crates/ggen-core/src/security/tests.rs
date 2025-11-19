//! Internal security module tests

#[cfg(test)]
mod command_tests {
    use crate::security::command::SafeCommand;

    #[test]
    fn test_command_whitelist() {
        assert!(SafeCommand::new("git").is_ok());
        assert!(SafeCommand::new("cargo").is_ok());
        assert!(SafeCommand::new("rm").is_err());
    }
}

#[cfg(test)]
mod validation_tests {
    use crate::security::validation::PathValidator;
    use std::path::Path;

    #[test]
    fn test_path_validation() {
        assert!(PathValidator::validate(Path::new("src/main.rs")).is_ok());
        assert!(PathValidator::validate(Path::new("../../../etc/passwd")).is_err());
    }
}

#[cfg(test)]
mod error_tests {
    use crate::security::error::ErrorSanitizer;
    use std::path::Path;

    #[test]
    fn test_error_sanitization() {
        let sanitized = ErrorSanitizer::sanitize_path(Path::new("/home/user/file.txt"));
        assert_eq!(sanitized, "file.txt");
    }
}
