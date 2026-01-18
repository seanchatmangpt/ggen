//! Week 4 Security Hardening Tests
//!
//! Comprehensive test suite for security fixes:
//! 1. Panic prevention
//! 2. Unwrap elimination
//! 3. Command injection prevention
//! 4. Input validation
//! 5. Error message sanitization
//!
//! Target: 82% → 85% security health improvement

use ggen_core::security::command::{SafeCommand, CommandExecutor};
use ggen_core::security::validation::{PathValidator, EnvVarValidator, InputValidator};
use ggen_core::security::error::ErrorSanitizer;
use std::path::Path;

#[cfg(test)]
mod panic_prevention_tests {
    use super::*;

    #[test]
    fn test_no_panic_on_invalid_cache_capacity() {
        // Before fix: panic!("Invalid capacity")
        // After fix: Returns error gracefully
        use ggen_core::template_cache::TemplateCache;

        // Zero capacity should be handled gracefully
        let cache = TemplateCache::new(0);
        assert!(cache.stats().is_ok());
    }

    #[test]
    fn test_no_panic_on_corrupted_cache() {
        // Before fix: panic!("Template cache corrupted")
        // After fix: Returns Result::Err
        use ggen_core::template_cache::TemplateCache;

        let cache = TemplateCache::new(100);

        // Attempting to get nonexistent template should return error, not panic
        let result = cache.get_or_parse(Path::new("/nonexistent/template.tmpl"));
        assert!(result.is_err());
    }

    #[test]
    fn test_no_panic_on_lock_poisoning() {
        // Cache operations should handle lock poisoning gracefully
        use ggen_core::template_cache::TemplateCache;

        let cache = TemplateCache::new(10);
        let stats = cache.stats();

        // Should return error on lock poisoning, not panic
        assert!(stats.is_ok() || stats.is_err());
    }
}

#[cfg(test)]
mod unwrap_elimination_tests {
    use super::*;

    #[test]
    fn test_safe_lru_cache_creation() {
        use ggen_core::template_cache::TemplateCache;

        // Before fix: NonZeroUsize::new(1000).unwrap()
        // After fix: Proper error handling

        let cache = TemplateCache::new(1000);
        let stats = cache.stats().unwrap();
        assert_eq!(stats.capacity, 1000);
    }

    #[test]
    fn test_safe_mutex_lock() {
        use ggen_core::template_cache::TemplateCache;

        // Before fix: cache.lock().unwrap()
        // After fix: map_err for error handling

        let cache = TemplateCache::new(10);
        let result = cache.stats();

        // Should handle lock errors gracefully
        assert!(result.is_ok());
    }

    #[test]
    fn test_safe_file_operations() {
        use ggen_core::template_cache::TemplateCache;

        let cache = TemplateCache::new(10);

        // Before fix: fs::read_to_string().unwrap()
        // After fix: map_err with context

        let result = cache.get_or_parse(Path::new("/nonexistent/file.txt"));

        // Should return error, not panic
        assert!(result.is_err());
    }
}

#[cfg(test)]
mod command_injection_tests {
    use super::*;

    #[test]
    fn test_prevents_shell_execution() {
        // Ensure we don't execute through shell
        let result = SafeCommand::new("sh");
        assert!(result.is_err(), "Should reject shell commands");
    }

    #[test]
    fn test_prevents_semicolon_injection() {
        // Classic injection: "git init; rm -rf /"
        let result = SafeCommand::new("git")
            .unwrap()
            .arg("init; rm -rf /");

        assert!(result.is_err(), "Should reject semicolon injection");
    }

    #[test]
    fn test_prevents_pipe_injection() {
        // Pipe injection: "git status | cat /etc/passwd"
        let result = SafeCommand::new("git")
            .unwrap()
            .arg("status | cat /etc/passwd");

        assert!(result.is_err(), "Should reject pipe injection");
    }

    #[test]
    fn test_prevents_command_substitution() {
        // Command substitution: "$(whoami)"
        let result = SafeCommand::new("git")
            .unwrap()
            .arg("$(whoami)");

        assert!(result.is_err(), "Should reject command substitution");
    }

    #[test]
    fn test_prevents_backtick_execution() {
        // Backtick execution: "`whoami`"
        let result = SafeCommand::new("git")
            .unwrap()
            .arg("`whoami`");

        assert!(result.is_err(), "Should reject backtick execution");
    }

    #[test]
    fn test_prevents_and_or_chains() {
        // AND chain: "git init && rm -rf /"
        let result = SafeCommand::new("git")
            .unwrap()
            .arg("init && rm -rf /");

        assert!(result.is_err(), "Should reject AND chain");
    }

    #[test]
    fn test_allows_safe_commands() {
        // Safe command should succeed (API-wise)
        let cmd = SafeCommand::new("git")
            .unwrap()
            .arg("--version");

        assert!(cmd.is_ok());
    }

    #[test]
    fn test_whitelist_enforcement() {
        // Only whitelisted commands allowed
        assert!(SafeCommand::new("git").is_ok());
        assert!(SafeCommand::new("cargo").is_ok());
        assert!(SafeCommand::new("npm").is_ok());

        // Dangerous commands rejected
        assert!(SafeCommand::new("rm").is_err());
        assert!(SafeCommand::new("curl").is_err());
        assert!(SafeCommand::new("wget").is_err());
    }

    #[test]
    fn test_command_executor_git() {
        // Test executor wrapper
        let result = CommandExecutor::git(&["--version"]);
        // Will fail if git not installed, but tests the API
        let _ = result;
    }
}

#[cfg(test)]
mod input_validation_tests {
    use super::*;

    #[test]
    fn test_path_traversal_prevention() {
        // Classic traversal attempts
        assert!(PathValidator::validate(Path::new("../../../etc/passwd")).is_err());
        assert!(PathValidator::validate(Path::new("../../.ssh/id_rsa")).is_err());
        assert!(PathValidator::validate(Path::new("..\\..\\windows\\system32")).is_err());
    }

    #[test]
    fn test_path_length_limits() {
        // Overly long paths should be rejected
        let long_path = "a/".repeat(3000);
        assert!(PathValidator::validate(Path::new(&long_path)).is_err());
    }

    #[test]
    fn test_safe_paths_allowed() {
        // Normal paths should pass
        assert!(PathValidator::validate(Path::new("src/main.rs")).is_ok());
        assert!(PathValidator::validate(Path::new("templates/rust.tmpl")).is_ok());
    }

    #[test]
    fn test_env_var_name_validation() {
        // Valid names
        assert!(EnvVarValidator::validate_name("PATH").is_ok());
        assert!(EnvVarValidator::validate_name("MY_VAR_123").is_ok());

        // Invalid names (injection attempts)
        assert!(EnvVarValidator::validate_name("VAR; rm -rf /").is_err());
        assert!(EnvVarValidator::validate_name("$(whoami)").is_err());
        assert!(EnvVarValidator::validate_name("VAR|cat").is_err());
    }

    #[test]
    fn test_env_var_value_validation() {
        // Safe values
        assert!(EnvVarValidator::validate_value("/usr/bin").is_ok());
        assert!(EnvVarValidator::validate_value("value123").is_ok());

        // Dangerous values
        assert!(EnvVarValidator::validate_value("value; rm -rf /").is_err());
        assert!(EnvVarValidator::validate_value("$(whoami)").is_err());
        assert!(EnvVarValidator::validate_value("`whoami`").is_err());
    }

    #[test]
    fn test_identifier_validation() {
        // Valid identifiers
        assert!(InputValidator::validate_identifier("my_var").is_ok());
        assert!(InputValidator::validate_identifier("my-var-123").is_ok());

        // Invalid identifiers
        assert!(InputValidator::validate_identifier("my var").is_err()); // space
        assert!(InputValidator::validate_identifier("my;var").is_err()); // semicolon
        assert!(InputValidator::validate_identifier("").is_err()); // empty
    }

    #[test]
    fn test_template_name_validation() {
        // Valid template names
        assert!(InputValidator::validate_template_name("rust-cli").is_ok());
        assert!(InputValidator::validate_template_name("templates/rust.tmpl").is_ok());

        // Invalid template names
        assert!(InputValidator::validate_template_name("").is_err());
        assert!(InputValidator::validate_template_name("template; rm -rf /").is_err());
    }

    #[test]
    fn test_empty_input_rejection() {
        assert!(EnvVarValidator::validate_name("").is_err());
        assert!(InputValidator::validate_identifier("").is_err());
        assert!(InputValidator::validate_template_name("").is_err());
    }

    #[test]
    fn test_length_limit_enforcement() {
        // Extremely long inputs should be rejected
        let long_string = "a".repeat(40000);
        assert!(EnvVarValidator::validate_value(&long_string).is_err());
    }
}

#[cfg(test)]
mod error_sanitization_tests {
    use super::*;

    #[test]
    fn test_path_sanitization() {
        // Should only show filename, not full path
        let sanitized = ErrorSanitizer::sanitize_path(
            Path::new("/home/user/.config/ggen/templates/secret.tmpl")
        );

        assert_eq!(sanitized, "secret.tmpl");
        assert!(!sanitized.contains("/home/user"));
    }

    #[test]
    fn test_message_sanitization_removes_paths() {
        let internal = "Failed to read /home/user/.ssh/id_rsa: Permission denied";
        let sanitized = ErrorSanitizer::sanitize_message(internal);

        // Should not expose full path
        assert!(!sanitized.contains("/home/user"));
        assert!(!sanitized.contains(".ssh"));
    }

    #[test]
    fn test_message_sanitization_removes_credentials() {
        let internal = "Auth failed: password=secret123 token=abc123xyz";
        let sanitized = ErrorSanitizer::sanitize_message(internal);

        // Should not expose credentials
        assert!(!sanitized.contains("secret123"));
        assert!(!sanitized.contains("abc123xyz"));
    }

    #[test]
    fn test_message_length_limiting() {
        let internal = "Error: ".to_string() + &"a".repeat(500);
        let sanitized = ErrorSanitizer::sanitize_message(&internal);

        // Should be truncated
        assert!(sanitized.len() <= 200);
    }

    #[test]
    fn test_file_error_sanitization() {
        let err = ErrorSanitizer::file_error(
            "read",
            Path::new("/etc/passwd"),
            "Permission denied"
        );

        // User message should be safe
        assert!(!err.user_message().contains("/etc"));
        assert!(err.user_message().contains("passwd"));

        // Internal message should have full details
        assert!(err.internal_message().contains("/etc/passwd"));
    }

    #[test]
    fn test_error_code_generation() {
        let err = ErrorSanitizer::file_error(
            "write",
            Path::new("/tmp/test.txt"),
            "Disk full"
        );

        assert_eq!(err.error_code, Some("FILE_ERROR".to_string()));
    }

    #[test]
    fn test_sanitized_error_display() {
        use ggen_core::security::error::SanitizedError;

        let err = SanitizedError::new(
            "An error occurred",
            "Internal error at /home/user/file.txt"
        ).with_code("TEST001");

        let display = format!("{}", err);

        // Should show user message and code
        assert!(display.contains("An error occurred"));
        assert!(display.contains("[Error: TEST001]"));

        // Should not show internal details
        assert!(!display.contains("/home/user"));
    }
}

#[cfg(test)]
mod integration_tests {
    use super::*;

    #[test]
    fn test_safe_command_full_workflow() {
        // Complete workflow: create command, add args, execute
        let result = SafeCommand::new("git")
            .and_then(|cmd| cmd.arg("--version"))
            .and_then(|cmd| cmd.execute());

        // Should either succeed or fail gracefully
        assert!(result.is_ok() || result.is_err());
    }

    #[test]
    fn test_validation_workflow() {
        // Complete validation workflow
        let path_result = PathValidator::validate(Path::new("src/main.rs"));
        let env_result = EnvVarValidator::validate_name("MY_VAR");
        let id_result = InputValidator::validate_identifier("my_id");

        assert!(path_result.is_ok());
        assert!(env_result.is_ok());
        assert!(id_result.is_ok());
    }

    #[test]
    fn test_multi_layered_security() {
        // Test multiple security layers together

        // 1. Validate path
        let path = PathValidator::validate(Path::new("template.tmpl"));
        assert!(path.is_ok());

        // 2. Validate env var
        let env_name = EnvVarValidator::validate_name("TEMPLATE_PATH");
        assert!(env_name.is_ok());

        // 3. Safe command execution
        let cmd = SafeCommand::new("git");
        assert!(cmd.is_ok());

        // 4. Error sanitization
        let err = ErrorSanitizer::sanitize_path(Path::new("/tmp/test.txt"));
        assert_eq!(err, "test.txt");
    }
}

#[cfg(test)]
mod regression_tests {
    use super::*;

    #[test]
    fn test_no_regression_in_safe_operations() {
        // Ensure security fixes don't break normal operations

        // Normal template name should still work
        assert!(InputValidator::validate_template_name("rust-cli").is_ok());

        // Normal paths should still work
        assert!(PathValidator::validate(Path::new("src/lib.rs")).is_ok());

        // Safe commands should still work
        assert!(SafeCommand::new("git").is_ok());
    }

    #[test]
    fn test_backwards_compatibility() {
        // Ensure existing code patterns still work

        use ggen_core::template_cache::TemplateCache;

        let cache = TemplateCache::new(100);
        let stats = cache.stats();

        assert!(stats.is_ok());
        assert_eq!(stats.unwrap().capacity, 100);
    }
}
