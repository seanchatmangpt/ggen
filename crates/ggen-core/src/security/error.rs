//! Error message sanitization to prevent information disclosure
//!
//! **SECURITY ISSUE 5: Error Message Information Leakage**
//!
//! This module sanitizes error messages to prevent exposing sensitive information
//! such as internal paths, stack traces, or system details to end users.

use ggen_utils::error::Error;
use std::path::Path;

/// Sanitized error that hides internal details from users
#[derive(Debug, Clone)]
pub struct SanitizedError {
    /// User-facing message (safe to display)
    pub user_message: String,
    /// Internal message (for logs only)
    pub internal_message: String,
    /// Error code for support reference
    pub error_code: Option<String>,
}

impl SanitizedError {
    /// Create a new sanitized error
    pub fn new(user_message: impl Into<String>, internal_message: impl Into<String>) -> Self {
        Self {
            user_message: user_message.into(),
            internal_message: internal_message.into(),
            error_code: None,
        }
    }

    /// Add an error code for support reference
    pub fn with_code(mut self, code: impl Into<String>) -> Self {
        self.error_code = Some(code.into());
        self
    }

    /// Get the user-facing message (safe to display)
    pub fn user_message(&self) -> &str {
        &self.user_message
    }

    /// Get the internal message (for logging only)
    pub fn internal_message(&self) -> &str {
        &self.internal_message
    }
}

impl std::fmt::Display for SanitizedError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if let Some(code) = &self.error_code {
            write!(f, "{} [Error: {}]", self.user_message, code)
        } else {
            write!(f, "{}", self.user_message)
        }
    }
}

impl std::error::Error for SanitizedError {}

/// Error sanitizer to remove sensitive information
pub struct ErrorSanitizer;

impl ErrorSanitizer {
    /// Sanitize a file path for user display
    ///
    /// Removes absolute paths and system-specific details
    ///
    /// # Examples
    ///
    /// ```rust
    /// use ggen_core::security::error::ErrorSanitizer;
    /// use std::path::Path;
    ///
    /// // Before: "/home/user/.config/ggen/templates/rust.tmpl"
    /// // After:  "rust.tmpl"
    /// let sanitized = ErrorSanitizer::sanitize_path(
    ///     Path::new("/home/user/.config/ggen/templates/rust.tmpl")
    /// );
    /// assert_eq!(sanitized, "rust.tmpl");
    /// ```
    pub fn sanitize_path(path: &Path) -> String {
        // Only show filename, not full path
        path.file_name()
            .and_then(|n| n.to_str())
            .unwrap_or("<unknown file>")
            .to_string()
    }

    /// Sanitize an error message for user display
    ///
    /// Removes:
    /// - Absolute file paths
    /// - Stack traces
    /// - Environment variables
    /// - System details
    ///
    /// # Examples
    ///
    /// ```rust
    /// use ggen_core::security::error::ErrorSanitizer;
    ///
    /// let internal_error = "Failed to read /home/user/.config/ggen/secret.key: Permission denied";
    /// let sanitized = ErrorSanitizer::sanitize_message(internal_error);
    /// // Returns generic message without path exposure
    /// ```
    pub fn sanitize_message(message: &str) -> String {
        let mut sanitized = message.to_string();

        // Remove absolute paths (Unix)
        sanitized = Self::remove_pattern(&sanitized, r"/(?:home|root|usr|etc|var)/[^\s:]+");

        // Remove absolute paths (Windows)
        sanitized = Self::remove_pattern(&sanitized, r"[A-Za-z]:\\[^\s:]+");

        // Remove environment variables
        sanitized = Self::remove_pattern(&sanitized, r"\$[A-Z_][A-Z0-9_]*");

        // Remove common sensitive patterns
        sanitized = Self::remove_pattern(&sanitized, r"password[=:]\s*\S+");
        sanitized = Self::remove_pattern(&sanitized, r"token[=:]\s*\S+");
        sanitized = Self::remove_pattern(&sanitized, r"key[=:]\s*\S+");

        // Remove stack trace markers
        sanitized = sanitized.replace("at ", "");
        sanitized = sanitized.replace("panicked at", "error:");

        // Limit message length
        if sanitized.len() > 200 {
            sanitized.truncate(197);
            sanitized.push_str("...");
        }

        sanitized
    }

    /// Remove pattern from string using regex
    fn remove_pattern(text: &str, pattern: &str) -> String {
        // Use regex crate for production pattern matching
        match regex::Regex::new(pattern) {
            Ok(re) => re.replace_all(text, "<redacted>").to_string(),
            Err(_) => text.to_string(), // Fallback if regex is invalid
        }
    }

    /// Create a sanitized error from an internal error
    pub fn sanitize_error(internal_error: &Error) -> SanitizedError {
        let internal_msg = internal_error.to_string();
        let user_msg = Self::sanitize_message(&internal_msg);

        SanitizedError::new(
            if user_msg == internal_msg {
                "An error occurred during processing".to_string()
            } else {
                user_msg
            },
            internal_msg,
        )
    }

    /// Create file operation error (sanitized)
    pub fn file_error(operation: &str, path: &Path, error: &str) -> SanitizedError {
        let user_msg = format!(
            "Failed to {} file: {}",
            operation,
            Self::sanitize_path(path)
        );

        let internal_msg = format!(
            "Failed to {} file at {}: {}",
            operation,
            path.display(),
            error
        );

        SanitizedError::new(user_msg, internal_msg).with_code("FILE_ERROR")
    }

    /// Create template error (sanitized)
    pub fn template_error(template: &str, error: &str) -> SanitizedError {
        let user_msg = format!("Template processing failed: {}", template);
        let internal_msg = format!("Template '{}' error: {}", template, error);

        SanitizedError::new(user_msg, internal_msg).with_code("TEMPLATE_ERROR")
    }

    /// Create command error (sanitized)
    pub fn command_error(command: &str, error: &str) -> SanitizedError {
        let user_msg = format!("Command execution failed: {}", command);
        let internal_msg = format!("Command '{}' failed: {}", command, error);

        SanitizedError::new(user_msg, internal_msg).with_code("COMMAND_ERROR")
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_sanitize_path() {
        // Unix paths
        assert_eq!(
            ErrorSanitizer::sanitize_path(Path::new("/home/user/file.txt")),
            "file.txt"
        );

        // Windows paths (skip on Unix as Path interpretation differs)
        #[cfg(windows)]
        assert_eq!(
            ErrorSanitizer::sanitize_path(Path::new("C:\\Users\\User\\file.txt")),
            "file.txt"
        );

        // Relative paths
        assert_eq!(
            ErrorSanitizer::sanitize_path(Path::new("src/main.rs")),
            "main.rs"
        );
    }

    #[test]
    fn test_sanitize_message_removes_paths() {
        let internal = "Failed to read /home/user/.config/ggen/secret.key";
        let sanitized = ErrorSanitizer::sanitize_message(internal);

        // Should not contain full path
        assert!(
            !sanitized.contains("/home/user"),
            "Sanitized message should not contain /home/user, got: {}",
            sanitized
        );
    }

    #[test]
    fn test_sanitize_message_removes_sensitive_data() {
        let internal = "Connection failed: password=secret123 token=abc123";
        let sanitized = ErrorSanitizer::sanitize_message(internal);

        // Should not contain credentials
        assert!(
            !sanitized.contains("secret123"),
            "Sanitized message should not contain secret123, got: {}",
            sanitized
        );
        assert!(
            !sanitized.contains("abc123"),
            "Sanitized message should not contain abc123, got: {}",
            sanitized
        );
    }

    #[test]
    fn test_sanitized_error_display() {
        let err = SanitizedError::new("An error occurred", "Internal error at /home/user/file.txt")
            .with_code("TEST001");

        let display = format!("{}", err);
        assert!(display.contains("An error occurred"));
        assert!(display.contains("[Error: TEST001]"));
        assert!(!display.contains("/home/user"));
    }

    #[test]
    fn test_file_error_sanitization() {
        let err = ErrorSanitizer::file_error("read", Path::new("/etc/passwd"), "Permission denied");

        // User message should be safe
        assert!(!err.user_message().contains("/etc"));
        assert!(err.user_message().contains("passwd"));

        // Internal message should have details
        assert!(err.internal_message().contains("/etc/passwd"));
    }
}
