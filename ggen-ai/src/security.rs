//! Security utilities for protecting sensitive data
//!
//! Provides secure handling of API keys and other sensitive strings,
//! ensuring they are never logged or displayed in full.

use serde::{Deserialize, Deserializer, Serialize, Serializer};
use std::fmt;

/// A secure wrapper for sensitive strings like API keys
///
/// This type ensures that sensitive data is never accidentally logged or displayed.
/// When debugging or displaying, it shows only a masked version of the string.
///
/// # Security Features
/// - Debug trait shows masked value (first 4 chars + "...")
/// - Display trait shows masked value
/// - Serialize shows masked value
/// - Clone and comparison work on full value
///
/// # Example
/// ```
/// use ggen_ai::security::SecretString;
///
/// let api_key = SecretString::new("sk-1234567890abcdef".to_string());
/// println!("{}", api_key); // Prints: "sk-1..."
/// println!("{:?}", api_key); // Prints: SecretString("sk-1...")
/// ```
#[derive(Clone, PartialEq, Eq)]
pub struct SecretString(String);

impl SecretString {
    /// Create a new SecretString
    pub fn new(value: String) -> Self {
        Self(value)
    }

    /// Get the underlying value (use with caution!)
    ///
    /// # Security Warning
    /// This exposes the raw secret value. Only use this when you need
    /// to pass the secret to an API client or similar trusted code.
    pub fn expose_secret(&self) -> &str {
        &self.0
    }

    /// Check if the secret is empty
    pub fn is_empty(&self) -> bool {
        self.0.is_empty()
    }

    /// Get the length of the secret
    pub fn len(&self) -> usize {
        self.0.len()
    }

    /// Create a masked version of the secret for display
    ///
    /// Shows first 4 characters followed by "..." for security.
    /// If the secret is shorter than 8 characters, shows fewer chars.
    pub fn mask(&self) -> String {
        if self.0.is_empty() {
            return "[empty]".to_string();
        }

        let chars_to_show = std::cmp::min(4, self.0.len().saturating_sub(4));
        if chars_to_show == 0 {
            return "***".to_string();
        }

        let prefix: String = self.0.chars().take(chars_to_show).collect();
        format!("{}...", prefix)
    }

    /// Convert from a plain string
    pub fn from_str(s: &str) -> Self {
        Self(s.to_string())
    }
}

impl fmt::Debug for SecretString {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "SecretString(\"{}\")", self.mask())
    }
}

impl fmt::Display for SecretString {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.mask())
    }
}

impl Serialize for SecretString {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        // Serialize as masked value to prevent leakage in logs/JSON
        serializer.serialize_str(&self.mask())
    }
}

impl<'de> Deserialize<'de> for SecretString {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: Deserializer<'de>,
    {
        let s = String::deserialize(deserializer)?;
        Ok(SecretString::new(s))
    }
}

impl From<String> for SecretString {
    fn from(s: String) -> Self {
        SecretString::new(s)
    }
}

impl From<&str> for SecretString {
    fn from(s: &str) -> Self {
        SecretString::new(s.to_string())
    }
}

/// Extension trait for masking API keys in error messages and logs
pub trait MaskApiKey {
    /// Mask API key in string, replacing it with masked version
    fn mask_api_key(&self) -> String;
}

impl MaskApiKey for String {
    fn mask_api_key(&self) -> String {
        mask_sensitive_patterns(self)
    }
}

impl MaskApiKey for &str {
    fn mask_api_key(&self) -> String {
        mask_sensitive_patterns(self)
    }
}

/// Mask sensitive patterns in a string
///
/// Detects common API key patterns and masks them:
/// - OpenAI keys (sk-...)
/// - Anthropic keys (sk-ant-...)
/// - Bearer tokens
/// - Generic API keys
fn mask_sensitive_patterns(input: &str) -> String {
    let mut result = input.to_string();

    // Pattern: sk-... (OpenAI and similar)
    let sk_pattern = regex::Regex::new(r"sk-[a-zA-Z0-9_-]{20,}").unwrap();
    result = sk_pattern.replace_all(&result, |caps: &regex::Captures| {
        let matched = caps.get(0).unwrap().as_str();
        format!("{}...", &matched[..4])
    }).to_string();

    // Pattern: sk-ant-... (Anthropic)
    let ant_pattern = regex::Regex::new(r"sk-ant-[a-zA-Z0-9_-]{20,}").unwrap();
    result = ant_pattern.replace_all(&result, |caps: &regex::Captures| {
        let matched = caps.get(0).unwrap().as_str();
        format!("{}...", &matched[..7])
    }).to_string();

    // Pattern: Bearer tokens
    let bearer_pattern = regex::Regex::new(r"Bearer\s+([a-zA-Z0-9_\-\.]+)").unwrap();
    result = bearer_pattern.replace_all(&result, "Bearer [masked]").to_string();

    // Pattern: api_key=... or api-key=... or apikey=...
    let api_key_pattern = regex::Regex::new(r"(?i)(api[_-]?key\s*[=:]\s*)([a-zA-Z0-9_\-\.]+)").unwrap();
    result = api_key_pattern.replace_all(&result, "$1[masked]").to_string();

    result
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_secret_string_masking() {
        let secret = SecretString::new("sk-1234567890abcdef".to_string());
        assert_eq!(secret.mask(), "sk-1...");
        assert_eq!(format!("{}", secret), "sk-1...");
        assert_eq!(format!("{:?}", secret), "SecretString(\"sk-1...\")");
    }

    #[test]
    fn test_secret_string_short() {
        let secret = SecretString::new("abc".to_string());
        assert_eq!(secret.mask(), "***");
    }

    #[test]
    fn test_secret_string_empty() {
        let secret = SecretString::new("".to_string());
        assert_eq!(secret.mask(), "[empty]");
        assert!(secret.is_empty());
    }

    #[test]
    fn test_secret_string_expose() {
        let secret = SecretString::new("sk-1234567890abcdef".to_string());
        assert_eq!(secret.expose_secret(), "sk-1234567890abcdef");
    }

    #[test]
    fn test_secret_string_clone() {
        let secret1 = SecretString::new("sk-1234567890abcdef".to_string());
        let secret2 = secret1.clone();
        assert_eq!(secret1, secret2);
    }

    #[test]
    fn test_mask_openai_key() {
        let input = "Error with API key sk-1234567890abcdefghijklmnop";
        let masked = mask_sensitive_patterns(input);
        assert!(masked.contains("sk-1..."));
        assert!(!masked.contains("sk-1234567890"));
    }

    #[test]
    fn test_mask_anthropic_key() {
        let input = "Using key sk-ant-1234567890abcdefghijklmnop";
        let masked = mask_sensitive_patterns(input);
        assert!(masked.contains("sk-ant-..."));
        assert!(!masked.contains("1234567890"));
    }

    #[test]
    fn test_mask_bearer_token() {
        let input = "Authorization: Bearer abc123.def456.ghi789";
        let masked = mask_sensitive_patterns(input);
        assert!(masked.contains("Bearer [masked]"));
        assert!(!masked.contains("abc123"));
    }

    #[test]
    fn test_mask_api_key_assignment() {
        let input = "api_key=secret123456";
        let masked = mask_sensitive_patterns(input);
        assert!(masked.contains("api_key=[masked]"));
        assert!(!masked.contains("secret123"));
    }

    #[test]
    fn test_mask_api_key_trait() {
        let text = "My key is sk-1234567890abcdefghijklmnop".to_string();
        let masked = text.mask_api_key();
        assert!(masked.contains("sk-1..."));
        assert!(!masked.contains("sk-1234567890"));
    }

    #[test]
    fn test_secret_string_serialization() {
        let secret = SecretString::new("sk-1234567890abcdef".to_string());
        let json = serde_json::to_string(&secret).unwrap();
        assert!(json.contains("sk-1..."));
        assert!(!json.contains("sk-1234567890"));
    }

    #[test]
    fn test_secret_string_from_conversions() {
        let from_string = SecretString::from("test".to_string());
        let from_str = SecretString::from("test");
        assert_eq!(from_string, from_str);
    }
}
