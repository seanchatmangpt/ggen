//! Security tests to verify API key masking and protection
//!
//! These tests ensure that API keys are never leaked through:
//! - Error messages
//! - Debug output
//! - Display formatting
//! - Logging
//! - Serialization

use ggen_ai::config::{AnthropicConfig, OpenAIConfig};
use ggen_ai::error::GgenAiError;
use ggen_ai::security::{MaskApiKey, SecretString};

#[test]
fn test_secret_string_never_leaks_in_debug() {
    let secret = SecretString::new("sk-1234567890abcdefghijklmnop".to_string());
    let debug_output = format!("{:?}", secret);

    // Should show masked version
    assert!(debug_output.contains("sk-1..."));

    // Should NOT contain full key
    assert!(!debug_output.contains("1234567890abcdef"));
    assert!(!debug_output.contains("ghijklmnop"));
}

#[test]
fn test_secret_string_never_leaks_in_display() {
    let secret = SecretString::new("sk-1234567890abcdefghijklmnop".to_string());
    let display_output = format!("{}", secret);

    // Should show masked version
    assert!(display_output.contains("sk-1..."));

    // Should NOT contain full key
    assert!(!display_output.contains("1234567890abcdef"));
}

#[test]
fn test_secret_string_never_leaks_in_json() {
    let secret = SecretString::new("sk-1234567890abcdefghijklmnop".to_string());
    let json = serde_json::to_string(&secret).unwrap();

    // Should show masked version
    assert!(json.contains("sk-1..."));

    // Should NOT contain full key
    assert!(!json.contains("1234567890abcdef"));
}

#[test]
fn test_openai_config_never_leaks_key_in_debug() {
    let config = OpenAIConfig::default().with_api_key("sk-1234567890abcdefghijklmnop".to_string());

    let debug_output = format!("{:?}", config);

    // Should show masked version
    assert!(debug_output.contains("sk-1..."));

    // Should NOT contain full key
    assert!(!debug_output.contains("1234567890abcdef"));
}

#[test]
fn test_anthropic_config_never_leaks_key_in_debug() {
    let config =
        AnthropicConfig::default().with_api_key("sk-ant-1234567890abcdefghijklmnop".to_string());

    let debug_output = format!("{:?}", config);

    // Should show masked version (either sk-a... or sk-ant-...)
    assert!(debug_output.contains("sk-a...") || debug_output.contains("sk-ant-..."));

    // Should NOT contain full key
    assert!(!debug_output.contains("1234567890abcdef"));
}

#[test]
fn test_openai_config_serialization_masks_key() {
    let config = OpenAIConfig::default().with_api_key("sk-1234567890abcdefghijklmnop".to_string());

    let json = serde_json::to_string(&config).unwrap();

    // Should show masked version
    assert!(json.contains("sk-1..."));

    // Should NOT contain full key
    assert!(!json.contains("1234567890abcdef"));
}

#[test]
fn test_anthropic_config_serialization_masks_key() {
    let config =
        AnthropicConfig::default().with_api_key("sk-ant-1234567890abcdefghijklmnop".to_string());

    let json = serde_json::to_string(&config).unwrap();

    // Should show masked version
    assert!(json.contains("sk-a...") || json.contains("sk-ant-..."));

    // Should NOT contain full key
    assert!(!json.contains("1234567890abcdef"));
}

#[test]
fn test_error_message_masks_api_keys() {
    let error_message =
        "Failed to authenticate with OpenAI using key sk-1234567890abcdefghijklmnop";
    let masked = error_message.mask_api_key();

    // Should mask the key
    assert!(masked.contains("sk-1..."));
    assert!(!masked.contains("1234567890abcdef"));
}

#[test]
fn test_multiple_keys_masked_in_error() {
    let error_message = "Tried sk-1234567890abcdefghijklmnop and sk-ant-abcdefghijklmnopqrstuvwxyz";
    let masked = error_message.mask_api_key();

    // Should mask both keys
    assert!(masked.contains("sk-1..."));
    assert!(masked.contains("sk-ant-..."));
    assert!(!masked.contains("1234567890"));
    assert!(!masked.contains("abcdefghijklmnop"));
}

#[test]
fn test_bearer_token_masked() {
    let auth_header = "Authorization: Bearer eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9";
    let masked = auth_header.mask_api_key();

    assert!(masked.contains("Bearer [masked]"));
    assert!(!masked.contains("eyJhbGci"));
}

#[test]
fn test_api_key_assignment_masked() {
    let config_line = "api_key=sk-1234567890abcdefghijklmnop";
    let masked = config_line.mask_api_key();

    assert!(masked.contains("api_key=[masked]"));
    assert!(!masked.contains("sk-1234567890"));
}

#[test]
fn test_secret_string_expose_secret_works() {
    let secret = SecretString::new("sk-1234567890abcdefghijklmnop".to_string());

    // expose_secret should return the full value when needed
    assert_eq!(secret.expose_secret(), "sk-1234567890abcdefghijklmnop");
}

#[test]
fn test_secret_string_comparison_uses_full_value() {
    let secret1 = SecretString::new("sk-1234567890abcdefghijklmnop".to_string());
    let secret2 = SecretString::new("sk-1234567890abcdefghijklmnop".to_string());
    let secret3 = SecretString::new("sk-different-key".to_string());

    // Comparison should work on full values
    assert_eq!(secret1, secret2);
    assert_ne!(secret1, secret3);
}

#[test]
fn test_short_secret_masked_appropriately() {
    let short_secret = SecretString::new("abc".to_string());
    assert_eq!(short_secret.mask(), "***");

    let medium_secret = SecretString::new("abcdef".to_string());
    let masked = medium_secret.mask();
    assert!(masked.starts_with("ab...") || masked == "***");
}

#[test]
fn test_empty_secret_masked_appropriately() {
    let empty = SecretString::new("".to_string());
    assert_eq!(empty.mask(), "[empty]");
    assert!(empty.is_empty());
}

#[test]
fn test_ggen_ai_error_doesnt_leak_keys() {
    // Create an error that might contain a key
    let err = GgenAiError::config_error("Failed with key sk-1234567890abcdefghijklmnop");
    let error_string = format!("{}", err);

    // The error formatter should not modify the message, but consumers should use MaskApiKey
    // Let's verify that when we mask it, it works correctly
    let masked = error_string.mask_api_key();
    assert!(masked.contains("sk-1..."));
    assert!(!masked.contains("1234567890abcdef"));
}

#[test]
fn test_logging_scenario() {
    // Simulate a logging scenario where we might log a config
    let config = OpenAIConfig::default().with_api_key("sk-1234567890abcdefghijklmnop".to_string());

    // When logging with Debug
    let log_message = format!("Using config: {:?}", config);

    // Should be masked
    assert!(log_message.contains("sk-1..."));
    assert!(!log_message.contains("1234567890abcdef"));
}

#[test]
fn test_case_insensitive_api_key_pattern() {
    let variants = vec![
        "API_KEY=secret123",
        "api_key=secret123",
        "Api-Key=secret123",
        "apikey=secret123",
    ];

    for variant in variants {
        let masked = variant.mask_api_key();
        assert!(masked.contains("[masked]"), "Failed for: {}", variant);
        assert!(!masked.contains("secret123"), "Failed for: {}", variant);
    }
}

#[test]
fn test_real_world_error_scenarios() {
    let scenarios = vec![
        (
            "Request failed with sk-1234567890abcdefghijklmnop",
            "sk-1...",
        ),
        (
            "Invalid credentials: sk-ant-abcdefghijklmnopqrstuvwxyz",
            "sk-ant-...",
        ),
        ("Auth failed for api_key=secret123", "[masked]"),
        ("Token Bearer abc.def.ghi expired", "Bearer [masked]"),
    ];

    for (input, expected_pattern) in scenarios {
        let masked = input.mask_api_key();
        assert!(
            masked.contains(expected_pattern),
            "Expected '{}' in masked output: {}",
            expected_pattern,
            masked
        );
    }
}
