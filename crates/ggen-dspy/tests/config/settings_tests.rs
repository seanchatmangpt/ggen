//! Tests for DSPy settings and global configuration

use ggen_dspy::config::{get_dspy_config, init_dspy_config, DspySettings};

#[test]
fn test_global_config_singleton() {
    let config1 = get_dspy_config();
    let config2 = get_dspy_config();

    // Same static reference
    assert!(std::ptr::eq(config1, config2));
}

#[test]
fn test_default_settings() {
    init_dspy_config();
    let config = get_dspy_config();

    assert!(config.default_temperature >= 0.0 && config.default_temperature <= 2.0);
    assert!(config.default_top_p >= 0.0 && config.default_top_p <= 1.0);
    assert!(config.default_max_tokens > 0);
    assert!(config.timeout_seconds > 0);
}

#[test]
fn test_builder_pattern() {
    let settings = DspySettings::new()
        .with_temperature(0.5)
        .with_max_tokens(4096)
        .with_top_p(0.95)
        .with_streaming(true)
        .with_timeout(60)
        .with_usage_tracking(true);

    assert_eq!(settings.default_temperature, 0.5);
    assert_eq!(settings.default_max_tokens, 4096);
    assert_eq!(settings.default_top_p, 0.95);
    assert!(settings.use_streaming);
    assert_eq!(settings.timeout_seconds, 60);
    assert!(settings.track_usage);
}

#[test]
fn test_validation_valid_config() {
    let valid = DspySettings::default();
    assert!(valid.validate().is_ok());
}

#[test]
fn test_validation_invalid_temperature() {
    let invalid = DspySettings::new().with_temperature(3.0);
    assert!(invalid.validate().is_err());
}

#[test]
fn test_validation_invalid_top_p() {
    let invalid = DspySettings::new().with_top_p(1.5);
    assert!(invalid.validate().is_err());
}

#[test]
fn test_validation_invalid_max_tokens() {
    let invalid = DspySettings::new().with_max_tokens(0);
    assert!(invalid.validate().is_err());
}

#[test]
fn test_validation_invalid_timeout() {
    let invalid = DspySettings::new().with_timeout(0);
    assert!(invalid.validate().is_err());
}

#[test]
fn test_llm_config_integration() {
    init_dspy_config();
    let config = get_dspy_config();

    // Should successfully get ggen-ai global config
    let llm_config = config.llm_config();
    assert!(llm_config.provider_name().len() > 0);
}
