//! Chicago TDD integration test: AppConfig — real env call, real Result.
//!
//! No mocks, no test doubles. `from_env()` reads real environment variables
//! and falls back to real defaults. Observable state: the returned `AppConfig`
//! fields are non-empty strings.

use bp_config::AppConfig;

/// Verify `AppConfig::from_env()` succeeds and returns non-empty fields.
///
/// This is a real collaborator test: no mocking of env, no patching of
/// `from_env`. The function reads actual env vars (or uses its defaults).
#[test]
fn app_config_from_env_succeeds_and_has_non_empty_bind_addr() {
    // Act — real env call
    let config = AppConfig::from_env().expect("AppConfig::from_env must succeed with defaults");

    // Assert — observable state: non-empty bind address
    assert!(
        !config.bind_addr.is_empty(),
        "bind_addr must be non-empty; got: {:?}",
        config.bind_addr
    );
}

/// Verify that setting BIND_ADDR env var is reflected in the config.
#[test]
fn app_config_from_env_reads_bind_addr_from_environment() {
    // Arrange: real env mutation
    std::env::set_var("BIND_ADDR", "127.0.0.1:7777");

    // Act
    let config = AppConfig::from_env().expect("from_env must succeed");

    // Assert
    assert_eq!(
        config.bind_addr, "127.0.0.1:7777",
        "bind_addr must reflect the BIND_ADDR env var"
    );

    // Cleanup — restore for other tests
    std::env::remove_var("BIND_ADDR");
}
