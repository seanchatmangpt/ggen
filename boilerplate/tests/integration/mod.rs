//! Chicago TDD integration test: AppConfig — real env call, real Result.
//!
//! No mocks, no test doubles. `from_env()` reads real environment variables
//! and falls back to real defaults.

use bp_config::AppConfig;

/// Verify `AppConfig::from_env()` succeeds and returns a non-empty bind_addr.
///
/// Real collaborator: no mocking of env. The function reads actual env vars
/// (or uses its compiled-in defaults).
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
