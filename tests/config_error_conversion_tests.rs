#![allow(dead_code, unused_imports, unused_variables, deprecated, clippy::all)]

//! ConfigError Conversion Tests
//!
//! Extracted from `tests/a2a_integration_tests.rs` (docs/jira/v26.7.16/ cleanup pass): these
//! tests verify that `A2aError`/`McpError`/`AgentError` convert properly into
//! `ggen_config::config_lib::ConfigError` via their `From` impls
//! (see `crates/ggen-config/src/domain/error.rs`). They use no mocks/test doubles — real error
//! types, real conversions, real assertions on the resulting error messages.

use ggen_config::domain::error::{A2aError, AgentError, McpError};

#[test]
fn test_error_conversion() {
    // Test that all error types convert properly to domain errors
    let a2a_error = A2aError::Connection("Test connection error".to_string());
    let mcp_error = McpError::ToolNotFound("Test tool not found".to_string());
    let agent_error = AgentError::StartupFailed("Test startup failed".to_string());

    // These should all be convertible to domain errors. Re-pointed off
    // `ggen_core::utils::error::Error` onto `ggen_config::config_lib::ConfigError` — the
    // real destination the ported `From<A2aError|McpError|AgentError>` impls now target
    // (see crates/ggen-config/src/domain/error.rs) — closing the last of the 3 dependencies
    // this test had on ggen-core (docs/jira/v26.7.16/12-OPEN-QUESTIONS.md item 2).
    let domain_error1: ggen_config::config_lib::ConfigError = a2a_error.into();
    let domain_error2: ggen_config::config_lib::ConfigError = mcp_error.into();
    let domain_error3: ggen_config::config_lib::ConfigError = agent_error.into();

    assert!(!domain_error1.to_string().is_empty());
    assert!(!domain_error2.to_string().is_empty());
    assert!(!domain_error3.to_string().is_empty());
}
