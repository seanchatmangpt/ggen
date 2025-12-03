//! Utility CLI commands - clap-noun-verb v5.3.0 Migration
//!
//! FMEA (Failure Mode and Effects Analysis) and other utility commands using
//! the v5.3.0 #[verb("verb_name", "noun")] pattern.
//!
//! ## Architecture: Three-Layer Pattern
//!
//! - **Layer 3 (CLI)**: Input validation, output formatting, structured JSON responses
//! - **Layer 2 (Integration)**: Async coordination, resource management
//! - **Layer 1 (Domain)**: Pure business logic from ggen_domain::utils

use clap_noun_verb::Result;
use clap_noun_verb_macros::verb;
use serde::Serialize;
use std::collections::HashMap;

pub mod fmea;

// ============================================================================
// Output Types
// ============================================================================

#[derive(Serialize)]
struct DoctorOutput {
    checks_passed: usize,
    checks_failed: usize,
    warnings: usize,
    results: Vec<CheckResult>,
    overall_status: String,
}

#[derive(Serialize)]
struct CheckResult {
    name: String,
    status: String,
    message: Option<String>,
}

#[derive(Serialize)]
struct EnvOutput {
    variables: HashMap<String, String>,
    total: usize,
}

// ============================================================================
// Verb Functions
// ============================================================================

/// Run system diagnostics
#[verb("doctor", "utils")]
fn doctor(all: bool, _fix: bool, format: Option<String>) -> Result<DoctorOutput> {
    let format = format.unwrap_or_else(|| "table".to_string());
    use ggen_domain::utils::{execute_doctor, DoctorInput};

    let input = DoctorInput {
        verbose: all,
        check: None,
        env: format == "env",
    };

    let result = crate::runtime::block_on(async move {
        Ok(execute_doctor(input).await.map_err(|e| {
            ggen_utils::error::Error::new(&format!("System diagnostics failed: {}", e))
        })?)
    })
    .map_err(|e: ggen_utils::Error| clap_noun_verb::NounVerbError::execution_error(e.to_string()))?
    .map_err(|e: ggen_utils::Error| {
        clap_noun_verb::NounVerbError::execution_error(e.to_string())
    })?;

    let results = result
        .checks
        .into_iter()
        .map(|r| CheckResult {
            name: r.name,
            status: format!("{:?}", r.status),
            message: Some(r.message),
        })
        .collect::<Vec<_>>();

    let checks_passed = results.iter().filter(|r| r.status == "Ok").count();
    let checks_failed = results.iter().filter(|r| r.status == "Error").count();
    let warnings = results.iter().filter(|r| r.status == "Warning").count();

    let overall_status = if checks_failed == 0 {
        "healthy".to_string()
    } else {
        "needs attention".to_string()
    };

    Ok(DoctorOutput {
        checks_passed,
        checks_failed,
        warnings,
        results,
        overall_status,
    })
}

/// Manage environment variables
#[verb("env", "utils")]
fn env(list: bool, get: Option<String>, set: Option<String>, _system: bool) -> Result<EnvOutput> {
    use ggen_domain::utils::{execute_env_get, execute_env_list, execute_env_set};

    let variables = if list || (get.is_none() && set.is_none()) {
        // List all GGEN_ variables
        crate::runtime::block_on(async move {
            Ok(execute_env_list().await.map_err(|e| {
                ggen_utils::error::Error::new(&format!("Failed to list environment: {}", e))
            })?)
        })
        .map_err(|e: ggen_utils::Error| {
            clap_noun_verb::NounVerbError::execution_error(e.to_string())
        })?
        .map_err(|e: ggen_utils::Error| {
            clap_noun_verb::NounVerbError::execution_error(e.to_string())
        })?
    } else if let Some(key) = get {
        // Get specific variable
        let key_clone = key.clone();
        let value = crate::runtime::block_on(async move {
            Ok(execute_env_get(key_clone).await.map_err(|e| {
                ggen_utils::error::Error::new(&format!("Failed to get variable: {}", e))
            })?)
        })
        .map_err(|e: ggen_utils::Error| {
            clap_noun_verb::NounVerbError::execution_error(e.to_string())
        })?
        .map_err(|e: ggen_utils::Error| {
            clap_noun_verb::NounVerbError::execution_error(e.to_string())
        })?;

        let mut vars = HashMap::new();
        if let Some(v) = value {
            vars.insert(key, v);
        }
        vars
    } else if let Some(set_str) = set {
        // Set variable (format: KEY=VALUE)
        if let Some((key, value)) = set_str.split_once('=') {
            crate::runtime::block_on(async move {
                Ok::<(), ggen_utils::Error>(
                    execute_env_set(key.to_string(), value.to_string())
                        .await
                        .map_err(|e| {
                            ggen_utils::error::Error::new(&format!("Failed to set variable: {}", e))
                        })?,
                )
            })
            .map_err(|e: ggen_utils::Error| {
                clap_noun_verb::NounVerbError::execution_error(e.to_string())
            })?
            .map_err(|e: ggen_utils::Error| {
                clap_noun_verb::NounVerbError::execution_error(e.to_string())
            })?;

            let mut vars = HashMap::new();
            vars.insert(key.to_string(), value.to_string());
            vars
        } else {
            return Err(clap_noun_verb::NounVerbError::argument_error(
                "Invalid format. Use KEY=VALUE",
            ));
        }
    } else {
        HashMap::new()
    };

    let total = variables.len();

    Ok(EnvOutput { variables, total })
}
