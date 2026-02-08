//! Utils Commands - clap-noun-verb v3.4.0 Migration
//!
//! This module implements utility commands using the v3.4.0 #[verb] pattern.

use clap_noun_verb::Result;
use clap_noun_verb_macros::verb;
use serde::Serialize;
use std::collections::HashMap;

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

/// Output for setting environment variables
#[derive(Serialize)]
#[allow(dead_code)]
struct EnvSetOutput {
    key: String,
    value: String,
    success: bool,
}

// ============================================================================
// Verb Functions
// ============================================================================

/// Run system diagnostics
#[verb]
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

/// Manage environment variables - Simplified
#[verb]
fn env(
    _list: bool, _get: Option<String>, _set: Option<String>, _system: bool,
) -> Result<EnvOutput> {
    // TODO: Fix compilation issue with environment variables
    // For now, return empty result
    Ok(EnvOutput {
        variables: std::collections::HashMap::new(),
        total: 0,
    })
}
