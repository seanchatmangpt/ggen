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
        execute_doctor(input).await.map_err(|e| {
            ggen_utils::error::Error::new(&format!("System diagnostics failed: {}", e))
        })
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
#[verb]
fn env(
    list: bool, get: Option<String>, set: Option<String>, _system: bool,
) -> Result<EnvOutput> {
    let variables = run_env(list, &get, &set);
    let total = variables.len();
    Ok(EnvOutput { variables, total })
}

fn run_env(list: bool, get: &Option<String>, set: &Option<String>) -> HashMap<String, String> {
    let mut variables = HashMap::new();

    if let Some(key) = get {
        if let Ok(value) = std::env::var(key) {
            variables.insert(key.clone(), value);
        }
    } else if let Some(kv) = set {
        if let Some((key, value)) = kv.split_once('=') {
            std::env::set_var(key, value);
            variables.insert(key.to_string(), value.to_string());
        }
    }

    if list || (get.is_none() && set.is_none()) {
        collect_ggen_env_vars(&mut variables);
    }

    variables
}

fn collect_ggen_env_vars(vars: &mut HashMap<String, String>) {
    for (key, value) in std::env::vars() {
        if key.starts_with("GGEN_") || key.starts_with("RUST_") || key == "HOME" || key == "PATH" {
            vars.insert(key, value);
        }
    }
}
