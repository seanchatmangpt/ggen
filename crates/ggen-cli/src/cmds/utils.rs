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

/// Manage environment variables
#[verb]
fn env(list: bool, get: Option<String>, set: Option<String>, system: bool) -> Result<EnvOutput> {
    let _ = system; // reserved CLI flag; system-wide env scope not yet implemented
    let variables = run_env(list, get.as_deref(), set.as_deref());
    let total = variables.len();
    Ok(EnvOutput { variables, total })
}

fn run_env(list: bool, get: Option<&str>, set: Option<&str>) -> HashMap<String, String> {
    let mut variables = HashMap::new();

    if let Some(key) = get {
        if let Ok(value) = std::env::var(key) {
            variables.insert(key.to_string(), value);
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

// ============================================================================
// Ontology-generated command reference (crate::generated_commands)
// ============================================================================

/// One row of the ontology-declared CLI command reference.
#[derive(Serialize)]
struct CommandRefEntry {
    label: String,
    description: String,
}

/// Output for `ggen utils commands`.
#[derive(Serialize)]
struct CommandsReferenceOutput {
    commands: Vec<CommandRefEntry>,
    total: usize,
}

/// List the ontology-declared CLI command reference (generated from
/// `.specify/cli-commands.ttl` into `crate::generated_commands`).
#[verb]
fn commands(describe: Option<String>) -> Result<CommandsReferenceOutput> {
    let commands: Vec<CommandRefEntry> = match describe.as_deref() {
        Some(label) => crate::generated_commands::describe_command(label)
            .map(|comment| CommandRefEntry {
                label: label.to_string(),
                description: comment.to_string(),
            })
            .into_iter()
            .collect(),
        None => crate::generated_commands::COMMANDS_REFERENCE
            .iter()
            .filter(|(_, label, _)| !label.is_empty())
            .map(|(_, label, comment)| CommandRefEntry {
                label: (*label).to_string(),
                description: (*comment).to_string(),
            })
            .collect(),
    };
    let total = commands.len();
    Ok(CommandsReferenceOutput { commands, total })
}
