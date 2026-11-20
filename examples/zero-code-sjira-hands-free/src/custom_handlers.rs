//! Hand-written domain logic for every `cnv:CustomBehavior` command.
//!
//! GENERATED ONCE by ggen (`unless_exists: true` — this file is scaffolding,
//! never overwritten by a later `ggen sync run`, so your edits below are
//! safe). Every function here is called directly by `src/generated_cli.rs`;
//! a missing or renamed function is a compile error, not a silent gap. This
//! is the sole consumer handler seam the zero-code compiler ever emits, and
//! only for commands that explicitly opted out of the closed behavior
//! primitives by declaring `cnv:CustomBehavior` in the ontology.
//!
//! These three handlers are thin, fail-closed process adapters over
//! `ggen sjira {admit,frontier,execute}` with real JSON file args.
//!
//! Binary resolution (fail closed):
//! - `SJIRA_BIN` set → invoke `{SJIRA_BIN} <verb> …` (standalone / ggen-sjira)
//! - else `{GGEN_BIN|ggen} sjira <verb> …`

use std::path::{Path, PathBuf};
use std::process::{Command, Stdio};

use clap_noun_verb::{NounVerbError, Result};
use serde_json::{Map, Value};

/// `issue admit` -- Admit one WorkOrder JSON file via ggen sjira admit.
pub fn issue_admit(inputs: Map<String, Value>) -> Result<Value> {
    let file = require_path(&inputs, "file")?;
    run_sjira(&["admit", &path_str(&file)])
}

/// `system frontier` -- Select the ready frontier via ggen sjira frontier.
pub fn system_frontier(inputs: Map<String, Value>) -> Result<Value> {
    let orders = require_path(&inputs, "orders_path")?;
    let mut args = vec![
        "frontier".to_owned(),
        "--orders-path".to_owned(),
        path_str(&orders),
    ];
    push_opt_path_flag(&mut args, &inputs, "events_path", "--events-path");
    push_opt_path_flag(&mut args, &inputs, "evidence_path", "--evidence-path");
    push_opt_path_flag(&mut args, &inputs, "authority_path", "--authority-path");
    let owned: Vec<&str> = args.iter().map(String::as_str).collect();
    run_sjira(&owned)
}

/// `system execute` -- Run the sjira execute loop via ggen sjira execute.
pub fn system_execute(inputs: Map<String, Value>) -> Result<Value> {
    let orders = require_path(&inputs, "orders_path")?;
    let ledger = require_path(&inputs, "ledger_path")?;
    let authority = require_path(&inputs, "authority_path")?;
    let mut args = vec![
        "execute".to_owned(),
        "--orders-path".to_owned(),
        path_str(&orders),
        "--ledger-path".to_owned(),
        path_str(&ledger),
        "--authority-path".to_owned(),
        path_str(&authority),
    ];
    if let Some(max) = inputs.get("max").and_then(Value::as_u64) {
        args.push("--max".to_owned());
        args.push(max.to_string());
    }
    push_opt_path_flag(&mut args, &inputs, "lease_path", "--lease-path");
    push_opt_path_flag(
        &mut args,
        &inputs,
        "epoch_watermark_path",
        "--epoch-watermark-path",
    );
    push_opt_path_flag(
        &mut args,
        &inputs,
        "epoch_extensions_path",
        "--epoch-extensions-path",
    );
    let owned: Vec<&str> = args.iter().map(String::as_str).collect();
    run_sjira(&owned)
}

// ── process seam ──────────────────────────────────────────────────────────

/// Resolve and invoke the real sjira CLI. Fail closed on missing binary,
/// spawn failure, non-zero exit, empty stdout, or non-JSON stdout.
fn run_sjira(verb_and_args: &[&str]) -> Result<Value> {
    let (program, prefix) = resolve_sjira_invocation()?;
    let mut command = Command::new(&program);
    for p in &prefix {
        command.arg(p);
    }
    for a in verb_and_args {
        command.arg(a);
    }
    // Compact JSON so stdout parses without depending on pretty-print.
    command.arg("--format").arg("json");
    command.stdin(Stdio::null()).stdout(Stdio::piped()).stderr(Stdio::piped());

    let output = command.output().map_err(|e| {
        NounVerbError::execution_error(format!(
            "REFUSED:SJIRA_HANDS_FREE: cannot spawn `{program}`: {e}"
        ))
    })?;

    let stdout = String::from_utf8_lossy(&output.stdout).trim().to_owned();
    let stderr = String::from_utf8_lossy(&output.stderr).trim().to_owned();

    if !output.status.success() {
        let detail = if !stderr.is_empty() {
            stderr
        } else if !stdout.is_empty() {
            stdout
        } else {
            format!("exit {}", output.status)
        };
        return Err(NounVerbError::execution_error(format!(
            "REFUSED:SJIRA_HANDS_FREE: ggen sjira failed: {detail}"
        )));
    }

    if stdout.is_empty() {
        return Err(NounVerbError::execution_error(format!(
            "REFUSED:SJIRA_HANDS_FREE: empty stdout from ggen sjira (stderr={stderr})"
        )));
    }

    serde_json::from_str(&stdout).map_err(|e| {
        NounVerbError::execution_error(format!(
            "REFUSED:SJIRA_HANDS_FREE: non-JSON stdout from ggen sjira: {e}; stdout={stdout:?}; stderr={stderr:?}"
        ))
    })
}

/// `(program, prefix_args)` — either (`ggen`, `["sjira"]`) or (`SJIRA_BIN`, `[]`).
fn resolve_sjira_invocation() -> Result<(String, Vec<String>)> {
    if let Ok(bin) = std::env::var("SJIRA_BIN") {
        let bin = bin.trim().to_owned();
        if bin.is_empty() {
            return Err(NounVerbError::execution_error(
                "REFUSED:SJIRA_HANDS_FREE: SJIRA_BIN is set but empty",
            ));
        }
        ensure_executable(&bin)?;
        return Ok((bin, Vec::new()));
    }

    let program = std::env::var("GGEN_BIN")
        .ok()
        .map(|s| s.trim().to_owned())
        .filter(|s| !s.is_empty())
        .unwrap_or_else(|| "ggen".to_owned());
    ensure_executable(&program)?;
    Ok((program, vec!["sjira".to_owned()]))
}

fn ensure_executable(program: &str) -> Result<()> {
    let path = Path::new(program);
    if path.components().count() > 1 || program.contains('/') || program.contains('\\') {
        if !path.is_file() {
            return Err(NounVerbError::execution_error(format!(
                "REFUSED:SJIRA_HANDS_FREE: binary not found at {program}"
            )));
        }
    }
    Ok(())
}

// ── input helpers ─────────────────────────────────────────────────────────

fn require_path(inputs: &Map<String, Value>, key: &str) -> Result<PathBuf> {
    let raw = inputs.get(key).ok_or_else(|| {
        NounVerbError::execution_error(format!(
            "REFUSED:SJIRA_HANDS_FREE: missing required argument `{key}`"
        ))
    })?;
    match raw {
        Value::String(s) if !s.is_empty() => Ok(PathBuf::from(s)),
        other => Err(NounVerbError::execution_error(format!(
            "REFUSED:SJIRA_HANDS_FREE: `{key}` must be a non-empty path string, got {other}"
        ))),
    }
}

fn push_opt_path_flag(
    args: &mut Vec<String>,
    inputs: &Map<String, Value>,
    key: &str,
    flag: &str,
) {
    if let Some(Value::String(s)) = inputs.get(key) {
        if !s.is_empty() {
            args.push(flag.to_owned());
            args.push(s.clone());
        }
    }
}

fn path_str(path: &Path) -> String {
    path.to_string_lossy().into_owned()
}

#[cfg(test)]
mod proofs {
    use super::*;
    use serde_json::json;

    #[test]
    fn issue_admit_refuses_missing_required_arg_fail_closed() {
        let err = issue_admit(Map::new()).expect_err("missing file must refuse before spawn");
        assert!(
            err.to_string().contains("missing required argument `file`"),
            "{err}"
        );
    }

    #[test]
    fn system_frontier_refuses_missing_orders_path() {
        let err =
            system_frontier(Map::new()).expect_err("missing orders_path must refuse before spawn");
        assert!(
            err.to_string()
                .contains("missing required argument `orders_path`"),
            "{err}"
        );
    }

    #[test]
    fn system_execute_refuses_missing_required_paths() {
        let err = system_execute(Map::new()).expect_err("missing paths must refuse");
        assert!(
            err.to_string().contains("missing required argument"),
            "{err}"
        );
    }

    #[test]
    fn empty_path_string_refuses_fail_closed() {
        let err = issue_admit([("file".to_owned(), json!(""))].into_iter().collect())
            .expect_err("empty path string must refuse");
        assert!(
            err.to_string().contains("non-empty path string"),
            "{err}"
        );
    }
}
