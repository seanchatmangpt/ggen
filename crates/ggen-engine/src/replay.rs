//! GALL-001 clean-state replay verification for portable semantic-pack receipts.

use std::path::{Component, Path};

use serde::Serialize;
use serde_json::{json, Map, Value};
use sha2::Digest as _;

use crate::{
    error::{AppError, Result},
    portable_receipt::{
        observe_environment_identity, observe_toolchain_identity, PORTABLE_RECEIPT_REL_PATH,
    },
    sync::{sync, SyncOptions},
};

#[derive(Debug, Clone, Serialize)]
pub struct ReplayReport {
    pub source_receipt_sha256: String,
    pub reconstructed_receipt_sha256: String,
    pub replay_receipt_sha256: String,
    pub cleared_consequences: usize,
    pub status: &'static str,
}

fn sha256_prefixed(bytes: &[u8]) -> String {
    format!("sha256:{}", hex::encode(sha2::Sha256::digest(bytes)))
}

fn sha256_value(value: &Value) -> Result<String> {
    Ok(sha256_prefixed(&serde_json::to_vec(value)?))
}

fn portable_receipt_path(root: &Path) -> std::path::PathBuf {
    root.join(PORTABLE_RECEIPT_REL_PATH)
}

fn read_receipt(root: &Path) -> Result<(Vec<u8>, Value)> {
    let bytes = std::fs::read(portable_receipt_path(root)).map_err(|e| {
        AppError::fm_chain(
            16,
            format!(
                "GALL-001 replay requires an existing portable receipt: {e}. Remediation: execute a non-dry-run sync before replay."
            ),
        )
    })?;
    let value = serde_json::from_slice(&bytes)?;
    Ok((bytes, value))
}

fn require_unknown_source_replay(source: &Value) -> Result<()> {
    if source
        .get("replay")
        .and_then(|value| value.get("status"))
        .and_then(Value::as_str)
        != Some("UNKNOWN")
    {
        return Err(AppError::fm_chain(
            16,
            "GALL-001 replay requires replay.status=UNKNOWN; ordinary manufacture cannot self-promote and an already-adjudicated receipt must be re-manufactured before another court.",
        ));
    }
    Ok(())
}

fn validate_subject_selection(receipt: &Value) -> Result<()> {
    let subject = receipt
        .get("subject")
        .and_then(Value::as_object)
        .ok_or_else(|| AppError::fm_chain(17, "GALL-001 receipt has no subject object."))?;
    let pack = subject
        .get("pack")
        .and_then(Value::as_str)
        .unwrap_or("UNKNOWN");
    let version = subject
        .get("version")
        .and_then(Value::as_str)
        .unwrap_or("UNKNOWN");
    let digest = subject
        .get("pack_digest")
        .and_then(Value::as_str)
        .unwrap_or("UNKNOWN");
    if pack == "UNKNOWN" || version == "UNKNOWN" || digest == "UNKNOWN" {
        return Err(AppError::fm_chain(
            17,
            "GALL-001 replay refuses an UNKNOWN pack subject.",
        ));
    }

    let members = receipt
        .pointer("/composition/resolved_packs")
        .and_then(Value::as_array)
        .ok_or_else(|| {
            AppError::fm_chain(
                17,
                "GALL-001 replay requires an explicit resolved top-level pack composition.",
            )
        })?;
    let matches = members
        .iter()
        .filter(|member| {
            member.get("name").and_then(Value::as_str) == Some(pack)
                && member.get("version").and_then(Value::as_str) == Some(version)
                && member.get("digest").and_then(Value::as_str) == Some(digest)
        })
        .count();
    if matches != 1 {
        return Err(AppError::fm_chain(
            17,
            format!(
                "GALL-001 subject-selection refusal: subject {pack}@{version} must identify exactly one member of the bound top-level composition; observed {matches}."
            ),
        ));
    }
    Ok(())
}

fn observe_runtime_identity_matches_source(source: &Value) -> Result<()> {
    let toolchain = serde_json::to_value(observe_toolchain_identity()?)?;
    let environment = serde_json::to_value(observe_environment_identity()?)?;
    if source.get("toolchain") != Some(&toolchain) {
        return Err(AppError::fm_chain(
            17,
            "GALL-001 replay toolchain identity diverged before reconstruction.",
        ));
    }
    if source.get("environment") != Some(&environment) {
        return Err(AppError::fm_chain(
            17,
            "GALL-001 replay environment identity diverged before reconstruction.",
        ));
    }
    Ok(())
}

fn compare_recomputed_identities(source: &Value, replayed: &Value) -> Result<Value> {
    const FIELDS: &[&str] = &[
        "schema",
        "spec",
        "engine",
        "subject",
        "dependencies",
        "composition",
        "graph",
        "work_order",
        "admission",
        "consequences",
        "toolchain",
        "environment",
        "standing",
    ];

    let mut witness = Map::new();
    for field in FIELDS {
        let left = source.get(*field).unwrap_or(&Value::Null);
        let right = replayed.get(*field).unwrap_or(&Value::Null);
        if left != right {
            return Err(AppError::fm_chain(
                17,
                format!(
                    "GALL-001 replay identity diverged at {field}; the clean reconstruction is not the source subject."
                ),
            ));
        }
        witness.insert(
            (*field).to_string(),
            json!({
                "sha256": sha256_value(left)?,
                "equal": true
            }),
        );
    }
    Ok(Value::Object(witness))
}

fn safe_relative_target(target: &str) -> bool {
    let path = Path::new(target);
    !path.is_absolute()
        && path
            .components()
            .all(|component| matches!(component, Component::Normal(_) | Component::CurDir))
}

fn clear_managed_writes(root: &Path, receipt: &Value) -> Result<usize> {
    let consequences = receipt
        .get("consequences")
        .and_then(Value::as_array)
        .ok_or_else(|| {
            AppError::fm_chain(
                16,
                "GALL-001 portable receipt has no consequences array; replay identity is invalid.",
            )
        })?;

    let mut cleared = 0usize;
    for consequence in consequences {
        let target = consequence
            .get("target")
            .and_then(Value::as_str)
            .ok_or_else(|| {
                AppError::fm_chain(16, "GALL-001 consequence is missing a string target.")
            })?;
        let operation = consequence
            .get("operation")
            .and_then(Value::as_str)
            .ok_or_else(|| {
                AppError::fm_chain(16, "GALL-001 consequence is missing a string operation.")
            })?;

        if !safe_relative_target(target) {
            return Err(AppError::fm_chain(
                16,
                format!("GALL-001 refuses unsafe replay target path {target}."),
            ));
        }

        if operation != "MANAGED_WRITE" {
            return Err(AppError::fm_chain(
                16,
                format!(
                    "GALL-001 clean replay currently supports MANAGED_WRITE only; {target} is {operation}. Refusing rather than destructively reconstructing an injected/manual file."
                ),
            ));
        }

        let path = root.join(target);
        match std::fs::remove_file(&path) {
            Ok(()) => cleared += 1,
            Err(e) if e.kind() == std::io::ErrorKind::NotFound => {}
            Err(e) => {
                return Err(AppError::fm_chain(
                    16,
                    format!("GALL-001 cannot clear replay consequence {target}: {e}"),
                ));
            }
        }
    }
    Ok(cleared)
}

/// Execute the real sync path from clean managed-output state. PASS is emitted
/// only after source identity, runtime identity, and clean reconstruction all
/// independently agree.
pub fn verify_project_replay(root: &Path, opts: SyncOptions) -> Result<ReplayReport> {
    if opts.dry_run {
        return Err(AppError::fm_chain(
            16,
            "GALL-001 replay cannot run with dry_run=true; dry-run has no durable consequence.",
        ));
    }

    let (source_bytes, source) = read_receipt(root)?;
    if source.get("standing").and_then(Value::as_str) != Some("ALIVE") {
        return Err(AppError::fm_chain(
            16,
            format!(
                "GALL-001 replay requires source standing ALIVE; observed {:?}.",
                source.get("standing")
            ),
        ));
    }
    require_unknown_source_replay(&source)?;
    validate_subject_selection(&source)?;
    observe_runtime_identity_matches_source(&source)?;

    let cleared = clear_managed_writes(root, &source)?;
    sync(root, opts)?;

    let (reconstructed_bytes, mut reconstructed) = read_receipt(root)?;
    require_unknown_source_replay(&reconstructed)?;
    validate_subject_selection(&reconstructed)?;
    let identity_witness = compare_recomputed_identities(&source, &reconstructed)?;

    let source_digest = sha256_prefixed(&source_bytes);
    let reconstructed_digest = sha256_prefixed(&reconstructed_bytes);
    reconstructed["replay"] = json!({
        "status": "PASS",
        "court": "ggen-engine::replay::verify_project_replay",
        "source_receipt_sha256": source_digest,
        "reconstructed_receipt_sha256": reconstructed_digest,
        "identity": identity_witness
    });
    let replay_bytes = serde_json::to_vec(&reconstructed)?;
    let replay_digest = sha256_prefixed(&replay_bytes);
    std::fs::write(portable_receipt_path(root), replay_bytes)?;

    Ok(ReplayReport {
        source_receipt_sha256: source_digest,
        reconstructed_receipt_sha256: reconstructed_digest,
        replay_receipt_sha256: replay_digest,
        cleared_consequences: cleared,
        status: "PASS",
    })
}
