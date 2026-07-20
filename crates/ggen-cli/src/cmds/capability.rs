//! Capability noun — resolve and enable capability surfaces
//! (`ggen capability <verb>`).
//!
//! A capability (e.g. `mcp`, `web`, `devops`) expands to a set of *atomic packs*.
//! `enable` records those packs in the project lockfile so a subsequent
//! `ggen sync` can generate from them; `list` and `inspect` are read-only
//! discovery. This is the capability-oriented entry to the same lockfile the
//! `packs` noun manages, and a natural first step for an agent bringing a project
//! up: pick a capability, enable it, then sync.

use clap_noun_verb::{NounVerbError, Result};
use clap_noun_verb_macros::verb;
use serde_json::{json, Value};
use std::path::PathBuf;

use ggen_marketplace::packs::lockfile::{LockedPack, PackLockfile, PackSource};
use ggen_marketplace::packs_registry::capability_registry::{
    list_capabilities, resolve_capability_to_packs,
};

// ── helpers ─────────────────────────────────────────────────────────────────

fn project_root() -> Result<PathBuf> {
    let dir = std::env::current_dir().map_err(|e| {
        NounVerbError::execution_error(format!("cannot resolve project dir: {}", e))
    })?;
    if !dir.join("ggen.toml").exists() {
        return Err(NounVerbError::argument_error(format!(
            "not a ggen project: no ggen.toml found in {} (run `ggen init` first)",
            dir.display()
        )));
    }
    Ok(dir)
}

/// Resolve the atomic packs for a capability surface, appending a
/// `projection-<proj>` pack when `--projection` is supplied.
///
/// Propagates `resolve_capability_to_packs`'s "unknown surface" error instead
/// of discarding it (GAP-001, `docs/jira/2026-07-17-JTBD-VERIFICATION-DISCOVERED-BUGS.md`)
/// — an unrecognised surface must be a typed refusal, not a silent empty result.
fn atomic_packs_for(
    surface: &str, projection: Option<&str>, runtime: Option<&str>,
) -> Result<Vec<String>> {
    let mut packs = resolve_capability_to_packs(surface, projection, runtime)
        .map_err(NounVerbError::argument_error)?;
    if let Some(p) = projection {
        let projection_pack = format!("projection-{}", p);
        if !packs.contains(&projection_pack) {
            packs.push(projection_pack);
        }
    }
    Ok(packs)
}

fn require_surface(surface: &str) -> Result<()> {
    if surface.trim().is_empty() {
        return Err(NounVerbError::argument_error(
            "capability surface must not be empty",
        ));
    }
    Ok(())
}

// ── verbs ───────────────────────────────────────────────────────────────────

/// Enable a capability: expand it to atomic packs and record them in the project
/// lockfile, returning the expansion as JSON.
#[verb]
pub fn enable(
    #[arg(index = 1)] surface: String, projection: Option<String>, runtime: Option<String>,
) -> Result<Value> {
    require_surface(&surface)?;
    let packs = atomic_packs_for(&surface, projection.as_deref(), runtime.as_deref())?;

    // Reject before touching the lockfile: every atomic pack id -- including
    // the `projection-<projection>` pack `atomic_packs_for` synthesizes from a
    // user-supplied `--projection` value -- must be a well-formed PackageId.
    // `resolve_capability_to_packs` itself only ever returns clean literal
    // ids, but an unsanitized `--projection` value (e.g. containing a `.`)
    // would otherwise reach the same `.ggen/packs.lock` that `ggen policy
    // check`/`validate` read with a strict validator, and crash them (see
    // `crates/ggen-cli/src/cmds/packs.rs::validate_pack_id` for the same gap
    // on the `packs install` path).
    for pid in &packs {
        ggen_marketplace::marketplace::models::PackageId::new(pid).map_err(|e| {
            NounVerbError::argument_error(format!("invalid atomic pack id '{}': {}", pid, e))
        })?;
    }

    // Record each atomic pack as a declared lockfile entry.
    let root = project_root()?;
    let lock_path = root.join(".ggen").join("packs.lock");
    let mut lockfile = if lock_path.exists() {
        PackLockfile::from_file(&lock_path)
            .map_err(|e| NounVerbError::execution_error(format!("cannot read lockfile: {}", e)))?
    } else {
        PackLockfile::new(env!("CARGO_PKG_VERSION"))
    };
    for pid in &packs {
        let digest = crate::utils::sha256_hex(format!("{}@0.0.0", pid).as_bytes());
        lockfile.add_pack(
            pid,
            LockedPack {
                version: "0.0.0".to_string(),
                source: PackSource::Registry {
                    url: "https://registry.ggen.io".to_string(),
                },
                integrity: Some(format!("sha256-{}", digest)),
                installed_at: chrono::Utc::now(),
                dependencies: Vec::new(),
            },
        );
    }
    lockfile
        .save(&lock_path)
        .map_err(|e| NounVerbError::execution_error(format!("cannot write lockfile: {}", e)))?;

    Ok(json!({
        "capability": surface,
        "projection": projection,
        "runtime": runtime,
        "atomic_packs": packs,
        "lockfile": lock_path.display().to_string(),
    }))
}

/// List the known capability surfaces.
#[verb]
pub fn list() -> Result<Value> {
    let caps: Vec<Value> = list_capabilities()
        .into_iter()
        .map(|c| {
            json!({
                "id": c.id,
                "name": c.name,
                "description": c.description,
                "category": c.category,
                "atomic_packs": c.atomic_packs,
            })
        })
        .collect();
    Ok(json!({ "total": caps.len(), "capabilities": caps }))
}

/// Inspect a capability surface: show the atomic packs it expands to.
///
/// Re-enabled (2026-07-18): was disabled because an unknown/typo'd surface
/// silently returned `atomic_packs: []` with exit 0 instead of a typed
/// refusal (GAP-001, `docs/jira/2026-07-17-JTBD-VERIFICATION-DISCOVERED-BUGS.md`).
/// `atomic_packs_for` now propagates `resolve_capability_to_packs`'s "unknown
/// surface" error instead of discarding it, so this is safe to re-enable.
#[verb]
pub fn inspect(#[arg(index = 1)] surface: String) -> Result<Value> {
    require_surface(&surface)?;
    let packs = atomic_packs_for(&surface, None, None)?;
    Ok(json!({
        "capability": surface,
        "atomic_packs": packs,
    }))
}
