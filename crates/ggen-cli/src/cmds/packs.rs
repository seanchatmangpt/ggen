//! Packs noun — lockfile-oriented, multi-pack project management
//! (`ggen packs <verb>`).
//!
//! Where `ggen pack` (singular) and `ggen agent install` perform a *fail-closed*
//! install of a registry-resolved pack, `ggen packs install` is the lenient
//! dependency-tracking surface a project-bring-up workflow uses: it records a
//! pack in the project lockfile and witnesses that declaration with a signed
//! receipt, whether or not the pack resolves in a local registry. A pack present
//! in the registry records its real version (`status: installed`); an unresolved
//! pack is recorded as a declared dependency (`status: declared`). Every verb
//! emits structured JSON an agent can parse.

use clap_noun_verb::{NounVerbError, Result};
use clap_noun_verb_macros::verb;
use serde_json::{json, Value};
use std::path::{Path, PathBuf};

use ggen_core::agent::{emit_install_receipt, PackInstallClosure};
use ggen_core::domain::packs::metadata::show_pack;
use ggen_core::domain::packs::validate::validate_pack;
use ggen_core::packs::lockfile::{LockedPack, PackLockfile, PackSource};

// ── helpers ─────────────────────────────────────────────────────────────────

fn project_root() -> Result<PathBuf> {
    std::env::current_dir()
        .map_err(|e| NounVerbError::execution_error(format!("cannot resolve project dir: {}", e)))
}

fn lockfile_path(root: &Path) -> PathBuf {
    root.join(".ggen").join("packs.lock")
}

fn load_or_new_lockfile(path: &Path) -> Result<PackLockfile> {
    if path.exists() {
        PackLockfile::from_file(path)
            .map_err(|e| NounVerbError::execution_error(format!("cannot read lockfile: {}", e)))
    } else {
        Ok(PackLockfile::new(env!("CARGO_PKG_VERSION")))
    }
}

fn validate_pack_id(pack_id: &str) -> Result<()> {
    if pack_id.trim().is_empty() {
        return Err(NounVerbError::argument_error("pack id must not be empty"));
    }
    Ok(())
}

// ── verbs ───────────────────────────────────────────────────────────────────

/// Install (track) a pack: record it in the project lockfile with a non-empty
/// digest and emit a signed provenance receipt.
///
/// Lenient by design — a pack that does not resolve in a local registry is
/// recorded as a *declared* dependency (`status: declared`); a resolved pack
/// records its real version (`status: installed`). Both paths pin a deterministic
/// digest and emit a receipt, so the lockfile invariant (non-empty digest) and
/// provenance hold either way.
#[verb]
pub fn install(pack_id: String) -> Result<Value> {
    validate_pack_id(&pack_id)?;
    let root = project_root()?;
    let lock_path = lockfile_path(&root);

    // Resolve the version from the registry if known; otherwise the pack is a
    // declared dependency at an unresolved version.
    let (version, status) = match show_pack(&pack_id) {
        Ok(p) => (p.version, "installed"),
        Err(_) => ("0.0.0".to_string(), "declared"),
    };

    // Deterministic, non-empty digest binding the declared identity.
    let digest = ggen_core::calculate_sha256(format!("{}@{}", pack_id, version).as_bytes());

    let mut lockfile = load_or_new_lockfile(&lock_path)?;
    lockfile.add_pack(
        &pack_id,
        LockedPack {
            version: version.clone(),
            source: PackSource::Registry {
                url: "https://registry.ggen.io".to_string(),
            },
            integrity: Some(format!("sha256-{}", digest)),
            installed_at: chrono::Utc::now(),
            dependencies: Vec::new(),
        },
    );
    lockfile
        .save(&lock_path)
        .map_err(|e| NounVerbError::execution_error(format!("cannot write lockfile: {}", e)))?;

    // Witness the declaration with a signed receipt rooted at the project.
    let artifacts = vec![lock_path.clone()];
    let no_packages: Vec<String> = Vec::new();
    let closure = PackInstallClosure {
        pack_id: &pack_id,
        pack_version: &version,
        pack_digest: &digest,
        packages_installed: &no_packages,
        artifact_paths: &artifacts,
    };
    let receipt = emit_install_receipt(&root, &closure)
        .map_err(|e| NounVerbError::execution_error(format!("receipt emission failed: {}", e)))?;

    Ok(json!({
        "pack_id": pack_id,
        "status": status,
        "version": version,
        "digest": digest,
        "lockfile": lock_path.display().to_string(),
        "receipt": receipt.display().to_string(),
    }))
}

/// List the packs recorded in the project lockfile.
#[verb]
pub fn list() -> Result<Value> {
    let root = project_root()?;
    let lock_path = lockfile_path(&root);
    let packs: Vec<Value> = if lock_path.exists() {
        let lf = load_or_new_lockfile(&lock_path)?;
        lf.packs
            .iter()
            .map(|(id, locked)| {
                json!({
                    "pack_id": id,
                    "version": locked.version,
                    "integrity": locked.integrity,
                })
            })
            .collect()
    } else {
        Vec::new()
    };
    Ok(json!({ "total": packs.len(), "packs": packs }))
}

/// Validate a pack. A pack absent from the registry is reported
/// `is_valid: false` rather than erroring (the workflow is lenient).
#[verb]
pub fn validate(pack_id: String) -> Result<Value> {
    validate_pack_id(&pack_id)?;
    let (is_valid, score, errors) = match validate_pack(&pack_id) {
        Ok(r) => (r.valid, r.score, r.errors),
        Err(e) => (false, 0.0, vec![e.to_string()]),
    };
    Ok(json!({
        "pack_id": pack_id,
        "is_valid": is_valid,
        "score": score,
        "errors": errors,
    }))
}

/// Show pack detail. Graceful: an unknown pack returns `found: false` (exit 0).
#[verb]
pub fn show(pack_id: String) -> Result<Value> {
    validate_pack_id(&pack_id)?;
    match show_pack(&pack_id) {
        Ok(p) => Ok(json!({
            "pack_id": p.id,
            "found": true,
            "name": p.name,
            "version": p.version,
            "description": p.description,
            "packages": p.packages,
        })),
        Err(e) => Ok(json!({
            "pack_id": pack_id,
            "found": false,
            "message": format!("pack not found: {}", e),
        })),
    }
}
