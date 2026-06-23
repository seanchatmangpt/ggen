//! Chicago TDD for the agent-facing packs facade (`ggen_core::agent`).
//!
//! Real collaborators only: a real `TempDir` registry of `<id>.toml` packs, a
//! real `.ggen/packs.lock`, and real Ed25519-signed receipt files on disk. Every
//! assertion is on observable, externalizable state (file contents, lockfile
//! entries, signature verification) — never on internal flags. Sabotage tests
//! prove the facade is fail-closed: empty digests, absent packs, missing
//! lockfiles, and tampered signatures all surface as typed errors or
//! `is_valid == false`, never a fake success.

use std::fs;
use std::path::{Path, PathBuf};
use std::sync::Mutex;

use ggen_core::agent::{
    emit_install_receipt, AgentError, InstallRequest, PackAgent, PackInstallClosure,
};
use tempfile::TempDir;

/// Serializes the tests that mutate process-global state (`cwd`, `HOME`,
/// `GGEN_PACKS_DIR`). Tests rooted purely via `PackAgent::at_root` need no lock.
static ENV_LOCK: Mutex<()> = Mutex::new(());

// ── Fixtures ────────────────────────────────────────────────────────────────

/// Write a real, loader-compatible pack TOML into `registry_dir`.
fn write_pack(registry_dir: &Path, id: &str, version: &str) {
    let toml = format!(
        r#"[pack]
id = "{id}"
name = "Facade Pack {id}"
version = "{version}"
description = "Real pack fixture for agent_facade_test"
category = "test"
author = "facade-test"
license = "MIT"
production_ready = true
packages = ["{id}-core"]
tags = ["facade", "test"]
keywords = ["{id}"]
"#
    );
    fs::write(registry_dir.join(format!("{id}.toml")), toml).expect("write pack toml");
}

/// Seed a real `.ggen/packs.lock` under `root` with the given packs.
fn seed_lockfile(root: &Path, packs: &[(&str, &str)]) -> PathBuf {
    let mut entries = String::new();
    for (i, (id, version)) in packs.iter().enumerate() {
        if i > 0 {
            entries.push(',');
        }
        entries.push_str(&format!(
            r#"
    "{id}": {{
      "version": "{version}",
      "source": {{ "type": "Registry", "url": "https://registry.ggen.io" }},
      "integrity": "sha256-seeded{id}",
      "installed_at": "2024-01-01T00:00:00Z",
      "dependencies": []
    }}"#
        ));
    }
    let json = format!(
        r#"{{
  "packs": {{{entries}
  }},
  "updated_at": "2024-01-01T00:00:00Z",
  "ggen_version": "6.0.0"
}}"#
    );
    let lock_path = root.join(".ggen").join("packs.lock");
    fs::create_dir_all(lock_path.parent().unwrap()).unwrap();
    fs::write(&lock_path, json).expect("seed lockfile");
    lock_path
}

/// RAII guard restoring the previous working directory on drop.
struct CwdGuard {
    prev: PathBuf,
}
impl CwdGuard {
    fn enter(p: &Path) -> Self {
        let prev = std::env::current_dir().expect("cwd");
        std::env::set_current_dir(p).expect("set cwd");
        Self { prev }
    }
}
impl Drop for CwdGuard {
    fn drop(&mut self) {
        let _ = std::env::set_current_dir(&self.prev);
    }
}

// ── Discovery ───────────────────────────────────────────────────────────────

#[test]
fn capabilities_advertises_full_lifecycle_and_surfaces() {
    let agent = PackAgent::new().expect("agent");
    let caps = agent.capabilities();

    let op_names: Vec<&str> = caps.operations.iter().map(|o| o.name.as_str()).collect();
    for expected in [
        "search", "list", "show", "resolve", "status", "verify", "install", "remove",
    ] {
        assert!(op_names.contains(&expected), "missing operation {expected}");
    }

    // install/remove are flagged mutating; search is not — agents must be able to
    // tell which operations change durable state.
    let install = caps
        .operations
        .iter()
        .find(|o| o.name == "install")
        .unwrap();
    assert!(install.mutating, "install must be flagged mutating");
    let search = caps.operations.iter().find(|o| o.name == "search").unwrap();
    assert!(!search.mutating, "search must be flagged non-mutating");

    // The MCP capability surface is always known.
    assert!(
        caps.surfaces.iter().any(|s| s.id == "mcp"),
        "mcp surface must be advertised"
    );
}

// ── status (read-only, lockfile-rooted) ─────────────────────────────────────

#[test]
fn status_without_lockfile_reports_absent_not_error() {
    let root = TempDir::new().unwrap();
    let status = PackAgent::at_root(root.path()).status().expect("status");
    assert!(!status.lockfile_present);
    assert!(status.installed.is_empty());
}

#[test]
fn status_reads_real_lockfile_entries() {
    let root = TempDir::new().unwrap();
    seed_lockfile(root.path(), &[("alpha", "1.2.3"), ("beta", "0.1.0")]);

    let status = PackAgent::at_root(root.path()).status().expect("status");
    assert!(status.lockfile_present);
    assert_eq!(status.installed.len(), 2);
    let alpha = status
        .installed
        .iter()
        .find(|p| p.pack_id == "alpha")
        .unwrap();
    assert_eq!(alpha.version, "1.2.3");
    assert_eq!(alpha.integrity.as_deref(), Some("sha256-seededalpha"));
}

// ── remove (mutating lockfile, fail-closed) ─────────────────────────────────

#[test]
fn remove_mutates_real_lockfile_and_reports_remaining() {
    let root = TempDir::new().unwrap();
    let lock_path = seed_lockfile(root.path(), &[("alpha", "1.0.0"), ("beta", "2.0.0")]);

    let outcome = PackAgent::at_root(root.path())
        .remove("alpha")
        .expect("remove");
    assert!(outcome.removed);
    assert_eq!(outcome.remaining, vec!["beta".to_string()]);

    // Externalizable evidence: the on-disk lockfile no longer mentions alpha.
    let on_disk = fs::read_to_string(&lock_path).unwrap();
    assert!(
        !on_disk.contains("\"alpha\""),
        "alpha must be gone from disk"
    );
    assert!(on_disk.contains("\"beta\""), "beta must remain on disk");
}

#[test]
fn remove_absent_pack_is_fail_closed_and_preserves_lockfile() {
    let root = TempDir::new().unwrap();
    let lock_path = seed_lockfile(root.path(), &[("beta", "2.0.0")]);
    let before = fs::read_to_string(&lock_path).unwrap();

    let err = PackAgent::at_root(root.path()).remove("ghost").unwrap_err();
    assert!(matches!(err, AgentError::NotInstalled(_)), "got {err:?}");

    // The lockfile is untouched — no partial mutation on a refused removal.
    assert_eq!(fs::read_to_string(&lock_path).unwrap(), before);
}

#[test]
fn remove_without_lockfile_is_fail_closed() {
    let root = TempDir::new().unwrap();
    let err = PackAgent::at_root(root.path()).remove("alpha").unwrap_err();
    assert!(matches!(err, AgentError::NotInstalled(_)), "got {err:?}");
}

#[test]
fn remove_rejects_invalid_pack_id() {
    let root = TempDir::new().unwrap();
    let err = PackAgent::at_root(root.path())
        .remove("bad name!")
        .unwrap_err();
    assert!(matches!(err, AgentError::InvalidRequest(_)), "got {err:?}");
}

// ── receipt emit + verify (real Ed25519 signatures) ─────────────────────────

#[test]
fn receipt_emit_then_verify_roundtrip() {
    let root = TempDir::new().unwrap();
    let install_dir = root.path().join("install");
    fs::create_dir_all(&install_dir).unwrap();
    let packages = vec!["alpha-core".to_string()];
    let artifacts = vec![install_dir.clone()];
    let closure = PackInstallClosure {
        pack_id: "alpha",
        pack_version: "1.0.0",
        pack_digest: "deadbeef",
        packages_installed: &packages,
        artifact_paths: &artifacts,
    };

    let receipt_path = emit_install_receipt(root.path(), &closure).expect("emit");
    assert!(receipt_path.exists(), "receipt file must exist");

    // The receipt carries a real, non-empty signature.
    let body: serde_json::Value =
        serde_json::from_slice(&fs::read(&receipt_path).unwrap()).unwrap();
    assert!(
        !body["signature"].as_str().unwrap_or("").is_empty(),
        "signature must be non-empty"
    );

    // Verification against the key written under the same root succeeds.
    let outcome = PackAgent::at_root(root.path()).verify(&receipt_path);
    assert!(outcome.is_valid, "valid receipt must verify: {outcome:?}");
    assert!(outcome.operation_id.is_some());
    assert!(outcome.reason.is_none());
}

#[test]
fn receipt_emit_refuses_empty_digest() {
    // Sabotage: an install that pinned no digest has nothing lawful to witness.
    let root = TempDir::new().unwrap();
    let packages: Vec<String> = vec![];
    let artifacts: Vec<PathBuf> = vec![];
    let closure = PackInstallClosure {
        pack_id: "alpha",
        pack_version: "1.0.0",
        pack_digest: "   ",
        packages_installed: &packages,
        artifact_paths: &artifacts,
    };
    let err = emit_install_receipt(root.path(), &closure);
    assert!(err.is_err(), "empty-digest receipt must be refused");
}

#[test]
fn verify_tampered_signature_is_invalid() {
    let root = TempDir::new().unwrap();
    let packages = vec!["alpha-core".to_string()];
    let artifacts: Vec<PathBuf> = vec![];
    let closure = PackInstallClosure {
        pack_id: "alpha",
        pack_version: "1.0.0",
        pack_digest: "deadbeef",
        packages_installed: &packages,
        artifact_paths: &artifacts,
    };
    let receipt_path = emit_install_receipt(root.path(), &closure).expect("emit");

    // Tamper: flip the signature to a different (still hex) value.
    let mut body: serde_json::Value =
        serde_json::from_slice(&fs::read(&receipt_path).unwrap()).unwrap();
    body["signature"] = serde_json::json!("00".repeat(64));
    fs::write(&receipt_path, serde_json::to_vec_pretty(&body).unwrap()).unwrap();

    let outcome = PackAgent::at_root(root.path()).verify(&receipt_path);
    assert!(!outcome.is_valid, "tampered signature must not verify");
    assert!(outcome.reason.is_some());
}

#[test]
fn verify_malformed_receipt_is_invalid() {
    let root = TempDir::new().unwrap();
    let receipts_dir = root.path().join(".ggen").join("receipts");
    fs::create_dir_all(&receipts_dir).unwrap();
    let bogus = receipts_dir.join("bogus.json");
    fs::write(&bogus, "{}").unwrap();

    let outcome = PackAgent::at_root(root.path()).verify(&bogus);
    assert!(!outcome.is_valid, "malformed receipt must not verify");
    assert!(outcome.reason.is_some());
}

#[test]
fn verify_missing_key_is_invalid() {
    // Emit under root_a (which writes the key), then verify under root_b (no key).
    let root_a = TempDir::new().unwrap();
    let root_b = TempDir::new().unwrap();
    let packages = vec!["alpha-core".to_string()];
    let artifacts: Vec<PathBuf> = vec![];
    let closure = PackInstallClosure {
        pack_id: "alpha",
        pack_version: "1.0.0",
        pack_digest: "deadbeef",
        packages_installed: &packages,
        artifact_paths: &artifacts,
    };
    let receipt_path = emit_install_receipt(root_a.path(), &closure).expect("emit");

    let outcome = PackAgent::at_root(root_b.path()).verify(&receipt_path);
    assert!(!outcome.is_valid, "verify without the key must fail closed");
}

// ── registry-backed read ops (real GGEN_PACKS_DIR) ──────────────────────────

#[test]
fn list_and_search_against_real_registry() {
    let _guard = ENV_LOCK.lock().unwrap();
    let registry = TempDir::new().unwrap();
    write_pack(registry.path(), "alpha", "1.2.3");
    write_pack(registry.path(), "beta", "0.1.0");
    std::env::set_var("GGEN_PACKS_DIR", registry.path());

    let agent = PackAgent::new().expect("agent");

    let all = agent.list(None).expect("list");
    assert!(all.len() >= 2, "registry packs must be listed");
    assert!(all.iter().any(|p| p.id == "alpha"));

    // A name-match scores highest (1.0).
    let hits = agent.search("alpha", Some(10)).expect("search");
    let top = hits.first().expect("at least one hit");
    assert_eq!(top.pack.id, "alpha");
    assert!((top.score - 1.0).abs() < f64::EPSILON);

    std::env::remove_var("GGEN_PACKS_DIR");
}

#[test]
fn search_empty_query_is_rejected() {
    let err = PackAgent::new().unwrap().search("  ", None).unwrap_err();
    assert!(matches!(err, AgentError::InvalidRequest(_)), "got {err:?}");
}

#[test]
fn show_reflects_real_pack_metadata() {
    let _guard = ENV_LOCK.lock().unwrap();
    let registry = TempDir::new().unwrap();
    write_pack(registry.path(), "alpha", "1.2.3");
    std::env::set_var("GGEN_PACKS_DIR", registry.path());

    let detail = PackAgent::new().unwrap().show("alpha").expect("show");
    assert_eq!(detail.pack.id, "alpha");
    assert_eq!(detail.pack.version, "1.2.3");
    assert!(detail.packages.contains(&"alpha-core".to_string()));

    std::env::remove_var("GGEN_PACKS_DIR");
}

#[test]
fn show_nonexistent_pack_is_fail_closed() {
    let _guard = ENV_LOCK.lock().unwrap();
    let registry = TempDir::new().unwrap();
    std::env::set_var("GGEN_PACKS_DIR", registry.path());

    let err = PackAgent::new().unwrap().show("ghost").unwrap_err();
    assert!(matches!(err, AgentError::PackNotFound(_)), "got {err:?}");

    std::env::remove_var("GGEN_PACKS_DIR");
}

#[test]
fn resolve_capability_reports_missing_with_install_hints() {
    let _guard = ENV_LOCK.lock().unwrap();
    let registry = TempDir::new().unwrap(); // empty: the mapped pack is absent
    std::env::set_var("GGEN_PACKS_DIR", registry.path());

    let outcome = PackAgent::new()
        .unwrap()
        .resolve_capability("mcp", None, None)
        .expect("resolve");
    assert_eq!(outcome.surface, "mcp");
    assert!(
        !outcome.missing.is_empty(),
        "an empty registry must report the mapped pack as missing"
    );
    assert!(
        outcome
            .install_hints
            .iter()
            .any(|h| h.starts_with("ggen pack add ")),
        "missing packs must carry install hints"
    );

    std::env::remove_var("GGEN_PACKS_DIR");
}

// ── full lifecycle E2E: install → status → verify (multi-surface proof) ─────

#[test]
fn install_writes_lockfile_emits_signed_receipt_and_verifies() {
    let _guard = ENV_LOCK.lock().unwrap();
    let registry = TempDir::new().unwrap();
    write_pack(registry.path(), "alpha", "3.1.4");
    let home = TempDir::new().unwrap();
    let project = TempDir::new().unwrap();
    fs::create_dir_all(project.path().join(".ggen")).unwrap();

    std::env::set_var("GGEN_PACKS_DIR", registry.path());
    std::env::set_var("HOME", home.path());
    let _cwd = CwdGuard::enter(project.path());

    // PackAgent::new() roots at cwd == project, matching where install_pack writes
    // the lockfile, so all artifacts land in one `.ggen/`.
    let agent = PackAgent::new().expect("agent");
    let rt = tokio::runtime::Builder::new_current_thread()
        .enable_all()
        .build()
        .unwrap();
    let outcome = rt
        .block_on(agent.install(InstallRequest::new("alpha")))
        .expect("install");

    // 1. Evidence: non-empty digest pinned, lockfile + receipt produced.
    assert!(!outcome.digest.is_empty(), "real install must pin a digest");
    assert!(!outcome.dry_run);
    let lock_path = outcome.lockfile_path.clone().expect("lockfile path");
    assert!(
        Path::new(&lock_path).exists(),
        "lockfile must exist on disk"
    );
    let receipt = outcome.receipt.clone().expect("receipt emitted");
    assert!(receipt.signature_present, "receipt must carry a signature");
    assert!(Path::new(&receipt.receipt_path).exists());

    // 2. Corroboration: status() now reports alpha as installed.
    let status = agent.status().expect("status");
    assert!(status.installed.iter().any(|p| p.pack_id == "alpha"));

    // 3. Causality: the emitted receipt verifies against the project's key.
    let verify = agent.verify(&receipt.receipt_path);
    assert!(verify.is_valid, "emitted receipt must verify: {verify:?}");

    std::env::remove_var("GGEN_PACKS_DIR");
    std::env::remove_var("HOME");
}

#[test]
fn install_dry_run_pins_no_digest_and_emits_no_receipt() {
    let _guard = ENV_LOCK.lock().unwrap();
    let registry = TempDir::new().unwrap();
    write_pack(registry.path(), "alpha", "3.1.4");
    let home = TempDir::new().unwrap();
    let project = TempDir::new().unwrap();
    fs::create_dir_all(project.path().join(".ggen")).unwrap();

    std::env::set_var("GGEN_PACKS_DIR", registry.path());
    std::env::set_var("HOME", home.path());
    let _cwd = CwdGuard::enter(project.path());

    let agent = PackAgent::new().expect("agent");
    let rt = tokio::runtime::Builder::new_current_thread()
        .enable_all()
        .build()
        .unwrap();
    let req = InstallRequest {
        pack_id: "alpha".to_string(),
        force: false,
        dry_run: true,
        emit_receipt: true,
    };
    let outcome = rt.block_on(agent.install(req)).expect("dry-run install");

    assert!(outcome.dry_run);
    assert!(outcome.digest.is_empty(), "dry run must pin no digest");
    assert!(
        outcome.lockfile_path.is_none(),
        "dry run must write no lockfile"
    );
    assert!(outcome.receipt.is_none(), "dry run must emit no receipt");
    // No durable lockfile was written.
    assert!(!project.path().join(".ggen").join("packs.lock").exists());

    std::env::remove_var("GGEN_PACKS_DIR");
    std::env::remove_var("HOME");
}

#[test]
fn install_nonexistent_pack_is_fail_closed_and_writes_nothing() {
    let _guard = ENV_LOCK.lock().unwrap();
    let registry = TempDir::new().unwrap(); // empty
    let home = TempDir::new().unwrap();
    let project = TempDir::new().unwrap();
    fs::create_dir_all(project.path().join(".ggen")).unwrap();

    std::env::set_var("GGEN_PACKS_DIR", registry.path());
    std::env::set_var("HOME", home.path());
    let _cwd = CwdGuard::enter(project.path());

    let agent = PackAgent::new().expect("agent");
    let rt = tokio::runtime::Builder::new_current_thread()
        .enable_all()
        .build()
        .unwrap();
    let err = rt
        .block_on(agent.install(InstallRequest::new("ghost")))
        .unwrap_err();
    assert!(matches!(err, AgentError::PackNotFound(_)), "got {err:?}");
    // No lockfile written on a refused install.
    assert!(!project.path().join(".ggen").join("packs.lock").exists());

    std::env::remove_var("GGEN_PACKS_DIR");
    std::env::remove_var("HOME");
}
