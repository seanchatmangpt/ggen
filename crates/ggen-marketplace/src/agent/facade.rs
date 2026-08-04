//! [`PackAgent`] — the authoritative, agent-facing facade over the packs +
//! marketplace subsystems.
//!
//! Originally ported from `ggen-core/src/agent/facade.rs` into `ggen-cli`
//! during the ggen-core retirement migration (`specs/014-ggen-core-replacement`,
//! task T041), then moved again from `ggen-cli/src/agent/` into this crate
//! (`ggen-marketplace/src/agent/`) as tasks T048/T062: every dependency this
//! file has (pack metadata/validation/capability-registry, the install
//! pipeline, the lockfile) already lives in `ggen-marketplace`, so the facade
//! now sits alongside the subsystems it wraps instead of one crate away.
//! `ggen-cli`'s `crate::agent` module is a thin `pub use
//! ggen_marketplace::agent::*;` re-export shim so existing call sites keep
//! compiling unchanged.
//!
//! This is the single entry point an autonomous agent uses to discover, inspect,
//! resolve, install, remove, and verify packs. It wraps the existing
//! authoritative pipeline functions (`crate::marketplace::install::
//! install_pack_by_id`, `crate::packs_registry::metadata`,
//! `crate::packs_registry::validate`, the lockfile, and the receipt
//! emitter) and returns *structured, evidence-bearing* results
//! ([`crate::agent::types`]) rather than the human-oriented strings the CLI
//! produces. It does NOT introduce a second install/lockfile path — it routes
//! through the same durable-state writers the CLI uses, so authority is deepened
//! (one path, reachable by both humans and agents) rather than forked.

use std::path::{Path, PathBuf};

use crate::agent::receipt::{emit_install_receipt, verify_install_receipt, PackInstallClosure};
use crate::agent::types::{
    AgentError, AgentResult, AgentStatus, Capabilities, CapabilityRef, CompatibilityOutcome,
    DependencyRef, InstallOutcome, InstallRequest, InstalledPackRef, OperationRef, PackDetail,
    PackRef, PackValidation, ReceiptRef, RemoveOutcome, ResolveOutcome, SearchHit, VerifyOutcome,
};
use crate::marketplace::install::{install_pack_by_id, InstallByIdInput};
use crate::packs::lockfile::PackLockfile;
use crate::packs_registry::capability_registry::{list_capabilities, resolve_capability_to_packs};
use crate::packs_registry::check_packs_compatibility;
use crate::packs_registry::metadata::{list_packs, load_pack_metadata, show_pack};
use crate::packs_registry::types::Pack;
use crate::packs_registry::validate::validate_pack;

/// Agent-facing facade over packs + marketplace.
///
/// Construct with [`PackAgent::new`] (rooted at the current working directory,
/// the canonical project root for CLI/MCP invocations) or
/// [`PackAgent::at_root`] for an explicit project directory. The `root` is where
/// the facade reads/writes `.ggen/packs.lock`, receipts, and signing keys.
#[derive(Debug, Clone)]
pub struct PackAgent {
    root: PathBuf,
}

impl PackAgent {
    /// Create an agent rooted at the current working directory.
    ///
    /// # Errors
    /// Returns [`AgentError::Io`] if the current directory cannot be resolved.
    pub fn new() -> AgentResult<Self> {
        let root = std::env::current_dir().map_err(|e| AgentError::Io(e.to_string()))?;
        Ok(Self { root })
    }

    /// Create an agent rooted at an explicit project directory.
    pub fn at_root(root: impl Into<PathBuf>) -> Self {
        Self { root: root.into() }
    }

    /// The project root this agent operates against.
    pub fn root(&self) -> &Path {
        &self.root
    }

    /// Absolute path to the project lockfile this agent reads/writes.
    fn lockfile_path(&self) -> PathBuf {
        self.root.join(".ggen").join("packs.lock")
    }

    // ── Discovery ──────────────────────────────────────────────────────────

    /// Describe what this agent can do: its operations and the capability
    /// surfaces it knows how to resolve. This is the discovery entry point —
    /// an agent calls it first to learn the contract without out-of-band docs.
    pub fn capabilities(&self) -> Capabilities {
        let operations = vec![
            OperationRef {
                name: "search".to_string(),
                description: "Relevance-rank packs in the local registry by a text query."
                    .to_string(),
                mutating: false,
            },
            OperationRef {
                name: "list".to_string(),
                description: "List all packs in the local registry, optionally by category."
                    .to_string(),
                mutating: false,
            },
            OperationRef {
                name: "show".to_string(),
                description: "Full detail for one pack, including dependencies and validation."
                    .to_string(),
                mutating: false,
            },
            OperationRef {
                name: "resolve".to_string(),
                description: "Resolve a capability surface to concrete pack IDs.".to_string(),
                mutating: false,
            },
            OperationRef {
                name: "compatibility".to_string(),
                description: "Check whether a set of packs can be composed without conflicts."
                    .to_string(),
                mutating: false,
            },
            OperationRef {
                name: "status".to_string(),
                description: "Report installed packs from the project lockfile.".to_string(),
                mutating: false,
            },
            OperationRef {
                name: "verify".to_string(),
                description: "Verify a provenance receipt against its signing key.".to_string(),
                mutating: false,
            },
            OperationRef {
                name: "install".to_string(),
                description: "Install a pack: write the lockfile and emit a signed receipt."
                    .to_string(),
                mutating: true,
            },
            OperationRef {
                name: "remove".to_string(),
                description: "Remove a pack from the project lockfile.".to_string(),
                mutating: true,
            },
        ];

        let surfaces = list_capabilities()
            .into_iter()
            .map(|c| CapabilityRef {
                id: c.id,
                name: c.name,
                description: c.description,
                category: c.category,
                atomic_packs: c.atomic_packs,
            })
            .collect();

        Capabilities {
            operations,
            surfaces,
        }
    }

    // ── Read-only registry operations ──────────────────────────────────────

    /// List all packs in the local registry, optionally filtered by `category`.
    pub fn list(&self, category: Option<&str>) -> AgentResult<Vec<PackRef>> {
        let packs = list_packs(category).map_err(|e| AgentError::Internal(e.to_string()))?;
        Ok(packs.into_iter().map(pack_ref).collect())
    }

    /// Relevance-rank packs by a text query (name > id > description), highest
    /// first, capped at `limit` (default 20). An empty query is rejected.
    pub fn search(&self, query: &str, limit: Option<usize>) -> AgentResult<Vec<SearchHit>> {
        if query.trim().is_empty() {
            return Err(AgentError::InvalidRequest(
                "search query must not be empty".to_string(),
            ));
        }
        let packs = list_packs(None).map_err(|e| AgentError::Internal(e.to_string()))?;
        let q = query.to_lowercase();
        let max = limit.unwrap_or(20);

        let mut hits: Vec<SearchHit> = packs
            .into_iter()
            .filter_map(|p| {
                relevance(&p.name, &p.id, &p.description, &q).map(|score| SearchHit {
                    pack: pack_ref(p),
                    score,
                })
            })
            .collect();

        hits.sort_by(|a, b| {
            b.score
                .partial_cmp(&a.score)
                .unwrap_or(std::cmp::Ordering::Equal)
        });
        hits.truncate(max);
        Ok(hits)
    }

    /// Full detail for one pack: metadata, packages, templates, dependency
    /// edges, and the validation (quality-gate) result.
    pub fn show(&self, pack_id: &str) -> AgentResult<PackDetail> {
        validate_pack_name(pack_id)?;
        let pack = show_pack(pack_id).map_err(|_| AgentError::PackNotFound(pack_id.to_string()))?;

        let dependencies = pack
            .dependencies
            .iter()
            .map(|d| DependencyRef {
                pack_id: d.pack_id.clone(),
                version: d.version.clone(),
                optional: d.optional,
            })
            .collect();
        let templates = pack.templates.iter().map(|t| t.name.clone()).collect();
        let packages = pack.packages.clone();
        let sparql_query_count = pack.sparql_queries.len();

        // The validator reads the same registry; a failure to validate is
        // surfaced as an empty/invalid result rather than masking the pack.
        let validation = match validate_pack(pack_id) {
            Ok(v) => PackValidation {
                valid: v.valid,
                score: v.score,
                errors: v.errors,
                warnings: v.warnings,
            },
            Err(e) => PackValidation {
                valid: false,
                score: 0.0,
                errors: vec![format!("validation failed: {}", e)],
                warnings: Vec::new(),
            },
        };

        Ok(PackDetail {
            pack: pack_ref(pack),
            packages,
            templates,
            dependencies,
            sparql_query_count,
            validation,
        })
    }

    /// Resolve a capability surface (e.g. `mcp`, `web`) — optionally narrowed by
    /// `projection` and `runtime` — to concrete pack IDs, splitting them into
    /// `resolved` (present in the registry) and `missing` (with install hints).
    pub fn resolve_capability(
        &self, surface: &str, projection: Option<&str>, runtime: Option<&str>,
    ) -> AgentResult<ResolveOutcome> {
        if surface.trim().is_empty() {
            return Err(AgentError::InvalidRequest(
                "capability surface must not be empty".to_string(),
            ));
        }
        let pack_ids = resolve_capability_to_packs(surface, projection, runtime)
            .map_err(AgentError::ResolveFailed)?;

        let mut resolved = Vec::new();
        let mut missing = Vec::new();
        let mut install_hints = Vec::new();
        for id in pack_ids {
            if load_pack_metadata(&id).is_ok() {
                resolved.push(id);
            } else {
                install_hints.push(format!("ggen pack add {}", id));
                missing.push(id);
            }
        }

        Ok(ResolveOutcome {
            surface: surface.to_string(),
            projection: projection.map(String::from),
            runtime: runtime.map(String::from),
            resolved,
            missing,
            install_hints,
        })
    }

    /// Check whether a set of packs can be composed without conflicts, by
    /// loading each pack's real metadata and detecting overlapping package sets.
    ///
    /// This is the pre-flight an agent runs before installing a multi-pack
    /// composition. Fail-closed: an empty list is rejected, and a pack that
    /// cannot be loaded makes the set incompatible (reported as a conflict)
    /// rather than being silently dropped.
    pub async fn check_compatibility(
        &self, pack_ids: &[String],
    ) -> AgentResult<CompatibilityOutcome> {
        if pack_ids.is_empty() {
            return Err(AgentError::InvalidRequest(
                "at least one pack id is required".to_string(),
            ));
        }
        for id in pack_ids {
            validate_pack_name(id)?;
        }

        let result = check_packs_compatibility(pack_ids)
            .await
            .map_err(|e| AgentError::ResolveFailed(e.to_string()))?;

        Ok(CompatibilityOutcome {
            pack_ids: result.pack_ids,
            compatible: result.compatible,
            conflicts: result.conflicts,
            warnings: result.warnings,
            message: result.message,
        })
    }

    /// Read installed-pack state from the project lockfile. A missing lockfile
    /// is reported honestly (`lockfile_present == false`), not as an error.
    pub fn status(&self) -> AgentResult<AgentStatus> {
        let lockfile_path = self.lockfile_path();
        if !lockfile_path.exists() {
            return Ok(AgentStatus {
                lockfile_present: false,
                lockfile_path: lockfile_path.display().to_string(),
                ggen_version: None,
                installed: Vec::new(),
            });
        }

        let lockfile = PackLockfile::from_file(&lockfile_path)
            .map_err(|e| AgentError::Io(format!("cannot read lockfile: {}", e)))?;

        let installed = lockfile
            .packs
            .iter()
            .map(|(id, locked)| InstalledPackRef {
                pack_id: id.clone(),
                version: locked.version.clone(),
                integrity: locked.integrity.clone(),
                installed_at: locked.installed_at.to_rfc3339(),
            })
            .collect();

        Ok(AgentStatus {
            lockfile_present: true,
            lockfile_path: lockfile_path.display().to_string(),
            ggen_version: Some(lockfile.ggen_version),
            installed,
        })
    }

    /// Verify a provenance receipt at `receipt_path` against the signing key
    /// under `<root>/.ggen/keys/`. Fail-closed: a missing key, malformed
    /// receipt, or empty signature yields `is_valid == false` with a reason.
    pub fn verify(&self, receipt_path: impl AsRef<Path>) -> VerifyOutcome {
        let receipt_path = receipt_path.as_ref();
        let (is_valid, operation_id, reason) = verify_install_receipt(&self.root, receipt_path);
        VerifyOutcome {
            receipt_path: receipt_path.display().to_string(),
            is_valid,
            operation_id,
            reason,
        }
    }

    // ── Mutating lifecycle operations ──────────────────────────────────────

    /// Install a pack. On a real (non-dry-run) install this writes the project
    /// lockfile with a non-empty digest and, when `emit_receipt` is set, emits a
    /// signed provenance receipt — both bound into the returned [`InstallOutcome`]
    /// as proof of the durable state transition.
    ///
    /// Fail-closed: a pack that does not exist returns [`AgentError::PackNotFound`]
    /// and writes nothing; a receipt is emitted only after a successful install
    /// that pinned a non-empty digest.
    ///
    /// The underlying installer writes the lockfile relative to the current
    /// working directory; for the canonical [`PackAgent::new`] (root == cwd) this
    /// coincides with the receipt root, keeping all artifacts in one `.ggen/`.
    pub async fn install(&self, req: InstallRequest) -> AgentResult<InstallOutcome> {
        validate_pack_name(&req.pack_id)?;

        // Existence gate: refuse before touching durable state. A local pack
        // must resolve in the registry; an external (`prefix:id`) pack is
        // resolved by the installer itself.
        if !req.pack_id.contains(':') && load_pack_metadata(&req.pack_id).is_err() {
            return Err(AgentError::PackNotFound(req.pack_id.clone()));
        }

        let input = InstallByIdInput {
            pack_id: req.pack_id.clone(),
            target_dir: Some(self.root.join(".ggen").join("packs").join(&req.pack_id)),
            force: req.force,
            dry_run: req.dry_run,
        };

        let output = install_pack_by_id(&input)
            .await
            .map_err(|e| AgentError::InstallFailed(e.to_string()))?;

        // Emit a provenance receipt for a real install when requested. Emission
        // is gated on a non-empty digest by the receipt emitter itself; a dry
        // run pins no digest and therefore produces no receipt.
        let receipt = if req.emit_receipt && !req.dry_run && !output.digest.trim().is_empty() {
            let mut artifact_paths = vec![output.install_path.clone()];
            if let Some(lock) = &output.lockfile_path {
                artifact_paths.push(lock.clone());
            }
            let closure = PackInstallClosure {
                pack_id: &output.pack_id,
                pack_version: &output.pack_version,
                pack_digest: &output.digest,
                packages_installed: &output.packages_installed,
                artifact_paths: &artifact_paths,
            };
            let path = emit_install_receipt(&self.root, &closure)
                .map_err(|e| AgentError::Receipt(e.to_string()))?;
            Some(receipt_ref(&path))
        } else {
            None
        };

        Ok(InstallOutcome {
            pack_id: output.pack_id,
            pack_name: output.pack_name,
            pack_version: output.pack_version,
            packages_installed: output.packages_installed,
            templates_available: output.templates_available,
            digest: output.digest,
            install_path: output.install_path.display().to_string(),
            lockfile_path: output.lockfile_path.map(|p| p.display().to_string()),
            receipt,
            dry_run: req.dry_run,
        })
    }

    /// Remove a pack from the project lockfile. Fail-closed: a missing lockfile
    /// or an absent pack returns a typed error and leaves the lockfile intact.
    pub fn remove(&self, pack_id: &str) -> AgentResult<RemoveOutcome> {
        validate_pack_name(pack_id)?;
        let lockfile_path = self.lockfile_path();

        if !lockfile_path.exists() {
            return Err(AgentError::NotInstalled(format!(
                "{}: no lockfile at {}",
                pack_id,
                lockfile_path.display()
            )));
        }

        let mut lockfile = PackLockfile::from_file(&lockfile_path)
            .map_err(|e| AgentError::Io(format!("cannot read lockfile: {}", e)))?;

        if lockfile.get_pack(pack_id).is_none() {
            return Err(AgentError::NotInstalled(pack_id.to_string()));
        }

        let removed = lockfile.remove_pack(pack_id);
        lockfile
            .save(&lockfile_path)
            .map_err(|e| AgentError::Io(format!("cannot save lockfile: {}", e)))?;

        let remaining = lockfile.packs.keys().cloned().collect();

        Ok(RemoveOutcome {
            pack_id: pack_id.to_string(),
            removed,
            lockfile_path: lockfile_path.display().to_string(),
            remaining,
        })
    }
}

// ── Helpers ────────────────────────────────────────────────────────────────

fn pack_ref(p: Pack) -> PackRef {
    PackRef {
        id: p.id,
        name: p.name,
        version: p.version,
        description: p.description,
        category: p.category,
        registry_type: p.registry_type.unwrap_or_else(|| "local".to_string()),
        production_ready: p.production_ready,
    }
}

fn receipt_ref(path: &Path) -> ReceiptRef {
    // Best-effort read of the receipt to surface the operation_id and confirm a
    // non-empty signature. A read/parse failure does not invalidate the install
    // (the receipt file exists); it just yields a conservative descriptor.
    let (operation_id, signature_present) = std::fs::read(path)
        .ok()
        .and_then(|bytes| serde_json::from_slice::<serde_json::Value>(&bytes).ok())
        .map(|v| {
            let op = v
                .get("operation_id")
                .and_then(|x| x.as_str())
                .unwrap_or_default()
                .to_string();
            let sig = v
                .get("signature")
                .and_then(|x| x.as_str())
                .map(|s| !s.trim().is_empty())
                .unwrap_or(false);
            (op, sig)
        })
        .unwrap_or_default();

    ReceiptRef {
        receipt_path: path.display().to_string(),
        operation_id,
        signature_present,
    }
}

/// Relevance score for a query against a pack's fields, mirroring the CLI's
/// `calculate_relevance`: exact-substring priority name > id > description.
fn relevance(name: &str, id: &str, desc: &str, query_lower: &str) -> Option<f64> {
    if name.to_lowercase().contains(query_lower) {
        Some(1.0)
    } else if id.to_lowercase().contains(query_lower) {
        Some(0.8)
    } else if desc.to_lowercase().contains(query_lower) {
        Some(0.5)
    } else {
        None
    }
}

/// Validate a pack identifier: non-empty, no path separators or traversal
/// sequences, and limited to alphanumerics, `-`, `_`, `.`, and `:` (the
/// external-registry separator, e.g. `npm:lodash`).
///
/// This is the ONLY gate standing in front of two unguarded filesystem joins
/// reachable from every mutating and read-only operation on this facade (via
/// MCP/A2A in `ggen-lsp/src/a2a_mcp/mcp_packs.rs`):
/// `packs_registry::metadata::load_pack_metadata` does
/// `packs_dir.join(format!("{pack_id}.toml"))`, and [`PackAgent::install`]
/// does `root.join(".ggen").join("packs").join(pack_id)`. `Path::join` with a
/// `pack_id` containing `..` walks out of the intended directory, and an
/// absolute `pack_id` (leading `/`) replaces the base directory entirely
/// (documented `PathBuf::join` behavior) — so both must be rejected here,
/// before either join ever runs, not sanitized after the fact. This mirrors
/// the stricter guard `packs_registry::repository::FileSystemRepository::
/// validate_pack_id` already applies for its own local-filesystem pack
/// lookups; no real local pack ID in this codebase's tests or fixtures uses
/// `/` (e.g. `io.ggen.trust-test`), and external (`prefix:id`) pack IDs in
/// this codebase are also always slash-free (`npm:lodash`, `cratesio:demo`),
/// so disallowing `/`/`\`/`..` outright costs no legitimate case.
fn validate_pack_name(pack_id: &str) -> AgentResult<()> {
    if pack_id.trim().is_empty() {
        return Err(AgentError::InvalidRequest(
            "pack id must not be empty".to_string(),
        ));
    }
    if pack_id.contains("..") || pack_id.contains('/') || pack_id.contains('\\') {
        return Err(AgentError::InvalidRequest(format!(
            "pack id '{}' must not contain path separators or traversal sequences",
            pack_id
        )));
    }
    let valid = pack_id
        .chars()
        .all(|c| c.is_alphanumeric() || matches!(c, '-' | '_' | '.' | ':'));
    if !valid {
        return Err(AgentError::InvalidRequest(format!(
            "pack id '{}' contains invalid characters",
            pack_id
        )));
    }
    Ok(())
}

#[cfg(test)]
mod path_traversal_tests {
    use super::*;
    use serial_test::serial;
    use tempfile::TempDir;

    /// Saves the prior value of an env var on construction and restores it
    /// (or removes it if previously unset) on Drop, mirroring
    /// `packs_registry::metadata`'s own test-only `EnvVarGuard` so env
    /// mutation in one test cannot leak into another.
    struct EnvVarGuard {
        key: &'static str,
        previous: Option<std::ffi::OsString>,
    }

    impl EnvVarGuard {
        fn set(key: &'static str, value: &str) -> Self {
            let previous = std::env::var_os(key);
            std::env::set_var(key, value);
            Self { key, previous }
        }
    }

    impl Drop for EnvVarGuard {
        fn drop(&mut self) {
            match &self.previous {
                None => std::env::remove_var(self.key),
                Some(v) => std::env::set_var(self.key, v),
            }
        }
    }

    // ── Unit tests: validate_pack_name itself ──────────────────────────────

    #[test]
    fn sabotage_validate_pack_name_rejects_relative_traversal() {
        assert!(validate_pack_name("../x").is_err());
        assert!(validate_pack_name("../../etc/passwd").is_err());
        assert!(validate_pack_name("a/../../b").is_err());
    }

    #[test]
    fn sabotage_validate_pack_name_rejects_absolute_path() {
        assert!(validate_pack_name("/etc/passwd").is_err());
    }

    #[test]
    fn sabotage_validate_pack_name_rejects_any_path_separator() {
        // A bare slash with no ".." is still a directory escape hazard once
        // joined with `packs_dir.join(format!("{pack_id}.toml"))` (it changes
        // which directory the ".toml" file is read from), so it must be
        // rejected even without an explicit "..".
        assert!(validate_pack_name("sub/pack").is_err());
        assert!(validate_pack_name("sub\\pack").is_err());
    }

    #[test]
    fn sabotage_validate_pack_name_accepts_real_local_pack_id_conventions() {
        // Reverse-domain dotted names (as used by install.rs's own fixtures,
        // e.g. "io.ggen.trust-test") and external "<prefix>:<id>" references
        // (e.g. "npm:lodash") must keep working after tightening the guard.
        assert!(validate_pack_name("io.ggen.trust-test").is_ok());
        assert!(validate_pack_name("npm:lodash").is_ok());
        assert!(validate_pack_name("wasm4pm-facts-pack").is_ok());
    }

    // ── End-to-end: the traversal must be refused before any filesystem I/O ──

    /// Reproduces FINDING #1 end-to-end: a `pack_id` of `"../secret"` against
    /// a `GGEN_PACKS_DIR` of `<root>/packs` resolves (pre-fix) to
    /// `<root>/secret.toml` -- a real, parseable `Pack` TOML file that sits
    /// OUTSIDE the packs directory. Before the fix this file was
    /// path-traversal-readable via `PackAgent::show`; the assertion checks
    /// both that the call fails, and that it fails with the validation
    /// error (not a coincidental "not found"/parse error), proving the guard
    /// -- not luck -- is what stops the read.
    #[test]
    #[serial(GGEN_PACKS_DIR)]
    fn sabotage_show_refuses_path_traversal_pack_id_outside_packs_dir() {
        // Arrange: <root>/packs (the declared packs dir) and a sibling
        // <root>/secret.toml (the traversal target, OUTSIDE packs dir) that
        // is a real, validly-parseable pack file -- so a pre-fix run of this
        // test would actually succeed in reading it, not merely 404.
        let root = TempDir::new().unwrap();
        let packs_dir = root.path().join("packs");
        std::fs::create_dir_all(&packs_dir).unwrap();
        std::fs::write(
            root.path().join("secret.toml"),
            r#"
[pack]
id = "secret"
name = "Secret Outside Packs Dir"
version = "1.0.0"
description = "must never be reachable via a packs_dir-relative pack id"
category = "test"
license = "MIT"
packages = []
"#,
        )
        .unwrap();
        let _guard = EnvVarGuard::set("GGEN_PACKS_DIR", packs_dir.to_str().unwrap());

        let agent = PackAgent::at_root(root.path());

        // Act
        let result = agent.show("../secret");

        // Assert -- must be Err, and specifically the validation error, not
        // a coincidental not-found/parse failure.
        match result {
            Ok(detail) => panic!(
                "path traversal pack id must be refused, but got Ok: {:?} -- \
                 this is exactly FINDING #1 (arbitrary file read via pack_id)",
                detail
            ),
            Err(AgentError::PackNotFound(_)) => {
                // show() maps validate_pack_name's Err into PackNotFound via
                // the `?` propagation path is NOT expected here -- Rust's `?`
                // preserves the original error variant, so a PackNotFound
                // here would mean the traversal reached load_pack_metadata's
                // real not-found path instead of being blocked up front.
                panic!(
                    "traversal reached load_pack_metadata (PackNotFound) instead of being \
                     blocked by validate_pack_name -- the guard did not fire"
                );
            }
            Err(AgentError::InvalidRequest(msg)) => {
                assert!(
                    msg.contains("path separators") || msg.contains("traversal"),
                    "expected the path-traversal validation message, got: {msg}"
                );
            }
            Err(other) => panic!("expected AgentError::InvalidRequest, got: {other:?}"),
        }
    }

    /// Same reproduction for the WRITE side ([`PackAgent::install`]): a
    /// crafted `pack_id` must not be able to steer `target_dir` outside
    /// `<root>/.ggen/packs/`. Asserts both the `Err` and that no directory
    /// materializes at the escaped location.
    #[tokio::test]
    #[serial(GGEN_PACKS_DIR)]
    async fn sabotage_install_refuses_path_traversal_pack_id() {
        let root = TempDir::new().unwrap();
        let packs_dir = root.path().join("packs");
        std::fs::create_dir_all(&packs_dir).unwrap();
        let _guard = EnvVarGuard::set("GGEN_PACKS_DIR", packs_dir.to_str().unwrap());

        let agent = PackAgent::at_root(root.path());
        let escaped_target = root.path().join("evil-escaped-dir");

        let req = InstallRequest {
            pack_id: "../../evil-escaped-dir".to_string(),
            force: false,
            dry_run: false,
            emit_receipt: false,
        };

        // Act
        let result = agent.install(req).await;

        // Assert
        assert!(
            result.is_err(),
            "install with a path-traversal pack id must be refused, got Ok: {:?}",
            result.ok()
        );
        assert!(
            !escaped_target.exists(),
            "a refused install must not create any directory outside .ggen/packs/ -- \
             found one at {}",
            escaped_target.display()
        );
    }

    /// Same reproduction for [`PackAgent::remove`].
    #[test]
    #[serial(GGEN_PACKS_DIR)]
    fn sabotage_remove_refuses_path_traversal_pack_id() {
        let root = TempDir::new().unwrap();
        let packs_dir = root.path().join("packs");
        std::fs::create_dir_all(&packs_dir).unwrap();
        let _guard = EnvVarGuard::set("GGEN_PACKS_DIR", packs_dir.to_str().unwrap());

        let agent = PackAgent::at_root(root.path());
        let result = agent.remove("../secret");

        assert!(
            result.is_err(),
            "remove with a path-traversal pack id must be refused, got Ok: {:?}",
            result.ok()
        );
    }
}
