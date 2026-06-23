//! [`PackAgent`] — the authoritative, agent-facing facade over the packs +
//! marketplace subsystems.
//!
//! This is the single entry point an autonomous agent uses to discover, inspect,
//! resolve, install, remove, and verify packs. It wraps the existing
//! authoritative pipeline functions (`domain::packs::install::install_pack`,
//! `domain::packs::metadata`, `domain::packs::validate`, the lockfile, and the
//! receipt emitter) and returns *structured, evidence-bearing* results
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
use crate::domain::packs::capability_registry::{list_capabilities, resolve_capability_to_packs};
use crate::domain::packs::check_packs_compatibility;
use crate::domain::packs::install::{install_pack, InstallInput};
use crate::domain::packs::metadata::{list_packs, load_pack_metadata, show_pack};
use crate::domain::packs::types::Pack;
use crate::domain::packs::validate::validate_pack;
use crate::packs::lockfile::PackLockfile;

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
                name: "check_compatibility".to_string(),
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

        let input = InstallInput {
            pack_id: req.pack_id.clone(),
            target_dir: None,
            force: req.force,
            dry_run: req.dry_run,
        };

        let output = install_pack(&input)
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

/// Validate a pack identifier the same way the CLI does: non-empty, and limited
/// to alphanumerics, `-`, `_`, `.`, and `:` (the external-registry separator).
fn validate_pack_name(pack_id: &str) -> AgentResult<()> {
    if pack_id.trim().is_empty() {
        return Err(AgentError::InvalidRequest(
            "pack id must not be empty".to_string(),
        ));
    }
    let valid = pack_id
        .chars()
        .all(|c| c.is_alphanumeric() || matches!(c, '-' | '_' | '.' | ':' | '/'));
    if !valid {
        return Err(AgentError::InvalidRequest(format!(
            "pack id '{}' contains invalid characters",
            pack_id
        )));
    }
    Ok(())
}
