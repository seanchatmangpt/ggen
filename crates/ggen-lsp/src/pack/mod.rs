//! Agent Admissibility Pack emitter.
//!
//! Manufactures a per-project `.agent-admissibility/` pack from embedded
//! templates: an LSP config, coding-agent hooks (which shell out to
//! `ggen lsp check`), and default SHACL policies. This is the "hook foundry"
//! half of ggen-as-foundry — the LSP gives live type pressure, these hooks are
//! the admission gate, and both enforce one canon.

use std::io;
use std::path::{Path, PathBuf};

use serde::Serialize;

const PRE_EDIT: &str = include_str!("../../assets/hooks/pre-edit.sh");
const POST_EDIT: &str = include_str!("../../assets/hooks/post-edit.sh");
const PRE_COMMIT: &str = include_str!("../../assets/hooks/pre-commit.sh");
const REFUSAL: &str = include_str!("../../assets/hooks/refusal.sh");
const CLAUDE_SETTINGS: &str = include_str!("../../assets/hooks/claude-code/settings-snippet.json");

const POLICY_PUBLIC_VOCAB: &str = include_str!("../../assets/policies/public-vocab.shacl.ttl");
const POLICY_NO_PRIVATE_NS: &str =
    include_str!("../../assets/policies/no-private-namespace.shacl.ttl");
const POLICY_RECEIPT_REQUIRED: &str =
    include_str!("../../assets/policies/receipt-required.shacl.ttl");

const README: &str = include_str!("../../assets/pack-readme.md");

const POWL_README: &str = "# Repair routes (POWL)\n\n\
Repair routes are POWL models (partial orders of steps with choice/loop). They are\n\
PROMOTED from mined dominant failure edges — run `ggen lsp mine` after `ggen lsp check`\n\
has accumulated agent-edit events, then review `../ocel/discovery/error-edge-mining.md`.\n\
Seeded cold-start routes ship inside the `ggen-lsp` binary; mined routes land here.\n";

const OCEL_README: &str = "# Agent-edit event log + discovery\n\n\
`agent-edit-events.ocel.jsonl` accumulates object-centric events (DiagnosticRaised,\n\
RepairSuggested, GatePassed/Failed, ...) captured by `ggen lsp check`. `ggen lsp mine`\n\
projects them to RDF and discovers failure edges via SPARQL, writing\n\
`discovery/error-edge-mining.md`. This is the 80/20 evidence behind repair routes.\n";

/// Coding-agent targets the pack can emit hooks for.
pub const DEFAULT_AGENTS: &[&str] = &["claude-code", "cursor", "codex", "generic"];

/// Options for emitting an admissibility pack.
#[derive(Debug, Clone)]
pub struct PackOptions {
    /// Agent targets to emit hook sets for.
    pub agents: Vec<String>,
    /// Output directory (typically `.agent-admissibility`).
    pub out_dir: PathBuf,
    /// Aggregate hash of the capability scan this pack is manufactured from
    /// (e.g. cpmp's `aggregate_hash`). When present, the emitted pack receipt
    /// binds `scan_hash → pack_hash`, making the pack's provenance replayable.
    pub scan_hash: Option<String>,
}

impl Default for PackOptions {
    fn default() -> Self {
        Self {
            agents: DEFAULT_AGENTS.iter().map(|s| (*s).to_string()).collect(),
            out_dir: PathBuf::from(".agent-admissibility"),
            scan_hash: None,
        }
    }
}

/// Filename of the in-pack provenance record (excluded from the pack hash).
const PROVENANCE_FILE: &str = "pack-provenance.json";
/// Filename of the advertised pack manifest (excluded from the pack hash).
const MANIFEST_FILE: &str = "pack-manifest.json";

/// Law-surface globs advertised by the pack (shared by the LSP config + manifest).
const LAW_SURFACES: &[&str] = &[
    "**/*.ttl",
    "**/*.nt",
    "**/*.nq",
    "**/*.rq",
    "**/*.sparql",
    "**/*.tera",
    "**/ggen.toml",
];

/// One advertised SHACL policy (path + content hash).
#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
pub struct PolicyEntry {
    pub path: String,
    pub hash: String,
}

/// One advertised repair route (the movable route-law parts a remote agent gets).
#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
pub struct RouteEntry {
    pub route_id: String,
    pub family: String,
    pub source: String,
}

/// The advertised, hash-bound surface of a pack: what routes/policies/law-surfaces
/// it carries, under which canon/version, bound to its content hash.
///
/// A remote
/// (MCP/A2A) agent reads this to consume the pack as a movable route-law part and
/// to bind a route response to a specific pack.
#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
pub struct PackManifest {
    pub version: u8,
    pub canon: String,
    pub pack_hash: String,
    pub law_surfaces: Vec<String>,
    pub policies: Vec<PolicyEntry>,
    pub routes: Vec<RouteEntry>,
}

impl PackManifest {
    pub const VERSION: u8 = 1;
    pub const CANON: &'static str = "v30.1.1";
}

/// Records what the pack was manufactured from and its content hash, so
/// [`verify_pack`] can reconstruct the binding (mirrors `PromotedRoutes`).
#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
pub struct PackProvenance {
    pub version: u8,
    /// Aggregate hash of the source capability scan (empty if none).
    pub scan_hash: String,
    /// Content hash of the emitted pack (all files except receipts/ + this file).
    pub pack_hash: String,
}

impl PackProvenance {
    pub const VERSION: u8 = 1;
}

/// Summary of an emit run (serializable for the CLI).
#[derive(Debug, Clone, Serialize)]
pub struct EmitReport {
    /// Directory the pack was written to.
    pub out_dir: String,
    /// Agent targets emitted.
    pub agents: Vec<String>,
    /// Relative paths written.
    pub files_written: Vec<String>,
    /// Content hash of the emitted pack.
    pub pack_hash: String,
    /// Signature of the scan→pack receipt (None only on a write failure).
    pub receipt_sig: Option<String>,
}

/// Emit the admissibility pack to disk.
///
/// # Errors
/// Returns an I/O error if any directory or file cannot be created.
pub fn emit(opts: &PackOptions) -> io::Result<EmitReport> {
    let root = &opts.out_dir;
    let mut written = Vec::new();

    // lsp/ — config shared by the LSP and hooks.
    write_file(
        &root.join("lsp/lsp-config.json"),
        &lsp_config_json(),
        false,
        &mut written,
        root,
    )?;

    // hooks/<agent>/ — one script set per requested agent.
    let hooks: [(&str, &str, bool); 4] = [
        ("pre-edit.sh", PRE_EDIT, true),
        ("post-edit.sh", POST_EDIT, true),
        ("pre-commit.sh", PRE_COMMIT, true),
        ("refusal.sh", REFUSAL, true),
    ];
    for agent in &opts.agents {
        for (name, body, exec) in &hooks {
            write_file(
                &root.join("hooks").join(agent).join(name),
                body,
                *exec,
                &mut written,
                root,
            )?;
        }
        if agent == "claude-code" {
            write_file(
                &root.join("hooks/claude-code/settings-snippet.json"),
                CLAUDE_SETTINGS,
                false,
                &mut written,
                root,
            )?;
        }
    }

    // policies/ — default SHACL policies.
    for (name, body) in [
        ("public-vocab.shacl.ttl", POLICY_PUBLIC_VOCAB),
        ("no-private-namespace.shacl.ttl", POLICY_NO_PRIVATE_NS),
        ("receipt-required.shacl.ttl", POLICY_RECEIPT_REQUIRED),
    ] {
        write_file(
            &root.join("policies").join(name),
            body,
            false,
            &mut written,
            root,
        )?;
    }

    // receipts/ — keep the directory under version control.
    write_file(
        &root.join("receipts/.gitkeep"),
        "",
        false,
        &mut written,
        root,
    )?;

    // Process-intelligence stewardship dirs (the pack travels with these):
    //   powl/       — repair routes (POWL), promoted from mined edges via `ggen lsp mine`
    //   ocel/       — agent-edit event log + mined discovery reports
    //   replay/     — fixtures for replaying repair episodes
    write_file(
        &root.join("powl/README.md"),
        POWL_README,
        false,
        &mut written,
        root,
    )?;
    write_file(
        &root.join("ocel/README.md"),
        OCEL_README,
        false,
        &mut written,
        root,
    )?;
    write_file(&root.join("replay/.gitkeep"), "", false, &mut written, root)?;

    // README.
    write_file(&root.join("README.md"), README, false, &mut written, root)?;

    // Provenance: content-hash the pack and bind it to the source scan with a
    // receipt (scan_hash → pack_hash). This is the "capability map manufactures
    // the pack" proof — replayable, tamper-evident (see `verify_pack`).
    let pack_hash = compute_pack_hash(root);
    let scan_hash = opts.scan_hash.clone().unwrap_or_default();
    let provenance = PackProvenance {
        version: PackProvenance::VERSION,
        scan_hash: scan_hash.clone(),
        pack_hash: pack_hash.clone(),
    };
    if let Ok(json) = serde_json::to_string_pretty(&provenance) {
        let _ = std::fs::write(root.join(PROVENANCE_FILE), json);
    }
    let receipt_sig = emit_pack_receipt(root, &scan_hash, &pack_hash);

    // Advertised manifest: the movable route-law surface, bound to the pack hash.
    let manifest = build_manifest(root, &pack_hash);
    if let Ok(json) = serde_json::to_string_pretty(&manifest) {
        let _ = std::fs::write(root.join(MANIFEST_FILE), json);
    }

    Ok(EmitReport {
        out_dir: root.to_string_lossy().to_string(),
        agents: opts.agents.clone(),
        files_written: written,
        pack_hash,
        receipt_sig,
    })
}

/// Deterministic content hash of an emitted pack.
///
/// BLAKE3 over each file's
/// `(relative path, bytes)` in sorted order, EXCLUDING the `receipts/` subtree
/// and the provenance file (so the hash is stable and recomputable).
///
/// Same walk
/// is used at emit and verify time.
#[must_use]
pub fn compute_pack_hash(pack_dir: &Path) -> String {
    let mut entries: Vec<(String, PathBuf)> = walkdir::WalkDir::new(pack_dir)
        .sort_by_file_name()
        .into_iter()
        .filter_map(std::result::Result::ok)
        .filter(|e| e.file_type().is_file())
        .filter_map(|e| {
            let rel = e.path().strip_prefix(pack_dir).ok()?.to_path_buf();
            // Exclude provenance + manifest + receipts (derived from the hash).
            let rel_str = rel.to_string_lossy();
            if rel_str == PROVENANCE_FILE
                || rel_str == MANIFEST_FILE
                || rel.components().any(|c| c.as_os_str() == "receipts")
            {
                return None;
            }
            Some((
                rel.to_string_lossy().replace('\\', "/"),
                e.path().to_path_buf(),
            ))
        })
        .collect();
    entries.sort_by(|a, b| a.0.cmp(&b.0));
    let mut hasher = blake3::Hasher::new();
    for (rel, abs) in entries {
        hasher.update(rel.as_bytes());
        hasher.update(&[0]);
        hasher.update(&std::fs::read(&abs).unwrap_or_default());
        hasher.update(&[0]);
    }
    hasher.finalize().to_hex().to_string()
}

/// Write the scan→pack receipt under `pack_dir/receipts/pack-{sig}.json`,
/// returning its signature hex. `pre = blake3(scan_hash)`, `post = blake3(pack_hash)`.
fn emit_pack_receipt(pack_dir: &Path, scan_hash: &str, pack_hash: &str) -> Option<String> {
    let pre: [u8; 32] = blake3::hash(scan_hash.as_bytes()).into();
    let post: [u8; 32] = blake3::hash(pack_hash.as_bytes()).into();
    let receipt = crate::intel::RepairReceipt::new(
        "pack".to_string(),
        "CPMP-PACK-1".to_string(),
        pre,
        post,
        true,
    );
    let sig = receipt.signature_hex();
    let dir = pack_dir.join("receipts");
    if std::fs::create_dir_all(&dir).is_ok() {
        if let Ok(json) = serde_json::to_string_pretty(&receipt) {
            let _ = std::fs::write(dir.join(format!("pack-{}.json", &sig[..16])), json);
        }
    }
    Some(sig)
}

/// Verification of an emitted pack against its scan→pack receipt.
#[derive(Debug, Clone, Serialize)]
pub struct PackReplay {
    pub matches: bool,
    pub reason: String,
}

/// Re-derive the pack content hash and check it against the stored provenance
/// and a matching scan→pack receipt. Mutating any pack file changes the
/// recomputed hash ⇒ mismatch. Mirrors `verify_promotion`.
#[must_use]
pub fn verify_pack(pack_dir: &Path) -> PackReplay {
    let Some(provenance) = std::fs::read_to_string(pack_dir.join(PROVENANCE_FILE))
        .ok()
        .and_then(|s| serde_json::from_str::<PackProvenance>(&s).ok())
    else {
        return PackReplay {
            matches: false,
            reason: "no pack provenance record".to_string(),
        };
    };
    let current = compute_pack_hash(pack_dir);
    if current != provenance.pack_hash {
        return PackReplay {
            matches: false,
            reason: format!(
                "pack hash mismatch: provenance={} current={} (tampered)",
                &provenance.pack_hash[..8.min(provenance.pack_hash.len())],
                &current[..8.min(current.len())]
            ),
        };
    }
    let pre: [u8; 32] = blake3::hash(provenance.scan_hash.as_bytes()).into();
    let post: [u8; 32] = blake3::hash(provenance.pack_hash.as_bytes()).into();
    let matches = find_pack_receipt(pack_dir, pre, post);
    PackReplay {
        matches,
        reason: if matches {
            "pack receipt reconstructs from current pack + scan hash".to_string()
        } else {
            "no receipt binds the current pack to its scan (tampered or stale)".to_string()
        },
    }
}

fn find_pack_receipt(pack_dir: &Path, pre: [u8; 32], post: [u8; 32]) -> bool {
    let Ok(entries) = std::fs::read_dir(pack_dir.join("receipts")) else {
        return false;
    };
    for entry in entries.flatten() {
        if !entry.file_name().to_string_lossy().starts_with("pack-") {
            continue;
        }
        if let Ok(content) = std::fs::read_to_string(entry.path()) {
            if let Ok(r) = serde_json::from_str::<crate::intel::RepairReceipt>(&content) {
                if r.pre_state_hash == pre && r.post_state_hash == post && r.verify() {
                    return true;
                }
            }
        }
    }
    false
}

fn lsp_config_json() -> String {
    let config = serde_json::json!({
        "law_surfaces": LAW_SURFACES,
        "policies": [
            "policies/public-vocab.shacl.ttl",
            "policies/no-private-namespace.shacl.ttl",
            "policies/receipt-required.shacl.ttl"
        ],
        "check_command": "ggen lsp check --files",
        "canon": "v30.1.1"
    });
    serde_json::to_string_pretty(&config).unwrap_or_else(|_| "{}".to_string())
}

/// The default pack directory under a project root.
#[must_use]
pub fn default_pack_dir(project_root: &Path) -> PathBuf {
    project_root.join(".agent-admissibility")
}

/// Build the advertised manifest for an emitted pack: law surfaces, hashed
/// policies, and the promoted routes the pack carries — bound to `pack_hash`.
fn build_manifest(pack_dir: &Path, pack_hash: &str) -> PackManifest {
    let policies = [
        "public-vocab.shacl.ttl",
        "no-private-namespace.shacl.ttl",
        "receipt-required.shacl.ttl",
    ]
    .iter()
    .filter_map(|name| {
        let path = pack_dir.join("policies").join(name);
        let bytes = std::fs::read(&path).ok()?;
        Some(PolicyEntry {
            path: format!("policies/{name}"),
            hash: blake3::hash(&bytes).to_hex().to_string(),
        })
    })
    .collect();

    // Routes the pack carries (from its promoted-route artifact, if mined).
    let routes = crate::route::load_promoted(&pack_dir.join("powl").join("repair-routes.json"))
        .map(|promoted| {
            promoted
                .routes
                .iter()
                .map(|r| RouteEntry {
                    route_id: r.id.0.clone(),
                    family: format!("{:?}", r.family),
                    source: match r.provenance {
                        crate::route::Provenance::Seeded => "seed".to_string(),
                        crate::route::Provenance::Mined { .. } => "mined".to_string(),
                    },
                })
                .collect()
        })
        .unwrap_or_default();

    PackManifest {
        version: PackManifest::VERSION,
        canon: PackManifest::CANON.to_string(),
        pack_hash: pack_hash.to_string(),
        law_surfaces: LAW_SURFACES.iter().map(|s| (*s).to_string()).collect(),
        policies,
        routes,
    }
}

/// Load a pack's advertised manifest, if present.
#[must_use]
pub fn load_manifest(pack_dir: &Path) -> Option<PackManifest> {
    let content = std::fs::read_to_string(pack_dir.join(MANIFEST_FILE)).ok()?;
    serde_json::from_str(&content).ok()
}

/// Whether a manifest is compatible with the current canon + schema version. A
/// stale or future pack fails this check (the remote-pack staleness guard).
#[must_use]
pub fn manifest_is_current(manifest: &PackManifest) -> bool {
    manifest.version == PackManifest::VERSION && manifest.canon == PackManifest::CANON
}

/// The content hash of the pack installed under `project_root` (from its
/// manifest), so a route response can bind to a specific pack. None if no pack.
#[must_use]
pub fn pack_hash_at(project_root: &Path) -> Option<String> {
    load_manifest(&default_pack_dir(project_root)).map(|m| m.pack_hash)
}

fn write_file(
    path: &Path, contents: &str, executable: bool, written: &mut Vec<String>, root: &Path,
) -> io::Result<()> {
    if let Some(parent) = path.parent() {
        std::fs::create_dir_all(parent)?;
    }
    std::fs::write(path, contents)?;
    if executable {
        set_executable(path)?;
    }
    let rel = path.strip_prefix(root).unwrap_or(path);
    written.push(rel.to_string_lossy().to_string());
    Ok(())
}

#[cfg(unix)]
fn set_executable(path: &Path) -> io::Result<()> {
    use std::os::unix::fs::PermissionsExt;
    let mut perms = std::fs::metadata(path)?.permissions();
    perms.set_mode(0o755);
    std::fs::set_permissions(path, perms)
}

#[cfg(not(unix))]
fn set_executable(_path: &Path) -> io::Result<()> {
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;
    use tempfile::TempDir;

    #[test]
    fn emit_writes_full_pack_layout() {
        let dir = TempDir::new().expect("tempdir");
        let out = dir.path().join(".agent-admissibility");
        let report = emit(&PackOptions {
            agents: vec!["claude-code".to_string(), "generic".to_string()],
            out_dir: out.clone(),
            scan_hash: None,
        })
        .expect("emit");

        assert!(out.join("lsp/lsp-config.json").is_file());
        assert!(out.join("hooks/claude-code/pre-edit.sh").is_file());
        assert!(out
            .join("hooks/claude-code/settings-snippet.json")
            .is_file());
        assert!(out.join("hooks/generic/pre-commit.sh").is_file());
        assert!(out
            .join("policies/no-private-namespace.shacl.ttl")
            .is_file());
        assert!(out.join("receipts/.gitkeep").is_file());
        assert!(out.join("README.md").is_file());
        assert!(!report.files_written.is_empty());
    }

    #[cfg(unix)]
    #[test]
    fn emitted_hooks_are_executable() {
        use std::os::unix::fs::PermissionsExt;
        let dir = TempDir::new().expect("tempdir");
        let out = dir.path().join(".agent-admissibility");
        emit(&PackOptions {
            agents: vec!["generic".to_string()],
            out_dir: out.clone(),
            scan_hash: None,
        })
        .expect("emit");
        let mode = std::fs::metadata(out.join("hooks/generic/pre-commit.sh"))
            .expect("metadata")
            .permissions()
            .mode();
        assert_eq!(mode & 0o111, 0o111, "hook should be executable");
    }
}
