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
const CLAUDE_SETTINGS: &str =
    include_str!("../../assets/hooks/claude-code/settings-snippet.json");

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
}

impl Default for PackOptions {
    fn default() -> Self {
        Self {
            agents: DEFAULT_AGENTS.iter().map(|s| (*s).to_string()).collect(),
            out_dir: PathBuf::from(".agent-admissibility"),
        }
    }
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
    write_file(
        &root.join("replay/.gitkeep"),
        "",
        false,
        &mut written,
        root,
    )?;

    // README.
    write_file(&root.join("README.md"), README, false, &mut written, root)?;

    Ok(EmitReport {
        out_dir: root.to_string_lossy().to_string(),
        agents: opts.agents.clone(),
        files_written: written,
    })
}

fn lsp_config_json() -> String {
    let config = serde_json::json!({
        "law_surfaces": [
            "**/*.ttl", "**/*.nt", "**/*.nq",
            "**/*.rq", "**/*.sparql",
            "**/*.tera", "**/ggen.toml"
        ],
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

fn write_file(
    path: &Path,
    contents: &str,
    executable: bool,
    written: &mut Vec<String>,
    root: &Path,
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
        })
        .expect("emit");

        assert!(out.join("lsp/lsp-config.json").is_file());
        assert!(out.join("hooks/claude-code/pre-edit.sh").is_file());
        assert!(out.join("hooks/claude-code/settings-snippet.json").is_file());
        assert!(out.join("hooks/generic/pre-commit.sh").is_file());
        assert!(out.join("policies/no-private-namespace.shacl.ttl").is_file());
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
        })
        .expect("emit");
        let mode = std::fs::metadata(out.join("hooks/generic/pre-commit.sh"))
            .expect("metadata")
            .permissions()
            .mode();
        assert_eq!(mode & 0o111, 0o111, "hook should be executable");
    }
}
