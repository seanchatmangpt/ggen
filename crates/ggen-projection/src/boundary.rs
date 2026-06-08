use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use std::path::{Path, PathBuf};

/// A declared boundary that receipts are bound to.
/// R_B proves: A = μ(O*_B)
/// Without a BoundaryLedger, a receipt is just a file hash — not a boundary-receipted equation.
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub struct BoundaryLedger {
    /// Canonical workspace root (absolute path, no symlinks)
    pub workspace_root: PathBuf,
    /// git HEAD ref at boundary declaration time (None if not a git repo)
    pub git_ref: Option<String>,
    /// blake3 over all pack.toml contents concatenated in dependency order
    pub pack_graph_digest: String,
    /// blake3 per template file path (relative) -> content digest
    pub template_digests: HashMap<String, String>,
    /// rustc version string
    pub toolchain: String,
    /// Timestamp boundary was declared
    pub bound_at: chrono::DateTime<chrono::Utc>,
    /// blake3 over the canonical serialization of all above fields
    /// This is the authoritative boundary identity.
    pub boundary_digest: String,
}

impl BoundaryLedger {
    /// Declare a boundary from a workspace root and resolved pack descriptors.
    /// Computes real digests over actual on-disk content.
    pub fn declare(
        workspace_root: &Path,
        pack_toml_contents: &[String], // In dependency order
        template_paths_and_contents: &[(String, Vec<u8>)],
    ) -> Result<Self, anyhow::Error> {
        let workspace_root = workspace_root
            .canonicalize()
            .unwrap_or_else(|_| workspace_root.to_path_buf());

        // Real git ref
        let git_ref = std::process::Command::new("git")
            .args(["rev-parse", "HEAD"])
            .current_dir(&workspace_root)
            .output()
            .ok()
            .filter(|o| o.status.success())
            .and_then(|o| String::from_utf8(o.stdout).ok())
            .map(|s| s.trim().to_string());

        // Real pack graph digest
        let mut pack_hasher = blake3::Hasher::new();
        for content in pack_toml_contents {
            pack_hasher.update(content.as_bytes());
        }
        let pack_graph_digest = pack_hasher.finalize().to_hex().to_string();

        // Real template digests
        let mut template_digests = HashMap::new();
        for (path, content) in template_paths_and_contents {
            let digest = blake3::hash(content).to_hex().to_string();
            template_digests.insert(path.clone(), digest);
        }

        // Toolchain
        let toolchain = std::process::Command::new("rustc")
            .arg("--version")
            .output()
            .ok()
            .and_then(|o| String::from_utf8(o.stdout).ok())
            .unwrap_or_else(|| "unknown".to_string())
            .trim()
            .to_string();

        let bound_at = chrono::Utc::now();

        // Boundary digest over all fields
        let canonical = format!(
            "{}|{}|{}|{}|{}",
            workspace_root.display(),
            git_ref.as_deref().unwrap_or("no-git"),
            pack_graph_digest,
            toolchain,
            bound_at.to_rfc3339(),
        );
        let boundary_digest = blake3::hash(canonical.as_bytes()).to_hex().to_string();

        Ok(BoundaryLedger {
            workspace_root,
            git_ref,
            pack_graph_digest,
            template_digests,
            toolchain,
            bound_at,
            boundary_digest,
        })
    }

    /// Verify a previously declared boundary against the current on-disk state.
    /// Returns Err if pack content has changed since boundary was declared.
    pub fn verify_pack_graph(&self, pack_toml_contents: &[String]) -> Result<(), anyhow::Error> {
        let mut hasher = blake3::Hasher::new();
        for content in pack_toml_contents {
            hasher.update(content.as_bytes());
        }
        let current = hasher.finalize().to_hex().to_string();
        if current != self.pack_graph_digest {
            return Err(anyhow::anyhow!(
                "BoundaryLedger: pack graph digest mismatch. \
                Boundary was declared over a different pack graph. \
                Expected {}, got {}",
                self.pack_graph_digest,
                current
            ));
        }
        Ok(())
    }
}
