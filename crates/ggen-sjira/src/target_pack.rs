//! Sync-time WORLD enforcement for WorkOrder `sj:targetPack` names — Rust
//! port of `GgenIgniter.SemanticJira.TargetPack` (ggen_igniter
//! `lib/ggen_igniter/semantic_jira/target_pack.ex`, 81L).
//!
//! The FORMAT/WORLD law split: admission (`admit_work_order`) verifies a
//! target pack NAME as FORMAT only; this module owns the WORLD half — for
//! every row declaring a `target_pack`, the pack must name a real directory
//! under `packs_root` and its content identity is stamped as a digest.
//!
//! Divergence from `GgenIgniter.PackLock.digest_checked/1` (disclosed): the
//! Elixir law walks the pack with a symlink-escape check per entry
//! (`REFUSED:PACK_SYMLINK_ESCAPE`) and an unreadable-file check
//! (`REFUSED:PACK_FILE_UNREADABLE`) before digesting. This port carries the
//! same typed refusals and refuses any entry that is a symlink or unreadable,
//! but the digest itself is sha256 over sorted `(relative path, sha256(bytes))`
//! pairs — confirm against `PackLock.digest_checked`'s exact canonical form
//! before cross-repo replay equality is claimed.

use std::collections::BTreeMap;
use std::collections::BTreeSet;
use std::path::{Path, PathBuf};

use sha2::{Digest, Sha256};

/// The typed refusal prefix (target_pack.ex:28).
pub const REFUSAL_PREFIX: &str = "REFUSED:TARGET_PACK_UNKNOWN";

/// A sync query row declaring a `target_pack` — the flattened rows of the
/// `[{query_name, rows}]` shape `enforce!/2` walks.
#[derive(Debug, Clone)]
pub struct TargetPackRow {
    /// The work order id (first row naming an unknown pack is carried in the
    /// refusal).
    pub work_order: String,
    /// The declared pack name (`sj:targetPack`).
    pub target_pack: String,
}

/// Typed refusal — Elixir raises `ArgumentError` with these prefixes; this
/// port returns typed errors carrying the same refusal text.
#[derive(Debug, thiserror::Error)]
pub enum TargetPackRefusal {
    /// A declared pack names no real directory under the pack root.
    #[error("{REFUSAL_PREFIX} pack={pack:?} work_order={work_order:?}")]
    TargetUnknown {
        /// The unknown pack name.
        pack: String,
        /// The first work order that declared it.
        work_order: String,
    },

    /// A pack entry is a symlink (Elixir `REFUSED:PACK_SYMLINK_ESCAPE`).
    #[error("REFUSED:PACK_SYMLINK_ESCAPE {0}")]
    SymlinkEscape(String),

    /// A pack entry could not be read (Elixir `REFUSED:PACK_FILE_UNREADABLE`).
    #[error("REFUSED:PACK_FILE_UNREADABLE {0}")]
    FileUnreadable(String),

    /// The pack path is not a directory.
    #[error("REFUSED:PACK_NOT_A_DIR {0}")]
    NotADir(String),
}

/// One row per distinct declared pack: resolves `<packs_root>/<pack>` and
/// digests its content, returning the stamp `{pack => digest}` (Elixir's
/// `%{pack_name => pack_digest}`). The vacuous no-op (no row declares a
/// pack) returns an empty map without touching the filesystem.
pub fn enforce(
    rows: &[TargetPackRow], packs_root: &Path,
) -> Result<BTreeMap<String, String>, TargetPackRefusal> {
    // First unknown refusal names the first row that carried the pack, in
    // row order (Elixir Map.new/Enum order over the filter).
    let mut declared: BTreeMap<&str, &str> = BTreeMap::new();
    for row in rows {
        if !row.target_pack.is_empty() {
            declared
                .entry(row.target_pack.as_str())
                .or_insert(row.work_order.as_str());
        }
    }

    let mut stamp = BTreeMap::new();
    for (pack, work_order) in declared {
        let dir = packs_root.join(pack);
        if !dir.is_dir() {
            return Err(TargetPackRefusal::TargetUnknown {
                pack: pack.to_string(),
                work_order: work_order.to_string(),
            });
        }
        let digest = digest_pack(&dir)?;
        stamp.insert(pack.to_string(), digest);
    }
    Ok(stamp)
}

/// Content identity of one pack directory: sha256 over the sorted
/// `(relative path, sha256(bytes))` pairs (see module doc for the disclosed
/// divergence from `PackLock.digest_checked/1`).
pub fn digest_pack(dir: &Path) -> Result<String, TargetPackRefusal> {
    let mut files = Vec::new();
    collect(dir, dir, &mut files)?;

    let mut hasher = Sha256::new();
    for (rel, bytes_hash) in files {
        hasher.update(rel.as_bytes());
        hasher.update([0u8]);
        hasher.update(bytes_hash.as_slice());
    }
    Ok(hex_digest(&hasher.finalize()))
}

fn hex_digest(bytes: &[u8]) -> String {
    bytes.iter().map(|b| format!("{b:02x}")).collect()
}

fn collect(
    root: &Path, dir: &Path, out: &mut Vec<(String, [u8; 32])>,
) -> Result<(), TargetPackRefusal> {
    let entries = std::fs::read_dir(dir)
        .map_err(|e| TargetPackRefusal::FileUnreadable(format!("{}: {e}", dir.display())))?;
    let mut names = BTreeSet::new();
    for entry in entries {
        names.insert(
            entry
                .map_err(|e| TargetPackRefusal::FileUnreadable(format!("{}: {e}", dir.display())))?
                .file_name(),
        );
    }
    for name in names {
        let path: PathBuf = dir.join(&name);
        let meta = std::fs::symlink_metadata(&path)
            .map_err(|e| TargetPackRefusal::FileUnreadable(format!("{}: {e}", path.display())))?;
        if meta.file_type().is_symlink() {
            return Err(TargetPackRefusal::SymlinkEscape(path.display().to_string()));
        }
        if meta.is_dir() {
            collect(root, &path, out)?;
        } else {
            let bytes = std::fs::read(&path).map_err(|e| {
                TargetPackRefusal::FileUnreadable(format!("{}: {e}", path.display()))
            })?;
            let mut h = Sha256::new();
            h.update(&bytes);
            out.push((rel_to_root(root, &path), h.finalize().into()));
        }
    }
    Ok(())
}

fn rel_to_root(root: &Path, path: &Path) -> String {
    path.strip_prefix(root)
        .map(|p| p.to_string_lossy().into_owned())
        .unwrap_or_else(|_| path.to_string_lossy().into_owned())
}

#[cfg(test)]
mod tests {
    use super::*;

    fn write_pack(root: &Path, name: &str, files: &[(&str, &str)]) {
        for (rel, content) in files {
            let p = root.join(name).join(rel);
            std::fs::create_dir_all(p.parent().unwrap()).unwrap();
            std::fs::write(p, content).unwrap();
        }
    }

    #[test]
    fn vacuous_no_op_returns_empty_stamp() {
        let stamp = enforce(&[], Path::new("/nonexistent")).unwrap();
        assert!(stamp.is_empty());
    }

    #[test]
    fn known_pack_is_stamped_and_deterministic() {
        let dir = tempfile::tempdir().unwrap();
        let root = dir.path();
        write_pack(
            root,
            "p1",
            &[("ontology.ttl", "x"), ("gates/a.rq", "SELECT 1")],
        );
        let rows = vec![TargetPackRow {
            work_order: "WO-1".into(),
            target_pack: "p1".into(),
        }];
        let stamp = enforce(&rows, root).unwrap();
        assert_eq!(stamp.len(), 1);
        let d1 = stamp["p1"].clone();
        // Digest law is deterministic and content-sensitive.
        assert_eq!(enforce(&rows, root).unwrap()["p1"], d1);
        write_pack(root, "p1", &[("gates/b.rq", "SELECT 2")]);
        assert_ne!(enforce(&rows, root).unwrap()["p1"], d1);
    }

    #[test]
    fn unknown_pack_refuses_with_first_declaring_row() {
        let dir = tempfile::tempdir().unwrap();
        let rows = vec![
            TargetPackRow {
                work_order: "WO-1".into(),
                target_pack: "no-such-pack".into(),
            },
            TargetPackRow {
                work_order: "WO-2".into(),
                target_pack: "no-such-pack".into(),
            },
        ];
        let err = enforce(&rows, dir.path()).unwrap_err();
        match &err {
            TargetPackRefusal::TargetUnknown { pack, work_order } => {
                assert_eq!(pack, "no-such-pack");
                assert_eq!(work_order, "WO-1");
            }
            _ => panic!("got {err:?}"),
        }
        assert!(err.to_string().starts_with("REFUSED:TARGET_PACK_UNKNOWN"));
    }

    #[test]
    fn symlink_in_pack_refuses_symlink_escape() {
        let dir = tempfile::tempdir().unwrap();
        let root = dir.path();
        write_pack(root, "p1", &[("ontology.ttl", "x")]);
        #[cfg(unix)]
        std::os::unix::fs::symlink("/etc/hosts", root.join("p1").join("escape.ttl")).unwrap();
        let rows = vec![TargetPackRow {
            work_order: "WO-1".into(),
            target_pack: "p1".into(),
        }];
        let err = enforce(&rows, root).unwrap_err();
        assert!(
            matches!(err, TargetPackRefusal::SymlinkEscape(_)),
            "got {err:?}"
        );
    }

    #[test]
    fn digest_excludes_nothing_and_is_stable_across_calls() {
        let dir = tempfile::tempdir().unwrap();
        write_pack(dir.path(), "p", &[("a.ttl", "aaa")]);
        let d1 = digest_pack(&dir.path().join("p")).unwrap();
        let d2 = digest_pack(&dir.path().join("p")).unwrap();
        assert_eq!(d1, d2);
    }
}
