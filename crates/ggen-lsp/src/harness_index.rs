//! Harness index: the I/O boundary for GGEN-HARNESS-001.
//!
//! Discovers a crate's declared proof topology — the `[[test]]` / `[[bench]]`
//! tables in `<root>/Cargo.toml` that carry an explicit `path = "..."` — and the
//! set of proof files that actually exist on disk under `tests/**` and
//! `benches/**`. The pure detector [`crate::analyzers::harness_mismatch_diagnostics`]
//! consumes this index and reads no files itself.
//!
//! This mirrors [`crate::project_index::ProjectIndex`] (the TPL-001 I/O boundary):
//! all filesystem reads live here, the detector stays pure.
//!
//! ## Scoping decision (GALL-CHECKPOINT-002 pre-inventory §1)
//!
//! The mismatch predicate is defined over **`Cargo.toml` explicit-`path`
//! `[[test]]`/`[[bench]]` declarations vs. on-disk files only**. `Makefile.toml`
//! task-reference shell strings are deliberately EXCLUDED from the predicate
//! (they ship green on the clean tree behind guarded probes); `Makefile.toml`
//! remains a legitimate *repair surface* in the species table, but is not a
//! detection input. This keeps the live clean tree free of false positives.

use std::collections::BTreeSet;
use std::path::{Path, PathBuf};

use walkdir::WalkDir;

use crate::analyzers::DeclaredTarget;

/// Structured errors for harness-index construction.
///
/// Construction is best-effort: a missing `Cargo.toml` is NOT an error (it
/// yields an empty index), mirroring how a non-manifest dir simply produces no
/// HARNESS diagnostics. The only error is a `Cargo.toml` that exists but cannot
/// be read/parsed at all — and even then callers treat it as "no diagnostics"
/// rather than panicking.
#[derive(Debug)]
pub enum HarnessIndexError {
    /// The `Cargo.toml` existed but could not be read from disk.
    ManifestRead {
        /// The manifest path that failed to read.
        path: PathBuf,
        /// Human-readable I/O error message.
        message: String,
    },
    /// The `Cargo.toml` existed but was not valid TOML.
    ManifestParse {
        /// The manifest path that failed to parse.
        path: PathBuf,
        /// Human-readable parse error message.
        message: String,
    },
}

impl std::fmt::Display for HarnessIndexError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            HarnessIndexError::ManifestRead { path, message } => {
                write!(f, "failed to read {}: {}", path.display(), message)
            }
            HarnessIndexError::ManifestParse { path, message } => {
                write!(f, "failed to parse {}: {}", path.display(), message)
            }
        }
    }
}

impl std::error::Error for HarnessIndexError {}

/// A resolved view of one crate's declared proof topology vs. its on-disk proof
/// files.
#[derive(Debug, Clone)]
pub struct HarnessIndex {
    /// Crate root the index was built from (holds the `Cargo.toml`).
    pub root: PathBuf,
    /// `[[test]]`/`[[bench]]` tables that carry an explicit `path`, resolved.
    pub targets: Vec<DeclaredTarget>,
    /// Existing `tests/**/*.rs` + `benches/**/*.rs` files (resolved, absolute).
    pub existing_files: BTreeSet<PathBuf>,
}

impl HarnessIndex {
    /// Build a [`HarnessIndex`] from a crate root directory.
    ///
    /// Parses `<root>/Cargo.toml` for `[[test]]`/`[[bench]]` tables with an
    /// explicit `path`, resolves each relative to `root`, and captures the
    /// 0-based declaration line of its `path = "..."` entry. Enumerates existing
    /// `tests/**/*.rs` + `benches/**/*.rs`.
    ///
    /// Best-effort and panic-free: a missing `Cargo.toml` yields an empty index
    /// (`Ok` with no targets). Resolved target paths and `existing_files` are
    /// both built from the same `root` join so the comparison is consistent
    /// (no canonicalization required).
    ///
    /// # Errors
    /// - [`HarnessIndexError::ManifestRead`] if `<root>/Cargo.toml` exists but
    ///   cannot be read.
    /// - [`HarnessIndexError::ManifestParse`] if it exists but is not valid TOML.
    pub fn from_root(root: &Path) -> Result<HarnessIndex, HarnessIndexError> {
        let manifest_path = root.join("Cargo.toml");
        let existing_files = enumerate_proof_files(root);

        if !manifest_path.is_file() {
            return Ok(HarnessIndex {
                root: root.to_path_buf(),
                targets: Vec::new(),
                existing_files,
            });
        }

        let raw = std::fs::read_to_string(&manifest_path).map_err(|e| {
            HarnessIndexError::ManifestRead {
                path: manifest_path.clone(),
                message: e.to_string(),
            }
        })?;

        let doc: toml::Value =
            toml::from_str(&raw).map_err(|e| HarnessIndexError::ManifestParse {
                path: manifest_path.clone(),
                message: e.to_string(),
            })?;

        let mut targets = Vec::new();
        for table_key in ["test", "bench"] {
            collect_targets(&doc, table_key, root, &manifest_path, &raw, &mut targets);
        }

        Ok(HarnessIndex {
            root: root.to_path_buf(),
            targets,
            existing_files,
        })
    }
}

/// Collect explicit-`path` targets from one `[[test]]`/`[[bench]]` array.
fn collect_targets(
    doc: &toml::Value, table_key: &str, root: &Path, manifest_path: &Path, raw: &str,
    out: &mut Vec<DeclaredTarget>,
) {
    let Some(toml::Value::Array(entries)) = doc.get(table_key) else {
        return;
    };
    for entry in entries {
        let Some(table) = entry.as_table() else {
            continue;
        };
        // Only entries with an explicit `path` participate in the predicate.
        let Some(path_str) = table.get("path").and_then(toml::Value::as_str) else {
            continue;
        };
        let name = table
            .get("name")
            .and_then(toml::Value::as_str)
            .unwrap_or(path_str)
            .to_string();
        let resolved = root.join(path_str);
        let line = locate_path_line(raw, path_str);
        out.push(DeclaredTarget {
            name,
            path: resolved,
            manifest: manifest_path.to_path_buf(),
            line,
        });
    }
}

/// Best-effort 0-based line of the `path = "<path_str>"` literal in the raw
/// manifest text. `toml` (serde) does not expose spans, so we scan the raw text
/// for the path literal. Returns 0 if not located (the squiggle still lands on
/// the manifest, just at its top).
fn locate_path_line(raw: &str, path_str: &str) -> u32 {
    for (idx, line) in raw.lines().enumerate() {
        let trimmed = line.trim_start();
        if trimmed.starts_with("path") && line.contains(path_str) {
            return u32::try_from(idx).unwrap_or(0);
        }
    }
    0
}

/// Enumerate existing `tests/**/*.rs` + `benches/**/*.rs` under `root`.
/// Missing dirs simply contribute nothing. Resolved as `root.join(rel)` (i.e.
/// the absolute path WalkDir yields), consistent with how target paths resolve.
fn enumerate_proof_files(root: &Path) -> BTreeSet<PathBuf> {
    let mut found = BTreeSet::new();
    for sub in ["tests", "benches"] {
        let dir = root.join(sub);
        if !dir.is_dir() {
            continue;
        }
        for entry in WalkDir::new(&dir)
            .into_iter()
            .filter_map(std::result::Result::ok)
            .filter(|e| e.file_type().is_file())
        {
            let p = entry.into_path();
            if p.extension().and_then(|e| e.to_str()) == Some("rs") {
                found.insert(p);
            }
        }
    }
    found
}

#[cfg(test)]
mod tests {
    use super::*;
    use tempfile::TempDir;

    fn write(dir: &Path, rel: &str, content: &str) {
        let p = dir.join(rel);
        if let Some(parent) = p.parent() {
            std::fs::create_dir_all(parent).expect("mkdir");
        }
        std::fs::write(p, content).expect("write");
    }

    #[test]
    fn missing_cargo_toml_yields_empty_index_not_error() {
        let dir = TempDir::new().expect("tempdir");
        let index = HarnessIndex::from_root(dir.path()).expect("best-effort ok");
        assert!(index.targets.is_empty());
        assert!(index.existing_files.is_empty());
    }

    #[test]
    fn explicit_path_test_target_with_missing_file_is_a_target() {
        let dir = TempDir::new().expect("tempdir");
        write(
            dir.path(),
            "Cargo.toml",
            r#"
[package]
name = "demo"
version = "0.1.0"

[[test]]
name = "proof"
path = "tests/proof/nonexistent.rs"
"#,
        );
        let index = HarnessIndex::from_root(dir.path()).expect("index");
        assert_eq!(index.targets.len(), 1);
        let t = &index.targets[0];
        assert_eq!(t.name, "proof");
        assert_eq!(t.path, dir.path().join("tests/proof/nonexistent.rs"));
        assert!(
            !index.existing_files.contains(&t.path),
            "declared file must NOT exist in this fixture"
        );
    }

    #[test]
    fn declaration_line_is_located() {
        let dir = TempDir::new().expect("tempdir");
        write(
            dir.path(),
            "Cargo.toml",
            "[package]\nname = \"demo\"\nversion = \"0.1.0\"\n\n[[test]]\nname = \"proof\"\npath = \"tests/proof/x.rs\"\n",
        );
        let index = HarnessIndex::from_root(dir.path()).expect("index");
        // 0-based: line 6 holds `path = ...`.
        assert_eq!(index.targets[0].line, 6);
    }

    #[test]
    fn existing_proof_file_is_enumerated() {
        let dir = TempDir::new().expect("tempdir");
        write(dir.path(), "tests/proof/run.rs", "// proof\n");
        write(dir.path(), "benches/b.rs", "// bench\n");
        let index = HarnessIndex::from_root(dir.path()).expect("index");
        assert!(index
            .existing_files
            .contains(&dir.path().join("tests/proof/run.rs")));
        assert!(index
            .existing_files
            .contains(&dir.path().join("benches/b.rs")));
    }

    #[test]
    fn path_less_target_is_ignored() {
        let dir = TempDir::new().expect("tempdir");
        write(
            dir.path(),
            "Cargo.toml",
            r#"
[package]
name = "demo"
version = "0.1.0"

[[test]]
name = "convention"
"#,
        );
        let index = HarnessIndex::from_root(dir.path()).expect("index");
        assert!(
            index.targets.is_empty(),
            "a path-less [[test]] uses convention and is not in the explicit-path predicate"
        );
    }

    #[test]
    fn invalid_toml_returns_parse_error_not_panic() {
        let dir = TempDir::new().expect("tempdir");
        write(dir.path(), "Cargo.toml", "this is not = = valid toml [[[");
        let result = HarnessIndex::from_root(dir.path());
        assert!(matches!(
            result,
            Err(HarnessIndexError::ManifestParse { .. })
        ));
    }

    #[test]
    fn bench_target_with_explicit_path_is_collected() {
        let dir = TempDir::new().expect("tempdir");
        write(
            dir.path(),
            "Cargo.toml",
            r#"
[package]
name = "demo"
version = "0.1.0"

[[bench]]
name = "slo"
path = "benches/slo.rs"
"#,
        );
        let index = HarnessIndex::from_root(dir.path()).expect("index");
        assert_eq!(index.targets.len(), 1);
        assert_eq!(index.targets[0].name, "slo");
        assert_eq!(index.targets[0].path, dir.path().join("benches/slo.rs"));
    }
}
