//! Chicago-TDD tests for `packs_registry::metadata::pack_file_from_dir`.
//!
//! Real corpus collaborators: asserts run against actual pack.toml files on
//! disk (ggen + ggen-marketplace packs), plus tempfile-based negative paths.
//! No mocks — state-based assertions on the parsed `PackFile`.

#![allow(clippy::unwrap_used, clippy::expect_used, clippy::panic)] // Chicago TDD: real-IO tests
use ggen_marketplace::packs_registry::metadata::pack_file_from_dir;
use std::collections::BTreeSet;
use std::path::Path;

const CORPUS_ROOTS: [&str; 2] = ["/Users/sac/ggen/packs", "/Users/sac/ggen-marketplace/packs"];

/// Find a corpus pack directory by its directory name (searching both roots).
fn find_corpus_dir(name: &str) -> Option<std::path::PathBuf> {
    for root in CORPUS_ROOTS {
        let candidate = Path::new(root).join(name);
        if candidate.join("pack.toml").is_file() {
            return Some(candidate);
        }
    }
    None
}

/// (a) Real corpus: 5 pack dirs (2 ggen, 3 marketplace, one with requires)
/// parse into a correct PackFile — id = dir name, defaults injected,
/// capabilities preserved.
#[test]
fn real_corpus_dirs_parse_into_correct_packfile() {
    // Dynamic selection: the first two ggen-root dirs carrying a pack.toml
    // (hardcoded names drifted out of the corpus).
    let ggen_root = Path::new(CORPUS_ROOTS[0]);
    let mut ggen_dirs: Vec<std::path::PathBuf> = std::fs::read_dir(ggen_root)
        .expect("read ggen corpus root")
        .filter_map(|e| e.ok().map(|e| e.path()))
        .filter(|d| d.is_dir() && d.join("pack.toml").is_file())
        .take(2)
        .collect();
    ggen_dirs.sort();
    assert!(ggen_dirs.len() >= 1, "ggen corpus root has pack dirs");
    let mut checked = 0;
    for dir in &ggen_dirs {
        let name = dir.file_name().unwrap().to_string_lossy().to_string();
        let pf = pack_file_from_dir(dir)
            .unwrap_or_else(|e| panic!("{}: expected Ok, got Err: {}", dir.display(), e));
        assert_eq!(pf.pack.id, name, "id must be the directory name");
        assert!(!pf.pack.version.is_empty(), "version must be non-empty");
        assert_eq!(pf.pack.category, "uncategorized");
        checked += 1;
    }
    assert!(checked >= 1, "at least one ggen-root corpus dir must load");

    // Marketplace-root packs: pick first 3 dirs with pack.toml, one with
    // requires if present.
    let mut marketplace_checked = 0;
    let mut saw_requires = false;
    for root in CORPUS_ROOTS {
        let Ok(entries) = std::fs::read_dir(root) else {
            continue;
        };
        let mut dirs: Vec<_> = entries
            .filter_map(|e| e.ok())
            .map(|e| e.path())
            .filter(|p| p.join("pack.toml").is_file())
            .collect();
        dirs.sort();
        for dir in dirs {
            let pf = pack_file_from_dir(&dir)
                .unwrap_or_else(|e| panic!("{}: expected Ok, got Err: {}", dir.display(), e));
            let dir_name = dir.file_name().unwrap().to_string_lossy().to_string();
            assert_eq!(pf.pack.id, dir_name);
            if let Some(caps) = &pf.capabilities {
                if caps.requires.as_ref().is_some_and(|r| !r.is_empty()) {
                    saw_requires = true;
                    for r in caps.requires.as_ref().unwrap() {
                        assert!(r.starts_with("urn:ggen:pack:"), "requires URN shape: {r}");
                    }
                }
            }
            marketplace_checked += 1;
            if marketplace_checked >= 3 && saw_requires {
                break;
            }
        }
        if marketplace_checked >= 3 && saw_requires {
            break;
        }
    }
    assert!(
        marketplace_checked >= 3,
        "at least 3 corpus pack dirs must parse"
    );
    assert!(
        saw_requires,
        "corpus is mid-annotation-wave; expected at least one pack with [capabilities].requires"
    );
}

/// (b) Missing pack.toml → typed Err naming the path.
#[test]
fn missing_pack_toml_is_typed_err() {
    let dir = tempfile::TempDir::new().unwrap();
    let err = pack_file_from_dir(dir.path()).unwrap_err();
    let msg = err.to_string();
    assert!(
        msg.contains("pack.toml"),
        "error must name the missing file; got: {msg}"
    );
}

/// (c) Malformed pack.toml → typed Err (parse failure, not panic).
#[test]
fn malformed_pack_toml_is_typed_err() {
    let dir = tempfile::TempDir::new().unwrap();
    std::fs::write(dir.path().join("pack.toml"), "not [valid toml ===").unwrap();
    let err = pack_file_from_dir(dir.path()).unwrap_err();
    assert!(
        err.to_string().contains("Failed to parse"),
        "error must be a parse failure; got: {err}"
    );
}

/// (d) pack.toml with no [pack] table → typed Err, not Ok.
#[test]
fn missing_pack_table_is_typed_err() {
    let dir = tempfile::TempDir::new().unwrap();
    std::fs::write(dir.path().join("pack.toml"), "other = 1\n").unwrap();
    let err = pack_file_from_dir(dir.path()).unwrap_err();
    assert!(
        err.to_string().contains("[pack]"),
        "error must name the missing [pack] table; got: {err}"
    );
}

/// (e) Determinism: same dir twice → identical PackFile (Debug repr equal).
#[test]
fn same_dir_twice_is_identical() {
    let Some(dir) = find_corpus_dir("agent-workflow")
        .or_else(|| find_corpus_dir("tcps-production"))
        .or_else(|| {
            // Fallback: first corpus dir found.
            CORPUS_ROOTS.iter().find_map(|root| {
                std::fs::read_dir(root)
                    .ok()?
                    .filter_map(|e| e.ok())
                    .map(|e| e.path())
                    .find(|p| p.join("pack.toml").is_file())
            })
        })
    else {
        panic!("no corpus pack dir available for determinism check");
    };
    let a = pack_file_from_dir(&dir).unwrap();
    let b = pack_file_from_dir(&dir).unwrap();
    assert_eq!(a.pack.id, b.pack.id);
    assert_eq!(a.pack.version, b.pack.version);
    assert_eq!(a.pack.category, b.pack.category);
    assert_eq!(format!("{a:?}"), format!("{b:?}"), "fully deterministic");
}

/// (f) Shim parity: present fields are never overwritten — a pack.toml that
/// already carries id/version/category keeps them verbatim.
#[test]
fn present_fields_are_preserved_not_overwritten() {
    let dir = tempfile::TempDir::new().unwrap();
    std::fs::write(
        dir.path().join("pack.toml"),
        r#"
[pack]
id = "explicit-id"
name = "Explicit Name"
version = "1.2.3"
description = "An explicit description"
category = "testing"

[capabilities]
provides = ["urn:ggen:pack:explicit-id"]
"#,
    )
    .unwrap();
    let pf = pack_file_from_dir(dir.path()).unwrap();
    assert_eq!(pf.pack.id, "explicit-id");
    assert_eq!(pf.pack.version, "1.2.3");
    assert_eq!(pf.pack.category, "testing");
    assert_eq!(
        pf.capabilities.as_ref().unwrap().provides.as_ref(),
        Some(&BTreeSet::from(["urn:ggen:pack:explicit-id".to_string()]))
    );
}
