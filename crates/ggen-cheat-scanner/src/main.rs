//! ggen-cheat-scanner binary: walks `crates/*/src/**/*.rs`,
//! `crates/*/tests/**/*.rs`, and `tests/**/*.rs` (excluding `archive/` dirs
//! and this scanner's own fixture files), reports every finding as
//! `file:line — CHEAT[RULE-ID]: message`, and exits nonzero if any are
//! found. Follows the ALIVE/BUILD_BROKEN vocabulary used by the other guard
//! scripts under `scripts/ci/`.

use ggen_cheat_scanner::{collect_impls, find_mock_substitutes, get_rules, scan_source, should_skip, Finding};
use std::fs;
use std::path::{Path, PathBuf};
use std::process;
use walkdir::WalkDir;

/// Roots to scan, relative to the workspace root this binary is invoked
/// from (matching `just`'s convention of running recipes from the repo
/// root).
fn scan_roots() -> Vec<PathBuf> {
    let mut roots = Vec::new();
    if let Ok(entries) = fs::read_dir("crates") {
        for entry in entries.flatten() {
            let p = entry.path();
            if !p.is_dir() {
                continue;
            }
            roots.push(p.join("src"));
            roots.push(p.join("tests"));
        }
    }
    roots.push(PathBuf::from("tests"));
    roots
}

fn walk_rs_files(root: &Path) -> Vec<PathBuf> {
    if !root.exists() {
        return Vec::new();
    }
    WalkDir::new(root)
        .into_iter()
        .filter_map(|e| e.ok())
        .map(|e| e.into_path())
        .filter(|p| p.extension().is_some_and(|ext| ext == "rs"))
        .filter(|p| !should_skip(p))
        .collect()
}

fn main() {
    let _rules = get_rules();
    let mut findings: Vec<Finding> = Vec::new();
    let mut total_files = 0usize;

    // Per-crate directory (crates/<name>) so T04's cross-file mock/trait
    // aggregation is scoped to "the same crate", as specified, rather than
    // the whole workspace (which would produce false positives across
    // unrelated crates that happen to share a trait name).
    let mut crate_dirs: Vec<PathBuf> = Vec::new();
    if let Ok(entries) = fs::read_dir("crates") {
        for entry in entries.flatten() {
            let p = entry.path();
            if p.is_dir() {
                crate_dirs.push(p);
            }
        }
    }

    for root in scan_roots() {
        for file in walk_rs_files(&root) {
            total_files += 1;
            match fs::read_to_string(&file) {
                Ok(src) => {
                    findings.extend(scan_source(&src, &file));
                }
                Err(e) => {
                    eprintln!("ERROR: failed to read {}: {e}", file.display());
                }
            }
        }
    }

    // T04 cross-file half: aggregate impls per crate directory.
    for crate_dir in &crate_dirs {
        let mut records = Vec::new();
        for sub in ["src", "tests"] {
            for file in walk_rs_files(&crate_dir.join(sub)) {
                if let Ok(src) = fs::read_to_string(&file) {
                    records.extend(collect_impls(&src, &file));
                }
            }
        }
        findings.extend(find_mock_substitutes(&records));
    }
    // Also aggregate the top-level tests/ tree as its own "crate" scope.
    {
        let mut records = Vec::new();
        for file in walk_rs_files(Path::new("tests")) {
            if let Ok(src) = fs::read_to_string(&file) {
                records.extend(collect_impls(&src, &file));
            }
        }
        findings.extend(find_mock_substitutes(&records));
    }

    findings.sort_by(|a, b| (&a.file, a.line, a.rule_id).cmp(&(&b.file, b.line, b.rule_id)));
    findings.dedup();

    if !findings.is_empty() {
        for f in &findings {
            eprintln!("{f}");
        }
        eprintln!(
            "\nBUILD_BROKEN: {} cheat finding(s) across {} scanned file(s). Fix before committing.",
            findings.len(),
            total_files
        );
        process::exit(1);
    }

    println!("ALIVE: no cheat patterns detected across {total_files} scanned file(s).");
}
