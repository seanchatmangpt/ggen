#![allow(
    clippy::unwrap_used,
    clippy::expect_used,
    clippy::panic,
    clippy::needless_raw_string_hashes,
    clippy::duration_suboptimal_units,
    clippy::branches_sharing_code,
    clippy::used_underscore_binding,
    clippy::single_char_pattern,
    clippy::ignore_without_reason,
    clippy::cloned_ref_to_slice_refs,
    clippy::doc_overindented_list_items,
    clippy::match_wildcard_for_single_variants,
    clippy::ignored_unit_patterns,
    clippy::needless_collect,
    clippy::unnecessary_map_or,
    clippy::manual_flatten,
    clippy::manual_strip,
    clippy::future_not_send,
    clippy::unnested_or_patterns,
    clippy::no_effect_underscore_binding,
    clippy::literal_string_with_formatting_args
)]
use std::error::Error;
use std::fs;
use std::path::{Path, PathBuf};

/// Recursively collect all `.rs` files under `dir`, excluding `exclude_filename` if specified.
fn collect_rs_files(
    dir: &Path, exclude_filename: Option<&str>,
) -> Result<Vec<PathBuf>, Box<dyn Error>> {
    let mut files = Vec::new();
    if !dir.exists() {
        return Ok(files);
    }
    for entry in fs::read_dir(dir)? {
        let entry = entry?;
        let path = entry.path();
        if path.is_dir() {
            files.extend(collect_rs_files(&path, exclude_filename)?);
        } else if path.is_file() && path.extension().is_some_and(|ext| ext == "rs") {
            if let Some(exclude) = exclude_filename {
                if path.file_name().is_some_and(|n| n == exclude) {
                    continue;
                }
            }
            files.push(path);
        }
    }
    Ok(files)
}

/// Scans library source (src/ excluding src/bin/) and tests/ for forbidden surface patterns.
/// Binary files in src/bin/ are boundary adapters and are ALLOWED to use std::process::Command
/// because they are the executors — not library code under test.
/// Library code (src/lib.rs, src/*.rs, src/*/mod.rs) must NEVER spawn subprocesses.
#[test]
fn test_forbidden_surfaces_scan() -> Result<(), Box<dyn Error>> {
    let manifest_dir = Path::new(env!("CARGO_MANIFEST_DIR"));
    let src_dir = manifest_dir.join("src");
    let tests_dir = manifest_dir.join("tests");
    let bin_dir = manifest_dir.join("src").join("bin");

    // Patterns that indicate unsafe or forbidden constructs in library code.
    // Per AGENTS.md constitution: no subprocess spawning, no raw networking,
    // no HTTP client calls from within the deterministic graph library crate.
    let forbidden_patterns = [
        "std::process::Command",
        "Command::new",
        "std::net",
        "tokio::net",
        "reqwest",
        "hyper",
    ];

    let mut violations = Vec::new();

    // Collect .rs files from src/ EXCLUDING src/bin/ (boundary adapter binaries)
    // and from tests/ EXCLUDING this file itself.
    let mut all_files: Vec<PathBuf> = Vec::new();
    for entry in fs::read_dir(&src_dir)? {
        let entry = entry?;
        let path = entry.path();
        if path.is_file() && path.extension().is_some_and(|ext| ext == "rs") {
            all_files.push(path);
        } else if path.is_dir() && path != bin_dir {
            // Include subdirs (e.g. src/ocel/, src/graph/) but not src/bin/
            all_files.extend(collect_rs_files(&path, None)?);
        }
    }
    all_files.extend(collect_rs_files(&tests_dir, Some("forbidden_surface.rs"))?);

    assert!(
        !all_files.is_empty(),
        "No source files found to scan under {:?} and {:?}",
        src_dir,
        tests_dir
    );

    // Track how many files were scanned (must be non-trivial)
    let scanned_count = all_files.len();

    for path in &all_files {
        let content = fs::read_to_string(path)?;
        for pattern in &forbidden_patterns {
            if content.contains(pattern) {
                violations.push(format!(
                    "File {:?} contains forbidden pattern: '{}'",
                    path.strip_prefix(manifest_dir).unwrap_or(path),
                    pattern
                ));
            }
        }
    }

    // Prove the scan was non-trivial — must scan at least 3 source files
    assert!(
        scanned_count >= 3,
        "Forbidden surface scan is too shallow: only {} files scanned.",
        scanned_count
    );

    if !violations.is_empty() {
        for violation in &violations {
            eprintln!("{}", violation);
        }
        return Err(format!(
            "Forbidden surfaces check failed: {} violations found across {} files scanned",
            violations.len(),
            scanned_count
        )
        .into());
    }

    Ok(())
}

/// Negative test: Verify the scanner correctly detects a forbidden pattern in
/// an in-memory file (proves it doesn't trivially pass everything).
#[test]
fn test_forbidden_surfaces_scanner_detects_violation() {
    let forbidden_patterns = [
        "std::process::Command",
        "Command::new",
        "std::net",
        "tokio::net",
        "reqwest",
        "hyper",
    ];

    // Synthesize content that should be caught
    let fake_content = r#"
        fn launch_process() {
            let _child = std::process::Command::new("sh").arg("-c").arg("ls").spawn();
        }
    "#;

    let mut found = Vec::new();
    for pattern in &forbidden_patterns {
        if fake_content.contains(pattern) {
            found.push(*pattern);
        }
    }

    assert!(
        !found.is_empty(),
        "Scanner failed to detect 'std::process::Command' in synthetic content — the scanner itself is broken"
    );
    assert!(
        found.contains(&"std::process::Command"),
        "Expected 'std::process::Command' to be flagged, got: {:?}",
        found
    );
}
