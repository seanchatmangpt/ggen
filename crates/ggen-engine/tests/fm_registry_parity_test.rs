//! FM registry parity court (SA-16, closes XG-2 GAP-2).
//!
//! `docs/FM_REGISTRY.md` claims one row per statically observable failure-mode
//! code in the workspace. This court re-derives the census from the real tree
//! on every run — the same shape as
//! `crates/ggen-config/tests/system_crate_map_parity_test.rs`: plain line
//! scanning of real files, no mocks, set equality asserted in BOTH directions.
//!
//! Census definition (mirrors the registry's own header): a code is observable
//! iff it appears in `crates/ggen-engine/src/**` or `crates/ggen-cli/src/**` as
//! (a) a static literal `FM-<FAM>-<3 digits>` at a word boundary, or (b) a
//! parametric numeric call site `fm_<fam>(N,` for one of the twelve known
//! families minted via `AppError::fm_*` (`crates/ggen-engine/src/error.rs`).
//!
//! Non-vacuity: the census parser is positively witnessed before use (it must
//! find codes known to exist), the registry parser must find a non-empty row
//! set, and a divergence fails with the exact codes named.

#![allow(clippy::unwrap_used, clippy::expect_used, clippy::panic)]

use std::collections::BTreeSet;
use std::fs;
use std::path::{Path, PathBuf};

fn repository_root() -> PathBuf {
    Path::new(env!("CARGO_MANIFEST_DIR"))
        .parent()
        .and_then(Path::parent)
        .expect("ggen-engine must be located under <repo>/crates/ggen-engine")
        .to_path_buf()
}

/// All `.rs` files under `dir`, recursively.
fn rs_files(dir: &Path, out: &mut Vec<PathBuf>) {
    let entries = match fs::read_dir(dir) {
        Ok(e) => e,
        Err(_) => return,
    };
    for entry in entries.flatten() {
        let path = entry.path();
        if path.is_dir() {
            rs_files(&path, out);
        } else if path.extension().and_then(|e| e.to_str()) == Some("rs") {
            out.push(path);
        }
    }
}

/// Extract static literal codes from one line: `FM-<UPPERCASE>-<digits>` where
/// the digits are followed by a non-alphanumeric character (word boundary).
/// This keeps the doc-comment wildcard `FM-CHAIN-0NN` from minting a phantom
/// `FM-CHAIN-0` (witnessed: `crates/ggen-engine/src/verbs/handlers.rs:427`).
fn static_codes_in_line(line: &str) -> Vec<String> {
    let b = line.as_bytes();
    let mut codes = Vec::new();
    let mut i = 0;
    'outer: while let Some(pos) = line[i..].find("FM-") {
        let start = i + pos;
        let mut j = start + 3;
        let fam_start = j;
        while j < b.len() && b[j].is_ascii_uppercase() {
            j += 1;
        }
        if j == fam_start || j >= b.len() || b[j] != b'-' {
            i = start + 3;
            continue;
        }
        j += 1;
        let num_start = j;
        while j < b.len() && b[j].is_ascii_digit() {
            j += 1;
        }
        // End-of-input is a word boundary too (a code may end the line).
        if j == num_start || (j < b.len() && b[j].is_ascii_alphanumeric()) {
            i = start + 3;
            continue 'outer;
        }
        codes.push(line[start..j].to_string());
        i = j;
    }
    codes
}

/// Extract parametric numeric call sites from one line: `fm_<fam>(N` for one
/// of the twelve `AppError::fm_*` families; normalizes to `FM-<FAM>-NNN`.
fn param_codes_in_line(line: &str) -> Vec<String> {
    const FAMILIES: [&str; 12] = [
        "cli", "chain", "graph", "tpl", "write", "pack", "config", "watch", "shell", "law", "gen",
        "key",
    ];
    let mut codes = Vec::new();
    for fam in FAMILIES {
        let needle = format!("fm_{fam}(");
        let mut from = 0;
        while let Some(rel) = line[from..].find(&needle) {
            let start = from + rel + needle.len();
            let rest = &line[start..];
            let digits: String = rest.chars().take_while(|c| c.is_ascii_digit()).collect();
            if !digits.is_empty() {
                if let Ok(n) = digits.parse::<u32>() {
                    codes.push(format!("FM-{}-{:03}", fam.to_uppercase(), n));
                }
            }
            from = start;
        }
    }
    codes
}

/// Re-derive the census from the real source tree (both minting mechanisms).
fn census() -> BTreeSet<String> {
    let root = repository_root();
    let mut files = Vec::new();
    rs_files(&root.join("crates/ggen-engine/src"), &mut files);
    rs_files(&crates_cli_src(&root), &mut files);
    let mut codes = BTreeSet::new();
    for f in &files {
        let text =
            fs::read_to_string(f).unwrap_or_else(|e| panic!("failed to read {}: {e}", f.display()));
        for line in text.lines() {
            codes.extend(static_codes_in_line(line));
            codes.extend(param_codes_in_line(line));
        }
    }
    codes
}

fn crates_cli_src(root: &Path) -> PathBuf {
    root.join("crates/ggen-cli/src")
}

/// Parse `docs/FM_REGISTRY.md` and return the set of row codes: every table row
/// whose first cell is `` `FM-<FAM>-NNN` ``.
fn registry_codes() -> BTreeSet<String> {
    let path = repository_root().join("docs/FM_REGISTRY.md");
    let text = fs::read_to_string(&path)
        .unwrap_or_else(|e| panic!("failed to read {}: {e}", path.display()));
    let mut codes = BTreeSet::new();
    for line in text.lines() {
        let t = line.trim();
        if !t.starts_with("| `FM-") {
            continue;
        }
        let cell = t.trim_start_matches("| `");
        let Some(end) = cell.find('`') else {
            continue;
        };
        let code = &cell[..end];
        if static_codes_in_line(code) == vec![code.to_string()]
            || param_codes_in_line(&format!("fm_{}", code.to_lowercase()))
                .contains(&code.to_string())
        {
            codes.insert(code.to_string());
        }
    }
    codes
}

#[test]
fn census_parser_is_positively_witnessed() {
    let c = census();
    assert!(
        c.contains("FM-CLI-001"),
        "census parser missed known static literal FM-CLI-001 (error.rs:186): {c:?}"
    );
    assert!(
        c.contains("FM-CHAIN-002"),
        "census parser missed known parametric site fm_chain(2 (sync.rs:4569)): {c:?}"
    );
    assert!(
        c.contains("FM-CHAIN-007"),
        "census parser missed known parametric site fm_chain(7 (error.rs:574)): {c:?}"
    );
    assert!(
        !c.contains("FM-CHAIN-0"),
        "census parser minted the phantom FM-CHAIN-0 from the FM-CHAIN-0NN doc wildcard"
    );
    assert!(c.len() >= 100, "census suspiciously small: {}", c.len());
}

#[test]
fn registry_parser_is_positively_witnessed() {
    let r = registry_codes();
    assert!(
        r.contains("FM-CONFIG-001") && r.contains("FM-WRITE-002") && r.contains("FM-TPL-025"),
        "registry parser missed known rows: {r:?}"
    );
    assert!(r.len() >= 100, "registry suspiciously small: {}", r.len());
}

#[test]
fn registry_rows_match_census_in_both_directions() {
    let census = census();
    let registry = registry_codes();
    let missing_rows: Vec<_> = census.difference(&registry).collect();
    let stale_rows: Vec<_> = registry.difference(&census).collect();
    assert!(
        missing_rows.is_empty(),
        "FM code(s) emitted in ggen-engine/src|ggen-cli/src but MISSING from \
         docs/FM_REGISTRY.md: {missing_rows:?}. Add one row per code (trigger + \
         remediation from the emission site's context), then re-run."
    );
    assert!(
        stale_rows.is_empty(),
        "FM_REGISTRY.md row(s) with no emission site in the tree (stale registry, \
         real Contract Drift): {stale_rows:?}. Remove the row or restore the \
         emission site, then re-run."
    );
}
