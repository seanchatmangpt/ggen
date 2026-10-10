//! Build-time provenance for `ggen-engine`.
//!
//! Embeds an immutable provenance digest into the compiled binary:
//! BLAKE3 over (exact commit SHA + the 6 canonical strata pack digests +
//! the strata ontology triple count).
//!
//! Fail-soft contract (crates.io / foreign builds MUST NOT break):
//! - No git (e.g. a crates.io publish): commit sha = "unknown" and
//!   [`GGEN_BUILD_SOURCE`] = "crates.io" — the published crate records its
//!   source honestly instead of faking a SHA.
//! - Strata root absent (probed at `$GGEN_STRATA_ROOT`, then
//!   `CARGO_MANIFEST_DIR/../../strata`): every absent pack contributes the
//!   documented all-zero 32-byte digest placeholder
//!   (`0000…00`, 64 hex zeros), and the ontology triple count is 0.
//! - All inputs are always hashed into [`GGEN_BUILD_PROVENANCE_HEX`];
//!   absence is represented in-band, never by a build failure.

use std::path::PathBuf;
use std::process::Command;

/// Canonical strata packs, sorted lexicographically. Absent packs hash as
/// the all-zero placeholder.
const STRATA_PACKS: [&str; 6] = [
    "strata-cas-pack",
    "strata-protocol-pack",
    "strata-signer-pack",
    "strata-stratus-pack",
    "strata-temprun-pack",
    "strata-valve-pack",
];

const ZERO_DIGEST: &str = "0000000000000000000000000000000000000000000000000000000000000000";

fn main() {
    println!("cargo:rerun-if-env-changed=GGEN_STRATA_ROOT");
    println!("cargo:rerun-if-changed=build.rs");

    let manifest_dir = PathBuf::from(std::env::var("CARGO_MANIFEST_DIR").unwrap_or_default());
    let strata_root = std::env::var("GGEN_STRATA_ROOT").ok().map(PathBuf::from).or_else(
        || {
            // Canonical checkout layout: /Users/sac/ggen/crates/ggen-engine →
            // /Users/sac (3 ancestors) → sibling /Users/sac/strata.
            manifest_dir
                .ancestors()
                .nth(3)
                .map(|p| p.join("strata"))
                .filter(|p| p.is_dir())
        },
    );

    // ── Commit SHA ──────────────────────────────────────────────────────
    let (commit_sha, source) = match Command::new("git")
        .args(["rev-parse", "HEAD"])
        .current_dir(&manifest_dir)
        .output()
    {
        Ok(out) if out.status.success() => {
            let sha = String::from_utf8_lossy(&out.stdout).trim().to_string();
            if sha.len() == 40 && sha.chars().all(|c| c.is_ascii_hexdigit()) {
                (sha, "git")
            } else {
                ("unknown".to_string(), "crates.io")
            }
        }
        _ => ("unknown".to_string(), "crates.io"),
    };
    println!("cargo:rustc-env=GGEN_BUILD_COMMIT_SHA={commit_sha}");
    println!("cargo:rustc-env=GGEN_BUILD_SOURCE={source}");

    // ── Strata pack digests ─────────────────────────────────────────────
    // Each digest is BLAKE3 hex of the pack's `pack.toml` bytes; an absent
    // pack contributes the documented zero placeholder. Canonical pack
    // sources are probed at `<strata>/packs` and, as the common on-disk
    // layout, the sibling marketplace checkout `<strata>/../ggen-marketplace/packs`.
    let pack_roots: Vec<PathBuf> = strata_root
        .as_ref()
        .map(|root| {
            let mut v = vec![root.join("packs")];
            if let Some(mkt) = root.parent().map(|p| p.join("ggen-marketplace").join("packs")) {
                v.push(mkt);
            }
            v
        })
        .unwrap_or_default();
    let pack_toml = |name: &str| -> Option<(PathBuf, Vec<u8>)> {
        pack_roots
            .iter()
            .map(|r| r.join(name).join("pack.toml"))
            .find_map(|p| std::fs::read(&p).ok().map(|b| (p, b)))
    };
    let mut pack_binding = String::new();
    let mut hasher = blake3::Hasher::new();
    hasher.update(commit_sha.as_bytes());
    for name in STRATA_PACKS {
        let (path, digest) = match pack_toml(name) {
            Some((path, bytes)) => {
                let d = blake3::hash(&bytes).to_hex().to_string();
                (path, d)
            }
            None => (PathBuf::from("build.rs"), ZERO_DIGEST.to_string()),
        };
        println!("cargo:rerun-if-changed={}", path.display());
        if !pack_binding.is_empty() {
            pack_binding.push(';');
        }
        pack_binding.push_str(name);
        pack_binding.push('=');
        pack_binding.push_str(&digest);
        hasher.update(name.as_bytes());
        hasher.update(digest.as_bytes());
    }
    println!("cargo:rustc-env=GGEN_BUILD_STRATA_PACKS={pack_binding}");

    // ── Ontology triple count ───────────────────────────────────────────
    // A real count of statement-terminating `.` lines in the strata
    // ontology, not a dossier number. Absent ontology → 0.
    let triple_count = strata_root
        .as_ref()
        .and_then(|root| std::fs::read_to_string(root.join("ontology.ttl")).ok())
        .map(|ttl| {
            ttl.lines()
                .filter(|line| !line.trim_start().starts_with('#'))
                .filter(|line| line.trim_end().ends_with('.') && !line.trim().is_empty())
                .count()
        })
        .unwrap_or(0);
    println!("cargo:rustc-env=GGEN_BUILD_ONTOLOGY_TRIPLES={triple_count}");
    if let Some(root) = &strata_root {
        println!("cargo:rerun-if-changed={}", root.join("ontology.ttl").display());
    }

    hasher.update(triple_count.to_string().as_bytes());
    let provenance_hex = hasher.finalize().to_hex().to_string();
    println!("cargo:rustc-env=GGEN_BUILD_PROVENANCE_HEX={provenance_hex}");
}
