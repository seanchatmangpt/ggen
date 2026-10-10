//! Chicago-style integration tests for `ggen pack capabilities`.
//!
//! Drives the real compiled `ggen` binary against the REAL pack corpora
//! (`/Users/sac/ggen-marketplace/packs` and `/Users/sac/ggen/packs`, both
//! annotated with `[capabilities]` provides/requires URN lists). No mocks,
//! no stubs — state-based assertions on actual stdout/stderr from the actual
//! process, plus a determinism check (two runs, byte-identical output).

#![allow(clippy::unwrap_used, clippy::expect_used)]

use assert_cmd::Command;
use serde_json::Value;
use std::path::PathBuf;

/// Resolve the real `ggen` binary: `CARGO_BIN_EXE_ggen` (set by
/// `cargo test -p ggen-cli-lib`), then the workspace
/// `target/{debug,release}/ggen`. Panics loudly if nothing resolves.
fn ggen_bin() -> PathBuf {
    if let Ok(path) = std::env::var("CARGO_BIN_EXE_ggen") {
        let p = PathBuf::from(path);
        if p.exists() {
            return p;
        }
    }

    let target_root = std::env::var_os("CARGO_TARGET_DIR")
        .map(PathBuf::from)
        .or_else(|| {
            let manifest_dir =
                std::env::var_os("CARGO_MANIFEST_DIR").map(PathBuf::from)?;
            let mut dir: &std::path::Path = manifest_dir.as_path();
            loop {
                if dir.join("Cargo.lock").exists() {
                    return Some(dir.join("target"));
                }
                match dir.parent() {
                    Some(p) => dir = p,
                    None => return None,
                }
            }
        });

    if let Some(target) = target_root {
        for profile in &["debug", "release"] {
            let candidate = target.join(profile).join("ggen");
            if candidate.is_file() {
                return candidate;
            }
        }
    }

    panic!(
        "could not resolve the `ggen` binary: CARGO_BIN_EXE_ggen unset and no \
         target/debug/ggen found; build it with `cargo build -p ggen-cli-lib --bin ggen`"
    );
}

fn run_capabilities(name: &str) -> std::process::Output {
    Command::new(ggen_bin())
        .args(["pack", "capabilities", name])
        .output()
        .expect("ggen pack capabilities must spawn")
}

fn run_capabilities_json(name: &str) -> Value {
    let out = run_capabilities(name);
    assert!(
        out.status.success(),
        "ggen pack capabilities {name} exited non-zero: {}",
        String::from_utf8_lossy(&out.stderr)
    );
    serde_json::from_slice(&out.stdout)
        .unwrap_or_else(|e| panic!("stdout was not valid JSON: {e}"))
}

/// A pack with `requires`: a2a-conformance-pack requires the
/// a2a-durability-pack and standing-ladder-pack URNs, both provided by real
/// annotated packs in the marketplace corpus.
#[test]
fn requires_are_matched_against_live_corpus() {
    let v = run_capabilities_json("a2a-conformance-pack");
    assert_eq!(v["name"], "a2a-conformance-pack");
    assert!(v["provides"]
        .as_array()
        .unwrap()
        .iter()
        .any(|u| u == "urn:ggen:pack:a2a-conformance-pack"));
    for required in [
        "urn:ggen:pack:standing-ladder-pack",
        "urn:ggen:pack:a2a-durability-pack",
    ] {
        assert!(
            v["requires"].as_array().unwrap().iter().any(|u| u == required),
            "expected {required} in requires"
        );
        let providers = v["satisfied_by"][required]
            .as_array()
            .unwrap_or_else(|| panic!("satisfied_by missing key {required}"));
        assert!(
            providers
                .iter()
                .any(|p| p == "a2a-durability-pack" || p == "standing-ladder-pack"),
            "expected a real provider pack for {required}, got {providers:?}"
        );
    }
    // Exact cross-check: the durability URN is provided by the pack of the
    // same name (read from the real corpus), so satisfied_by lists it.
    let providers: Vec<&str> = v["satisfied_by"]["urn:ggen:pack:a2a-durability-pack"]
        .as_array()
        .unwrap()
        .iter()
        .map(|p| p.as_str().unwrap())
        .collect();
    assert!(
        providers.contains(&"a2a-durability-pack"),
        "a2a-durability-pack must be a provider of its own URN, got {providers:?}"
    );
}

/// A provides-only pack: beam4pm-contracts-pack has `provides` and no
/// `requires`, so `satisfied_by` is an empty object.
#[test]
fn provides_only_pack_has_empty_satisfied_by() {
    let v = run_capabilities_json("beam4pm-contracts-pack");
    assert_eq!(v["name"], "beam4pm-contracts-pack");
    assert!(v["provides"]
        .as_array()
        .unwrap()
        .iter()
        .any(|u| u == "urn:ggen:pack:beam4pm-contracts-pack"));
    assert!(v["requires"].as_array().unwrap().is_empty());
    assert!(v["satisfied_by"].as_object().unwrap().is_empty());
}

/// A requires-only annotated pack (requires written as a bare line inside
/// the still-open `[pack]` table, per the 2026-10-09 corpus annotation
/// shape): requires must still surface and be matched, even though the
/// pack declares no `[capabilities].requires`.
///
/// NOTE on the `{ name, capabilities: null }` honest-absence branch: the
/// live corpora are currently 100% annotated (verified 2026-10-09 — every
/// pack.toml in both roots carries `[capabilities]` and/or a `requires`
/// line), so that branch has no live-corpus subject to spawn against and is
/// intentionally NOT asserted here; forcing a synthetic subject would
/// violate the real-corpus discipline of this file.
#[test]
fn requires_only_annotation_still_surfaces_and_matches() {
    let v = run_capabilities_json("clap-noun-verb-pack");
    assert_eq!(v["name"], "clap-noun-verb-pack");
    let requires: Vec<&str> = v["requires"]
        .as_array()
        .unwrap()
        .iter()
        .map(|u| u.as_str().unwrap())
        .collect();
    assert_eq!(
        requires,
        vec![
            "urn:ggen:pack:praxis-core-pack",
            "urn:ggen:pack:star-toml-pack"
        ]
    );
    for required in &requires {
        let providers = v["satisfied_by"][required]
            .as_array()
            .unwrap_or_else(|| panic!("satisfied_by missing key {required}"));
        assert!(
            !providers.is_empty(),
            "expected a real provider for {required}"
        );
    }
}

/// Unknown pack: typed, non-zero exit whose message names both searched
/// corpus roots.
#[test]
fn unknown_pack_names_searched_roots() {
    let out = run_capabilities("definitely-no-such-pack-xyz");
    assert!(!out.status.success(), "unknown pack must fail");
    let stderr = String::from_utf8_lossy(&out.stderr);
    assert!(
        stderr.contains("definitely-no-such-pack-xyz"),
        "error must name the pack: {stderr}"
    );
    assert!(
        stderr.contains("/Users/sac/ggen-marketplace/packs")
            && stderr.contains("/Users/sac/ggen/packs"),
        "error must name both corpus roots: {stderr}"
    );
}

/// Determinism: two runs over the live corpus produce byte-identical output.
#[test]
fn two_runs_identical_output() {
    let a = run_capabilities("a2a-conformance-pack");
    let b = run_capabilities("a2a-conformance-pack");
    assert!(a.status.success() && b.status.success());
    assert_eq!(a.stdout, b.stdout, "capability output must be deterministic");
}
