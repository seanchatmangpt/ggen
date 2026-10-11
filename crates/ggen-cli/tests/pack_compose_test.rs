#![allow(clippy::unwrap_used, clippy::expect_used, clippy::panic)] // Chicago TDD: real-IO tests
//! Chicago-style integration tests for `ggen pack compose`.
//!
//! Drives the real compiled `ggen` binary against the REAL pack corpora
//! (`/Users/sac/ggen-marketplace/packs` and `/Users/sac/ggen/packs`). No
//! mocks, no stubs — state-based assertions on actual stdout/stderr from the
//! actual process, plus a determinism check (two runs, byte-identical
//! stdout).

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
            let manifest_dir = std::env::var_os("CARGO_MANIFEST_DIR").map(PathBuf::from)?;
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

fn run_compose(packs: &[&str]) -> std::process::Output {
    let mut cmd = Command::new(ggen_bin());
    cmd.arg("pack").arg("compose");
    if !packs.is_empty() {
        // The generated clap surface takes `--packs <PACKS>` with a
        // comma-delimited value; repeating the flag overwrites, not appends.
        cmd.arg("--packs").arg(packs.join(","));
    }
    cmd.output().expect("failed to run ggen pack compose")
}

/// (a) Two known-compatible packs compose into a plan containing both.
#[test]
fn compose_two_compatible_packs_returns_plan_with_both() {
    let output = run_compose(&["dfcm-pack", "dspy-pack"]);
    assert!(
        output.status.success(),
        "compose of two dependency-free packs must succeed; stderr: {}",
        String::from_utf8_lossy(&output.stderr)
    );

    let stdout = String::from_utf8_lossy(&output.stdout);
    let value: Value = serde_json::from_str(stdout.trim())
        .unwrap_or_else(|e| panic!("stdout is not JSON ({e}): {stdout}"));

    let ids = value["pack_ids"].as_array().expect("pack_ids array");
    let id_strs: Vec<&str> = ids.iter().filter_map(|v| v.as_str()).collect();
    assert!(
        id_strs.contains(&"dfcm-pack"),
        "plan missing dfcm-pack: {value}"
    );
    assert!(
        id_strs.contains(&"dspy-pack"),
        "plan missing dspy-pack: {value}"
    );

    // Both self-URNs must appear in the provides map.
    let provides = value["provides"].as_object().expect("provides object");
    assert!(
        provides.contains_key("urn:ggen:pack:dfcm-pack"),
        "provides: {provides:?}"
    );
    assert!(
        provides.contains_key("urn:ggen:pack:dspy-pack"),
        "provides: {provides:?}"
    );

    // order covers exactly the composed set.
    let order = value["order"].as_array().expect("order array");
    assert_eq!(order.len(), 2, "order: {order:?}");

    // self_satisfied is projected: empty for a clean dependency-free compose.
    let self_satisfied = value["self_satisfied"]
        .as_array()
        .expect("self_satisfied array must be present in the compose JSON projection");
    assert!(
        self_satisfied.is_empty(),
        "dependency-free 2-pack compose must have empty self_satisfied: {self_satisfied:?}"
    );
}

/// (b) The same pack twice is a typed error, non-zero exit, refusal on stderr.
#[test]
fn compose_duplicate_pack_name_is_typed_error() {
    let output = run_compose(&["dfcm-pack", "dfcm-pack"]);
    assert!(
        !output.status.success(),
        "duplicate pack name must fail; stdout: {}",
        String::from_utf8_lossy(&output.stdout)
    );
    let stderr = String::from_utf8_lossy(&output.stderr);
    assert!(
        stderr.contains("duplicate pack 'dfcm-pack'"),
        "stderr must name the duplicate typed error; got: {stderr}"
    );
}

/// (c) An unknown pack name is a typed error naming the searched roots.
#[test]
fn compose_unknown_pack_names_searched_roots() {
    let output = run_compose(&["dfcm-pack", "no-such-pack-anywhere-xyz"]);
    assert!(
        !output.status.success(),
        "unknown pack must fail; stdout: {}",
        String::from_utf8_lossy(&output.stdout)
    );
    let stderr = String::from_utf8_lossy(&output.stderr);
    assert!(
        stderr.contains("no-such-pack-anywhere-xyz"),
        "stderr must name the missing pack; got: {stderr}"
    );
    assert!(
        stderr.contains("/Users/sac/ggen-marketplace/packs")
            && stderr.contains("/Users/sac/ggen/packs"),
        "stderr must name both searched corpus roots; got: {stderr}"
    );
}

/// (d) A requires-bearing pack composed WITHOUT its provider is refused.
/// `ash-extension-starter-pack` declares `capabilities.requires =
/// ["urn:ggen:pack:ash-extension-core-pack"]` (an in-corpus provider).
/// Composing only the consumer must fail with the kernel's refusal text on
/// stderr; adding the provider flips it to success.
///
/// Note: top-level `requires = [...]` keys (outside `[capabilities]`, e.g.
/// a2a-hex-migration-pack) are NOT part of the kernel's requires universe —
/// composing a2a-hex-migration-pack alone succeeds. That corpus/model gap is
/// recorded in the SJIRA-08 note; this test pins the capabilities.requires
/// channel that the kernel does honor.
#[test]
fn compose_requires_bearing_pack_without_provider_is_refused() {
    let output = run_compose(&["ash-extension-starter-pack"]);
    assert!(
        !output.status.success(),
        "composing a pack without its required provider must be refused; stdout: {}",
        String::from_utf8_lossy(&output.stdout)
    );
    let stderr = String::from_utf8_lossy(&output.stderr);
    assert!(
        stderr.contains("composition refused"),
        "stderr must carry the composition refusal; got: {stderr}"
    );
    assert!(
        stderr.contains("urn:ggen:pack:ash-extension-core-pack"),
        "refusal must name the unbound requirement; got: {stderr}"
    );

    // The provider present flips the refusal to success.
    let with_provider = run_compose(&["ash-extension-starter-pack", "ash-extension-core-pack"]);
    assert!(
        with_provider.status.success(),
        "composing consumer + provider must succeed; stderr: {}",
        String::from_utf8_lossy(&with_provider.stderr)
    );
}

/// Determinism: two identical compose runs produce byte-identical stdout,
/// INCLUDING the `order` array. The kernel's topological tie-break among
/// dependency-free packs is unstable across runs (SJIRA-08), so the verb's
/// JSON projection canonicalizes `order` (sorted ascending) before
/// serialization. This pins raw-stdout determinism: no canonicalization in
/// the test — the bytes must match.
#[test]
fn compose_is_deterministic_across_runs() {
    let a = run_compose(&["dfcm-pack", "dspy-pack"]);
    assert!(
        a.status.success(),
        "first run failed: {}",
        String::from_utf8_lossy(&a.stderr)
    );
    let b = run_compose(&["dfcm-pack", "dspy-pack"]);
    assert!(
        b.status.success(),
        "second run failed: {}",
        String::from_utf8_lossy(&b.stderr)
    );

    // The order field itself must be present and identical, not just the
    // canonicalized remainder.
    let va: Value = serde_json::from_slice(&a.stdout).expect("run A is JSON");
    let vb: Value = serde_json::from_slice(&b.stdout).expect("run B is JSON");
    assert_eq!(
        va["order"], vb["order"],
        "order array must be identical across runs"
    );
    assert!(
        va["order"].as_array().map_or(false, |o| {
            let strs: Vec<&str> = o.iter().filter_map(|x| x.as_str()).collect();
            strs.windows(2).all(|w| w[0] <= w[1])
        }),
        "order must be sorted ascending; got: {}",
        va["order"]
    );

    assert!(
        va["self_satisfied"].as_array().is_some(),
        "self_satisfied must be present and identical-shaped across runs; got: {}",
        va["self_satisfied"]
    );
    assert_eq!(
        va["self_satisfied"], vb["self_satisfied"],
        "self_satisfied must be identical across runs"
    );

    // Full raw stdout: byte-identical across the two runs.
    assert_eq!(
        a.stdout, b.stdout,
        "raw stdout must be byte-identical across identical compose runs"
    );
}
