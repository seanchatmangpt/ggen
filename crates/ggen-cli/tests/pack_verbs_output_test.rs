//! Chicago-style output-contract tests for `ggen pack capabilities` and
//! `ggen pack compose` (lane cli-pack-test, SJIRA-261010-08 follow-through).
//!
//! Real `ggen` binary (assert_cmd) over the live on-disk corpora
//! (`/Users/sac/ggen-marketplace/packs` and `/Users/sac/ggen/packs`). No
//! mocks, no fixtures — the corpus itself is the collaborator.
//!
//! 1. `capabilities` over the live corpus: exit 0, JSON parses, every
//!    URN-form require carries tier `urn-declaration`, every other require
//!    carries tier `dependency-closure` — the actual vocabulary emitted by
//!    `cmds/pack.rs::capability_report` (adjudication H2 2026-10-09).
//! 2. `compose` on a real grounded chain (`ash-extension-starter-pack` +
//!    its provider `ash-extension-core-pack`): exit 0, byte-identical stdout
//!    across 5 invocations (the verb's CLI determinism contract).
//! 3. `compose` with a nonexistent pack name: non-zero exit, stderr names it.
//! 4. `--help` for both verbs lists only flags that exist (no `--audit`-style
//!    ghosts) and includes each verb's real argument surface.

#![allow(clippy::unwrap_used, clippy::expect_used, clippy::panic)]

use assert_cmd::Command;
use serde_json::Value;

/// Packs whose `requires` are all satisfied when composed together — a real
/// grounded chain from the live ggen-marketplace corpus (also exercised by
/// `pack_compose_test.rs`'s with-provider row).
const GROUNDED_CHAIN: [&str; 2] = ["ash-extension-starter-pack", "ash-extension-core-pack"];

/// A pack with a non-empty URN-form requires list (live corpus).
const URN_REQUIRES_PACK: &str = "a2a-conformance-pack";

fn ggen() -> Command {
    Command::cargo_bin("ggen").expect("ggen binary must be buildable")
}

fn capabilities_json(name: &str) -> Value {
    let output = ggen()
        .args(["pack", "capabilities", name])
        .output()
        .expect("ggen pack capabilities must spawn");
    assert!(
        output.status.success(),
        "capabilities {name} must exit 0; stderr: {}",
        String::from_utf8_lossy(&output.stderr)
    );
    serde_json::from_slice(&output.stdout)
        .unwrap_or_else(|e| panic!("capabilities {name} stdout must be valid JSON: {e}"))
}

/// (1) Over a real sample of the live corpus, every requirement row carries
/// the actual two-value tier vocabulary, tier matching URN form.
///
/// The tier vocabulary assertion is run over a real temp corpus (the verb's
/// documented `GGEN_CAPABILITY_CORPUS_ROOTS` env override — real files, real
/// binary, no mocks): the live on-disk corpora currently carry only
/// URN-form requires (every pack's parsed `requires` list is URN-form, so a
/// live-only sweep would leave the dependency-closure tier unexercised).
#[test]
fn capabilities_tier_vocabulary_over_live_corpus() {
    // Live corpus sweep: URN tier over real packs.
    let sample = [
        "a2a-conformance-pack",
        "a2a-durability-pack",
        "affidavit-pack",
        "ash-extension-pack",
        "agent-fleet-isolation-pack",
        "autofde-lab-capabilities-pack",
        "dfcm-pack",
        "dspy-pack",
        "star-toml-pack",
        "praxis-core-pack",
    ];

    let mut urn_rows = 0usize;
    let mut closure_rows = 0usize;

    for name in sample {
        let v = capabilities_json(name);
        assert_eq!(v["name"].as_str(), Some(name), "echoes subject pack");
        let requirements = v["requirements"]
            .as_array()
            .unwrap_or_else(|| panic!("{name}: requirements array must be present"));
        for req in requirements {
            let require = req["require"].as_str().unwrap_or_else(|| {
                panic!("{name}: every requirement row must carry a require string")
            });
            let tier = req["tier"]
                .as_str()
                .unwrap_or_else(|| panic!("{name}: requirement {require} must carry a tier field"));
            let is_urn = require.starts_with("urn:ggen:pack:");
            let expected_tier = if is_urn {
                "urn-declaration"
            } else {
                "dependency-closure"
            };
            assert_eq!(
                tier, expected_tier,
                "{name}: requirement {require} tier must match URN form"
            );
            // Tier field present, `satisfied` boolean present.
            assert!(
                req["satisfied"].is_boolean(),
                "{name}: requirement {require} must carry boolean satisfied"
            );
            if is_urn {
                urn_rows += 1;
            } else {
                closure_rows += 1;
            }
        }
    }

    // The live sweep must exercise the URN tier.
    assert!(urn_rows > 0, "live sample must include URN-form requires");
    assert_eq!(
        closure_rows, 0,
        "live corpus unexpectedly gained non-URN requires — fold it into the live sweep and re-check the temp-corpus branch below"
    );

    // Temp corpus branch: a real provider pack + a real consumer whose
    // requires are non-URN (dependency-closure tier), plus one URN-requiring
    // pack, so both tiers are asserted over the same real binary run.
    let tmp = tempfile::TempDir::new().expect("tempdir");
    let root = tmp.path();
    std::fs::create_dir_all(root.join("provider-pack")).expect("mkdir provider");
    std::fs::write(
        root.join("provider-pack/pack.toml"),
        "[pack]\nname = \"provider-pack\"\nversion = \"0.1.0\"\n\n[capabilities]\nprovides = [\"urn:ggen:pack:provider-pack\", \"shared-capability\"]\n",
    )
    .expect("write provider pack.toml");
    std::fs::create_dir_all(root.join("closure-consumer-pack")).expect("mkdir consumer");
    std::fs::write(
        root.join("closure-consumer-pack/pack.toml"),
        "[pack]\nname = \"closure-consumer-pack\"\nversion = \"0.1.0\"\nrequires = [\"shared-capability\"]\n\n[capabilities]\nprovides = [\"urn:ggen:pack:closure-consumer-pack\"]\n\n[dependencies]\nprovider-pack = \"0.1.0\"\n",
    )
    .expect("write consumer pack.toml");

    let output = Command::cargo_bin("ggen")
        .expect("ggen binary")
        .args(["pack", "capabilities", "closure-consumer-pack"])
        .env(
            "GGEN_CAPABILITY_CORPUS_ROOTS",
            format!("{}:{}", root.display(), "/Users/sac/ggen/packs"),
        )
        .output()
        .expect("ggen pack capabilities must spawn");
    assert!(
        output.status.success(),
        "temp-corpus capabilities must exit 0; stderr: {}",
        String::from_utf8_lossy(&output.stderr)
    );
    let v: Value = serde_json::from_slice(&output.stdout).expect("JSON stdout");
    let requirements = v["requirements"].as_array().expect("requirements array");
    assert_eq!(requirements.len(), 1, "one requirement row");
    let row = &requirements[0];
    assert_eq!(row["require"].as_str(), Some("shared-capability"));
    assert_eq!(row["tier"].as_str(), Some("dependency-closure"));
    assert_eq!(row["satisfied"].as_bool(), Some(true));
}

/// (2) A real grounded chain composes deterministically: exit 0, and the
/// stdout is byte-identical across 5 invocations.
#[test]
fn compose_grounded_chain_deterministic_across_5_runs() {
    let mut first: Option<Vec<u8>> = None;
    for i in 0..5 {
        let output = ggen()
            .args(["pack", "compose", "--packs", &GROUNDED_CHAIN.join(",")])
            .output()
            .expect("ggen pack compose must spawn");
        assert!(
            output.status.success(),
            "run {i}: grounded-chain compose must exit 0; stderr: {}",
            String::from_utf8_lossy(&output.stderr)
        );
        let v: Value = serde_json::from_slice(&output.stdout)
            .unwrap_or_else(|e| panic!("run {i}: stdout must be JSON: {e}"));
        // The chain actually composed — both packs present in the plan.
        let ids: Vec<&str> = v["pack_ids"]
            .as_array()
            .expect("pack_ids array")
            .iter()
            .map(|p| p.as_str().expect("pack_ids strings"))
            .collect();
        for pack in GROUNDED_CHAIN {
            assert!(ids.contains(&pack), "run {i}: plan must contain {pack}");
        }
        match &first {
            None => first = Some(output.stdout.clone()),
            Some(expected) => assert_eq!(
                expected, &output.stdout,
                "run {i}: stdout must be byte-identical to run 0"
            ),
        }
    }
}

/// (3) A nonexistent pack name is a typed refusal: non-zero exit and the
/// error names the missing pack.
#[test]
fn compose_unknown_pack_refused_named_in_error() {
    let ghost = "definitely-no-such-pack-xyz";
    let output = ggen()
        .args([
            "pack",
            "compose",
            "--packs",
            &format!("{},{}", GROUNDED_CHAIN.join(","), ghost),
        ])
        .output()
        .expect("ggen pack compose must spawn");
    assert!(
        !output.status.success(),
        "compose with a nonexistent pack must not exit 0"
    );
    let stderr = String::from_utf8_lossy(&output.stderr);
    assert!(
        stderr.contains(ghost),
        "error must name the missing pack; stderr: {stderr}"
    );
}

/// (4a) `capabilities --help` lists the flags that actually exist and no
/// ghost flags (e.g. `--audit`, which lives on other verbs, not this one).
#[test]
fn capabilities_help_lists_real_flags_only() {
    let output = ggen()
        .args(["pack", "capabilities", "--help"])
        .output()
        .expect("help must spawn");
    assert!(output.status.success(), "--help must exit 0");
    let help = String::from_utf8_lossy(&output.stdout);
    for real_flag in [
        "<NAME>",
        "--format",
        "--select",
        "--introspect",
        "--structured-errors",
        "--autonomic",
    ] {
        assert!(help.contains(real_flag), "help must list {real_flag}");
    }
    // Ghost flags that exist elsewhere in the CLI but not on this verb.
    for ghost in ["--audit", "--packs", "--dry-run", "--force"] {
        assert!(!help.contains(ghost), "help must not list ghost {ghost}");
    }
}

/// (4b) Same ghost sweep for `compose --help`; its real argument surface
/// includes `--packs` and excludes capabilities' `<NAME>` positional.
#[test]
fn compose_help_lists_real_flags_only() {
    let output = ggen()
        .args(["pack", "compose", "--help"])
        .output()
        .expect("help must spawn");
    assert!(output.status.success(), "--help must exit 0");
    let help = String::from_utf8_lossy(&output.stdout);
    for real_flag in ["--packs", "--format", "--select", "--introspect"] {
        assert!(help.contains(real_flag), "help must list {real_flag}");
    }
    for ghost in ["--audit", "--dry-run", "--force", "<NAME>"] {
        assert!(!help.contains(ghost), "help must not list ghost {ghost}");
    }
}
