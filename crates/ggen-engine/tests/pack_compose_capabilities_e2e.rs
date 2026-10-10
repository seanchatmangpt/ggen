//! End-to-end tests for the capability-aware `ggen pack compose` path.
//!
//! Two layers exercised, both real:
//!
//! 1. The real CLI binary (`assert_cmd::Command::cargo_bin("ggen")`) against
//!    the live marketplace corpus (`/Users/sac/ggen-marketplace/packs`) and
//!    against a real temp corpus via the `GGEN_CAPABILITY_CORPUS_ROOTS`
//!    override — never a mock transport.
//! 2. The deterministic composition kernel
//!    (`ggen_marketplace::packs_registry::composer::compose`) directly, for
//!    assertions the CLI cannot express (its wire layer sorts `order` before
//!    emitting, so topological provider-before-consumer placement is only
//!    observable at the kernel boundary).
//!
//! No mocks: every refusal fixture is a real directory tree parsed by the
//! real `pack_file_from_dir` loader.

use assert_cmd::Command;
use ggen_marketplace::packs_registry::composer::compose;
use ggen_marketplace::packs_registry::metadata::pack_file_from_dir;
use ggen_marketplace::packs_registry::types::PackFile;
use std::path::Path;

const LIVE_CORPUS: &str = "/Users/sac/ggen-marketplace/packs";

fn cli() -> Command {
    Command::cargo_bin("ggen").expect("ggen binary builds in this workspace")
}

/// Parse a real pack dir from the live corpus.
fn corpus_pack(name: &str) -> PackFile {
    let dir = Path::new(LIVE_CORPUS).join(name);
    assert!(
        dir.join("pack.toml").exists(),
        "corpus pack {} missing at {}",
        name,
        dir.display()
    );
    pack_file_from_dir(&dir).expect("live corpus pack must parse")
}

/// Write a minimal grounded pack.toml fixture and return its dir.
fn fixture_pack(root: &Path, id: &str, capabilities: Option<&str>, extra: Option<&str>) -> PathBuf {
    let dir = root.join(id);
    std::fs::create_dir_all(&dir).expect("fixture dir created");
    let mut toml = format!(
        "[pack]\nid = \"{}\"\nname = \"{}\"\npackages = []\n",
        id, id
    );
    if let Some(caps) = capabilities {
        toml.push_str(caps);
    }
    if let Some(x) = extra {
        toml.push_str(x);
    }
    std::fs::write(dir.join("pack.toml"), toml).expect("fixture pack.toml written");
    dir
}

fn fixture_caps(provides: &[&str], requires: &[&str]) -> String {
    let p: Vec<String> = provides.iter().map(|u| format!("\"{}\"", u)).collect();
    let r: Vec<String> = requires.iter().map(|u| format!("\"{}\"", u)).collect();
    format!(
        "\n[capabilities]\nprovides = [{}]\nrequires = [{}]\n",
        p.join(", "),
        r.join(", ")
    )
}

use std::path::PathBuf;

/// (1) Real dependency chain from the live corpus: mcpp-pack
/// requires urn:ggen:pack:clap-noun-verb-pack; clap-noun-verb-pack
/// requires praxis-core-pack and star-toml-pack. The kernel's topological
/// order must place every provider before its consumer (capability edges are
/// order-bearing). The CLI is exercised over the same chain for the
/// end-to-end success path.
#[test]
fn compose_real_dependency_chain_orders_provider_before_consumer() {
    let mcpp = corpus_pack("mcpp-pack");
    let cnv = corpus_pack("clap-noun-verb-pack");
    let praxis = corpus_pack("praxis-core-pack");
    let startoml = corpus_pack("star-toml-pack");

    let plan =
        compose(&[mcpp, cnv, praxis, startoml]).expect("grounded 4-pack corpus chain must compose");

    let pos = |id: &str| {
        plan.order
            .iter()
            .position(|p| p == id)
            .unwrap_or_else(|| panic!("pack {} missing from plan order {:?}", id, plan.order))
    };
    assert!(
        pos("clap-noun-verb-pack") < pos("mcpp-pack"),
        "capability provider clap-noun-verb-pack must precede consumer mcpp-pack; got {:?}",
        plan.order
    );
    assert!(
        pos("praxis-core-pack") < pos("clap-noun-verb-pack")
            && pos("star-toml-pack") < pos("clap-noun-verb-pack"),
        "praxis-core-pack and star-toml-pack must precede clap-noun-verb-pack; got {:?}",
        plan.order
    );

    // End-to-end through the real CLI over the live corpus: all four names
    // resolve, kernel admits, wire JSON carries the sorted order and the
    // self-satisfied audit.
    let out = cli()
        .args([
            "pack",
            "compose",
            "--packs",
            "mcpp-pack,clap-noun-verb-pack,praxis-core-pack,star-toml-pack",
        ])
        .env("GGEN_CAPABILITY_CORPUS_ROOTS", LIVE_CORPUS)
        .output()
        .expect("ggen pack compose runs");
    assert!(
        out.status.success(),
        "CLI compose of grounded corpus chain must exit 0; stderr: {}",
        String::from_utf8_lossy(&out.stderr)
    );
    let json: serde_json::Value = serde_json::from_slice(&out.stdout).expect("wire output is JSON");
    let ids = json["pack_ids"].as_array().expect("pack_ids array");
    assert_eq!(ids.len(), 4, "all 4 packs present in wire plan");
    // This chain is cross-satisfied (each pack's requirements are bound by a
    // *different* pack's provides), so the union self-satisfaction audit is
    // correctly empty — the wire field must still be present.
    let sat = json["self_satisfied"]
        .as_array()
        .expect("self_satisfied field present");
    assert!(
        sat.is_empty(),
        "cross-satisfied chain has no self-satisfied members; got {:?}",
        sat
    );
}

/// (2) A self-satisfied pack (provides its own URN, requires only its own
/// URN) composes alone with no refusal, and the union audit marks it
/// self-satisfied.
#[test]
fn self_satisfied_pack_composes_alone() {
    let tmp = tempfile::tempdir().expect("tempdir");
    fixture_pack(
        tmp.path(),
        "self-sat-pack",
        Some(&fixture_caps(
            &["urn:ggen:pack:self-sat-pack"],
            &["urn:ggen:pack:self-sat-pack"],
        )),
        None,
    );
    let pf = pack_file_from_dir(&tmp.path().join("self-sat-pack")).expect("fixture parses");

    let plan = compose(&[pf]).expect("self-satisfied pack composes without refusal");
    assert_eq!(plan.order, vec!["self-sat-pack".to_string()]);
    assert!(
        plan.self_satisfied.contains(&"self-sat-pack".to_string()),
        "union self-satisfaction audited; got {:?}",
        plan.self_satisfied
    );

    // End-to-end through the CLI against the temp corpus root.
    let out = cli()
        .args(["pack", "compose", "--packs", "self-sat-pack"])
        .env("GGEN_CAPABILITY_CORPUS_ROOTS", tmp.path())
        .output()
        .expect("ggen pack compose runs");
    assert!(
        out.status.success(),
        "self-satisfied pack composes via CLI; stderr: {}",
        String::from_utf8_lossy(&out.stderr)
    );
}

/// (3a) `DuplicateCapability` refusal: two differing-id packs claim the same
/// capability URN.
#[test]
fn refusal_duplicate_capability() {
    let tmp = tempfile::tempdir().expect("tempdir");
    fixture_pack(
        tmp.path(),
        "dup-provider-a",
        Some(&fixture_caps(&["urn:ggen:pack:dup-urn"], &[])),
        None,
    );
    fixture_pack(
        tmp.path(),
        "dup-provider-b",
        Some(&fixture_caps(&["urn:ggen:pack:dup-urn"], &[])),
        None,
    );
    let a = pack_file_from_dir(&tmp.path().join("dup-provider-a")).expect("parses");
    let b = pack_file_from_dir(&tmp.path().join("dup-provider-b")).expect("parses");

    let refusal = compose(&[a, b]).expect_err("duplicate capability must be refused");
    assert!(
        refusal.to_string().contains("duplicate capability")
            && refusal.to_string().contains("urn:ggen:pack:dup-urn"),
        "typed DuplicateCapability refusal; got: {}",
        refusal
    );
}

/// (3b) `UnboundRequirement` refusal: a required provider URN absent from the
/// composed set.
#[test]
fn refusal_unbound_requirement() {
    let tmp = tempfile::tempdir().expect("tempdir");
    fixture_pack(
        tmp.path(),
        "orphan-consumer",
        Some(&fixture_caps(
            &["urn:ggen:pack:orphan-consumer"],
            &["urn:ggen:pack:missing-provider-urn"],
        )),
        None,
    );
    let pf = pack_file_from_dir(&tmp.path().join("orphan-consumer")).expect("parses");

    let refusal = compose(&[pf]).expect_err("unbound requirement must be refused");
    assert!(
        refusal.to_string().contains("unbound requirement")
            && refusal
                .to_string()
                .contains("urn:ggen:pack:missing-provider-urn"),
        "typed UnboundRequirement refusal; got: {}",
        refusal
    );
}

/// (3c) `DuplicateArtifactPath` refusal: two packs write the same template
/// output path.
#[test]
fn refusal_duplicate_artifact_path() {
    let extra = "\n[[pack.templates]]\nname = \"out\"\npath = \"shared/generated/out.rs\"\ndescription = \"collides\"\n";
    let tmp = tempfile::tempdir().expect("tempdir");
    fixture_pack(
        tmp.path(),
        "writer-a",
        Some(&fixture_caps(&["urn:ggen:pack:writer-a"], &[])),
        Some(extra),
    );
    fixture_pack(
        tmp.path(),
        "writer-b",
        Some(&fixture_caps(&["urn:ggen:pack:writer-b"], &[])),
        Some(extra),
    );
    let a = pack_file_from_dir(&tmp.path().join("writer-a")).expect("parses");
    let b = pack_file_from_dir(&tmp.path().join("writer-b")).expect("parses");

    let refusal = compose(&[a, b]).expect_err("duplicate artifact path must be refused");
    assert!(
        refusal.to_string().contains("duplicate artifact path")
            && refusal.to_string().contains("shared/generated/out.rs"),
        "typed DuplicateArtifactPath refusal; got: {}",
        refusal
    );
}

/// (3d) `CyclicDependencies` refusal: inter-pack `dependencies` edges form a
/// cycle — composition has no install order.
#[test]
fn refusal_cyclic_dependencies() {
    let dep_on = |other: &str| {
        format!(
            "\n[[pack.dependencies]]\npack_id = \"{}\"\nversion = \"1.0.0\"\n",
            other
        )
    };
    let tmp = tempfile::tempdir().expect("tempdir");
    fixture_pack(
        tmp.path(),
        "dep-cycle-a",
        Some(&fixture_caps(&["urn:ggen:pack:dep-cycle-a"], &[])),
        Some(&dep_on("dep-cycle-b")),
    );
    fixture_pack(
        tmp.path(),
        "dep-cycle-b",
        Some(&fixture_caps(&["urn:ggen:pack:dep-cycle-b"], &[])),
        Some(&dep_on("dep-cycle-a")),
    );
    let a = pack_file_from_dir(&tmp.path().join("dep-cycle-a")).expect("parses");
    let b = pack_file_from_dir(&tmp.path().join("dep-cycle-b")).expect("parses");

    let refusal = compose(&[a, b]).expect_err("cyclic dependency edges must be refused");
    assert!(
        refusal.to_string().contains("cyclic"),
        "typed CyclicDependencies refusal; got: {}",
        refusal
    );
}

/// (4) Order determinism: the same input set composed 10 times yields a
/// byte-identical plan order every time.
#[test]
fn compose_order_is_deterministic_across_iterations() {
    let packs: Vec<PackFile> = [
        "mcpp-pack",
        "clap-noun-verb-pack",
        "praxis-core-pack",
        "star-toml-pack",
        "wasi-json-abi-pack",
    ]
    .iter()
    .map(|n| corpus_pack(n))
    .collect();

    let first = compose(&packs).expect("corpus set composes");
    for i in 0..10 {
        let again = compose(&packs).expect("corpus set composes");
        assert_eq!(
            first.order, again.order,
            "iteration {} diverged from the first plan order",
            i
        );
        assert_eq!(first.self_satisfied, again.self_satisfied);
    }
}

/// (5) Capability cycle: A requires B's URN, B requires A's URN, both
/// provide their own. Capability edges are order-only and dropped
/// cycle-deterministically — composition succeeds, never refuses.
#[test]
fn capability_cycle_composes_with_cycle_edges_dropped() {
    let tmp = tempfile::tempdir().expect("tempdir");
    fixture_pack(
        tmp.path(),
        "cap-cycle-a",
        Some(&fixture_caps(
            &["urn:ggen:pack:cap-cycle-a"],
            &["urn:ggen:pack:cap-cycle-b"],
        )),
        None,
    );
    fixture_pack(
        tmp.path(),
        "cap-cycle-b",
        Some(&fixture_caps(
            &["urn:ggen:pack:cap-cycle-b"],
            &["urn:ggen:pack:cap-cycle-a"],
        )),
        None,
    );
    let a = pack_file_from_dir(&tmp.path().join("cap-cycle-a")).expect("parses");
    let b = pack_file_from_dir(&tmp.path().join("cap-cycle-b")).expect("parses");

    let plan = compose(&[a.clone(), b.clone()])
        .expect("capability cycle must compose (order-only edges, deterministic drop)");
    let mut ids = plan.order.clone();
    ids.sort();
    assert_eq!(ids, vec!["cap-cycle-a", "cap-cycle-b"]);

    // End-to-end through the CLI over the temp corpus: same admission.
    let out = cli()
        .args(["pack", "compose", "--packs", "cap-cycle-a,cap-cycle-b"])
        .env("GGEN_CAPABILITY_CORPUS_ROOTS", tmp.path())
        .output()
        .expect("ggen pack compose runs");
    assert!(
        out.status.success(),
        "capability cycle composes via CLI; stderr: {}",
        String::from_utf8_lossy(&out.stderr)
    );
}
