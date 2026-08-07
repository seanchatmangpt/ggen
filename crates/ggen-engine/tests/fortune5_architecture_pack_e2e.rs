//! Real end-to-end checkpoint for `packs/fortune5-architecture-pack` — the
//! Fortune 5 enterprise-architecture law pack. Uses the real `ggen` binary
//! subprocess (`chicago_tdd_tools::cli_proof::CliHarness`, no mocks), a real
//! temp consumer project wired to the pack by relative path exactly the way
//! `examples/fortune5-architecture` (the pack's own reference fixture) does,
//! and asserts on real generated file content on disk.
//!
//! Before this test existed, the pack had zero e2e coverage anywhere in the
//! workspace, and its `gates/040_reliability_and_promotion_contract.rq` gate
//! could not even be parsed: the replication-quorum sub-`SELECT` shared its
//! enclosing `{ }` with two sibling `BIND`s instead of being wrapped in its
//! own braces, which is invalid SPARQL 1.1 grammar (a `GroupGraphPattern`'s
//! `{ }` must hold either a pure `SubSelect` or a `GroupGraphPatternSub`, not
//! a mix) — `cargo run --bin ggen -- sync run` against the pack's own
//! reference fixture failed every single time with `[FM-GRAPH-003] SPARQL
//! parse failed: error at 63:21: expected ENCODE_FOR_URI`, confirmed by
//! bisecting each of the pack's 7 gate files individually before the fix and
//! re-running the identical fixture after it. Fixed in the gate file itself
//! (wrapped the sub-`SELECT` in its own `{ }`), not in `ggen-engine`: the
//! standard SPARQL parser (`spargebra`, `[FM-GRAPH-003]`'s source per
//! `crates/ggen-engine/src/graph.rs`) is correctly rejecting genuinely
//! invalid syntax, and the same nested-subquery shape parses and evaluates
//! correctly elsewhere in this same file's sibling gates once braced
//! properly — so this is a pack-authoring bug, not an engine gap.

#![allow(clippy::unwrap_used, clippy::expect_used, clippy::panic)]

mod support;

use std::path::{Path, PathBuf};

use chicago_tdd_tools::cli_proof::CliHarness;
use support::{copy_tree, read};
use tempfile::TempDir;

fn pack_dir() -> PathBuf {
    Path::new(env!("CARGO_MANIFEST_DIR")).join("../../packs/fortune5-architecture-pack")
}

fn reference_ontology() -> PathBuf {
    Path::new(env!("CARGO_MANIFEST_DIR")).join("../../examples/fortune5-architecture/ontology.ttl")
}

/// Scaffolds `<root>/fortune5-architecture-pack` (a real copy of the pack)
/// and `<root>/consumer` (a real consumer project referencing it by relative
/// path), matching `examples/fortune5-architecture`'s own `ggen.toml` shape
/// exactly. `ontology` becomes the consumer's `ontology.ttl`.
fn scaffold(root: &Path, ontology: &str) -> PathBuf {
    copy_tree(&pack_dir(), &root.join("fortune5-architecture-pack"));

    let project = root.join("consumer");
    std::fs::create_dir_all(project.join("templates")).expect("mkdir templates");
    std::fs::write(project.join("ontology.ttl"), ontology).expect("write ontology");
    std::fs::write(
        project.join("ggen.toml"),
        "[project]\nname = \"fortune5-architecture-consumer\"\n\n\
         [ontology]\nsource = \"ontology.ttl\"\n\n\
         [packs]\nfortune5-architecture-pack = { path = \"../fortune5-architecture-pack\" }\n\n\
         [templates]\ndir = \"templates\"\n\n\
         [law]\nreflexive = true\n",
    )
    .expect("write ggen.toml");
    project
}

fn run_sync(root: &Path) -> chicago_tdd_tools::cli_proof::CliOutput {
    CliHarness::cargo_bin("ggen")
        .args(["sync", "run"])
        .current_dir(root)
        .run()
        .expect("spawn ggen sync run")
}

/// The realistic scenario a consumer of this pack relies on: a complete
/// Tier 0 enterprise-architecture graph (the pack's own reference fixture,
/// `examples/fortune5-architecture/ontology.ttl`) syncs cleanly, manufactures
/// all four documented artifacts with real graph-derived content (not empty
/// scaffolds), and a second sync is byte-identical (idempotent).
#[test]
fn tier0_reference_fixture_generates_all_four_artifacts_and_is_idempotent() {
    let dir = TempDir::new().expect("tempdir");
    let ontology = std::fs::read_to_string(reference_ontology()).expect("read reference fixture");
    let project = scaffold(dir.path(), &ontology);

    run_sync(&project).assert_success();

    for relative in [
        "docs/FORTUNE5_ARCHITECTURE_CATALOG.md",
        "docs/FORTUNE5_ARCHITECTURE_DAG.dot",
        "docs/FORTUNE5_CONTROL_MATRIX.md",
        "docs/FORTUNE5_WORKFLOW_PATTERN_COVERAGE.md",
    ] {
        assert!(project.join(relative).is_file(), "missing {relative}");
    }

    let catalog = read(&project, "docs/FORTUNE5_ARCHITECTURE_CATALOG.md");
    for required in [
        "**Program:** `FORTUNE5-E2E`",
        "**Release:** `v26.7.30`",
        "`orders` — Order execution service | commerce | Tier0 | Admitted",
        "`execute-orders` — Execute orders | commerce | Active | orders",
    ] {
        assert!(
            catalog.contains(required),
            "catalog missing {required}: {catalog}"
        );
    }

    let dag = read(&project, "docs/FORTUNE5_ARCHITECTURE_DAG.dot");
    assert!(dag.contains("digraph Fortune5Architecture"), "{dag}");
    assert!(
        dag.contains(r#""orders" [label="orders\nOrder execution service\nTier0"];"#),
        "dag missing orders node: {dag}"
    );

    let matrix = read(&project, "docs/FORTUNE5_CONTROL_MATRIX.md");
    assert!(
        matrix.contains("| `orders` | Tier0 | YES | YES | YES | YES | YES | YES | YES |"),
        "control matrix missing all-controls-present row for `orders`: {matrix}"
    );
    assert!(
        matrix.contains("| `fortune5-autonomics` | BRCE | false | 16 | blake3 |"),
        "control matrix missing autonomic authority row: {matrix}"
    );

    let coverage = read(&project, "docs/FORTUNE5_WORKFLOW_PATTERN_COVERAGE.md");
    for pattern in 1..=43 {
        let row = format!("`wasm4pm:wcp{pattern:02}`");
        assert!(
            coverage.contains(&row),
            "workflow pattern coverage missing WCP{pattern:02} evidence row: {coverage}"
        );
    }
    assert!(
        coverage.contains("| 1 | Sequence |"),
        "coverage missing WCP01 pattern name row: {coverage}"
    );
    assert!(
        coverage.contains("| 43 | Explicit Termination |"),
        "coverage missing WCP43 pattern name row: {coverage}"
    );

    let second = run_sync(&project);
    second.assert_success();
    assert!(
        second.stdout.contains(r#""written": []"#),
        "second sync over unchanged facts must be byte-identical (no new writes): {}",
        second.stdout
    );
}

/// Fortune 5 reliability law requires multi-region replication (gate
/// `040_reliability_and_promotion_contract.rq`'s `f5:replicatesTo` clause):
/// an asset's `ReplicationPolicy` naming only a single region must refuse
/// the sync, not silently admit a single point of failure. This is the exact
/// gate clause that was unparseable before this test's fix -- see the module
/// doc comment.
#[test]
fn single_region_replication_policy_is_refused() {
    let dir = TempDir::new().expect("tempdir");
    let ontology = std::fs::read_to_string(reference_ontology()).expect("read reference fixture");
    let sabotaged = ontology.replace(
        "f5:replicatesTo ex:us-west, ex:us-east, ex:us-central ;",
        "f5:replicatesTo ex:us-west ;",
    );
    assert_ne!(
        sabotaged, ontology,
        "negative control must actually narrow replication to one region"
    );
    let project = scaffold(dir.path(), &sabotaged);

    let output = run_sync(&project);
    output.assert_failure();
    output.assert_stderr_contains("040_reliability_and_promotion_contract.rq");
    output.assert_stderr_contains("f5:replicatesTo");
    assert!(
        !project
            .join("docs/FORTUNE5_ARCHITECTURE_CATALOG.md")
            .exists(),
        "a refused sync must not write any generated artifact"
    );
}

/// Broker-only autonomics law (gate `050_broker_only_autonomics.rq`):
/// `f5:directActuation` must be `false` -- architecture autonomics may only
/// manufacture bounded intents through BRCE, never actuate directly. Proves
/// the pack's central architectural boundary (see `README.md`'s "Architecture
/// boundary" section) is a real, enforced refusal and not just documentation.
#[test]
fn direct_actuation_autonomic_policy_is_refused() {
    let dir = TempDir::new().expect("tempdir");
    let ontology = std::fs::read_to_string(reference_ontology()).expect("read reference fixture");
    let sabotaged = ontology.replace("f5:directActuation false ;", "f5:directActuation true ;");
    assert_ne!(
        sabotaged, ontology,
        "negative control must actually flip directActuation to true"
    );
    let project = scaffold(dir.path(), &sabotaged);

    let output = run_sync(&project);
    output.assert_failure();
    output.assert_stderr_contains("050_broker_only_autonomics.rq");
    output.assert_stderr_contains("f5:directActuation");
}

/// Workflow-pattern evidence law (gate `060_workflow_pattern_evidence.rq`):
/// every one of the revised control-flow catalogue's WCP01-WCP43 patterns
/// requires a complete four-dimension evidence contract (implementation,
/// positive witness, negative falsifier, receipt verifier) on the program.
/// Deleting one pattern's entire evidence individual must refuse the sync.
#[test]
fn missing_workflow_pattern_evidence_is_refused() {
    let dir = TempDir::new().expect("tempdir");
    let ontology = std::fs::read_to_string(reference_ontology()).expect("read reference fixture");
    // Remove WCP43's evidence individual entirely (its triple block plus its
    // membership in the program's f5:hasWorkflowPatternEvidence list).
    let sabotaged = ontology
        .replace(", ex:wcp43-evidence", "")
        .replace(
            "ex:wcp43-evidence a f5:WorkflowPatternEvidence ; f5:workflowPattern f5:WCP43 ; f5:implementationEvidence \"wasm4pm:wcp43\" ; f5:positiveWitness \"fixtures/wcp43/positive\" ; f5:negativeFalsifier \"fixtures/wcp43/negative\" ; f5:receiptVerifier \"receipts/wcp43.json\" .\n",
            "",
        );
    assert_ne!(
        sabotaged, ontology,
        "negative control must actually remove WCP43's evidence"
    );
    let project = scaffold(dir.path(), &sabotaged);

    let output = run_sync(&project);
    output.assert_failure();
    output.assert_stderr_contains("060_workflow_pattern_evidence.rq");
}
