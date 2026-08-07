//! End-to-end checkpoint for `packs/tai-enterprise-rebuild-pack`: a real
//! consumer project, a real `ggen sync run` subprocess (`ggen` binary via
//! `chicago_tdd_tools::cli_proof::CliHarness` -- no library calls, no
//! mocks), and assertions on real generated file content on disk.
//!
//! `tai-enterprise-rebuild-pack` ships only its `ontology.ttl` vocabulary
//! plus 18 `tai:RecentGgenCapability` individuals -- every other class
//! (`EnterpriseRebuildProgram`, `EnterprisePhase`, `BuildingBlockFamily`,
//! `CertificationHorizon`, `SimulationScenario`, `EvidenceObligation`,
//! `GallCheckpoint`, the CMD internal/external dimensions and
//! `CandidateOption`s) is pure vocabulary with zero shipped individuals.
//! The pack's own `gates/*.rq` (auto-discovered from its `gates/` directory
//! and evaluated unconditionally by `ggen sync`, see `sync.rs`'s
//! "Pack-shipped SPARQL gates" stage) enforce EXACT cardinalities against
//! the union graph: 9 families, 7 phases, 10 certifications, 7 scenarios,
//! 18 recent-capabilities, 6 evidence-obligations, 10 CMD checkpoints, 12
//! internal + 13 external CMD dimensions, 24 internal + 26 external
//! `CandidateOption`s (exactly 2 per dimension). A consumer of this pack
//! MUST supply a fully-conforming domain ontology or `ggen sync run`
//! refuses outright -- there is no partial/degraded generation path. This
//! file's `full_conforming_ontology()` is that real consumer ontology.
//!
//! Three real bugs were found and fixed while building this checkpoint
//! (all noted in the test bodies below at their point of relevance):
//!  1. `packs/tai-enterprise-rebuild-pack/pack.toml` declared an unknown
//!     `license` key, making the pack fail to resolve at all
//!     (`[FM-PACK-003]`) -- fixed by removing the key.
//!  2. `ggen-engine/src/pack.rs`'s `resolve_pack_dir` discovered pack
//!     templates with a flat, non-recursive `read_dir`, silently dropping
//!     every `*.tmpl` in a subdirectory (18 of this pack's 19 templates
//!     live under `templates/{generated,docs,scripts,src,tests}/`) while
//!     the project-side loader (`sync::collect_tmpl_paths`) is explicitly
//!     recursive -- fixed by making pack template discovery recursive too
//!     (`collect_pack_tmpl_paths`).
//!  3. `templates/generated/combinatorial-maximalism-program.json.tmpl`
//!     assumed the `GROUP_CONCAT`-derived `?requires`/`?incompatibleWith`
//!     SPARQL bindings are always present (`row["requires"] != ""`), but
//!     an `OPTIONAL` clause that never matches for a given option leaves
//!     the binding absent from that row entirely, not bound to `""` --
//!     `row["requires"]` (bracket indexing) then raises "Variable not
//!     found" instead of comparing against an empty string. Fixed with
//!     `| default(value="")`.
//!
//! A fourth, narrower fix improved diagnosability rather than correctness:
//! `sync.rs`'s `render_str` mapped Tera render failures to `[FM-TPL-017]`
//! using bare `{e}` Display, which for Tera is frequently just "Failed to
//! render '__tera_one_off'" with no real cause -- the actual reason (bug 3
//! above) was only reachable via `Error::source()` chaining, exactly what
//! `template::tera_error_full_chain` already exists for (and what
//! `generation_rules.rs`'s `[FM-GEN-008]` path already uses). `render_str`
//! now uses the same helper.

#![allow(clippy::unwrap_used, clippy::expect_used, clippy::panic)]

mod support;

use std::fmt::Write as _;
use std::path::{Path, PathBuf};

use chicago_tdd_tools::cli_proof::CliHarness;
use support::{read_json, scaffold_pack_with_ontology};
use tempfile::TempDir;

fn packs_dir() -> PathBuf {
    Path::new(env!("CARGO_MANIFEST_DIR")).join("../../packs")
}

/// Cardinalities exactly matching `gates/020_exact_cardinality.rq` and
/// `gates/080_cmd_checkpoint_contract.rq`. Changing any of these without
/// updating the corresponding gate expectation will make the golden-path
/// test's sync refuse -- that coupling is deliberate, it is what proves the
/// gates are actually being evaluated against this test's own facts.
const N_FAMILIES: u32 = 9;
const N_PHASES: u32 = 7;
const N_CERTS: u32 = 10;
const N_SCENARIOS: u32 = 7;
const N_OBLIGATIONS: u32 = 6;
const N_CHECKPOINTS: u32 = 10;
const N_INTERNAL_DIMS: u32 = 12;
const N_EXTERNAL_DIMS: u32 = 13;

/// Build a consumer `ontology.ttl` that satisfies every one of the pack's
/// nine `gates/*.rq` closure/cardinality checks. `family_count` is the only
/// deliberately-tunable knob (used by the negative test below to violate
/// `020_exact_cardinality.rq`'s `families` count without hand-editing raw
/// Turtle text, which risks producing a syntactically-broken document
/// rather than the intended semantic violation).
fn conforming_ontology(family_count: u32) -> String {
    let mut ttl = String::new();
    ttl.push_str(
        "@prefix tai: <https://chatmangpt.com/ontology/tai-rebuild#> .\n\
         @prefix skos: <http://www.w3.org/2004/02/skos/core#> .\n\
         @prefix dcterms: <http://purl.org/dc/terms/> .\n\n",
    );

    // Program (also the CMD dimension holder -- gates/080 and /090 both
    // query `tai:tai-rebuild-v26-7-31 tai:internalDimension/externalDimension`).
    ttl.push_str(
        "tai:tai-rebuild-v26-7-31 a tai:EnterpriseRebuildProgram ;\n  \
           tai:identifier \"tai-rebuild-v26-7-31\" ;\n  \
           tai:version \"26.7.31\" ;\n  \
           dcterms:title \"Technology Applications, Inc. Enterprise Rebuild\" ;\n  \
           tai:requiredBroker \"BRCE\" ;\n  \
           tai:directActuation false ;\n  \
           tai:llmCalls false .\n\n",
    );
    ttl.push_str(
        "tai:roadmap-generator a tai:RoadmapGenerator ;\n  \
           tai:requiredBroker \"BRCE\" ;\n  \
           tai:directActuation false ;\n  \
           tai:llmCalls false ;\n  \
           tai:outcome \"Projects the dependency-ordered rebuild roadmap from admitted facts only.\" .\n\n",
    );
    ttl.push_str("tai:cmd-program tai:requiredBroker \"BRCE\" ; tai:directActuation false .\n\n");

    // Families: order 1..=family_count, linear dependsOn chain.
    for i in 1..=family_count {
        write!(
            ttl,
            "tai:family-{i} a tai:BuildingBlockFamily ;\n  \
               tai:order {i} ;\n  \
               tai:identifier \"family-{i}\" ;\n  \
               skos:prefLabel \"Building Block Family {i}\" ;\n"
        )
        .unwrap();
        if i > 1 {
            write!(ttl, "  tai:dependsOn tai:family-{} ;\n", i - 1).unwrap();
        }
        write!(
            ttl,
            "  tai:outcome \"Family {i} outcome: a canonical building-block surface admitted to the graph.\" .\n\n"
        )
        .unwrap();
    }

    // Certifications: order 1..=N_CERTS.
    for i in 1..=N_CERTS {
        write!(
            ttl,
            "tai:cert-{i} a tai:CertificationHorizon ;\n  \
               tai:order {i} ;\n  \
               tai:identifier \"cert-{i}\" ;\n  \
               skos:prefLabel \"Certification Horizon {i}\" ;\n  \
               tai:knowledgeBody \"Body of knowledge {i}\" ;\n  \
               tai:automatableWork \"Automatable work surface {i}\" ;\n  \
               tai:humanBoundary \"Human judgment boundary {i}\" ;\n  \
               tai:failurePrevention \"Failure mode prevented by certification {i}\" .\n\n"
        )
        .unwrap();
    }

    // Phases: order 0..N_PHASES, dependsOn predecessor, one family + one
    // certification each (cycled through the conforming set so a reduced
    // `family_count` in the negative test still yields a valid target).
    for i in 0..N_PHASES {
        let fam = (i % family_count) + 1;
        let cert = (i % N_CERTS) + 1;
        write!(
            ttl,
            "tai:phase-{i} a tai:EnterprisePhase ;\n  \
               tai:order {i} ;\n  \
               tai:identifier \"phase-{i}\" ;\n  \
               skos:prefLabel \"Phase {i}\" ;\n  \
               tai:outcome \"Phase {i} outcome: dependency-ordered reconstruction milestone.\" ;\n  \
               tai:requiresFamily tai:family-{fam} ;\n  \
               tai:requiresCertification tai:cert-{cert} "
        )
        .unwrap();
        if i > 0 {
            write!(ttl, ";\n  tai:dependsOn tai:phase-{} .\n\n", i - 1).unwrap();
        } else {
            ttl.push_str(".\n\n");
        }
    }

    // Scenarios: order 1..=N_SCENARIOS.
    for i in 1..=N_SCENARIOS {
        write!(
            ttl,
            "tai:scenario-{i} a tai:SimulationScenario ;\n  \
               tai:order {i} ;\n  \
               tai:identifier \"scenario-{i}\" ;\n  \
               skos:prefLabel \"Scenario {i}\" ;\n  \
               tai:trigger \"Trigger condition {i}\" ;\n  \
               tai:mutation \"Mutation applied in scenario {i}\" ;\n  \
               tai:expectedStanding \"UNKNOWN\" .\n\n"
        )
        .unwrap();
    }

    // Evidence obligations: order 1..=N_OBLIGATIONS.
    for i in 1..=N_OBLIGATIONS {
        write!(
            ttl,
            "tai:obligation-{i} a tai:EvidenceObligation ;\n  \
               tai:order {i} ;\n  \
               tai:identifier \"obligation-{i}\" ;\n  \
               tai:evidenceKind \"Evidence kind {i}\" ;\n  \
               tai:falsifier \"Falsifier condition {i}\" .\n\n"
        )
        .unwrap();
    }

    // CMD checkpoints G0..G9: order 0..N_CHECKPOINTS, dependsOn predecessor.
    for i in 0..N_CHECKPOINTS {
        write!(
            ttl,
            "tai:checkpoint-g{i} a tai:GallCheckpoint ;\n  \
               tai:order {i} ;\n  \
               tai:identifier \"g{i}\" ;\n  \
               skos:prefLabel \"CMD Checkpoint G{i}\" ;\n  \
               tai:maximumStanding \"PARTIAL_ALIVE\" ;\n  \
               tai:requiredArtifact \"artifacts/g{i}.json\" ;\n  \
               tai:refusalCode \"TAI-CMD-G{i}-REFUSED\" "
        )
        .unwrap();
        if i > 0 {
            write!(ttl, ";\n  tai:dependsOn tai:checkpoint-g{} .\n\n", i - 1).unwrap();
        } else {
            ttl.push_str(".\n\n");
        }
    }

    // Internal + external CMD dimensions, attached to the program.
    for i in 1..=N_INTERNAL_DIMS {
        write!(
            ttl,
            "tai:int-dim-{i} a tai:CandidateDimension ;\n  \
               tai:order {i} ;\n  \
               tai:identifier \"int-dim-{i}\" ;\n  \
               tai:selectionCardinality \"exactly-one\" ;\n  \
               tai:coverageMode \"internal\" .\n\
             tai:tai-rebuild-v26-7-31 tai:internalDimension tai:int-dim-{i} .\n\n"
        )
        .unwrap();
    }
    for i in 1..=N_EXTERNAL_DIMS {
        write!(
            ttl,
            "tai:ext-dim-{i} a tai:CandidateDimension ;\n  \
               tai:order {i} ;\n  \
               tai:identifier \"ext-dim-{i}\" ;\n  \
               tai:authorityCeiling \"PLAN_ONLY\" .\n\
             tai:tai-rebuild-v26-7-31 tai:externalDimension tai:ext-dim-{i} .\n\n"
        )
        .unwrap();
    }

    // Exactly 2 CandidateOptions per dimension (24 internal + 26 external
    // -- `gates/090_cmd_dependency_closure.rq`'s "dimension-option-cardinality"
    // check refuses any dimension with a count other than 2).
    let mut order = 1u32;
    for i in 1..=N_INTERNAL_DIMS {
        for k in 1..=2u32 {
            write!(
                ttl,
                "tai:int-opt-{i}-{k} a tai:CandidateOption ;\n  \
                   tai:order {order} ;\n  \
                   tai:identifier \"int-opt-{i}-{k}\" ;\n  \
                   tai:scope \"internal\" ;\n  \
                   tai:dimensionIdentifier \"int-dim-{i}\" ;\n  \
                   skos:prefLabel \"Internal option {i}.{k}\" ;\n  \
                   tai:authorityCeiling \"PLAN_ONLY\" ;\n  \
                   tai:reversibility \"reversible\" .\n\n"
            )
            .unwrap();
            order += 1;
        }
    }
    let mut order = 1u32;
    for i in 1..=N_EXTERNAL_DIMS {
        for k in 1..=2u32 {
            write!(
                ttl,
                "tai:ext-opt-{i}-{k} a tai:CandidateOption ;\n  \
                   tai:order {order} ;\n  \
                   tai:identifier \"ext-opt-{i}-{k}\" ;\n  \
                   tai:scope \"external\" ;\n  \
                   tai:dimensionIdentifier \"ext-dim-{i}\" ;\n  \
                   skos:prefLabel \"External option {i}.{k}\" ;\n  \
                   tai:authorityCeiling \"PLAN_ONLY\" ;\n  \
                   tai:reversibility \"reversible\" .\n\n"
            )
            .unwrap();
            order += 1;
        }
    }

    ttl
}

fn full_conforming_ontology() -> String {
    conforming_ontology(N_FAMILIES)
}

fn scaffold(ontology: &str) -> (TempDir, PathBuf) {
    scaffold_pack_with_ontology(&packs_dir().join("tai-enterprise-rebuild-pack"), ontology)
}

fn run_sync(root: &Path) -> chicago_tdd_tools::cli_proof::CliOutput {
    CliHarness::cargo_bin("ggen")
        .args(["sync", "run"])
        .current_dir(root)
        .run()
        .expect("spawn ggen sync run")
}

/// Golden-path composite: a fully-conforming consumer ontology, one real
/// `ggen sync run` subprocess, real generated-file assertions across every
/// one of the pack's 8 `generated/*.json` + roadmap-doc templates, then a
/// second sync proving byte-identical (empty `written`) reproduction.
#[allow(clippy::too_many_lines)]
#[test]
fn full_pack_generates_all_catalogs_and_is_idempotent() {
    let (_dir, project) = scaffold(&full_conforming_ontology());

    let first = run_sync(&project);
    first.assert_success();

    let base = "consumer/tai-enterprise-rebuild";
    for relative in [
        format!("{base}/Cargo.toml"),
        format!("{base}/RELEASE_STANDING.json"),
        format!("{base}/docs/TAI_REBUILD_ROADMAP.md"),
        format!("{base}/generated/certifications.json"),
        format!("{base}/generated/combinatorial-maximalism-program.json"),
        format!("{base}/generated/enterprise.json"),
        format!("{base}/generated/gall-program.json"),
        format!("{base}/generated/recent-capabilities.json"),
        format!("{base}/generated/roadmap.json"),
        format!("{base}/generated/scenarios.json"),
        format!("{base}/scripts/verify.sh"),
        format!("{base}/scripts/verify_cmd.py"),
        format!("{base}/src/bin/tai_rebuild.rs"),
        format!("{base}/src/blocks.rs"),
        format!("{base}/src/lib.rs"),
        format!("{base}/src/model.rs"),
        format!("{base}/src/receipt.rs"),
        format!("{base}/src/simulation.rs"),
        format!("{base}/tests/process_receipt.rs"),
    ] {
        assert!(project.join(&relative).is_file(), "missing {relative}");
    }

    // recent-capabilities.json: the pack's own 18 shipped individuals,
    // untouched by the consumer ontology -- proves the pack's SPARQL
    // query + Tera loop wiring works independent of consumer data.
    let recent = read_json(
        &project,
        &format!("{base}/generated/recent-capabilities.json"),
    );
    assert_eq!(recent["requiredCount"], 18);
    let capabilities = recent["capabilities"].as_array().expect("array");
    assert_eq!(
        capabilities.len(),
        18,
        "all 18 admitted recent capabilities"
    );
    assert_eq!(capabilities[0]["order"], 1);
    assert_eq!(
        capabilities[0]["label"],
        "Level-Five book self-host and 360-degree validation"
    );
    assert_eq!(capabilities[0]["sourcePr"], 493);
    assert_eq!(capabilities[17]["order"], 18);
    assert_eq!(
        capabilities[17]["label"],
        "Fortune-5 global cloud deployment Building Blocks"
    );

    // enterprise.json: consumer-supplied program + family catalog + closure.
    let enterprise = read_json(&project, &format!("{base}/generated/enterprise.json"));
    assert_eq!(enterprise["identifier"], "tai-rebuild-v26-7-31");
    assert_eq!(enterprise["version"], "26.7.31");
    assert_eq!(enterprise["requiredBroker"], "BRCE");
    assert_eq!(enterprise["directActuation"], false);
    assert_eq!(enterprise["llmCalls"], false);
    assert_eq!(
        enterprise["families"].as_array().expect("array").len(),
        N_FAMILIES as usize
    );

    // roadmap.json: 7 phases, dependency-chained, family/cert bindings present.
    let roadmap = read_json(&project, &format!("{base}/generated/roadmap.json"));
    assert_eq!(roadmap["standing"], "UNKNOWN");
    assert_eq!(
        roadmap["phases"].as_array().expect("array").len(),
        N_PHASES as usize
    );
    assert_eq!(
        roadmap["dependencies"].as_array().expect("array").len(),
        (N_PHASES - 1) as usize,
        "phase-0 has no predecessor; the other 6 phases each depend on exactly one"
    );
    assert_eq!(
        roadmap["familyBindings"].as_array().expect("array").len(),
        N_PHASES as usize
    );

    // combinatorial-maximalism-program.json: CMD G0-G9 + dimension/option
    // closure, and the `requires`/`incompatibleWith` bug-3 fix (empty
    // arrays render as `[]`, not a render failure).
    let cmd = read_json(
        &project,
        &format!("{base}/generated/combinatorial-maximalism-program.json"),
    );
    assert_eq!(
        cmd["checkpoints"].as_array().expect("array").len(),
        N_CHECKPOINTS as usize
    );
    assert_eq!(
        cmd["internalDimensions"].as_array().expect("array").len(),
        N_INTERNAL_DIMS as usize
    );
    assert_eq!(
        cmd["externalDimensions"].as_array().expect("array").len(),
        N_EXTERNAL_DIMS as usize
    );
    let options = cmd["options"].as_array().expect("array");
    assert_eq!(
        options.len(),
        (N_INTERNAL_DIMS * 2 + N_EXTERNAL_DIMS * 2) as usize,
        "exactly 2 CandidateOptions per dimension, 12 internal + 13 external dims"
    );
    for option in options {
        assert_eq!(
            option["requires"],
            serde_json::json!([]),
            "no CandidateOption in this ontology declares tai:requiresOption; \
             the GROUP_CONCAT-derived binding must render as an empty array, \
             not a Tera render failure (the bug-3 regression this asserts against)"
        );
        assert_eq!(option["incompatibleWith"], serde_json::json!([]));
    }

    // certifications.json / scenarios.json / gall-program.json: exact
    // cardinalities the pack's own gates enforce.
    let certs = read_json(&project, &format!("{base}/generated/certifications.json"));
    assert_eq!(
        certs["certifications"].as_array().expect("array").len(),
        N_CERTS as usize
    );
    let scenarios = read_json(&project, &format!("{base}/generated/scenarios.json"));
    assert_eq!(
        scenarios["scenarios"].as_array().expect("array").len(),
        N_SCENARIOS as usize
    );
    let gall = read_json(&project, &format!("{base}/generated/gall-program.json"));
    assert_eq!(
        gall["checkpoints"].as_array().expect("array").len(),
        N_PHASES as usize,
        "gall-program.json's checkpoints come from EnterprisePhase, not GallCheckpoint"
    );
    assert_eq!(
        gall["obligations"].as_array().expect("array").len(),
        N_OBLIGATIONS as usize
    );

    // RELEASE_STANDING.json: never self-promotes standing.
    let standing = read_json(&project, &format!("{base}/RELEASE_STANDING.json"));
    assert_eq!(standing["standing"], "UNKNOWN");
    assert_eq!(standing["externalEnterpriseStanding"], "UNKNOWN");
    assert_eq!(standing["declaredCrown"]["families"], N_FAMILIES);
    assert_eq!(standing["declaredCrown"]["recentCapabilities"], 18);

    // Cargo.toml: real content, not just presence.
    let cargo_toml = std::fs::read_to_string(project.join(format!("{base}/Cargo.toml")))
        .expect("read Cargo.toml");
    assert!(cargo_toml.contains("name = \"tai-enterprise-rebuild\""));
    assert!(cargo_toml.contains("ggen-architecture"));

    // Second sync over the identical facts must write nothing.
    let second = run_sync(&project);
    second.assert_success();
    assert_eq!(
        second.stdout.contains("\"written\": []") || second.stdout.contains("\"written\":[]"),
        true,
        "second sync must be byte-identical (empty written list): {}",
        second.stdout
    );
}

/// Negative control proving `gates/020_exact_cardinality.rq` is actually
/// evaluated (fail-closed), not merely present on disk: 8 families instead
/// of the required 9 must refuse the sync by name, before any file is
/// written.
#[test]
fn exact_cardinality_gate_refuses_wrong_family_count() {
    let (_dir, project) = scaffold(&conforming_ontology(N_FAMILIES - 1));

    let result = run_sync(&project);
    result.assert_failure();
    assert!(
        result.stderr.contains("020_exact_cardinality.rq"),
        "refusal must name the offending gate file: {}",
        result.stderr
    );
    assert!(
        result.stderr.contains("families"),
        "refusal must identify which cardinality kind failed: {}",
        result.stderr
    );
    assert!(
        result.stderr.contains(&format!("{}", N_FAMILIES - 1))
            && result.stderr.contains(&format!("{N_FAMILIES}")),
        "refusal must show actual vs. expected counts: {}",
        result.stderr
    );
    assert!(
        !project
            .join("consumer/tai-enterprise-rebuild/generated/enterprise.json")
            .exists(),
        "a gate refusal must happen before any file is written"
    );
}
