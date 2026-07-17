//! Joseph as chief administrator of Egypt: many admitted PDDL Turtle
//! planning artifacts become one POWL v2 Turtle workflow artifact.
//!
//! Chatman Equation framing: `A = μ(O*)` where `O*` is Joseph's national
//! famine-management plan admitted as many per-phase PDDL Turtle artifacts
//! (`*.domain.ttl` / `*.problem.ttl`), `μ` is the lawful projection from
//! admitted planning artifacts into a workflow artifact, and `A` is the
//! exported `joseph-egypt-famine-management.powl.ttl`:
//! `joseph-egypt-famine-management.powl.ttl = μ({*.domain.ttl, *.problem.ttl}*)`.
//! Joseph's plan is the modeled source; any CLI in front of this pipeline is
//! only the human artifact handle. Formal planning-to-workflow demonstration
//! domain — argues no doctrine, claims no historicity.
//!
//! Artifact boundary: Turtle/PDDL is imported from committed `.ttl` files
//! (`tests/pddl_to_powl/joseph/`) and exported to one generated `.ttl` file
//! (`target/chatman/powl/joseph-egypt-famine-management.powl.ttl`); no
//! Turtle or PDDL is embedded in Rust strings; the POWL output is generated
//! by the pipeline, never hand-authored.
//!
//! Many-to-one composition: each phase artifact is loaded as its own
//! snapshot; `ChatmanEngine::plan_tape_for_snapshots` merges the parsed
//! domain/problem fragments structurally (shared domain name, deduplicated
//! unions, conjunctive goal across every problem fragment) and plans ONCE —
//! one combined national plan, not one plan per phase. Institutional
//! authority is preserved as activity labels (appoint-joseph,
//! activate-rationing-policy, …), never as invisible automation. The current
//! projection supports linear plans only, so the fourteen-year arc is
//! expressed as the base-case linear program path from forecast to
//! stabilization; loops (yearly cycles), choices (payment modes, buyer
//! paths), and cross-city partial orders remain unimplemented.

use chicago_tdd_tools::prelude::*;

use std::fs;
use std::path::PathBuf;

use oxigraph::io::{RdfFormat, RdfParser};
use oxigraph::store::Store;
use powl2_decompose::Powl;

use bcinr_pddl::Pddl8Tape;
use praxis_graphlaw::chatman::abi::{GraphSnapshotId, ProfileId, Refusal};
use praxis_graphlaw::chatman::engine::{AdmissionSpec, ChatmanEngine, EngineProfile};
use praxis_graphlaw::chatman::powl_projection::{powl_to_turtle, project_pddl_tape_to_powl};
use praxis_graphlaw::chatman::router::ProfileGates;
use praxis_graphlaw::chatman::triple8::ProfileSymbolTable;

const PROFILE_IRI: &str = "profile:joseph-famine-projection-test";
/// Provenance target for `powl2:derivedFrom`: the combined national plan.
const PLAN_SOURCE_IRI: &str = "urn:chatman:plan:joseph-famine";
/// Base IRI under which the serializer mints deterministic structural IRIs.
const BASE_IRI: &str = "urn:chatman:powl:joseph-egypt-famine-management";
const EXPORT_FILE: &str = "joseph-egypt-famine-management.powl.ttl";

/// The thirteen phase artifact stems, in program order; each contributes a
/// `<stem>.domain.ttl` and `<stem>.problem.ttl` planning fragment.
const PHASES: [&str; 13] = [
    "forecast",
    "appointment",
    "abundance-collection",
    "storage-logistics",
    "inventory-control",
    "famine-detection",
    "rationing-policy",
    "grain-distribution",
    "payment-treasury",
    "foreign-buyers",
    "family-arrival",
    "land-livestock-policy",
    "closure-stabilization",
];
/// Source-sensitivity variant of the grain-distribution domain fragment
/// (allocate-grain renamed to ration-grain).
const GRAIN_DISTRIBUTION_VARIANT: &str = "grain-distribution.domain.variant.ttl";
/// Refusal fixture declaring a different domain name.
const MISMATCHED_DOMAIN: &str = "mismatched.domain.ttl";

/// The 20 expected national program steps, in plan order.
const EXPECTED_STEPS: [&str; 20] = [
    "receive-forecast",
    "interpret-forecast",
    "appoint-joseph",
    "activate-collection-policy",
    "collect-fifth-of-produce",
    "store-grain-in-cities",
    "record-inventory",
    "detect-famine",
    "activate-rationing-policy",
    "open-storehouses",
    "allocate-grain",
    "receive-payment",
    "update-treasury",
    "register-foreign-buyers",
    "supply-foreign-buyers",
    "receive-family",
    "provision-family",
    "transfer-land-livestock",
    "stabilize-famine-response",
    "close-program",
];
/// C(20,2): closed order-pair count for the 20-step linear chain.
const EXPECTED_ORDER_PAIRS: usize = 190;

fn joseph_fixture_dir() -> PathBuf {
    PathBuf::from(env!("CARGO_MANIFEST_DIR"))
        .join("tests")
        .join("pddl_to_powl")
        .join("joseph")
}

/// Repo-local generated-artifact directory (`<repo>/target/chatman/powl`).
fn powl_output_dir() -> PathBuf {
    PathBuf::from(env!("CARGO_MANIFEST_DIR"))
        .join("..")
        .join("..")
        .join("target")
        .join("chatman")
        .join("powl")
}

fn build_profile() -> Result<EngineProfile, Refusal> {
    let profile_id = ProfileId::new(PROFILE_IRI);
    let gates = ProfileGates::new(profile_id.clone(), ProfileGates::DEFAULT_ENABLED_MASK, 0, 8)?;
    let symbol_table = ProfileSymbolTable::build(
        profile_id,
        vec![
            "<urn:chatman:t0>".to_string(),
            "<urn:chatman:t1>".to_string(),
        ],
    )?;
    Ok(EngineProfile {
        gates,
        symbol_table,
        admission: AdmissionSpec {
            constraint_names: vec!["c0".to_string()],
            required_mask: 0,
            forbidden_mask: 0,
            set_on_admit: 0,
            clear_on_admit: 0,
        },
        breed_permits: Vec::new(),
    })
}

/// Imports the given fixture artifacts from disk (the only route Turtle
/// enters this test), loads each as its own snapshot in one engine, prints
/// the `IMPORTED_PDDL_TTL_PATHS=` marker, and returns the engine plus the
/// ordered snapshot ids.
fn import_artifacts(
    file_names: &[String],
) -> Result<(ChatmanEngine, Vec<GraphSnapshotId>), Refusal> {
    let mut engine = ChatmanEngine::in_memory(build_profile()?)?;
    let mut snapshot_ids = Vec::with_capacity(file_names.len());
    let mut paths = Vec::with_capacity(file_names.len());
    for file_name in file_names {
        let path = joseph_fixture_dir().join(file_name);
        let path = path
            .canonicalize()
            .unwrap_or_else(|e| panic!("artifact {file_name} must canonicalize: {e}"));
        let turtle = fs::read_to_string(&path)
            .unwrap_or_else(|e| panic!("artifact {file_name} must be readable: {e}"));
        let snapshot_id = GraphSnapshotId::new(format!("urn:chatman:snapshot:joseph:{file_name}"));
        engine.load_snapshot(&snapshot_id, &turtle)?;
        snapshot_ids.push(snapshot_id);
        paths.push(path.display().to_string());
    }
    println!("IMPORTED_PDDL_TTL_PATHS={}", paths.join(","));
    Ok((engine, snapshot_ids))
}

/// The full canonical artifact set: every phase's domain and problem
/// fragment, in program order, with an optional substitution.
fn canonical_artifact_set(substitute: Option<(&str, &str)>) -> Vec<String> {
    let mut files = Vec::with_capacity(PHASES.len() * 2);
    for phase in PHASES {
        for suffix in ["domain", "problem"] {
            let default_name = format!("{phase}.{suffix}.ttl");
            files.push(match substitute {
                Some((from, to)) if from == default_name => to.to_string(),
                _ => default_name,
            });
        }
    }
    files
}

/// Imports the artifact set and generates the ONE combined national plan via
/// the engine's many-to-one side-door, printing `GENERATED_PLAN_ID=` (BLAKE3
/// over the ordered plan-step labels).
fn generate_combined_plan(substitute: Option<(&str, &str)>) -> Result<Pddl8Tape, Refusal> {
    let (engine, snapshot_ids) = import_artifacts(&canonical_artifact_set(substitute))?;
    let tape = engine.plan_tape_for_snapshots(&snapshot_ids)?;
    let labels: Vec<&str> = tape.ops.iter().map(|op| op.label.as_str()).collect();
    let plan_id = blake3::hash(labels.join("\n").as_bytes()).to_hex();
    println!("GENERATED_PLAN_ID=blake3:{plan_id}");
    Ok(tape)
}

/// Exports the generated POWL Turtle artifact and prints its canonical
/// absolute path with the `GENERATED_POWL_TTL_PATH=` marker.
fn export_powl(file_name: &str, turtle: &str) -> PathBuf {
    let dir = powl_output_dir();
    fs::create_dir_all(&dir).expect("POWL output directory must be creatable");
    let file_path = dir.join(file_name);
    fs::write(&file_path, turtle).expect("exported POWL Turtle must be writable");
    let path = file_path
        .canonicalize()
        .expect("exported POWL Turtle path must canonicalize");
    println!("GENERATED_POWL_TTL_PATH={}", path.display());
    path
}

test!(
    joseph_artifact_set_generates_one_plan_and_one_partial_order,
    {
        // Arrange + Act: import all 26 phase fragments, generate ONE combined
        // plan, and project it to POWL.
        let tape = generate_combined_plan(None)?;
        assert_eq!(
            tape.ops.len(),
            EXPECTED_STEPS.len(),
            "the thirteen phase fragments must merge into one 20-step national plan"
        );
        let model = project_pddl_tape_to_powl(&tape)?;

        // Assert: one PartialOrder, 20 leaves, C(20,2)=190 closed order pairs,
        // and leaf i carries the expected program step (authority-bearing steps
        // like appoint-joseph and activate-rationing-policy stay visible as
        // activities).
        match model {
            Powl::PartialOrder { children, order } => {
                assert_eq!(
                    children.len(),
                    EXPECTED_STEPS.len(),
                    "expected one POWL child per national program step"
                );
                assert_eq!(
                    order.len(),
                    EXPECTED_ORDER_PAIRS,
                    "expected the full transitively-closed chain order (C(20,2) = 190)"
                );
                for (i, child) in children.iter().enumerate() {
                    match child {
                        Powl::Leaf(Some(label)) => {
                            assert!(
                                label.contains(EXPECTED_STEPS[i]),
                                "leaf {i} label {label:?} must carry program step {:?}",
                                EXPECTED_STEPS[i]
                            );
                        }
                        other => {
                            panic!("expected Powl::Leaf(Some(_)) for every child, got {other:?}")
                        }
                    }
                }
            }
            other => panic!("expected Powl::PartialOrder, got {other:?}"),
        }

        Ok::<(), Refusal>(())
    }
);

test!(joseph_powl_exports_parses_and_is_deterministic, {
    // Arrange: regenerate the combined plan and model.
    let tape = generate_combined_plan(None)?;
    let model = project_pddl_tape_to_powl(&tape)?;

    // Act: serialize twice with provenance pointing at the combined plan,
    // then export the one national workflow artifact.
    let turtle_a = powl_to_turtle(&model, BASE_IRI, Some(PLAN_SOURCE_IRI))?;
    let turtle_b = powl_to_turtle(&model, BASE_IRI, Some(PLAN_SOURCE_IRI))?;
    let exported = export_powl(EXPORT_FILE, &turtle_a);

    // Assert: exported bytes equal the serialized Turtle; serialization is
    // byte-identical across repeated calls; parsing runs on the file content
    // read back from disk.
    let exported_turtle = fs::read_to_string(&exported)
        .expect("exported POWL artifact must be readable back from disk");
    assert_eq!(
        exported_turtle, turtle_a,
        "exported artifact bytes must equal the serialized Turtle"
    );
    assert_eq!(
        turtle_a, turtle_b,
        "powl_to_turtle must produce byte-identical output across repeated calls"
    );

    let store = Store::new().expect("in-memory oxigraph store construction must not fail");
    store
        .load_from_slice(
            RdfParser::from_format(RdfFormat::Turtle),
            exported_turtle.as_bytes(),
        )
        .expect("exported POWL Turtle must parse as valid Turtle");

    // Assert non-vacuity against the *parsed* RDF via SPARQL counts.
    let solution_count = |query: &str| -> usize {
        let prepared = oxigraph::sparql::SparqlEvaluator::new()
            .parse_query(query)
            .expect("test SPARQL query must parse");
        match prepared.on_store(&store).execute() {
            Ok(oxigraph::sparql::QueryResults::Solutions(solutions)) => solutions.count(),
            _ => panic!("SPARQL query must return solutions: {query}"),
        }
    };
    let count_of = |class_iri: &str| -> usize {
        solution_count(&format!(
            "SELECT ?s WHERE {{ ?s <http://www.w3.org/1999/02/22-rdf-syntax-ns#type> <https://truex.io/ontology/powl2#{class_iri}> }}"
        ))
    };
    assert_eq!(count_of("Model"), 1, "exactly one powl2:Model root node");
    assert_eq!(
        count_of("PartialOrder"),
        1,
        "exactly one powl2:PartialOrder — one national workflow, not one per phase"
    );
    assert_eq!(
        count_of("ActivityLeaf"),
        EXPECTED_STEPS.len(),
        "exactly one powl2:ActivityLeaf per national program step"
    );
    assert_eq!(
        count_of("ChildBinding"),
        EXPECTED_STEPS.len(),
        "exactly one powl2:ChildBinding per national program step"
    );
    let count_predicate = |pred_iri: &str| -> usize {
        solution_count(&format!(
            "SELECT ?s WHERE {{ ?s <https://truex.io/ontology/powl2#{pred_iri}> ?o }}"
        ))
    };
    assert_eq!(
        count_predicate("precedes"),
        EXPECTED_ORDER_PAIRS,
        "the closed order relation for 20 steps is C(20,2) = 190 precedes triples"
    );
    assert_eq!(
        count_predicate("derivedFrom"),
        1,
        "the root model node carries exactly one powl2:derivedFrom provenance triple"
    );

    // Authority-bearing steps must survive as parsed activities — human
    // authority is not turned into invisible automation.
    for step in [
        "appoint-joseph",
        "activate-rationing-policy",
        "provision-family",
    ] {
        let labelled = solution_count(&format!(
            "SELECT ?s WHERE {{ ?s <https://truex.io/ontology/powl2#activityLabel> ?l . FILTER(CONTAINS(STR(?l), \"{step}\")) }}"
        ));
        assert!(
            labelled >= 1,
            "authority-bearing step {step:?} must appear among the parsed activityLabel values"
        );
    }

    Ok::<(), Refusal>(())
});

test!(changing_one_source_artifact_changes_the_powl, {
    // Arrange + Act: the same 26-artifact flow, once canonical and once with
    // the grain-distribution domain fragment substituted by its variant.
    let tape_a = generate_combined_plan(None)?;
    let tape_b = generate_combined_plan(Some((
        "grain-distribution.domain.ttl",
        GRAIN_DISTRIBUTION_VARIANT,
    )))?;
    let turtle_a = powl_to_turtle(
        &project_pddl_tape_to_powl(&tape_a)?,
        BASE_IRI,
        Some(PLAN_SOURCE_IRI),
    )?;
    let turtle_b = powl_to_turtle(
        &project_pddl_tape_to_powl(&tape_b)?,
        BASE_IRI,
        Some(PLAN_SOURCE_IRI),
    )?;

    // Assert: changing one source artifact changes the generated POWL — a
    // hardcoded canned document would be identical for both inputs.
    assert_ne!(
        turtle_a, turtle_b,
        "substituting one domain fragment must change the generated POWL Turtle bytes"
    );
    assert!(
        turtle_a.contains("allocate-grain") && !turtle_a.contains("ration-grain"),
        "canonical output must carry the canonical grain-distribution action label"
    );
    assert!(
        turtle_b.contains("ration-grain") && !turtle_b.contains("allocate-grain"),
        "variant output must carry the variant grain-distribution action label"
    );

    // Assert: both generated documents parse as valid RDF/Turtle.
    for turtle in [&turtle_a, &turtle_b] {
        let store = Store::new().expect("in-memory oxigraph store construction must not fail");
        store
            .load_from_slice(RdfParser::from_format(RdfFormat::Turtle), turtle.as_bytes())
            .expect("every generated POWL Turtle document must parse as valid Turtle");
    }

    Ok::<(), Refusal>(())
});

test!(fragment_merge_refusals_are_typed, {
    // Empty snapshot set refuses PlanInfeasible.
    let engine = ChatmanEngine::in_memory(build_profile()?)?;
    match engine.plan_tape_for_snapshots(&[]) {
        Err(Refusal::PlanInfeasible(msg)) => {
            assert!(!msg.is_empty(), "empty-set refusal must carry a diagnostic");
        }
        other => panic!("expected PlanInfeasible for an empty snapshot set, got {other:?}"),
    }

    // Domain fragments without any problem fragment refuse PlanInfeasible.
    let domain_only: Vec<String> = PHASES.iter().map(|p| format!("{p}.domain.ttl")).collect();
    let (engine, ids) = import_artifacts(&domain_only)?;
    match engine.plan_tape_for_snapshots(&ids) {
        Err(Refusal::PlanInfeasible(msg)) => {
            assert!(
                msg.contains("problem"),
                "missing-problem refusal must name the missing fragment kind, got {msg:?}"
            );
        }
        other => panic!("expected PlanInfeasible without problem fragments, got {other:?}"),
    }

    // A fragment declaring a different domain name refuses ValidationFailed.
    let mut mismatched = canonical_artifact_set(None);
    mismatched.push(MISMATCHED_DOMAIN.to_string());
    let (engine, ids) = import_artifacts(&mismatched)?;
    match engine.plan_tape_for_snapshots(&ids) {
        Err(Refusal::ValidationFailed(msg)) => {
            assert!(
                msg.contains("mismatch"),
                "domain-name refusal must name the mismatch, got {msg:?}"
            );
        }
        other => panic!("expected ValidationFailed for a mismatched domain name, got {other:?}"),
    }

    Ok::<(), Refusal>(())
});
