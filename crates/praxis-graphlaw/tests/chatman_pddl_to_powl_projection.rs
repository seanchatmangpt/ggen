//! Integration tests for `chatman::powl_projection`: importing a PDDL-bearing
//! RDF/Turtle snapshot artifact, planning it into a canonical [`Pddl8Tape`],
//! projecting that tape into a POWL 2.0 model ([`Powl`]), and exporting the
//! model as a POWL v2 RDF/Turtle artifact.
//!
//! Artifact boundary: Turtle is imported or exported, never embedded as Rust
//! string literals. The snapshot fixture lives as a `.ttl` template artifact
//! (`tests/pddl_to_powl/pddl-snapshot.template.ttl`) whose placeholder names
//! are substituted with seeded fake-generated values; the rendered fixture is
//! written under `target/chatman/powl/fixtures/` and read back from disk
//! before import. The exported POWL Turtle is written under
//! `target/chatman/powl/` and its absolute path is printed with the
//! `GENERATED_POWL_TTL_PATH=` marker.
//!
//! The flow is projection-only: `ChatmanEngine::in_memory` -> `load_snapshot`
//! -> `plan_tape_for_snapshot` -> `project_pddl_tape_to_powl` ->
//! `powl_to_turtle`. No `admit_transition`, OCEL, hooks, or receipts — the
//! projection must stand on its own (see `plan_tape_for_snapshot`'s read-only
//! contract in `chatman/engine.rs`).
//!
//! The template PDDL is a strict linear 8-step chain (STRIPS8-bounded): each
//! action's precondition is exactly the previous step's effect, matching
//! `Pddl8Tape::from_plan`'s total-order-chain encoding that
//! `project_pddl_tape_to_powl` is designed to project. Seeded fake values
//! rename every domain/problem/object/predicate/action identifier, so the
//! exported POWL Turtle cannot be a hardcoded canned document: same seed ->
//! byte-identical output, different seed -> different labels and bytes.

use chicago_tdd_tools::prelude::*;

use std::fs;
use std::path::PathBuf;

use bcinr_pddl::Pddl8Tape;
use fake::faker::lorem::en::Word;
use fake::Fake;
use oxigraph::io::{RdfFormat, RdfParser};
use oxigraph::store::Store;
use powl2_decompose::Powl;
use rand::rngs::StdRng;
use rand::SeedableRng;

use praxis_graphlaw::chatman::abi::{GraphSnapshotId, ProfileId, Refusal};
use praxis_graphlaw::chatman::engine::{AdmissionSpec, ChatmanEngine, EngineProfile};
use praxis_graphlaw::chatman::powl_projection::{powl_to_turtle, project_pddl_tape_to_powl};
use praxis_graphlaw::chatman::router::ProfileGates;
use praxis_graphlaw::chatman::triple8::ProfileSymbolTable;

const PROFILE_IRI: &str = "profile:pddl-to-powl-projection-test";
/// Fixed fake-data seeds: `SEED_A` drives the main projection/export tests;
/// `SEED_B` exists only to prove that a different seed produces different
/// PDDL fixture bytes and different POWL output bytes (anti-hardcoding).
const SEED_A: u64 = 1;
const SEED_B: u64 = 2;
/// Number of plan steps in the template's linear chain (and thus leaves in
/// the projected PartialOrder). C(8,2) = 28 is the closed order-pair count.
const STEPS: usize = 8;

/// A seeded, fully fake-named PDDL snapshot fixture rendered to disk.
struct Fixture {
    /// Canonical absolute path of the rendered importable `.ttl` artifact.
    rendered_path: PathBuf,
    /// The snapshot graph IRI the fixture is loaded under.
    snapshot_iri: String,
    /// Base IRI for the exported POWL model's structural node IRIs.
    base_iri: String,
    /// Generated action names, in plan order (`action_names[i]` is step i).
    action_names: Vec<String>,
    seed: u64,
}

/// Repo-local generated-artifact directory (`<repo>/target/chatman/powl`),
/// following the `target/praxis-standing/` precedent. Never a source or docs
/// directory, never outside the repo.
fn powl_output_dir() -> PathBuf {
    PathBuf::from(env!("CARGO_MANIFEST_DIR"))
        .join("..")
        .join("..")
        .join("target")
        .join("chatman")
        .join("powl")
}

/// Renders the `.ttl` template artifact with seeded fake-generated names and
/// writes the importable fixture to
/// `target/chatman/powl/fixtures/pddl-snapshot.seed-<seed>.ttl`.
///
/// All identifiers (domain, problem, case object, 9 predicates, 8 actions)
/// come from a seeded RNG via the `fake` crate, index-suffixed so they stay
/// unique, lowercase-ASCII+hyphen so the PDDL stays valid, and chain-shaped
/// by the template so the problem stays solvable.
fn generate_fixture(seed: u64) -> Fixture {
    let mut rng = StdRng::seed_from_u64(seed);
    let mut word = || -> String { Word().fake_with_rng::<String, _>(&mut rng).to_lowercase() };

    let domain_name = format!("domain-{}", word());
    let problem_name = format!("problem-{}", word());
    let case_object = format!("case-{}", word());
    let base_suffix = word();
    let predicates: Vec<String> = (0..=STEPS)
        .map(|i| format!("state-{}-{i}", word()))
        .collect();
    let action_names: Vec<String> = (0..STEPS).map(|i| format!("step-{}-{i}", word())).collect();

    let template_path = PathBuf::from(env!("CARGO_MANIFEST_DIR"))
        .join("tests")
        .join("pddl_to_powl")
        .join("pddl-snapshot.template.ttl");
    let mut rendered = fs::read_to_string(&template_path)
        .expect("template artifact tests/pddl_to_powl/pddl-snapshot.template.ttl must be readable");

    rendered = rendered
        .replace("{{DOMAIN_NAME}}", &domain_name)
        .replace("{{PROBLEM_NAME}}", &problem_name)
        .replace("{{CASE_OBJECT}}", &case_object);
    for (i, pred) in predicates.iter().enumerate() {
        rendered = rendered.replace(&format!("{{{{PRED_{i}}}}}"), pred);
    }
    for (i, action) in action_names.iter().enumerate() {
        rendered = rendered.replace(&format!("{{{{ACTION_{i}}}}}"), action);
    }
    assert!(
        !rendered.contains("{{"),
        "every template placeholder must be substituted; leftover marker in rendered fixture"
    );

    let fixtures_dir = powl_output_dir().join("fixtures");
    fs::create_dir_all(&fixtures_dir).expect("fixture output directory must be creatable");
    let file_path = fixtures_dir.join(format!("pddl-snapshot.seed-{seed:03}.ttl"));
    fs::write(&file_path, &rendered).expect("rendered PDDL fixture must be writable");
    let rendered_path = file_path
        .canonicalize()
        .expect("rendered PDDL fixture path must canonicalize");
    println!("IMPORTED_PDDL_TTL_PATH={}", rendered_path.display());

    Fixture {
        rendered_path,
        snapshot_iri: format!("urn:chatman:snapshot:pddl-to-powl:seed-{seed}"),
        base_iri: format!("urn:chatman:powl:{base_suffix}-seed-{seed}"),
        action_names,
        seed,
    }
}

/// Writes the exported POWL Turtle artifact to `target/chatman/powl/` and
/// prints its canonical absolute path with the `GENERATED_POWL_TTL_PATH=`
/// marker.
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

/// Imports the rendered fixture artifact from disk (the only route Turtle
/// enters this test) and returns the plan tape for its snapshot.
fn plan_tape_for_fixture(fixture: &Fixture) -> Result<Pddl8Tape, Refusal> {
    let turtle = fs::read_to_string(&fixture.rendered_path)
        .expect("rendered PDDL fixture must be readable back from disk");
    let profile = build_profile()?;
    let mut engine = ChatmanEngine::in_memory(profile)?;
    let snapshot_id = GraphSnapshotId::new(&fixture.snapshot_iri);
    engine.load_snapshot(&snapshot_id, &turtle)?;
    engine.plan_tape_for_snapshot(&snapshot_id)
}

test!(pddl_plan_projects_to_powl_partial_order_chain, {
    // Arrange + Act: render the seeded fixture artifact, import it from disk,
    // pull the plan tape via the read-only side-door, and project to POWL.
    let fixture = generate_fixture(SEED_A);
    let tape = plan_tape_for_fixture(&fixture)?;
    let model = project_pddl_tape_to_powl(&tape)?;

    // Assert: PartialOrder with 8 leaf children and the full
    // transitively-closed chain order (C(8,2) = 28 pairs).
    match model {
        Powl::PartialOrder { children, order } => {
            assert_eq!(
                children.len(),
                STEPS,
                "expected one POWL child per plan step"
            );
            assert_eq!(
                order.len(),
                28,
                "expected the full transitively-closed chain order for 8 steps (C(8,2) = 28)"
            );
            for (i, child) in children.iter().enumerate() {
                match child {
                    Powl::Leaf(Some(label)) => {
                        assert!(
                            !label.is_empty(),
                            "every plan-step leaf must carry a non-empty activity label"
                        );
                        // Anti-hardcoding: leaf i's label must carry the
                        // seeded fake-generated action name for step i.
                        assert!(
                            label.contains(&fixture.action_names[i]),
                            "leaf {i} label {label:?} must contain generated action name {:?}",
                            fixture.action_names[i]
                        );
                    }
                    other => panic!("expected Powl::Leaf(Some(_)) for every child, got {other:?}"),
                }
            }
        }
        other => panic!("expected Powl::PartialOrder, got {other:?}"),
    }

    Ok::<(), Refusal>(())
});

test!(powl_turtle_output_parses_and_is_deterministic, {
    // Arrange: rebuild the POWL model from the same seeded fixture artifact.
    let fixture = generate_fixture(SEED_A);
    let tape = plan_tape_for_fixture(&fixture)?;
    let model = project_pddl_tape_to_powl(&tape)?;

    // Act: serialize twice with provenance pointing at the imported snapshot,
    // then export the Turtle as an on-disk artifact.
    let derived_from = Some(fixture.snapshot_iri.as_str());
    let turtle_a = powl_to_turtle(&model, &fixture.base_iri, derived_from)?;
    let turtle_b = powl_to_turtle(&model, &fixture.base_iri, derived_from)?;
    let exported = export_powl(
        &format!("pddl-to-powl-projection.seed-{:03}.ttl", fixture.seed),
        &turtle_a,
    );

    // Assert: the exported artifact exists and holds exactly the serialized
    // bytes; all parsing below runs on the file content read back from disk.
    let exported_turtle = fs::read_to_string(&exported)
        .expect("exported POWL Turtle artifact must be readable back from disk");
    assert_eq!(
        exported_turtle, turtle_a,
        "exported artifact bytes must equal the serialized Turtle"
    );

    // Assert: the exported artifact parses as valid Turtle, using the same
    // oxigraph Turtle-parsing API as `ChatmanEngine::load_snapshot`.
    let store = Store::new().expect("in-memory oxigraph store construction must not fail");
    store
        .load_from_slice(
            RdfParser::from_format(RdfFormat::Turtle),
            exported_turtle.as_bytes(),
        )
        .expect("exported POWL Turtle artifact must parse as valid Turtle");
    let triple_count = store
        .len()
        .expect("store.len() must succeed after a successful load");
    assert!(
        triple_count > 0,
        "expected a non-zero triple count from parsing the exported Turtle"
    );

    // Assert non-vacuity against the *parsed* RDF, not just the source text:
    // count instances of each required node kind via SPARQL over the store
    // (same `SparqlEvaluator` interface as `ChatmanEngine`'s query paths).
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
        "exactly one powl2:PartialOrder node for a linear plan"
    );
    assert_eq!(
        count_of("ActivityLeaf"),
        STEPS,
        "exactly one powl2:ActivityLeaf per PDDL plan action (8-step fixture)"
    );
    assert_eq!(
        count_of("ChildBinding"),
        STEPS,
        "exactly one powl2:ChildBinding per plan step"
    );

    let count_predicate = |pred_iri: &str| -> usize {
        solution_count(&format!(
            "SELECT ?s WHERE {{ ?s <https://truex.io/ontology/powl2#{pred_iri}> ?o }}"
        ))
    };
    assert_eq!(
        count_predicate("precedes"),
        28,
        "the transitively-closed order relation for 8 steps is C(8,2) = 28 precedes triples"
    );
    assert_eq!(
        count_predicate("derivedFrom"),
        1,
        "the root model node carries exactly one powl2:derivedFrom provenance triple"
    );

    // Anti-hardcoding: every seeded fake-generated action name must appear
    // among the parsed graph's activityLabel values — a canned Turtle
    // document could not contain them.
    for action in &fixture.action_names {
        let labelled = solution_count(&format!(
            "SELECT ?s WHERE {{ ?s <https://truex.io/ontology/powl2#activityLabel> ?l . FILTER(CONTAINS(STR(?l), \"{action}\")) }}"
        ));
        assert_eq!(
            labelled, 1,
            "generated action name {action:?} must appear in exactly one parsed activityLabel"
        );
    }
    // Supplemental string check only (the parsed-graph checks above are the proof).
    assert!(
        exported_turtle.contains(&fixture.snapshot_iri),
        "the derivedFrom target IRI must appear in the exported Turtle"
    );

    // Assert: determinism — two calls on the same model produce
    // byte-identical Turtle.
    assert_eq!(
        turtle_a, turtle_b,
        "powl_to_turtle must produce byte-identical output across repeated calls on the same model"
    );

    Ok::<(), Refusal>(())
});

test!(different_fake_seeds_produce_different_powl, {
    // Arrange + Act: run the full import -> plan -> project -> export flow
    // for two different fake seeds.
    let fixture_a = generate_fixture(SEED_A);
    let fixture_b = generate_fixture(SEED_B);
    let pddl_a =
        fs::read_to_string(&fixture_a.rendered_path).expect("seed-A PDDL fixture must be readable");
    let pddl_b =
        fs::read_to_string(&fixture_b.rendered_path).expect("seed-B PDDL fixture must be readable");
    assert_ne!(
        pddl_a, pddl_b,
        "different fake seeds must render different PDDL fixture artifacts"
    );

    let tape_a = plan_tape_for_fixture(&fixture_a)?;
    let tape_b = plan_tape_for_fixture(&fixture_b)?;
    let model_a = project_pddl_tape_to_powl(&tape_a)?;
    let model_b = project_pddl_tape_to_powl(&tape_b)?;
    let turtle_a = powl_to_turtle(&model_a, &fixture_a.base_iri, Some(&fixture_a.snapshot_iri))?;
    let turtle_b = powl_to_turtle(&model_b, &fixture_b.base_iri, Some(&fixture_b.snapshot_iri))?;
    let path_a = export_powl(
        &format!("pddl-to-powl-projection.seed-{:03}.ttl", fixture_a.seed),
        &turtle_a,
    );
    let path_b = export_powl(
        &format!("pddl-to-powl-projection.seed-{:03}.ttl", fixture_b.seed),
        &turtle_b,
    );
    assert_ne!(path_a, path_b, "seeded export paths must not collide");

    // Assert: the exported POWL artifacts differ byte-wise and label-wise —
    // a hardcoded canned Turtle document would be identical for both seeds.
    assert_ne!(
        turtle_a, turtle_b,
        "different fake seeds must produce different POWL Turtle bytes"
    );
    assert_ne!(
        fixture_a.action_names, fixture_b.action_names,
        "different fake seeds must generate different visible activity labels"
    );
    for (action_a, action_b) in fixture_a.action_names.iter().zip(&fixture_b.action_names) {
        assert!(
            turtle_a.contains(action_a) && !turtle_b.contains(action_a),
            "seed-A action {action_a:?} must appear only in seed-A output"
        );
        assert!(
            turtle_b.contains(action_b) && !turtle_a.contains(action_b),
            "seed-B action {action_b:?} must appear only in seed-B output"
        );
    }

    // Assert: both exported artifacts parse as valid RDF/Turtle.
    for path in [&path_a, &path_b] {
        let content = fs::read_to_string(path).expect("exported POWL artifact must be readable");
        let store = Store::new().expect("in-memory oxigraph store construction must not fail");
        store
            .load_from_slice(
                RdfParser::from_format(RdfFormat::Turtle),
                content.as_bytes(),
            )
            .expect("every exported POWL Turtle artifact must parse as valid Turtle");
    }

    Ok::<(), Refusal>(())
});

test!(empty_pddl_tape_refuses_plan_infeasible, {
    // Arrange: an empty PDDL plan tape (no ops), constructed directly
    // since `Pddl8Tape` and its `ops` field are both public.
    let empty_tape = Pddl8Tape { ops: vec![] };

    // Act
    let result = project_pddl_tape_to_powl(&empty_tape);

    // Assert: refused specifically as PlanInfeasible, not any other
    // Refusal variant.
    match result {
        Err(Refusal::PlanInfeasible(msg)) => {
            assert!(
                !msg.is_empty(),
                "PlanInfeasible refusal must carry a non-empty diagnostic message"
            );
        }
        Err(other) => panic!("expected Refusal::PlanInfeasible, got {other:?}"),
        Ok(model) => panic!("expected a refusal for an empty plan tape, got Ok({model:?})"),
    }

    Ok::<(), Refusal>(())
});
