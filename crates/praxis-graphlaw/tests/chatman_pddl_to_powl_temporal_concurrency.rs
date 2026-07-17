//! Temporal-concurrency PDDL to POWL projection: a genuinely concurrent
//! `:durative-action` schedule (real precondition/effect time-interval
//! overlap via `bcinr_pddl::ground::GroundTemporalProblem`, not a fixed BFS
//! total order) projects into a POWL 2.0 `PartialOrder` that keeps
//! independent steps as siblings.
//!
//! This proves `chatman::powl_projection::project_temporal_plan_to_powl`
//! captures real concurrency instead of spuriously serializing it, the way
//! the classical `project_pddl_tape_to_powl` path (bounded to a
//! total-order `Pddl8Tape`, see that function's own doc comment) necessarily
//! does for its own input shape.
//!
//! Scope: this proves the new temporal wiring
//! (`ChatmanEngine::plan_temporal_tape_for_snapshot` ->
//! `bcinr_pddl::ground::GroundTemporalProblem::find_temporal_plan` ->
//! `bcinr_pddl::powl_bridge::temporal_plan_to_powl_tape` ->
//! `project_temporal_plan_to_powl`) is real and sound against a
//! purpose-built durative-action fixture. It does NOT migrate any existing
//! classical-action `chatman_pddl_to_powl_*` fixture to durative syntax, and
//! it does NOT make the temporal path the default for all PDDL input --
//! both are explicitly out of scope for this test; every other
//! `chatman_pddl_to_powl_*` test keeps exercising the classical
//! `project_pddl_tape_to_powl` path unchanged.
//!
//! Artifact boundary: PDDL is imported from a committed `.ttl` file
//! (`tests/pddl_to_powl/temporal-concurrency-snapshot.ttl`); no PDDL or
//! Turtle is embedded in this Rust file. The exported POWL Turtle is
//! written under `target/chatman/powl/`.
//!
//! Scenario: `wash-dishes` (duration 2) and `sweep-floor` (duration 3)
//! share no predicate and have no precondition on each other, so the real
//! temporal planner starts both at t=0 -- genuinely concurrent. A third
//! action, `inspect-house` (duration 1), requires both finished, so it can
//! only start at t=3, strictly after both -- proving real sequential edges
//! are still captured, not just "no edges anywhere".

use chicago_tdd_tools::prelude::*;

use std::fs;
use std::path::PathBuf;

use oxigraph::io::{RdfFormat, RdfParser};
use oxigraph::sparql::{QueryResults, SparqlEvaluator};
use oxigraph::store::Store;
use powl2_decompose::Powl;

use bcinr_pddl::{TemporalPlan, TemporalPlanStep};
use praxis_graphlaw::chatman::abi::{GraphSnapshotId, ProfileId, Refusal};
use praxis_graphlaw::chatman::engine::{AdmissionSpec, ChatmanEngine, EngineProfile};
use praxis_graphlaw::chatman::powl_projection::{powl_to_turtle, project_temporal_plan_to_powl};
use praxis_graphlaw::chatman::router::ProfileGates;
use praxis_graphlaw::chatman::triple8::ProfileSymbolTable;

const PROFILE_IRI: &str = "profile:temporal-concurrency-projection-test";
const SNAPSHOT_IRI: &str = "urn:chatman:snapshot:temporal-concurrency-demo";
const BASE_IRI: &str = "urn:chatman:powl:temporal-concurrency-demo";
const EXPORT_FILE: &str = "temporal-concurrency.powl.ttl";
const POWL2_PRECEDES: &str = "https://truex.io/ontology/powl2#precedes";
const POWL2_CHILD_MODEL: &str = "https://truex.io/ontology/powl2#childModel";
const POWL2_ACTIVITY_LABEL: &str = "https://truex.io/ontology/powl2#activityLabel";

fn fixture_path() -> PathBuf {
    PathBuf::from(env!("CARGO_MANIFEST_DIR"))
        .join("tests")
        .join("pddl_to_powl")
        .join("temporal-concurrency-snapshot.ttl")
}

/// Repo-local generated-artifact directory (`<repo>/target/chatman/powl`),
/// same convention as the other `chatman_pddl_to_powl_*` tests.
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

/// Imports the committed fixture from disk (the only route PDDL/Turtle
/// enters this test), loads it as a snapshot, and returns the real
/// temporal plan `ChatmanEngine::plan_temporal_tape_for_snapshot` computes
/// for it -- the `GroundTemporalProblem`/`find_temporal_plan` path, never
/// the classical BFS `GroundProblem`/`find_plan` path.
fn temporal_plan_for_fixture() -> Result<TemporalPlan, Refusal> {
    let path = fixture_path()
        .canonicalize()
        .unwrap_or_else(|e| panic!("fixture must canonicalize: {e}"));
    let turtle = fs::read_to_string(&path)
        .unwrap_or_else(|e| panic!("fixture must be readable back from disk: {e}"));
    println!("IMPORTED_PDDL_TTL_PATH={}", path.display());

    let profile = build_profile()?;
    let mut engine = ChatmanEngine::in_memory(profile)?;
    let snapshot_id = GraphSnapshotId::new(SNAPSHOT_IRI);
    engine.load_snapshot(&snapshot_id, &turtle)?;
    engine.plan_temporal_tape_for_snapshot(&snapshot_id)
}

/// Writes the exported POWL Turtle artifact and prints its canonical
/// absolute path with the `GENERATED_POWL_TTL_PATH=` marker, same
/// convention as the other `chatman_pddl_to_powl_*` tests.
fn export_powl(turtle: &str) -> PathBuf {
    let dir = powl_output_dir();
    fs::create_dir_all(&dir).expect("POWL output directory must be creatable");
    let file_path = dir.join(EXPORT_FILE);
    fs::write(&file_path, turtle).expect("exported POWL Turtle must be writable");
    let path = file_path
        .canonicalize()
        .expect("exported POWL Turtle path must canonicalize");
    println!("GENERATED_POWL_TTL_PATH={}", path.display());
    path
}

test!(temporal_plan_schedules_independent_chores_concurrently, {
    // Arrange + Act: real temporal planning through GroundTemporalProblem,
    // not the classical BFS.
    let plan = temporal_plan_for_fixture()?;

    // Assert: three scheduled steps; wash-dishes and sweep-floor both start
    // at t=0 (genuinely concurrent -- neither depends on the other), and
    // inspect-house starts only at t=3, strictly after both finish.
    assert_eq!(
        plan.steps.len(),
        3,
        "expected three scheduled temporal steps, got {:?}",
        plan.steps
    );
    let find_step = |name: &str| {
        plan.steps
            .iter()
            .find(|s| s.action_name == name)
            .unwrap_or_else(|| panic!("step {name:?} missing from plan: {:?}", plan.steps))
    };
    let wash = find_step("wash-dishes");
    let sweep = find_step("sweep-floor");
    let inspect = find_step("inspect-house");

    assert_eq!(wash.start_time, 0.0, "wash-dishes must start at t=0");
    assert_eq!(
        sweep.start_time, 0.0,
        "sweep-floor must start at t=0, concurrently with wash-dishes"
    );
    assert_eq!(
        inspect.start_time, 3.0,
        "inspect-house must wait until both chores finish (t=3, sweep-floor's completion)"
    );

    Ok::<(), Refusal>(())
});

test!(
    temporal_projection_keeps_independent_steps_as_siblings_not_a_chain,
    {
        // Arrange
        let plan = temporal_plan_for_fixture()?;

        // Act
        let model = project_temporal_plan_to_powl(&plan)?;

        // Assert: a single PartialOrder over 3 leaves, with exactly the two
        // real sequential edges (wash-dishes -> inspect-house, sweep-floor
        // -> inspect-house) and NO edge at all between wash-dishes and
        // sweep-floor in either direction -- proving the projector did not
        // fall back to full transitive-closure-of-tape-position (the bug
        // this fixture exists to catch; a naive total-order projection of 3
        // steps would report C(3,2) = 3 pairs, including a spurious one
        // between the two concurrent chores).
        let (children, order) = match model {
            Powl::PartialOrder { children, order } => (children, order),
            other => panic!("expected Powl::PartialOrder, got {other:?}"),
        };
        assert_eq!(
            children.len(),
            3,
            "expected one POWL leaf per scheduled step"
        );

        let index_of = |name: &str| -> usize {
            children
                .iter()
                .position(|c| matches!(c, Powl::Leaf(Some(label)) if label.starts_with(name)))
                .unwrap_or_else(|| panic!("no leaf labelled {name:?} among {children:?}"))
        };
        let wash_idx = index_of("wash-dishes");
        let sweep_idx = index_of("sweep-floor");
        let inspect_idx = index_of("inspect-house");

        assert_eq!(
            order.len(),
            2,
            "expected exactly two precedence pairs (wash->inspect, sweep->inspect); got {order:?}"
        );
        assert!(
            order.contains(&(wash_idx, inspect_idx)),
            "wash-dishes must precede inspect-house in the closed order: {order:?}"
        );
        assert!(
            order.contains(&(sweep_idx, inspect_idx)),
            "sweep-floor must precede inspect-house in the closed order: {order:?}"
        );
        assert!(
            !order.contains(&(wash_idx, sweep_idx)) && !order.contains(&(sweep_idx, wash_idx)),
            "wash-dishes and sweep-floor are independent and concurrent -- there must be NO \
             precedes edge between them in either direction: {order:?}"
        );

        Ok::<(), Refusal>(())
    }
);

test!(
    temporal_projection_turtle_omits_precedes_between_concurrent_siblings,
    {
        // Arrange + Act: serialize the temporal model to Turtle and assert on
        // the actual parsed RDF structure, not just "it doesn't error".
        let plan = temporal_plan_for_fixture()?;
        let model = project_temporal_plan_to_powl(&plan)?;
        let turtle_a = powl_to_turtle(&model, BASE_IRI, Some(SNAPSHOT_IRI))?;
        let turtle_b = powl_to_turtle(&model, BASE_IRI, Some(SNAPSHOT_IRI))?;
        let exported = export_powl(&turtle_a);

        // Assert: determinism -- byte-identical output across repeated calls
        // on the same model, same pattern as the other
        // `chatman_pddl_to_powl_*` tests' turtle_a/turtle_b checks.
        assert_eq!(
            turtle_a, turtle_b,
            "powl_to_turtle must produce byte-identical output across repeated calls"
        );
        let exported_turtle = fs::read_to_string(&exported)
            .expect("exported POWL Turtle artifact must be readable back from disk");
        assert_eq!(
            exported_turtle, turtle_a,
            "exported artifact bytes must equal the serialized Turtle"
        );

        // Assert: the exported artifact parses as valid Turtle.
        let store = Store::new().expect("in-memory oxigraph store construction must not fail");
        store
            .load_from_slice(
                RdfParser::from_format(RdfFormat::Turtle),
                exported_turtle.as_bytes(),
            )
            .expect("exported POWL Turtle artifact must parse as valid Turtle");

        let solution_count = |query: &str| -> usize {
            let prepared = SparqlEvaluator::new()
                .parse_query(query)
                .expect("test SPARQL query must parse");
            match prepared.on_store(&store).execute() {
                Ok(QueryResults::Solutions(solutions)) => solutions.count(),
                _ => panic!("SPARQL query must return solutions: {query}"),
            }
        };

        // Exactly 2 powl2:precedes triples over the PARSED graph (3 would be
        // the naive total-order-chain bug's signature for a 3-step plan).
        let precedes_count = solution_count(&format!(
            "SELECT ?s ?o WHERE {{ ?s <{POWL2_PRECEDES}> ?o }}"
        ));
        assert_eq!(
            precedes_count, 2,
            "expected exactly 2 powl2:precedes triples over the parsed RDF"
        );

        // Directly resolve every powl2:precedes triple's endpoints down to
        // their activityLabel and assert no (wash-dishes, sweep-floor) pair
        // appears in either direction -- proving the *serialized, parsed*
        // artifact (not just the in-memory model) never encodes a
        // concurrency-violating edge between the two independent chores.
        let precedes_labels = solution_count(&format!(
            "SELECT ?labelS ?labelO WHERE {{
                ?bs <{POWL2_PRECEDES}> ?bo .
                ?bs <{POWL2_CHILD_MODEL}> ?ls .
                ?ls <{POWL2_ACTIVITY_LABEL}> ?labelS .
                ?bo <{POWL2_CHILD_MODEL}> ?lo .
                ?lo <{POWL2_ACTIVITY_LABEL}> ?labelO .
                FILTER(
                    (CONTAINS(STR(?labelS), \"wash-dishes\") && CONTAINS(STR(?labelO), \"sweep-floor\")) ||
                    (CONTAINS(STR(?labelS), \"sweep-floor\") && CONTAINS(STR(?labelO), \"wash-dishes\"))
                )
            }}"
        ));
        assert_eq!(
            precedes_labels, 0,
            "no powl2:precedes triple may connect the wash-dishes and sweep-floor activity \
             leaves in either direction -- they are independent and concurrent"
        );

        // Sanity: both real sequential edges resolve to inspect-house as the
        // target label, over the parsed graph.
        let inspect_targets = solution_count(&format!(
            "SELECT ?labelS WHERE {{
                ?bs <{POWL2_PRECEDES}> ?bo .
                ?bs <{POWL2_CHILD_MODEL}> ?ls .
                ?ls <{POWL2_ACTIVITY_LABEL}> ?labelS .
                ?bo <{POWL2_CHILD_MODEL}> ?lo .
                ?lo <{POWL2_ACTIVITY_LABEL}> ?labelO .
                FILTER(CONTAINS(STR(?labelO), \"inspect-house\"))
            }}"
        ));
        assert_eq!(
            inspect_targets, 2,
            "both wash-dishes and sweep-floor must precede inspect-house over the parsed graph"
        );

        Ok::<(), Refusal>(())
    }
);

test!(empty_temporal_plan_refuses_plan_infeasible, {
    // A TemporalPlan with zero steps, constructed directly (both `steps`
    // and `TemporalPlan` are public), must refuse the same way the
    // classical projection's empty-tape case does.
    let empty_plan = TemporalPlan {
        steps: vec![],
        makespan: 0.0,
        metric_value: None,
    };

    let result = project_temporal_plan_to_powl(&empty_plan);

    match result {
        Err(Refusal::PlanInfeasible(msg)) => {
            assert!(
                !msg.is_empty(),
                "PlanInfeasible refusal must carry a non-empty diagnostic message"
            );
        }
        Err(other) => panic!("expected Refusal::PlanInfeasible, got {other:?}"),
        Ok(model) => panic!("expected a refusal for an empty temporal plan, got Ok({model:?})"),
    }

    Ok::<(), Refusal>(())
});

test!(oversized_temporal_plan_refuses_plan_infeasible, {
    // A TemporalPlan with 65 steps, one past the 64-bit pred_mask/succ_mask
    // width `bcinr_pddl::powl_bridge::temporal_plan_to_powl_tape` assumes.
    // Not reachable through real PDDL today (bcinr_pddl's own
    // PDDL8_MAX_PLAN_DEPTH bounds TemporalPlan::steps at exactly 64), but
    // constructed directly here to prove the defensive bound in
    // `project_temporal_plan_to_powl` actually refuses rather than
    // overflowing a `1u64 << i` shift if that upstream invariant ever
    // changes.
    let steps: Vec<TemporalPlanStep> = (0..65)
        .map(|i| TemporalPlanStep {
            start_time: i as f64,
            duration: 1.0,
            action_name: format!("step-{i}"),
            args: vec![],
        })
        .collect();
    let oversized_plan = TemporalPlan {
        steps,
        makespan: 65.0,
        metric_value: None,
    };

    let result = project_temporal_plan_to_powl(&oversized_plan);

    match result {
        Err(Refusal::PlanInfeasible(msg)) => {
            assert!(
                !msg.is_empty(),
                "PlanInfeasible refusal must carry a non-empty diagnostic message"
            );
        }
        Err(other) => panic!("expected Refusal::PlanInfeasible, got {other:?}"),
        Ok(model) => {
            panic!("expected a refusal for a 65-step temporal plan, got Ok({model:?})")
        }
    }

    Ok::<(), Refusal>(())
});

test!(out_of_order_temporal_plan_refuses_validation_failed, {
    // `project_temporal_plan_to_powl`'s transitive-closure recovery is exact
    // only when `TemporalPlan::steps` is sorted by non-decreasing
    // `start_time` (see that function's doc comment). The one real producer
    // (`GroundTemporalProblem::find_temporal_plan`) guarantees this by
    // construction, but both `TemporalPlan` and `TemporalPlanStep` are
    // public with a public `steps: Vec<TemporalPlanStep>` field -- the same
    // direct-construction pattern the empty-plan and oversized-plan tests
    // above already rely on. A directly constructed, out-of-order plan must
    // be refused loudly rather than silently dropping a required precedence
    // edge.
    let out_of_order_plan = TemporalPlan {
        steps: vec![
            TemporalPlanStep {
                start_time: 5.0,
                duration: 1.0,
                action_name: "starts-late".to_string(),
                args: vec![],
            },
            TemporalPlanStep {
                start_time: 0.0,
                duration: 1.0,
                action_name: "starts-early".to_string(),
                args: vec![],
            },
        ],
        makespan: 6.0,
        metric_value: None,
    };

    let result = project_temporal_plan_to_powl(&out_of_order_plan);

    match result {
        Err(Refusal::ValidationFailed(msg)) => {
            assert!(
                !msg.is_empty(),
                "ValidationFailed refusal must carry a non-empty diagnostic message"
            );
        }
        Err(other) => panic!("expected Refusal::ValidationFailed, got {other:?}"),
        Ok(model) => {
            panic!("expected a refusal for an out-of-order temporal plan, got Ok({model:?})")
        }
    }

    Ok::<(), Refusal>(())
});
