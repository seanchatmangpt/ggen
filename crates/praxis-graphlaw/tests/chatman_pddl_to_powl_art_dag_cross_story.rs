//! SAFe Agile-Release-Train work breakdown as a real PDDL temporal planning
//! problem admitted through the real POWL projection + closure-law pipeline:
//! a general DAG (one shared story required by two otherwise-independent
//! features, converging again at a downstream milestone), not a strict
//! Galton-Watson tree, is handled by the engine's own admission logic rather
//! than asserted by hand.
//!
//! Sibling to `chatman_pddl_to_powl_temporal_concurrency.rs`: same fixture
//! conventions, same real `GroundTemporalProblem::find_temporal_plan` ->
//! `temporal_plan_to_powl_tape` -> `project_temporal_plan_to_powl` pipeline.
//! That fixture proves a single 2-parent convergence (inspect-house requires
//! both wash-dishes and sweep-floor). This fixture proves a richer shape: a
//! shared story (`auth-migration`) fans out to two independent features
//! (`checkout-v2`, `admin-portal`) of unequal duration, both of which -- plus
//! a third, fully independent feature (`reporting-dashboard`) -- converge
//! again at `integration-milestone`. Because `auth-migration` reaches
//! `integration-milestone` via two distinct paths (through `checkout-v2` and
//! through `admin-portal`), the resulting precedence graph is a genuine
//! diamond: no strict tree can express two distinct paths between the same
//! pair of nodes.
//!
//! # A real, disclosed structural gap this test surfaces
//!
//! `project_temporal_plan_to_powl` (`chatman::powl_projection`) always
//! returns a single *flat* `Powl::PartialOrder`: every scheduled step is a
//! direct child (sibling) of one root node, and the DAG structure (fan-out,
//! convergence, the diamond above) lives entirely in that node's `order`
//! (precedence) relation -- never in `Powl` tree *containment*.
//! `chatman::closure::RecursiveSocketClosure` / `powl2_decompose::
//! ParentChildClosure`, by contrast, only ever see tree containment
//! (`ParentChildClosure::from_model`'s DFS walks `PartialOrder`/`Choice`
//! `children` and `ExternalCut`'s `region`, never a `PartialOrder`'s `order`
//! set -- confirmed by reading `powl2-decompose/src/powl.rs`'s
//! `ParentChildClosure::walk`). So there is today no direct wiring from a
//! temporal-plan-projected precedence DAG to a closure-law socket's declared
//! children: a caller wanting to run `RecursiveSocketClosure` over the shape
//! `project_temporal_plan_to_powl` just proved real must build a *separate*,
//! genuinely nested `Powl` model whose containment mirrors the desired
//! socket/child relationship. This test does exactly that in
//! `feature_readiness_socket`/`integration_readiness_socket` below, reusing
//! the *real* leaf activity labels the temporal projection produced (never
//! fabricating a new identifier), but hand-building the nesting itself --
//! this is the one honest seam between "the DAG survives the temporal
//! projection" (asserted directly against `project_temporal_plan_to_powl`'s
//! own output below) and "the DAG is admitted via closure law" (asserted
//! against these small derived models). Both halves are real, but they are
//! two different `Powl` values, not one continuous pipeline call.
//!
//! Artifact boundary: PDDL is imported from a committed `.ttl` file
//! (`tests/pddl_to_powl/art-cross-story-dag-snapshot.ttl`); no PDDL or Turtle
//! is embedded in this Rust file.

use chicago_tdd_tools::prelude::*;

use std::collections::BTreeSet;
use std::fs;
use std::path::PathBuf;

use powl2_decompose::{ParentChildClosure, Powl, SocketKind, SocketPath, WorkflowSocketId};

use bcinr_pddl::TemporalPlan;
use praxis_graphlaw::chatman::abi::{GraphSnapshotId, ProfileId, Refusal};
use praxis_graphlaw::chatman::closure::{ClosureLaw, RecursiveSocketClosure};
use praxis_graphlaw::chatman::engine::{AdmissionSpec, ChatmanEngine, EngineProfile};
use praxis_graphlaw::chatman::powl_projection::project_temporal_plan_to_powl;
use praxis_graphlaw::chatman::router::ProfileGates;
use praxis_graphlaw::chatman::triple8::ProfileSymbolTable;

const PROFILE_IRI: &str = "profile:art-cross-story-dag-projection-test";
const SNAPSHOT_IRI: &str = "urn:chatman:snapshot:art-cross-story-dag-demo";

fn fixture_path() -> PathBuf {
    PathBuf::from(env!("CARGO_MANIFEST_DIR"))
        .join("tests")
        .join("pddl_to_powl")
        .join("art-cross-story-dag-snapshot.ttl")
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

/// Imports the committed fixture from disk, loads it as a snapshot, and
/// returns the real temporal plan `ChatmanEngine::plan_temporal_tape_for_snapshot`
/// computes for it -- the `GroundTemporalProblem`/`find_temporal_plan` path.
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

/// Locates the real projected leaf whose activity label starts with `name`
/// (labels carry a trailing `(args...)` suffix -- see
/// `bcinr_pddl::powl_bridge::temporal_plan_to_powl_tape`'s
/// `format!("{}({})", step.action_name, step.args.join(","))` -- empty for
/// this fixture's zero-parameter actions, but still present), returning its
/// index in `children` and its exact real label text.
///
/// # Complexity
/// O(n) linear scan, n = children.len() (5 here).
fn find_leaf<'a>(children: &'a [Powl], name: &str) -> (usize, &'a str) {
    children
        .iter()
        .enumerate()
        .find_map(|(i, c)| match c {
            Powl::Leaf(Some(label)) if label.starts_with(name) => Some((i, label.as_str())),
            _ => None,
        })
        .unwrap_or_else(|| panic!("no leaf labelled {name:?} among {children:?}"))
}

// ---------------------------------------------------------------------------
// Step 1/2: the real temporal plan and its real POWL precedence graph.
// ---------------------------------------------------------------------------

test!(
    temporal_plan_schedules_shared_story_and_converges_at_integration_milestone,
    {
        // Arrange + Act: real temporal planning through GroundTemporalProblem.
        let plan = temporal_plan_for_fixture()?;

        // Assert: five scheduled steps with the exact computed start times a
        // shared-story fan-out plus a 3-way convergence implies.
        assert_eq!(
            plan.steps.len(),
            5,
            "expected five scheduled temporal steps, got {:?}",
            plan.steps
        );
        let find_step = |name: &str| {
            plan.steps
                .iter()
                .find(|s| s.action_name == name)
                .unwrap_or_else(|| panic!("step {name:?} missing from plan: {:?}", plan.steps))
        };
        let auth = find_step("auth-migration");
        let reporting = find_step("reporting-dashboard");
        let checkout = find_step("checkout-v2");
        let admin = find_step("admin-portal");
        let integration = find_step("integration-milestone");

        // auth-migration and reporting-dashboard are independent of each
        // other -- both start at t=0, genuinely concurrently.
        assert_eq!(auth.start_time, 0.0, "auth-migration must start at t=0");
        assert_eq!(
            reporting.start_time, 0.0,
            "reporting-dashboard must start at t=0, independently of auth-migration"
        );

        // checkout-v2 and admin-portal both gate on auth-migration's real
        // completion at t=4 (duration 4), then run concurrently themselves
        // -- of unequal duration (5 vs 2), proving real concurrent
        // scheduling from a shared start, not incidental sequencing.
        assert_eq!(
            checkout.start_time, 4.0,
            "checkout-v2 must wait for auth-migration's completion (t=4)"
        );
        assert_eq!(
            admin.start_time, 4.0,
            "admin-portal must wait for auth-migration's completion (t=4), concurrently with \
             checkout-v2"
        );
        assert_eq!(checkout.duration, 5.0);
        assert_eq!(admin.duration, 2.0);

        // integration-milestone requires ALL THREE features finished:
        // checkout-v2 ends at 4+5=9, admin-portal at 4+2=6,
        // reporting-dashboard at 0+6=6 -- real max(9, 6, 6) = 9, not a
        // hand-asserted value.
        assert_eq!(
            integration.start_time, 9.0,
            "integration-milestone must wait for the LATEST of all three features to finish \
             (real max(9, 6, 6) = 9, driven by checkout-v2's longer duration)"
        );

        Ok::<(), Refusal>(())
    }
);

test!(
    temporal_projection_preserves_the_shared_story_diamond_as_real_graph_edges,
    {
        // Arrange
        let plan = temporal_plan_for_fixture()?;

        // Act: the real projector under test.
        let model = project_temporal_plan_to_powl(&plan)?;

        // Assert: a flat PartialOrder over 5 leaves (confirms empirically --
        // not by assumption -- that this projector never produces a
        // Powl::Choice/ChoiceGraph for a linear-precedence TemporalPlan; see
        // this file's module doc comment for what that flatness means for
        // closure-law wiring).
        let (children, order) = match model {
            Powl::PartialOrder { children, order } => (children, order),
            other => panic!("expected Powl::PartialOrder, got {other:?}"),
        };
        assert_eq!(
            children.len(),
            5,
            "expected one POWL leaf per scheduled step"
        );

        let (auth_idx, _) = find_leaf(&children, "auth-migration");
        let (reporting_idx, _) = find_leaf(&children, "reporting-dashboard");
        let (checkout_idx, _) = find_leaf(&children, "checkout-v2");
        let (admin_idx, _) = find_leaf(&children, "admin-portal");
        let (integration_idx, _) = find_leaf(&children, "integration-milestone");

        // Real, exact edge count: 6 precedence pairs, computed by the
        // projector's transitive-closure recovery, not hand-picked.
        assert_eq!(
            order.len(),
            6,
            "expected exactly 6 precedence pairs (auth->checkout, auth->admin, \
             auth->integration, checkout->integration, admin->integration, \
             reporting->integration); got {order:?}"
        );

        // The shared story fans out to BOTH otherwise-independent features
        // -- the literal "one story required by two otherwise-independent
        // feature branches" this fixture exists to prove.
        assert!(
            order.contains(&(auth_idx, checkout_idx)),
            "auth-migration must precede checkout-v2: {order:?}"
        );
        assert!(
            order.contains(&(auth_idx, admin_idx)),
            "auth-migration must precede admin-portal: {order:?}"
        );

        // The two features that share auth-migration as a common ancestor
        // are themselves independent of each other -- no edge in either
        // direction.
        assert!(
            !order.contains(&(checkout_idx, admin_idx))
                && !order.contains(&(admin_idx, checkout_idx)),
            "checkout-v2 and admin-portal are independent and concurrent -- there must be NO \
             precedes edge between them in either direction: {order:?}"
        );

        // auth-migration and reporting-dashboard are independent of each
        // other too (both start at t=0) -- no edge in either direction.
        assert!(
            !order.contains(&(auth_idx, reporting_idx))
                && !order.contains(&(reporting_idx, auth_idx)),
            "auth-migration and reporting-dashboard are independent and concurrent -- there \
             must be NO precedes edge between them in either direction: {order:?}"
        );

        // All three features converge at integration-milestone -- genuine
        // multi-parent convergence, in-degree 3 direct.
        assert!(
            order.contains(&(checkout_idx, integration_idx)),
            "checkout-v2 must precede integration-milestone: {order:?}"
        );
        assert!(
            order.contains(&(admin_idx, integration_idx)),
            "admin-portal must precede integration-milestone: {order:?}"
        );
        assert!(
            order.contains(&(reporting_idx, integration_idx)),
            "reporting-dashboard must precede integration-milestone: {order:?}"
        );

        // The real diamond: auth-migration also transitively precedes
        // integration-milestone (via BOTH checkout-v2 and admin-portal --
        // two distinct paths through the same graph, the shape no strict
        // tree can express).
        assert!(
            order.contains(&(auth_idx, integration_idx)),
            "auth-migration must transitively precede integration-milestone (via both \
             checkout-v2 and admin-portal): {order:?}"
        );

        Ok::<(), Refusal>(())
    }
);

// ---------------------------------------------------------------------------
// Step 3: closure-law admission over the same real activity identities, via
// the real `RecursiveSocketClosure`/`ClosureLaw` machinery (see this file's
// module doc comment for exactly why a separate nested model is required).
// ---------------------------------------------------------------------------

fn root_socket() -> WorkflowSocketId {
    WorkflowSocketId {
        path: SocketPath::root(),
        kind: SocketKind::PartialOrder,
    }
}

fn leaf_socket(i: usize) -> WorkflowSocketId {
    WorkflowSocketId {
        path: SocketPath::root().child(i),
        kind: SocketKind::Leaf,
    }
}

/// A two-child `PartialOrder` over `(shared, own)`, both real activity
/// labels copied verbatim from the real temporal projection above -- models
/// one feature's own readiness socket: it cannot close under `AllRequired`
/// until BOTH the shared cross-cutting story AND its own local work are
/// admitted.
fn feature_readiness_socket(shared_label: &str, own_label: &str) -> Powl {
    Powl::PartialOrder {
        children: vec![
            Powl::Leaf(Some(shared_label.to_string())),
            Powl::Leaf(Some(own_label.to_string())),
        ],
        order: BTreeSet::from([(0usize, 1usize)]),
    }
}

test!(
    all_required_closure_shows_two_independent_features_share_one_story,
    {
        // Arrange: the real temporal plan and projection once more, to pull
        // the real activity labels (never fabricated) this closure-law
        // admission is declared over.
        let plan = temporal_plan_for_fixture()?;
        let model = project_temporal_plan_to_powl(&plan)?;
        let children = match &model {
            Powl::PartialOrder { children, .. } => children,
            other => panic!("expected Powl::PartialOrder, got {other:?}"),
        };
        let (_, auth_label) = find_leaf(children, "auth-migration");
        let (_, checkout_label) = find_leaf(children, "checkout-v2");
        let (_, admin_label) = find_leaf(children, "admin-portal");

        // Act + Assert: checkout-v2's own readiness socket, AllRequired over
        // [auth-migration, checkout-v2].
        let checkout_model = feature_readiness_socket(auth_label, checkout_label);
        let checkout_pcc = ParentChildClosure::from_model(&checkout_model);
        let mut checkout_rsc =
            RecursiveSocketClosure::declare(&checkout_pcc, root_socket(), ClosureLaw::AllRequired)?;
        // The shared story alone is not enough -- checkout-v2's own work is
        // still merely observed, not admitted.
        checkout_rsc.admit(&leaf_socket(0))?; // auth-migration
        checkout_rsc.observe(&leaf_socket(1))?; // checkout-v2's own work
        assert!(
            !checkout_rsc.is_closed()?,
            "checkout-v2's readiness must stay open until its own work is admitted too"
        );
        checkout_rsc.admit(&leaf_socket(1))?;
        assert!(
            checkout_rsc.is_closed()?,
            "checkout-v2's readiness closes once BOTH the shared story and its own work are \
             admitted (AllRequired)"
        );
        checkout_rsc.close()?;

        // Act + Assert: admin-portal's own readiness socket, AllRequired
        // over [auth-migration, admin-portal] -- the SAME shared
        // auth-migration label, proving convergence: one story genuinely
        // required by two otherwise-independent feature branches.
        let admin_model = feature_readiness_socket(auth_label, admin_label);
        let admin_pcc = ParentChildClosure::from_model(&admin_model);
        let mut admin_rsc =
            RecursiveSocketClosure::declare(&admin_pcc, root_socket(), ClosureLaw::AllRequired)?;
        // This time: admin-portal's own work is admitted early, but the
        // shared story is not -- AllRequired must still refuse to close.
        admin_rsc.admit(&leaf_socket(1))?; // admin-portal's own work
        assert!(
            !admin_rsc.is_closed()?,
            "admin-portal's readiness must stay open until the shared auth-migration story is \
             admitted too, even though its own work is already admitted"
        );
        assert!(matches!(
            admin_rsc.close(),
            Err(Refusal::ParentClosureUnsatisfied(_))
        ));
        admin_rsc.admit(&leaf_socket(0))?; // auth-migration
        assert!(
            admin_rsc.is_closed()?,
            "admin-portal's readiness closes once BOTH the shared story and its own work are \
             admitted"
        );
        admin_rsc.close()?;

        Ok::<(), Refusal>(())
    }
);

test!(
    quorum_closure_admits_integration_milestone_on_two_of_three_features,
    {
        // Arrange: real activity labels for the three features that
        // converge at integration-milestone.
        let plan = temporal_plan_for_fixture()?;
        let model = project_temporal_plan_to_powl(&plan)?;
        let children = match &model {
            Powl::PartialOrder { children, .. } => children,
            other => panic!("expected Powl::PartialOrder, got {other:?}"),
        };
        let (_, checkout_label) = find_leaf(children, "checkout-v2");
        let (_, admin_label) = find_leaf(children, "admin-portal");
        let (_, reporting_label) = find_leaf(children, "reporting-dashboard");

        let integration_model = Powl::PartialOrder {
            children: vec![
                Powl::Leaf(Some(checkout_label.to_string())),
                Powl::Leaf(Some(admin_label.to_string())),
                Powl::Leaf(Some(reporting_label.to_string())),
            ],
            order: BTreeSet::new(),
        };
        let pcc = ParentChildClosure::from_model(&integration_model);

        // Negative case first: only one of three admitted -- Quorum(2) must
        // refuse to close (a real, non-fabricated failing verdict).
        let mut rsc_one =
            RecursiveSocketClosure::declare(&pcc, root_socket(), ClosureLaw::Quorum(2))?;
        rsc_one.admit(&leaf_socket(0))?; // checkout-v2 only
        assert!(
            !rsc_one.is_closed()?,
            "quorum(2) over 3 features must not close with only 1 admitted"
        );
        assert!(matches!(
            rsc_one.close(),
            Err(Refusal::ParentClosureUnsatisfied(_))
        ));

        // Positive case: 2 of 3 features "good enough" to proceed --
        // reporting-dashboard is left entirely Open, never even observed --
        // modeling Fuller's redundant-alternate-load-path point directly:
        // the integration milestone does not require every branch to be
        // healthy, only a quorum of them.
        let mut rsc_two =
            RecursiveSocketClosure::declare(&pcc, root_socket(), ClosureLaw::Quorum(2))?;
        rsc_two.admit(&leaf_socket(0))?; // checkout-v2
        rsc_two.admit(&leaf_socket(1))?; // admin-portal
        assert!(
            rsc_two.is_closed()?,
            "quorum(2) over 3 features must close once 2 are admitted, even with the third \
             left entirely Open"
        );
        rsc_two.close()?;

        Ok::<(), Refusal>(())
    }
);

test!(
    any_sufficient_closure_admits_integration_readiness_on_the_first_feature_alone,
    {
        // A weaker redundancy law than Quorum(2): AnySufficient closes as
        // soon as a single feature is admitted -- the maximal
        // alternate-load-path reading (any one of the three is enough).
        let plan = temporal_plan_for_fixture()?;
        let model = project_temporal_plan_to_powl(&plan)?;
        let children = match &model {
            Powl::PartialOrder { children, .. } => children,
            other => panic!("expected Powl::PartialOrder, got {other:?}"),
        };
        let (_, checkout_label) = find_leaf(children, "checkout-v2");
        let (_, admin_label) = find_leaf(children, "admin-portal");
        let (_, reporting_label) = find_leaf(children, "reporting-dashboard");

        let integration_model = Powl::PartialOrder {
            children: vec![
                Powl::Leaf(Some(checkout_label.to_string())),
                Powl::Leaf(Some(admin_label.to_string())),
                Powl::Leaf(Some(reporting_label.to_string())),
            ],
            order: BTreeSet::new(),
        };
        let pcc = ParentChildClosure::from_model(&integration_model);
        let mut rsc =
            RecursiveSocketClosure::declare(&pcc, root_socket(), ClosureLaw::AnySufficient)?;
        assert!(
            !rsc.is_closed()?,
            "no feature admitted yet -- must stay open"
        );
        rsc.admit(&leaf_socket(2))?; // reporting-dashboard alone
        assert!(
            rsc.is_closed()?,
            "any_sufficient closes as soon as a single feature is admitted, regardless of \
             which one"
        );
        rsc.close()?;

        Ok::<(), Refusal>(())
    }
);
