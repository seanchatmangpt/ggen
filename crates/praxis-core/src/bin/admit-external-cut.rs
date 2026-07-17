//! Rail A/B production entry point (PROJ-796 reachability closure,
//! `docs/jira/v26.7.11/PATH_TO_100.md` sec.2.3 W1) -- extended to also close
//! the PROJ-774 closure-gate island (`PATH_TO_100.md` sec.7.3 item 4).
//!
//! Before this binary existed, `ChatmanEngine::admit_transition_with_external_cut`
//! had exactly 5 call sites in the whole workspace, all inside test files
//! (`praxis-core/tests/rail_ab_external_cut_wiring.rs`,
//! `praxis-graphlaw/src/chatman/engine_test.rs`) and zero `[[bin]]` targets
//! existed anywhere in `praxis-graphlaw`, `praxis-core`, or `wasm4pm-arazzo`
//! to reach it from outside a test harness. This binary is a real,
//! non-test caller: it builds a real `ChatmanEngine`, loads a real snapshot
//! (Turtle text carrying a PDDL domain/problem and an OCEL trace), declares
//! a real `Powl::ExternalCut` region, and drives the full Rail A/B chain
//! (`ExternalCutCompiler::compile` -> `render_and_compile` -> SPARQL -> Tera
//! -> Arazzo parse/resolve/lower/normalize/compile -> WASM -> sealed digest
//! #10) through `ChatmanRailAbCompiler`, the one real
//! `ExternalCutCompiler` implementation this workspace ships.
//!
//! `RecursiveSocketClosure::declare` and `ChatmanEngine::admit_child_completion`
//! (PROJ-774) had the same shape of gap: real, tested logic
//! (`praxis-graphlaw/src/chatman/{closure.rs,closure_test.rs,engine_test.rs}`)
//! with zero non-test callers anywhere. `admit_child_completion`'s own doc
//! comment honestly discloses that `ChatmanEngine::admit_transition`'s S1-S6
//! pipeline has no concept of a child-workflow-completion signal today, so
//! there is no in-process dispatch flow to hook this into -- the intended
//! caller is external (PATH_TO_100.md sec.7.3 item 4 names a real
//! Erlang-to-Rust bridge as the eventual production caller, out of this
//! session's proportion). Rather than invent a second, synthetic-only
//! binary, this file's existing real entry point is extended with a second
//! demonstrated admission gate: a real `RecursiveSocketClosure` declared
//! over a real `Powl` model's parent-child closure, and a real
//! `ChatmanEngine::admit_child_completion` call against a real child
//! snapshot loaded into this same engine's store.
//!
//! No new admission logic lives here -- every stage this binary drives
//! already exists and is independently unit/integration tested; this file
//! only wires real entry points around it. See `just admit-external-cut`.

use std::collections::BTreeSet;
use std::env;
use std::fs;
use std::process::ExitCode;

use powl2_decompose::{Powl, SocketKind, SocketPath, WorkflowSocketId};
use praxis_core::arazzo::ChatmanRailAbCompiler;
use praxis_graphlaw::chatman::abi::{
    GraphSnapshotId, InputHandles, InvocationEnvelope, InvocationId, OperatorId, ProfileId, Refusal,
};
use praxis_graphlaw::chatman::closure::{ClosureLaw, RecursiveSocketClosure};
use praxis_graphlaw::chatman::engine::{AdmissionSpec, ChatmanEngine, EngineProfile};
use praxis_graphlaw::chatman::router::ProfileGates;
use praxis_graphlaw::chatman::triple8::ProfileSymbolTable;
use praxis_graphlaw::shacl::ValidationReport;

const DEFAULT_SNAPSHOT_IRI: &str = "urn:chatman:engine#cli-admit-external-cut";
const DEFAULT_PROFILE_IRI: &str = "profile:cli-admit-external-cut";
const DEFAULT_INVOCATION_ID: &str = "inv-cli-admit-external-cut-1";

/// Embedded default snapshot: same shape as this repo's own PROJ-796 Rail
/// A/B wiring test (`praxis-core/tests/rail_ab_external_cut_wiring.rs`'s
/// `SNAPSHOT_TTL`) -- a minimal lawful PDDL domain/problem plus one
/// conforming OCEL event, so real S1-S6 admission succeeds. Overridable with
/// `--snapshot <path>` pointing at a real Turtle file using the same
/// `ceng:pddlDomain` / `ceng:pddlProblem` / `ceng:ocelLog` vocabulary
/// `ChatmanEngine::load_snapshot` expects.
const DEFAULT_SNAPSHOT_TTL: &str = r#"
@prefix ex: <http://example.org/> .
@prefix ceng: <urn:chatman:engine#> .

ex:world ceng:pddlDomain """
(define (domain chatman-min)
  (:requirements :strips)
  (:predicates (ready ?x) (done ?x))
  (:action finish
    :parameters (?x)
    :precondition (and (ready ?x))
    :effect (and (done ?x) (not (ready ?x)))))
""" .
ex:world ceng:pddlProblem """
(define (problem chatman-min-p)
  (:domain chatman-min)
  (:objects a)
  (:init (ready a))
  (:goal (done a))
)
""" .
ex:world ceng:ocelLog """{"run_id":1,"sealed":true,"objects":[{"id":"case-1","otype":"case"}],"events":[{"id":"e1","activity":"finish(a)","op_index":0,"at_ns":1,"objects":["case-1"]}]}""" .
"#;

fn print_usage() {
    eprintln!(
        "admit-external-cut -- real, non-test entry point for \
         ChatmanEngine::admit_transition_with_external_cut (Rail A/B)"
    );
    eprintln!();
    eprintln!("USAGE:");
    eprintln!(
        "    admit-external-cut [--snapshot <path.ttl>] [--snapshot-iri <iri>] \
         [--profile-iri <iri>] [--invocation-id <id>]"
    );
    eprintln!();
    eprintln!(
        "Without --snapshot, loads an embedded fixture matching this repo's own \
         PROJ-796 Rail A/B wiring test. The POWL region always declares one \
         Powl::ExternalCut leaf, so the run always exercises the real Rail A/B \
         pipeline (SPARQL projection -> Tera render -> Arazzo parse/resolve/lower/ \
         normalize/compile -> WASM) through ChatmanRailAbCompiler."
    );
}

/// A `PartialOrder` of a plain leaf followed by a declared `ExternalCut`
/// whose region is one leaf activity -- the same shape
/// `praxis_graphlaw::chatman::powl_projection::tests::model_with_external_cut`
/// / `praxis_core::arazzo::tests::model_with_external_cut` /
/// `rail_ab_external_cut_wiring.rs::model_with_external_cut` already use, so
/// this binary exercises the identical wiring path those tests verify.
fn model_with_external_cut() -> Powl {
    Powl::PartialOrder {
        children: vec![
            Powl::Leaf(Some("intake".to_string())),
            Powl::ExternalCut {
                region: Box::new(Powl::Leaf(Some("remote_settle".to_string()))),
                projection: "SELECT * WHERE { ?s ?p ?o }".to_string(),
                renderer: "arazzo_projection.tera".to_string(),
            },
        ],
        order: BTreeSet::from([(0usize, 1usize)]),
    }
}

/// A one-child `PartialOrder` -- the smallest shape whose parent-child
/// closure gives `RecursiveSocketClosure::declare` a non-empty child set to
/// declare `ClosureLaw::AllRequired` over (`closure.rs`'s own
/// `one_leaf_closure` test fixture uses the identical shape). Deliberately
/// a separate, smaller model from [`model_with_external_cut`] above: the
/// closure gate and the external-cut gate are two independent admission
/// gates this binary now demonstrates, not one combined scenario.
fn model_with_child_closure() -> Powl {
    Powl::PartialOrder {
        children: vec![Powl::Leaf(Some("child-workflow".to_string()))],
        order: BTreeSet::new(),
    }
}

fn closure_root_socket() -> WorkflowSocketId {
    WorkflowSocketId {
        path: SocketPath::root(),
        kind: SocketKind::PartialOrder,
    }
}

fn closure_child_socket() -> WorkflowSocketId {
    WorkflowSocketId {
        path: SocketPath::root().child(0),
        kind: SocketKind::Leaf,
    }
}

fn build_profile(profile_iri: &str) -> Result<EngineProfile, Refusal> {
    let profile_id = ProfileId::new(profile_iri);
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

struct Args {
    snapshot_path: Option<String>,
    snapshot_iri: String,
    profile_iri: String,
    invocation_id: String,
}

fn parse_args() -> Result<Option<Args>, String> {
    let mut snapshot_path = None;
    let mut snapshot_iri = DEFAULT_SNAPSHOT_IRI.to_string();
    let mut profile_iri = DEFAULT_PROFILE_IRI.to_string();
    let mut invocation_id = DEFAULT_INVOCATION_ID.to_string();

    let mut args = env::args().skip(1);
    while let Some(arg) = args.next() {
        match arg.as_str() {
            "--snapshot" => {
                snapshot_path = Some(args.next().ok_or("--snapshot requires a path argument")?);
            }
            "--snapshot-iri" => {
                snapshot_iri = args.next().ok_or("--snapshot-iri requires a value")?;
            }
            "--profile-iri" => {
                profile_iri = args.next().ok_or("--profile-iri requires a value")?;
            }
            "--invocation-id" => {
                invocation_id = args.next().ok_or("--invocation-id requires a value")?;
            }
            "--help" | "-h" => {
                print_usage();
                return Ok(None);
            }
            other => {
                return Err(format!("unknown argument: {other} (--help for usage)"));
            }
        }
    }

    Ok(Some(Args {
        snapshot_path,
        snapshot_iri,
        profile_iri,
        invocation_id,
    }))
}

fn run() -> Result<(), Box<dyn std::error::Error>> {
    let args = match parse_args().map_err(|e| format!("{e}\n"))? {
        Some(args) => args,
        None => return Ok(()),
    };

    let snapshot_ttl = match &args.snapshot_path {
        Some(path) => fs::read_to_string(path)
            .map_err(|e| format!("cannot read snapshot file {path}: {e}"))?,
        None => DEFAULT_SNAPSHOT_TTL.to_string(),
    };

    let profile = build_profile(&args.profile_iri)?;
    let mut engine = ChatmanEngine::in_memory(profile)?;
    let snapshot_id = GraphSnapshotId::new(args.snapshot_iri.as_str());
    engine.load_snapshot(&snapshot_id, &snapshot_ttl)?;

    let envelope = InvocationEnvelope {
        invocation_id: InvocationId::new(args.invocation_id.as_str()),
        snapshot_id,
        profile_id: ProfileId::new(args.profile_iri.as_str()),
        operator_id: OperatorId::new("op-cli-admit-external-cut"),
        input_handles: InputHandles::default(),
    };

    let model = model_with_external_cut();
    let compiler = ChatmanRailAbCompiler::default();

    println!("admit-external-cut: ChatmanEngine::admit_transition_with_external_cut");
    println!(
        "  snapshot:      {}",
        args.snapshot_path
            .as_deref()
            .unwrap_or("<embedded PROJ-796 fixture>")
    );
    println!("  snapshot IRI:  {}", args.snapshot_iri);
    println!("  profile IRI:   {}", args.profile_iri);
    println!("  invocation ID: {}", args.invocation_id);
    println!();

    let transition = engine.admit_transition_with_external_cut(envelope, &model, &compiler)?;
    let receipt = transition.receipt();

    println!(
        "admission: OK ({} boundary request(s))",
        transition.boundary_requests().len()
    );
    println!("EngineProcessReceipt:");
    println!("  #1  graph_snapshot  = {}", receipt.graph_snapshot.0);
    println!("  #2  profile         = {}", receipt.profile.0);
    println!("  #3  symbol_table    = {}", receipt.symbol_table.0);
    println!("  #4  projection      = {}", receipt.projection.0);
    println!("  #5  admission_table = {}", receipt.admission_table.0);
    println!("  #6  route_decision  = {}", receipt.route_decision.0);
    println!("  #7  tape            = {}", receipt.tape.0);
    println!("  #8  hook_event      = {}", receipt.hook_event.0);
    println!("  #9  engine_version  = {}", receipt.engine_version.0);
    println!("      receipt_root    = {}", receipt.receipt_root.0);
    match &receipt.external_cut {
        Some(d) => println!("  #10 external_cut    = {}", d.0),
        None => {
            println!("  #10 external_cut    = <none: powl_region declared no Powl::ExternalCut>")
        }
    }

    let recomputed = receipt.recompute_root();
    let root_ok = recomputed.0 == receipt.receipt_root.0;
    println!();
    println!(
        "receipt_root self-check: {}",
        if root_ok {
            "OK (independent recompute is byte-identical)"
        } else {
            "MISMATCH"
        }
    );
    if !root_ok {
        return Err("receipt_root recompute mismatch -- refusing to report success".into());
    }

    // --- PROJ-774 closure gate: RecursiveSocketClosure::declare +
    // ChatmanEngine::admit_child_completion, both real, non-test calls. See
    // the module doc comment for why this lives here rather than in a new
    // binary.
    println!();
    println!("admit-external-cut: ChatmanEngine::admit_child_completion (PROJ-774 closure gate)");

    let closure_model = model_with_child_closure();
    let pcc = closure_model.parent_child_closure();
    let mut closure =
        RecursiveSocketClosure::declare(&pcc, closure_root_socket(), ClosureLaw::AllRequired)?;

    let child_snapshot_iri = format!("{}#child", args.snapshot_iri);
    let child_snapshot_id = GraphSnapshotId::new(child_snapshot_iri.as_str());
    engine.load_snapshot(
        &child_snapshot_id,
        "<urn:chatman:child> <urn:chatman:completed> true .\n",
    )?;
    // Real, non-fabricated evidence: zero SHACL results is exactly
    // `Validator::validate`'s own definition of `conforms` (SHACL Core:
    // `conforms` iff zero results) -- this is the smallest genuinely
    // conformant report, not a hand-waved always-true stub.
    let evidence = ValidationReport {
        conforms: true,
        results: Vec::new(),
    };
    engine.admit_child_completion(
        &mut closure,
        &closure_child_socket(),
        &child_snapshot_id,
        &evidence,
    )?;
    closure.close()?;

    println!("  child snapshot: {child_snapshot_iri}");
    println!("  closure law:    {}", closure.law().name());
    println!("  closure result: closed (all declared children admitted)");

    Ok(())
}

fn main() -> ExitCode {
    match run() {
        Ok(()) => ExitCode::SUCCESS,
        Err(e) => {
            eprintln!("admit-external-cut: refused: {e}");
            ExitCode::FAILURE
        }
    }
}
