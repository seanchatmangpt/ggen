use static_assertions::assert_not_impl_any;
use tcps_lifecycle::{
    Authorization, CargoCicdEvidence, ProductionLine, Receipted, ReferenceCargoCicdExecutor,
    ReferencePraxisGate, Refusal,
};

assert_not_impl_any!(Authorization: Clone, Copy);
assert_not_impl_any!(CargoCicdEvidence: Clone, Copy);
assert_not_impl_any!(ProductionLine<Receipted>: Clone, Copy);

fn planned(
    id: &str,
) -> (
    ReferencePraxisGate,
    ProductionLine<tcps_lifecycle::Planned>,
    ReferenceCargoCicdExecutor,
) {
    let gate = ReferencePraxisGate::new("standard-v1");
    let executor = ReferenceCargoCicdExecutor::green();
    let evidence = executor.verify("source", vec!["tcps-lifecycle".to_owned()]);
    let planned = ProductionLine::observe(id, 1, "standard-v1")
        .admit(&gate)
        .expect("admission")
        .plan(evidence)
        .expect("plan");
    (gate, planned, executor)
}

#[test]
fn downstream_demand_flows_to_framework_receipted_execution() {
    let (gate, planned, mut executor) = planned("change-1");
    let authorization = gate.authorize(&planned, "release-manager");
    let authorized = planned.authorize(authorization).expect("authorize");
    let receipted = authorized.execute(&mut executor).expect("execute");

    assert_eq!(receipted.receipt().executor(), "cargo-cicd");
    assert_eq!(receipted.receipt().receipt_digest().len(), 64);
    assert_eq!(receipted.receipt().artifact_digest().len(), 64);
}

#[test]
fn zero_demand_is_refused_before_planning() {
    let gate = ReferencePraxisGate::new("standard-v1");
    let refusal = ProductionLine::observe("change-2", 0, "standard-v1")
        .admit(&gate)
        .expect_err("zero demand must refuse");
    assert_eq!(refusal, Refusal::ZeroDemand);
}

#[test]
fn red_evidence_is_manufactured_by_workcell_and_refused() {
    let gate = ReferencePraxisGate::new("standard-v1");
    let executor = ReferenceCargoCicdExecutor::verification_failing();
    let evidence = executor.verify("source-3", vec!["tcps-lifecycle".to_owned()]);
    let admitted = ProductionLine::observe("change-3", 1, "standard-v1")
        .admit(&gate)
        .expect("admission");
    let refusal = admitted.plan(evidence).expect_err("red evidence must refuse");
    assert_eq!(refusal, Refusal::EvidenceNotGreen);
}

#[test]
fn authorization_is_opaque_and_bound_to_one_plan() {
    let gate = ReferencePraxisGate::new("standard-v1");
    let executor = ReferenceCargoCicdExecutor::green();
    let first = ProductionLine::observe("change-4a", 1, "standard-v1")
        .admit(&gate)
        .expect("admission")
        .plan(executor.verify("source-4a", vec!["a".to_owned()]))
        .expect("plan");
    let second = ProductionLine::observe("change-4b", 1, "standard-v1")
        .admit(&gate)
        .expect("admission")
        .plan(executor.verify("source-4b", vec!["b".to_owned()]))
        .expect("plan");

    let wrong = gate.authorize(&first, "release-manager");
    let refusal = second.authorize(wrong).expect_err("wrong plan must refuse");
    assert_eq!(refusal, Refusal::AuthorizationMismatch);
}

#[test]
fn abnormality_stops_and_unchanged_standard_cannot_restart() {
    let (gate, planned, _) = planned("change-5");
    let authorization = gate.authorize(&planned, "release-manager");
    let authorized = planned.authorize(authorization).expect("authorize");
    let mut executor = ReferenceCargoCicdExecutor::failing();
    let stopped = authorized.execute(&mut executor).expect_err("must stop");

    assert!(stopped.andon().reason().contains("abnormality"));
    let unchanged = stopped
        .recover("standard-v1")
        .expect_err("same standard must not restart");
    assert_eq!(unchanged, Refusal::StandardUnchanged);
}

#[test]
fn updated_standard_reenters_observation_and_requires_readmission() {
    let (gate, planned, _) = planned("change-6");
    let authorization = gate.authorize(&planned, "release-manager");
    let authorized = planned.authorize(authorization).expect("authorize");
    let mut executor = ReferenceCargoCicdExecutor::failing();
    let stopped = authorized.execute(&mut executor).expect_err("must stop");
    let recovered = stopped.recover("standard-v2").expect("updated standard");
    let new_gate = ReferencePraxisGate::new("standard-v2");
    recovered.admit(&new_gate).expect("updated standard admits");
}

#[test]
fn typestate_markers_add_no_runtime_storage() {
    use std::mem::size_of;
    use tcps_lifecycle::{Observed, Planned};

    assert_eq!(
        size_of::<ProductionLine<Observed>>(),
        size_of::<ProductionLine<Planned>>()
    );
}
