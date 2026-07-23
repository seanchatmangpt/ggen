use static_assertions::assert_not_impl_any;
use tcps_lifecycle::autonomics::{
    AutonomicAuthorization, AutonomicEvidence, AutonomicLoop, AutonomicPlan, AutonomicPolicy,
    AutonomicRefusal, AutonomicAction, PhaseRegime, Receipted, ReferenceAutonomicCargoCicd,
    ReferenceAutonomicPraxis, PHASE_SHIFT_MULTIPLIER,
};

assert_not_impl_any!(AutonomicEvidence: Clone, Copy);
assert_not_impl_any!(AutonomicPlan: Clone, Copy);
assert_not_impl_any!(AutonomicAuthorization: Clone, Copy);
assert_not_impl_any!(AutonomicLoop<Receipted>: Clone, Copy);

fn policy() -> AutonomicPolicy {
    AutonomicPolicy::phase_shift_1000x("knowledge-blake3-v1").expect("policy")
}

#[test]
fn threshold_is_exactly_one_thousand() {
    assert_eq!(PHASE_SHIFT_MULTIPLIER, 1_000);

    let observer = ReferenceAutonomicCargoCicd::green();
    let below = observer
        .observe("cycle-999", 10, 9_999, 0, "source-999")
        .expect("observation")
        .classify(&policy())
        .expect("classification")
        .propose(&policy())
        .expect("proposal");
    assert_eq!(below.plan().regime(), PhaseRegime::Continuous);
    assert_eq!(
        below.plan().actions(),
        &[AutonomicAction::PreserveStandardWork]
    );

    let exact = observer
        .observe("cycle-1000", 10, 10_000, 0, "source-1000")
        .expect("observation")
        .classify(&policy())
        .expect("classification")
        .propose(&policy())
        .expect("proposal");
    assert_eq!(exact.plan().regime(), PhaseRegime::PhaseShift1000x);
}

#[test]
fn phase_shift_recomposes_the_factory_instead_of_tuning_one_knob() {
    let observer = ReferenceAutonomicCargoCicd::green();
    let proposed = observer
        .observe("cycle-recompose", 1, 1_000, 0, "source-recompose")
        .expect("observation")
        .classify(&policy())
        .expect("classification")
        .propose(&policy())
        .expect("proposal");

    assert_eq!(
        proposed.plan().actions(),
        &[
            AutonomicAction::PartitionDemand,
            AutonomicAction::EnablePullShards,
            AutonomicAction::RequireIndependentOracle,
            AutonomicAction::RequireProofCarryingReceipts,
            AutonomicAction::StageCanaryRollback,
        ]
    );
}

#[test]
fn worker_cannot_adjudicate_its_own_phase_shift() {
    let observer = ReferenceAutonomicCargoCicd::green();
    let proposed = observer
        .observe("cycle-self-judge", 1, 1_000, 0, "source-self-judge")
        .expect("observation")
        .classify(&policy())
        .expect("classification")
        .propose(&policy())
        .expect("proposal");
    let self_judge = ReferenceAutonomicPraxis::new("cargo-cicd");

    let refusal = self_judge
        .authorize(&proposed, "same-worker")
        .expect_err("self-adjudication must refuse");
    assert_eq!(refusal, AutonomicRefusal::SelfAdjudication);
}

#[test]
fn successful_phase_shift_is_independently_judged_and_receipted() {
    let mut worker = ReferenceAutonomicCargoCicd::green();
    let proposed = worker
        .observe("cycle-receipt", 2, 2_000, 0, "source-receipt")
        .expect("observation")
        .classify(&policy())
        .expect("classification")
        .propose(&policy())
        .expect("proposal");
    let praxis = ReferenceAutonomicPraxis::new("praxis");
    let authorization = praxis
        .authorize(&proposed, "production-controller")
        .expect("independent authorization");
    let authorized = proposed.authorize(authorization).expect("bound authorization");
    let receipted = authorized.apply(&mut worker).expect("receipted adaptation");

    assert_eq!(receipted.receipt().regime(), PhaseRegime::PhaseShift1000x);
    assert_eq!(receipted.receipt().judge(), "praxis");
    assert_eq!(receipted.receipt().receipt_digest().len(), 64);
}

#[test]
fn unsafe_observation_stops_before_planning() {
    let observer = ReferenceAutonomicCargoCicd::green();
    let refusal = observer
        .observe("cycle-unsafe", 1, 1_000, 101, "source-unsafe")
        .expect("well-formed observation")
        .classify(&policy())
        .expect_err("unsafe evidence must not produce a plan");
    assert_eq!(refusal, AutonomicRefusal::UnsafeObservation);
}

#[test]
fn zero_baseline_is_refused_without_division_or_fake_infinity() {
    let observer = ReferenceAutonomicCargoCicd::green();
    let refusal = observer
        .observe("cycle-zero", 0, u64::MAX, 0, "source-zero")
        .expect_err("zero baseline must refuse");
    assert_eq!(refusal, AutonomicRefusal::ZeroBaseline);
}

#[test]
fn failed_adaptation_raises_and_on_without_receipt() {
    let observer = ReferenceAutonomicCargoCicd::green();
    let proposed = observer
        .observe("cycle-fail", 1, 1_000, 0, "source-fail")
        .expect("observation")
        .classify(&policy())
        .expect("classification")
        .propose(&policy())
        .expect("proposal");
    let praxis = ReferenceAutonomicPraxis::new("praxis");
    let authorization = praxis.authorize(&proposed, "controller").expect("authorization");
    let authorized = proposed.authorize(authorization).expect("bound authorization");
    let mut failing = ReferenceAutonomicCargoCicd::failing();
    let stopped = authorized.apply(&mut failing).expect_err("must stop");

    assert!(stopped.andon().reason().contains("abnormality"));
}
