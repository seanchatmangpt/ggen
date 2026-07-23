use std::num::NonZeroU64;
use tcps_lifecycle::production::{
    AuthorityKey, Digest, Measurement, ProductionBroker, RateVector, Refusal, Regime, SafeRegion,
    StateVector, Unit,
};

fn measurement(value: u64) -> Measurement {
    Measurement::new(value, Unit::Builds, NonZeroU64::new(60).expect("non-zero"))
}

fn state(value: u64) -> StateVector {
    StateVector {
        demand: measurement(value),
        error_basis_points: 0,
        latency_micros: 100,
        receipt_gap: 0,
    }
}

fn region() -> SafeRegion {
    SafeRegion {
        max_error_basis_points: 100,
        max_latency_micros: 1_000_000,
        max_receipt_gap: 0,
        max_error_acceleration_milli: 1_000,
        max_latency_acceleration_milli: 1_000_000,
    }
}

fn broker() -> (AuthorityKey, ProductionBroker) {
    let authority = AuthorityKey::derive("praxis", b"production-secret-v1");
    let broker = ProductionBroker::new("enterprise-deployment-broker", &authority, region())
        .expect("broker");
    (authority, broker)
}

#[test]
fn algebra_geometry_and_calculus_admit_exact_phase_shift() {
    let (authority, mut broker) = broker();
    let baseline = state(1);
    let observed = state(1_000);
    let rate = RateVector::between(baseline, observed, NonZeroU64::new(60).unwrap())
        .expect("rate");
    let plan = broker
        .plan(
            "cycle-1",
            baseline,
            observed,
            rate,
            Digest::from_bytes(b"source"),
            Digest::from_bytes(b"knowledge"),
        )
        .expect("plan");
    assert_eq!(plan.regime(), Regime::PhaseShift1000x);
    assert_eq!(plan.actions().len(), 5);
    assert!(plan.target_topology().pull_enabled);
    assert!(plan.target_topology().rollback_armed);

    let capability = authority
        .authorize(
            &plan,
            "tcps-auto-select",
            "enterprise-deployment-broker",
            7,
            10,
            11,
        )
        .expect("capability");
    let receipt = broker.execute(plan, capability, 10).expect("execute");
    assert_eq!(receipt.previous_receipt, Digest::zero());
    assert_eq!(broker.last_receipt(), receipt.receipt_digest);
    assert!(broker.topology().proof_receipts_required);
    assert_eq!(broker.topology().canary_percent, 5);
}

#[test]
fn replayed_capability_is_refused() {
    let (authority, mut broker) = broker();
    let baseline = state(1);
    let observed = state(1_000);
    let rate = RateVector::between(baseline, observed, NonZeroU64::new(60).unwrap()).unwrap();
    let plan = broker
        .plan(
            "cycle-replay",
            baseline,
            observed,
            rate,
            Digest::from_bytes(b"source"),
            Digest::from_bytes(b"knowledge"),
        )
        .unwrap();
    let duplicate_plan = plan.clone();
    let first = authority
        .authorize(
            &plan,
            "tcps-auto-select",
            "enterprise-deployment-broker",
            99,
            1,
            2,
        )
        .unwrap();
    broker.execute(plan, first, 1).unwrap();

    let replay = authority
        .authorize(
            &duplicate_plan,
            "tcps-auto-select",
            "enterprise-deployment-broker",
            99,
            1,
            2,
        )
        .unwrap();
    assert_eq!(broker.execute(duplicate_plan, replay, 1), Err(Refusal::Replay));
}

#[test]
fn broken_receipt_ancestry_is_refused() {
    let (authority, mut broker) = broker();
    let baseline = state(1);
    let observed = state(1_000);
    let rate = RateVector::between(baseline, observed, NonZeroU64::new(60).unwrap()).unwrap();
    let stale = broker
        .plan(
            "cycle-stale",
            baseline,
            observed,
            rate,
            Digest::from_bytes(b"source-a"),
            Digest::from_bytes(b"knowledge"),
        )
        .unwrap();
    let current = broker
        .plan(
            "cycle-current",
            baseline,
            observed,
            rate,
            Digest::from_bytes(b"source-b"),
            Digest::from_bytes(b"knowledge"),
        )
        .unwrap();
    let current_cap = authority
        .authorize(
            &current,
            "tcps-auto-select",
            "enterprise-deployment-broker",
            1,
            1,
            2,
        )
        .unwrap();
    broker.execute(current, current_cap, 1).unwrap();

    let stale_cap = authority
        .authorize(
            &stale,
            "tcps-auto-select",
            "enterprise-deployment-broker",
            2,
            1,
            2,
        )
        .unwrap();
    assert_eq!(
        broker.execute(stale, stale_cap, 1),
        Err(Refusal::BrokenReceiptChain)
    );
}

#[test]
fn unit_or_window_mismatch_is_refused() {
    let (_, broker) = broker();
    let baseline = state(1);
    let mut observed = state(1_000);
    observed.demand = Measurement::new(
        1_000,
        Unit::Requests,
        NonZeroU64::new(60).unwrap(),
    );
    let rate = RateVector {
        demand_per_second_milli: 0,
        error_basis_points_per_second_milli: 0,
        latency_micros_per_second_milli: 0,
    };
    assert_eq!(
        broker.plan(
            "cycle-unit",
            baseline,
            observed,
            rate,
            Digest::from_bytes(b"source"),
            Digest::from_bytes(b"knowledge"),
        ),
        Err(Refusal::MeasurementMismatch)
    );
}

#[test]
fn unsafe_geometric_state_is_refused_before_authorization() {
    let (_, broker) = broker();
    let baseline = state(1);
    let mut observed = state(1_000);
    observed.receipt_gap = 1;
    let rate = RateVector::between(baseline, observed, NonZeroU64::new(60).unwrap()).unwrap();
    assert_eq!(
        broker.plan(
            "cycle-unsafe",
            baseline,
            observed,
            rate,
            Digest::from_bytes(b"source"),
            Digest::from_bytes(b"knowledge"),
        ),
        Err(Refusal::UnsafeGeometry)
    );
}

#[test]
fn collapsed_authorities_and_expired_capabilities_are_refused() {
    let authority = AuthorityKey::derive("praxis", b"secret");
    assert_eq!(
        ProductionBroker::new("praxis", &authority, region()),
        Err(Refusal::AuthorityCollapse)
    );

    let mut broker = ProductionBroker::new("enterprise-deployment-broker", &authority, region())
        .unwrap();
    let baseline = state(1);
    let observed = state(1_000);
    let rate = RateVector::between(baseline, observed, NonZeroU64::new(60).unwrap()).unwrap();
    let plan = broker
        .plan(
            "cycle-expired",
            baseline,
            observed,
            rate,
            Digest::from_bytes(b"source"),
            Digest::from_bytes(b"knowledge"),
        )
        .unwrap();
    let capability = authority
        .authorize(
            &plan,
            "tcps-auto-select",
            "enterprise-deployment-broker",
            3,
            1,
            1,
        )
        .unwrap();
    assert_eq!(
        broker.execute(plan, capability, 2),
        Err(Refusal::ExpiredCapability)
    );
}
