use std::{fs, num::NonZeroU64, path::Path};
use tcps_production::{
    AuthorityKey, BrokerKey, Digest, Measurement, ProductionBroker, RateVector, Refusal, Regime,
    SafeRegion, StateVector, Unit,
};
use tempfile::tempdir;

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

fn system() -> (AuthorityKey, BrokerKey, ProductionBroker) {
    let authority = AuthorityKey::derive("praxis", b"production-authority-secret-v1")
        .expect("authority");
    let broker_key = BrokerKey::derive(
        "enterprise-deployment-broker",
        b"production-broker-secret-v1",
    )
    .expect("broker key");
    let broker = ProductionBroker::new(
        &broker_key,
        authority.id(),
        authority.public_key(),
        region(),
    )
    .expect("broker");
    (authority, broker_key, broker)
}

fn phase_plan(
    broker: &ProductionBroker,
    cycle: &str,
    target: &Path,
) -> tcps_production::Plan {
    let baseline = state(1);
    let observed = state(1_000);
    let rate = RateVector::between(baseline, observed, NonZeroU64::new(60).unwrap())
        .expect("rate");
    broker
        .plan(
            cycle,
            target.to_string_lossy(),
            baseline,
            observed,
            rate,
            Digest::from_bytes(b"source"),
            Digest::from_bytes(b"knowledge"),
        )
        .expect("plan")
}

#[test]
fn algebra_geometry_calculus_and_effect_form_one_receipted_actuation() {
    let (authority, broker_key, mut broker) = system();
    let directory = tempdir().unwrap();
    let desired_state = directory.path().join("production-state.txt");
    let plan = phase_plan(&broker, "cycle-1", &desired_state);
    assert_eq!(plan.regime(), Regime::PhaseShift1000x);
    assert_eq!(plan.actions().len(), 5);
    assert_eq!(plan.target_uri(), desired_state.to_string_lossy());
    assert!(plan.target_topology().pull_enabled);
    assert!(plan.target_topology().rollback_armed);

    let capability = authority
        .authorize(
            &plan,
            "tcps-auto-select",
            broker_key.id(),
            7,
            10,
            11,
        )
        .expect("capability");
    let receipt = broker
        .execute_to_path(plan, capability, 10, &desired_state)
        .expect("execute");

    assert!(desired_state.is_file());
    let state_text = fs::read_to_string(&desired_state).unwrap();
    assert!(state_text.contains("pull_enabled=true"));
    assert!(state_text.contains("rollback_armed=true"));
    assert_eq!(receipt.previous_receipt, Digest::zero());
    assert_eq!(broker.last_receipt(), receipt.receipt_digest);
    assert!(broker.topology().proof_receipts_required);
    receipt.verify(broker_key.public_key()).expect("broker signature");
}

#[test]
fn receipt_field_tampering_is_detected_before_signature_acceptance() {
    let (authority, broker_key, mut broker) = system();
    let directory = tempdir().unwrap();
    let target = directory.path().join("state");
    let plan = phase_plan(&broker, "cycle-tamper", &target);
    let capability = authority
        .authorize(&plan, "tcps-auto-select", broker_key.id(), 8, 1, 2)
        .unwrap();
    let mut receipt = broker
        .execute_to_path(plan, capability, 1, &target)
        .unwrap();
    receipt.target_uri.push_str("-redirected");
    assert!(matches!(
        receipt.verify(broker_key.public_key()),
        Err(Refusal::InvalidReceiptDigest)
    ));
}

#[test]
fn replayed_capability_is_refused() {
    let (authority, broker_key, mut broker) = system();
    let directory = tempdir().unwrap();
    let target = directory.path().join("state-replay");
    let first_plan = phase_plan(&broker, "cycle-replay", &target);
    let replay_plan = phase_plan(&broker, "cycle-replay", &target);
    assert_eq!(first_plan.digest(), replay_plan.digest());
    let first = authority
        .authorize(
            &first_plan,
            "tcps-auto-select",
            broker_key.id(),
            99,
            1,
            2,
        )
        .unwrap();
    broker
        .execute_to_path(first_plan, first, 1, &target)
        .unwrap();

    let replay = authority
        .authorize(
            &replay_plan,
            "tcps-auto-select",
            broker_key.id(),
            99,
            1,
            2,
        )
        .unwrap();
    assert!(matches!(
        broker.execute_to_path(replay_plan, replay, 1, &target),
        Err(Refusal::Replay)
    ));
}

#[test]
fn broken_receipt_ancestry_is_refused() {
    let (authority, broker_key, mut broker) = system();
    let directory = tempdir().unwrap();
    let stale_target = directory.path().join("state-stale");
    let current_target = directory.path().join("state-current");
    let stale = phase_plan(&broker, "cycle-stale", &stale_target);
    let current = phase_plan(&broker, "cycle-current", &current_target);
    let current_cap = authority
        .authorize(
            &current,
            "tcps-auto-select",
            broker_key.id(),
            1,
            1,
            2,
        )
        .unwrap();
    broker
        .execute_to_path(current, current_cap, 1, &current_target)
        .unwrap();

    let stale_cap = authority
        .authorize(
            &stale,
            "tcps-auto-select",
            broker_key.id(),
            2,
            1,
            2,
        )
        .unwrap();
    assert!(matches!(
        broker.execute_to_path(stale, stale_cap, 1, &stale_target),
        Err(Refusal::BrokenReceiptChain)
    ));
}

#[test]
fn authorized_plan_cannot_be_redirected_to_another_target() {
    let (authority, broker_key, mut broker) = system();
    let directory = tempdir().unwrap();
    let authorized_target = directory.path().join("authorized");
    let redirected_target = directory.path().join("redirected");
    let plan = phase_plan(&broker, "cycle-target", &authorized_target);
    let capability = authority
        .authorize(&plan, "tcps-auto-select", broker_key.id(), 4, 1, 2)
        .unwrap();
    assert!(matches!(
        broker.execute_to_path(plan, capability, 1, &redirected_target),
        Err(Refusal::WrongTarget)
    ));
    assert!(!redirected_target.exists());
}

#[test]
fn unit_or_window_mismatch_is_refused() {
    let (_, _, broker) = system();
    let directory = tempdir().unwrap();
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
    assert!(matches!(
        broker.plan(
            "cycle-unit",
            directory.path().join("state").to_string_lossy(),
            baseline,
            observed,
            rate,
            Digest::from_bytes(b"source"),
            Digest::from_bytes(b"knowledge"),
        ),
        Err(Refusal::MeasurementMismatch)
    ));
}

#[test]
fn unsafe_geometric_state_is_refused_before_authorization() {
    let (_, _, broker) = system();
    let directory = tempdir().unwrap();
    let baseline = state(1);
    let mut observed = state(1_000);
    observed.receipt_gap = 1;
    let rate = RateVector::between(baseline, observed, NonZeroU64::new(60).unwrap()).unwrap();
    assert!(matches!(
        broker.plan(
            "cycle-unsafe",
            directory.path().join("state").to_string_lossy(),
            baseline,
            observed,
            rate,
            Digest::from_bytes(b"source"),
            Digest::from_bytes(b"knowledge"),
        ),
        Err(Refusal::UnsafeGeometry)
    ));
}

#[test]
fn collapsed_authorities_empty_secrets_and_expired_capabilities_refuse() {
    assert!(matches!(
        AuthorityKey::derive("praxis", b""),
        Err(Refusal::EmptySecret)
    ));
    let authority = AuthorityKey::derive("praxis", b"secret").unwrap();
    let collapsed = BrokerKey::derive("praxis", b"broker-secret").unwrap();
    assert!(matches!(
        ProductionBroker::new(
            &collapsed,
            authority.id(),
            authority.public_key(),
            region()
        ),
        Err(Refusal::AuthorityCollapse)
    ));

    let broker_key = BrokerKey::derive("enterprise-deployment-broker", b"broker-secret").unwrap();
    let mut broker = ProductionBroker::new(
        &broker_key,
        authority.id(),
        authority.public_key(),
        region(),
    )
    .unwrap();
    let directory = tempdir().unwrap();
    let target = directory.path().join("state");
    let plan = phase_plan(&broker, "cycle-expired", &target);
    let capability = authority
        .authorize(
            &plan,
            "tcps-auto-select",
            broker_key.id(),
            3,
            1,
            1,
        )
        .unwrap();
    assert!(matches!(
        broker.execute_to_path(plan, capability, 2, &target),
        Err(Refusal::ExpiredCapability)
    ));
}

#[test]
fn broker_cannot_verify_authority_with_the_wrong_public_key() {
    let authority = AuthorityKey::derive("praxis", b"authority-a").unwrap();
    let wrong_authority = AuthorityKey::derive("praxis", b"authority-b").unwrap();
    let broker_key = BrokerKey::derive("enterprise-deployment-broker", b"broker").unwrap();
    let mut broker = ProductionBroker::new(
        &broker_key,
        authority.id(),
        wrong_authority.public_key(),
        region(),
    )
    .unwrap();
    let directory = tempdir().unwrap();
    let target = directory.path().join("state");
    let plan = phase_plan(&broker, "cycle-wrong-key", &target);
    let capability = authority
        .authorize(
            &plan,
            "tcps-auto-select",
            broker_key.id(),
            5,
            1,
            2,
        )
        .unwrap();
    assert!(matches!(
        broker.execute_to_path(plan, capability, 1, &target),
        Err(Refusal::InvalidCapability)
    ));
}
