// Chicago TDD: this file asserts on real, observed state via `match ... =>
// panic!(...)` (fail loudly on an unexpected variant) and `.expect(...)` (fail
// loudly on an unexpected `None`) — the same pattern already established as
// file-level allows across this crate's other integration tests (e.g.
// `tests/adversarial_dfg_test.rs`, `tests/hash_stability.rs`). Scoped to only
// the two lints this file actually triggers, not a blanket copy of the wider
// per-file allow lists used elsewhere.
#![allow(clippy::panic, clippy::expect_used)]

use ggen_graph::rwr::{
    read_committed_payload, Action, AndonLevel, AutomaticReplayVerifier, AutomaticRuntime,
    AutonomicOperationsController, AutonomicResource, CircuitBreaker, ConsequenceObserver,
    Dimension, ErrorBudget, ExecutionError, ExecutionPolicy, FilesystemActuator,
    FilesystemConsequenceObserver, FoundationMachine, ObservationError, OperationsError,
    OperationsGovernor, RetryPolicy, Route, Trigger, TriggerAdmissionPolicy, TriggerRouter,
    ALL_OPERATIONS_CAPABILITIES, REQUIRED_OPERATIONS_PROOF_SURFACES,
};

fn machine() -> FoundationMachine {
    FoundationMachine::new(ExecutionPolicy::full_level5(1024 * 1024))
}

fn router() -> Result<TriggerRouter, OperationsError> {
    let mut router = TriggerRouter::new();
    router.insert(Route::new(
        "sync",
        Dimension::AutomationClosure,
        "automatic-sync",
    )?)?;
    router.insert(Route::new(
        "repair",
        Dimension::AutonomicClosure,
        "autonomic-repair",
    )?)?;
    Ok(router)
}

fn runtime(
    budget: u64, threshold: u32, retry_attempts: u8,
) -> Result<AutomaticRuntime, OperationsError> {
    Ok(AutomaticRuntime::new(
        TriggerAdmissionPolicy::new(["sync", "repair"], 1024, 10_000),
        router()?,
        OperationsGovernor::new(
            RetryPolicy::new(retry_attempts, 1),
            CircuitBreaker::new(threshold),
            ErrorBudget::new(budget, 1),
        ),
    ))
}

#[derive(Debug)]
struct FlakyObserver {
    failures_remaining: u8,
}

impl ConsequenceObserver for FlakyObserver {
    fn observe(
        &mut self, actuator: &FilesystemActuator, receipt: &ggen_graph::rwr::ActuationReceipt,
    ) -> Result<Vec<u8>, ObservationError> {
        if self.failures_remaining > 0 {
            self.failures_remaining -= 1;
            return Err(ObservationError::Transient("not visible yet".to_string()));
        }
        let mut observer = FilesystemConsequenceObserver;
        observer.observe(actuator, receipt)
    }
}

#[derive(Debug, Default)]
struct MismatchObserver;

impl ConsequenceObserver for MismatchObserver {
    fn observe(
        &mut self, _actuator: &FilesystemActuator, _receipt: &ggen_graph::rwr::ActuationReceipt,
    ) -> Result<Vec<u8>, ObservationError> {
        Ok(b"unexpected-state".to_vec())
    }
}

#[test]
fn crown_inventory_is_15_capabilities_times_3_surfaces() {
    assert_eq!(ALL_OPERATIONS_CAPABILITIES.len(), 15);
    assert_eq!(REQUIRED_OPERATIONS_PROOF_SURFACES.len(), 3);
    assert_eq!(
        ALL_OPERATIONS_CAPABILITIES.len() * REQUIRED_OPERATIONS_PROOF_SURFACES.len(),
        45
    );
}

#[test]
fn trigger_to_consequence_is_fully_automatic_and_causally_receipted(
) -> Result<(), Box<dyn std::error::Error>> {
    let temp = tempfile::tempdir()?;
    let actuator = FilesystemActuator::new(temp.path().join("actuation"));
    let machine = machine();
    let mut runtime = runtime(8, 3, 3)?;
    let mut observer = FilesystemConsequenceObserver;

    let first = runtime.execute(
        Trigger::new("event-1", "sync", 1, b"state=one".to_vec()),
        &machine,
        &actuator,
        &mut observer,
    )?;
    first.verify()?;
    assert_eq!(
        read_committed_payload(&actuator, &first.actuation_receipt.action_id)?,
        b"state=one"
    );
    assert!(first.predecessor_receipt_digest.is_none());
    assert_eq!(
        first.knowledge_hook.cause_receipt_digest,
        first.actuation_receipt.receipt_digest
    );

    let second = runtime.execute(
        Trigger::new("event-2", "sync", 2, b"state=two".to_vec()),
        &machine,
        &actuator,
        &mut observer,
    )?;
    second.verify()?;
    assert_eq!(
        second.predecessor_receipt_digest,
        Some(first.receipt_digest)
    );
    assert_eq!(runtime.governor().error_budget.remaining(), 6);
    Ok(())
}

#[test]
fn admission_routing_intent_and_grant_fail_closed() -> Result<(), Box<dyn std::error::Error>> {
    let policy = TriggerAdmissionPolicy::new(["sync"], 4, 10);

    let mut changed = Trigger::new("event", "sync", 1, b"ok".to_vec());
    changed.payload.push(b'!');
    assert!(matches!(
        policy.admit(changed),
        Err(OperationsError::TriggerPayloadDigestMismatch(_))
    ));
    assert!(matches!(
        policy.admit(Trigger::new("event", "unknown", 1, Vec::new())),
        Err(OperationsError::TriggerKindRefused(_))
    ));
    assert!(matches!(
        policy.admit(Trigger::new("event", "sync", 0, Vec::new())),
        Err(OperationsError::InvalidTriggerSequence)
    ));

    let mut router = TriggerRouter::new();
    let route = Route::new("sync", Dimension::AutomationClosure, "sync-intent")?;
    router.insert(route.clone())?;
    assert!(matches!(
        router.insert(route),
        Err(OperationsError::DuplicateRoute(_))
    ));

    let machine = machine();
    let mut action = Action::new(
        "exact-grant",
        Dimension::AutomationClosure,
        b"payload".to_vec(),
    );
    let grant = machine.derive_grant(&action)?;
    grant.verify_for(&action)?;
    action.payload.push(b'!');
    assert!(matches!(
        grant.verify_for(&action),
        Err(ExecutionError::PayloadDigestMismatch(_))
    ));
    Ok(())
}

#[test]
fn idempotency_atomicity_and_replay_are_enforced() -> Result<(), Box<dyn std::error::Error>> {
    let temp = tempfile::tempdir()?;
    let actuator = FilesystemActuator::new(temp.path().join("actuation"));
    let machine = machine();
    let mut runtime = runtime(4, 3, 2)?;
    let mut observer = FilesystemConsequenceObserver;
    let trigger = Trigger::new("once", "sync", 1, b"only-once".to_vec());

    let receipt = runtime.execute(trigger.clone(), &machine, &actuator, &mut observer)?;
    assert!(matches!(
        runtime.execute(trigger, &machine, &actuator, &mut observer),
        Err(OperationsError::DuplicateTriggerRefused)
    ));

    let mut replay = AutomaticReplayVerifier::default();
    replay.verify_once(&receipt)?;
    assert!(matches!(
        replay.verify_once(&receipt),
        Err(OperationsError::AutomaticReceiptReplayRefused)
    ));

    let action = Action::new(
        "atomic-action",
        Dimension::AutomationClosure,
        b"atomic".to_vec(),
    );
    let grant = machine.derive_grant(&action)?;
    let committed = actuator.actuate(&grant, &action)?;
    actuator.verify_committed(&committed)?;
    assert!(matches!(
        actuator.actuate(&grant, &action),
        Err(ExecutionError::ReplayRefused(_))
    ));
    Ok(())
}

/// FINDING #10 regression: `verify_once` proves each receipt is internally
/// self-consistent (its own `receipt_digest` covers its own fields, including
/// whatever it carries in `predecessor_receipt_digest`), but never compares
/// that value against another receipt's *actual* `receipt_digest`. A
/// downstream verifier replaying a persisted log must be able to detect a
/// broken causal chain -- not just individually-valid receipts -- which is
/// exactly what `AutomaticReplayVerifier::verify_chain` closes.
#[test]
fn replay_verifier_detects_broken_causal_chain_linkage() -> Result<(), Box<dyn std::error::Error>> {
    let temp = tempfile::tempdir()?;
    let actuator = FilesystemActuator::new(temp.path().join("actuation"));
    let machine = machine();
    let mut chain_runtime = runtime(8, 3, 3)?;
    let mut observer = FilesystemConsequenceObserver;

    let first = chain_runtime.execute(
        Trigger::new("chain-event-1", "sync", 1, b"chain=one".to_vec()),
        &machine,
        &actuator,
        &mut observer,
    )?;
    let second = chain_runtime.execute(
        Trigger::new("chain-event-2", "sync", 2, b"chain=two".to_vec()),
        &machine,
        &actuator,
        &mut observer,
    )?;
    assert_eq!(
        second.predecessor_receipt_digest,
        Some(first.receipt_digest)
    );

    // Positive case: the intact, in-order chain verifies cleanly.
    let mut intact = AutomaticReplayVerifier::default();
    intact.verify_chain(&[first.clone(), second.clone()])?;

    // Negative case 1: dropping the first receipt of a two-receipt sequence
    // before replay. `second` alone still passes `verify_once` (it is
    // internally self-consistent), but it is no longer the genesis of the
    // replayed sequence -- its declared predecessor cannot be produced.
    let mut dropped_genesis = AutomaticReplayVerifier::default();
    dropped_genesis.verify_once(&second)?;
    let mut dropped_genesis_chain = AutomaticReplayVerifier::default();
    match dropped_genesis_chain.verify_chain(std::slice::from_ref(&second)) {
        Err(OperationsError::ChainLinkageBroken {
            position,
            expected_predecessor,
            observed_predecessor,
        }) => {
            assert_eq!(position, 0);
            assert_eq!(expected_predecessor, None);
            assert_eq!(observed_predecessor, Some(first.receipt_digest));
        }
        other => panic!("expected ChainLinkageBroken, got {other:?}"),
    }

    // Negative case 2: splicing two independently-issued receipts together.
    // Build a second, wholly unrelated two-receipt chain in its own
    // actuation root, then splice its second receipt after `first` from the
    // original chain. Both receipts still verify individually -- the
    // splice is only detectable by comparing declared predecessor digests
    // against real prior receipt digests.
    let other_temp = tempfile::tempdir()?;
    let other_actuator = FilesystemActuator::new(other_temp.path().join("actuation"));
    let mut other_runtime = runtime(8, 3, 3)?;
    let mut other_observer = FilesystemConsequenceObserver;
    let other_first = other_runtime.execute(
        Trigger::new("other-chain-event-1", "sync", 1, b"other=one".to_vec()),
        &machine,
        &other_actuator,
        &mut other_observer,
    )?;
    let other_second = other_runtime.execute(
        Trigger::new("other-chain-event-2", "sync", 2, b"other=two".to_vec()),
        &machine,
        &other_actuator,
        &mut other_observer,
    )?;
    assert_eq!(
        other_second.predecessor_receipt_digest,
        Some(other_first.receipt_digest)
    );

    let mut spliced = AutomaticReplayVerifier::default();
    match spliced.verify_chain(&[first.clone(), other_second.clone()]) {
        Err(OperationsError::ChainLinkageBroken {
            position,
            expected_predecessor,
            observed_predecessor,
        }) => {
            assert_eq!(position, 1);
            assert_eq!(expected_predecessor, Some(first.receipt_digest));
            assert_eq!(observed_predecessor, Some(other_first.receipt_digest));
        }
        other => panic!("expected ChainLinkageBroken, got {other:?}"),
    }
    Ok(())
}

#[test]
fn retry_circuit_breaker_and_andon_are_bounded() -> Result<(), Box<dyn std::error::Error>> {
    let temp = tempfile::tempdir()?;
    let actuator = FilesystemActuator::new(temp.path().join("actuation"));
    let machine = machine();

    let mut retrying = runtime(4, 2, 3)?;
    let mut flaky = FlakyObserver {
        failures_remaining: 1,
    };
    let receipt = retrying.execute(
        Trigger::new("retry", "sync", 1, b"visible".to_vec()),
        &machine,
        &actuator,
        &mut flaky,
    )?;
    assert_eq!(receipt.observation_attempts, 2);
    assert_eq!(receipt.logical_backoff_ticks, 1);
    assert!(!retrying.governor().circuit_breaker.is_open());

    let second_temp = tempfile::tempdir()?;
    let second_actuator = FilesystemActuator::new(second_temp.path().join("actuation"));
    let mut failing = runtime(4, 1, 2)?;
    let mut mismatch = MismatchObserver;
    let failure = failing.execute(
        Trigger::new("failure", "sync", 1, b"desired".to_vec()),
        &machine,
        &second_actuator,
        &mut mismatch,
    );
    assert!(matches!(
        failure,
        Err(OperationsError::PostconditionRefused { .. })
    ));
    assert!(failing.governor().circuit_breaker.is_open());
    assert_eq!(failing.governor().andon.level(), AndonLevel::Red);
    assert!(matches!(
        failing.execute(
            Trigger::new("blocked", "sync", 2, b"desired".to_vec()),
            &machine,
            &second_actuator,
            &mut mismatch,
        ),
        Err(OperationsError::AndonStopLine)
    ));
    assert!(matches!(
        failing.governor_mut().andon.escalate(AndonLevel::Yellow),
        Err(OperationsError::AndonDeescalationRefused { .. })
    ));
    Ok(())
}

#[test]
fn error_budget_gates_risky_change() -> Result<(), Box<dyn std::error::Error>> {
    let temp = tempfile::tempdir()?;
    let actuator = FilesystemActuator::new(temp.path().join("actuation"));
    let machine = machine();
    let mut runtime = runtime(1, 3, 1)?;
    let mut observer = FilesystemConsequenceObserver;

    runtime.execute(
        Trigger::new("budget-1", "sync", 1, b"first".to_vec()),
        &machine,
        &actuator,
        &mut observer,
    )?;
    assert_eq!(runtime.governor().error_budget.remaining(), 0);
    assert!(matches!(
        runtime.execute(
            Trigger::new("budget-2", "sync", 2, b"second".to_vec()),
            &machine,
            &actuator,
            &mut observer,
        ),
        Err(OperationsError::ErrorBudgetExhausted { .. })
    ));
    Ok(())
}

#[test]
fn failed_postcondition_executes_lawful_inverse() -> Result<(), Box<dyn std::error::Error>> {
    let temp = tempfile::tempdir()?;
    let actuator = FilesystemActuator::new(temp.path().join("actuation"));
    let machine = machine();
    let mut runtime = runtime(4, 3, 2)?;
    let mut mismatch = MismatchObserver;

    let result = runtime.execute_with_inverse(
        Trigger::new("rollback", "sync", 1, b"new-state".to_vec()),
        Some(b"old-state".to_vec()),
        &machine,
        &actuator,
        &mut mismatch,
    );
    let failure = match result {
        Err(OperationsError::PostconditionRefused { failure, .. }) => failure,
        other => panic!("expected postcondition refusal, got {other:?}"),
    };
    assert_eq!(failure.attempts, 2);
    let rollback = failure.rollback_receipt.expect("lawful inverse receipt");
    rollback.verify()?;
    assert_eq!(
        read_committed_payload(&actuator, &rollback.action_id)?,
        b"old-state"
    );
    assert_eq!(runtime.governor().andon.level(), AndonLevel::Yellow);
    Ok(())
}

#[test]
fn autonomic_controller_repairs_and_records_knowledge() -> Result<(), Box<dyn std::error::Error>> {
    let temp = tempfile::tempdir()?;
    let actuator = FilesystemActuator::new(temp.path().join("actuation"));
    let machine = machine();

    let initial = Action::new(
        "resource-initial",
        Dimension::AutonomicClosure,
        b"state=degraded".to_vec(),
    );
    let initial_grant = machine.derive_grant(&initial)?;
    let initial_receipt = actuator.actuate(&initial_grant, &initial)?;

    let mut resource = AutonomicResource::new(
        "service-a",
        b"state=healthy".to_vec(),
        initial_receipt.action_id,
        "repair",
    )?;
    let mut runtime = runtime(8, 3, 2)?;
    let mut observer = FilesystemConsequenceObserver;
    let receipt = AutonomicOperationsController::new(3).converge(
        &mut resource,
        &mut runtime,
        &machine,
        &actuator,
        &mut observer,
    )?;
    receipt.verify()?;
    assert!(receipt.converged);
    assert_eq!(receipt.cycles, 1);
    assert_eq!(receipt.operations.len(), 1);
    assert_eq!(
        read_committed_payload(&actuator, &resource.current_action_id)?,
        b"state=healthy"
    );
    assert_ne!(receipt.knowledge_digest, [0; 32]);

    assert!(matches!(
        AutonomicOperationsController::new(0).converge(
            &mut resource,
            &mut runtime,
            &machine,
            &actuator,
            &mut observer,
        ),
        Err(OperationsError::AutonomicCycleBoundZero)
    ));

    let mut tampered = receipt;
    tampered.final_state_digest[0] ^= 1;
    assert!(matches!(
        tampered.verify(),
        Err(OperationsError::AutonomicReceiptMismatch)
    ));
    Ok(())
}
