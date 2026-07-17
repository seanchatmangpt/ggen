//! Tests for [`super::manufacture_compensation_workflow`] (PRD v26.7.11
//! §10, PROJ-759/PROJ-776).

use super::*;

fn valid_prior_ref() -> PriorActuationRef {
    PriorActuationRef {
        prior_receipt_root: Digest::new("blake3:deadbeef"),
        prior_hook_name: "hook:issue-invoice".to_string(),
        prior_idempotency_key: "idem:invoice-42".to_string(),
    }
}

fn valid_inputs() -> InputHandles {
    InputHandles {
        nodes: vec!["urn:node:invoice-42".to_string()],
        events: vec![],
        plan_steps: vec![],
    }
}

fn valid_kind() -> CompensationKind {
    CompensationKind::Cancellation {
        cancelled_artifact: "urn:node:invoice-42".to_string(),
    }
}

fn manufacture_ok() -> Result<CompensationWorkflow, Refusal> {
    manufacture_compensation_workflow(
        valid_prior_ref(),
        valid_kind(),
        OperatorId::new("urn:operator:remediation-desk"),
        valid_inputs(),
        "cancel the mistakenly issued invoice #42".to_string(),
    )
}

/// One valid instance of every [`CompensationKind`] variant, paired with
/// its PRD-vocabulary name — used by both the positive "all seven
/// construct" test and the digest-differs-by-kind test.
fn all_valid_kinds() -> Vec<(&'static str, CompensationKind)> {
    vec![
        (
            "cancellation",
            CompensationKind::Cancellation {
                cancelled_artifact: "urn:node:invoice-42".to_string(),
            },
        ),
        (
            "revocation",
            CompensationKind::Revocation {
                revoked_grant: "urn:grant:vendor-access-77".to_string(),
            },
        ),
        (
            "corrective_communication",
            CompensationKind::CorrectiveCommunication {
                recipient: "urn:party:customer-9".to_string(),
                correction_text: "the prior invoice total was incorrect".to_string(),
            },
        ),
        (
            "provisional_state_reversal",
            CompensationKind::ProvisionalStateReversal {
                provisional_state_handle: "urn:state:reservation-hold-3".to_string(),
            },
        ),
        (
            "replacement_document_request",
            CompensationKind::ReplacementDocumentRequest {
                superseded_document: "urn:doc:invoice-42".to_string(),
                replacement_reason: "wrong line-item total".to_string(),
            },
        ),
        (
            "exception_review",
            CompensationKind::ExceptionReview {
                reviewer_role: "urn:role:finance-exceptions-desk".to_string(),
                escalation_reason: "amount exceeds automated remediation authority".to_string(),
            },
        ),
        (
            "responsible_role_notification",
            CompensationKind::ResponsibleRoleNotification {
                responsible_role: "urn:role:invoice-owner".to_string(),
            },
        ),
    ]
}

// ---------------------------------------------------------------------------
// Happy path: all six PRD §10 elements present and verifiable.
// ---------------------------------------------------------------------------

#[test]
fn manufacture_produces_a_verifiable_receipt_carrying_all_declared_elements() -> Result<(), Refusal>
{
    let workflow = manufacture_ok()?;
    workflow.receipt().verify()?;
    assert_eq!(
        workflow.authority().as_str(),
        "urn:operator:remediation-desk"
    );
    assert_eq!(
        workflow.expected_consequence(),
        "cancel the mistakenly issued invoice #42"
    );
    assert_eq!(workflow.remediates().prior_hook_name, "hook:issue-invoice");
    assert_eq!(workflow.kind().name(), "cancellation");
    assert!(!workflow.dispatch().dispatch_digest().0.is_empty());
    assert!(!workflow.dispatch().invocation_id().as_str().is_empty());
    Ok(())
}

#[test]
fn manufacture_is_deterministic_across_repeated_calls() -> Result<(), Refusal> {
    let a = manufacture_ok()?;
    let b = manufacture_ok()?;
    assert_eq!(
        a.dispatch().dispatch_digest(),
        b.dispatch().dispatch_digest()
    );
    assert_eq!(a.dispatch().invocation_id(), b.dispatch().invocation_id());
    assert_eq!(
        a.receipt().envelope.digest.as_inner(),
        b.receipt().envelope.digest.as_inner()
    );
    Ok(())
}

#[test]
fn manufacture_digest_changes_when_expected_consequence_changes() -> Result<(), Refusal> {
    let a = manufacture_ok()?;
    let b = manufacture_compensation_workflow(
        valid_prior_ref(),
        valid_kind(),
        OperatorId::new("urn:operator:remediation-desk"),
        valid_inputs(),
        "issue a corrective notice instead".to_string(),
    )?;
    assert_ne!(
        a.dispatch().dispatch_digest(),
        b.dispatch().dispatch_digest()
    );
    Ok(())
}

// ---------------------------------------------------------------------------
// "No generic rollback()": every required element is independently
// enforced, and none can be silently defaulted.
// ---------------------------------------------------------------------------

#[test]
fn refuses_empty_authority() {
    let result = manufacture_compensation_workflow(
        valid_prior_ref(),
        valid_kind(),
        OperatorId::new(""),
        valid_inputs(),
        "cancel it".to_string(),
    );
    assert!(matches!(result, Err(Refusal::ValidationFailed(_))));
}

#[test]
fn refuses_empty_admitted_inputs() {
    let result = manufacture_compensation_workflow(
        valid_prior_ref(),
        valid_kind(),
        OperatorId::new("urn:operator:remediation-desk"),
        InputHandles::default(),
        "cancel it".to_string(),
    );
    assert!(matches!(result, Err(Refusal::ValidationFailed(_))));
}

#[test]
fn refuses_empty_expected_consequence() {
    let result = manufacture_compensation_workflow(
        valid_prior_ref(),
        valid_kind(),
        OperatorId::new("urn:operator:remediation-desk"),
        valid_inputs(),
        "   ".to_string(),
    );
    assert!(matches!(result, Err(Refusal::ValidationFailed(_))));
}

#[test]
fn refuses_missing_prior_receipt_root() {
    let mut prior = valid_prior_ref();
    prior.prior_receipt_root = Digest::new("");
    let result = manufacture_compensation_workflow(
        prior,
        valid_kind(),
        OperatorId::new("urn:operator:remediation-desk"),
        valid_inputs(),
        "cancel it".to_string(),
    );
    assert!(matches!(result, Err(Refusal::ValidationFailed(_))));
}

#[test]
fn refuses_missing_prior_hook_name() {
    let mut prior = valid_prior_ref();
    prior.prior_hook_name = String::new();
    let result = manufacture_compensation_workflow(
        prior,
        valid_kind(),
        OperatorId::new("urn:operator:remediation-desk"),
        valid_inputs(),
        "cancel it".to_string(),
    );
    assert!(matches!(result, Err(Refusal::ValidationFailed(_))));
}

#[test]
fn refuses_missing_prior_idempotency_key() {
    let mut prior = valid_prior_ref();
    prior.prior_idempotency_key = String::new();
    let result = manufacture_compensation_workflow(
        prior,
        valid_kind(),
        OperatorId::new("urn:operator:remediation-desk"),
        valid_inputs(),
        "cancel it".to_string(),
    );
    assert!(matches!(result, Err(Refusal::ValidationFailed(_))));
}

#[test]
fn refuses_a_newline_embedded_in_expected_consequence() {
    let result = manufacture_compensation_workflow(
        valid_prior_ref(),
        valid_kind(),
        OperatorId::new("urn:operator:remediation-desk"),
        valid_inputs(),
        "cancel it\nand also do something else".to_string(),
    );
    assert!(matches!(result, Err(Refusal::ValidationFailed(_))));
}

// ---------------------------------------------------------------------------
// PROJ-776 — the seven-kind compensation catalog: real construction and
// per-kind validation, not just labels.
// ---------------------------------------------------------------------------

#[test]
fn compensation_kind_name_matches_prd_vocabulary() {
    for (expected_name, kind) in all_valid_kinds() {
        assert_eq!(kind.name(), expected_name);
    }
}

#[test]
fn manufacture_accepts_all_seven_compensation_kinds() -> Result<(), Refusal> {
    for (expected_name, kind) in all_valid_kinds() {
        let workflow = manufacture_compensation_workflow(
            valid_prior_ref(),
            kind,
            OperatorId::new("urn:operator:remediation-desk"),
            valid_inputs(),
            "remediate the prior actuation".to_string(),
        )?;
        workflow.receipt().verify()?;
        assert_eq!(workflow.kind().name(), expected_name);
    }
    Ok(())
}

#[test]
fn manufacture_digest_differs_across_compensation_kinds() -> Result<(), Refusal> {
    let mut digests = Vec::new();
    for (_, kind) in all_valid_kinds() {
        let workflow = manufacture_compensation_workflow(
            valid_prior_ref(),
            kind,
            OperatorId::new("urn:operator:remediation-desk"),
            valid_inputs(),
            "remediate the prior actuation".to_string(),
        )?;
        digests.push(workflow.dispatch().dispatch_digest().clone());
    }
    let mut sorted = digests.clone();
    sorted.sort_by(|a, b| a.0.cmp(&b.0));
    sorted.dedup_by(|a, b| a.0 == b.0);
    assert_eq!(
        sorted.len(),
        digests.len(),
        "every compensation kind must manufacture a distinct dispatch digest, all else equal"
    );
    Ok(())
}

#[test]
fn compensation_kind_validate_refuses_each_kinds_own_empty_required_field() {
    let invalid_kinds: Vec<(&str, CompensationKind)> = vec![
        (
            "cancellation.cancelled_artifact",
            CompensationKind::Cancellation {
                cancelled_artifact: String::new(),
            },
        ),
        (
            "revocation.revoked_grant",
            CompensationKind::Revocation {
                revoked_grant: "   ".to_string(),
            },
        ),
        (
            "corrective_communication.recipient",
            CompensationKind::CorrectiveCommunication {
                recipient: String::new(),
                correction_text: "the total was wrong".to_string(),
            },
        ),
        (
            "corrective_communication.correction_text",
            CompensationKind::CorrectiveCommunication {
                recipient: "urn:party:customer-9".to_string(),
                correction_text: String::new(),
            },
        ),
        (
            "provisional_state_reversal.provisional_state_handle",
            CompensationKind::ProvisionalStateReversal {
                provisional_state_handle: String::new(),
            },
        ),
        (
            "replacement_document_request.superseded_document",
            CompensationKind::ReplacementDocumentRequest {
                superseded_document: String::new(),
                replacement_reason: "wrong total".to_string(),
            },
        ),
        (
            "replacement_document_request.replacement_reason",
            CompensationKind::ReplacementDocumentRequest {
                superseded_document: "urn:doc:invoice-42".to_string(),
                replacement_reason: String::new(),
            },
        ),
        (
            "exception_review.reviewer_role",
            CompensationKind::ExceptionReview {
                reviewer_role: String::new(),
                escalation_reason: "exceeds authority".to_string(),
            },
        ),
        (
            "exception_review.escalation_reason",
            CompensationKind::ExceptionReview {
                reviewer_role: "urn:role:finance-exceptions-desk".to_string(),
                escalation_reason: String::new(),
            },
        ),
        (
            "responsible_role_notification.responsible_role",
            CompensationKind::ResponsibleRoleNotification {
                responsible_role: String::new(),
            },
        ),
    ];
    for (label, kind) in invalid_kinds {
        assert!(
            matches!(kind.validate(), Err(Refusal::ValidationFailed(_))),
            "expected ValidationFailed for {label}"
        );
        // Manufacture itself must refuse the same way, not silently accept.
        let result = manufacture_compensation_workflow(
            valid_prior_ref(),
            kind,
            OperatorId::new("urn:operator:remediation-desk"),
            valid_inputs(),
            "remediate the prior actuation".to_string(),
        );
        assert!(
            matches!(result, Err(Refusal::ValidationFailed(_))),
            "expected manufacture to refuse for {label}"
        );
    }
}

#[test]
fn compensation_kind_validate_refuses_a_newline_in_a_kind_specific_field() {
    let kind = CompensationKind::Cancellation {
        cancelled_artifact: "urn:node:invoice-42\nand-something-else".to_string(),
    };
    assert!(matches!(kind.validate(), Err(Refusal::ValidationFailed(_))));
}

// ---------------------------------------------------------------------------
// Append-only ledger: process history is never erased (PRD §10).
// ---------------------------------------------------------------------------

#[test]
fn ledger_append_preserves_insertion_order_and_never_shrinks() -> Result<(), Refusal> {
    let first = manufacture_ok()?;
    let second = manufacture_compensation_workflow(
        valid_prior_ref(),
        valid_kind(),
        OperatorId::new("urn:operator:remediation-desk"),
        valid_inputs(),
        "issue a corrective notice instead".to_string(),
    )?;
    let mut ledger = CompensationLedger::new();
    assert!(ledger.entries().is_empty());
    ledger.append(first.clone());
    assert_eq!(ledger.entries().len(), 1);
    ledger.append(second.clone());
    assert_eq!(ledger.entries().len(), 2);
    assert_eq!(ledger.entries()[0], first);
    assert_eq!(ledger.entries()[1], second);
    Ok(())
}
