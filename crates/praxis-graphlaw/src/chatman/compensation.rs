//! Compensation-as-workflow (PRD v26.7.11 §10, PROJ-759).
//!
//! PRD §10: "Compensation SHALL be modeled as workflow... There SHALL be no
//! generic `rollback()` assumption for real-world work." This module gives
//! that requirement a real type/manufacture-path skeleton:
//!
//! - [`PriorActuationRef`] cites the specific prior actuation being
//!   remediated (never mutates or removes it — process history is
//!   append-only).
//! - [`CompensationDispatch`] is a sealed dispatch envelope; the only
//!   constructor is [`manufacture_compensation_workflow`], so possessing
//!   one proves it went through validation.
//! - [`CompensationWorkflow`] bundles the PRD's six required elements
//!   (authority, admitted inputs, expected consequence, dispatch, receipt,
//!   replay) — every field is required and validated non-empty at
//!   manufacture time; there is no default/empty constructor and no setter.
//! - [`CompensationLedger`] records manufactured compensation workflows
//!   append-only: its only mutator is [`CompensationLedger::append`] — there
//!   is no `remove`/`clear`/`pop`/indexed-replace anywhere on the type, so
//!   "a partial real-world consequence SHALL never be erased from process
//!   history" (PRD §10) is a fact about this module's API surface, not just
//!   a documented intention.
//!
//! ## Scope: PROJ-759 / PROJ-776
//!
//! PROJ-776's typed catalog is built here: [`CompensationKind`] gives each
//! of the PRD's seven named compensation examples (cancellation, revocation,
//! corrective communication, provisional-state reversal,
//! replacement-document request, exception review, responsible-role
//! notification) its own variant carrying the kind-specific data a real
//! compensation of that kind requires — not just a label distinguishing
//! them syntactically. [`CompensationKind::validate`] extends this module's
//! existing authority/inputs/expected-consequence rigor per-kind (every
//! kind-specific field is required non-empty, newline-free, exactly like
//! the six PRD §10 elements already were), and
//! [`manufacture_compensation_workflow`] folds the declared kind and its
//! fields into both the dispatch digest and the receipt's canonical
//! N-Quads, so the receipt genuinely names *what kind* of compensation was
//! manufactured, not merely that *some* compensation was.

use wasm4pm_compat::hash::blake3_combined;

use super::abi::{Digest, InputHandles, InvocationId, OperatorId, Receipt, Refusal};

/// Domain-separation tag for the compensation dispatch digest, so it can
/// never collide with any other hash scheme in this crate.
const COMPENSATION_DISPATCH_TAG: &str = "chatman/compensation/dispatch/v1";

/// A citation into the specific prior actuation a compensation workflow
/// remediates (PRD §10). Mirrors the shape `ChatmanEngine::actuate` already
/// records in `engine::ActuationRecord.applied`
/// (`(hook_name, idempotency_key)` pairs), plus the `receipt_root` of the
/// run that produced it. This is a pointer, never a mutation path: nothing
/// in this module can use a `PriorActuationRef` to alter or delete the
/// record it cites.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct PriorActuationRef {
    /// `EngineProcessReceipt.receipt_root` of the run whose consequence is
    /// being remediated.
    pub prior_receipt_root: Digest,
    /// The hook name from the prior `engine::ActuationRecord.applied` entry.
    pub prior_hook_name: String,
    /// The idempotency key from that same entry.
    pub prior_idempotency_key: String,
}

/// One of the seven compensation kinds PRD §10 names as examples
/// (cancellation, revocation, corrective communication, provisional-state
/// reversal, replacement-document request, exception review,
/// responsible-role notification). PRD §10 gives only the English phrase
/// list, no field-level schema, so each variant's required data is this
/// module's own reasonable formalization of what a real compensation of
/// that kind must name to be distinguishable from a generic `rollback()`:
/// what specific artifact/grant/state/document is affected, and (for the
/// two communicative kinds) who is being told what.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum CompensationKind {
    /// Cancels a specific prior-actuation artifact outright — no partial
    /// replacement, no accompanying communication.
    Cancellation {
        /// The specific artifact being cancelled (e.g. an invoice number,
        /// a reservation ID) — a cancellation must name what it cancels.
        cancelled_artifact: String,
    },
    /// Revokes a specific prior grant, permission, or credential.
    Revocation {
        /// The specific grant/permission/credential being revoked.
        revoked_grant: String,
    },
    /// Sends a corrective communication to a named recipient about a prior
    /// actuation's mistaken consequence.
    CorrectiveCommunication {
        /// Who the correction is addressed to.
        recipient: String,
        /// The correction being communicated.
        correction_text: String,
    },
    /// Reverses a provisional (not-yet-final) state change.
    ProvisionalStateReversal {
        /// The provisional-state handle being reversed.
        provisional_state_handle: String,
    },
    /// Requests a replacement document for one issued in error.
    ReplacementDocumentRequest {
        /// The document being superseded.
        superseded_document: String,
        /// Why a replacement is required.
        replacement_reason: String,
    },
    /// Escalates a prior actuation to human exception review rather than
    /// an automated compensation.
    ExceptionReview {
        /// The role/desk the exception is escalated to.
        reviewer_role: String,
        /// Why this actuation requires exception review.
        escalation_reason: String,
    },
    /// Notifies the responsible role that a prior actuation requires their
    /// attention.
    ResponsibleRoleNotification {
        /// The responsible role being notified.
        responsible_role: String,
    },
}

impl CompensationKind {
    /// The PRD's own compensation-example phrase, in lower_snake_case
    /// (matching this crate's `ClosureLaw::name`-style vocabulary
    /// convention).
    ///
    /// # Complexity
    /// O(1).
    #[must_use]
    pub fn name(&self) -> &'static str {
        match self {
            CompensationKind::Cancellation { .. } => "cancellation",
            CompensationKind::Revocation { .. } => "revocation",
            CompensationKind::CorrectiveCommunication { .. } => "corrective_communication",
            CompensationKind::ProvisionalStateReversal { .. } => "provisional_state_reversal",
            CompensationKind::ReplacementDocumentRequest { .. } => "replacement_document_request",
            CompensationKind::ExceptionReview { .. } => "exception_review",
            CompensationKind::ResponsibleRoleNotification { .. } => "responsible_role_notification",
        }
    }

    /// This kind's own required field name/value pairs, in fixed declared
    /// order (never a `HashMap`; the order is the match arm's own field
    /// order, stable across runs and platforms).
    ///
    /// # Complexity
    /// O(1) — at most two fields per variant.
    fn canonical_fields(&self) -> Vec<(&'static str, &str)> {
        match self {
            CompensationKind::Cancellation { cancelled_artifact } => {
                vec![("cancelled_artifact", cancelled_artifact.as_str())]
            }
            CompensationKind::Revocation { revoked_grant } => {
                vec![("revoked_grant", revoked_grant.as_str())]
            }
            CompensationKind::CorrectiveCommunication {
                recipient,
                correction_text,
            } => vec![
                ("recipient", recipient.as_str()),
                ("correction_text", correction_text.as_str()),
            ],
            CompensationKind::ProvisionalStateReversal {
                provisional_state_handle,
            } => vec![(
                "provisional_state_handle",
                provisional_state_handle.as_str(),
            )],
            CompensationKind::ReplacementDocumentRequest {
                superseded_document,
                replacement_reason,
            } => vec![
                ("superseded_document", superseded_document.as_str()),
                ("replacement_reason", replacement_reason.as_str()),
            ],
            CompensationKind::ExceptionReview {
                reviewer_role,
                escalation_reason,
            } => vec![
                ("reviewer_role", reviewer_role.as_str()),
                ("escalation_reason", escalation_reason.as_str()),
            ],
            CompensationKind::ResponsibleRoleNotification { responsible_role } => {
                vec![("responsible_role", responsible_role.as_str())]
            }
        }
    }

    /// Validates that every field this kind requires is present
    /// (non-empty, non-whitespace-only) and newline-free — the per-kind
    /// extension of this module's existing authority/inputs/
    /// expected-consequence rigor (PRD §10 names seven compensation
    /// examples; this catalog validates each one's own kind-specific data,
    /// not just its label).
    ///
    /// # Errors
    /// [`Refusal::ValidationFailed`] naming the first empty/whitespace-only
    /// or newline-containing field found.
    ///
    /// # Complexity
    /// O(1) — at most two fields per variant.
    pub fn validate(&self) -> Result<(), Refusal> {
        for (field_name, value) in self.canonical_fields() {
            if value.trim().is_empty() {
                return Err(Refusal::ValidationFailed(format!(
                    "compensation kind {} requires a non-empty {field_name} field (PRD §10 \
                     names {} as a compensation example; this catalog validates its \
                     kind-specific data, not just its label)",
                    self.name(),
                    self.name()
                )));
            }
            refuse_if_contains_newline(field_name, value)?;
        }
        Ok(())
    }
}

/// A sealed manufactured dispatch envelope for one compensation workflow.
/// Private fields: the only constructor is
/// [`manufacture_compensation_workflow`], so a caller cannot hand-build one
/// — there is no `rollback()`-shaped shortcut that skips validation.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct CompensationDispatch {
    invocation_id: InvocationId,
    dispatch_digest: Digest,
    #[allow(dead_code)] // the seal's whole job is existing privately
    seal: CompensationSeal,
}

/// Private zero-sized seal; not constructible outside this module.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
struct CompensationSeal(());

impl CompensationDispatch {
    /// Deterministic invocation identity for this compensation dispatch
    /// (derived from [`Self::dispatch_digest`], never random or wall-clock
    /// derived).
    #[must_use]
    pub fn invocation_id(&self) -> &InvocationId {
        &self.invocation_id
    }

    /// The canonical BLAKE3 digest binding every declared element of this
    /// compensation's manufacture (see [`manufacture_compensation_workflow`]
    /// for the exact field order hashed).
    #[must_use]
    pub fn dispatch_digest(&self) -> &Digest {
        &self.dispatch_digest
    }
}

/// A manufactured compensation workflow (PRD §10): authority, admitted
/// inputs, expected consequence, dispatch, receipt, and (via the receipt
/// envelope's replay hint) replay — never a generic `rollback()`. Every
/// field is required and validated non-empty at manufacture time.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct CompensationWorkflow {
    remediates: PriorActuationRef,
    kind: CompensationKind,
    authority: OperatorId,
    admitted_inputs: InputHandles,
    expected_consequence: String,
    dispatch: CompensationDispatch,
    receipt: Receipt,
}

impl CompensationWorkflow {
    /// The prior actuation this compensation remediates.
    #[must_use]
    pub fn remediates(&self) -> &PriorActuationRef {
        &self.remediates
    }
    /// The compensation kind this workflow was manufactured as (PRD §10's
    /// seven named examples) — see [`CompensationKind`].
    #[must_use]
    pub fn kind(&self) -> &CompensationKind {
        &self.kind
    }
    /// The declared authority manufacturing this compensation (PRD §10
    /// "authority").
    #[must_use]
    pub fn authority(&self) -> &OperatorId {
        &self.authority
    }
    /// The admitted inputs this compensation workflow is scoped to (PRD §10
    /// "admitted inputs").
    #[must_use]
    pub fn admitted_inputs(&self) -> &InputHandles {
        &self.admitted_inputs
    }
    /// The declared expected consequence of running this compensation (PRD
    /// §10 "expected consequence").
    #[must_use]
    pub fn expected_consequence(&self) -> &str {
        &self.expected_consequence
    }
    /// The manufactured dispatch envelope (PRD §10 "dispatch").
    #[must_use]
    pub fn dispatch(&self) -> &CompensationDispatch {
        &self.dispatch
    }
    /// The receipt covering this compensation's declared facts (PRD §10
    /// "receipt"; its envelope's replay hint covers "replay").
    #[must_use]
    pub fn receipt(&self) -> &Receipt {
        &self.receipt
    }
}

/// Canonical, sorted digest of one [`InputHandles`] value's three vectors,
/// so two logically-equal handle sets (same members, different insertion
/// order) manufacture the same dispatch digest. A local helper rather than
/// a reuse of `abi`'s private `sorted_handles_digest` (not exposed outside
/// `abi.rs`) — same canonicalization law (sort, then length-prefixed
/// combine via [`blake3_combined`]), independently applied here.
///
/// # Complexity
/// O(h log h) for the three sorts, h = total handle count across the three
/// vectors.
fn admitted_inputs_digest(handles: &InputHandles) -> String {
    let mut nodes: Vec<&str> = handles.nodes.iter().map(String::as_str).collect();
    nodes.sort_unstable();
    let mut events: Vec<&str> = handles.events.iter().map(String::as_str).collect();
    events.sort_unstable();
    let mut plan_steps: Vec<&str> = handles.plan_steps.iter().map(String::as_str).collect();
    plan_steps.sort_unstable();
    blake3_combined(&[
        "nodes",
        &blake3_combined(&nodes),
        "events",
        &blake3_combined(&events),
        "plan_steps",
        &blake3_combined(&plan_steps),
    ])
}

/// Refuses if `field` contains a newline: this module's receipt material is
/// one-fact-per-line (see [`manufacture_compensation_workflow`]), so an
/// embedded newline would silently split one declared fact into two lines
/// and corrupt the correspondence between the digest and the declared
/// facts.
///
/// # Complexity
/// O(len(field)).
fn refuse_if_contains_newline(field_name: &str, field: &str) -> Result<(), Refusal> {
    if field.contains('\n') {
        return Err(Refusal::ValidationFailed(format!(
            "compensation workflow field {field_name:?} contains a newline; receipt material \
             is one fact per line and an embedded newline would corrupt the digest"
        )));
    }
    Ok(())
}

/// Manufactures a compensation workflow for a prior actuation requiring
/// remediation (PRD §10). This is the *only* constructor of
/// [`CompensationWorkflow`]/[`CompensationDispatch`]: there is no
/// `rollback()` function anywhere in this module, and none of
/// [`CompensationWorkflow`]'s fields are settable after construction — a
/// caller cannot manufacture a compensation without declaring all of PRD
/// §10's required elements.
///
/// # Errors
/// [`Refusal::ValidationFailed`] when:
/// - `remediates.prior_receipt_root` is empty, or
///   `remediates.prior_hook_name`/`prior_idempotency_key` is empty — a
///   compensation must cite the specific prior actuation it remediates;
///   process history is appended to, never inferred.
/// - `kind`'s own required fields are missing (see
///   [`CompensationKind::validate`]).
/// - `authority` is empty — PRD §10's required "authority" element.
/// - `admitted_inputs` is entirely empty (all three vectors) — a
///   compensation over no admitted inputs is indistinguishable from a
///   generic `rollback()`.
/// - `expected_consequence` is empty or whitespace-only — PRD §10's
///   required "expected consequence" element.
/// - any field contains a newline (see [`refuse_if_contains_newline`]).
///
/// # Complexity
/// O(h log h) for [`admitted_inputs_digest`]'s sorts (h = total handle
/// count) plus O(1) additional BLAKE3 hashing and O(1) receipt-line
/// construction (`kind`'s own fields contribute at most 2 more lines,
/// [`CompensationKind::canonical_fields`] is O(1)).
pub fn manufacture_compensation_workflow(
    remediates: PriorActuationRef,
    kind: CompensationKind,
    authority: OperatorId,
    admitted_inputs: InputHandles,
    expected_consequence: String,
) -> Result<CompensationWorkflow, Refusal> {
    kind.validate()?;
    if remediates.prior_receipt_root.0.is_empty() {
        return Err(Refusal::ValidationFailed(
            "compensation workflow must cite a non-empty prior_receipt_root; a compensation \
             cannot remediate an unnamed actuation (PRD §10)"
                .to_string(),
        ));
    }
    if remediates.prior_hook_name.is_empty() || remediates.prior_idempotency_key.is_empty() {
        return Err(Refusal::ValidationFailed(
            "compensation workflow must cite the specific prior actuation it remediates (hook \
             name + idempotency key); process history is append-only, not inferred (PRD §10)"
                .to_string(),
        ));
    }
    if authority.as_str().is_empty() {
        return Err(Refusal::ValidationFailed(
            "compensation workflow requires a non-empty declared authority (PRD §10); there is \
             no generic rollback() default"
                .to_string(),
        ));
    }
    if admitted_inputs.nodes.is_empty()
        && admitted_inputs.events.is_empty()
        && admitted_inputs.plan_steps.is_empty()
    {
        return Err(Refusal::ValidationFailed(
            "compensation workflow requires at least one admitted input handle (PRD §10); a \
             compensation over no admitted inputs cannot be distinguished from a generic \
             rollback()"
                .to_string(),
        ));
    }
    if expected_consequence.trim().is_empty() {
        return Err(Refusal::ValidationFailed(
            "compensation workflow requires a non-empty declared expected consequence (PRD §10)"
                .to_string(),
        ));
    }
    refuse_if_contains_newline("prior_hook_name", &remediates.prior_hook_name)?;
    refuse_if_contains_newline("prior_idempotency_key", &remediates.prior_idempotency_key)?;
    refuse_if_contains_newline("authority", authority.as_str())?;
    refuse_if_contains_newline("expected_consequence", &expected_consequence)?;

    let inputs_digest = admitted_inputs_digest(&admitted_inputs);
    // O(1): `kind`'s own field list is bounded (<= 2 entries per variant).
    let kind_fields = kind.canonical_fields();
    let mut kind_digest_parts: Vec<&str> = Vec::with_capacity(1 + kind_fields.len() * 2);
    kind_digest_parts.push(kind.name());
    for (field_name, value) in kind_fields.iter().copied() {
        kind_digest_parts.push(field_name);
        kind_digest_parts.push(value);
    }
    let kind_digest = blake3_combined(&kind_digest_parts);

    let dispatch_digest = Digest::new(blake3_combined(&[
        COMPENSATION_DISPATCH_TAG,
        "prior_receipt_root",
        &remediates.prior_receipt_root.0,
        "prior_hook_name",
        &remediates.prior_hook_name,
        "prior_idempotency_key",
        &remediates.prior_idempotency_key,
        "kind",
        &kind_digest,
        "authority",
        authority.as_str(),
        "admitted_inputs",
        &inputs_digest,
        "expected_consequence",
        &expected_consequence,
    ]));
    let invocation_id = InvocationId::new(format!("compensation:{}", dispatch_digest.0));

    let dispatch = CompensationDispatch {
        invocation_id,
        dispatch_digest: dispatch_digest.clone(),
        seal: CompensationSeal(()),
    };

    // Canonical N-Quads-shaped receipt material: four fixed declared facts
    // plus one line per kind-specific field, all sorted lexicographically
    // (matching `Receipt::from_canonical_nquads`'s own sortedness check).
    let mut lines: Vec<String> = vec![
        format!(
            "<urn:chatman:compensation:{}> <urn:chatman:compensation:authority> \"{}\" .",
            dispatch_digest.0,
            authority.as_str()
        ),
        format!(
            "<urn:chatman:compensation:{}> <urn:chatman:compensation:expected-consequence> \"{}\" .",
            dispatch_digest.0, expected_consequence
        ),
        format!(
            "<urn:chatman:compensation:{}> <urn:chatman:compensation:remediates> \
             <urn:chatman:receipt-root:{}> .",
            dispatch_digest.0, remediates.prior_receipt_root.0
        ),
        format!(
            "<urn:chatman:compensation:{}> <urn:chatman:compensation:kind> \"{}\" .",
            dispatch_digest.0,
            kind.name()
        ),
    ];
    for (field_name, value) in &kind_fields {
        lines.push(format!(
            "<urn:chatman:compensation:{}> <urn:chatman:compensation:kind:{field_name}> \"{}\" .",
            dispatch_digest.0, value
        ));
    }
    lines.sort();
    let canon_nquads = lines.join("\n");

    let subject = format!("urn:chatman:compensation:{}", dispatch_digest.0);
    let witness = format!("urn:chatman:authority:{}", authority.as_str());
    let replay_hint = format!("chatman-compensation-replay:{}", dispatch_digest.0);
    let receipt = Receipt::from_canonical_nquads(&subject, &witness, &replay_hint, &canon_nquads)?;

    Ok(CompensationWorkflow {
        remediates,
        kind,
        authority,
        admitted_inputs,
        expected_consequence,
        dispatch,
        receipt,
    })
}

/// An append-only ledger of manufactured compensation workflows (PRD §10:
/// "A partial real-world consequence SHALL never be erased from process
/// history"). The only mutator is [`Self::append`]; there is no `remove`,
/// `clear`, `pop`, or indexed replacement anywhere on this type.
#[derive(Debug, Clone, Default, PartialEq, Eq)]
pub struct CompensationLedger {
    entries: Vec<CompensationWorkflow>,
}

impl CompensationLedger {
    /// An empty ledger.
    #[must_use]
    pub fn new() -> Self {
        Self {
            entries: Vec::new(),
        }
    }

    /// Appends one manufactured compensation workflow. Never removes or
    /// reorders prior entries.
    ///
    /// # Complexity
    /// O(1) amortized (`Vec::push`).
    pub fn append(&mut self, workflow: CompensationWorkflow) {
        self.entries.push(workflow);
    }

    /// Read-only, insertion-ordered view of every compensation workflow
    /// recorded so far.
    #[must_use]
    pub fn entries(&self) -> &[CompensationWorkflow] {
        &self.entries
    }
}

#[cfg(test)]
#[path = "compensation_test.rs"]
mod tests;
