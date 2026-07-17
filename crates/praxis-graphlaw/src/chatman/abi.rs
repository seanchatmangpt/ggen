//! Chatman engine ABI: identity newtypes, invocation envelopes, receipts, and
//! the refusal taxonomy shared by every chatman lane.
//!
//! Identity discipline: `wasm4pm_compat` owns receipt identity. Every hash in
//! this module is computed through [`wasm4pm_compat::hash::blake3_hex`] or
//! [`wasm4pm_compat::hash::blake3_combined`]; no local hashing scheme exists.
//! All hash material is field-tagged and sorted before hashing, so the same
//! logical envelope always produces byte-identical digests.

use serde::{Deserialize, Serialize};
use wasm4pm_compat::hash::{blake3_combined, blake3_hex};
use wasm4pm_compat::receipt::{ReceiptEnvelope, ReplayHint};

pub use wasm4pm_compat::receipt::Digest;

/// Version tag mixed into every stage seal so a seal can never collide with
/// the [`Digest`] it seals or with any other hash scheme in this module.
const STAGE_SEAL_TAG: &str = "chatman/engine/stage-seal/v1";

/// A per-transition integrity seal threaded between adjacent S1-S6 pipeline
/// stages (PROJ-SEC-01).
///
/// Distinct from the nine [`Digest`]s composed into the final
/// `receipt_root` at the end of a run: those are asserted once, at S6. A
/// `StageSeal` is checked *before* the next stage runs — it proves the
/// value handed from stage N to stage N+1 is exactly what stage N produced,
/// closing the gap where corruption or tampering between two stage calls
/// would otherwise go undetected until (or unless) the end-of-run receipt
/// happened to catch it.
///
/// Wraps the same [`Digest`] representation compat already uses for receipt
/// identity; no parallel hashing scheme is invented.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct StageSeal(pub Digest);

impl StageSeal {
    /// Computes the seal of a stage's output digest, tagged by stage name so
    /// a seal computed for `"S1"` can never collide with one computed for
    /// `"S2"` over the same digest bytes.
    ///
    /// # Complexity
    /// O(len(stage_name) + len(prior.0)) — one BLAKE3 hash over
    /// tag-length-prefixed material; no iteration.
    pub fn of(stage_name: &str, prior: &Digest) -> Self {
        StageSeal(Digest::new(blake3_combined(&[
            STAGE_SEAL_TAG,
            stage_name,
            &prior.0,
        ])))
    }

    /// Verifies this seal against a freshly recomputed one over
    /// `(stage_name, prior)`. Refuses with [`Refusal::StageSealMismatch`] on
    /// any mismatch — a real digest recomputation and comparison, not a
    /// no-op check.
    ///
    /// # Complexity
    /// O(len(stage_name) + len(prior.0)) — one BLAKE3 hash plus one string
    /// comparison.
    pub fn verify(&self, stage_name: &str, prior: &Digest) -> Result<(), Refusal> {
        let recomputed = StageSeal::of(stage_name, prior);
        if recomputed.0 .0 != self.0 .0 {
            return Err(Refusal::StageSealMismatch(format!(
                "stage {stage_name} recomputed seal {} does not match seal {} threaded from \
                 the prior stage",
                recomputed.0 .0, self.0 .0
            )));
        }
        Ok(())
    }
}

/// Defines a string-backed identity newtype with ordered, hashable semantics.
macro_rules! chatman_id {
    ($(#[$doc:meta])* $name:ident) => {
        $(#[$doc])*
        #[derive(
            Debug,
            Clone,
            PartialEq,
            Eq,
            Hash,
            PartialOrd,
            Ord,
            Serialize,
            Deserialize,
        )]
        #[serde(transparent)]
        pub struct $name(String);

        impl $name {
            /// Wraps a raw identity string. Performs no validation; identity
            /// admission is the caller's law, not the newtype's.
            pub fn new(inner: impl Into<String>) -> Self {
                Self(inner.into())
            }

            /// Borrows the underlying identity string.
            pub fn as_str(&self) -> &str {
                &self.0
            }
        }

        impl core::fmt::Display for $name {
            fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
                write!(f, "{}", self.0)
            }
        }
    };
}

chatman_id!(
    /// Identity of one engine invocation (one envelope, one receipt).
    InvocationId
);
chatman_id!(
    /// Identity of the immutable graph snapshot an invocation reads from.
    GraphSnapshotId
);
chatman_id!(
    /// Identity of the semantic profile governing dialect availability.
    ProfileId
);
chatman_id!(
    /// Identity of the operator (human or agent) requesting the invocation.
    OperatorId
);

/// Handles into the input graph an invocation is allowed to touch.
///
/// Handle order is not semantic: [`InvocationEnvelope::envelope_hash`] sorts
/// each vector before hashing, so permutations of the same handle sets are
/// hash-identical.
#[derive(Debug, Clone, PartialEq, Eq, Hash, Serialize, Deserialize, Default)]
pub struct InputHandles {
    /// Node handles (subjects/objects) admitted as invocation input.
    pub nodes: Vec<String>,
    /// OCEL event handles admitted as invocation input.
    pub events: Vec<String>,
    /// Plan-step handles admitted as invocation input.
    pub plan_steps: Vec<String>,
}

/// The invocation envelope: who invokes what, over which snapshot, under
/// which profile, touching which handles.
#[derive(Debug, Clone, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct InvocationEnvelope {
    /// Identity of this invocation.
    pub invocation_id: InvocationId,
    /// Immutable graph snapshot the invocation reads from.
    pub snapshot_id: GraphSnapshotId,
    /// Semantic profile governing dialect availability for this invocation.
    pub profile_id: ProfileId,
    /// Operator requesting the invocation.
    pub operator_id: OperatorId,
    /// Input handles admitted for this invocation.
    pub input_handles: InputHandles,
}

/// Version tag mixed into every envelope hash so a future field change can
/// never silently collide with v1 digests.
const ENVELOPE_HASH_TAG: &str = "chatman:invocation-envelope:v1";

impl InvocationEnvelope {
    /// Computes the canonical BLAKE3 digest of this envelope as 64 lowercase
    /// hex characters.
    ///
    /// Canonical form: each handle vector is sorted, then combined with
    /// [`blake3_combined`] (length-prefixed, injective); the scalar identity
    /// fields and the three handle digests are combined field-tagged, again
    /// via [`blake3_combined`]. No wall clock, no randomness.
    ///
    /// # Complexity
    /// O(h log h) where h is the total number of input handles (dominated by
    /// the three canonical sorts); hashing itself is O(bytes).
    pub fn envelope_hash(&self) -> String {
        let nodes_digest = sorted_handles_digest(&self.input_handles.nodes);
        let events_digest = sorted_handles_digest(&self.input_handles.events);
        let plan_steps_digest = sorted_handles_digest(&self.input_handles.plan_steps);
        blake3_combined(&[
            ENVELOPE_HASH_TAG,
            "invocation_id",
            self.invocation_id.as_str(),
            "snapshot_id",
            self.snapshot_id.as_str(),
            "profile_id",
            self.profile_id.as_str(),
            "operator_id",
            self.operator_id.as_str(),
            "nodes",
            &nodes_digest,
            "events",
            &events_digest,
            "plan_steps",
            &plan_steps_digest,
        ])
    }
}

/// Digest of one handle vector in canonical (sorted) order.
///
/// # Complexity
/// O(n log n) for the sort over n handles; hashing is O(total bytes).
fn sorted_handles_digest(handles: &[String]) -> String {
    let mut sorted: Vec<&str> = handles.iter().map(String::as_str).collect();
    sorted.sort_unstable();
    blake3_combined(&sorted)
}

/// A chatman receipt: a compat-owned [`ReceiptEnvelope`] paired with the
/// canonical N-Quads material its digest was computed from.
///
/// The digest is always *computed* here (never asserted by a caller): the only
/// constructor is [`Receipt::from_canonical_nquads`], which hashes the
/// material itself, and [`Receipt::verify`] recomputes that hash.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Receipt {
    /// Compat-owned receipt identity (subject, witness, digest, replay hint).
    pub envelope: ReceiptEnvelope,
    /// The canonical N-Quads material the digest covers, lines sorted.
    pub canon_nquads: String,
}

impl Receipt {
    /// Builds a receipt by hashing canonical N-Quads material.
    ///
    /// Refuses with [`Refusal::ValidationFailed`] when the N-Quads lines are
    /// not in canonical sorted order (receipt material must be sorted before
    /// BLAKE3), and with [`Refusal::MissingReceipt`] when any envelope part
    /// (subject, witness, replay hint, material) is empty.
    ///
    /// # Complexity
    /// O(n) over the N-Quads lines for the sortedness check, plus O(bytes)
    /// for the BLAKE3 digest.
    pub fn from_canonical_nquads(
        subject: &str,
        witness: &str,
        replay_hint: &str,
        canon_nquads: &str,
    ) -> Result<Receipt, Refusal> {
        if canon_nquads.is_empty() {
            return Err(Refusal::MissingReceipt(
                "canonical N-Quads material is empty; a receipt must cover at least one quad"
                    .to_string(),
            ));
        }
        // Sortedness check: adjacent-pair comparison over lines, O(n).
        let mut lines = canon_nquads.lines();
        if let Some(first) = lines.next() {
            let mut prev = first;
            for line in lines {
                if line < prev {
                    return Err(Refusal::ValidationFailed(format!(
                        "canonical N-Quads material is not sorted: line {line:?} \
                         follows {prev:?}; sort all receipt material before hashing"
                    )));
                }
                prev = line;
            }
        }
        let digest = Digest::new(blake3_hex(canon_nquads.as_bytes()));
        let envelope =
            ReceiptEnvelope::try_from_parts(subject, witness, digest, ReplayHint::new(replay_hint))
                .map_err(|shape_refusal| {
                    Refusal::MissingReceipt(format!(
                        "receipt envelope is not well-shaped: {shape_refusal}"
                    ))
                })?;
        Ok(Receipt {
            envelope,
            canon_nquads: canon_nquads.to_string(),
        })
    }

    /// Recomputes the digest of `canon_nquads` and compares it with the digest
    /// carried in the envelope.
    ///
    /// Refuses with [`Refusal::ValidationFailed`] when the recomputed digest
    /// differs — receipt identity is computed, never trusted.
    ///
    /// # Complexity
    /// O(bytes) of the canonical material (one BLAKE3 pass).
    pub fn verify(&self) -> Result<(), Refusal> {
        let recomputed = blake3_hex(self.canon_nquads.as_bytes());
        if self.envelope.digest.as_inner() != recomputed {
            return Err(Refusal::ValidationFailed(format!(
                "receipt digest mismatch for subject {:?}: carried {} but canonical \
                 material recomputes to {recomputed}",
                self.envelope.subject,
                self.envelope.digest.as_inner()
            )));
        }
        Ok(())
    }
}

/// The chatman refusal taxonomy. Every variant is a binding contract with a
/// `String` context payload naming the concrete offender; no catch-all
/// variant exists.
#[derive(Debug, Clone, PartialEq, Eq, Hash, Serialize, Deserialize, thiserror::Error)]
pub enum Refusal {
    /// Input failed structural or canonical-form validation.
    #[error("validation failed: {0}")]
    ValidationFailed(String),
    /// The requested plan cannot be realized over the admitted inputs.
    #[error("plan infeasible: {0}")]
    PlanInfeasible(String),
    /// An execution trace violates the governing law.
    #[error("trace unlawful: {0}")]
    TraceUnlawful(String),
    /// A hook is not permitted under the active profile or admission table.
    #[error("hook unpermitted: {0}")]
    HookUnpermitted(String),
    /// A required receipt is absent or not well-shaped.
    #[error("missing receipt: {0}")]
    MissingReceipt(String),
    /// The referenced graph snapshot does not exist.
    #[error("snapshot not found: {0}")]
    SnapshotNotFound(String),
    /// A boundary request crossed without carrying its receipt.
    #[error("boundary request missing receipt: {0}")]
    BoundaryRequestMissingReceipt(String),
    /// The Triple8 universe exceeded its bounded capacity.
    #[error("triple8 universe overflow: {0}")]
    Triple8UniverseOverflow(String),
    /// A term was referenced that is not in the Triple8 universe.
    #[error("term not in triple8 universe: {0}")]
    TermNotInTriple8Universe(String),
    /// The profile's symbol table does not match the snapshot's symbol table.
    #[error("profile symbol table mismatch: {0}")]
    ProfileSymbolTableMismatch(String),
    /// A projection's recomputed hash differs from its carried hash.
    #[error("projection hash mismatch: {0}")]
    ProjectionHashMismatch(String),
    /// The operation requires the warm path; the cold path was requested.
    #[error("warm path required: {0}")]
    WarmPathRequired(String),
    /// The admission table's recomputed identity differs from the carried one.
    #[error("admission table mismatch: {0}")]
    AdmissionTableMismatch(String),
    /// A hook pattern is absent from the admission table.
    #[error("hook pattern not admitted: {0}")]
    HookPatternNotAdmitted(String),
    /// An OCEL event type is absent from the admission table.
    #[error("OCEL event not admitted: {0}")]
    OcelEventNotAdmitted(String),
    /// A route chose a more expressive dialect than the least expressive
    /// dialect sufficient for the request.
    #[error("least-expressive route violation: {0}")]
    LeastExpressiveRouteViolation(String),
    /// The requested dialect is not supported by this engine.
    #[error("unsupported dialect: {0}")]
    UnsupportedDialect(String),
    /// N3 is not available under the active profile.
    #[error("N3 unavailable by profile: {0}")]
    N3UnavailableByProfile(String),
    /// An N3 rule attempted actuation (side effects), which is refused.
    #[error("N3 actuation refused: {0}")]
    N3ActuationRefused(String),
    /// An admitted N3 execution's cumulative declared cost exceeded its
    /// profile's declared cost bound (PROJ-779,
    /// `chatman::router::N3Executor`). Reserves the PRD §18
    /// `N3_COST_BOUND_EXCEEDED` catalog name; full N3/measurement typed-
    /// refusal-catalog wiring (this crate's `ALL_REFUSAL_NAMES` and the
    /// acceptance schemas) is PROJ-787's scope, not added here.
    #[error("N3 cost bound exceeded: {0}")]
    N3CostBoundExceeded(String),
    /// An N3 rule referenced a builtin outside its profile's N3 execution
    /// builtin whitelist (PROJ-779, `chatman::router::N3Executor`). Reserves
    /// the PRD §18 `N3_BUILTIN_REFUSED` catalog name; full N3/measurement
    /// typed-refusal-catalog wiring is PROJ-787's scope, not added here.
    #[error("N3 builtin refused: {0}")]
    N3BuiltinRefused(String),
    /// An N3 rule referenced a recognized actuation-triggering builtin (I/O,
    /// network, or process dispatch — e.g. `log:webOperation`), refused
    /// unconditionally regardless of any profile's N3 execution whitelist
    /// mask (PROJ-780, PRD.md §12 "N3 Quarantine": "An N3 rule that requests
    /// direct actuation SHALL be refused", `PRD.md:608`). Distinct from
    /// [`Refusal::N3ActuationRefused`] (the `ProfileGates`/`DialectRouter`
    /// level refusal for a *query shape* declaring `wants_actuation` while
    /// requiring N3) and from [`Refusal::N3BuiltinRefused`] (a pure builtin
    /// simply outside the declared whitelist mask, which a profile *could*
    /// admit by widening the mask) — a direct-actuation builtin can never be
    /// admitted by any mask, at any executor. Wired at
    /// `chatman::router::N3Executor::run`. Reserves the PRD §18
    /// `N3_DIRECT_ACTUATION_REFUSED` catalog name; full N3/measurement
    /// typed-refusal-catalog wiring is PROJ-787's scope, not added here.
    #[error("N3 direct actuation refused: {0}")]
    N3DirectActuationRefused(String),
    /// A replayed route decision differs from the recorded decision.
    #[error("route decision mismatch: {0}")]
    RouteDecisionMismatch(String),
    /// The invocation's snapshot identity differs from the engine's snapshot.
    #[error("graph snapshot mismatch: {0}")]
    GraphSnapshotMismatch(String),
    /// The profile's recomputed hash differs from its carried hash.
    #[error("profile hash mismatch: {0}")]
    ProfileHashMismatch(String),
    /// An agent attempted to override an operator decision without authority.
    #[error("agent override denied: {0}")]
    AgentOverrideDenied(String),
    /// A witness was presented as authority; witnesses attest, they do not
    /// authorize.
    #[error("witness is not authority: {0}")]
    WitnessNotAuthority(String),
    /// A breed (agent lineage) operation is not permitted for this operator.
    #[error("breed unpermitted: {0}")]
    BreedUnpermitted(String),
    /// A nondeterministic operator was invoked without a covering receipt.
    #[error("nondeterministic operator requires receipt: {0}")]
    NondeterministicOperatorRequiresReceipt(String),
    /// A local type shadows the canonical compat `ProcessReceipt`.
    #[error("process receipt shadow type: {0}")]
    ProcessReceiptShadowType(String),
    /// A canonical tape type was defined more than once in the crate.
    #[error("duplicate canonical tape type: {0}")]
    DuplicateCanonicalTapeType(String),
    /// An RDF 1.2 triple term appeared inside a snapshot, which the Triple8
    /// universe does not admit.
    #[error("triple term in snapshot: {0}")]
    TripleTermInSnapshot(String),
    /// A stage's re-computed digest at entry to the next pipeline stage does
    /// not match the seal threaded from the prior stage — distinct from the
    /// final receipt-root mismatch; this is a per-transition integrity
    /// failure, not an end-of-run one.
    #[error("stage seal mismatch: {0}")]
    StageSealMismatch(String),
    /// Actuation was attempted at the machine-state boundary without a
    /// receipt-verified, sealed admitted transition — the general case,
    /// distinct from [`Refusal::N3ActuationRefused`] (N3-specific) and
    /// [`Refusal::BoundaryRequestMissingReceipt`] (boundary-specific).
    #[error("unlawful actuation: {0}")]
    UnlawfulActuation(String),
    /// PROJ-783 (PRD.md v26.7.11 sec.18, `POWL_REGION_NOT_ADMITTED`): a
    /// declared `powl2_decompose::Powl::ExternalCut`'s inner region fails
    /// admission — an empty composite (`PartialOrder`/`Choice` with zero
    /// children) or a further-nested external cut inside the region, per
    /// `powl2_decompose::validate_external_cut`'s own
    /// `ExternalCutRefusal::PowlRegionNotAdmitted`. Wired at
    /// `chatman::powl_projection::admit_powl_model`.
    #[error("POWL_REGION_NOT_ADMITTED: {0}")]
    PowlRegionNotAdmitted(String),
    /// PROJ-783 (PRD.md v26.7.11 sec.18, `EXTERNAL_CUT_UNDECLARED`): a
    /// declared `powl2_decompose::Powl::ExternalCut` carries an empty SPARQL
    /// projection or Tera renderer string — the cut names no real projection
    /// authority to dispatch to. Wired at
    /// `chatman::powl_projection::admit_powl_model`.
    #[error("EXTERNAL_CUT_UNDECLARED: {0}")]
    ExternalCutUndeclared(String),
    /// PROJ-783 (PRD.md v26.7.11 sec.18, `EXTERNAL_CUT_TYPE_MISMATCH`): a
    /// caller-declared structural address (a `powl2_decompose::SocketPath`)
    /// claimed to name a `powl2_decompose::Powl::ExternalCut` resolves to a
    /// different POWL variant, or to no node at all. Wired at
    /// `chatman::powl_projection::resolve_external_cut_at`.
    #[error("EXTERNAL_CUT_TYPE_MISMATCH: {0}")]
    ExternalCutTypeMismatch(String),
    /// PROJ-783 (PRD.md v26.7.11 sec.18, `EXTERNAL_CUT_AUTHORITY_MISMATCH`):
    /// a POWL model that declares an external cut names a `derived_from`
    /// provenance IRI whose RFC 3986-style authority component differs from
    /// the region's own `base_iri` authority — a manufactured artifact
    /// cannot claim provenance from an authority other than the one
    /// producing it (PRD.md sec.7.1: "No runtime layer SHALL infer ambient
    /// authority from syntax"). Wired at
    /// `chatman::powl_projection::powl_to_turtle`.
    #[error("EXTERNAL_CUT_AUTHORITY_MISMATCH: {0}")]
    ExternalCutAuthorityMismatch(String),
    /// PROJ-759 (PRD.md v26.7.11 sec.9, closure-law foundation): a recursive
    /// socket's declared closure law was evaluated against zero declared
    /// children (`powl2_decompose::ParentChildClosure::children_of` returned
    /// an empty set) — a recursive socket with no children cannot
    /// meaningfully declare a closure law. Wired at
    /// `chatman::closure::RecursiveSocketClosure::declare`.
    #[error("closure law declares zero children: {0}")]
    ClosureLawNoChildren(String),
    /// PROJ-759 (PRD.md v26.7.11 sec.9): a declared `quorum(q)` closure
    /// law's `q` is zero or exceeds the declared child count — `Close(W)
    /// iff |{c in C(W): TerminalAdmitted(c)}| >= q` is vacuous or
    /// unreachable for such `q`. Wired at
    /// `chatman::closure::RecursiveSocketClosure::declare`.
    #[error("closure law quorum out of range: {0}")]
    ClosureLawQuorumOutOfRange(String),
    /// PROJ-759 (PRD.md v26.7.11 sec.9): a completion/admission signal
    /// (`observe`/`admit`/`require_terminal_admitted`) cited a
    /// `powl2_decompose::WorkflowSocketId` that is not one of the recursive
    /// socket's declared children. Wired at
    /// `chatman::closure::RecursiveSocketClosure`.
    #[error("closure law unknown child: {0}")]
    ClosureLawUnknownChild(String),
    /// PROJ-773 (PRD.md v26.7.11 sec.9): a declared `ordered_subset`
    /// closure law's required child sequence is empty, contains a
    /// duplicate entry, or names a `powl2_decompose::WorkflowSocketId`
    /// that is not one of the recursive socket's own direct declared
    /// children — `Close(W) iff forall c in S, TerminalAdmitted(c)` is
    /// undefined/vacuous for such an `S`. Wired at
    /// `chatman::closure::RecursiveSocketClosure::declare`.
    #[error("closure law ordered_subset invalid: {0}")]
    ClosureLawOrderedSubsetInvalid(String),
    /// PROJ-773 (PRD.md v26.7.11 sec.9): `record_policy_decision` was
    /// called on a recursive socket whose declared law is not
    /// `policy_decides` — a policy verdict has no closure-law meaning
    /// outside that declared law. Wired at
    /// `chatman::closure::RecursiveSocketClosure::record_policy_decision`.
    #[error("closure law policy not declared: {0}")]
    ClosureLawPolicyNotDeclared(String),
    /// PROJ-774 (PRD.md v26.7.11 sec.9 line 525, the admission gate that
    /// actually promotes `Observed` to `Admitted`): a child's completion
    /// signal was submitted for promotion together with real SHACL
    /// conformance evidence (`crate::shacl::ValidationReport`) whose
    /// `.conforms` is `false` — the child stays `Observed`, never silently
    /// promoted on failing evidence. Wired at
    /// `chatman::closure::RecursiveSocketClosure::promote_observed_to_admitted`.
    #[error("CHILD_CONFORMANCE_REFUSED: {0}")]
    ChildConformanceRefused(String),
    /// PROJ-759 (PRD.md v26.7.11 sec.9 line 525; reserves the PRD §18
    /// `CHILD_COMPLETION_UNADMITTED` catalog name): "A child completion
    /// signal SHALL be treated as observation until admitted" — a caller
    /// asked whether a specific child is `TerminalAdmitted` and found it
    /// merely `Observed` or still `Open`. Distinct from
    /// [`Refusal::ClosureLawUnknownChild`] (the child isn't declared at
    /// all). Wired at
    /// `chatman::closure::RecursiveSocketClosure::require_terminal_admitted`.
    /// Full PRD §18 typed-refusal-catalog wiring (`ALL_REFUSAL_NAMES`, the
    /// acceptance schemas) is PROJ-786's scope, following the same
    /// PROJ-779/780/783 precedent below — not added there yet.
    #[error("CHILD_COMPLETION_UNADMITTED: {0}")]
    ChildCompletionUnadmitted(String),
    /// PROJ-759 (PRD.md v26.7.11 sec.9; reserves the PRD §18
    /// `PARENT_CLOSURE_UNSATISFIED` catalog name): an attempt to close a
    /// recursive socket's parent workflow found its declared closure law
    /// not yet satisfied (PRD §19.4/§19.5: an unsatisfied closure law
    /// leaves the parent open, it does not silently proceed). Wired at
    /// `chatman::closure::RecursiveSocketClosure::close`. Full PRD §18
    /// typed-refusal-catalog wiring is PROJ-786's scope, same precedent as
    /// [`Refusal::ChildCompletionUnadmitted`] above.
    #[error("PARENT_CLOSURE_UNSATISFIED: {0}")]
    ParentClosureUnsatisfied(String),
}

/// Every [`Refusal`] name, in declaration order. This is the cross-lane
/// contract mirrored by the `expected_refusal` enum in each acceptance
/// schema; `tests/chatman_static_gates.rs` asserts set-equality.
///
/// Catalog-complete (PROJ-786/787): all 46 [`Refusal`] variants appear here,
/// in the same order as the enum declaration above. This closes three
/// previously-deferred gaps, each tracked by its own NOTE in prior
/// revisions of this comment:
///
/// - PROJ-779/780: [`Refusal::N3CostBoundExceeded`],
///   [`Refusal::N3BuiltinRefused`], [`Refusal::N3DirectActuationRefused`]
///   (real, constructed in `chatman::router::N3Executor`).
/// - PROJ-783: [`Refusal::PowlRegionNotAdmitted`],
///   [`Refusal::ExternalCutUndeclared`],
///   [`Refusal::ExternalCutTypeMismatch`],
///   [`Refusal::ExternalCutAuthorityMismatch`] (real, constructed in
///   `chatman::powl_projection::admit_powl_model`/`resolve_external_cut_at`/
///   `powl_to_turtle`).
/// - PROJ-759/772/773/774: [`Refusal::ClosureLawNoChildren`],
///   [`Refusal::ClosureLawQuorumOutOfRange`],
///   [`Refusal::ClosureLawUnknownChild`],
///   [`Refusal::ClosureLawOrderedSubsetInvalid`],
///   [`Refusal::ClosureLawPolicyNotDeclared`],
///   [`Refusal::ChildConformanceRefused`],
///   [`Refusal::ChildCompletionUnadmitted`],
///   [`Refusal::ParentClosureUnsatisfied`] (real, constructed in
///   `chatman::closure::RecursiveSocketClosure`).
///
/// All 15 were already real, constructed, end-to-end-tested variants before
/// this change; only the catalog-completeness bookkeeping (this array, the
/// 8 acceptance schemas' `expected_refusal.enum`, and
/// `gate_refusal_name_matches_const_list`'s hand-built variant list) was
/// behind. `engine.rs::ReplayMismatch` is a separate, intentionally
/// uncataloged type (replay-tamper detection, not an admission-time
/// [`Refusal`]) and is out of scope for this array by design.
pub const ALL_REFUSAL_NAMES: [&str; 46] = [
    "ValidationFailed",
    "PlanInfeasible",
    "TraceUnlawful",
    "HookUnpermitted",
    "MissingReceipt",
    "SnapshotNotFound",
    "BoundaryRequestMissingReceipt",
    "Triple8UniverseOverflow",
    "TermNotInTriple8Universe",
    "ProfileSymbolTableMismatch",
    "ProjectionHashMismatch",
    "WarmPathRequired",
    "AdmissionTableMismatch",
    "HookPatternNotAdmitted",
    "OcelEventNotAdmitted",
    "LeastExpressiveRouteViolation",
    "UnsupportedDialect",
    "N3UnavailableByProfile",
    "N3ActuationRefused",
    "N3CostBoundExceeded",
    "N3BuiltinRefused",
    "N3DirectActuationRefused",
    "RouteDecisionMismatch",
    "GraphSnapshotMismatch",
    "ProfileHashMismatch",
    "AgentOverrideDenied",
    "WitnessNotAuthority",
    "BreedUnpermitted",
    "NondeterministicOperatorRequiresReceipt",
    "ProcessReceiptShadowType",
    "DuplicateCanonicalTapeType",
    "TripleTermInSnapshot",
    "StageSealMismatch",
    "UnlawfulActuation",
    "PowlRegionNotAdmitted",
    "ExternalCutUndeclared",
    "ExternalCutTypeMismatch",
    "ExternalCutAuthorityMismatch",
    "ClosureLawNoChildren",
    "ClosureLawQuorumOutOfRange",
    "ClosureLawUnknownChild",
    "ClosureLawOrderedSubsetInvalid",
    "ClosureLawPolicyNotDeclared",
    "ChildConformanceRefused",
    "ChildCompletionUnadmitted",
    "ParentClosureUnsatisfied",
];

impl Refusal {
    /// The variant name as a static string, matching the `expected_refusal`
    /// enum values in the acceptance schemas.
    ///
    /// The exhaustive match keeps this in sync with the enum: adding a
    /// variant without extending both this match and [`ALL_REFUSAL_NAMES`]
    /// is a compile error or a gate-test failure respectively.
    pub fn name(&self) -> &'static str {
        match self {
            Refusal::ValidationFailed(_) => "ValidationFailed",
            Refusal::PlanInfeasible(_) => "PlanInfeasible",
            Refusal::TraceUnlawful(_) => "TraceUnlawful",
            Refusal::HookUnpermitted(_) => "HookUnpermitted",
            Refusal::MissingReceipt(_) => "MissingReceipt",
            Refusal::SnapshotNotFound(_) => "SnapshotNotFound",
            Refusal::BoundaryRequestMissingReceipt(_) => "BoundaryRequestMissingReceipt",
            Refusal::Triple8UniverseOverflow(_) => "Triple8UniverseOverflow",
            Refusal::TermNotInTriple8Universe(_) => "TermNotInTriple8Universe",
            Refusal::ProfileSymbolTableMismatch(_) => "ProfileSymbolTableMismatch",
            Refusal::ProjectionHashMismatch(_) => "ProjectionHashMismatch",
            Refusal::WarmPathRequired(_) => "WarmPathRequired",
            Refusal::AdmissionTableMismatch(_) => "AdmissionTableMismatch",
            Refusal::HookPatternNotAdmitted(_) => "HookPatternNotAdmitted",
            Refusal::OcelEventNotAdmitted(_) => "OcelEventNotAdmitted",
            Refusal::LeastExpressiveRouteViolation(_) => "LeastExpressiveRouteViolation",
            Refusal::UnsupportedDialect(_) => "UnsupportedDialect",
            Refusal::N3UnavailableByProfile(_) => "N3UnavailableByProfile",
            Refusal::N3ActuationRefused(_) => "N3ActuationRefused",
            Refusal::N3CostBoundExceeded(_) => "N3CostBoundExceeded",
            Refusal::N3BuiltinRefused(_) => "N3BuiltinRefused",
            Refusal::N3DirectActuationRefused(_) => "N3DirectActuationRefused",
            Refusal::RouteDecisionMismatch(_) => "RouteDecisionMismatch",
            Refusal::GraphSnapshotMismatch(_) => "GraphSnapshotMismatch",
            Refusal::ProfileHashMismatch(_) => "ProfileHashMismatch",
            Refusal::AgentOverrideDenied(_) => "AgentOverrideDenied",
            Refusal::WitnessNotAuthority(_) => "WitnessNotAuthority",
            Refusal::BreedUnpermitted(_) => "BreedUnpermitted",
            Refusal::NondeterministicOperatorRequiresReceipt(_) => {
                "NondeterministicOperatorRequiresReceipt"
            }
            Refusal::ProcessReceiptShadowType(_) => "ProcessReceiptShadowType",
            Refusal::DuplicateCanonicalTapeType(_) => "DuplicateCanonicalTapeType",
            Refusal::TripleTermInSnapshot(_) => "TripleTermInSnapshot",
            Refusal::StageSealMismatch(_) => "StageSealMismatch",
            Refusal::UnlawfulActuation(_) => "UnlawfulActuation",
            Refusal::PowlRegionNotAdmitted(_) => "PowlRegionNotAdmitted",
            Refusal::ExternalCutUndeclared(_) => "ExternalCutUndeclared",
            Refusal::ExternalCutTypeMismatch(_) => "ExternalCutTypeMismatch",
            Refusal::ExternalCutAuthorityMismatch(_) => "ExternalCutAuthorityMismatch",
            Refusal::ClosureLawNoChildren(_) => "ClosureLawNoChildren",
            Refusal::ClosureLawQuorumOutOfRange(_) => "ClosureLawQuorumOutOfRange",
            Refusal::ClosureLawUnknownChild(_) => "ClosureLawUnknownChild",
            Refusal::ClosureLawOrderedSubsetInvalid(_) => "ClosureLawOrderedSubsetInvalid",
            Refusal::ClosureLawPolicyNotDeclared(_) => "ClosureLawPolicyNotDeclared",
            Refusal::ChildConformanceRefused(_) => "ChildConformanceRefused",
            Refusal::ChildCompletionUnadmitted(_) => "ChildCompletionUnadmitted",
            Refusal::ParentClosureUnsatisfied(_) => "ParentClosureUnsatisfied",
        }
    }
}

#[cfg(test)]
#[path = "abi_test.rs"]
mod refusal_variant_tests;
