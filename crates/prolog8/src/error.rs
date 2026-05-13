// GENERATED — DO NOT EDIT — source: schema/domain.ttl
use thiserror::Error;

/// Admission error: a rule, fact, or query failed the byte-governor invariant.
#[derive(Debug, Clone, PartialEq, Eq, Error)]
pub enum AdmissionError {
    /// Arity cap exceeded — Repair: Split predicate into entity plus attribute predicates.

    #[error("Arity cap exceeded")]
    EArityCapExceeded,

    /// Cut not admitted — Repair: Replace source-order control with explicit rule conditions.

    #[error("Cut not admitted")]
    ECutNotAdmitted,

    /// Dynamic mutation not admitted — Repair: Submit epoch delta and rebuild affected indexes.

    #[error("Dynamic mutation not admitted")]
    EDynamicMutationNotAdmitted,

    /// Foreign predicate contract missing — Repair: Declare determinism, replay policy, binding requirements, and cost.

    #[error("Foreign predicate contract missing")]
    EForeignContractMissing,

    /// Proof fan-in cap exceeded — Repair: Split proof into nested proof nodes.

    #[error("Proof fan-in cap exceeded")]
    EProofFaninExceeded,

    /// Rule body atom cap exceeded — Repair: Introduce intermediate derived predicates.

    #[error("Rule body atom cap exceeded")]
    ERuleBodyCapExceeded,

    /// Runtime parse rejected — Repair: Move parsing to compiler boundary.

    #[error("Runtime parse rejected")]
    ERuntimeParseRejected,

    /// Local state surface exceeds 256 — Repair: Split rule family into byte-capped surfaces.

    #[error("Local state surface exceeds 256")]
    EStateSurfaceExceeded,

    /// String query not admitted — Repair: Compile query into QueryAtom8 before invoking kernel.

    #[error("String query not admitted")]
    EStringQueryNotAdmitted,

    /// Uninterned term — Repair: Admit term into catalog and use TermId.

    #[error("Uninterned term")]
    EUninternedTerm,

    /// Unstratified negation — Repair: Split predicates into stratified rule layers.

    #[error("Unstratified negation")]
    EUnstratifiedNegation,

    /// Rule variable cap exceeded — Repair: Split rule around shared key or route condition.

    #[error("Rule variable cap exceeded")]
    EVariableCapExceeded,
}
