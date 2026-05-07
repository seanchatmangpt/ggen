// GENERATED — DO NOT EDIT — source: schema/domain.ttl
use thiserror::Error;

/// Admission error: a rule, fact, or query failed the byte-governor invariant.
#[derive(Debug, Clone, PartialEq, Eq, Error)]
pub enum AdmissionError {
    /// Arity cap exceeded — Repair: Split predicate into entity plus attribute predicates.

    #[error("Arity cap exceeded")]
    E_ARITY_CAP_EXCEEDED,

    /// Cut not admitted — Repair: Replace source-order control with explicit rule conditions.

    #[error("Cut not admitted")]
    E_CUT_NOT_ADMITTED,

    /// Dynamic mutation not admitted — Repair: Submit epoch delta and rebuild affected indexes.

    #[error("Dynamic mutation not admitted")]
    E_DYNAMIC_MUTATION_NOT_ADMITTED,

    /// Foreign predicate contract missing — Repair: Declare determinism, replay policy, binding requirements, and cost.

    #[error("Foreign predicate contract missing")]
    E_FOREIGN_CONTRACT_MISSING,

    /// Proof fan-in cap exceeded — Repair: Split proof into nested proof nodes.

    #[error("Proof fan-in cap exceeded")]
    E_PROOF_FANIN_EXCEEDED,

    /// Rule body atom cap exceeded — Repair: Introduce intermediate derived predicates.

    #[error("Rule body atom cap exceeded")]
    E_RULE_BODY_CAP_EXCEEDED,

    /// Runtime parse rejected — Repair: Move parsing to compiler boundary.

    #[error("Runtime parse rejected")]
    E_RUNTIME_PARSE_REJECTED,

    /// Local state surface exceeds 256 — Repair: Split rule family into byte-capped surfaces.

    #[error("Local state surface exceeds 256")]
    E_STATE_SURFACE_EXCEEDED,

    /// String query not admitted — Repair: Compile query into QueryAtom8 before invoking kernel.

    #[error("String query not admitted")]
    E_STRING_QUERY_NOT_ADMITTED,

    /// Uninterned term — Repair: Admit term into catalog and use TermId.

    #[error("Uninterned term")]
    E_UNINTERNED_TERM,

    /// Unstratified negation — Repair: Split predicates into stratified rule layers.

    #[error("Unstratified negation")]
    E_UNSTRATIFIED_NEGATION,

    /// Rule variable cap exceeded — Repair: Split rule around shared key or route condition.

    #[error("Rule variable cap exceeded")]
    E_VARIABLE_CAP_EXCEEDED,
}
