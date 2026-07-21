//! Core error types for law object lifecycle.

use thiserror::Error;

use crate::law::Obligation;

/// Errors that can occur during law object operations.
#[derive(Debug, Error, Clone)]
pub enum CoreError {
    /// One or more obligations were not met during judgment.
    #[error("obligations unmet: {0:?}")]
    ObligationUnmet(Vec<Obligation>),

    /// Signature verification failed.
    #[error("signature invalid")]
    SignatureInvalid,

    /// Chain hash verification failed (prev hash mismatch).
    #[error("chain hash mismatch")]
    ChainMismatch,

    /// Payload could not be serialized to canonical bytes for hashing.
    #[error("payload serialization failed: {0}")]
    SerializationFailed(String),

    /// Ed25519 signing failed (missing/invalid key, or the underlying
    /// signing primitive returned an error).
    #[error("signing failed: {0}")]
    SigningFailed(String),

    /// A hex-encoded field (payload/chain hash) failed to decode: wrong
    /// length, invalid hex characters, or not exactly 32 bytes.
    #[error("hex decode failed: {0}")]
    HexDecodeFailed(String),

    /// Filesystem I/O failed while reading or writing the receipt ledger.
    #[error("io error: {0}")]
    Io(String),

    /// PROJ-752 (PRD.md sec.7.4-7.5, `A_z = T(Q(W))`): the Tera template
    /// engine refused to parse or render the Rail A Arazzo-manufacture
    /// template. Carries Tera's own error text (formatted with `{e:?}` at
    /// the call site so the underlying cause -- e.g. a malformed template
    /// expression or a JSON-encoding failure on a projected value -- is not
    /// silently dropped).
    #[error("arazzo template render failed: {0}")]
    TemplateRenderFailed(String),

    /// PROJ-752: a Q-stage `ProjectionRow` referenced a `childModel`/region
    /// element IRI that does not appear as the `elementId` of any row in
    /// the same projection result set -- an internal-consistency violation
    /// of the row set being rendered. PROJ-751's own tests cover round-trip
    /// consistency of the real `run_render_model_projection` output; this
    /// variant exists so a malformed/hand-built row set fails loud here
    /// too, rather than silently manufacturing an incomplete Arazzo
    /// document.
    #[error("arazzo projection: unresolved projected element {0}")]
    UnresolvedProjectionElement(String),

    /// PROJ-796: the Rail A/B production pipeline
    /// (`ArazzoProjectionReceipt::project_and_compile` /
    /// `ChatmanRailAbCompiler`) refused during POWL admission, SPARQL Q(W)
    /// projection, Arazzo parsing, URI resolution, AIR lowering,
    /// normalization, or WASM compilation. `stage` names which of those
    /// sub-stages produced the failure; `detail` carries the underlying
    /// error's own formatted text verbatim (never summarized or dropped),
    /// so the failure is attributable without inventing a second taxonomy
    /// on top of `praxis_graphlaw::chatman::abi::Refusal` and
    /// `wasm4pm_arazzo::Refusal`. Named `detail`, not `source`: `thiserror`
    /// treats a field literally named `source` as `std::error::Error::
    /// source()` and requires it to implement `std::error::Error`, which a
    /// plain `String` does not.
    #[error("external cut compilation failed at {stage}: {detail}")]
    ExternalCutCompilationFailed {
        /// The pipeline sub-stage that refused (e.g. `"admission"`,
        /// `"sparql_projection"`, `"arazzo_parse"`, `"uri_resolution"`,
        /// `"air_lowering"`, `"air_normalization"`, `"air_compile"`).
        stage: String,
        /// The underlying error's formatted text.
        detail: String,
    },

    /// PROJ-783 (PRD.md v26.7.11 sec.7.5/18/19.3, `ARAZZO_UNMANUFACTURED`):
    /// a production Arazzo document was presented with no projection
    /// receipt at all -- PRD.md sec.5's non-goal "accept arbitrary Arazzo
    /// as production workflow authority" and sec.7.5's "Production Arazzo
    /// without an admitted POWL source and projection receipt SHALL be
    /// refused" made concrete (acceptance scenario 19.3, "Handwritten
    /// Arazzo Is Refused"). Wired at
    /// Was wired at `crate::arazzo::admit_manufactured_arazzo` (module removed 2026-07-17 with the ~/praxis path-dependency cleanup -- this variant is now unreachable, kept as a documented catalog entry).
    #[error("ARAZZO_UNMANUFACTURED: {0}")]
    ArazzoUnmanufactured(String),

    /// PROJ-783 (PRD.md v26.7.11 sec.18, `ARAZZO_SOURCE_RECEIPT_MISSING`): a
    /// projection receipt was presented, but it does not actually bind to
    /// an admitted POWL source (`source_powl_digest_hex`/
    /// `external_cut_identity` empty) -- a receipt shape that attests no
    /// real source material. Wired at
    /// Was wired at `crate::arazzo::admit_manufactured_arazzo` (module removed 2026-07-17 with the ~/praxis path-dependency cleanup -- this variant is now unreachable, kept as a documented catalog entry).
    #[error("ARAZZO_SOURCE_RECEIPT_MISSING: {0}")]
    ArazzoSourceReceiptMissing(String),

    /// PROJ-783 (PRD.md v26.7.11 sec.18, `ARAZZO_PROJECTION_DIGEST_MISMATCH`):
    /// a projection receipt's own `arazzo_digest_hex` does not equal the
    /// real BLAKE3 digest of the presented Arazzo document's bytes -- the
    /// receipt attests different material than what is actually being
    /// presented (a stale, substituted, or hand-edited document wearing a
    /// real receipt). Wired at
    /// Was wired at `crate::arazzo::admit_manufactured_arazzo` (module removed 2026-07-17 with the ~/praxis path-dependency cleanup -- this variant is now unreachable, kept as a documented catalog entry).
    #[error("ARAZZO_PROJECTION_DIGEST_MISMATCH: {0}")]
    ArazzoProjectionDigestMismatch(String),

    /// PROJ-777/778 (`docs/jira/v26.7.11/PRD.md:588`): a document was
    /// presented for admission under a dialect name that either is not
    /// registered in [`crate::graphlaw_authority::REGISTRY`] at all, or is
    /// registered under a *different* dialect's authority than the one the
    /// admission gate enforces (e.g. declaring `"SPARQL CONSTRUCT"` --
    /// which also carries "manufacture graph consequence" authority -- to
    /// route content through the Arazzo manufacture gate). Not a PRD
    /// section-18 catalog code; this crate's own typed refusal for the
    /// dialect-name check at
    /// Was wired at `crate::arazzo::admit_manufactured_arazzo_for_dialect` (module removed 2026-07-17 with the ~/praxis path-dependency cleanup -- this variant is now unreachable, kept as a documented catalog entry).
    #[error(
        "dialect authority mismatch: declared dialect {declared:?} does not hold {expected:?}'s \
         authority ({reason})"
    )]
    ArazzoDialectAuthorityMismatch {
        /// The dialect name the caller declared.
        declared: String,
        /// The dialect name whose authority the gate actually requires.
        expected: &'static str,
        /// Why the declared name failed to resolve to `expected`'s authority.
        reason: String,
    },

    /// Receipt-epoch migration (`crate::receipt_epoch`): a [`crate::receipt_record::ReceiptRecord`]
    /// declared a `schema` string this reader does not recognize as either
    /// [`crate::receipt_epoch::SCHEMA_V1`] or [`crate::receipt_epoch::SCHEMA_V2`] --
    /// most likely a future schema this build predates. Refused rather than
    /// silently treated as legacy or v2.
    #[error("unrecognized receipt schema: {0}")]
    UnrecognizedReceiptSchema(String),

    /// Receipt-epoch migration: the record's declared `schema` string
    /// contradicts whether its `v2` payload is present (schema says v1 but a
    /// `v2` payload exists, or schema says v2 but no `v2` payload is
    /// present) -- a receipt that lies about its own shape.
    #[error("receipt schema/payload mismatch: {0}")]
    ReceiptSchemaPayloadMismatch(String),

    /// Receipt-epoch ceiling monotonicity
    /// (`crate::receipt_epoch::ReceiptEpochV2Builder::build`): an explicit
    /// ceiling override was requested that exceeds
    /// `meet(recoverable(prev), supported(new evidence))` -- refused rather
    /// than silently accepted, since a receipt's ceiling must never claim
    /// more confidence than the evidence chain actually supports.
    #[error("ceiling {requested:?} exceeds allowed meet {allowed:?}")]
    CeilingExceedsMeet {
        /// The ceiling the caller tried to force.
        requested: crate::receipt_epoch::CeilingLevel,
        /// The highest ceiling `meet(recoverable(prev), supported(new))` allows.
        allowed: crate::receipt_epoch::CeilingLevel,
    },

    /// [`crate::receipt_epoch::validate_promotion`]: a `PromotionWitness`
    /// failed one of the promotion invariants (stale `from_ceiling`, not
    /// actually a raise, missing evidence/closed-obligation citations, empty
    /// authorization basis, or a self-authored verifier identity). Refused
    /// rather than silently ignored -- `recoverable(prev)` is a permanent
    /// one-way floor by design (see that fn's doc comment), so a promotion
    /// that is accepted incorrectly can never be walked back by ordinary
    /// generation.
    #[error("promotion refused: {reason}")]
    PromotionRefused {
        /// Which invariant the witness violated.
        reason: String,
    },
}

/// Every [`CoreError`] name, in declaration order. Mirrors
/// `praxis_graphlaw::chatman::abi::ALL_REFUSAL_NAMES`'s catalog-completeness
/// pattern: this array plus [`CoreError::name`]'s exhaustive match are the
/// enforcement pair. Adding an enum variant without adding a match arm to
/// `name()` is a compile error (non-exhaustive match); adding a match arm
/// without extending this array is caught at test time by
/// `tests::all_core_error_names_matches_enum` below, which builds one
/// instance of every variant and zip-checks `name()` against this array in
/// order.
pub const ALL_CORE_ERROR_NAMES: [&str; 18] = [
    "ObligationUnmet",
    "SignatureInvalid",
    "ChainMismatch",
    "SerializationFailed",
    "SigningFailed",
    "HexDecodeFailed",
    "Io",
    "TemplateRenderFailed",
    "UnresolvedProjectionElement",
    "ExternalCutCompilationFailed",
    "ArazzoUnmanufactured",
    "ArazzoSourceReceiptMissing",
    "ArazzoProjectionDigestMismatch",
    "ArazzoDialectAuthorityMismatch",
    "UnrecognizedReceiptSchema",
    "ReceiptSchemaPayloadMismatch",
    "CeilingExceedsMeet",
    "PromotionRefused",
];

impl CoreError {
    /// The variant name as a static string, matching [`ALL_CORE_ERROR_NAMES`].
    ///
    /// The exhaustive match keeps this in sync with the enum: adding a
    /// variant without extending both this match and
    /// [`ALL_CORE_ERROR_NAMES`] is a compile error or a test failure
    /// respectively (see `tests::all_core_error_names_matches_enum`).
    pub fn name(&self) -> &'static str {
        match self {
            CoreError::ObligationUnmet(_) => "ObligationUnmet",
            CoreError::SignatureInvalid => "SignatureInvalid",
            CoreError::ChainMismatch => "ChainMismatch",
            CoreError::SerializationFailed(_) => "SerializationFailed",
            CoreError::SigningFailed(_) => "SigningFailed",
            CoreError::HexDecodeFailed(_) => "HexDecodeFailed",
            CoreError::Io(_) => "Io",
            CoreError::TemplateRenderFailed(_) => "TemplateRenderFailed",
            CoreError::UnresolvedProjectionElement(_) => "UnresolvedProjectionElement",
            CoreError::ExternalCutCompilationFailed { .. } => "ExternalCutCompilationFailed",
            CoreError::ArazzoUnmanufactured(_) => "ArazzoUnmanufactured",
            CoreError::ArazzoSourceReceiptMissing(_) => "ArazzoSourceReceiptMissing",
            CoreError::ArazzoProjectionDigestMismatch(_) => "ArazzoProjectionDigestMismatch",
            CoreError::ArazzoDialectAuthorityMismatch { .. } => "ArazzoDialectAuthorityMismatch",
            CoreError::UnrecognizedReceiptSchema(_) => "UnrecognizedReceiptSchema",
            CoreError::ReceiptSchemaPayloadMismatch(_) => "ReceiptSchemaPayloadMismatch",
            CoreError::CeilingExceedsMeet { .. } => "CeilingExceedsMeet",
            CoreError::PromotionRefused { .. } => "PromotionRefused",
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::collections::BTreeSet;

    /// `ALL_CORE_ERROR_NAMES`'s declared length must match its actual
    /// element count (catches a hand-edit that changes the array body
    /// without updating the `[&str; N]` size annotation, which would
    /// otherwise be a compile error anyway -- kept as a same-shape
    /// companion to `chatman_static_gates.rs::gate_schema_refusal_enums_match_abi`).
    #[test]
    fn all_core_error_names_len_matches_array() {
        assert_eq!(
            ALL_CORE_ERROR_NAMES.len(),
            18,
            "ALL_CORE_ERROR_NAMES size drifted"
        );
    }

    /// No duplicate names in the catalog.
    #[test]
    fn all_core_error_names_has_no_duplicates() {
        let as_set: BTreeSet<&str> = ALL_CORE_ERROR_NAMES.iter().copied().collect();
        assert_eq!(
            as_set.len(),
            ALL_CORE_ERROR_NAMES.len(),
            "ALL_CORE_ERROR_NAMES must contain no duplicates"
        );
    }

    /// Construct one instance of every `CoreError` variant; `name()` of each
    /// must appear in `ALL_CORE_ERROR_NAMES` in declaration order. Mirrors
    /// `chatman_static_gates.rs::gate_refusal_name_matches_const_list`:
    /// adding a variant without updating this list fails here; removing one
    /// fails to compile at `CoreError::name`'s exhaustive match.
    #[test]
    fn all_core_error_names_matches_enum() {
        let all = vec![
            CoreError::ObligationUnmet(vec![Obligation::BlockingConstraint {
                reason: "gate".to_string(),
            }]),
            CoreError::SignatureInvalid,
            CoreError::ChainMismatch,
            CoreError::SerializationFailed("gate".to_string()),
            CoreError::SigningFailed("gate".to_string()),
            CoreError::HexDecodeFailed("gate".to_string()),
            CoreError::Io("gate".to_string()),
            CoreError::TemplateRenderFailed("gate".to_string()),
            CoreError::UnresolvedProjectionElement("gate".to_string()),
            CoreError::ExternalCutCompilationFailed {
                stage: "gate".to_string(),
                detail: "gate".to_string(),
            },
            CoreError::ArazzoUnmanufactured("gate".to_string()),
            CoreError::ArazzoSourceReceiptMissing("gate".to_string()),
            CoreError::ArazzoProjectionDigestMismatch("gate".to_string()),
            CoreError::ArazzoDialectAuthorityMismatch {
                declared: "gate".to_string(),
                expected: "gate",
                reason: "gate".to_string(),
            },
            CoreError::UnrecognizedReceiptSchema("gate".to_string()),
            CoreError::ReceiptSchemaPayloadMismatch("gate".to_string()),
            CoreError::CeilingExceedsMeet {
                requested: crate::receipt_epoch::CeilingLevel::Green,
                allowed: crate::receipt_epoch::CeilingLevel::Yellow,
            },
            CoreError::PromotionRefused {
                reason: "gate".to_string(),
            },
        ];
        assert_eq!(all.len(), ALL_CORE_ERROR_NAMES.len());
        for (err, expected) in all.iter().zip(ALL_CORE_ERROR_NAMES) {
            assert_eq!(err.name(), expected);
        }
    }
}
