//! Vacuity-court compiler (wave-1 lane L11 / wave-2 lane A12).
//!
//! Compiles a `vc:` mutant-catalog graph (RDF/Turtle) into the three
//! shim-court artifacts the vacuity protocol consumes:
//!
//! * `runner.sh` — the 5-step shim protocol (sha256 baseline → before →
//!   shim → after → restore), every step sha-verified, any mismatch exits
//!   `COURT_BLOCKED` (2) *before* the mutation is allowed to matter;
//! * `register_row.sh` — the court register row with the
//!   `witness_ts >= shim_ts` KD-COVERS flip guard compiled in (a witness
//!   receipt predating its shim can never authorize a flip);
//! * `fixture.rs` — the catalog-identity fixture the ggen-cheat-scanner
//!   output gates (T01–T04) run over.
//!
//! Verdict vocabulary mirrors [`crate::replay_court`]:
//! [`CourtVerdict::Pass`] / [`CourtVerdict::Refused`] / [`CourtVerdict::Unknown`].
//! There is no silent pass: a court either produces PASS with evidence or a
//! typed non-PASS bound to the failing condition.
//!
//! # Typed process exits (fail-closed contract)
//!
//! | Exit | Meaning |
//! |------|---------|
//! | 0    | court passed (or artifacts compiled cleanly with a PASS verdict) |
//! | [`EXIT_VACUOUS`] (1) | VACUOUS GATE — no surviving mutant / missing or stale witness (`WITNESS_STALE` flip refused) |
//! | [`EXIT_COURT_BLOCKED`] (2) | `COURT_BLOCKED` — malformed catalog, sha mismatch, restore tamper, scanner findings |
//! | [`EXIT_SKIP`] (3) | loud SKIP — flip unverifiable (no timestamps), never silent |
//!
//! # cargo-mutants integration note
//!
//! Mutation sourcing (cargo-mutants where possible, `shim_patch` diffs
//! otherwise) is owned by the coordinator's justfile recipes: the engine
//! exposes only [`compile_court`] / [`catalog::read_mutant_catalog`] as the
//! library seam. No justfile recipe is added from this lane (coordinator
//! owns the justfile).

pub mod catalog;
pub mod compile;

pub use catalog::{parse_mutant_catalog, read_mutant_catalog, MutantCatalog};
pub use compile::{compile_court, compile_court_from_catalog_file, flip_verdict, CourtManifest};

/// Exit code: VACUOUS GATE (no surviving mutant / missing or stale witness).
pub const EXIT_VACUOUS: i32 = 1;
/// Exit code: `COURT_BLOCKED` (malformed catalog / sha mismatch / scanner findings).
pub const EXIT_COURT_BLOCKED: i32 = 2;
/// Exit code: loud SKIP (flip unverifiable — never silent).
pub const EXIT_SKIP: i32 = 3;

/// Typed error of the court compiler. Missing catalog fields are
/// [`CourtError::CatalogMalformed`] — never defaulted, never skipped.
#[derive(Debug)]
pub enum CourtError {
    /// The catalog graph is missing a required field, has ambiguous
    /// multiplicity, or cannot be parsed as a `vc:` mutant catalog.
    CatalogMalformed(String),
    /// Template rendering failed (a court template and its context
    /// diverged — always a compiler bug or a template edit, never input).
    TemplateRender(String),
    /// Filesystem I/O failure, with the offending path bound.
    Io {
        /// The path the I/O failed on.
        path: String,
        /// The underlying failure detail.
        detail: String,
    },
}

impl std::fmt::Display for CourtError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::CatalogMalformed(detail) => {
                write!(f, "COURT_BLOCKED: catalog malformed: {detail}")
            }
            Self::TemplateRender(detail) => {
                write!(f, "COURT_BLOCKED: template render failed: {detail}")
            }
            Self::Io { path, detail } => {
                write!(f, "COURT_BLOCKED: I/O failure at {path}: {detail}")
            }
        }
    }
}

impl std::error::Error for CourtError {}

/// Court result alias.
pub type CourtResult<T> = std::result::Result<T, CourtError>;

/// Typed verdict of a court, per the `replay_court.rs` vocabulary
/// (Pass / Refused / Unknown — never collapsed, never silent-pass).
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum CourtVerdict {
    /// Court artifacts compiled; the flip is authorized by a witness
    /// receipt at or after the shim's timestamp.
    Pass {
        /// Witness timestamp that authorized the flip (unix millis).
        witness_ts: u64,
        /// Shim timestamp the witness was ordered against (unix millis).
        shim_ts: u64,
    },
    /// Typed refusal — the court ran and the evidence failed. `code` is a
    /// registry refusal identity (`WITNESS_STALE`, `VACUOUS_COURT`,
    /// `RESTORE_TAMPER`, ...).
    Refused {
        /// Typed refusal identity.
        code: String,
        /// First failing target, when one is identified.
        target: Option<String>,
        /// Human-readable detail.
        detail: String,
    },
    /// The court cannot evaluate (e.g. no witness/shim timestamps in the
    /// catalog) — flip unverifiable, surfaces as a loud SKIP.
    Unknown {
        /// Why evaluation is impossible.
        reason: String,
    },
}

impl CourtVerdict {
    /// `true` only for [`CourtVerdict::Pass`].
    #[must_use]
    pub fn is_pass(&self) -> bool {
        matches!(self, Self::Pass { .. })
    }

    /// The typed process exit this verdict maps to (see module docs).
    #[must_use]
    pub fn exit_code(&self) -> i32 {
        match self {
            Self::Pass { .. } => 0,
            Self::Refused { code, .. } => {
                if code == "WITNESS_STALE" {
                    EXIT_VACUOUS
                } else {
                    EXIT_COURT_BLOCKED
                }
            }
            Self::Unknown { .. } => EXIT_SKIP,
        }
    }
}
