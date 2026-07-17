//! Quarantine boundary index (PROJ-SEC-03).
//!
//! A red-team security review named 10 "quarantine" boundaries for the
//! Chatman Engine. Verification against this crate's source found real code
//! correlates for only 3 of them. This module does not add enforcement
//! logic — every boundary named here already exists in [`router`] or
//! [`engine`] — it exists to centralize the names for auditability, and to
//! be explicit about which of the security review's 10 named boundaries
//! have **no** code implementation, so a future reader does not have to
//! rediscover that gap by grepping the whole crate.
//!
//! # The 3 real quarantine boundaries
//!
//! 1. **N3/Rice quarantine** — [`DialectRouter::decide`] enforces
//!    capability-gated dialect routing: a shape needing N3 builtins is
//!    refused with [`Refusal::N3UnavailableByProfile`] unless the profile
//!    enables N3, is refused with [`Refusal::N3ActuationRefused`] if it also
//!    wants actuation (N3 may never drive actuation), and the hot
//!    (`Dialect::Triple8Pattern`) path is budget-quarantined by
//!    `ProfileGates::max_hot_constraints` (tracked internally via the
//!    `hot_blocked_by_budget` flag, surfaced as
//!    [`Refusal::WarmPathRequired`]).
//! 2. **Receipt/replay quarantine** — [`EngineProcessReceipt`] and
//!    [`ReplayInputs`] are only ever produced by the admission pipeline
//!    (`ChatmanEngine::admit_transition` for the receipt; the caller-supplied
//!    envelope/snapshot/profile triple for replay). The receipt's nine
//!    constitutional digests plus `receipt_root` are recomputed, never
//!    asserted, by [`EngineProcessReceipt::recompute_root`].
//! 3. **Actuation quarantine** — `ChatmanEngine::actuate` takes an owned
//!    [`AdmittedTransition`] by value. `AdmittedTransition`'s fields are
//!    private; the only constructor is `ChatmanEngine::admit_transition`, so
//!    no caller can synthesize a transition and hand it to `actuate` without
//!    passing through S1-S6 admission first.
//!
//! # Named quarantines with no code implementation
//!
//! These 7 names appear in the security review's vocabulary but have no
//! corresponding enforcement code in this crate as of this writing. Each is
//! listed with the reason it is NOT IMPLEMENTED, not with speculative future
//! work:
//!
//! - **raw-observation quarantine** — NOT IMPLEMENTED: no boundary exists
//!   between raw sensor/observation ingestion and the snapshot graph; S1
//!   (`fetch_snapshot`) trusts its input snapshot IRI as given.
//! - **public-ontology quarantine** — NOT IMPLEMENTED: no code enforces a
//!   separation between public-ontology vocabulary and private extensions;
//!   the closed-vocabulary refusal (`Refusal::UnknownVocabulary` and
//!   friends) checks predicate names, not provenance of the ontology itself.
//! - **private-IR quarantine** — NOT IMPLEMENTED: no boundary type isolates
//!   a private intermediate representation from external callers; the PDDL
//!   tape and POWL projection types are crate-public with no sealed IR gate.
//! - **generated-artifact quarantine** — NOT IMPLEMENTED: generated files
//!   (e.g. POWL Turtle output) carry no provenance tag or quarantine marker
//!   distinguishing them from hand-authored input at the type level.
//! - **executable-artifact quarantine** — NOT IMPLEMENTED: no sandboxing or
//!   capability boundary exists for executable output; this crate does not
//!   execute generated artifacts at all.
//! - **conformance quarantine (named subsystem)** — NOT IMPLEMENTED as a
//!   distinct subsystem: conformance checks exist, but scattered inline
//!   across admission8/router/engine validation, not behind one named
//!   quarantine boundary the review's vocabulary implies.
//! - **LLM quarantine** — NOT IMPLEMENTED: `crates/cng/src/bench.rs`'s
//!   `ModeledLlmComparison` is a benchmark cost-modeling struct for
//!   estimating relative compute cost; it is not an enforcement boundary and
//!   must not be conflated with a quarantine that gates LLM-originated input
//!   or output.

pub use super::engine::{AdmittedTransition, EngineProcessReceipt, ReplayInputs};
pub use super::router::{Dialect, DialectRouter};
