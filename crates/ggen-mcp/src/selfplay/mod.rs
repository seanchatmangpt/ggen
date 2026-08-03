//! Self-play: adversarial pack testing with a deterministic referee.
//!
//! # The game
//!
//! ggen ships 78 packs. Until now, exactly 5 of them had any lifecycle proof
//! (the consumers wired into `just guard-pack-proofs`). The other 73 were
//! never driven through generation at all — so a pack could ship a broken
//! ontology, an unrenderable template, or a rule that writes outside the
//! project root, and nothing would notice.
//!
//! This module closes that. Each pack becomes a game board: a real consumer
//! project is manufactured from the pack's own ontology, an adversarial
//! *case* (a SPARQL query plus a template) is played against it, and a
//! **referee** checks invariants that must hold no matter what the case
//! contains.
//!
//! # Who generates the cases
//!
//! Two sources, deliberately separated:
//!
//! - **Committed corpus** (`tests/corpus/*.json`) — every case that has ever
//!   tripped an invariant, minimized and checked in. `cargo test` replays
//!   these deterministically. No GPU, no network, CI-safe.
//! - **Live exploration** (`ggen-selfplay-explore`) — a local Gemma 4 26B on
//!   Metal reads a pack's real ontology and writes cases designed to break
//!   ggen. Nondeterministic by nature, so it never runs in the test suite;
//!   its only job is to *grow* the corpus.
//!
//! This is the libFuzzer split: nondeterministic discovery, deterministic
//! regression. The LLM proposes; the referee adjudicates. The LLM is never
//! in the truth-asserting path — it cannot declare a case passing or
//! failing, only supply bytes that the deterministic engine then judges.
//!
//! # What the referee actually enforces
//!
//! See [`Invariant`]. Each is independently falsifiable and each maps to a
//! failure class that would otherwise be silent — which is the whole reason
//! this crate exists (see the crate docs for the zero-of-113-rows incident).

pub mod board;
pub mod case;
pub mod referee;

pub use board::Board;
pub use case::{Case, CaseOrigin};
pub use referee::{referee_verdict, Invariant, Verdict, Violation};
