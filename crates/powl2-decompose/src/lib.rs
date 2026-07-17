//! # powl2-decompose — Kourani Stage-1: WF-net → POWL 2.0
//!
//! A faithful implementation of the recursive decomposition of Kourani, Park &
//! van der Aalst, *"Hierarchical Decomposition of Separable Workflow-Nets"*
//! (arXiv:2602.15739), transforming a **safe & sound workflow net** into an
//! equivalent **POWL 2.0** model built from partial orders (concurrency) and
//! choice graphs (generalized decision + cyclic logic).
//!
//! ## Separability is the admission predicate
//!
//! POWL 2.0 is a strict subclass of sound WF-nets; the algorithm is *complete*
//! precisely on the **separable** class (Def 3.13 — nets built by nesting
//! state machines and marked graphs). Where the paper's Algorithm 3 would fall
//! through, this crate **refuses** with a receipt ([`Refusal`]) instead of
//! approximating. This is a Rice-style boundary for process models: a WF-net
//! either decomposes (admitted, [`convert`] returns a [`Powl`]) or is refused
//! with a classified, content-addressed reason.
//!
//! ## Differential correctness
//!
//! The [`language`] module computes a WF-net's bounded token-game language;
//! [`Powl::language_upto`] computes the model's bounded language from the
//! POWL 2.0 semantics; and [`recompose`] maps a model back to a WF-net. The
//! test suite checks, on small nets, that all three agree — establishing the
//! paper's correctness theorem (`L(decompose(N)) = L(N)`) and a round-trip
//! (`L(recompose(decompose(N))) = L(N)`) by independent computation.

#![deny(unsafe_code)]
#![warn(missing_docs)]
// The decomposition works over pairwise index relations (execution order/flow,
// Floyd–Warshall transitive closure): explicit indices are clearer than
// enumerate-zip gymnastics here.
#![allow(clippy::needless_range_loop)]

pub mod decompose;
pub mod language;
pub mod net;
pub mod powl;
pub mod recompose;

/// External-Cut Validator module.
pub mod external_cut;

pub use decompose::{convert, convert_with_budget, Refusal, RefusalReason, DEFAULT_DEPTH_BUDGET};
pub use external_cut::{validate_external_cut, ExternalCutRefusal};
pub use net::{NetError, WfNet};
pub use powl::{
    ChoiceGraph, GNode, Language, ParentChildClosure, ParentChildEdge, Powl, SocketKind,
    SocketPath, Trace, WorkflowSocketId,
};
pub use recompose::recompose;
