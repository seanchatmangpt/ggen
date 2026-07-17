//! Backward-compatible re-export shim.
//!
//! `triples.rs` used to hold the term model (`VarOrTerm`/`Term`/`Triple`),
//! the rule model (`Rule`/`BodyLiteral`/`Aggregate`), and the global
//! list/formula registries all in one file. They now live in
//! `crate::term`, `crate::rule`, and `crate::registry` respectively; this
//! module just re-exports everything at the old `crate::triples::*` path
//! so existing call sites across the workspace keep compiling unchanged.

pub use crate::rule::{Aggregate, AggregateFunction, BodyLiteral, Rule, DENIAL_HEAD_MARKER};
pub use crate::term::{BlankNodeImpl, LiteralImpl, Term, TermImpl, Triple, VarOrTerm, Variable};
