//! Core graph operations, formatting, parsing, serialization, and hashing.

pub mod canonical;
pub mod dataset;
pub mod hash;
pub mod parse;
pub mod quad;
pub mod serialize;

pub use crate::delta::RdfDelta;
pub use dataset::{DeterministicGraph, KnowledgeHook, TransitionReceipt};
pub use quad::{parse_nquad, QuadBuilder};
