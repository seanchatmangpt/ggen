//! Core graph operations, formatting, parsing, serialization, and hashing.

pub mod canonical;
pub mod dataset;
pub mod hash;
pub mod introspect;
pub mod locate;
pub mod parse;
pub mod quad;
pub mod serialize;

pub use crate::delta::RdfDelta;
pub use dataset::{DeterministicGraph, KnowledgeHook, TransitionReceipt};
pub use introspect::{iri_terms, IriTerms};
pub use locate::{
    extract_prefixes, parse_nquads_located, parse_ntriples_located, parse_turtle_located,
    LocatedParse, ParseDiagnostic,
};
pub use quad::{parse_nquad, QuadBuilder};
