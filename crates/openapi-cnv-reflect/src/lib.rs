//! Reflects an OpenAPI 3.x document into a `cnv:Cli` RDF ontology consumable
//! unchanged by the zero-code clap-noun-verb compiler
//! (`packs/clap-noun-verb-*-pack`) -- so N API endpoints cost one reflection
//! pass instead of N hand-authored ontology fact blocks.
//!
//! See `docs/how-to/clap-noun-verb/reflect-openapi.md` for the 80/20 scope
//! this tool covers (GET operations, scalar path/query parameters,
//! `cnv:CustomBehavior` dispatch) and what it deliberately does not.

#![forbid(unsafe_code)]

pub mod error;
pub mod naming;
pub mod reflect;
pub mod write;

pub use error::ReflectError;
pub use reflect::{reflect, ReflectOutcome, ReflectWarning};
pub use write::write_ontology;
