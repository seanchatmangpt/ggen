//! Hyper-Advanced Zero-Copy Streaming Ontology Parser
//!
//! This library implements bleeding-edge Rust 2028 features for ontology parsing:
//!
//! - **Zero-copy streaming**: Efficient memory usage with lifetime-based parsing
//! - **Compile-time SPARQL validation**: Type-safe queries validated at compile time
//! - **Type-level programming**: Const generics and phantom types for zero-cost abstractions
//! - **GAT-based projections**: Generic Associated Types for flexible query results
//! - **Async iterators**: Streaming results with async/await support
//! - **Procedural macros**: Semantic code synthesis and query generation
//!
//! # Architecture
//!
//! The library is organized into several modules:
//!
//! - [`parser`]: Zero-copy streaming parsers for RDF formats
//! - [`query`]: Compile-time validated SPARQL query system
//! - [`type_level`]: Type-level programming constructs
//! - [`projection`]: GAT-based projection system
//! - [`stream`]: Async iterators for streaming results
//!
//! # Examples
//!
//! ```rust,no_run
//! use ggen_ontology_advanced::prelude::*;
//!
//! # async fn example() -> Result<(), Box<dyn std::error::Error>> {
//! // Zero-copy streaming parser
//! let parser = StreamingParser::new();
//! let stream = parser.parse_str(r#"
//!     @prefix : <http://example.org/> .
//!     :subject :predicate :object .
//! "#)?;
//!
//! // Compile-time validated query (using procedural macro)
//! // let query = sparql_query! { "SELECT ?s ?p ?o WHERE { ?s ?p ?o }" };
//!
//! // GAT-based projection
//! // let results = stream.project::<TripleProjection>().collect().await;
//! # Ok(())
//! # }
//! ```

#![cfg_attr(feature = "nightly", feature(generic_associated_types))]
#![cfg_attr(feature = "nightly", feature(type_alias_impl_trait))]
#![cfg_attr(feature = "nightly", feature(async_iterator))]
#![cfg_attr(feature = "nightly", feature(const_trait_impl))]
#![cfg_attr(feature = "nightly", feature(const_fn_floating_point_arithmetic))]

// Re-export procedural macros
pub use ggen_ontology_macros::{
    ontology_types, projection, semantic_synthesis, sparql_query, type_level_query,
};

// Public modules
pub mod parser;
pub mod projection;
pub mod query;
pub mod stream;
pub mod type_level;

// Internal modules
mod error;
mod traits;

// Re-exports
pub use error::{OntologyError, Result};
pub use traits::*;

/// Prelude module for convenient imports
pub mod prelude {
    pub use crate::parser::{StreamingParser, Triple, TripleRef};
    pub use crate::projection::{Projection, TripleProjection};
    pub use crate::query::{CompiledQuery, QueryEngine};
    pub use crate::stream::{TripleStream, AsyncTripleIterator};
    pub use crate::type_level::{TypedTriple, Var};
    pub use crate::OntologyError;
    pub use crate::Result;
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_library_compiles() {
        // Basic compilation test
        assert!(true);
    }
}
