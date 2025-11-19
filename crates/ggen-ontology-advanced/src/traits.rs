//! Core traits for the ontology parser system

use crate::error::Result;
use std::future::Future;
use std::pin::Pin;

/// Trait for zero-copy parsing of RDF data
///
/// This trait enables efficient, zero-copy parsing using lifetime-bound references
pub trait ZeroCopyParse<'a> {
    /// The output type of the parse operation
    type Output;

    /// Parse input data without copying
    ///
    /// # Lifetime
    ///
    /// The output is bound to the lifetime of the input data,
    /// ensuring zero-copy semantics.
    fn parse(&self, input: &'a [u8]) -> Result<Self::Output>;
}

/// Trait for streaming parsers that yield items incrementally
///
/// This trait supports both synchronous and asynchronous streaming
pub trait StreamingParse {
    /// The type of items yielded by the stream
    type Item;

    /// The stream type
    type Stream: Iterator<Item = Result<Self::Item>>;

    /// Create a stream from the input
    fn parse_stream(&self, input: &str) -> Result<Self::Stream>;
}

/// Trait for async streaming parsers with GAT support
///
/// This trait uses Generic Associated Types for flexible async iteration
pub trait AsyncStreamingParse {
    /// The type of items yielded by the stream
    type Item;

    /// The async stream type
    type Stream<'a>: futures::Stream<Item = Result<Self::Item>>
    where
        Self: 'a;

    /// Create an async stream from the input
    fn parse_async<'a>(&'a self, input: &'a str) -> Self::Stream<'a>;
}

/// Trait for compile-time query validation
///
/// Types implementing this trait have been validated at compile time
pub trait ValidatedQuery {
    /// The query hash computed at compile time
    const QUERY_HASH: u64;

    /// The query string
    const QUERY_STR: &'static str;

    /// Get the validated query string
    fn query_str(&self) -> &'static str {
        Self::QUERY_STR
    }

    /// Get the query hash for caching
    fn query_hash(&self) -> u64 {
        Self::QUERY_HASH
    }
}

/// Trait for type-level query construction
///
/// This trait enables building SPARQL queries at the type level
pub trait TypeLevelQuery {
    /// The resulting query type
    type QueryType;

    /// Build the query at the type level
    fn build() -> Self::QueryType;
}

/// Future type alias for async operations
pub type BoxFuture<'a, T> = Pin<Box<dyn Future<Output = T> + Send + 'a>>;
