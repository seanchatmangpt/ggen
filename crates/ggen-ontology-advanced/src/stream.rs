//! Async iterators and streaming support with GAT-based projections
//!
//! This module provides asynchronous streaming of RDF data with
//! Generic Associated Types for flexible, zero-cost abstractions.

use crate::error::{OntologyError, Result};
use crate::parser::{Triple, TripleRef};
use crate::projection::TripleProjection;
use async_trait::async_trait;
use std::pin::Pin;
use std::task::{Context, Poll};

/// Async stream of triples
///
/// This is a GAT-based async iterator for streaming RDF triples
pub struct TripleStream<'a> {
    /// The underlying data source
    source: TripleSource<'a>,
    /// Buffer for batching
    buffer: Vec<Triple>,
    /// Buffer position
    position: usize,
}

/// Source of triples for streaming
enum TripleSource<'a> {
    /// In-memory triples
    Memory(Vec<Triple>),
    /// String data to be parsed
    String(&'a str),
    /// Bytes data to be parsed
    Bytes(&'a [u8]),
}

impl<'a> TripleStream<'a> {
    /// Create a stream from in-memory triples
    pub fn from_triples(triples: Vec<Triple>) -> Self {
        Self {
            source: TripleSource::Memory(triples),
            buffer: Vec::new(),
            position: 0,
        }
    }

    /// Create a stream from a string
    pub fn from_str(data: &'a str) -> Self {
        Self {
            source: TripleSource::String(data),
            buffer: Vec::new(),
            position: 0,
        }
    }

    /// Create a stream from bytes
    pub fn from_bytes(data: &'a [u8]) -> Self {
        Self {
            source: TripleSource::Bytes(data),
            buffer: Vec::new(),
            position: 0,
        }
    }

    /// Apply a projection to the stream
    pub fn project<P: TripleProjection<'a>>(
        self,
    ) -> ProjectedTripleStream<'a, P> {
        ProjectedTripleStream::new(self)
    }

    /// Take only the first n triples
    pub fn take(self, n: usize) -> TakeStream<'a> {
        TakeStream::new(self, n)
    }

    /// Skip the first n triples
    pub fn skip(self, n: usize) -> SkipStream<'a> {
        SkipStream::new(self, n)
    }

    /// Filter triples by a predicate
    pub fn filter<F>(self, predicate: F) -> FilterStream<'a, F>
    where
        F: FnMut(&Triple) -> bool,
    {
        FilterStream::new(self, predicate)
    }
}

/// Projected stream that applies a GAT-based projection
pub struct ProjectedTripleStream<'a, P> {
    inner: TripleStream<'a>,
    _projection: std::marker::PhantomData<P>,
}

impl<'a, P> ProjectedTripleStream<'a, P> {
    fn new(inner: TripleStream<'a>) -> Self {
        Self {
            inner,
            _projection: std::marker::PhantomData,
        }
    }
}

/// Stream that takes only n items
pub struct TakeStream<'a> {
    inner: TripleStream<'a>,
    remaining: usize,
}

impl<'a> TakeStream<'a> {
    fn new(inner: TripleStream<'a>, n: usize) -> Self {
        Self {
            inner,
            remaining: n,
        }
    }
}

/// Stream that skips n items
pub struct SkipStream<'a> {
    inner: TripleStream<'a>,
    to_skip: usize,
}

impl<'a> SkipStream<'a> {
    fn new(inner: TripleStream<'a>, n: usize) -> Self {
        Self {
            inner,
            to_skip: n,
        }
    }
}

/// Stream that filters items by a predicate
pub struct FilterStream<'a, F> {
    inner: TripleStream<'a>,
    predicate: F,
}

impl<'a, F> FilterStream<'a, F> {
    fn new(inner: TripleStream<'a>, predicate: F) -> Self {
        Self { inner, predicate }
    }
}

/// Trait for async iteration over triples
///
/// This uses Generic Associated Types for flexible lifetime management
#[async_trait]
pub trait AsyncTripleIterator {
    /// The item type produced by the iterator
    type Item;

    /// Get the next item asynchronously
    async fn next(&mut self) -> Option<Result<Self::Item>>;

    /// Collect all items into a vector
    async fn collect(mut self) -> Result<Vec<Self::Item>>
    where
        Self: Sized,
        Self::Item: Sized,
    {
        let mut items = Vec::new();
        while let Some(result) = self.next().await {
            items.push(result?);
        }
        Ok(items)
    }

    /// Count the number of items
    async fn count(mut self) -> usize
    where
        Self: Sized,
    {
        let mut count = 0;
        while let Some(_) = self.next().await {
            count += 1;
        }
        count
    }

    /// Check if any item matches a predicate
    async fn any<F>(mut self, mut predicate: F) -> bool
    where
        Self: Sized,
        Self::Item: Sized,
        F: FnMut(&Self::Item) -> bool,
    {
        while let Some(result) = self.next().await {
            if let Ok(item) = result {
                if predicate(&item) {
                    return true;
                }
            }
        }
        false
    }

    /// Check if all items match a predicate
    async fn all<F>(mut self, mut predicate: F) -> bool
    where
        Self: Sized,
        Self::Item: Sized,
        F: FnMut(&Self::Item) -> bool,
    {
        while let Some(result) = self.next().await {
            if let Ok(item) = result {
                if !predicate(&item) {
                    return false;
                }
            }
        }
        true
    }
}

#[async_trait]
impl<'a> AsyncTripleIterator for TripleStream<'a> {
    type Item = Triple;

    async fn next(&mut self) -> Option<Result<Self::Item>> {
        match &mut self.source {
            TripleSource::Memory(triples) => {
                if self.position < triples.len() {
                    let triple = triples[self.position].clone();
                    self.position += 1;
                    Some(Ok(triple))
                } else {
                    None
                }
            }
            TripleSource::String(data) => {
                // Parse on demand
                // In production, this would use the streaming parser
                None
            }
            TripleSource::Bytes(data) => {
                // Parse on demand
                None
            }
        }
    }
}

#[async_trait]
impl<'a> AsyncTripleIterator for TakeStream<'a> {
    type Item = Triple;

    async fn next(&mut self) -> Option<Result<Self::Item>> {
        if self.remaining == 0 {
            return None;
        }
        self.remaining -= 1;
        self.inner.next().await
    }
}

#[async_trait]
impl<'a> AsyncTripleIterator for SkipStream<'a> {
    type Item = Triple;

    async fn next(&mut self) -> Option<Result<Self::Item>> {
        while self.to_skip > 0 {
            self.inner.next().await?;
            self.to_skip -= 1;
        }
        self.inner.next().await
    }
}

#[async_trait]
impl<'a, F> AsyncTripleIterator for FilterStream<'a, F>
where
    F: FnMut(&Triple) -> bool + Send,
{
    type Item = Triple;

    async fn next(&mut self) -> Option<Result<Self::Item>> {
        loop {
            match self.inner.next().await? {
                Ok(triple) => {
                    if (self.predicate)(&triple) {
                        return Some(Ok(triple));
                    }
                }
                Err(e) => return Some(Err(e)),
            }
        }
    }
}

/// Batched stream that yields chunks of triples
pub struct BatchedStream<'a> {
    inner: TripleStream<'a>,
    batch_size: usize,
}

impl<'a> BatchedStream<'a> {
    /// Create a new batched stream
    pub fn new(inner: TripleStream<'a>, batch_size: usize) -> Self {
        Self { inner, batch_size }
    }
}

#[async_trait]
impl<'a> AsyncTripleIterator for BatchedStream<'a> {
    type Item = Vec<Triple>;

    async fn next(&mut self) -> Option<Result<Self::Item>> {
        let mut batch = Vec::with_capacity(self.batch_size);

        for _ in 0..self.batch_size {
            match self.inner.next().await {
                Some(Ok(triple)) => batch.push(triple),
                Some(Err(e)) => return Some(Err(e)),
                None => break,
            }
        }

        if batch.is_empty() {
            None
        } else {
            Some(Ok(batch))
        }
    }
}

/// Extension trait for adding streaming methods to triple streams
pub trait TripleStreamExt<'a>: Sized {
    /// Create batches of the specified size
    fn batched(self, batch_size: usize) -> BatchedStream<'a>;
}

impl<'a> TripleStreamExt<'a> for TripleStream<'a> {
    fn batched(self, batch_size: usize) -> BatchedStream<'a> {
        BatchedStream::new(self, batch_size)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[tokio::test]
    async fn test_triple_stream() {
        let triples = vec![
            Triple::new("s1".to_string(), "p1".to_string(), "o1".to_string()),
            Triple::new("s2".to_string(), "p2".to_string(), "o2".to_string()),
        ];

        let mut stream = TripleStream::from_triples(triples);
        let result = stream.collect().await.expect("Failed to collect");

        assert_eq!(result.len(), 2);
    }

    #[tokio::test]
    async fn test_take_stream() {
        let triples = vec![
            Triple::new("s1".to_string(), "p1".to_string(), "o1".to_string()),
            Triple::new("s2".to_string(), "p2".to_string(), "o2".to_string()),
            Triple::new("s3".to_string(), "p3".to_string(), "o3".to_string()),
        ];

        let stream = TripleStream::from_triples(triples);
        let result = stream.take(2).collect().await.expect("Failed to collect");

        assert_eq!(result.len(), 2);
    }

    #[tokio::test]
    async fn test_filter_stream() {
        let triples = vec![
            Triple::new("s1".to_string(), "p1".to_string(), "o1".to_string()),
            Triple::new("s2".to_string(), "p2".to_string(), "o2".to_string()),
        ];

        let stream = TripleStream::from_triples(triples);
        let result = stream
            .filter(|t| t.subject == "s1")
            .collect()
            .await
            .expect("Failed to collect");

        assert_eq!(result.len(), 1);
        assert_eq!(result[0].subject, "s1");
    }

    #[tokio::test]
    async fn test_batched_stream() {
        let triples = vec![
            Triple::new("s1".to_string(), "p1".to_string(), "o1".to_string()),
            Triple::new("s2".to_string(), "p2".to_string(), "o2".to_string()),
            Triple::new("s3".to_string(), "p3".to_string(), "o3".to_string()),
        ];

        let stream = TripleStream::from_triples(triples);
        let batches = stream.batched(2).collect().await.expect("Failed to collect");

        assert_eq!(batches.len(), 2);
        assert_eq!(batches[0].len(), 2);
        assert_eq!(batches[1].len(), 1);
    }
}
