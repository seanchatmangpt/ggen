//! GAT-based projection system for query results
//!
//! This module provides a flexible projection system using Generic Associated Types (GATs)
//! to transform query results into application-specific types.

use crate::error::Result;
use crate::parser::TripleRef;
use crate::query::QueryBinding;

/// Trait for projecting query bindings into domain types
///
/// Uses Generic Associated Types to support lifetime-parameterized projections
pub trait Projection<'a> {
    /// The output type of the projection
    type Output;

    /// Project a query binding into the output type
    fn project(binding: &'a QueryBinding) -> Option<Self::Output>;
}

/// Trait for projecting triples into domain types
pub trait TripleProjection<'a> {
    /// The output type of the projection
    type Output;

    /// Project a triple into the output type
    fn project(triple: &'a TripleRef<'a>) -> Option<Self::Output>;
}

/// Standard triple projection that extracts subject, predicate, and object
#[derive(Debug, Clone, Copy)]
pub struct StandardTripleProjection;

impl<'a> TripleProjection<'a> for StandardTripleProjection {
    type Output = (&'a str, &'a str, &'a str);

    fn project(triple: &'a TripleRef<'a>) -> Option<Self::Output> {
        Some((
            triple.subject.as_ref(),
            triple.predicate.as_ref(),
            triple.object.as_ref(),
        ))
    }
}

/// Projection that extracts only the subject
#[derive(Debug, Clone, Copy)]
pub struct SubjectProjection;

impl<'a> TripleProjection<'a> for SubjectProjection {
    type Output = &'a str;

    fn project(triple: &'a TripleRef<'a>) -> Option<Self::Output> {
        Some(triple.subject.as_ref())
    }
}

/// Projection that extracts only the predicate
#[derive(Debug, Clone, Copy)]
pub struct PredicateProjection;

impl<'a> TripleProjection<'a> for PredicateProjection {
    type Output = &'a str;

    fn project(triple: &'a TripleRef<'a>) -> Option<Self::Output> {
        Some(triple.predicate.as_ref())
    }
}

/// Projection that extracts only the object
#[derive(Debug, Clone, Copy)]
pub struct ObjectProjection;

impl<'a> TripleProjection<'a> for ObjectProjection {
    type Output = &'a str;

    fn project(triple: &'a TripleRef<'a>) -> Option<Self::Output> {
        Some(triple.object.as_ref())
    }
}

/// Projection builder for composing multiple projections
#[derive(Debug)]
pub struct ProjectionBuilder<P> {
    _marker: std::marker::PhantomData<P>,
}

impl<P> ProjectionBuilder<P> {
    /// Create a new projection builder
    pub const fn new() -> Self {
        Self {
            _marker: std::marker::PhantomData,
        }
    }
}

impl<P> Default for ProjectionBuilder<P> {
    fn default() -> Self {
        Self::new()
    }
}

/// Combinator for composing two projections
pub struct AndThen<P1, P2> {
    _p1: std::marker::PhantomData<P1>,
    _p2: std::marker::PhantomData<P2>,
}

/// Combinator for optional projections
pub struct Optional<P> {
    _projection: std::marker::PhantomData<P>,
}

/// Projection that filters results based on a predicate
pub struct Filter<P, F> {
    _projection: std::marker::PhantomData<P>,
    filter_fn: F,
}

impl<P, F> Filter<P, F> {
    /// Create a new filter projection
    pub const fn new(filter_fn: F) -> Self {
        Self {
            _projection: std::marker::PhantomData,
            filter_fn,
        }
    }
}

/// Projection that maps the output using a function
pub struct Map<P, F> {
    _projection: std::marker::PhantomData<P>,
    map_fn: F,
}

impl<P, F> Map<P, F> {
    /// Create a new map projection
    pub const fn new(map_fn: F) -> Self {
        Self {
            _projection: std::marker::PhantomData,
            map_fn,
        }
    }
}

/// Streaming projection iterator
///
/// Applies a projection to each item in a stream
pub struct ProjectionIter<I, P> {
    inner: I,
    _projection: std::marker::PhantomData<P>,
}

impl<'a, I, P> ProjectionIter<I, P>
where
    I: Iterator<Item = Result<TripleRef<'a>>>,
    P: TripleProjection<'a>,
{
    /// Create a new projection iterator
    pub fn new(inner: I) -> Self {
        Self {
            inner,
            _projection: std::marker::PhantomData,
        }
    }
}

impl<'a, I, P> Iterator for ProjectionIter<I, P>
where
    I: Iterator<Item = Result<TripleRef<'a>>>,
    P: TripleProjection<'a>,
{
    type Item = Result<P::Output>;

    fn next(&mut self) -> Option<Self::Item> {
        match self.inner.next()? {
            Ok(triple) => P::project(&triple)
                .map(Ok)
                .or(Some(Err("Projection failed".into()))),
            Err(e) => Some(Err(e)),
        }
    }
}

/// Extension trait for adding projection methods to iterators
pub trait ProjectionExt<'a>: Iterator<Item = Result<TripleRef<'a>>> + Sized {
    /// Apply a projection to the stream
    fn project<P: TripleProjection<'a>>(self) -> ProjectionIter<Self, P> {
        ProjectionIter::new(self)
    }

    /// Project to subjects only
    fn subjects(self) -> ProjectionIter<Self, SubjectProjection> {
        ProjectionIter::new(self)
    }

    /// Project to predicates only
    fn predicates(self) -> ProjectionIter<Self, PredicateProjection> {
        ProjectionIter::new(self)
    }

    /// Project to objects only
    fn objects(self) -> ProjectionIter<Self, ObjectProjection> {
        ProjectionIter::new(self)
    }
}

impl<'a, I> ProjectionExt<'a> for I where I: Iterator<Item = Result<TripleRef<'a>>> {}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::parser::Triple;
    use std::borrow::Cow;

    #[test]
    fn test_standard_triple_projection() {
        let triple = TripleRef {
            subject: Cow::Borrowed("s"),
            predicate: Cow::Borrowed("p"),
            object: Cow::Borrowed("o"),
        };

        let result = StandardTripleProjection::project(&triple);
        assert_eq!(result, Some(("s", "p", "o")));
    }

    #[test]
    fn test_subject_projection() {
        let triple = TripleRef {
            subject: Cow::Borrowed("subject"),
            predicate: Cow::Borrowed("predicate"),
            object: Cow::Borrowed("object"),
        };

        let result = SubjectProjection::project(&triple);
        assert_eq!(result, Some("subject"));
    }

    #[test]
    fn test_projection_iter() {
        let triples = vec![
            Ok(TripleRef {
                subject: Cow::Borrowed("s1"),
                predicate: Cow::Borrowed("p1"),
                object: Cow::Borrowed("o1"),
            }),
            Ok(TripleRef {
                subject: Cow::Borrowed("s2"),
                predicate: Cow::Borrowed("p2"),
                object: Cow::Borrowed("o2"),
            }),
        ];

        let subjects: Vec<_> = triples
            .into_iter()
            .subjects()
            .collect::<Result<Vec<_>>>()
            .expect("Failed to project");

        assert_eq!(subjects, vec!["s1", "s2"]);
    }
}
