//! Deterministic Turtle serializer for the reverse pipeline.
//!
//! `ggen-core` cannot depend on `ggen-graph` (no dependency edge), and
//! oxigraph's dump is store-ordered (not byte-stable). This module provides a
//! small, self-contained, deterministic Turtle writer: triples are stored in
//! sorted [`BTreeMap`]/[`BTreeSet`] collections and emitted in a fixed,
//! canonical order, so repeated runs over identical input are byte-for-byte
//! identical. Subjects use deterministic *named* nodes (never anonymous blank
//! nodes), which keeps the output canonical and queryable downstream.

use std::collections::{BTreeMap, BTreeSet};

use crate::utils::error::{Error, Result};

/// An RDF object term in its serialized position.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum Object {
    /// An IRI written verbatim as a prefixed name (`code:Service`) or `<abs>`.
    Iri(String),
    /// A plain string literal; quoted and escaped on write.
    Str(String),
    /// A typed literal written as `"value"^^<datatype>` (datatype is a prefixed
    /// name or absolute IRI, written verbatim).
    Typed { value: String, datatype: String },
}

/// A deterministic set of RDF triples sharing a prefix table.
///
/// Iteration is sorted at every level (`BTreeMap`/`BTreeSet`), so
/// [`TripleSet::to_turtle`] is a pure function of the inserted triples.
#[derive(Debug, Clone, Default)]
pub struct TripleSet {
    prefixes: BTreeMap<String, String>,
    spo: BTreeMap<String, BTreeMap<String, BTreeSet<Object>>>,
}

impl TripleSet {
    /// Create an empty triple set with no prefixes.
    pub fn new() -> Self {
        Self::default()
    }

    /// Register a `@prefix prefix: <namespace> .` declaration.
    pub fn add_prefix(&mut self, prefix: &str, namespace: &str) {
        self.prefixes
            .insert(prefix.to_string(), namespace.to_string());
    }

    /// Insert a single triple. The predicate `"a"` is emitted verbatim
    /// (Turtle shorthand for `rdf:type`).
    pub fn insert(&mut self, subject: &str, predicate: &str, object: Object) {
        self.spo
            .entry(subject.to_string())
            .or_default()
            .entry(predicate.to_string())
            .or_default()
            .insert(object);
    }

    /// Number of distinct triples.
    pub fn len(&self) -> usize {
        self.spo
            .values()
            .flat_map(|po| po.values())
            .map(|os| os.len())
            .sum()
    }

    /// Whether the set contains no triples.
    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }

    /// Serialize to canonical Turtle.
    ///
    /// Returns `Err` for an empty graph: a non-empty input must never serialize
    /// to a decorative, triple-less file (honesty guard, see
    /// `.claude/rules/coding-agent-mistakes.md` §1.1).
    pub fn to_turtle(&self) -> Result<String> {
        if self.spo.is_empty() {
            return Err(Error::new(
                "refusing to serialize an empty RDF graph (no triples)",
            ));
        }

        let mut out = String::new();
        for (prefix, namespace) in &self.prefixes {
            out.push_str(&format!("@prefix {prefix}: <{namespace}> .\n"));
        }
        out.push('\n');

        for (subject, predicates) in &self.spo {
            out.push_str(subject);
            let mut first_pred = true;
            for (predicate, objects) in predicates {
                if first_pred {
                    out.push(' ');
                    first_pred = false;
                } else {
                    out.push_str(" ;\n    ");
                }
                out.push_str(predicate);
                out.push(' ');
                let mut first_obj = true;
                for object in objects {
                    if !first_obj {
                        out.push_str(", ");
                    }
                    first_obj = false;
                    out.push_str(&render_object(object));
                }
            }
            out.push_str(" .\n");
        }
        Ok(out)
    }
}

fn render_object(object: &Object) -> String {
    match object {
        Object::Iri(iri) => iri.clone(),
        Object::Str(value) => format!("\"{}\"", escape_literal(value)),
        Object::Typed { value, datatype } => {
            format!("\"{}\"^^{datatype}", escape_literal(value))
        }
    }
}

/// Escape a string for use inside a Turtle double-quoted literal.
fn escape_literal(value: &str) -> String {
    let mut out = String::with_capacity(value.len());
    for c in value.chars() {
        match c {
            '\\' => out.push_str("\\\\"),
            '"' => out.push_str("\\\""),
            '\n' => out.push_str("\\n"),
            '\r' => out.push_str("\\r"),
            '\t' => out.push_str("\\t"),
            _ => out.push(c),
        }
    }
    out
}

#[cfg(test)]
mod tests {
    use super::*;

    fn sample() -> TripleSet {
        let mut ts = TripleSet::new();
        ts.add_prefix("code", "https://ggen.io/code#");
        ts.add_prefix("disco", "https://ggen.io/discovered#");
        ts.insert("disco:Foo", "a", Object::Iri("code:Service".to_string()));
        ts.insert(
            "disco:Foo",
            "code:language",
            Object::Str("rust".to_string()),
        );
        ts
    }

    #[test]
    fn serialization_is_deterministic() {
        let a = sample().to_turtle().expect("turtle");
        let b = sample().to_turtle().expect("turtle");
        assert_eq!(a, b, "identical input must serialize byte-identically");
    }

    #[test]
    fn empty_graph_refuses_to_serialize() {
        let ts = TripleSet::new();
        assert!(
            ts.to_turtle().is_err(),
            "empty graph must not produce a decorative file"
        );
    }

    #[test]
    fn literals_are_escaped() {
        let mut ts = TripleSet::new();
        ts.add_prefix("disco", "https://ggen.io/discovered#");
        ts.insert(
            "disco:X",
            "disco:note",
            Object::Str("a\"b\\c\nd".to_string()),
        );
        let ttl = ts.to_turtle().expect("turtle");
        assert!(ttl.contains("\\\""), "double-quote must be escaped");
        assert!(ttl.contains("\\\\"), "backslash must be escaped");
        assert!(ttl.contains("\\n"), "newline must be escaped");
    }

    #[test]
    fn triple_count_is_accurate() {
        assert_eq!(sample().len(), 2);
    }
}
