//! Bounded Triple8 term universe: a frozen, closed-world, profile-scoped
//! interner mapping at most 256 canonical RDF term strings to [`Term8`]
//! (a single byte), plus projection of oxrdf triples/quads into that
//! universe with a verifiable BLAKE3 projection hash.
//!
//! Canonical term formatting matches the N-Triples discipline used by
//! `crate::hooks::quads::canonicalize_quads`: IRIs as `<iri>`, blank nodes
//! as `_:label`, literals in their N-Triples serialization (oxrdf's
//! `Display` impls produce exactly this form).
//!
//! All hashing routes through `wasm4pm_compat::hash` (compat owns hash
//! identity). No wall clock anywhere in this module.

use std::collections::BTreeMap;
use std::fmt;

use oxrdf::{QuadRef, TermRef, TripleRef};
use serde::{Deserialize, Serialize};
use wasm4pm_compat::hash::{blake3_combined, blake3_hex};

use super::abi::{GraphSnapshotId, ProfileId, Refusal};

/// A term in the bounded Triple8 universe: an index into a frozen
/// [`ProfileSymbolTable`] of at most 256 terms.
#[derive(
    Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize, Default,
)]
#[serde(transparent)]
pub struct Term8(pub u8);

impl fmt::Display for Term8 {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "t8:{}", self.0)
    }
}

/// A triple whose terms all live in one Triple8 universe. 3 bytes, `Copy`.
#[derive(
    Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize, Default,
)]
pub struct RDFTriple8 {
    /// Subject term.
    pub s: Term8,
    /// Predicate term.
    pub p: Term8,
    /// Object term.
    pub o: Term8,
}

/// A quad whose terms all live in one Triple8 universe. 4 bytes, `Copy`.
#[derive(
    Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize, Default,
)]
pub struct RDFQuad8 {
    /// Subject term.
    pub s: Term8,
    /// Predicate term.
    pub p: Term8,
    /// Object term.
    pub o: Term8,
    /// Graph term.
    pub g: Term8,
}

/// Canonical N-Triples formatting for an oxrdf term, matching the
/// `hooks::quads::canonicalize_quads` discipline: `<iri>` for named nodes,
/// `_:label` for blank nodes, N-Triples literal serialization (with escaped
/// `\` `"` `\n` `\r` `\t`) for literals.
///
/// oxrdf's `Display` for `TermRef` produces exactly this canonical form.
///
/// # Complexity
/// O(len of the term's serialization).
fn canonical_term(term: TermRef<'_>) -> String {
    term.to_string()
}

/// Frozen closed-world profile-scoped interner over at most 256 canonical
/// term strings. Built once from a sorted, deduplicated term list; never
/// interns on miss — unknown terms are refused by name.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct ProfileSymbolTable {
    /// Profile this universe is scoped to.
    profile_id: ProfileId,
    /// term string -> byte id (ids assigned in sorted term order).
    by_name: BTreeMap<String, u8>,
    /// byte id -> term string (index = id).
    by_id: Vec<String>,
    /// BLAKE3 hex over profile id + sorted terms (see [`Self::build`]).
    hash: String,
}

impl ProfileSymbolTable {
    /// Builds a frozen universe from an arbitrary term iterator.
    ///
    /// Terms are sorted and deduplicated; ids are assigned in sorted order,
    /// so the same term set always yields the same table and hash
    /// (deterministic). Refuses with [`Refusal::Triple8UniverseOverflow`]
    /// naming the 257th (first overflowing) term in sorted order when more
    /// than 256 distinct terms are supplied.
    ///
    /// The table hash is `blake3_combined` over the profile id followed by
    /// every sorted term — length-prefixed per part, hence injective.
    ///
    /// # Complexity
    /// O(n log n) in the number of input terms (sort + dedup + BTreeMap
    /// inserts), plus O(total term bytes) hashing.
    pub fn build(
        profile_id: ProfileId, terms: impl IntoIterator<Item = String>,
    ) -> Result<Self, Refusal> {
        // O(n log n): collect, sort, dedup.
        let mut sorted: Vec<String> = terms.into_iter().collect();
        sorted.sort();
        sorted.dedup();

        if sorted.len() > 256 {
            return Err(Refusal::Triple8UniverseOverflow(format!(
                "profile {}: {} distinct terms exceed the 256-term Triple8 universe; \
                 first overflowing term (index 256 in sorted order): {}",
                profile_id.as_str(),
                sorted.len(),
                sorted[256]
            )));
        }

        // O(n): assemble hash parts — profile id then sorted terms,
        // injectively length-prefixed inside blake3_combined.
        let mut parts: Vec<&str> = Vec::with_capacity(sorted.len() + 1);
        parts.push(profile_id.as_str());
        for t in &sorted {
            parts.push(t.as_str());
        }
        let hash = blake3_combined(&parts);

        // O(n log n): build the forward map; idx < 256 proven by the
        // overflow check above, so the `as u8` cast is lossless.
        let mut by_name = BTreeMap::new();
        for (idx, t) in sorted.iter().enumerate() {
            by_name.insert(t.clone(), idx as u8);
        }

        Ok(Self {
            profile_id,
            by_name,
            by_id: sorted,
            hash,
        })
    }

    /// Profile this universe is scoped to.
    pub fn profile_id(&self) -> &ProfileId {
        &self.profile_id
    }

    /// Number of terms in the frozen universe (≤ 256).
    pub fn len(&self) -> usize {
        self.by_id.len()
    }

    /// True when the universe contains no terms.
    pub fn is_empty(&self) -> bool {
        self.by_id.is_empty()
    }

    /// BLAKE3 hex identity of this table (profile id + sorted terms).
    pub fn hash(&self) -> &str {
        &self.hash
    }

    /// Resolves a canonical term string to its [`Term8`].
    ///
    /// Never interns on miss: an unknown term is refused with
    /// [`Refusal::TermNotInTriple8Universe`] naming the term. The universe
    /// is frozen after [`Self::build`].
    ///
    /// # Complexity
    /// O(log n) BTreeMap lookup.
    pub fn resolve(&self, term: &str) -> Result<Term8, Refusal> {
        self.by_name.get(term).map(|&id| Term8(id)).ok_or_else(|| {
            Refusal::TermNotInTriple8Universe(format!(
                "profile {}: term not in frozen Triple8 universe: {}",
                self.profile_id.as_str(),
                term
            ))
        })
    }

    /// Resolves a [`Term8`] back to its canonical term string.
    ///
    /// Refuses with [`Refusal::TermNotInTriple8Universe`] when the id is
    /// outside the frozen universe (possible when the universe holds fewer
    /// than 256 terms).
    ///
    /// # Complexity
    /// O(1) index lookup.
    pub fn name(&self, term: Term8) -> Result<&str, Refusal> {
        self.by_id
            .get(term.0 as usize)
            .map(String::as_str)
            .ok_or_else(|| {
                Refusal::TermNotInTriple8Universe(format!(
                    "profile {}: Term8({}) outside universe of {} terms",
                    self.profile_id.as_str(),
                    term.0,
                    self.by_id.len()
                ))
            })
    }

    /// Projects an oxrdf triple into this universe using canonical
    /// N-Triples term formatting.
    ///
    /// # Complexity
    /// O(log n) per term (3 resolves) plus term serialization cost.
    pub fn project_triple(&self, triple: TripleRef<'_>) -> Result<RDFTriple8, Refusal> {
        let s = self.resolve(&canonical_term(triple.subject.into()))?;
        let p = self.resolve(&canonical_term(triple.predicate.into()))?;
        let o = self.resolve(&canonical_term(triple.object))?;
        Ok(RDFTriple8 { s, p, o })
    }

    /// Projects an oxrdf quad into this universe. The graph name uses
    /// oxrdf's canonical `GraphName` serialization (the default graph
    /// serializes as `DEFAULT`), which must be a member of the universe for
    /// the projection to succeed.
    ///
    /// # Complexity
    /// O(log n) per term (4 resolves) plus term serialization cost.
    pub fn project_quad(&self, quad: QuadRef<'_>) -> Result<RDFQuad8, Refusal> {
        let s = self.resolve(&canonical_term(quad.subject.into()))?;
        let p = self.resolve(&canonical_term(quad.predicate.into()))?;
        let o = self.resolve(&canonical_term(quad.object))?;
        let g = self.resolve(&quad.graph_name.to_string())?;
        Ok(RDFQuad8 { s, p, o, g })
    }

    /// Computes the projection hash: BLAKE3 over the symbol table hash,
    /// the snapshot id, and the sorted triple bytes.
    ///
    /// Triples are copied, sorted (canonical order is the derived `Ord` on
    /// `(s, p, o)`), and deduplicated, then serialized as fixed-width
    /// 3-byte records — injective for a fixed table + snapshot, and
    /// insensitive to input order. The parts are combined injectively via
    /// `blake3_combined` (length-prefixed) under a version tag.
    ///
    /// # Complexity
    /// O(t log t) in the number of triples (sort), plus O(t) hashing.
    pub fn projection_hash(&self, snapshot_id: &GraphSnapshotId, triples: &[RDFTriple8]) -> String {
        // O(t log t): canonical sort of a copy; input order must not matter.
        let mut sorted: Vec<RDFTriple8> = triples.to_vec();
        sorted.sort();
        sorted.dedup();

        // O(t): fixed-width 3-byte records — injective for a fixed prefix.
        let mut triple_bytes = Vec::with_capacity(sorted.len() * 3);
        for t in &sorted {
            triple_bytes.push(t.s.0);
            triple_bytes.push(t.p.0);
            triple_bytes.push(t.o.0);
        }
        let triples_hash = blake3_hex(&triple_bytes);

        blake3_combined(&[
            "praxis-chatman-triple8-projection-v1",
            self.hash.as_str(),
            snapshot_id.as_str(),
            triples_hash.as_str(),
        ])
    }

    /// Verifies a previously computed projection hash by recomputation.
    ///
    /// Refuses with [`Refusal::ProjectionHashMismatch`] on drift.
    ///
    /// # Complexity
    /// O(t log t) (delegates to [`Self::projection_hash`]).
    pub fn verify_projection(
        &self, snapshot_id: &GraphSnapshotId, triples: &[RDFTriple8], expected_hash: &str,
    ) -> Result<(), Refusal> {
        let actual = self.projection_hash(snapshot_id, triples);
        if actual != expected_hash {
            return Err(Refusal::ProjectionHashMismatch(format!(
                "profile {} snapshot {}: expected {}, recomputed {}",
                self.profile_id.as_str(),
                snapshot_id.as_str(),
                expected_hash,
                actual
            )));
        }
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use chicago_tdd_tools::prelude::*;
    use oxrdf::{Literal, NamedNode, Quad, Triple};

    fn profile() -> ProfileId {
        ProfileId::new("profile:test")
    }

    fn snapshot() -> GraphSnapshotId {
        GraphSnapshotId::new("snapshot:test")
    }

    fn small_universe() -> Result<ProfileSymbolTable, Refusal> {
        // Canonical N-Triples forms, deliberately unsorted on input.
        ProfileSymbolTable::build(
            profile(),
            vec![
                "<http://example.org/s>".to_string(),
                "<http://example.org/p>".to_string(),
                "\"hello\"".to_string(),
                "<http://example.org/o>".to_string(),
            ],
        )
    }

    test!(build_assigns_ids_in_sorted_order_and_resolves, {
        // Arrange
        let table = small_universe()?;

        // Act
        let literal = table.resolve("\"hello\"");
        let s = table.resolve("<http://example.org/s>");

        // Assert: "\"hello\"" sorts before the <...> IRIs, so it gets id 0.
        assert_eq!(literal, Ok(Term8(0)));
        assert_ok!(&s);
        assert_eq!(table.len(), 4);
        assert_eq!(table.name(Term8(0)), Ok("\"hello\""));
        Ok::<(), Refusal>(())
    });

    test!(build_refuses_at_257_terms_naming_the_overflow_term, {
        // Arrange: 257 distinct zero-padded terms; sorted order == numeric order.
        let terms: Vec<String> = (0..257).map(|i| format!("<urn:t:{i:03}>")).collect();

        // Act
        let result = ProfileSymbolTable::build(profile(), terms);

        // Assert: refused, naming the 257th term in sorted order.
        assert_err!(&result);
        assert!(
            matches!(
                &result,
                Err(Refusal::Triple8UniverseOverflow(msg)) if msg.contains("<urn:t:256>")
            ),
            "expected Triple8UniverseOverflow naming <urn:t:256>, got {result:?}"
        );
    });

    test!(build_accepts_exactly_256_terms, {
        // Arrange: exactly 256 distinct terms is the boundary, not an overflow.
        let terms: Vec<String> = (0..256).map(|i| format!("<urn:t:{i:03}>")).collect();

        // Act
        let table = ProfileSymbolTable::build(profile(), terms)?;

        // Assert
        assert_eq!(table.len(), 256);
        assert_eq!(table.name(Term8(255)), Ok("<urn:t:255>"));
        Ok::<(), Refusal>(())
    });

    test!(resolve_miss_refuses_and_never_interns, {
        // Arrange
        let table = small_universe()?;
        let len_before = table.len();

        // Act
        let miss = table.resolve("<http://example.org/absent>");

        // Assert: refused by name; universe unchanged (frozen).
        assert!(
            matches!(
                &miss,
                Err(Refusal::TermNotInTriple8Universe(msg)) if msg.contains("absent")
            ),
            "expected TermNotInTriple8Universe naming the term, got {miss:?}"
        );
        assert_eq!(table.len(), len_before);
        Ok::<(), Refusal>(())
    });

    test!(build_is_deterministic_byte_identical_hash, {
        // Arrange: same term set, different input orders, one duplicate.
        let terms_a = vec![
            "<urn:a>".to_string(),
            "<urn:b>".to_string(),
            "<urn:c>".to_string(),
        ];
        let terms_b = vec![
            "<urn:c>".to_string(),
            "<urn:a>".to_string(),
            "<urn:b>".to_string(),
            "<urn:a>".to_string(),
        ];

        // Act
        let t1 = ProfileSymbolTable::build(profile(), terms_a)?;
        let t2 = ProfileSymbolTable::build(profile(), terms_b)?;

        // Assert: byte-identical hashes and fully equal tables.
        assert_eq!(t1.hash(), t2.hash());
        assert_eq!(t1, t2);
        Ok::<(), Refusal>(())
    });

    test!(projection_round_trip_and_hash_verify, {
        // Arrange
        let table = small_universe()?;
        let iri = |v: &str| {
            NamedNode::new(v).map_err(|e| Refusal::ValidationFailed(format!("bad test IRI: {e}")))
        };
        let s = iri("http://example.org/s")?;
        let p = iri("http://example.org/p")?;
        let o = Literal::new_simple_literal("hello");
        let triple = Triple::new(s, p, o);

        // Act: project, then round-trip each term back to its name.
        let projected = table.project_triple(triple.as_ref())?;

        // Assert round-trip against canonical N-Triples forms.
        assert_eq!(table.name(projected.s)?, "<http://example.org/s>");
        assert_eq!(table.name(projected.p)?, "<http://example.org/p>");
        assert_eq!(table.name(projected.o)?, "\"hello\"");

        // Act: projection hash verifies against itself...
        let triples = [projected];
        let hash = table.projection_hash(&snapshot(), &triples);
        table.verify_projection(&snapshot(), &triples, &hash)?;

        // Assert: ...and drift (different triple set) is refused.
        let drift = table.verify_projection(&snapshot(), &[], &hash);
        assert!(
            matches!(&drift, Err(Refusal::ProjectionHashMismatch(_))),
            "expected ProjectionHashMismatch on drift, got {drift:?}"
        );
        Ok::<(), Refusal>(())
    });

    test!(project_quad_round_trip, {
        // Arrange: small_universe() plus a fourth term for the graph name,
        // since project_quad resolves 4 terms (s, p, o, g) vs. project_triple's 3.
        let table = ProfileSymbolTable::build(
            profile(),
            vec![
                "<http://example.org/s>".to_string(),
                "<http://example.org/p>".to_string(),
                "\"hello\"".to_string(),
                "<http://example.org/o>".to_string(),
                "<http://example.org/g>".to_string(),
            ],
        )?;
        let iri = |v: &str| {
            NamedNode::new(v).map_err(|e| Refusal::ValidationFailed(format!("bad test IRI: {e}")))
        };
        let s = iri("http://example.org/s")?;
        let p = iri("http://example.org/p")?;
        let o = Literal::new_simple_literal("hello");
        let g = iri("http://example.org/g")?;
        let quad = Quad::new(s, p, o, g);

        // Act: project, then round-trip each term back to its name.
        let projected = table.project_quad(quad.as_ref())?;

        // Assert round-trip against canonical N-Triples forms, mirroring
        // projection_round_trip_and_hash_verify but also covering the graph term.
        assert_eq!(table.name(projected.s)?, "<http://example.org/s>");
        assert_eq!(table.name(projected.p)?, "<http://example.org/p>");
        assert_eq!(table.name(projected.o)?, "\"hello\"");
        assert_eq!(table.name(projected.g)?, "<http://example.org/g>");
        Ok::<(), Refusal>(())
    });
}
