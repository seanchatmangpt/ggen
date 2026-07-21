/// Equivalence Canonicalization Pipeline (Pattern 4)
///
/// Processes owl:sameAs, owl:equivalentClass, and owl:equivalentProperty
/// relationships through separate union-find structures. Produces canonical
/// edge lists for deterministic receipt hashing.
///
/// Integration: Call after parsing shapes, before ClosureMatrix construction
/// in validation pipeline.
///
/// # Complexity
///
/// - Union operations: O(α(n)) amortized per edge
/// - Canonicalization: O(n log n) for sorting edge lists
/// - Overall pipeline: O(e * α(n) + n log n) where e = equivalence edges
use super::canonicalization::UnionFind;
use super::index_utils::get_triples_by_predicate;
use super::Vocab;
use crate::tripleindex::TripleIndex;

/// Result of equivalence canonicalization phase.
/// Contains three canonical edge lists (one per equivalence universe).
#[derive(Debug, Clone)]
pub struct EquivalenceCanonical {
    /// Canonical edges for class equivalences (owl:equivalentClass + owl:sameAs on classes)
    pub class_edges: Vec<(u32, u32)>,
    /// Canonical edges for property equivalences (owl:equivalentProperty + owl:sameAs on properties)
    pub property_edges: Vec<(u32, u32)>,
    /// Canonical edges for general term equivalences (owl:sameAs on arbitrary terms)
    pub term_edges: Vec<(u32, u32)>,
}

/// Process equivalence triples and produce canonical edge lists.
///
/// # Arguments
/// * `data` - The data graph (containing assertions and inferences)
/// * `vocab` - The vocabulary structure with predicate IDs
///
/// # Returns
/// Canonical edge lists for each equivalence universe, suitable for
/// deterministic hashing and ClosureMatrix construction.
///
/// # Errors
/// Returns a String error if union-find operations fail (e.g., operations
/// on non-existent IDs).
pub fn canonicalize_equivalences(
    data: &TripleIndex, vocab: &Vocab,
) -> Result<EquivalenceCanonical, String> {
    let mut class_uf = UnionFind::new();
    let mut property_uf = UnionFind::new();
    let mut term_uf = UnionFind::new();

    // Collect all term IDs from the data for registration
    let all_triple_ids = collect_all_term_ids(data);

    // Initialize all IDs in union-find structures
    for id in &all_triple_ids {
        class_uf.make_set(*id);
        property_uf.make_set(*id);
        term_uf.make_set(*id);
    }

    // Process owl:equivalentClass triples
    let equiv_class_edges = get_triples_by_predicate(data, vocab.owl_equivalent_class);
    for (subj, obj) in equiv_class_edges {
        class_uf.union(subj, obj)?;
    }

    // Process owl:equivalentProperty triples
    let equiv_prop_edges = get_triples_by_predicate(data, vocab.owl_equivalent_property);
    for (subj, obj) in equiv_prop_edges {
        property_uf.union(subj, obj)?;
    }

    // Process owl:sameAs triples (can apply to any term kind)
    let same_as_edges = get_triples_by_predicate(data, vocab.owl_same_as);
    for (subj, obj) in same_as_edges.iter() {
        term_uf.union(*subj, *obj)?;
        class_uf.union(*subj, *obj)?;
        property_uf.union(*subj, *obj)?;
    }

    // Render canonical edge lists
    let class_edges = class_uf.render_canonical();
    let property_edges = property_uf.render_canonical();
    let term_edges = term_uf.render_canonical();

    Ok(EquivalenceCanonical {
        class_edges,
        property_edges,
        term_edges,
    })
}

/// Collect all term IDs from the data graph.
/// Returns the union of all subjects and objects in the graph.
fn collect_all_term_ids(data: &TripleIndex) -> Vec<usize> {
    let mut ids = std::collections::HashSet::new();

    // Iterate through all triples in the data
    for (subj, po_map) in &data.spo {
        ids.insert(*subj);
        for po_values in po_map.values() {
            for (obj, _, _) in po_values {
                ids.insert(*obj);
            }
        }
    }

    let mut sorted_ids: Vec<usize> = ids.into_iter().collect();
    sorted_ids.sort_unstable();
    sorted_ids
}

/// Render equivalence canonical edges for deterministic hashing.
///
/// This function combines all three equivalence universes into a single
/// sorted edge list suitable for BLAKE3 hashing in receipt generation.
///
/// # Determinism Guarantee
///
/// Same input equivalences → identical output bytes across all platforms.
pub fn render_all_equivalences_canonical(canonical: &EquivalenceCanonical) -> Vec<(u32, u32)> {
    let mut all_edges = Vec::new();
    all_edges.extend(&canonical.class_edges);
    all_edges.extend(&canonical.property_edges);
    all_edges.extend(&canonical.term_edges);
    all_edges.sort_unstable();
    all_edges.dedup();
    all_edges
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_canonicalize_equivalences_empty() {
        // Create empty TripleIndex
        let data = TripleIndex::new();
        let vocab = Vocab::new();

        let result = canonicalize_equivalences(&data, &vocab);
        assert!(result.is_ok());

        let canonical = result.unwrap();
        assert!(canonical.class_edges.is_empty());
        assert!(canonical.property_edges.is_empty());
        assert!(canonical.term_edges.is_empty());
    }

    #[test]
    fn test_render_all_equivalences_canonical() {
        let canonical = EquivalenceCanonical {
            class_edges: vec![(1, 2), (3, 4)],
            property_edges: vec![(5, 6)],
            term_edges: vec![(7, 8)],
        };

        let all_edges = render_all_equivalences_canonical(&canonical);
        assert_eq!(all_edges.len(), 4);
        // Should be sorted
        assert!(all_edges.windows(2).all(|w| w[0] <= w[1]));
    }

    #[test]
    fn test_render_all_equivalences_dedup() {
        let canonical = EquivalenceCanonical {
            class_edges: vec![(1, 2), (1, 2)], // Duplicate
            property_edges: vec![(1, 2)],      // Same as class_edges
            term_edges: vec![],
        };

        let all_edges = render_all_equivalences_canonical(&canonical);
        // Should deduplicate
        assert_eq!(all_edges.len(), 1);
    }
}
