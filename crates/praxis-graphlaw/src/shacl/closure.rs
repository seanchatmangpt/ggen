use super::index_utils::get_triples_by_predicate;
/// Subclass closure computation and bitset-based closure matrix
///
/// Implements efficient computation and storage of transitive closure
/// for subclass relationships in RDF ontologies.
use crate::tripleindex::TripleIndex;
use fixedbitset::FixedBitSet;
use std::collections::HashMap;

/// Compute and cache the transitive closure of rdfs:subClassOf relationships
/// using a bitset-based ClosureMatrix internally.
///
/// # Complexity
/// O(|C|²) for closure computation where |C| is the number of classes,
/// using iterative fixpoint. Query is O(1) after construction.
///
/// # Design
/// Dense ID remapping: Since global Encoder IDs are sparse, we build a
/// bidirectional mapping (global_id ↔ dense_index) to enable efficient
/// bitset indexing. The ClosureMatrix uses dense indices internally;
/// public is_subclass() queries remap on demand.
pub struct SubclassClosure {
    /// Bitset-based closure matrix using dense local indices
    matrix: ClosureMatrix,
    /// Maps global Encoder ID → dense index (0..num_classes)
    global_to_dense: HashMap<usize, usize>,
    /// Maps dense index → global Encoder ID (for render_canonical)
    // Reserved seam: written by `new()` but not yet read. Intended for a future
    // `SubclassClosure::render_canonical()` (PROJ-416 BLAKE3 hash consumer,
    // mirroring `ClosureMatrix::render_canonical`) that remaps dense edges back
    // to global IDs before canonical hashing. No consumer exists yet.
    #[allow(dead_code)]
    dense_to_global: Vec<usize>,
}

impl SubclassClosure {
    /// Create a new SubclassClosure from a data graph.
    ///
    /// Constructs a dense-index ClosureMatrix and maintains bidirectional
    /// ID remapping for transparent query semantics.
    pub fn new(data: &TripleIndex, rdfs_subclass_of: usize) -> Self {
        let subclass_triples = get_triples_by_predicate(data, rdfs_subclass_of);

        let mut direct_parents: HashMap<usize, Vec<usize>> = HashMap::new();
        for (sub, parent) in subclass_triples {
            direct_parents.entry(sub).or_default().push(parent);
        }

        // Step 1: Collect all class IDs that appear in the closure
        let mut all_classes: Vec<usize> = direct_parents.keys().cloned().collect();
        for parents in direct_parents.values() {
            for &p in parents {
                all_classes.push(p);
            }
        }
        all_classes.sort_unstable();
        all_classes.dedup();

        // Step 2: Build dense ID remap (global_id → dense_index)
        let mut global_to_dense = HashMap::new();
        let dense_to_global = all_classes.clone();
        for (dense_idx, &global_id) in all_classes.iter().enumerate() {
            global_to_dense.insert(global_id, dense_idx);
        }

        // Step 3: Create and populate ClosureMatrix using dense indices
        let num_classes = all_classes.len() as u32;
        let mut matrix = ClosureMatrix::new(num_classes.saturating_sub(1));

        // Add direct edges using dense indices
        for (sub, parents) in &direct_parents {
            if let Some(&sub_dense) = global_to_dense.get(sub) {
                for &parent in parents {
                    if let Some(&parent_dense) = global_to_dense.get(&parent) {
                        matrix.add_edge(sub_dense, parent_dense);
                    }
                }
            }
        }

        // Step 4: Compute transitive closure
        matrix.compute_transitive_closure();

        SubclassClosure {
            matrix,
            global_to_dense,
            dense_to_global,
        }
    }

    /// Check if `sub` is a subclass of `parent` (including reflexive case)
    ///
    /// Remaps global Encoder IDs to dense indices for bitset lookup.
    pub fn is_subclass(&self, sub: usize, parent: usize) -> bool {
        if sub == parent {
            return true;
        }

        // Remap to dense indices
        if let (Some(&sub_dense), Some(&parent_dense)) = (
            self.global_to_dense.get(&sub),
            self.global_to_dense.get(&parent),
        ) {
            self.matrix.is_reachable(sub_dense, parent_dense)
        } else {
            // If either ID is not in the closure graph, not a subclass
            false
        }
    }
}

/// Bitset-based transitive closure matrix for dense closure sites.
/// Replaces HashMap<usize, HashSet<usize>> for better performance and
/// memory density when closure cardinality is high (> 80% of ID space used).
///
/// PROJ-409 Canonical Rendering Rule: Raw bitset memory is never hashed.
/// The sorted edge list (render_canonical() output) is the intended canonical
/// form for future BLAKE3 hashing (PROJ-416); no hash consumer exists yet.
/// This ensures platform-independence and determinism.
#[derive(Debug, Clone)]
pub struct ClosureMatrix {
    /// One bitset per row: matrix[from_id] contains all reachable nodes
    matrix: Vec<FixedBitSet>,
    /// Highest ID in the closure
    pub max_id: u32,
}

impl ClosureMatrix {
    /// Create a new ClosureMatrix with capacity for up to max_id+1 nodes
    pub fn new(max_id: u32) -> Self {
        let capacity = (max_id + 1) as usize;
        ClosureMatrix {
            matrix: vec![FixedBitSet::with_capacity(capacity); capacity],
            max_id,
        }
    }

    /// Add a direct edge (from → to) and transitively close if needed
    pub fn add_edge(&mut self, from: usize, to: usize) {
        if from <= self.max_id as usize && to <= self.max_id as usize {
            self.matrix[from].insert(to);
        }
    }

    /// Get all reachable nodes from a source ID (as a FixedBitSet reference)
    pub fn reachable(&self, from: usize) -> Option<&FixedBitSet> {
        if from <= self.max_id as usize {
            Some(&self.matrix[from])
        } else {
            None
        }
    }

    /// Check if `to` is reachable from `from`
    pub fn is_reachable(&self, from: usize, to: usize) -> bool {
        if from <= self.max_id as usize && to <= self.max_id as usize {
            self.matrix[from].contains(to)
        } else {
            false
        }
    }

    /// Compute transitive closure using iterative fixpoint (Floyd-Warshall-like)
    /// until no new edges are discovered.
    pub fn compute_transitive_closure(&mut self) {
        let num_nodes = self.matrix.len();
        let mut changed = true;
        while changed {
            changed = false;
            for from in 0..num_nodes {
                // Collect all current reachable nodes from `from` into a vec
                // (can't borrow matrix while modifying it)
                let reachable: Vec<usize> = self.matrix[from].ones().collect();
                for via in reachable {
                    if via < num_nodes {
                        // Union self.matrix[via] into self.matrix[from]
                        let via_reachable = self.matrix[via].clone();
                        for target in via_reachable.ones() {
                            if !self.matrix[from].contains(target) {
                                self.matrix[from].insert(target);
                                changed = true;
                            }
                        }
                    }
                }
            }
        }
    }

    /// Render the closure as a sorted list of edges for deterministic hashing.
    /// PROJ-409 Canonical Rendering Rule: this is the intended canonical form
    /// for future BLAKE3 hashing (PROJ-416) — never the raw bitset memory.
    /// No hash consumer exists yet.
    pub fn render_canonical(&self) -> Vec<(u32, u32)> {
        let mut edges = Vec::new();
        for (from_id, bitset) in self.matrix.iter().enumerate() {
            for to_id in bitset.ones() {
                edges.push((from_id as u32, to_id as u32));
            }
        }
        edges.sort_unstable();
        edges
    }
}

/// Check if a value node has a given class (checking the subclass hierarchy)
pub(crate) fn has_class(
    data: &TripleIndex,
    x: usize,
    class: usize,
    rdf_type: usize,
    closure: &SubclassClosure,
) -> bool {
    let types = super::index_utils::get_objects(data, x, rdf_type);
    for t in types {
        if closure.is_subclass(t, class) {
            return true;
        }
    }
    false
}
