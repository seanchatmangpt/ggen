/// Union-Find Equivalence Canonicalization (Pattern 4)
///
/// Implements disjoint-set data structure for owl:sameAs, equivalentClass,
/// and equivalentProperty relationships. All receipts use only the canonical
/// sorted edge list, never raw parent-array memory.
///
/// # Complexity
///
/// - Union: O(α(n)) amortized with path compression + union-by-rank
/// - Find: O(α(n)) amortized with path compression
/// - Canonicalization: O(n log n) for sorting edge list
/// - Receipt hashing: O(n) for sorted edge list only
///
/// # Determinism
///
/// Same inputs → byte-identical canonical edge list across all runs.
/// Platform-independent: no raw memory serialization.
use std::collections::HashMap;

/// Union-Find structure with path compression and union-by-rank
/// for equivalence class canonicalization.
///
/// All term IDs are Encoder-issued usize values. Three separate instances
/// are maintained for class IDs, property IDs, and general term IDs (logically
/// distinct universes despite shared Encoder).
#[derive(Debug, Clone)]
pub struct UnionFind {
    parent: HashMap<usize, usize>,
    rank: HashMap<usize, u16>,
}

impl UnionFind {
    /// Create a new empty UnionFind structure
    pub fn new() -> Self {
        UnionFind {
            parent: HashMap::new(),
            rank: HashMap::new(),
        }
    }

    /// Add a term ID to the union-find if not already present.
    /// Each term is initially its own parent (singleton set).
    pub fn make_set(&mut self, x: usize) {
        if !self.parent.contains_key(&x) {
            self.parent.insert(x, x);
            self.rank.insert(x, 0);
        }
    }

    /// Find the canonical representative of the set containing x.
    /// Uses path compression: all visited nodes are re-pointed to the root.
    /// Returns Result to match project error-handling discipline.
    pub fn find(&mut self, x: usize) -> Result<usize, String> {
        if !self.parent.contains_key(&x) {
            return Err(format!(
                "Term ID {} not in union-find; call make_set first",
                x
            ));
        }

        let root = self.find_root(x);
        // Path compression: all intermediate nodes now point to root
        self.path_compress(x, root);
        Ok(root)
    }

    /// Merge the sets containing x and y.
    /// Union-by-rank: attach smaller tree to larger tree.
    /// Returns Result to match project error-handling discipline.
    pub fn union(&mut self, x: usize, y: usize) -> Result<(), String> {
        let root_x = self.find(x)?;
        let root_y = self.find(y)?;

        if root_x == root_y {
            // Already in same set; no-op
            return Ok(());
        }

        let rank_x = *self.rank.get(&root_x).unwrap_or(&0);
        let rank_y = *self.rank.get(&root_y).unwrap_or(&0);

        let (new_parent, new_child) = if rank_x < rank_y {
            (root_y, root_x)
        } else if rank_x > rank_y {
            (root_x, root_y)
        } else {
            // Equal rank: choose x as parent and increment rank
            self.rank.insert(root_x, rank_x + 1);
            (root_x, root_y)
        };

        self.parent.insert(new_child, new_parent);
        Ok(())
    }

    /// Find the root of the tree containing x (without path compression).
    fn find_root(&self, mut x: usize) -> usize {
        loop {
            let parent = self.parent[&x];
            if parent == x {
                return x;
            }
            x = parent;
        }
    }

    /// Path compression: re-point all intermediate nodes on path to root.
    fn path_compress(&mut self, mut x: usize, root: usize) {
        loop {
            let parent = self.parent[&x];
            if parent == x {
                return; // Reached root
            }
            self.parent.insert(x, root);
            x = parent;
        }
    }

    /// Render the equivalence relation as a canonical sorted edge list.
    /// PROJ-409 Canonical Rendering Rule: this is the intended canonical form
    /// for future BLAKE3 hashing (PROJ-416); no hash consumer exists yet.
    ///
    /// Returns a sorted Vec<(u32, u32)> of (canonical_id, equivalent_id) pairs
    /// where canonical_id is the root representative.
    pub fn render_canonical(&mut self) -> Vec<(u32, u32)> {
        let mut edges = Vec::new();
        let all_ids: Vec<usize> = self.parent.keys().copied().collect();

        for id in all_ids {
            if let Ok(root) = self.find(id) {
                if root != id {
                    // Add edge from root to this id (ensures deterministic direction)
                    edges.push((root as u32, id as u32));
                }
            }
        }

        edges.sort_unstable();
        edges
    }

    /// Count the number of disjoint sets (equivalence classes).
    /// Useful for diagnostics and testing.
    pub fn count_sets(&mut self) -> usize {
        let all_ids: Vec<usize> = self.parent.keys().copied().collect();
        all_ids
            .iter()
            .filter(|&&id| {
                if let Ok(root) = self.find(id) {
                    root == id
                } else {
                    false
                }
            })
            .count()
    }
}

impl Default for UnionFind {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_union_find_singleton() {
        let mut uf = UnionFind::new();
        uf.make_set(1);
        assert_eq!(uf.find(1).unwrap(), 1);
    }

    #[test]
    fn test_union_find_two_elements() {
        let mut uf = UnionFind::new();
        uf.make_set(1);
        uf.make_set(2);
        uf.union(1, 2).unwrap();

        let root1 = uf.find(1).unwrap();
        let root2 = uf.find(2).unwrap();
        assert_eq!(root1, root2, "After union, both should have same root");
    }

    #[test]
    fn test_union_find_three_elements() {
        let mut uf = UnionFind::new();
        uf.make_set(1);
        uf.make_set(2);
        uf.make_set(3);

        uf.union(1, 2).unwrap();
        uf.union(2, 3).unwrap();

        let root1 = uf.find(1).unwrap();
        let root2 = uf.find(2).unwrap();
        let root3 = uf.find(3).unwrap();

        assert_eq!(root1, root2);
        assert_eq!(root2, root3);
    }

    #[test]
    fn test_union_find_multiple_sets() {
        let mut uf = UnionFind::new();
        // Create two disjoint sets: {1,2} and {3,4}
        uf.make_set(1);
        uf.make_set(2);
        uf.make_set(3);
        uf.make_set(4);

        uf.union(1, 2).unwrap();
        uf.union(3, 4).unwrap();

        let root1 = uf.find(1).unwrap();
        let root2 = uf.find(2).unwrap();
        let root3 = uf.find(3).unwrap();
        let root4 = uf.find(4).unwrap();

        assert_eq!(root1, root2);
        assert_eq!(root3, root4);
        assert_ne!(root1, root3);
    }

    #[test]
    fn test_union_find_canonical_rendering() {
        let mut uf = UnionFind::new();
        uf.make_set(1);
        uf.make_set(2);
        uf.make_set(3);

        uf.union(1, 2).unwrap();
        uf.union(2, 3).unwrap();

        let edges = uf.render_canonical();
        // Should have edges from root to non-root elements
        assert!(!edges.is_empty());
        // Edges should be sorted
        assert!(edges.windows(2).all(|w| w[0] <= w[1]));
    }

    #[test]
    fn test_union_find_find_nonexistent() {
        let mut uf = UnionFind::new();
        let result = uf.find(999);
        assert!(result.is_err(), "Finding non-existent element should error");
    }

    #[test]
    fn test_union_find_union_nonexistent() {
        let mut uf = UnionFind::new();
        uf.make_set(1);
        let result = uf.union(1, 999);
        assert!(
            result.is_err(),
            "Union with non-existent element should error"
        );
    }

    #[test]
    fn test_union_find_path_compression() {
        let mut uf = UnionFind::new();
        // Create a chain: 1 <- 2 <- 3 <- 4
        // After path compression, all should point to root
        uf.make_set(1);
        uf.make_set(2);
        uf.make_set(3);
        uf.make_set(4);

        uf.union(1, 2).unwrap();
        uf.union(2, 3).unwrap();
        uf.union(3, 4).unwrap();

        // Find 4 should compress the path
        let root = uf.find(4).unwrap();
        let root1 = uf.find(1).unwrap();

        assert_eq!(root, root1);
    }

    #[test]
    fn test_union_find_deterministic_canonical() {
        let mut uf1 = UnionFind::new();
        let mut uf2 = UnionFind::new();

        // Populate both with same structure but in different order
        for i in 1..=5 {
            uf1.make_set(i);
            uf2.make_set(i);
        }

        // Same operations in same order
        uf1.union(1, 2).unwrap();
        uf1.union(2, 3).unwrap();
        uf1.union(3, 4).unwrap();

        uf2.union(1, 2).unwrap();
        uf2.union(2, 3).unwrap();
        uf2.union(3, 4).unwrap();

        let edges1 = uf1.render_canonical();
        let edges2 = uf2.render_canonical();

        assert_eq!(edges1, edges2, "Canonical renderings should be identical");
    }

    #[test]
    fn test_union_find_count_sets() {
        let mut uf = UnionFind::new();
        uf.make_set(1);
        uf.make_set(2);
        uf.make_set(3);
        uf.make_set(4);

        assert_eq!(uf.count_sets(), 4);

        uf.union(1, 2).unwrap();
        assert_eq!(uf.count_sets(), 3);

        uf.union(3, 4).unwrap();
        assert_eq!(uf.count_sets(), 2);

        uf.union(1, 3).unwrap();
        assert_eq!(uf.count_sets(), 1);
    }
}
