//! Permutation-friendly, dictionary-encoded fact storage.
//!
//! Ground atoms are stored as tuples of [`SymId`]s — never strings — partitioned
//! by predicate and kept in sorted order (a `BTreeSet` per predicate). This is
//! the "sorted-ID storage" a triple/quad store keeps as its permutation
//! indexes: within a predicate the argument tuples are a sorted relation, so a
//! precondition atom can be resolved by an ordered scan (sorted-merge join)
//! rather than a hash probe, and prefix-bound lookups are a `range` on the set.
//!
//! Each finalized store also carries an [`XorFilter`] over all its atom keys.
//! Membership is then a two-stage test — the filter rejects impossible atoms in
//! O(1) with no allocation, and only survivors pay for the exact `BTreeSet`
//! lookup. The filter has no false negatives, so this never hides a real fact.

use crate::ground::dict::SymId;
use crate::ground::xorf::XorFilter;
use std::collections::{BTreeSet, HashMap};

/// A dictionary-encoded set of ground atoms.
#[derive(Debug, Clone, Default)]
pub struct FactStore {
    /// predicate ID → sorted set of argument-ID tuples.
    by_pred: HashMap<u32, BTreeSet<Vec<u32>>>,
    /// Approximate-membership gate over all atom keys; rebuilt by [`freeze`].
    ///
    /// [`freeze`]: Self::freeze
    filter: Option<XorFilter>,
    /// Total atom count (across predicates).
    len: usize,
}

/// Fold a predicate ID and its argument IDs into a single 64-bit filter key.
///
/// Collisions only raise the filter's false-positive rate (never its
/// correctness — an exact `BTreeSet` check always follows), so a fast rolling
/// mix is enough.
#[must_use]
pub fn atom_key(pred: u32, args: &[u32]) -> u64 {
    let mut h: u64 = 0xcbf2_9ce4_8422_2325; // FNV-ish offset basis
    h ^= u64::from(pred).wrapping_add(1);
    h = h.wrapping_mul(0x0100_0000_01b3);
    for (i, &a) in args.iter().enumerate() {
        h ^= u64::from(a).wrapping_add(i as u64).wrapping_add(1);
        h = h.wrapping_mul(0x0100_0000_01b3);
        h ^= h >> 29;
    }
    h
}

impl FactStore {
    /// Create an empty store.
    #[must_use]
    pub fn new() -> Self {
        Self::default()
    }

    /// Insert a ground atom. Returns `true` if it was newly added.
    ///
    /// Mutating the store invalidates any previously built [`XorFilter`]; call
    /// [`freeze`](Self::freeze) again before relying on the fast membership gate.
    pub fn insert(&mut self, pred: SymId, args: &[SymId]) -> bool {
        let ids: Vec<u32> = args.iter().map(|s| s.0).collect();
        let added = self.by_pred.entry(pred.0).or_default().insert(ids);
        if added {
            self.len += 1;
            self.filter = None;
        }
        added
    }

    /// Build the [`XorFilter`] over the current atom set. Idempotent; cheap to
    /// call once after a batch of inserts.
    pub fn freeze(&mut self) {
        let mut keys: Vec<u64> = Vec::with_capacity(self.len);
        for (&pred, tuples) in &self.by_pred {
            for args in tuples {
                keys.push(atom_key(pred, args));
            }
        }
        self.filter = Some(XorFilter::build(&keys));
    }

    /// Exact membership test.
    ///
    /// If a filter has been built, it gates the lookup: a filter miss returns
    /// `false` immediately, and only a filter hit pays for the `BTreeSet`
    /// probe. Correct with or without a frozen filter.
    #[must_use]
    pub fn contains(&self, pred: SymId, args: &[SymId]) -> bool {
        let ids: Vec<u32> = args.iter().map(|s| s.0).collect();
        if let Some(f) = &self.filter {
            if !f.contains(atom_key(pred.0, &ids)) {
                return false;
            }
        }
        self.by_pred.get(&pred.0).is_some_and(|s| s.contains(&ids))
    }

    /// Sorted iterator over the argument tuples stored for `pred` (empty if the
    /// predicate has no facts). The order is the merge-join order.
    pub fn tuples_for(&self, pred: SymId) -> impl Iterator<Item = &Vec<u32>> {
        self.by_pred
            .get(&pred.0)
            .into_iter()
            .flat_map(BTreeSet::iter)
    }

    /// Number of facts stored for `pred`.
    ///
    /// Pre-existing dead-code under `make check`/`make clippy`'s
    /// `RUSTFLAGS=-D warnings` (Makefile.toml's `[env]` section): `FactStore`
    /// is a private, un-re-exported type (`mod facts;` not `pub mod facts;`
    /// in `ground/mod.rs`), and only this module's own `#[cfg(test)]` tests
    /// call this today. Kept as real public-API surface, not removed.
    #[must_use]
    #[allow(dead_code)]
    pub fn arity_count(&self, pred: SymId) -> usize {
        self.by_pred.get(&pred.0).map_or(0, BTreeSet::len)
    }

    /// Total number of stored atoms.
    #[must_use]
    pub fn len(&self) -> usize {
        self.len
    }

    /// Whether the store holds no atoms. See [`Self::arity_count`]'s doc
    /// comment for why this is `#[allow(dead_code)]`.
    #[must_use]
    #[allow(dead_code)]
    pub fn is_empty(&self) -> bool {
        self.len == 0
    }

    /// Whether the fast filter has been built. See [`Self::arity_count`]'s
    /// doc comment for why this is `#[allow(dead_code)]`.
    #[must_use]
    #[allow(dead_code)]
    pub fn is_frozen(&self) -> bool {
        self.filter.is_some()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ground::dict::Dict;

    #[test]
    fn insert_dedup_and_contains() {
        let mut d = Dict::new();
        let at = d.intern("at");
        let l1 = d.intern("loc1");
        let l2 = d.intern("loc2");
        let mut s = FactStore::new();
        assert!(s.insert(at, &[l1]));
        assert!(!s.insert(at, &[l1]), "duplicate insert must be a no-op");
        assert!(s.insert(at, &[l2]));
        assert_eq!(s.len(), 2);
        assert!(s.contains(at, &[l1]));
        assert!(!s.contains(at, &[d.intern("loc3")]));
    }

    #[test]
    fn frozen_filter_agrees_with_exact() {
        let mut d = Dict::new();
        let link = d.intern("link");
        let locs: Vec<SymId> = (0..50).map(|i| d.intern(&format!("l{i}"))).collect();
        let mut s = FactStore::new();
        for i in 0..49 {
            s.insert(link, &[locs[i], locs[i + 1]]);
        }
        s.freeze();
        assert!(s.is_frozen());
        // Members present.
        for i in 0..49 {
            assert!(s.contains(link, &[locs[i], locs[i + 1]]));
        }
        // Non-members absent (filter must not produce a false *negative*, and
        // the exact check catches any filter false positive).
        assert!(!s.contains(link, &[locs[0], locs[0]]));
        assert!(!s.contains(link, &[locs[10], locs[40]]));
    }

    #[test]
    fn tuples_for_is_sorted() {
        let mut d = Dict::new();
        let p = d.intern("p");
        let a = d.intern("a");
        let b = d.intern("b");
        let c = d.intern("c");
        let mut s = FactStore::new();
        // Insert out of order.
        s.insert(p, &[c]);
        s.insert(p, &[a]);
        s.insert(p, &[b]);
        let got: Vec<u32> = s.tuples_for(p).map(|v| v[0]).collect();
        assert_eq!(got, vec![a.0, b.0, c.0], "iteration must be sorted by ID");
    }

    #[test]
    fn mutation_invalidates_filter() {
        let mut d = Dict::new();
        let p = d.intern("p");
        let a = d.intern("a");
        let mut s = FactStore::new();
        s.insert(p, &[a]);
        s.freeze();
        assert!(s.is_frozen());
        s.insert(p, &[d.intern("b")]);
        assert!(!s.is_frozen(), "insert must invalidate the frozen filter");
    }
}
