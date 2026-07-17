//! A fixed-capacity, 512-slot bitset for tracking sets of action-occurrence
//! events within one planning epoch.
//!
//! Backed by 8 `u64` words (`MAX_EPOCH_EVENTS / 64`), matching this
//! workspace's branchless-bitset ethos (see `bcinr-powl`'s `PowlTapeLarge`,
//! which uses the same 8-word predecessor/successor mask shape for up to
//! 512 ops).

use std::fmt;

/// Maximum number of distinct events (action occurrences) one `EventSet`
/// can track.
pub const MAX_EPOCH_EVENTS: usize = 512;

/// Number of `u64` words backing an `EventSet`.
pub const EVENT_WORDS: usize = MAX_EPOCH_EVENTS / 64;

/// A fixed-capacity bitset over `0..MAX_EPOCH_EVENTS`.
#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub struct EventSet {
    words: [u64; EVENT_WORDS],
}

impl EventSet {
    /// The empty set.
    pub const fn empty() -> Self {
        Self {
            words: [0u64; EVENT_WORDS],
        }
    }

    /// Insert `id` into the set in place.
    ///
    /// O(1): `EVENT_WORDS` is a fixed compile-time constant (8), so this is
    /// a single word lookup plus a bitwise OR — no loop, no dependence on
    /// set size.
    ///
    /// # Panics
    /// Panics if `id >= MAX_EPOCH_EVENTS`.
    pub fn insert(&mut self, id: usize) {
        assert!(
            id < MAX_EPOCH_EVENTS,
            "EventSet::insert: id {id} out of range (max {MAX_EPOCH_EVENTS})"
        );
        let word = id / 64;
        let bit = id % 64;
        self.words[word] |= 1u64 << bit;
    }

    /// True iff `id` is a member of the set. Out-of-range IDs are simply
    /// not members (no panic) — only `insert` enforces the range.
    ///
    /// O(1): single word lookup plus a bit test, `EVENT_WORDS` fixed.
    pub fn contains(&self, id: usize) -> bool {
        if id >= MAX_EPOCH_EVENTS {
            return false;
        }
        let word = id / 64;
        let bit = id % 64;
        (self.words[word] >> bit) & 1 == 1
    }

    /// True iff every member of `self` is also a member of `other`.
    ///
    /// O(1): iterates the fixed `EVENT_WORDS` (8) words, not the set's
    /// member count — data-independent instruction count.
    pub fn is_subset_of(&self, other: &Self) -> bool {
        self.words
            .iter()
            .zip(other.words.iter())
            .all(|(a, b)| a & !b == 0)
    }

    /// True iff `self` and `other` share at least one member.
    ///
    /// O(1): iterates the fixed `EVENT_WORDS` (8) words, not the set's
    /// member count.
    pub fn intersects(&self, other: &Self) -> bool {
        self.words
            .iter()
            .zip(other.words.iter())
            .any(|(a, b)| a & b != 0)
    }

    /// The union of `self` and `other`, as a new set.
    ///
    /// O(1): iterates the fixed `EVENT_WORDS` (8) words, not the sets'
    /// member counts.
    pub fn union(&self, other: &Self) -> Self {
        let mut out = [0u64; EVENT_WORDS];
        for ((o, a), b) in out
            .iter_mut()
            .zip(self.words.iter())
            .zip(other.words.iter())
        {
            *o = a | b;
        }
        Self { words: out }
    }

    /// Non-mutating insert: returns a new set equal to `self` plus `id`.
    ///
    /// # Panics
    /// Panics if `id >= MAX_EPOCH_EVENTS`.
    pub fn with(&self, id: usize) -> Self {
        let mut out = *self;
        out.insert(id);
        out
    }

    /// Number of members.
    pub fn len(&self) -> u32 {
        self.words.iter().map(|w| w.count_ones()).sum()
    }

    /// True iff the set has no members.
    pub fn is_empty(&self) -> bool {
        self.words.iter().all(|&w| w == 0)
    }

    /// Iterate over set bit positions in ascending order. O(popcount): each
    /// step clears the lowest set bit of the current word via
    /// `w & (w - 1)` rather than scanning bit-by-bit.
    pub fn iter_stable(&self) -> EventSetIter {
        EventSetIter {
            words: self.words,
            word_idx: 0,
        }
    }
}

impl fmt::Debug for EventSet {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_set().entries(self.iter_stable()).finish()
    }
}

/// Ascending iterator over an `EventSet`'s members, returned by
/// [`EventSet::iter_stable`].
pub struct EventSetIter {
    words: [u64; EVENT_WORDS],
    word_idx: usize,
}

impl Iterator for EventSetIter {
    type Item = usize;

    fn next(&mut self) -> Option<usize> {
        while self.word_idx < EVENT_WORDS {
            let w = self.words[self.word_idx];
            if w == 0 {
                self.word_idx += 1;
                continue;
            }
            let bit = w.trailing_zeros() as usize;
            // Clear the lowest set bit and yield its position.
            self.words[self.word_idx] = w & (w - 1);
            return Some(self.word_idx * 64 + bit);
        }
        None
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn insert_contains_round_trip() {
        let mut s = EventSet::empty();
        assert!(!s.contains(3));
        s.insert(3);
        assert!(s.contains(3));
        assert!(!s.contains(4));
    }

    #[test]
    fn with_is_non_mutating() {
        let s = EventSet::empty();
        let s2 = s.with(5);
        assert!(!s.contains(5));
        assert!(s2.contains(5));
    }

    #[test]
    fn insert_out_of_range_panics() {
        let mut s = EventSet::empty();
        let result = std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
            s.insert(MAX_EPOCH_EVENTS);
        }));
        assert!(result.is_err());
    }

    #[test]
    fn insert_at_boundary_words_works() {
        let mut s = EventSet::empty();
        s.insert(0);
        s.insert(63);
        s.insert(64);
        s.insert(MAX_EPOCH_EVENTS - 1);
        assert!(s.contains(0));
        assert!(s.contains(63));
        assert!(s.contains(64));
        assert!(s.contains(MAX_EPOCH_EVENTS - 1));
        assert_eq!(s.len(), 4);
    }

    #[test]
    fn subset_and_superset_relationships() {
        let ab = EventSet::empty().with(0).with(1);
        let abc = ab.with(2);
        assert!(ab.is_subset_of(&abc));
        assert!(!abc.is_subset_of(&ab));
        assert!(ab.is_subset_of(&ab)); // reflexive
        let empty = EventSet::empty();
        assert!(empty.is_subset_of(&ab));
    }

    #[test]
    fn intersects_detects_shared_members() {
        let ab = EventSet::empty().with(0).with(1);
        let bc = EventSet::empty().with(1).with(2);
        let d = EventSet::empty().with(3);
        assert!(ab.intersects(&bc));
        assert!(!ab.intersects(&d));
    }

    #[test]
    fn union_combines_members() {
        let a = EventSet::empty().with(0);
        let b = EventSet::empty().with(1);
        let union = a.union(&b);
        assert!(union.contains(0));
        assert!(union.contains(1));
        assert_eq!(union.len(), 2);
    }

    #[test]
    fn iter_stable_is_ascending() {
        let s = EventSet::empty().with(200).with(0).with(64).with(63);
        let collected: Vec<usize> = s.iter_stable().collect();
        assert_eq!(collected, vec![0, 63, 64, 200]);
    }

    /// The worked 3-action-capacity scenario: actions A=0, B=1, C=2 are
    /// pairwise concurrency-OK (every 2-element subset is a valid
    /// candidate) but not all three together (the 3-element set is not,
    /// expressed here purely as an EventSet relationship — no
    /// `MinimalNonFace`/`ExecutableConcurrencyComplex` involved, that's
    /// `concurrency.rs`'s job). Here we only check the pure-set-algebra
    /// facts an admission check would need: every pair is a proper subset
    /// of the full triple, and the triple is not a subset of any pair.
    #[test]
    fn worked_three_action_scenario_set_relationships() {
        let a = EventSet::empty().with(0);
        let b = EventSet::empty().with(1);
        let c = EventSet::empty().with(2);
        let ab = a.union(&b);
        let bc = b.union(&c);
        let ac = a.union(&c);
        let abc = ab.union(&c);

        assert_eq!(abc.len(), 3);
        for pair in [ab, bc, ac] {
            assert!(pair.is_subset_of(&abc));
            assert!(!abc.is_subset_of(&pair));
            assert_ne!(pair, abc);
        }
        assert_eq!(abc.iter_stable().collect::<Vec<_>>(), vec![0, 1, 2]);
    }
}
