//! Dictionary encoder — interns predicate / object / type strings to compact
//! `u32` IDs.
//!
//! This is the scalable sibling of `bcinr_powl_receipt::intern::ActivityTable`.
//! That table is a fixed 4 KiB byte arena with a `u16` offset table and a
//! linear-scan `intern` — perfect for the ≤256 activity labels of a single
//! receipt, but it panics past 256 entries and its lookup is O(n). A grounder
//! that dictionary-encodes a domain with thousands of objects needs O(1)
//! interning and no hard 256 ceiling, so this table keeps the same *contract*
//! (idempotent intern → dense, stable, insertion-ordered IDs; total
//! `resolve`) while swapping the arena for a `Vec<String>` + `HashMap` — the
//! textbook "qlever" dictionary layout where every term is an integer and all
//! downstream structures store IDs, never bytes.
//!
//! IDs are dense and assigned in first-seen order, so a manufactured encoding
//! is byte-deterministic given a deterministic insertion order.

use std::collections::HashMap;

/// A stable, dense `u32` identifier for an interned string.
///
/// Wrapping the raw `u32` keeps predicate/object/type IDs from being mixed up
/// with array indices or arities at the type level.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct SymId(pub u32);

/// Bidirectional string ↔ `SymId` dictionary.
///
/// `intern` is idempotent: the same string always maps to the same ID, and IDs
/// are handed out densely (`0, 1, 2, …`) in first-seen order.
#[derive(Debug, Clone, Default)]
pub struct Dict {
    /// ID → string (index is the raw `SymId`).
    terms: Vec<String>,
    /// string → ID.
    index: HashMap<String, u32>,
}

impl Dict {
    /// Create an empty dictionary.
    #[must_use]
    pub fn new() -> Self {
        Self {
            terms: Vec::new(),
            index: HashMap::new(),
        }
    }

    /// Intern `s`, returning its stable ID. Idempotent.
    pub fn intern(&mut self, s: &str) -> SymId {
        if let Some(&id) = self.index.get(s) {
            return SymId(id);
        }
        let id = u32::try_from(self.terms.len()).expect("Dict: more than u32::MAX terms");
        self.terms.push(s.to_owned());
        self.index.insert(s.to_owned(), id);
        SymId(id)
    }

    /// Look up an already-interned string, or `None` if it was never interned.
    ///
    /// Distinct from [`intern`](Self::intern): a pure query that never mutates,
    /// so a grounder can ask "is this constant even in the domain?" without
    /// polluting the ID space.
    #[must_use]
    pub fn get(&self, s: &str) -> Option<SymId> {
        self.index.get(s).copied().map(SymId)
    }

    /// Resolve an ID back to its string.
    ///
    /// # Panics
    ///
    /// Panics if `id` was not produced by this dictionary — a programming
    /// error, since IDs are opaque handles minted only by [`intern`](Self::intern).
    #[must_use]
    pub fn resolve(&self, id: SymId) -> &str {
        &self.terms[id.0 as usize]
    }

    /// Number of distinct interned strings.
    ///
    /// Pre-existing dead-code under `make check`/`make clippy`'s
    /// `RUSTFLAGS=-D warnings` (Makefile.toml's `[env]` section): only
    /// this module's own `#[cfg(test)]` tests call it today (`Dict` is a
    /// private, un-re-exported type, `mod dict;` not `pub mod dict;` in
    /// `ground/mod.rs`) — real public-API surface kept for symmetry with
    /// `is_empty`/other interning-table types in this crate, not removed.
    #[must_use]
    #[allow(dead_code)]
    pub fn len(&self) -> usize {
        self.terms.len()
    }

    /// Whether the dictionary is empty. See [`Self::len`]'s doc comment for
    /// why this is `#[allow(dead_code)]`.
    #[must_use]
    #[allow(dead_code)]
    pub fn is_empty(&self) -> bool {
        self.terms.is_empty()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn intern_is_idempotent_and_dense() {
        let mut d = Dict::new();
        let a = d.intern("move");
        let b = d.intern("at");
        let c = d.intern("move"); // duplicate
        assert_eq!(a, c, "duplicate must return same id");
        assert_ne!(a, b);
        assert_eq!(a, SymId(0));
        assert_eq!(b, SymId(1));
        assert_eq!(d.len(), 2);
    }

    #[test]
    fn resolve_round_trips() {
        let mut d = Dict::new();
        let id = d.intern("location-7");
        assert_eq!(d.resolve(id), "location-7");
    }

    #[test]
    fn get_does_not_mutate() {
        let mut d = Dict::new();
        d.intern("known");
        assert_eq!(d.get("known"), Some(SymId(0)));
        assert_eq!(d.get("unknown"), None);
        assert_eq!(d.len(), 1, "get must not intern");
    }

    #[test]
    fn scales_past_the_activitytable_256_ceiling() {
        // The receipt ActivityTable panics at 256; this must not.
        let mut d = Dict::new();
        for i in 0..1000u32 {
            let id = d.intern(&format!("obj{i}"));
            assert_eq!(id, SymId(i));
        }
        assert_eq!(d.len(), 1000);
        assert_eq!(d.resolve(SymId(500)), "obj500");
    }
}
