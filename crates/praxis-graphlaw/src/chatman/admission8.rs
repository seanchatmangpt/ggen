//! Admission tables for hook patterns and OCEL events over an 8-bit
//! constraint state.
//!
//! An [`AdmissionTable8`] precomputes, for every possible 8-bit constraint
//! state, whether an invocation is admitted and what the successor state is.
//! Lookup is a single branchless indexed load into a 256-entry array — the
//! hot path performs no comparisons, no hashing, and no allocation.
//!
//! Identity discipline: the table hash is computed through
//! [`wasm4pm_compat::hash::blake3_hex`] over a canonical, field-tagged byte
//! encoding of the entries in index order followed by the sorted constraint
//! names. No wall clock and no randomness participate anywhere in this
//! module; identical inputs yield byte-identical tables and hashes.
//!
//! The 8-bit width is not a convenience: it is the *Need9 means split* law.
//! The constitutional owner of that law is
//! `wasm4pm_compat::law::ConditionCell<8>`; a zero-sized [`Law8`] witness
//! field stands in for it here (see the [`Law8`] doc comment for why).

use serde::{Deserialize, Serialize};
use wasm4pm_compat::hash::blake3_hex;

use super::abi::Refusal;

/// Zero-sized witness of the *Need9 means split* law: at most 8 primary
/// condition bits per admission table.
///
/// The constitutional owner of this law is
/// `wasm4pm_compat::law::ConditionCell<8>` (Blue River Dam covenant).
/// Instantiating `ConditionCell<8>` from this crate requires
/// `generic_const_exprs` (the `Require<{ BITS <= 8 }>: IsTrue` bound cannot
/// be discharged without the feature), and enabling that incomplete feature
/// crate-wide for one witness field is not warranted; this private marker
/// carries the same intent, and the >8 bound is enforced at runtime by
/// [`AdmissionTable8::from_masks`] via [`Refusal::WarmPathRequired`].
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Default)]
struct Law8;

/// Version tag mixed into every table hash so a future encoding change can
/// never collide with the current scheme.
const TABLE_HASH_VERSION: &[u8] = b"admission-table8-v1";

/// An 8-bit constraint mask: bit `i` set means constraint `i` holds.
///
/// Newtype over `u8` so masks are not silently confused with states or bit
/// indices at call sites.
#[derive(
    Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize, Default,
)]
#[serde(transparent)]
pub struct ConstraintMask(pub u8);

/// One precomputed admission decision for a single 8-bit state.
///
/// On admission, the successor state is `(state | next_state_or) &
/// next_state_and` — a set-then-clear update expressed as two masks so the
/// transition itself is branchless.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct Admission8 {
    /// Whether a state indexing this entry is admitted.
    pub admit: bool,
    /// Bits OR-ed into the state on admission (constraints set by the step).
    pub next_state_or: u8,
    /// Bits AND-ed into the state on admission (constraints cleared by the
    /// step are the zero bits here).
    pub next_state_and: u8,
}

/// Binds a triple predicate to a constraint bit: a triple whose predicate
/// equals `predicate` sets bit `bit` (taken modulo 8) in the state mask.
///
/// Generic over the predicate representation so the Triple8 lane's `Term8`
/// plugs in without this module depending on it before it lands.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct ConstraintBinding<P> {
    /// Predicate that triggers the constraint bit.
    pub predicate: P,
    /// Bit index in `0..8`; values `>= 8` are folded via `bit & 7` so the
    /// computation stays branchless and in-range by construction.
    pub bit: u8,
}

/// Access to a triple's predicate, implemented by the Triple8 lane's
/// `RDFTriple8` (predicate type `Term8`). Kept as a trait so this module
/// compiles independently of the triple8 lane's landing order.
pub trait PredicateBearer {
    /// The predicate term type.
    type Predicate: PartialEq;
    /// The predicate of this triple.
    fn predicate(&self) -> &Self::Predicate;
}

/// Folds triples into an 8-bit constraint state, branchlessly.
///
/// For every (triple, binding) pair, bit `binding.bit & 7` is OR-ed into the
/// result when the triple's predicate equals the binding's predicate. The
/// comparison result is converted to `u8` and shifted — no data-dependent
/// branch is taken per pair.
///
/// # Complexity
/// O(|triples| * |bindings|) predicate comparisons; O(1) space. Bindings are
/// bounded at 8 by the Need9 law, so this is O(8 * |triples|) in practice.
pub fn state_mask<T: PredicateBearer>(
    triples: &[T], bindings: &[ConstraintBinding<T::Predicate>],
) -> u8 {
    let mut state: u8 = 0;
    for triple in triples {
        // O(|bindings|) per triple; |bindings| <= 8 under the Need9 law.
        for binding in bindings {
            let hit = u8::from(*triple.predicate() == binding.predicate);
            state |= hit << (binding.bit & 7);
        }
    }
    state
}

/// A fully precomputed admission table over the 256 possible 8-bit states.
///
/// Constructed once via [`AdmissionTable8::from_masks`]; thereafter every
/// lookup is a branchless indexed load. The table hash binds the entries and
/// the sorted constraint names, so two tables agree on their hash iff they
/// encode the same law.
pub struct AdmissionTable8 {
    /// One entry per possible 8-bit state, indexed directly by the state.
    entries: Box<[Admission8; 256]>,
    /// BLAKE3 hex hash over the canonical encoding of entries + names.
    hash: String,
    /// Sorted constraint names, at most 8 (Need9 law).
    constraint_names: Vec<String>,
    /// Zero-sized constitutional witness: at most 8 primary condition bits.
    _law: Law8,
}

impl core::fmt::Debug for AdmissionTable8 {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        f.debug_struct("AdmissionTable8")
            .field("hash", &self.hash)
            .field("constraint_names", &self.constraint_names)
            .finish_non_exhaustive()
    }
}

impl Clone for AdmissionTable8 {
    fn clone(&self) -> Self {
        AdmissionTable8 {
            entries: self.entries.clone(),
            hash: self.hash.clone(),
            constraint_names: self.constraint_names.clone(),
            _law: Law8,
        }
    }
}

impl AdmissionTable8 {
    /// Builds the table from constraint masks.
    ///
    /// A state `s` is admitted iff `(s & required) == required` and
    /// `(s & forbidden) == 0`. On admission the successor state is
    /// `(s | set_on_admit) & !clear_on_admit`.
    ///
    /// Refuses with [`Refusal::WarmPathRequired`] when more than 8 constraint
    /// names are supplied — a ninth condition bit means split (Need9 law),
    /// which is warm-path work, not an 8-bit table.
    ///
    /// # Complexity
    /// O(256) entry precomputation + O(n log n) name sort (n <= 8) +
    /// O(256) hashing work; O(256) space.
    pub fn from_masks(
        constraint_names: Vec<String>, required_mask: ConstraintMask,
        forbidden_mask: ConstraintMask, set_on_admit: ConstraintMask,
        clear_on_admit: ConstraintMask,
    ) -> Result<Self, Refusal> {
        if constraint_names.len() > 8 {
            return Err(Refusal::WarmPathRequired(format!(
                "admission table supports at most 8 constraint names (Need9 means split), got {}",
                constraint_names.len()
            )));
        }
        let mut names = constraint_names;
        // O(n log n), n <= 8: canonical name order for hashing and display.
        names.sort();

        let next_or = set_on_admit.0;
        let next_and = !clear_on_admit.0;
        let mut entries = Box::new(
            [Admission8 {
                admit: false,
                next_state_or: next_or,
                next_state_and: next_and,
            }; 256],
        );
        // O(256): precompute every state's decision.
        for state in 0u16..=255 {
            let s = state as u8;
            entries[state as usize].admit =
                (s & required_mask.0) == required_mask.0 && (s & forbidden_mask.0) == 0;
        }

        let hash = table_hash(&entries, &names);
        Ok(AdmissionTable8 {
            entries,
            hash,
            constraint_names: names,
            _law: Law8,
        })
    }

    /// Branchless admission lookup: a `u8` index into a 256-entry array
    /// cannot be out of bounds, so this compiles to a single indexed load.
    ///
    /// # Complexity
    /// O(1), no branches, no allocation.
    #[inline(always)]
    pub fn lookup(&self, state: u8) -> Admission8 {
        self.entries[state as usize]
    }

    /// Admits a state, returning the successor state, or refuses with
    /// [`Refusal::HookPatternNotAdmitted`] carrying the offending state.
    ///
    /// # Complexity
    /// O(1) on the admit path; the refusal path allocates the context string.
    pub fn admit(&self, state: u8) -> Result<u8, Refusal> {
        let entry = self.lookup(state);
        if entry.admit {
            Ok((state | entry.next_state_or) & entry.next_state_and)
        } else {
            Err(Refusal::HookPatternNotAdmitted(format!(
                "state 0b{state:08b} (0x{state:02x}) not admitted by table {}",
                self.hash
            )))
        }
    }

    /// Admits a batch of states in order; the first non-admitted state
    /// refuses the whole batch with [`Refusal::OcelEventNotAdmitted`].
    ///
    /// # Complexity
    /// O(|states|) lookups; O(|states|) space for the successor vector.
    pub fn admit_all(&self, states: &[u8]) -> Result<Vec<u8>, Refusal> {
        let mut out = Vec::with_capacity(states.len());
        for (i, &state) in states.iter().enumerate() {
            let entry = self.lookup(state);
            if !entry.admit {
                return Err(Refusal::OcelEventNotAdmitted(format!(
                    "event {i}: state 0b{state:08b} (0x{state:02x}) not admitted by table {}",
                    self.hash
                )));
            }
            out.push((state | entry.next_state_or) & entry.next_state_and);
        }
        Ok(out)
    }

    /// Verifies a claimed table hash, refusing with
    /// [`Refusal::AdmissionTableMismatch`] on disagreement.
    ///
    /// # Complexity
    /// O(|hash|) string comparison.
    pub fn verify_hash(&self, claimed: &str) -> Result<(), Refusal> {
        if self.hash == claimed {
            Ok(())
        } else {
            Err(Refusal::AdmissionTableMismatch(format!(
                "claimed table hash {claimed} does not match computed {}",
                self.hash
            )))
        }
    }

    /// The BLAKE3 hex hash binding entries and sorted constraint names.
    pub fn hash(&self) -> &str {
        &self.hash
    }

    /// The sorted constraint names (at most 8).
    pub fn constraint_names(&self) -> &[String] {
        &self.constraint_names
    }
}

/// Canonical, injective table hash: version tag, then all 256 entries in
/// index order (index, admit, or-mask, and-mask), then length-prefixed
/// sorted names.
///
/// # Complexity
/// O(256 + total name bytes) buffer build + one BLAKE3 pass.
fn table_hash(entries: &[Admission8; 256], sorted_names: &[String]) -> String {
    let mut material = Vec::with_capacity(TABLE_HASH_VERSION.len() + 256 * 4 + 64);
    material.extend_from_slice(TABLE_HASH_VERSION);
    // O(256): entries in index order — canonical by construction.
    for (index, entry) in entries.iter().enumerate() {
        material.push(index as u8);
        material.push(u8::from(entry.admit));
        material.push(entry.next_state_or);
        material.push(entry.next_state_and);
    }
    material.extend_from_slice(&(sorted_names.len() as u64).to_be_bytes());
    // O(n), n <= 8: names are pre-sorted; length-prefix keeps encoding injective.
    for name in sorted_names {
        material.extend_from_slice(&(name.len() as u64).to_be_bytes());
        material.extend_from_slice(name.as_bytes());
    }
    blake3_hex(&material)
}

#[cfg(test)]
mod tests {
    use super::*;

    /// Minimal predicate bearer standing in for the Triple8 lane's
    /// `RDFTriple8` (predicate type `Term8`).
    struct TestTriple {
        predicate: u32,
    }

    impl PredicateBearer for TestTriple {
        type Predicate = u32;
        fn predicate(&self) -> &u32 {
            &self.predicate
        }
    }

    fn names(n: usize) -> Vec<String> {
        (0..n).map(|i| format!("constraint-{i}")).collect()
    }

    fn table() -> Result<AdmissionTable8, Refusal> {
        AdmissionTable8::from_masks(
            names(3),
            ConstraintMask(0b0000_0011), // bits 0 and 1 required
            ConstraintMask(0b0000_0100), // bit 2 forbidden
            ConstraintMask(0b0000_1000), // bit 3 set on admit
            ConstraintMask(0b0000_0001), // bit 0 cleared on admit
        )
    }

    #[test]
    fn exhaustive_256_state_truth_check() -> Result<(), Refusal> {
        // Arrange
        let required = 0b0000_0011u8;
        let forbidden = 0b0000_0100u8;
        let t = table()?;
        // Act + Assert: every state's precomputed decision matches arithmetic.
        for state in 0u16..=255 {
            let s = state as u8;
            let expected = (s & required) == required && (s & forbidden) == 0;
            let entry = t.lookup(s);
            assert_eq!(entry.admit, expected, "state 0b{s:08b}");
            match t.admit(s) {
                Ok(next) => {
                    assert!(expected, "state 0b{s:08b} admitted but arithmetic refuses");
                    assert_eq!(next, (s | 0b0000_1000) & !0b0000_0001);
                }
                Err(Refusal::HookPatternNotAdmitted(ctx)) => {
                    assert!(!expected, "state 0b{s:08b} refused but arithmetic admits");
                    assert!(ctx.contains(&format!("0b{s:08b}")));
                }
                Err(other) => assert_eq!(other.name(), "HookPatternNotAdmitted"),
            }
        }
        Ok(())
    }

    #[test]
    fn more_than_eight_names_refused_as_warm_path() {
        // Arrange + Act
        let result = AdmissionTable8::from_masks(
            names(9),
            ConstraintMask(0),
            ConstraintMask(0),
            ConstraintMask(0),
            ConstraintMask(0),
        );
        // Assert
        assert!(matches!(result, Err(Refusal::WarmPathRequired(_))));
    }

    #[test]
    fn hash_is_deterministic_and_name_sensitive() -> Result<(), Refusal> {
        // Arrange + Act: same inputs twice, plus one with different names.
        let a = table()?;
        let b = table()?;
        let c = AdmissionTable8::from_masks(
            names(4),
            ConstraintMask(0b0000_0011),
            ConstraintMask(0b0000_0100),
            ConstraintMask(0b0000_1000),
            ConstraintMask(0b0000_0001),
        )?;
        // Assert
        assert_eq!(a.hash(), b.hash());
        assert_ne!(a.hash(), c.hash());
        a.verify_hash(b.hash())?;
        Ok(())
    }

    #[test]
    fn verify_hash_mismatch_refuses() -> Result<(), Refusal> {
        // Arrange
        let t = table()?;
        // Act
        let result = t.verify_hash("not-the-hash");
        // Assert
        assert!(matches!(result, Err(Refusal::AdmissionTableMismatch(_))));
        Ok(())
    }

    #[test]
    fn constraint_names_are_sorted() -> Result<(), Refusal> {
        // Arrange: names supplied out of order.
        let t = AdmissionTable8::from_masks(
            vec!["b".to_string(), "a".to_string(), "c".to_string()],
            ConstraintMask(0),
            ConstraintMask(0),
            ConstraintMask(0),
            ConstraintMask(0),
        )?;
        // Assert
        assert_eq!(t.constraint_names(), ["a", "b", "c"]);
        Ok(())
    }

    #[test]
    fn admit_all_stops_at_first_refusal() -> Result<(), Refusal> {
        // Arrange: 0b011 admitted; 0b111 has forbidden bit 2.
        let t = table()?;
        // Act
        let ok = t.admit_all(&[0b0000_0011, 0b0000_1011])?;
        let bad = t.admit_all(&[0b0000_0011, 0b0000_0111, 0b0000_0011]);
        // Assert
        assert_eq!(ok, vec![0b0000_1010, 0b0000_1010]);
        match bad {
            Err(Refusal::OcelEventNotAdmitted(ctx)) => assert!(ctx.contains("event 1")),
            other => {
                return Err(Refusal::ValidationFailed(format!(
                    "expected OcelEventNotAdmitted, got {other:?}"
                )))
            }
        }
        Ok(())
    }

    #[test]
    fn state_mask_is_branchless_or_of_matching_bits() {
        // Arrange
        let triples = [
            TestTriple { predicate: 7 },
            TestTriple { predicate: 9 },
            TestTriple { predicate: 42 },
        ];
        let bindings = [
            ConstraintBinding {
                predicate: 7u32,
                bit: 0,
            },
            ConstraintBinding {
                predicate: 42u32,
                bit: 5,
            },
            ConstraintBinding {
                predicate: 99u32,
                bit: 3,
            },
            // bit 11 folds to 11 & 7 == 3; predicate 9 matches.
            ConstraintBinding {
                predicate: 9u32,
                bit: 11,
            },
        ];
        // Act
        let state = state_mask(&triples, &bindings);
        // Assert: bits 0 (pred 7), 5 (pred 42), 3 (pred 9 via folded bit).
        assert_eq!(state, 0b0010_1001);
        // No triples or no bindings yield the empty state.
        assert_eq!(state_mask::<TestTriple>(&[], &bindings), 0);
        assert_eq!(state_mask(&triples, &[]), 0);
    }
}
