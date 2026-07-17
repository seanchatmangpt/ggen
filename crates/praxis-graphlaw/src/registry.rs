//! Process-wide singleton side tables backing the synthetic list/formula
//! terms defined in `crate::term`. These are global `static`s (not fields
//! on `TripleStore`), so their state is shared across *every* `TripleStore`
//! instance in the process, not scoped to one store. This is known, pre-
//! existing coupling (see `crate::term`'s module doc for the design
//! rationale behind representing lists/formulas as synthetic blank nodes
//! keyed into these tables) and is not being fixed in this pass -- this
//! module only relocates the statics verbatim from their prior home in
//! `triples.rs`.

use crate::fastmap::FxHashMap;
use crate::term::{Triple, VarOrTerm};
use once_cell::sync::Lazy;
use std::sync::atomic::AtomicUsize;
use std::sync::Mutex;

pub(crate) static LIST_REGISTRY: Lazy<Mutex<FxHashMap<usize, Vec<VarOrTerm>>>> =
    Lazy::new(|| Mutex::new(FxHashMap::default()));
// Reverse index for `new_list`: structurally-identical member sequences
// must always resolve to the SAME list id, not a fresh one each call --
// otherwise a rule that reconstructs a list term in its head (e.g.
// `?X :is (:good ?Y)` as a consequent) mints a brand-new, never-equal-to-
// anything-previous id on every fixpoint iteration, and `materialize()`
// never converges (each iteration "derives" a structurally-identical but
// id-different fact forever). Found the hard way: a real infinite loop
// while closing the EYE `good_cobbler` corpus case's list-substitution gap.
pub(crate) static LIST_INTERN: Lazy<Mutex<FxHashMap<Vec<VarOrTerm>, usize>>> =
    Lazy::new(|| Mutex::new(FxHashMap::default()));
pub(crate) static FORMULA_REGISTRY: Lazy<Mutex<FxHashMap<usize, Vec<Triple>>>> =
    Lazy::new(|| Mutex::new(FxHashMap::default()));
pub(crate) static SYNTHETIC_COUNTER: AtomicUsize = AtomicUsize::new(0);

// Content-addressed intern table for SPARQL CONSTRUCT template blank nodes
// (`crate::hooks::construct::mint_or_reuse_construct_blank_node`). Keyed by
// (query text, template blank-node label, canonical solution-row encoding)
// -> the fresh label minted the first time that exact key was seen. This
// exists for the SAME reason `LIST_INTERN` above does: `TripleStore::
// materialize()`'s fixpoint loop (`reasoner/mod.rs`) re-evaluates a firing
// hook's CONSTRUCT query on every round until no hook produces a genuinely
// NEW triple; a bare per-call counter would mint a fresh label for the
// IDENTICAL solution on every round, so the round's output would never stop
// looking "new" and materialize() would never converge. Interning by full
// (query, label, row) content gives idempotence across repeated evaluation
// of the SAME solution while still minting a genuinely fresh label the
// first time a given (query, label, row) combination is seen -- which is
// what keeps different solution rows (the aliasing bug this table exists to
// fix) from sharing an identity.
pub(crate) static CONSTRUCT_BNODE_INTERN: Lazy<Mutex<FxHashMap<String, String>>> =
    Lazy::new(|| Mutex::new(FxHashMap::default()));
