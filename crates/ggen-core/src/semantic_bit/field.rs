//! Field8: The core 8-bit semantic machine word

use serde::{Deserialize, Serialize};
use std::marker::PhantomData;

/// An 8-bit root field representing a complete semantic automaton.
///
/// L is the Law (the automaton logic).
/// P is the Phase (the current typestate).
#[repr(transparent)]
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub struct Field8<L, P> {
    pub raw: u8,
    _marker: PhantomData<(L, P)>,
}

impl<L, P> Field8<L, P> {
    /// Create a new field from a raw byte.
    /// In production, use the typestate constructors.
    pub const fn new_unchecked(raw: u8) -> Self {
        Self {
            raw,
            _marker: PhantomData,
        }
    }
}
