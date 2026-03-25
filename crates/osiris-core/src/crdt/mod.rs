//! CRDT (Conflict-free Replicated Data Type) module
//!
//! Provides lock-free, merge-friendly data structures for distributed state management.
//! Based on the design specification in CRDT_DESIGN.md.

pub mod lww_register;
pub mod or_set;
pub mod store;

pub use lww_register::LWWRegister;
pub use or_set::OrSet;
pub use store::CRDTStore;
