//! Domain receipt logic — composition receipts and provenance verification.
//!
//! This module moves receipt logic out of CLI verbs into the domain layer.

pub mod composition;
pub mod provenance;

pub use composition::emit_composition_receipt;
pub use provenance::verify_pack_provenance;
