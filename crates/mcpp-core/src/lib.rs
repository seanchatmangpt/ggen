//! MCPP core — canonical types promoted from `chatmangpt-mcpp-v2-cell`.
//!
//! Closes `portfolio-obl-0002-promote-v2-constraints-into-canonical-ggen-mcpp`
//! by absorbing the v2 receipt/protocol surfaces into the canonical
//! ggen MCPP implementation.

pub mod autonomics;
pub mod blake3_hash;
pub mod envelope;
pub mod exit_code;
pub mod invocation;
pub mod receipt;

pub use blake3_hash::{blake3_file, blake3_str, causality_chain};
pub use envelope::{Envelope, SR_RESULT_SCHEMA};
pub use invocation::{classify, InvocationMode};
pub use receipt::{AndonClass, Receipt, ReceiptEvidence, VerifyInputs, VerifyOutcome};

pub fn run() {}
