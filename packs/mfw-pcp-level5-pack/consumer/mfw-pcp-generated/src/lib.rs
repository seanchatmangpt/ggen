#![forbid(unsafe_code)]
//! Generated bounded verifier for proof-carrying plan admission, alignment, receipt pairing, replay, consequence admission, and standing closure

pub mod certificates;
pub mod distinctions;
pub mod obligations;
pub mod receipts;
pub mod replay;

pub const GENERATED_CRATE: &str = "mfw-pcp-generated";
pub const GENERATED_VERSION: &str = "26.7.21";
pub const DECLARED_STANDING: &str = "PARTIAL_ALIVE";
