pub mod autonomics;
pub mod envelope;
pub mod invocation;
pub mod receipt_evidence;

pub use envelope::{emit_fail, emit_pass, Envelope, ENVELOPE_SCHEMA};
pub use invocation::{classify_invocation, InvocationMode, INVOCATION_DEFECT_CLASS};
pub use receipt_evidence::{
    blake3_bytes, blake3_file, blake3_str, composite_hash, receipt_defect, EvidenceBuilder,
    ReceiptEvidence, BLAKE3_EMPTY, RECEIPT_DEFECT_CLASS, RECEIPT_EVIDENCE_SCHEMA,
};

pub struct Receipt {
    pub id: String,
    pub status: String,
}

pub fn run() {}
