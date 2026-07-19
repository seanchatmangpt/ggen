//! BLAKE3 receipt chain validation.
//!
//! This module provides generic utilities to validate that any project's BLAKE3
//! receipt chain can be **replayed** and is **tamper-evident**. The invariant
//! is universal: if a project claims BLAKE3 receipt chaining, these utilities
//! must pass, no matter which project.
//!
//! # Replay law
//!
//! For a chain of N entries where entry `i` has:
//! - `prev_hash(i)` = `chain_hash(i-1)` (zeroed for `i=0`)
//! - `content_bytes(i)` = the bytes hashed to produce `chain_hash(i)`
//! - `stored_hash(i)` = the stored BLAKE3 digest
//!
//! The chain is valid iff for every `i`:
//! ```text
//! BLAKE3(prev_hash(i) ‖ content_bytes(i)) == stored_hash(i)
//! ```
//!
//! # Tamper-evidence law
//!
//! Mutating any byte in any entry's `content_bytes` must change `stored_hash`
//! for that entry and all subsequent entries.
//!
//! # Usage with bcinr-powl
//!
//! ```rust,ignore
//! use chicago_tdd_tools::observability::receipt::{Blake3ChainValidator, RawReceiptEntry};
//! use bcinr_powl::receipt_worker::{ReceiptWorker, ENTRY_BYTES};
//!
//! // After draining a ReceiptWorker:
//! let entries: Vec<RawReceiptEntry> = (0..worker.log.len())
//!     .filter_map(|i| worker.log.entry(i).map(|e| RawReceiptEntry::from_bcinr_powl(e, [0u8; 32])))
//!     .collect();
//! Blake3ChainValidator::assert_chain_valid(&entries);
//! Blake3ChainValidator::assert_tamper_evident(&entries);
//! ```

/// A single receipt entry in a generic BLAKE3-chained log.
pub trait Blake3ReceiptEntry {
    /// Bytes fed before `content_bytes` in the BLAKE3 call — the previous
    /// chain hash (32 zeroed bytes for the first entry).
    fn prev_hash(&self) -> [u8; 32];

    /// The entry-specific bytes hashed to produce `stored_hash`.
    /// Does NOT include `prev_hash` — that is prepended by the validator.
    fn content_bytes(&self) -> Vec<u8>;

    /// The stored BLAKE3 chain hash for this entry.
    fn stored_hash(&self) -> [u8; 32];

    /// The replay pointer for this entry, if available.
    ///
    /// Returns `None` by default. Implementations that store a replay pointer
    /// (e.g. bytes 49..57 of the bcinr-powl 57-byte format) should override this.
    fn replay_ptr(&self) -> Option<u64> {
        None
    }
}

// ─── Error type ──────────────────────────────────────────────────────────────

/// Reason a BLAKE3 chain failed validation.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ChainError {
    /// The chain hash at `index` does not match the recomputed value.
    HashMismatch {
        /// Zero-based index of the entry that failed.
        index: usize,
        /// The hash stored in the entry.
        stored: [u8; 32],
        /// The hash we recomputed from the entry's content.
        computed: [u8; 32],
    },
    /// The prev_hash at `index` does not match the stored_hash of the previous entry.
    PrevHashMismatch {
        /// Zero-based index of the entry whose prev_hash is wrong.
        index: usize,
        /// The stored_hash of the prior entry (what prev_hash must equal).
        expected: [u8; 32],
        /// The actual prev_hash the entry claims.
        actual: [u8; 32],
    },
    /// The chain is empty — no entries to validate.
    Empty,
}

impl std::fmt::Display for ChainError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::HashMismatch {
                index,
                stored,
                computed,
            } => write!(
                f,
                "hash mismatch at entry {index}: stored={} computed={}",
                hex_encode(stored),
                hex_encode(computed)
            ),
            Self::PrevHashMismatch {
                index,
                expected,
                actual,
            } => write!(
                f,
                "prev_hash mismatch at entry {index}: expected={} actual={}",
                hex_encode(expected),
                hex_encode(actual)
            ),
            Self::Empty => write!(f, "chain is empty"),
        }
    }
}

fn hex_encode(b: &[u8; 32]) -> String {
    b.iter().fold(String::with_capacity(64), |mut s, byte| {
        use std::fmt::Write as _;
        let _ = write!(s, "{byte:02x}");
        s
    })
}

// ─── Validator ───────────────────────────────────────────────────────────────

/// Validates BLAKE3 receipt chains. Works with any type implementing
/// [`Blake3ReceiptEntry`].
pub struct Blake3ChainValidator;

impl Blake3ChainValidator {
    /// Verify that the chain hashes forward correctly from the first entry.
    ///
    /// Checks two invariants for each entry `i`:
    /// 1. `entry.prev_hash()` matches `stored_hash(i-1)` (or zeros for i=0).
    /// 2. `BLAKE3(prev_hash ‖ content_bytes)` matches `entry.stored_hash()`.
    ///
    /// Returns `Err(ChainError)` on the first failure found.
    pub fn validate_chain<E: Blake3ReceiptEntry>(entries: &[E]) -> Result<(), ChainError> {
        if entries.is_empty() {
            return Err(ChainError::Empty);
        }

        let mut expected_prev = [0u8; 32]; // zeroed for first entry

        for (i, entry) in entries.iter().enumerate() {
            // Rule 1: prev_hash must match the prior stored_hash.
            let actual_prev = entry.prev_hash();
            if actual_prev != expected_prev {
                return Err(ChainError::PrevHashMismatch {
                    index: i,
                    expected: expected_prev,
                    actual: actual_prev,
                });
            }

            // Rule 2: BLAKE3(prev_hash ‖ content) must match stored_hash.
            let mut h = blake3::Hasher::new();
            h.update(&actual_prev);
            h.update(&entry.content_bytes());
            let computed: [u8; 32] = *h.finalize().as_bytes();
            let stored = entry.stored_hash();
            if computed != stored {
                return Err(ChainError::HashMismatch {
                    index: i,
                    stored,
                    computed,
                });
            }

            expected_prev = stored;
        }

        Ok(())
    }

    /// Assert the chain is valid, panicking with a diagnostic message on failure.
    ///
    /// Use this in test code where you want a clear failure message:
    /// ```rust,ignore
    /// Blake3ChainValidator::assert_chain_valid(&entries);
    /// ```
    // Same rationale as cli_proof/receipt.rs's file-level allow: this is an
    // assertion helper (name says so) whose contract is to panic with a
    // diagnostic message on failure, mirroring stdlib `assert!` semantics.
    #[allow(clippy::panic)]
    pub fn assert_chain_valid<E: Blake3ReceiptEntry>(entries: &[E]) {
        if let Err(e) = Self::validate_chain(entries) {
            panic!("BLAKE3 chain replay failed: {e}");
        }
    }

    /// Assert tamper-evidence: confirm that the chain contains enough entries
    /// to exercise the chaining property (at least 2), and that validation passes.
    ///
    /// A valid chain with ≥ 2 entries demonstrates that `prev_hash` threading
    /// is active — entry 1's hash depends on entry 0's hash.
    pub fn assert_tamper_evident<E: Blake3ReceiptEntry>(entries: &[E]) {
        assert!(
            entries.len() >= 2,
            "tamper-evidence requires ≥ 2 entries; got {}",
            entries.len()
        );
        Self::assert_chain_valid(entries);

        // Verify that entries[1].prev_hash == entries[0].stored_hash —
        // confirming the link is actually present.
        let e0_hash = entries[0].stored_hash();
        let e1_prev = entries[1].prev_hash();
        assert_eq!(
            e0_hash, e1_prev,
            "tamper-evidence: entry[1].prev_hash must equal entry[0].stored_hash"
        );
    }
}

// ─── bcinr-powl raw entry adapter ────────────────────────────────────────────

/// Adapter for bcinr-powl's 57-byte receipt entry format:
///
/// | Offset | Field       |
/// |--------|-------------|
/// | 0..8   | run_id LE   |
/// | 8..16  | op_trace LE |
/// | 16     | topo_tag    |
/// | 17..49 | chain_hash  |
/// | 49..57 | replay_ptr LE |
///
/// Chain law: `BLAKE3(prev_chain_hash ‖ run_id_le ‖ op_trace_le ‖ topo_tag)`
#[derive(Clone, Debug)]
pub struct RawReceiptEntry {
    /// Previous chain hash (zeroed for first entry, copied from prior entry's stored_hash).
    pub prev: [u8; 32],
    /// run_id as little-endian bytes.
    pub run_id_le: [u8; 8],
    /// op_trace as little-endian bytes.
    pub op_trace_le: [u8; 8],
    /// topo_tag byte (bit 7 = overflow flag).
    pub topo_tag: u8,
    /// Stored BLAKE3 chain hash (bytes 17..49 of the raw entry).
    pub chain_hash: [u8; 32],
    /// Replay pointer (bytes 49..57 of the raw entry), not included in the hash.
    pub replay_ptr_bytes: [u8; 8],
}

impl RawReceiptEntry {
    /// Build a `RawReceiptEntry` from a raw bcinr-powl 57-byte entry slice
    /// and the previous entry's stored hash (zeroed for the first entry).
    ///
    /// # Panics
    /// Panics if `raw.len() != 57`.
    pub fn from_bcinr_powl(raw: &[u8; 57], prev_hash: [u8; 32]) -> Self {
        let mut run_id_le = [0u8; 8];
        let mut op_trace_le = [0u8; 8];
        let mut chain_hash = [0u8; 32];
        run_id_le.copy_from_slice(&raw[0..8]);
        op_trace_le.copy_from_slice(&raw[8..16]);
        let topo_tag = raw[16];
        chain_hash.copy_from_slice(&raw[17..49]);
        let mut replay_ptr_bytes = [0u8; 8];
        replay_ptr_bytes.copy_from_slice(&raw[49..57]);
        Self {
            prev: prev_hash,
            run_id_le,
            op_trace_le,
            topo_tag,
            chain_hash,
            replay_ptr_bytes,
        }
    }

    /// Build a Vec of `RawReceiptEntry` from a bcinr-powl `ReceiptLog`-style
    /// iterator of raw 57-byte entries.
    pub fn chain_from_raw_entries<'a>(entries: impl Iterator<Item = &'a [u8; 57]>) -> Vec<Self> {
        let mut prev = [0u8; 32];
        let mut result = Vec::new();
        for raw in entries {
            let entry = Self::from_bcinr_powl(raw, prev);
            prev = entry.chain_hash;
            result.push(entry);
        }
        result
    }
}

impl Blake3ReceiptEntry for RawReceiptEntry {
    fn prev_hash(&self) -> [u8; 32] {
        self.prev
    }

    /// Content bytes: run_id_le ‖ op_trace_le ‖ topo_tag
    fn content_bytes(&self) -> Vec<u8> {
        let mut v = Vec::with_capacity(17);
        v.extend_from_slice(&self.run_id_le);
        v.extend_from_slice(&self.op_trace_le);
        v.push(self.topo_tag);
        v
    }

    fn stored_hash(&self) -> [u8; 32] {
        self.chain_hash
    }

    fn replay_ptr(&self) -> Option<u64> {
        Some(u64::from_le_bytes(self.replay_ptr_bytes))
    }
}

// ─── Builder for test fixtures ────────────────────────────────────────────────

/// Build a synthetic BLAKE3-chained receipt log for testing.
///
/// Each entry is constructed such that the chain hashes forward correctly:
/// ```rust,ignore
/// let entries = ReceiptChainBuilder::new()
///     .add_entry(run_id: 1, op_trace: 0b111, topo_tag: 0)
///     .add_entry(run_id: 2, op_trace: 0b11,  topo_tag: 0)
///     .build();
/// Blake3ChainValidator::assert_chain_valid(&entries);
/// ```
pub struct ReceiptChainBuilder {
    entries: Vec<RawReceiptEntry>,
    prev_hash: [u8; 32],
}

impl ReceiptChainBuilder {
    /// Create a new builder with a zeroed initial chain hash.
    pub fn new() -> Self {
        Self {
            entries: Vec::new(),
            prev_hash: [0u8; 32],
        }
    }

    /// Append an entry with the given run_id, op_trace, and topo_tag.
    /// The chain hash is computed automatically.
    pub fn add_entry(mut self, run_id: u64, op_trace: u64, topo_tag: u8) -> Self {
        let run_id_le = run_id.to_le_bytes();
        let op_trace_le = op_trace.to_le_bytes();

        let mut h = blake3::Hasher::new();
        h.update(&self.prev_hash);
        h.update(&run_id_le);
        h.update(&op_trace_le);
        h.update(&[topo_tag]);
        let chain_hash: [u8; 32] = *h.finalize().as_bytes();

        let entry = RawReceiptEntry {
            prev: self.prev_hash,
            run_id_le,
            op_trace_le,
            topo_tag,
            chain_hash,
            replay_ptr_bytes: [0u8; 8],
        };
        self.prev_hash = chain_hash;
        self.entries.push(entry);
        self
    }

    /// Consume the builder and return the chain entries.
    pub fn build(self) -> Vec<RawReceiptEntry> {
        self.entries
    }
}

impl Default for ReceiptChainBuilder {
    fn default() -> Self {
        Self::new()
    }
}
