//! Receipt assembly: append events, compute the rolling BLAKE3 chain hash,
//! serialize and deserialize receipts.
//!
//! Owned by the `chain` phase-2 agent. Codes against `crate::types`.
//!
//! Chain rule (deterministic, append-only):
//!   `chain_hash_0 = blake3(GENESIS)`
//!   `chain_hash_n = blake3(chain_hash_{n-1}.as_bytes() || canonical_bytes(event_n))`
//! The finalized `Receipt.chain_hash` is `chain_hash` after the last appended
//! event. Any change to an event's bytes propagates through every subsequent
//! link, so tampering is unconstructable without breaking the chain.

use std::fs;
use std::path::Path;

use crate::types::{canonical_bytes, Blake3Hash, OperationEvent, Receipt};

/// Format version stamped into assembled receipts.
pub const FORMAT_VERSION: &str = "core/v1";

/// Genesis seed for the rolling chain hash. Binds chains to this release.
///
/// Built from the package version at compile time so the seed always matches
/// the running binary. Cross-version receipt verification is expected to fail.
const GENESIS_SEED_STR: &str =
    concat!("affidavit-v", env!("CARGO_PKG_VERSION"), "-genesis");
pub const GENESIS_SEED: &[u8] = GENESIS_SEED_STR.as_bytes();

/// Default on-disk path for the in-progress working receipt.
pub const WORKING_PATH: &str = ".affi/working.json";

/// Errors raised while assembling, serializing, or persisting receipts.
#[derive(Debug, thiserror::Error)]
pub enum ChainError {
    /// Canonical JSON encoding of a value failed.
    #[error("canonical encoding failed: {0}")]
    Encode(#[source] serde_json::Error),
    /// Deserialization of receipt JSON failed.
    #[error("receipt decode failed: {0}")]
    Decode(#[source] serde_json::Error),
    /// A filesystem operation on the given path failed.
    #[error("io error at {path}: {source}")]
    Io {
        /// Path involved in the failing operation.
        path: String,
        /// Underlying I/O error.
        #[source]
        source: std::io::Error,
    },
}

/// Compute the genesis link `chain_hash_0 = blake3(GENESIS_SEED)`.
fn genesis_hash() -> Blake3Hash {
    Blake3Hash::from_bytes(GENESIS_SEED)
}

/// Fold one event into a running chain hash:
/// `blake3(prev.as_hex().as_bytes() || canonical_bytes(event))`.
fn fold_event(prev: &Blake3Hash, event: &OperationEvent) -> Result<Blake3Hash, ChainError> {
    let event_bytes = canonical_bytes(event).map_err(ChainError::Encode)?;
    let mut buf = Vec::with_capacity(prev.as_hex().len() + event_bytes.len());
    buf.extend_from_slice(prev.as_hex().as_bytes());
    buf.extend_from_slice(&event_bytes);
    Ok(Blake3Hash::from_bytes(&buf))
}

/// Purely recompute the rolling chain hash over an ordered slice of events.
///
/// Used by the verifier to re-derive the chain hash from event bytes alone and
/// compare it against the receipt's stored `chain_hash`.
///
/// # Example: see `examples/chain_build.rs` (run: `cargo run --example chain_build`).
pub fn recompute_chain(events: &[OperationEvent]) -> Result<Blake3Hash, ChainError> {
    let mut acc = genesis_hash();
    for event in events {
        acc = fold_event(&acc, event)?;
    }
    Ok(acc)
}

/// An append-only assembler that maintains a rolling chain hash as events arrive.
///
/// Mirrors a working receipt: events accumulate in order and the hash is folded
/// incrementally so `finalize` is `O(1)` over the stored running hash.
///
/// # Example: see `examples/chain_build.rs` (run: `cargo run --example chain_build`).
#[derive(Debug, Clone)]
pub struct ChainAssembler {
    events: Vec<OperationEvent>,
    running: Blake3Hash,
}

impl Default for ChainAssembler {
    fn default() -> Self {
        Self::new()
    }
}

impl ChainAssembler {
    /// Create an empty assembler seeded with the genesis hash.
    pub fn new() -> Self {
        ChainAssembler {
            events: Vec::new(),
            running: genesis_hash(),
        }
    }

    /// Rebuild an assembler from a previously persisted set of events.
    ///
    /// Recomputes the running hash so the assembler is consistent with the
    /// canonical chain rule regardless of how the events were stored.
    pub fn from_events(events: Vec<OperationEvent>) -> Result<Self, ChainError> {
        let running = recompute_chain(&events)?;
        Ok(ChainAssembler { events, running })
    }

    /// Append one operation-event, folding it into the running chain hash.
    pub fn append(&mut self, event: OperationEvent) -> Result<(), ChainError> {
        self.running = fold_event(&self.running, &event)?;
        self.events.push(event);
        Ok(())
    }

    /// Borrow the events appended so far, in order.
    pub fn events(&self) -> &[OperationEvent] {
        &self.events
    }

    /// Number of events appended so far.
    pub fn len(&self) -> usize {
        self.events.len()
    }

    /// Whether no events have been appended yet.
    pub fn is_empty(&self) -> bool {
        self.events.is_empty()
    }

    /// Finalize into an immutable `Receipt` carrying the final chain hash.
    pub fn finalize(self) -> Receipt {
        Receipt::sealed(FORMAT_VERSION.to_string(), self.events, self.running)
    }
}

/// Content address of a receipt: `blake3(canonical_bytes(receipt))`.
///
/// Used as the assembled receipt's immutable filename.
pub fn content_address(receipt: &Receipt) -> Result<Blake3Hash, ChainError> {
    let bytes = canonical_bytes(receipt).map_err(ChainError::Encode)?;
    Ok(Blake3Hash::from_bytes(&bytes))
}

/// Serialize a receipt to canonical (sorted-key) JSON bytes.
pub fn serialize_receipt(receipt: &Receipt) -> Result<Vec<u8>, ChainError> {
    canonical_bytes(receipt).map_err(ChainError::Encode)
}

/// Deserialize a receipt from JSON bytes.
pub fn deserialize_receipt(bytes: &[u8]) -> Result<Receipt, ChainError> {
    serde_json::from_slice(bytes).map_err(ChainError::Decode)
}

/// Persist the working set of events to `.affi/working.json` as canonical JSON.
///
/// Creates the parent directory if it does not yet exist.
pub fn save_working(events: &[OperationEvent]) -> Result<(), ChainError> {
    let bytes = canonical_bytes(&events.to_vec()).map_err(ChainError::Encode)?;
    write_file(Path::new(WORKING_PATH), &bytes)
}

/// Load the working set of events from `.affi/working.json`.
///
/// Returns an empty vector when no working receipt exists yet.
pub fn load_working() -> Result<Vec<OperationEvent>, ChainError> {
    let path = Path::new(WORKING_PATH);
    if !path.exists() {
        return Ok(Vec::new());
    }
    let bytes = fs::read(path).map_err(|source| ChainError::Io {
        path: WORKING_PATH.to_string(),
        source,
    })?;
    serde_json::from_slice(&bytes).map_err(ChainError::Decode)
}

/// Write a finalized receipt to `path` as canonical JSON bytes.
pub fn save_receipt(receipt: &Receipt, path: &Path) -> Result<(), ChainError> {
    let bytes = serialize_receipt(receipt)?;
    write_file(path, &bytes)
}

/// Write bytes to `path`, creating parent directories as needed.
fn write_file(path: &Path, bytes: &[u8]) -> Result<(), ChainError> {
    if let Some(parent) = path.parent() {
        if !parent.as_os_str().is_empty() {
            fs::create_dir_all(parent).map_err(|source| ChainError::Io {
                path: parent.display().to_string(),
                source,
            })?;
        }
    }
    fs::write(path, bytes).map_err(|source| ChainError::Io {
        path: path.display().to_string(),
        source,
    })
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::types::{ObjectRef, OperationEvent};

    fn event(seq: u64, payload: &[u8]) -> OperationEvent {
        OperationEvent {
            id: format!("e{seq}"),
            seq,
            event_type: "test.op".to_string(),
            objects: vec![ObjectRef {
                id: format!("obj{seq}"),
                obj_type: "artifact".to_string(),
                qualifier: Some("input".to_string()),
            }],
            payload_commitment: Blake3Hash::from_bytes(payload),
        }
    }

    #[test]
    fn chain_empty_equals_genesis() {
        assert_eq!(recompute_chain(&[]).unwrap(), genesis_hash());
    }

    #[test]
    fn chain_append_matches_recompute() {
        let mut asm = ChainAssembler::new();
        let evs = vec![event(0, b"a"), event(1, b"b"), event(2, b"c")];
        for e in &evs {
            asm.append(e.clone()).unwrap();
        }
        let recomputed = recompute_chain(&evs).unwrap();
        let receipt = asm.finalize();
        assert_eq!(receipt.chain_hash, recomputed);
    }

    #[test]
    fn chain_is_deterministic() {
        let evs = vec![event(0, b"x"), event(1, b"y")];
        assert_eq!(
            recompute_chain(&evs).unwrap(),
            recompute_chain(&evs).unwrap()
        );
    }

    #[test]
    fn chain_tamper_changes_hash() {
        let evs = vec![event(0, b"a"), event(1, b"b")];
        let honest = recompute_chain(&evs).unwrap();

        let mut tampered = evs.clone();
        tampered[0].payload_commitment = Blake3Hash::from_bytes(b"forged");
        let forged = recompute_chain(&tampered).unwrap();

        assert_ne!(
            honest, forged,
            "tampering a commitment must break the chain"
        );
    }

    #[test]
    fn chain_order_matters() {
        let a = vec![event(0, b"a"), event(1, b"b")];
        let b = vec![event(1, b"b"), event(0, b"a")];
        assert_ne!(recompute_chain(&a).unwrap(), recompute_chain(&b).unwrap());
    }

    #[test]
    fn receipt_round_trips() {
        let mut asm = ChainAssembler::new();
        asm.append(event(0, b"a")).unwrap();
        let receipt = asm.finalize();

        let bytes = serialize_receipt(&receipt).unwrap();
        let back = deserialize_receipt(&bytes).unwrap();
        assert_eq!(receipt, back);
    }

    #[test]
    fn content_address_is_stable() {
        let mut asm = ChainAssembler::new();
        asm.append(event(0, b"a")).unwrap();
        let receipt = asm.finalize();
        assert_eq!(
            content_address(&receipt).unwrap(),
            content_address(&receipt).unwrap()
        );
    }

    #[test]
    fn from_events_reconstructs_running_hash() {
        let evs = vec![event(0, b"a"), event(1, b"b")];
        let asm = ChainAssembler::from_events(evs.clone()).unwrap();
        assert_eq!(asm.finalize().chain_hash, recompute_chain(&evs).unwrap());
    }
}
