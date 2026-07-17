//! Append-only JSONL persistence for [`crate::receipt_record::ReceiptRecord`]s.
//!
//! Modeled on `crates/rust-fable-testbed/src/receipt.rs`'s
//! append/read/last-hash pattern: one JSON object per line, opened in
//! append+create mode, no in-place rewrites. The default location
//! (`receipts/receipts.jsonl`) satisfies `src/bin/dod.rs`'s soft check that
//! the `receipts/` directory exists and is non-empty; callers that have a
//! configured directory (e.g. the root binary's `PraxisConfig.receipts.dir`)
//! should pass it to [`ReceiptStore::open`] instead of relying on the default.

use std::{
    fs::OpenOptions,
    io::Write as _,
    path::{Path, PathBuf},
};

use crate::{error::CoreError, receipt_record::ReceiptRecord};

/// Default receipts directory when no configured path is available.
pub const DEFAULT_RECEIPTS_DIR: &str = "receipts";

/// Ledger file name within the receipts directory.
pub const LEDGER_FILE_NAME: &str = "receipts.jsonl";

/// Genesis chain hash (32 zero bytes) used when the ledger has no entries yet.
pub const GENESIS_CHAIN_HASH: [u8; 32] = [0u8; 32];

/// Append-only JSONL receipt ledger.
pub struct ReceiptStore {
    path: PathBuf,
}

impl ReceiptStore {
    /// Open (or prepare to create) a store at `<dir>/receipts.jsonl`,
    /// creating `dir` if it doesn't exist yet. Does not create the ledger
    /// file itself until the first [`ReceiptStore::append`].
    ///
    /// # Errors
    /// Returns [`CoreError::Io`] if `dir` cannot be created.
    pub fn open(dir: impl AsRef<Path>) -> Result<Self, CoreError> {
        let dir = dir.as_ref();
        std::fs::create_dir_all(dir).map_err(|e| CoreError::Io(e.to_string()))?;
        Ok(Self {
            path: dir.join(LEDGER_FILE_NAME),
        })
    }

    /// Open the default store ([`DEFAULT_RECEIPTS_DIR`]`/receipts.jsonl`).
    ///
    /// # Errors
    /// See [`ReceiptStore::open`].
    pub fn open_default() -> Result<Self, CoreError> {
        Self::open(DEFAULT_RECEIPTS_DIR)
    }

    /// The ledger file path.
    #[must_use]
    pub fn path(&self) -> &Path {
        &self.path
    }

    /// Append one record as a single JSON line.
    ///
    /// # Errors
    /// Returns [`CoreError::SerializationFailed`] if `record` fails to
    /// serialize, or [`CoreError::Io`] if the file can't be opened/written.
    pub fn append(&self, record: &ReceiptRecord) -> Result<(), CoreError> {
        let line = serde_json::to_string(record)
            .map_err(|e| CoreError::SerializationFailed(e.to_string()))?;
        let mut file = OpenOptions::new()
            .create(true)
            .append(true)
            .open(&self.path)
            .map_err(|e| CoreError::Io(e.to_string()))?;
        writeln!(file, "{line}").map_err(|e| CoreError::Io(e.to_string()))?;
        Ok(())
    }

    /// Load every record in the ledger, in append order. Returns an empty
    /// `Vec` if the ledger file doesn't exist yet.
    ///
    /// # Errors
    /// Returns [`CoreError::Io`] if an existing ledger can't be read, or
    /// [`CoreError::SerializationFailed`] if a line isn't a valid
    /// [`ReceiptRecord`].
    pub fn load_all(&self) -> Result<Vec<ReceiptRecord>, CoreError> {
        if !self.path.exists() {
            return Ok(Vec::new());
        }
        let content =
            std::fs::read_to_string(&self.path).map_err(|e| CoreError::Io(e.to_string()))?;
        content
            .lines()
            .filter(|l| !l.trim().is_empty())
            .map(|l| {
                serde_json::from_str(l).map_err(|e| CoreError::SerializationFailed(e.to_string()))
            })
            .collect()
    }

    /// The `chain_hash` of the last record, or [`GENESIS_CHAIN_HASH`] if the
    /// ledger is empty (or doesn't exist yet).
    ///
    /// # Errors
    /// See [`ReceiptStore::load_all`], plus [`CoreError::HexDecodeFailed`] if
    /// the last record's `chain_hash_hex` is malformed.
    pub fn last_chain_hash(&self) -> Result<[u8; 32], CoreError> {
        let records = self.load_all()?;
        match records.last() {
            Some(r) => r.chain_hash(),
            None => Ok(GENESIS_CHAIN_HASH),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::law::Andon;

    fn record(instruction_id: u64, prev: [u8; 32], chain: [u8; 32]) -> ReceiptRecord {
        ReceiptRecord {
            version: crate::receipt_record::RECEIPT_RECORD_VERSION,
            instruction_id,
            activity_idx: 0,
            activity: None,
            node_kind: 0,
            ts_ns: instruction_id * 1000,
            duration_ms: None,
            payload_hash_hex: "11".repeat(32),
            prev_chain_hash_hex: hex::encode(prev),
            chain_hash_hex: hex::encode(chain),
            andon: Andon::Green,
            obligation_count: 0,
            object_ids: vec![],
            signature_hex: None,
        }
    }

    #[test]
    fn genesis_chain_hash_when_store_is_empty() {
        let dir = tempfile::tempdir().expect("tempdir");
        let store = ReceiptStore::open(dir.path()).expect("open");
        assert_eq!(
            store.last_chain_hash().expect("genesis"),
            GENESIS_CHAIN_HASH
        );
        assert_eq!(store.load_all().expect("load_all"), Vec::new());
    }

    #[test]
    fn append_and_read_back_last_chain_hash() {
        let dir = tempfile::tempdir().expect("tempdir");
        let store = ReceiptStore::open(dir.path()).expect("open");

        let r1 = record(1, GENESIS_CHAIN_HASH, [1u8; 32]);
        store.append(&r1).expect("append r1");
        assert_eq!(store.last_chain_hash().expect("read after r1"), [1u8; 32]);

        let r2 = record(2, [1u8; 32], [2u8; 32]);
        store.append(&r2).expect("append r2");
        assert_eq!(store.last_chain_hash().expect("read after r2"), [2u8; 32]);

        let all = store.load_all().expect("load_all");
        assert_eq!(all.len(), 2);
        assert_eq!(all[0].instruction_id, 1);
        assert_eq!(all[1].instruction_id, 2);
    }

    #[test]
    fn open_creates_the_directory() {
        let dir = tempfile::tempdir().expect("tempdir");
        let nested = dir.path().join("nested/receipts");
        let store = ReceiptStore::open(&nested).expect("open");
        assert!(nested.is_dir());
        store
            .append(&record(1, GENESIS_CHAIN_HASH, [3u8; 32]))
            .expect("append");
        assert!(store.path().exists());
    }
}
