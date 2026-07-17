//! [`ReceiptAssertions`] — reads and verifies signed receipt JSON files on disk.

use std::fs;
use std::path::{Path, PathBuf};

use serde_json::Value;

/// Reads and verifies signed receipt JSON files from a directory.
///
/// Receipts are JSON files matching `*.json` in the given directory,
/// sorted by modification time (newest first).
pub struct ReceiptAssertions {
    dir: PathBuf,
    receipts: Vec<Value>,
}

impl ReceiptAssertions {
    /// Load all `*.json` files from `dir` as receipts, sorted newest-first by mtime.
    ///
    /// # Errors
    ///
    /// Returns an error when the directory cannot be read or any JSON file
    /// cannot be parsed.
    pub fn from_dir(dir: impl AsRef<Path>) -> Result<Self, Box<dyn std::error::Error>> {
        let dir = dir.as_ref().to_path_buf();

        // Collect (mtime, path) pairs for *.json files.
        let mut entries: Vec<(std::time::SystemTime, PathBuf)> = fs::read_dir(&dir)?
            .filter_map(|e| e.ok())
            .filter(|e| {
                e.path()
                    .extension()
                    .is_some_and(|ext| ext.eq_ignore_ascii_case("json"))
            })
            .filter_map(|e| {
                let mtime = e.metadata().ok()?.modified().ok()?;
                Some((mtime, e.path()))
            })
            .collect();

        // Newest first.
        entries.sort_by(|a, b| b.0.cmp(&a.0));

        let mut receipts = Vec::with_capacity(entries.len());
        for (_, path) in &entries {
            let text = fs::read_to_string(path)?;
            let value: Value = serde_json::from_str(&text)?;
            receipts.push(value);
        }

        Ok(Self { dir, receipts })
    }

    /// Panic if the number of loaded receipts is not exactly `expected`.
    pub fn assert_count(&self, expected: usize) -> &Self {
        if self.receipts.len() != expected {
            panic!(
                "expected {} receipt(s) in {} but found {}",
                expected,
                self.dir.display(),
                self.receipts.len()
            );
        }
        self
    }

    /// Panic if the latest receipt does not have `"is_valid": true`.
    pub fn assert_latest_is_valid(&self) -> &Self {
        let receipt = self
            .latest()
            .unwrap_or_else(|| panic!("no receipts found in {}", self.dir.display()));
        let is_valid = receipt
            .get("is_valid")
            .and_then(Value::as_bool)
            .unwrap_or(false);
        if !is_valid {
            panic!(
                "expected latest receipt to have is_valid=true\nreceipt:\n{}",
                serde_json::to_string_pretty(receipt).unwrap_or_default()
            );
        }
        self
    }

    /// Panic if the latest receipt's `"signature"` field is absent or empty.
    pub fn assert_latest_signature_nonempty(&self) -> &Self {
        let receipt = self
            .latest()
            .unwrap_or_else(|| panic!("no receipts found in {}", self.dir.display()));
        let sig = receipt
            .get("signature")
            .and_then(Value::as_str)
            .unwrap_or("");
        if sig.is_empty() {
            panic!(
                "expected latest receipt to have a non-empty 'signature' field\nreceipt:\n{}",
                serde_json::to_string_pretty(receipt).unwrap_or_default()
            );
        }
        self
    }

    /// Panic if the latest receipt's `"input_hashes"` array contains no entry
    /// for which `predicate` returns `true`.
    pub fn assert_latest_has_input(&self, predicate: impl Fn(&str) -> bool) -> &Self {
        let receipt = self
            .latest()
            .unwrap_or_else(|| panic!("no receipts found in {}", self.dir.display()));
        let hashes = receipt
            .get("input_hashes")
            .and_then(Value::as_array)
            .unwrap_or_else(|| {
                panic!(
                    "latest receipt has no 'input_hashes' array\nreceipt:\n{}",
                    serde_json::to_string_pretty(receipt).unwrap_or_default()
                )
            });
        let found = hashes
            .iter()
            .filter_map(Value::as_str)
            .any(|s| predicate(s));
        if !found {
            panic!(
                "no entry in latest receipt's 'input_hashes' matched the predicate\nhashes: {:?}",
                hashes
            );
        }
        self
    }

    /// Panic if the latest receipt's `"previous_receipt_hash"` does not match
    /// the hash field of the second-most-recent receipt.
    ///
    /// This verifies the receipt chain is properly linked.
    pub fn assert_chain_linked(&self) -> &Self {
        if self.receipts.len() < 2 {
            panic!(
                "need at least 2 receipts to assert chain linkage, found {}",
                self.receipts.len()
            );
        }
        let latest = &self.receipts[0];
        let second = &self.receipts[1];

        let prev_hash = latest
            .get("previous_receipt_hash")
            .and_then(Value::as_str)
            .unwrap_or_else(|| {
                panic!(
                    "latest receipt missing 'previous_receipt_hash'\nreceipt:\n{}",
                    serde_json::to_string_pretty(latest).unwrap_or_default()
                )
            });

        let second_hash = second
            .get("hash")
            .and_then(Value::as_str)
            .unwrap_or_else(|| {
                panic!(
                    "second receipt missing 'hash' field\nreceipt:\n{}",
                    serde_json::to_string_pretty(second).unwrap_or_default()
                )
            });

        if prev_hash != second_hash {
            panic!(
                "receipt chain broken: latest.previous_receipt_hash={:?} \
                 != second.hash={:?}",
                prev_hash, second_hash
            );
        }
        self
    }

    /// Panic if the two most-recent receipts share the same `"hash"` value.
    ///
    /// Used in the DriftProof pattern:
    /// run → mutate → re-run → assert receipt hash changed.
    pub fn assert_receipt_hash_changed(&self) -> &Self {
        if self.receipts.len() < 2 {
            panic!(
                "need at least 2 receipts to assert hash changed, found {}",
                self.receipts.len()
            );
        }
        let hash_of = |r: &Value| {
            r.get("hash")
                .and_then(Value::as_str)
                .unwrap_or("")
                .to_owned()
        };
        let latest_hash = hash_of(&self.receipts[0]);
        let prev_hash = hash_of(&self.receipts[1]);

        if latest_hash == prev_hash {
            panic!(
                "expected receipt hash to change after mutation but both receipts \
                 have hash={:?}",
                latest_hash
            );
        }
        self
    }

    /// Return the most-recent receipt, or `None` if no receipts were loaded.
    fn latest(&self) -> Option<&Value> {
        self.receipts.first()
    }
}
