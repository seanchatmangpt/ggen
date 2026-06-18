//! Honest, lightweight provenance receipt for reverse-pipeline operations.
//!
//! Distinct from the forward pipeline's `BuildReceipt` (its epoch / passes /
//! packs fields do not apply to a reverse operation, and forcing reverse data
//! into that shape would itself be contract drift). A [`ReverseReceipt`] binds
//! real input file hashes to real output hashes for one reverse run.
//!
//! No `signature` field is claimed: these are *unsigned provenance records*,
//! not cryptographic attestations. We do not assert signing we are not doing
//! (see `.claude/rules/coding-agent-mistakes.md` §1.1 / §4.2).

use std::collections::BTreeMap;
use std::path::Path;

use serde::{Deserialize, Serialize};
use sha2::{Digest, Sha256};

use crate::utils::error::{Error, Result};

/// A provenance record for a single reverse-pipeline run.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ReverseReceipt {
    /// Operation name, e.g. `"reverse-scan"`.
    pub operation: String,
    /// Unique, non-nil operation id (UUID v4).
    pub operation_id: String,
    /// RFC-3339 timestamp of the run.
    pub timestamp: String,
    /// `CARGO_PKG_VERSION` of the emitting crate.
    pub tool_version: String,
    /// Map of `input key -> sha256 hex` for every consumed input.
    pub input_hashes: BTreeMap<String, String>,
    /// Map of `output key -> sha256 hex` for every produced artifact.
    pub output_hashes: BTreeMap<String, String>,
}

impl ReverseReceipt {
    /// Begin a new receipt with a fresh operation id and current timestamp.
    pub fn new(operation: &str) -> Self {
        Self {
            operation: operation.to_string(),
            operation_id: uuid::Uuid::new_v4().to_string(),
            timestamp: chrono::Utc::now().to_rfc3339(),
            tool_version: env!("CARGO_PKG_VERSION").to_string(),
            input_hashes: BTreeMap::new(),
            output_hashes: BTreeMap::new(),
        }
    }

    /// Record an input artifact by its real content hash.
    pub fn add_input(&mut self, key: &str, content: &[u8]) {
        self.input_hashes
            .insert(key.to_string(), sha256_hex(content));
    }

    /// Record an output artifact by its real content hash.
    pub fn add_output(&mut self, key: &str, content: &[u8]) {
        self.output_hashes
            .insert(key.to_string(), sha256_hex(content));
    }

    /// Write the receipt as pretty JSON, creating parent directories.
    pub fn write_to(&self, path: &Path) -> Result<()> {
        if let Some(parent) = path.parent() {
            std::fs::create_dir_all(parent)
                .map_err(|e| Error::new(&format!("create receipt dir: {e}")))?;
        }
        let json = serde_json::to_string_pretty(self)
            .map_err(|e| Error::new(&format!("serialize receipt: {e}")))?;
        std::fs::write(path, json).map_err(|e| Error::new(&format!("write receipt: {e}")))?;
        Ok(())
    }
}

fn sha256_hex(content: &[u8]) -> String {
    format!("{:x}", Sha256::digest(content))
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn receipt_has_nonnil_uuid_and_real_hashes() {
        let mut r = ReverseReceipt::new("reverse-scan");
        r.add_input("src:foo.rs", b"hello");
        r.add_output("out.ttl", b"world");

        // operation_id is a parseable, non-nil UUID.
        let id = uuid::Uuid::parse_str(&r.operation_id).expect("valid uuid");
        assert_ne!(id, uuid::Uuid::nil(), "operation_id must not be the nil UUID");

        // Hashes are real sha256 (64 hex chars), not placeholders.
        let h = &r.input_hashes["src:foo.rs"];
        assert_eq!(h.len(), 64);
        assert_eq!(h, &sha256_hex(b"hello"));
        assert_eq!(&r.output_hashes["out.ttl"], &sha256_hex(b"world"));
    }
}
