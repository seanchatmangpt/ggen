//! Invert-to-undo transactional edits over ggen's planned writes (reverse-80/20 §1).
//!
//! [`plan_write`](crate::write::plan_write) decides and applies one write at
//! a time. This module layers a rollback journal over the same planned
//! writes: capture each target's current bytes (or absence) into a
//! [`TransactionalEdit`], apply with byte verification, and on any failure
//! [`rollback`](TransactionalEdit::rollback) restores byte-identical state —
//! the rollback journal without git.
//!
//! Fail-closed laws:
//! - apply verifies each target's current bytes against `before_sha256`
//!   before overwriting (a diverged file is a typed refusal, never a silent
//!   clobber);
//! - rollback verifies each target against `after_sha256` before restoring;
//! - the journal is a BLAKE3 hash-chained JSONL ledger, so tampering is
//!   detected on replay;
//! - partial-apply crash recovery rolls the journal forward to a consistent
//!   state.
//!
//! Error codes occupy the `[FM-WRITE-020..]` band, disjoint from
//! `write.rs`'s `[FM-WRITE-001..012]`.

use std::path::{Path, PathBuf};

use serde::{Deserialize, Serialize};
use sha2::{Digest, Sha256};

use crate::error::{AppError, Result};

/// Base of the error-code band for transactional refusals.
pub const TX_ERR_BASE: u16 = 20;

fn tx_err(code: u16, msg: impl Into<String>) -> AppError {
    AppError::fm_write(TX_ERR_BASE + code, msg)
}

fn sha256_hex(bytes: &[u8]) -> String {
    let mut h = Sha256::new();
    h.update(bytes);
    let d = h.finalize();
    let mut s = String::with_capacity(d.len() * 2);
    for b in d {
        s.push_str(&format!("{b:02x}"));
    }
    s
}

/// SHA-256 of the empty byte image — the commitment to "file absent".
pub fn empty_image_sha256() -> String {
    sha256_hex(&[])
}

/// A single-file inverse-delta entry: before/after byte images plus SHA-256
/// commitments over both.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct FileEdit {
    /// Absolute path of the target file.
    pub path: PathBuf,
    /// Bytes present before the edit; `None` = the file did not exist.
    pub before_bytes: Option<Vec<u8>>,
    /// Bytes to be present after the edit.
    pub after_bytes: Vec<u8>,
    /// SHA-256 (hex) of `before_bytes`; empty-image hash when absent.
    pub before_sha256: String,
    /// SHA-256 (hex) of `after_bytes`.
    pub after_sha256: String,
}

impl FileEdit {
    /// Capture the inverse delta for one target: read its current bytes (or
    /// record absence) and commit to the planned `after` image.
    pub fn capture(path: impl Into<PathBuf>, after_bytes: Vec<u8>) -> Result<Self> {
        let path = path.into();
        let (before, before_sha) = match std::fs::read(&path) {
            Ok(bytes) => {
                let digest = sha256_hex(&bytes);
                (Some(bytes), digest)
            }
            Err(e) if e.kind() == std::io::ErrorKind::NotFound => (None, empty_image_sha256()),
            Err(e) => return Err(e.into()),
        };
        let after_sha = sha256_hex(&after_bytes);
        Ok(Self {
            path,
            before_bytes: before,
            after_bytes,
            before_sha256: before_sha,
            after_sha256: after_sha,
        })
    }

    /// The exact inverse delta: restore `before_bytes`, deleting the file
    /// when it was absent before the original edit.
    pub fn invert(&self) -> FileEdit {
        let before_was_absent = self.before_bytes.is_none();
        FileEdit {
            path: self.path.clone(),
            before_bytes: Some(self.after_bytes.clone()),
            after_bytes: self.before_bytes.clone().unwrap_or_default(),
            before_sha256: self.after_sha256.clone(),
            after_sha256: if before_was_absent {
                empty_image_sha256()
            } else {
                self.before_sha256.clone()
            },
        }
    }

    /// Whether the inverse edit deletes the file (it was absent before).
    pub fn deletes_file(&self) -> bool {
        self.before_bytes.is_none()
    }
}

/// Verify on-disk state against a commitment, refusing with a typed error
/// on divergence.
///
/// `expect_absent` asserts the file must not exist (the commitment is the
/// empty-image hash). The refusal names the path, both hashes, and a
/// remediation hint.
fn verify_state(
    path: &Path, expect_sha: &str, expect_absent: bool, code: u16, direction: &str,
) -> Result<()> {
    let current = match std::fs::read(path) {
        Ok(bytes) => Some(bytes),
        Err(e) if e.kind() == std::io::ErrorKind::NotFound => None,
        Err(e) => return Err(e.into()),
    };
    match (&current, expect_absent) {
        (Some(_), false) => {}
        (None, true) => {}
        (None, false) => {
            return Err(tx_err(
                code,
                format!(
                    "transactional {} refused: `{}` is ABSENT but the journal commits \
                     {expect_sha}. Remediation: the file was removed outside the transaction; \
                     rebuild it or discard this journal.",
                    direction,
                    path.display()
                ),
            ));
        }
        (Some(_), true) => {
            return Err(tx_err(
                code,
                format!(
                    "transactional {} refused: `{}` EXISTS but the journal commits it to \
                     absent. Remediation: the file was created outside the transaction; \
                     delete it or discard this journal.",
                    direction,
                    path.display()
                ),
            ));
        }
    }
    if let Some(bytes) = &current {
        let observed = sha256_hex(bytes);
        if observed != expect_sha {
            return Err(tx_err(
                code,
                format!(
                    "transactional {} refused: `{}` hashes to {observed} but the journal \
                     commits {expect_sha}. Remediation: the file changed outside the \
                     transaction; restore the committed bytes or discard this journal.",
                    direction,
                    path.display()
                ),
            ));
        }
    }
    Ok(())
}

/// Crash-safe byte write: temp file, fsync, rename over the target.
fn write_bytes(path: &Path, bytes: &[u8]) -> Result<()> {
    let mut tmp = path.as_os_str().to_owned();
    tmp.push(".ggen-tx-tmp");
    let tmp = PathBuf::from(tmp);
    {
        use std::io::Write as _;
        let mut f = std::fs::File::create(&tmp)?;
        f.write_all(bytes)?;
        f.sync_all()?;
    }
    std::fs::rename(&tmp, path)?;
    Ok(())
}

/// Apply one [`FileEdit`] with verification against the `before` commitment.
pub fn apply_edit(edit: &FileEdit) -> Result<()> {
    verify_state(
        &edit.path,
        &edit.before_sha256,
        edit.before_bytes.is_none(),
        1,
        "apply",
    )?;
    // Apply always writes `after_bytes` — even when the file was absent
    // before. (`deletes_file()` describes the INVERSE edit, not apply.)
    if let Some(parent) = edit.path.parent() {
        std::fs::create_dir_all(parent)?;
    }
    write_bytes(&edit.path, &edit.after_bytes)?;
    Ok(())
}

/// Roll one [`FileEdit`] back with verification against the `after`
/// commitment.
fn rollback_edit(edit: &FileEdit) -> Result<()> {
    verify_state(&edit.path, &edit.after_sha256, false, 2, "rollback")?;
    match &edit.before_bytes {
        Some(before) => {
            if let Some(parent) = edit.path.parent() {
                std::fs::create_dir_all(parent)?;
            }
            write_bytes(&edit.path, before)?;
        }
        None => {
            std::fs::remove_file(&edit.path)?;
        }
    }
    Ok(())
}

/// The transaction record: an id, the per-file inverse-delta set, and a
/// BLAKE3 prev-hash chain linking transactions in application order.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct TransactionalEdit {
    /// Unique id (opaque, caller-chosen).
    pub id: String,
    /// Per-file inverse-delta entries, in application order.
    pub edits: Vec<FileEdit>,
    /// BLAKE3 hash of the previous transaction (empty for genesis).
    pub prev_hash: String,
    /// BLAKE3 hash over (prev_hash, id, edit count, per-file commitments).
    pub hash: String,
}

impl TransactionalEdit {
    /// Chain hash over prev_hash + id + every edit's commitments.
    fn chain_hash(id: &str, prev_hash: &str, edits: &[FileEdit]) -> String {
        let mut h = blake3::Hasher::new();
        h.update(prev_hash.as_bytes());
        h.update(id.as_bytes());
        h.update(&(edits.len() as u64).to_le_bytes());
        for e in edits {
            h.update(e.path.to_string_lossy().as_bytes());
            h.update(e.before_sha256.as_bytes());
            h.update(e.after_sha256.as_bytes());
        }
        h.finalize().to_string()
    }

    /// Build a transaction; chain it to `prev_hash` (empty for genesis).
    pub fn new(id: impl Into<String>, edits: Vec<FileEdit>, prev_hash: impl Into<String>) -> Self {
        let id = id.into();
        let prev_hash = prev_hash.into();
        let hash = Self::chain_hash(&id, &prev_hash, &edits);
        Self {
            id,
            edits,
            prev_hash,
            hash,
        }
    }

    /// The exact inverse delta set for the whole transaction, in reverse
    /// edit order. The inverse keeps the same chain position (prev_hash and
    /// hash are unchanged) — it is a derived journal, not a new link.
    pub fn invert(&self) -> TransactionalEdit {
        TransactionalEdit {
            id: format!("invert-{}", self.id),
            edits: self.edits.iter().rev().map(|e| e.invert()).collect(),
            prev_hash: self.prev_hash.clone(),
            hash: self.hash.clone(),
        }
    }

    /// Apply the transaction's edits in order with byte verification
    /// against the `before` commitments.
    pub fn apply(&self) -> Result<()> {
        for edit in &self.edits {
            apply_edit(edit)?;
        }
        Ok(())
    }

    /// Roll the whole transaction back: verify every target still hashes to
    /// its `after` commitment FIRST (a mid-set divergence must not leave a
    /// half-rolled-back state), then restore in reverse order.
    pub fn rollback(&self) -> Result<()> {
        for edit in &self.edits {
            verify_state(&edit.path, &edit.after_sha256, false, 2, "rollback")?;
        }
        for edit in self.edits.iter().rev() {
            rollback_edit(edit)?;
        }
        Ok(())
    }
}

/// Append-only JSONL journal of [`TransactionalEdit`] records with a BLAKE3
/// chain, supporting replay-based crash recovery.
#[derive(Debug)]
pub struct Journal {
    path: PathBuf,
    records: Vec<TransactionalEdit>,
    head: String,
}

/// Outcome summary of [`Journal::recover`].
#[derive(Debug, Default, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct RecoveryReport {
    /// Records verified as already fully settled (nothing to do).
    pub settled_records: u64,
    /// Records that were not fully applied and were rolled forward.
    pub rolled_forward_records: u64,
    /// Individual edits re-applied during roll-forward.
    pub reapplied_edits: u64,
}

/// Check whether a path currently satisfies a commitment. `Ok(true)` = the
/// on-disk state matches; `Ok(false)` = it does not; `Err` = unreadable
/// (I/O failure other than NotFound).
fn matches_commitment(path: &Path, expect_sha: &str, expect_absent: bool) -> Result<bool> {
    let current = match std::fs::read(path) {
        Ok(bytes) => Some(bytes),
        Err(e) if e.kind() == std::io::ErrorKind::NotFound => None,
        Err(e) => return Err(e.into()),
    };
    Ok(match (&current, expect_absent) {
        (None, true) => true,
        (Some(bytes), false) => sha256_hex(bytes) == expect_sha,
        _ => false,
    })
}

impl Journal {
    /// Open (or create) a journal at `path`, verifying the BLAKE3 chain of
    /// any existing records. A corrupt line, chain break, or hash mismatch
    /// is a typed refusal — a tampered journal never replays.
    pub fn open(path: impl Into<PathBuf>) -> Result<Self> {
        let path = path.into();
        let mut records = Vec::new();
        let mut head = String::new();
        let content = match std::fs::read_to_string(&path) {
            Ok(c) => c,
            Err(e) if e.kind() == std::io::ErrorKind::NotFound => String::new(),
            Err(e) => return Err(e.into()),
        };
        for (lineno, line) in content.lines().enumerate() {
            let lineno = lineno + 1;
            if line.trim().is_empty() {
                continue;
            }
            let record: TransactionalEdit = serde_json::from_str(line)
                .map_err(|e| tx_err(3, format!("journal line {lineno} is corrupt: {e}")))?; // [FM-WRITE-023]
            if record.prev_hash != head {
                return Err(tx_err(
                    4,
                    format!(
                        "journal chain break at line {lineno}: record links {} but head is {head}",
                        record.prev_hash
                    ),
                )); // [FM-WRITE-024]
            }
            let expected =
                TransactionalEdit::chain_hash(&record.id, &record.prev_hash, &record.edits);
            if record.hash != expected {
                return Err(tx_err(
                    5,
                    format!(
                        "journal hash mismatch at line {lineno}: record claims {} but the chain computes {expected}",
                        record.hash
                    ),
                )); // [FM-WRITE-025]
            }
            head = record.hash.clone();
            records.push(record);
        }
        Ok(Self {
            path,
            records,
            head,
        })
    }

    /// All records, in application order.
    pub fn records(&self) -> &[TransactionalEdit] {
        &self.records
    }

    /// The chain head hash (empty string before any record).
    pub fn head(&self) -> &str {
        &self.head
    }

    /// Append a transaction to the journal (fsync'd).
    ///
    /// Ordering law: append BEFORE apply, so a crash mid-apply leaves a
    /// journal that [`recover`](Self::recover) can replay.
    pub fn append(&mut self, tx: &TransactionalEdit) -> Result<()> {
        if tx.prev_hash != self.head {
            return Err(tx_err(
                6,
                format!(
                    "cannot append `{}`: it links {} but journal head is {}",
                    tx.id, tx.prev_hash, self.head
                ),
            )); // [FM-WRITE-026]
        }
        let line = serde_json::to_string(tx)?;
        use std::io::Write as _;
        let mut f = std::fs::OpenOptions::new()
            .create(true)
            .append(true)
            .open(&self.path)?;
        writeln!(f, "{line}")?;
        f.sync_all()?;
        self.records.push(tx.clone());
        self.head = tx.hash.clone();
        Ok(())
    }

    /// Crash recovery: replay the journal to a consistent state.
    ///
    /// For each record in order, each edit is classified against its
    /// commitments: `after` holds → already settled; `before` holds → the
    /// apply never reached this edit, roll it forward; neither holds → the
    /// file diverged outside the transaction, refuse (typed, fail-closed).
    /// After recovery every record's full `after` state holds on disk.
    pub fn recover(&self) -> Result<RecoveryReport> {
        let mut report = RecoveryReport::default();
        for tx in &self.records {
            let mut all_settled = true;
            let mut reapplied = 0u64;
            for edit in &tx.edits {
                let expect_absent_before = edit.before_bytes.is_none();
                let after_holds = matches_commitment(&edit.path, &edit.after_sha256, false)?;
                if after_holds {
                    continue;
                }
                all_settled = false;
                let before_holds =
                    matches_commitment(&edit.path, &edit.before_sha256, expect_absent_before)?;
                if before_holds {
                    apply_edit(edit)?;
                    reapplied += 1;
                } else {
                    return Err(tx_err(
                        7,
                        format!(
                            "crash recovery refused: `{}` matches neither the before nor the \
                             after commitment of transaction `{}`. Remediation: the file was \
                             modified outside the transaction; restore it manually or discard \
                             the journal.",
                            edit.path.display(),
                            tx.id
                        ),
                    )); // [FM-WRITE-027]
                }
            }
            if all_settled {
                report.settled_records += 1;
            } else {
                report.rolled_forward_records += 1;
                report.reapplied_edits += reapplied;
            }
        }
        Ok(report)
    }
}
