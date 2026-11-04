//! Chicago tests for the invert-to-undo journal (reverse-80/20 §1):
//! real filesystem, byte-identical restore, tamper refusal, crash recovery.
//! No mocks — the collaborator is the real disk.

use ggen_engine::transactional::{
    apply_edit, empty_image_sha256, FileEdit, Journal, RecoveryReport, TransactionalEdit,
};
use sha2::Digest as _;
use tempfile::TempDir;

fn sha(bytes: &[u8]) -> String {
    let mut h = sha2::Sha256::new();
    h.update(bytes);
    let d = h.finalize();
    let mut s = String::new();
    for b in d {
        s.push_str(&format!("{b:02x}"));
    }
    s
}

fn create(path: &std::path::Path, bytes: &[u8]) {
    std::fs::write(path, bytes).unwrap();
}

fn read(path: &std::path::Path) -> Vec<u8> {
    std::fs::read(path).unwrap()
}

// ---------------------------------------------------------------------------
// capture + invert: the exact inverse delta set
// ---------------------------------------------------------------------------

#[test]
fn capture_absent_file_commits_empty_image() {
    let dir = TempDir::new().unwrap();
    let p = dir.path().join("new.txt");
    let e = FileEdit::capture(&p, b"after".to_vec()).unwrap();
    assert_eq!(e.before_bytes, None);
    assert_eq!(e.before_sha256, empty_image_sha256());
    assert_eq!(e.after_sha256, sha(b"after"));
    assert!(e.deletes_file());
}

#[test]
fn capture_existing_file_reads_real_before_bytes() {
    let dir = TempDir::new().unwrap();
    let p = dir.path().join("f.txt");
    create(&p, b"before");
    let e = FileEdit::capture(&p, b"after".to_vec()).unwrap();
    assert_eq!(e.before_bytes.as_deref(), Some(b"before".as_slice()));
    assert_eq!(e.before_sha256, sha(b"before"));
    assert_eq!(e.after_sha256, sha(b"after"));
    assert!(!e.deletes_file());
}

#[test]
fn invert_is_exact_inverse_including_absence() {
    let dir = TempDir::new().unwrap();
    let p = dir.path().join("f.txt");
    create(&p, b"before");
    let e = FileEdit::capture(&p, b"after".to_vec()).unwrap();
    let inv = e.invert();
    assert_eq!(inv.path, e.path);
    assert_eq!(inv.before_bytes.as_deref(), Some(b"after".as_slice()));
    assert_eq!(inv.after_bytes, b"before");
    assert_eq!(inv.before_sha256, e.after_sha256);
    assert_eq!(inv.after_sha256, e.before_sha256);
    assert!(!inv.deletes_file());
}

#[test]
fn invert_of_new_file_deletes_on_rollback() {
    let dir = TempDir::new().unwrap();
    let p = dir.path().join("new.txt");
    let e = FileEdit::capture(&p, b"created".to_vec()).unwrap();
    // The inverse of "create file" is "delete file": the inverted edit's
    // after-image is the empty image and its apply deletes via rollback of
    // the original (before_bytes = None).
    let inv = e.invert();
    assert!(inv.after_bytes.is_empty());
    assert_eq!(inv.after_sha256, empty_image_sha256());
}

// ---------------------------------------------------------------------------
// apply + rollback: byte-identical restore
// ---------------------------------------------------------------------------

#[test]
fn apply_rollback_roundtrip_is_byte_identical() {
    let dir = TempDir::new().unwrap();
    let a = dir.path().join("a.txt");
    let b = dir.path().join("b.txt");
    create(&a, b"alpha-v1");
    create(&b, b"beta-v1");

    let e1 = FileEdit::capture(&a, b"alpha-v2".to_vec()).unwrap();
    let e2 = FileEdit::capture(&b, b"beta-v2".to_vec()).unwrap();
    let tx = TransactionalEdit::new("t1", vec![e1, e2], "");

    tx.apply().unwrap();
    assert_eq!(read(&a), b"alpha-v2");
    assert_eq!(read(&b), b"beta-v2");

    tx.rollback().unwrap();
    assert_eq!(read(&a), b"alpha-v1");
    assert_eq!(read(&b), b"beta-v1");
}

#[test]
fn rollback_of_new_file_restores_absence() {
    let dir = TempDir::new().unwrap();
    let p = dir.path().join("created.txt");
    let tx = TransactionalEdit::new(
        "t2",
        vec![FileEdit::capture(&p, b"created".to_vec()).unwrap()],
        "",
    );
    tx.apply().unwrap();
    assert!(p.exists());
    tx.rollback().unwrap();
    assert!(!p.exists());
}

#[test]
fn apply_refuses_when_file_diverged_before_apply() {
    let dir = TempDir::new().unwrap();
    let p = dir.path().join("f.txt");
    create(&p, b"original");
    let e = FileEdit::capture(&p, b"planned".to_vec()).unwrap();
    create(&p, b"TAMPERED-OUTSIDE-TX");
    let tx = TransactionalEdit::new("t3", vec![e], "");
    let err = tx.apply().unwrap_err();
    assert!(
        format!("{err}").contains("hashes to"),
        "expected divergence refusal, got: {err}"
    );
}

#[test]
fn apply_refuses_when_committed_file_was_deleted() {
    let dir = TempDir::new().unwrap();
    let p = dir.path().join("f.txt");
    create(&p, b"original");
    let e = FileEdit::capture(&p, b"planned".to_vec()).unwrap();
    std::fs::remove_file(&p).unwrap();
    let tx = TransactionalEdit::new("t4", vec![e], "");
    let err = tx.apply().unwrap_err();
    assert!(
        format!("{err}").contains("ABSENT"),
        "expected absence refusal, got: {err}"
    );
    assert!(!p.exists(), "refused apply must not have written");
}

#[test]
fn rollback_refuses_when_file_diverged_after_apply() {
    let dir = TempDir::new().unwrap();
    let p = dir.path().join("f.txt");
    create(&p, b"v1");
    let e = FileEdit::capture(&p, b"v2".to_vec()).unwrap();
    let tx = TransactionalEdit::new("t5", vec![e], "");
    tx.apply().unwrap();
    create(&p, b"TAMPERED-AFTER-APPLY");
    let err = tx.rollback().unwrap_err();
    assert!(
        format!("{err}").contains("hashes to"),
        "expected rollback divergence refusal, got: {err}"
    );
    // verify-first law: the tampered state is still on disk, untouched
    assert_eq!(read(&p), b"TAMPERED-AFTER-APPLY");
}

// ---------------------------------------------------------------------------
// journal: BLAKE3 chain + tamper refusal
// ---------------------------------------------------------------------------

#[test]
fn journal_chain_links_and_head_advances() {
    let dir = TempDir::new().unwrap();
    let jpath = dir.path().join("journal.jsonl");
    let mut j = Journal::open(&jpath).unwrap();
    assert_eq!(j.head(), "");

    let tx1 = TransactionalEdit::new("tx-1", Vec::new(), j.head());
    j.append(&tx1).unwrap();
    assert_eq!(j.head(), tx1.hash);

    let tx2 = TransactionalEdit::new("tx-2", Vec::new(), j.head());
    j.append(&tx2).unwrap();
    assert_eq!(tx2.prev_hash, tx1.hash);
    assert_eq!(j.records().len(), 2);
}

#[test]
fn journal_append_with_stale_prev_hash_refuses() {
    let dir = TempDir::new().unwrap();
    let jpath = dir.path().join("journal.jsonl");
    let mut j = Journal::open(&jpath).unwrap();
    let tx = TransactionalEdit::new("tx-1", Vec::new(), "not-the-head");
    assert!(j.append(&tx).is_err());
    assert!(j.records().is_empty(), "refused append must not persist");
}

#[test]
fn tampered_journal_refuses_to_replay() {
    let dir = TempDir::new().unwrap();
    let jpath = dir.path().join("journal.jsonl");

    {
        let mut j = Journal::open(&jpath).unwrap();
        let p = dir.path().join("f.txt");
        create(&p, b"v1");
        let e = FileEdit::capture(&p, b"v2".to_vec()).unwrap();
        let tx = TransactionalEdit::new("tx-1", vec![e], j.head());
        j.append(&tx).unwrap();
    }

    // tamper: rewrite a byte image inside the journal on disk
    let raw = std::fs::read_to_string(&jpath).unwrap();
    // Byte images serialize as arrays, so tamper the recorded chain hash
    // itself (flip one hex digit) — the classic forged-ledger mutation.
    let marker = "\"hash\":\"";
    let idx = raw.find(marker).unwrap() + marker.len();
    let mut tampered = raw.clone();
    let flipped = if raw.as_bytes()[idx] == b'0' {
        '1'
    } else {
        '0'
    };
    tampered.replace_range(idx..idx + 1, &flipped.to_string());
    assert_ne!(raw, tampered, "tamper setup must change the journal");
    std::fs::write(&jpath, tampered).unwrap();

    let err = Journal::open(&jpath).unwrap_err();
    let msg = format!("{err}");
    assert!(
        msg.contains("journal hash mismatch") || msg.contains("corrupt"),
        "expected tamper refusal, got: {msg}"
    );
}

#[test]
fn journal_chain_break_refuses() {
    let dir = TempDir::new().unwrap();
    let jpath = dir.path().join("journal.jsonl");
    {
        let mut j = Journal::open(&jpath).unwrap();
        let tx = TransactionalEdit::new("tx-1", Vec::new(), j.head());
        j.append(&tx).unwrap();
    }
    // splice: a second genesis record that claims no previous hash
    let mut raw = std::fs::read_to_string(&jpath).unwrap();
    let forged = TransactionalEdit::new("forged", Vec::new(), "");
    raw.push_str(&serde_json::to_string(&forged).unwrap());
    raw.push('\n');
    std::fs::write(&jpath, raw).unwrap();

    let err = Journal::open(&jpath).unwrap_err();
    assert!(
        format!("{err}").contains("chain break"),
        "expected chain-break refusal, got: {err}"
    );
}

// ---------------------------------------------------------------------------
// crash recovery: journal replays to a consistent state
// ---------------------------------------------------------------------------

#[test]
fn crash_mid_apply_rolls_forward() {
    let dir = TempDir::new().unwrap();
    let jpath = dir.path().join("journal.jsonl");
    let a = dir.path().join("a.txt");
    let b = dir.path().join("b.txt");
    create(&a, b"a-v1");
    create(&b, b"b-v1");

    let e1 = FileEdit::capture(&a, b"a-v2".to_vec()).unwrap();
    let e2 = FileEdit::capture(&b, b"b-v2".to_vec()).unwrap();
    let tx = TransactionalEdit::new("tx-crash", vec![e1, e2], "");

    {
        let mut j = Journal::open(&jpath).unwrap();
        j.append(&tx).unwrap();
    }

    // simulate a crash mid-apply: only the first edit landed
    create(&a, b"a-v2");

    let j = Journal::open(&jpath).unwrap();
    let report = j.recover().unwrap();
    assert_eq!(
        report,
        RecoveryReport {
            settled_records: 0,
            rolled_forward_records: 1,
            reapplied_edits: 1,
        }
    );
    assert_eq!(read(&a), b"a-v2");
    assert_eq!(read(&b), b"b-v2");
    // journal itself must remain well-formed after recovery
    assert!(Journal::open(&jpath).is_ok());
}

#[test]
fn fully_applied_journal_recovers_as_settled() {
    let dir = TempDir::new().unwrap();
    let jpath = dir.path().join("journal.jsonl");
    let p = dir.path().join("f.txt");
    create(&p, b"v1");

    let tx = TransactionalEdit::new(
        "tx-full",
        vec![FileEdit::capture(&p, b"v2".to_vec()).unwrap()],
        "",
    );
    tx.apply().unwrap();
    {
        let mut j = Journal::open(&jpath).unwrap();
        j.append(&tx).unwrap();
    }

    let j = Journal::open(&jpath).unwrap();
    let report = j.recover().unwrap();
    assert_eq!(report.settled_records, 1);
    assert_eq!(report.rolled_forward_records, 0);
    assert_eq!(read(&p), b"v2");
}

#[test]
fn recovery_refuses_when_file_matches_neither_commitment() {
    let dir = TempDir::new().unwrap();
    let jpath = dir.path().join("journal.jsonl");
    let p = dir.path().join("f.txt");
    create(&p, b"v1");

    let tx = TransactionalEdit::new(
        "tx-diverged",
        vec![FileEdit::capture(&p, b"v2".to_vec()).unwrap()],
        "",
    );
    {
        let mut j = Journal::open(&jpath).unwrap();
        j.append(&tx).unwrap();
    }
    create(&p, b"DIVERGED");

    let j = Journal::open(&jpath).unwrap();
    let err = j.recover().unwrap_err();
    assert!(
        format!("{err}").contains("matches neither"),
        "expected neither-commitment refusal, got: {err}"
    );
}

#[test]
fn apply_edit_standalone_matches_transaction_apply() {
    let dir = TempDir::new().unwrap();
    let p = dir.path().join("f.txt");
    create(&p, b"v1");
    let e = FileEdit::capture(&p, b"v2".to_vec()).unwrap();
    apply_edit(&e).unwrap();
    assert_eq!(read(&p), b"v2");
}
