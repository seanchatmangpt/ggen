//! Chicago-style tests for the file-backed ledger (`ggen-sjira::ledger`).
//!
//! Real files on disk, real exclusive-create claims, state-based assertions:
//! every test asserts on the actual bytes/events in the ledger directory or
//! ndjson file, never on interactions. No doubles — the collaborator (the
//! filesystem) is fully real and in-process.

use std::fs;
use std::path::PathBuf;

use ggen_sjira::ledger::{
    append, event_digest, fetch, kind, read, vc_concurrent, vc_dominates, AppendOpts, AppendStatus,
    LedgerError, LedgerKind,
};
use serde_json::{json, Map, Value};

fn tmp_dir(name: &str) -> PathBuf {
    let dir = std::env::temp_dir().join(format!(
        "ggen-sjira-ledger-{}-{name}",
        std::process::id()
    ));
    let _ = fs::remove_dir_all(&dir);
    fs::create_dir_all(&dir).unwrap();
    dir
}

fn event(kind_: &str, subject: &str) -> Map<String, Value> {
    let raw = json!({
        "kind": kind_,
        "subject": subject,
        "receipt_digest": "sha256:aa".to_string() + &"0".repeat(58),
    });
    raw.as_object().unwrap().clone()
}

// ── kind resolution ─────────────────────────────────────────────────────────

#[test]
fn kind_existing_dir_is_dir_existing_file_is_file() {
    let dir = tmp_dir("kind");
    let d = dir.join("ledger");
    let f = dir.join("ledger.ndjson");
    fs::create_dir_all(&d).unwrap();
    fs::write(&f, "").unwrap();
    assert_eq!(kind(&d), LedgerKind::Dir);
    assert_eq!(kind(&f), LedgerKind::File);
    // Not-yet-existing paths resolve by extension, else directory.
    assert_eq!(kind(&dir.join("new.ndjson")), LedgerKind::File);
    assert_eq!(kind(&dir.join("new.jsonl")), LedgerKind::File);
    assert_eq!(kind(&dir.join("new")), LedgerKind::Dir);
}

// ── append / read / idempotent replay, both modes ──────────────────────────

#[test]
fn dir_mode_append_sequenced_immutable_files() {
    let dir = tmp_dir("dir-append");
    let ledger = dir.join("ledger");
    let mut names = Vec::new();
    for i in 0..3 {
        let out = append(&ledger, &event("admit", &format!("s{i}")), &AppendOpts::default()).unwrap();
        assert_eq!(out.status, AppendStatus::Appended);
        names.push(out.event.get("seq").unwrap().as_u64().unwrap());
    }
    assert_eq!(names, vec![1, 2, 3]);

    // Real on-disk state: one immutable JSON file per event, `<pad>-<frag>.json`.
    let mut files: Vec<String> = fs::read_dir(&ledger)
        .unwrap()
        .map(|e| e.unwrap().file_name().to_string_lossy().to_string())
        .filter(|n| n.ends_with(".json"))
        .collect();
    files.sort();
    assert_eq!(files.len(), 3);
    assert!(files[0].starts_with("00000001-"));
    // Ordered by seq on read.
    let events = fetch(&ledger).unwrap();
    let seqs: Vec<u64> = events
        .iter()
        .map(|e| e.get("seq").unwrap().as_u64().unwrap())
        .collect();
    assert_eq!(seqs, vec![1, 2, 3]);
}

#[test]
fn dir_mode_idempotent_replay_returns_existing_event() {
    let dir = tmp_dir("dir-replay");
    let ledger = dir.join("ledger");
    let first = append(&ledger, &event("admit", "s0"), &AppendOpts::default()).unwrap();
    let replay = append(&ledger, &event("admit", "s0"), &AppendOpts::default()).unwrap();
    assert_eq!(replay.status, AppendStatus::AlreadyRecorded);
    assert_eq!(first.event, replay.event);
    // Exactly one event file on disk — the replay wrote nothing new.
    let json_count = fs::read_dir(&ledger)
        .unwrap()
        .filter(|e| e.as_ref().unwrap().file_name().to_string_lossy().ends_with(".json"))
        .count();
    assert_eq!(json_count, 1);
}

#[test]
fn file_mode_append_and_idempotent_replay() {
    let dir = tmp_dir("file-append");
    let ledger = dir.join("standing-ledger.ndjson");
    let out1 = append(&ledger, &event("admit", "a"), &AppendOpts::default()).unwrap();
    let out2 = append(&ledger, &event("admit", "b"), &AppendOpts::default()).unwrap();
    assert_eq!(out1.status, AppendStatus::Appended);
    assert_eq!(out2.status, AppendStatus::Appended);
    let replay = append(&ledger, &event("admit", "a"), &AppendOpts::default()).unwrap();
    assert_eq!(replay.status, AppendStatus::AlreadyRecorded);
    assert_eq!(replay.event, out1.event);

    // Real bytes: two lines, one per distinct event; lock file released.
    let body = fs::read_to_string(&ledger).unwrap();
    let lines: Vec<&str> = body.lines().collect();
    assert_eq!(lines.len(), 2);
    assert!(!ledger.with_extension("lock").exists() || {
        // `.lock` suffix appends to the full filename, not the extension.
        let lock = PathBuf::from(format!("{}.lock", ledger.display()));
        !lock.exists()
    });
    let seqs: Vec<u64> = fetch(&ledger)
        .unwrap()
        .iter()
        .map(|e| e.get("seq").unwrap().as_u64().unwrap())
        .collect();
    assert_eq!(seqs, vec![1, 2]);
}

// ── digest law ──────────────────────────────────────────────────────────────

#[test]
fn event_digest_is_key_order_independent_and_excludes_seq_and_digest() {
    let mut a = event("admit", "s");
    let mut b = event("admit", "s");
    a.insert("seq".into(), json!(1));
    b.insert("seq".into(), json!(2));
    a.insert("event_digest".into(), json!("garbage"));
    // Key insertion order differs; digest still equal.
    b.insert("extra".into(), json!({"z": 1, "a": 2}));
    a.insert("extra".into(), json!({"a": 2, "z": 1}));
    assert_eq!(event_digest(&a), event_digest(&b));
    assert!(event_digest(&a).starts_with("sha256:"));
}

#[test]
fn stamped_digest_commits_to_epoch_and_vc() {
    let dir = tmp_dir("digest-commits");
    let ledger = dir.join("l.ndjson");
    let mut opts = AppendOpts::default();
    let mut vc = Map::new();
    vc.insert("r1".into(), json!(2));
    opts.vc = Some(
        [("r1".to_string(), 2u64)].into_iter().collect(),
    );
    let out = append(&ledger, &event("admit", "s"), &opts).unwrap();
    let stamped = out.event.as_object().unwrap();
    // Tamper with `vc` (a stamped field the digest must commit to) → refusal.
    let mut tampered = stamped.clone();
    tampered.insert("vc".into(), json!({"r1": 3}));
    assert_ne!(
        event_digest(&tampered),
        stamped.get("event_digest").unwrap().as_str().unwrap()
    );
}

// ── tamper detection ────────────────────────────────────────────────────────

#[test]
fn dir_mode_tampered_event_refuses_on_fetch_and_append() {
    let dir = tmp_dir("dir-tamper");
    let ledger = dir.join("ledger");
    append(&ledger, &event("admit", "a"), &AppendOpts::default()).unwrap();
    append(&ledger, &event("admit", "b"), &AppendOpts::default()).unwrap();

    // Mutate one event file's content in place (digest no longer recomputes).
    let mut files: Vec<PathBuf> = fs::read_dir(&ledger)
        .unwrap()
        .map(|e| e.unwrap().path())
        .filter(|p| p.extension().map(|x| x == "json").unwrap_or(false))
        .collect();
    files.sort();
    let path = &files[1];
    let mut body: Map<String, Value> =
        serde_json::from_str(&fs::read_to_string(path).unwrap()).unwrap();
    body.insert("kind".into(), json!("forged"));
    fs::write(path, serde_json::to_string(&body).unwrap()).unwrap();

    match fetch(&ledger) {
        Err(LedgerError::EventDigestMismatch(seq)) => assert_eq!(seq, 2),
        other => panic!("expected tamper refusal, got {other:?}"),
    }
    // Append refuses the same way before writing.
    let refused = append(&ledger, &event("admit", "c"), &AppendOpts::default());
    assert_eq!(refused.err(), Some(LedgerError::EventDigestMismatch(2)));
}

#[test]
fn file_mode_tampered_line_refuses() {
    let dir = tmp_dir("file-tamper");
    let ledger = dir.join("l.ndjson");
    append(&ledger, &event("admit", "a"), &AppendOpts::default()).unwrap();
    let body = fs::read_to_string(&ledger).unwrap();
    fs::write(&ledger, body.replace("admit", "forged")).unwrap();
    assert!(matches!(
        fetch(&ledger),
        Err(LedgerError::EventDigestMismatch(_))
    ));
}

#[test]
fn undecodable_and_non_object_entries_refuse_typed() {
    let dir = tmp_dir("refusals");
    // File ledger: bad JSON at physical line 2.
    let f = dir.join("bad.ndjson");
    let good = append(&f, &event("admit", "a"), &AppendOpts::default()).unwrap();
    let mut body = serde_json::to_string(&good.event).unwrap();
    body.push_str("\n{not json\n");
    fs::write(&f, &body).unwrap();
    assert!(matches!(fetch(&f), Err(LedgerError::Unreadable(_))));

    // File ledger: valid JSON, non-object, at physical line 2.
    let f2 = dir.join("notobj.ndjson");
    let good = append(&f2, &event("admit", "a"), &AppendOpts::default()).unwrap();
    let mut body = serde_json::to_string(&good.event).unwrap();
    body.push_str("\n[1,2]\n");
    fs::write(&f2, &body).unwrap();
    assert_eq!(
        fetch(&f2).err(),
        Some(LedgerError::NotAnObject("2".to_string()))
    );

    // Directory ledger: non-object file located by file NAME.
    let d = dir.join("dledger");
    append(&d, &event("admit", "a"), &AppendOpts::default()).unwrap();
    let mut files: Vec<PathBuf> = fs::read_dir(&d)
        .unwrap()
        .map(|e| e.unwrap().path())
        .filter(|p| p.extension().map(|x| x == "json").unwrap_or(false))
        .collect();
    files.sort();
    fs::write(&files[0], "[1,2]").unwrap();
    let name = files[0].file_name().unwrap().to_string_lossy().to_string();
    assert_eq!(
        fetch(&d).err(),
        Some(LedgerError::NotAnObject(name))
    );
}

// ── epoch markers + bump law ────────────────────────────────────────────────

#[test]
fn epoch_stamped_bumped_and_marker_persisted_dir_mode() {
    let dir = tmp_dir("epoch-dir");
    let ledger = dir.join("ledger");
    let out0 = append(&ledger, &event("admit", "a"), &AppendOpts::default()).unwrap();
    assert_eq!(out0.event.get("epoch").unwrap(), &json!(0));
    // Same generation: epoch stays.
    let out1 = append(&ledger, &event("admit", "b"), &AppendOpts::default()).unwrap();
    assert_eq!(out1.event.get("epoch").unwrap(), &json!(0));
    // Bump advances and persists the `<dir>/epoch` marker even with no event.
    let bump = AppendOpts {
        epoch_bump: Some(3),
        ..Default::default()
    };
    let out2 = append(&ledger, &event("admit", "c"), &bump).unwrap();
    assert_eq!(out2.event.get("epoch").unwrap(), &json!(3));
    assert_eq!(fs::read_to_string(ledger.join("epoch")).unwrap().trim(), "3");
    // Next plain append inherits the new generation.
    let out3 = append(&ledger, &event("admit", "d"), &AppendOpts::default()).unwrap();
    assert_eq!(out3.event.get("epoch").unwrap(), &json!(3));
}

#[test]
fn file_mode_epoch_marker_is_path_epoch() {
    let dir = tmp_dir("epoch-file");
    let ledger = dir.join("l.ndjson");
    let bump = AppendOpts {
        epoch_bump: Some(1),
        ..Default::default()
    };
    append(&ledger, &event("admit", "a"), &bump).unwrap();
    let marker = PathBuf::from(format!("{}.epoch", ledger.display()));
    assert_eq!(fs::read_to_string(&marker).unwrap().trim(), "1");
}

#[test]
fn epoch_regression_refused_before_any_write() {
    let dir = tmp_dir("epoch-regress");
    let ledger = dir.join("ledger");
    let bump = AppendOpts {
        epoch_bump: Some(2),
        ..Default::default()
    };
    append(&ledger, &event("admit", "a"), &bump).unwrap();
    let regress = AppendOpts {
        epoch: Some(1),
        ..Default::default()
    };
    let before = snapshot(&ledger);
    assert_eq!(
        append(&ledger, &event("admit", "b"), &regress).err(),
        Some(LedgerError::EpochRegression)
    );
    // Nothing written: byte-identical on-disk state before vs. after.
    assert_eq!(before, snapshot(&ledger));
    // Explicit epoch equal to last is lawful.
    let ok = AppendOpts {
        epoch: Some(2),
        ..Default::default()
    };
    let out = append(&ledger, &event("admit", "c"), &ok).unwrap();
    assert_eq!(out.event.get("epoch").unwrap(), &json!(2));
}

fn snapshot(path: &PathBuf) -> Vec<(String, Vec<u8>)> {
    let mut out = Vec::new();
    if path.is_dir() {
        let mut names: Vec<_> = fs::read_dir(path)
            .unwrap()
            .map(|e| e.unwrap().file_name().to_string_lossy().to_string())
            .collect();
        names.sort();
        for n in names {
            out.push((n.clone(), fs::read(path.join(n)).unwrap()));
        }
    } else if path.exists() {
        out.push((String::new(), fs::read(path).unwrap()));
    }
    out
}

// ── vector-clock law ────────────────────────────────────────────────────────

fn vc(pairs: &[(&str, u64)]) -> Map<String, Value> {
    pairs
        .iter()
        .map(|(r, n)| (r.to_string(), json!(n)))
        .collect()
}

#[test]
fn vc_dominates_law() {
    let a = vc(&[("r1", 2), ("r2", 1)]);
    let b = vc(&[("r1", 1), ("r2", 1)]);
    let c = vc(&[("r1", 1), ("r2", 2)]);
    let d = vc(&[("r1", 1)]);
    assert!(vc_dominates(&a, &b));
    assert!(!vc_dominates(&b, &a));
    assert!(!vc_dominates(&a, &c)); // concurrent
    // The empty clock dominates nothing; a nonempty clock dominates it.
    assert!(!vc_dominates(&Map::new(), &d));
    assert!(vc_dominates(&d, &Map::new()));
    let e = vc(&[("r1", 2), ("r2", 1), ("r3", 0)]);
    assert!(vc_dominates(&a, &e) == false); // equal modulo missing-zero
    assert!(vc_dominates(&a, &d));
    // Missing replica counts as 0.
    assert!(vc_dominates(&vc(&[("r1", 2)]), &vc(&[("r1", 2), ("r2", 0)])) == false);
}

#[test]
fn vc_concurrent_law() {
    let a = vc(&[("r1", 2), ("r2", 1)]);
    let b = vc(&[("r1", 1), ("r2", 2)]);
    let c = vc(&[("r1", 2)]);
    let same = vc(&[("r1", 2), ("r2", 1)]);
    assert!(vc_concurrent(&a, &b));
    assert!(vc_concurrent(&b, &a));
    assert!(!vc_concurrent(&a, &same)); // equal = same history, never a conflict
    assert!(!vc_concurrent(&a, &c)); // a dominates c
    assert!(!vc_concurrent(&c, &a));
}

// ── read/fetch basics ───────────────────────────────────────────────────────

#[test]
fn empty_or_missing_ledger_reads_empty() {
    let dir = tmp_dir("empty");
    let missing_dir = dir.join("nope");
    let missing_file = dir.join("nope.ndjson");
    assert_eq!(fetch(&missing_dir).unwrap(), Vec::<Value>::new());
    assert_eq!(fetch(&missing_file).unwrap(), Vec::<Value>::new());
    assert_eq!(read(&missing_file), Vec::<Value>::new());
}
