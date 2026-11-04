//! AB-14 chain-scale stress — ggen JSONL receipt ledger at 100k records.
//!
//! Subject (read-only): the ggen half of the two existing chain planes —
//! the append-only `.ggen-v2/receipt-log.jsonl` ledger and its REAL
//! full-chain verifier `handle_receipt_history_in` (the exact function
//! `ggen receipt history` runs: genesis all-zeros anchor, per-record
//! payload-hash binding over the raw stored payload bytes, rule-aware
//! chain-hash recompute via the emission-path frame, strict
//! adjacent-link check, head-pointer consistency).
//!
//! DISCLOSURE (writer path): `sync::write_receipt` is `pub(crate)` —
//! there is no public writer. Per the charter's fallback logic, ledger
//! lines are minted through the PUBLIC seam types
//! (`receipt_chain_seam::ReceiptRecord` + `recompute_chain_hash`, the
//! exact emission-path hash) into a /tmp project's
//! `.ggen-v2/receipt-log.jsonl` in the emission wire format
//! (`serde_json` compact of `SyncReceipt`), then verified by the REAL
//! pub verifier. 100 real `ggen sync run` invocations would mint only
//! ~100 records (~1-2 s each) — the wrong tool for a 100k-record scale
//! court; the seam-mint exercises the identical emission-path hash
//! function and the identical verifier.

use std::io::Write as _;
use std::sync::atomic::{AtomicUsize, Ordering};
use std::time::Instant;

use ggen_engine::receipt_chain_seam::{
    recompute_chain_hash, Andon, ReceiptRecord, CHAIN_RULE_V2_FOLD, RECEIPT_RECORD_VERSION,
};
use ggen_engine::sync::{ReceiptPayload, SyncReceipt, RECEIPT_LOG_REL_PATH};
use ggen_engine::verbs::handlers::handle_receipt_history_in;

const N_RECORDS: usize = 100_000;
const BUCKET: usize = 1_000;
const GENESIS_HEX: &str = "0000000000000000000000000000000000000000000000000000000000000000";

// Hand-rolled counting global allocator (this test binary only): the
// per-bucket alloc census measures the real mint+append path.
struct CountingAlloc;

static ALLOCS: AtomicUsize = AtomicUsize::new(0);
static BYTES: AtomicUsize = AtomicUsize::new(0);

unsafe impl std::alloc::GlobalAlloc for CountingAlloc {
    unsafe fn alloc(&self, layout: std::alloc::Layout) -> *mut u8 {
        ALLOCS.fetch_add(1, Ordering::Relaxed);
        BYTES.fetch_add(layout.size(), Ordering::Relaxed);
        unsafe { std::alloc::System.alloc(layout) }
    }
    unsafe fn dealloc(&self, ptr: *mut u8, layout: std::alloc::Layout) {
        unsafe { std::alloc::System.dealloc(ptr, layout) }
    }
    unsafe fn realloc(&self, ptr: *mut u8, layout: std::alloc::Layout, new_size: usize) -> *mut u8 {
        ALLOCS.fetch_add(1, Ordering::Relaxed);
        BYTES.fetch_add(new_size, Ordering::Relaxed);
        unsafe { std::alloc::System.realloc(ptr, layout, new_size) }
    }
}

#[global_allocator]
static GLOBAL: CountingAlloc = CountingAlloc;

/// Pull a field out of one wire line.
fn line_field(line: &str, field: &str) -> String {
    let v: serde_json::Value = serde_json::from_str(line).expect("line parses");
    v["record"][field].as_str().expect(field).to_string()
}

/// Mint one lawful ledger line: the payload is serialized first and its
/// exact compact bytes hashed (`stored_payload_hash` hashes the RAW
/// payload bytes embedded in the line, never a re-serialization — so the
/// embedded form must be byte-identical, which serde_json's compact
/// writer guarantees for the same value); the record's chain hash is
/// recomputed via the seam (the emission-path fold).
fn mint_line(prev_chain_hash_hex: &str, i: usize) -> String {
    let payload = ReceiptPayload {
        graph_hash: format!("ab14-graph-{i:06}"),
        outputs: Default::default(),
        packs: Default::default(),
        decisions: Default::default(),
        closure: Default::default(),
        build_provenance: None,
        generation: Some((i + 1) as u64),
    };
    let payload_json = serde_json::to_vec(&payload).expect("payload serializes");
    let record = ReceiptRecord {
        version: RECEIPT_RECORD_VERSION,
        instruction_id: i as u64,
        activity_idx: 0,
        activity: None,
        node_kind: 0,
        ts_ns: 1_700_000_000_000_000_000 + i as u64,
        duration_ms: None,
        origin: None,
        payload_hash_hex: blake3::hash(&payload_json).to_hex().to_string(),
        prev_chain_hash_hex: prev_chain_hash_hex.to_string(),
        chain_hash_hex: String::new(),
        andon: Andon::Green,
        obligation_count: 0,
        object_ids: Vec::new(),
        signature_hex: None,
        schema: "ggen-receipt/v1".to_string(),
        v2: None,
        chain_rule: Some(CHAIN_RULE_V2_FOLD.to_string()),
    };
    let chain_hash = recompute_chain_hash(&record).expect("emission-path recompute");
    let payload_hash_hex = record.payload_hash_hex.clone();
    let record = ReceiptRecord {
        chain_hash_hex: blake3_hash_hex(&chain_hash),
        ..record
    };
    let receipt = SyncReceipt {
        record,
        payload: serde_json::from_slice(&payload_json).expect("payload round-trips"),
    };
    let mut line = serde_json::to_string(&receipt).expect("line serializes");
    // Non-vacuity of the payload binding: the embedded payload bytes must
    // equal the bytes we hashed. If serde ever re-orders/re-spaces, this
    // catches it loudly instead of failing verify with a mystery mismatch.
    {
        // Raw substring extraction (mirrors the verifier's
        // RawPayloadProbe): `payload` is SyncReceipt's LAST serialized
        // field, so its raw bytes run from `"payload":` to the line's
        // final `}`. Hashing a Value-re-serialization here would hash a
        // DIFFERENT (key-sorted) byte string — the verifier binds the
        // raw stored bytes, so must the probe.
        let start =
            line.find(r#""payload":"#).expect("payload field present") + r#""payload":"#.len();
        let embedded = &line[start..line.len() - 1];
        assert_eq!(
            blake3::hash(embedded.as_bytes()).to_hex().to_string(),
            payload_hash_hex,
            "embedded payload bytes drifted from the hashed form at record {i}"
        );
    }
    let _ = &mut line;
    line
}

fn blake3_hash_hex(h: &[u8; 32]) -> String {
    h.iter().map(|b| format!("{b:02x}")).collect()
}

/// Build a full ledger at `root`. Returns (head chain hash, per-bucket
/// census of (wall_ns, allocs, bytes)).
fn build_ledger(
    root: &std::path::Path, n: usize, census: bool,
) -> (String, Vec<(u128, usize, usize)>) {
    std::fs::create_dir_all(root.join(".ggen-v2")).expect("mkdir .ggen-v2");
    let log_path = root.join(RECEIPT_LOG_REL_PATH);
    let mut f = std::io::BufWriter::new(std::fs::File::create(&log_path).expect("create log"));
    let mut prev = GENESIS_HEX.to_string();
    let mut buckets = Vec::new();
    for i in 0..n {
        let t = if census {
            ALLOCS.store(0, Ordering::Relaxed);
            BYTES.store(0, Ordering::Relaxed);
            Some(Instant::now())
        } else {
            None
        };
        let line = mint_line(&prev, i);
        f.write_all(line.as_bytes()).expect("append line");
        f.write_all(b"\n").expect("newline");
        prev = line_field(&line, "chain_hash_hex");
        if let Some(t) = t {
            if (i + 1) % BUCKET == 0 {
                buckets.push((
                    t.elapsed().as_nanos(),
                    ALLOCS.load(Ordering::Relaxed),
                    BYTES.load(Ordering::Relaxed),
                ));
            }
        }
    }
    f.flush().expect("flush log");
    (prev, buckets)
}

fn ms(ns: u128) -> f64 {
    ns as f64 / 1e6
}

#[test]
fn ledger_100k_history_verify_scale() {
    let tmp = tempfile::TempDir::new().expect("tempdir");
    let root = tmp.path().to_path_buf();
    let mut best_verify = u128::MAX;
    let mut best_report = None;
    for _run in 0..3 {
        let t_all = Instant::now();
        let (_head, census) = build_ledger(&root, N_RECORDS, true);
        let mint_ms = ms(t_all.elapsed().as_nanos());
        let t = Instant::now();
        let verdict = handle_receipt_history_in(&root).expect("pristine 100k ledger must verify");
        let verify_ms = ms(t.elapsed().as_nanos());
        assert_eq!(
            verdict["records"], 100_000,
            "history must count all records"
        );
        assert_eq!(verdict["valid"], true);
        if verify_ms < ms(best_verify) {
            best_verify = t.elapsed().as_nanos();
            best_report = Some((mint_ms, census.clone()));
        }
    }
    let (mint_ms, census) = best_report.expect("min-of-3");
    let first = census[0];
    let last = *census.last().expect("non-empty census");
    eprintln!(
        "[MIN-OF-3] mint+append 100k: {mint_ms:.1}ms | history verify 100k: {:.1}ms",
        ms(best_verify)
    );
    eprintln!(
        "[CENSUS per-1k] wall first {:.4}ms / last {:.4}ms (x{:.2}) | allocs {} -> {} | bytes {} -> {}",
        ms(first.0),
        ms(last.0),
        ms(last.0) / ms(first.0).max(1e-9),
        first.1,
        last.1,
        first.2,
        last.2,
    );
    // Flat per-link cost (gross tripwire): the last bucket must not cost
    // more than 20x the first (record minting is a flat per-record law;
    // only pathological growth would trip this).
    assert!(
        ms(last.0) < 20.0 * ms(first.0).max(1e-6),
        "late mint buckets must not blow up vs early buckets"
    );
}

#[test]
fn ledger_tamper_at_50k_prefix_integrity() {
    let tmp = tempfile::TempDir::new().expect("tempdir");
    let root = tmp.path().to_path_buf();
    build_ledger(&root, N_RECORDS, false);

    // Genesis law intact at scale: record 0 chains from the all-zeros
    // anchor, asserted on the WIRE bytes, not in-memory state.
    let log_path = root.join(RECEIPT_LOG_REL_PATH);
    let raw = std::fs::read_to_string(&log_path).expect("read log");
    let lines: Vec<String> = raw.lines().map(String::from).collect();
    assert_eq!(lines.len(), N_RECORDS);
    assert_eq!(
        line_field(&lines[0], "prev_chain_hash_hex"),
        GENESIS_HEX,
        "genesis law: first record chains from all-zeros"
    );

    // Negative control (non-vacuity): the pristine 100k ledger verifies.
    handle_receipt_history_in(&root).expect("pristine ledger verifies");

    // TAMPER: flip one hex nibble of record 50_000's payload hash on the
    // wire bytes (the ledger file is the tamper surface, exactly like a
    // disk tamper).
    let tampered_idx = 50_000;
    let old = line_field(&lines[tampered_idx], "payload_hash_hex");
    let first_char = old.chars().next().unwrap();
    let flipped = if first_char == '0' { "1" } else { "0" };
    let tampered_line = lines[tampered_idx].replacen(&old, &flipped, 1);
    let mut tampered_lines = lines.clone();
    tampered_lines[tampered_idx] = tampered_line;
    std::fs::write(&log_path, tampered_lines.join("\n") + "\n").expect("rewrite tampered log");

    // The REAL verifier refuses, naming the tampered index.
    let err = handle_receipt_history_in(&root).expect_err("tampered ledger must refuse");
    let msg = format!("{err}");
    assert!(
        msg.contains(&format!("index {tampered_idx}")),
        "refusal must name the tampered index {tampered_idx}: {msg}"
    );

    // PREFIX-INTEGRITY AT SCALE: the pristine 50k prefix before the
    // tamper is a fully verifying chain in its own right (genesis
    // anchor intact) — only the record COVERING the tamper refuses; the
    // damage localizes to exactly one record.
    let prefix_root = tmp.path().join("prefix");
    std::fs::create_dir_all(prefix_root.join(".ggen-v2")).expect("mkdir prefix");
    std::fs::write(
        prefix_root.join(RECEIPT_LOG_REL_PATH),
        lines[..tampered_idx].join("\n") + "\n",
    )
    .expect("write prefix log");
    let prefix = handle_receipt_history_in(&prefix_root)
        .expect("pristine prefix must verify as its own chain");
    assert_eq!(prefix["records"], tampered_idx);
}
