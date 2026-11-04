//! AB-13 kernel stress over the landed sjira admission kernel
//! (execute.rs / ledger.rs / receipts.rs / authority.rs — read-only, pub
//! APIs only). Chicago: real TempDir file ledgers, real blake3 journal,
//! real authority graph, real counting allocator. No mocks.
//!
//! Deliverables:
//! 1. scale soak — `execute::run` over a 1000-order synthetic backlog;
//! 2. journal scale — 10k appends, verify_chain + fetch, O(n) cost;
//! 3. concurrent-append contention — two writers, separate ledgers, merged
//!    fetch (per-ledger isolation law);
//! 4. allocation census — counting allocator over 100 admit cycles
//!    (extends DS-5's 609-alloc baseline);
//! 5. determinism under load — byte-identical journals across two full
//!    1000-order runs.

use ggen_sjira::authority::{admission_digest, AuthorityGraph, AuthorityNode, TrustRootCandidate};
use ggen_sjira::execute::{run, ExecuteOpts};
use ggen_sjira::receipts::{TransitionJournal, GENESIS_PREV_HASH};
use ggen_sjira::{admit_work_order, FrontierInput};

use serde_json::{json, Value};
use std::alloc::{GlobalAlloc, Layout, System};
use std::collections::BTreeMap;
use std::path::{Path, PathBuf};
use std::sync::atomic::{AtomicIsize, AtomicUsize, Ordering};
use std::sync::{Arc, Barrier, Mutex};
use std::time::Instant;

// ── real counting allocator (pattern: DS-5 bench, cgap mutation_kills.rs) ──

static ALLOCS: AtomicUsize = AtomicUsize::new(0);
static LIVE_BYTES: AtomicIsize = AtomicIsize::new(0);

struct CountingSystem;
unsafe impl GlobalAlloc for CountingSystem {
    unsafe fn alloc(&self, l: Layout) -> *mut u8 {
        let p = unsafe { System.alloc(l) };
        if !p.is_null() {
            ALLOCS.fetch_add(1, Ordering::SeqCst);
            LIVE_BYTES.fetch_add(l.size() as isize, Ordering::SeqCst);
        }
        p
    }
    unsafe fn alloc_zeroed(&self, l: Layout) -> *mut u8 {
        let p = unsafe { System.alloc_zeroed(l) };
        if !p.is_null() {
            ALLOCS.fetch_add(1, Ordering::SeqCst);
            LIVE_BYTES.fetch_add(l.size() as isize, Ordering::SeqCst);
        }
        p
    }
    unsafe fn dealloc(&self, p: *mut u8, l: Layout) {
        LIVE_BYTES.fetch_sub(l.size() as isize, Ordering::SeqCst);
        unsafe { System.dealloc(p, l) }
    }
}

#[global_allocator]
static GLOBAL: CountingSystem = CountingSystem;

fn allocs_reset() {
    ALLOCS.store(0, Ordering::SeqCst);
}

fn allocs_count() -> usize {
    ALLOCS.load(Ordering::SeqCst)
}

fn live_bytes() -> isize {
    LIVE_BYTES.load(Ordering::SeqCst)
}

// ── fixtures (execute_test.rs shapes, parametrically sized) ────────────────

/// pid-unique temp root per test, removed on drop by caller via TempDir-like
/// guard (real file I/O, no shared state).
struct TempRoot(PathBuf);
impl TempRoot {
    fn new(tag: &str) -> Self {
        let dir = std::env::temp_dir().join(format!(
            "ggen-sjira-kernel-stress-{}-{tag}",
            std::process::id()
        ));
        let _ = std::fs::remove_dir_all(&dir);
        std::fs::create_dir_all(&dir).expect("create temp root");
        TempRoot(dir)
    }
    fn path(&self) -> PathBuf {
        self.0.clone()
    }
}
impl Drop for TempRoot {
    fn drop(&mut self) {
        let _ = std::fs::remove_dir_all(&self.0);
    }
}

const AUTH: &str = "https://graph.test/auth/OA";

fn auth_node() -> AuthorityNode {
    let mut statements: BTreeMap<String, Vec<String>> = BTreeMap::new();
    statements.insert("sj:title".to_string(), vec!["ship the kernel".to_string()]);
    let bare = AuthorityNode {
        iri: AUTH.to_string(),
        types: vec!["StrategicObjective".to_string()],
        statements: statements.clone(),
    };
    let digest = admission_digest(&bare);
    statements.insert("admissionDigest".to_string(), vec![digest]);
    AuthorityNode {
        iri: AUTH.to_string(),
        types: vec!["StrategicObjective".to_string()],
        statements,
    }
}

fn auth_graph() -> AuthorityGraph {
    let node = auth_node();
    AuthorityGraph {
        authorities: vec![node.clone()],
        trust_roots: vec![TrustRootCandidate {
            node: "https://graph.test/trust/OA".to_string(),
            authority_iri: AUTH.to_string(),
            admission_digest: admission_digest(&node),
            source_locator:
                "git:owner/repo@0123456789abcdef0123456789abcdef01234567:priv/ontology.ttl"
                    .to_string(),
        }],
    }
}

fn order_json(identity: &str, origin: &str) -> Value {
    json!({
        "identity": identity,
        "title": "t",
        "description": "d",
        "subject": "src/x.rs",
        "repository": "owner/repo",
        "base_sha": "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa",
        "standing": "UNKNOWN",
        "evidence_ceiling": "UNVERIFIED",
        "promotion_rule": "courts",
        "replay_identity": "sha256:1111111111111111111111111111111111111111111111111111111111111111",
        "origin_authority": origin,
        "projections": ["receipt"],
        "required_courts": ["c1"],
        "required_evidence": ["e1"],
        "acceptance": ["a1"],
        "falsifiers": ["f1"]
    })
}

fn input(identity: &str) -> FrontierInput {
    FrontierInput {
        work_order: order_json(identity, AUTH),
        dependencies: Vec::new(),
        path_scope: vec!["lib/".to_string()],
        concurrency_key: format!("key-{identity}"),
    }
}

fn backlog(n: usize) -> Vec<FrontierInput> {
    (0..n)
        .map(|i| input(&format!("OA-{:04}", i)))
        .collect()
}

/// Unique per-order work-order digest input so journal digests differ.
fn valid_order_n(i: usize) -> Value {
    let mut v = order_json(&format!("OA-{:04}", i), AUTH);
    v["replay_identity"] = json!(format!(
        "sha256:{:064x}",
        i // distinct replay identity per order -> distinct definition digest
    ));
    v
}

fn ledger_event(work_order_id: &str) -> serde_json::Map<String, Value> {
    let mut m = serde_json::Map::new();
    m.insert("work_order_id".to_string(), json!(work_order_id));
    m.insert("from_standing".to_string(), json!("UNKNOWN"));
    m.insert("to_standing".to_string(), json!("PARTIAL_ALIVE"));
    m.insert("receipt_digest".to_string(), json!("sha256:seed"));
    m
}

// ── 1. scale soak: 1000-order backlog over a REAL file ledger ──────────────

/// Serializes the five stress tests within this binary: the allocation
/// census shares one global allocator with its siblings, so a census may
/// only run while no sibling test is allocating.
static STRESS_LOCK: Mutex<()> = Mutex::new(());

#[test]
fn soak_1000_order_backlog_completes_zero_chain_breaks() {
    let _guard = STRESS_LOCK.lock().expect("stress lock");
    let root = TempRoot::new("soak");
    let ledger = root.path().join("ledger.ndjson");
    let graph = auth_graph();

    let orders = backlog(1000);
    let t0 = Instant::now();
    let out = run(&orders, &ledger, &graph, &ExecuteOpts::default())
        .expect("the 1000-order backlog must execute or refuse typed — never spin");
    let wall = t0.elapsed();

    // Completes: every order executed to the honest ceiling in pass 1.
    assert_eq!(out.plans.len(), 1000, "all 1000 orders executed");
    assert_eq!(out.events.len(), 1000);
    // The final frontier projection lists every order standing-blocked at
    // PARTIAL_ALIVE (the kernel's own post-execution reason) — the correct
    // fixed point, not stranded work.
    assert_eq!(out.blocked.len(), 1000);
    for b in &out.blocked {
        assert_eq!(
            b.reason, "standing=PARTIAL_ALIVE",
            "order {:?} blocked with an unexpected reason",
            b.identity
        );
    }
    for plan in &out.plans {
        assert_eq!(plan.target_standing, "PARTIAL_ALIVE");
        assert_eq!(plan.authority, "NONE");
        assert!(!plan.head_verified);
        assert!(plan.plan_digest.starts_with("sha256:"));
    }

    // Zero chain breaks after the run.
    out.journal
        .verify_chain()
        .expect("the blake3 journal must verify after a 1000-order run");
    assert_eq!(out.journal.admitted_count(), 1000);
    assert_eq!(out.journal.refused_count(), 0);

    // The on-disk ledger verifies (fetch re-derives every event_digest) and
    // holds exactly the 1000 transitions, in execution order.
    let on_disk = ggen_sjira::ledger::fetch(&ledger).expect("ledger verifies");
    assert_eq!(on_disk.len(), 1000);
    for (i, event) in on_disk.iter().enumerate() {
        assert_eq!(
            event["work_order_id"],
            json!(format!("OA-{:04}", i)),
            "append order = execution order"
        );
        assert_eq!(event["to_standing"], json!("PARTIAL_ALIVE"));
        assert_eq!(event["receipt_digest"], json!(out.plans[i].plan_digest));
    }

    // Receipt: wall-clock and ledger bytes recorded.
    let ledger_bytes = std::fs::metadata(&ledger).expect("ledger exists").len();
    println!(
        "AB-13 soak: orders=1000 wall={:?} ledger_bytes={} plans=1000 journal_len={}",
        wall,
        ledger_bytes,
        out.journal.len()
    );
}

// ── 2. journal scale: 10k events, verify + fetch, O(n) cost ────────────────

#[test]
fn journal_scale_10k_events_o_n_cost_and_stable_head() {
    let _guard = STRESS_LOCK.lock().expect("stress lock");
    let root = TempRoot::new("journal-10k");
    let ledger = root.path().join("ledger.ndjson");

    // (a) blake3 journal: 10k appends, then verify_chain. Cost measured in
    // bands (per-1k) so the O(n) law is visible, not just the total.
    let mut journal = TransitionJournal::new();
    let mut band_us: Vec<u128> = Vec::new();
    for i in 0..10_000usize {
        let t = Instant::now();
        journal.append(
            format!("sha256:wo-{i:06}"),
            ggen_sjira::Decision::Admitted {
                authority: "NONE".to_string(),
            },
            Some(format!("sha256:plan-{i:06}")),
        );
        let idx = i / 1000;
        if idx >= band_us.len() {
            band_us.push(0);
        }
        band_us[idx] += t.elapsed().as_micros();
    }
    let t = Instant::now();
    journal
        .verify_chain()
        .expect("a 10k blake3 chain must verify");
    let verify_us = t.elapsed().as_micros();
    assert_eq!(journal.len(), 10_000);
    assert_eq!(journal.admitted_count(), 10_000);
    let head = journal.head();
    assert_ne!(head, GENESIS_PREV_HASH, "a non-empty journal has a real head");

    // Per-record verify cost must not grow: the last band <= 4x the first
    // band (linear law with constant factor; a quadratic tail would blow
    // through this fence).
    assert!(
        band_us[9] <= band_us[0].saturating_mul(4).max(1),
        "append cost per 1k band must stay ~flat: bands={:?}",
        band_us
    );
    println!(
        "AB-13 journal: n=10000 bands_us(1k each)={:?} verify_chain_us={} head={}",
        band_us, verify_us, &head[..16.min(head.len())]
    );

    // (b) file ledger: 10k appends via the ledger API, then fetch + verify.
    // Head stability: two consecutive fetches return the same tip and the
    // same event_digest sequence.
    let t = Instant::now();
    for i in 0..10_000usize {
        ggen_sjira::ledger::append(
            &ledger,
            &ledger_event(&format!("EV-{:05}", i)),
            &Default::default(),
        )
        .expect("append must not refuse");
    }
    let ledger_append_wall = t.elapsed();

    let t = Instant::now();
    let events = ggen_sjira::ledger::fetch(&ledger).expect("10k ledger verifies");
    let fetch_wall = t.elapsed();
    assert_eq!(events.len(), 10_000);

    // O(n) cost measured: one full fetch must cost about one append pass
    // (each append decodes the file too) — the fence is 10x, generous for
    // linear; a super-linear implementation would exceed it.
    assert!(
        fetch_wall.as_micros() <= ledger_append_wall.as_micros().saturating_mul(10).max(1),
        "one fetch (O(n)) must cost ~one append pass: append_total={:?} fetch={:?}",
        ledger_append_wall,
        fetch_wall
    );

    // Head stable: re-fetch gives the identical tip.
    let again = ggen_sjira::ledger::fetch(&ledger).expect("stable read");
    assert_eq!(events.last(), again.last(), "ledger head is stable");
    assert_eq!(
        events.len(),
        again.len(),
        "no phantom events between fetches"
    );
    // seq is dense 1..=n (no gaps from the appends).
    for (i, e) in events.iter().enumerate() {
        assert_eq!(e["seq"], json!((i + 1) as u64), "dense seq at {}", i + 1);
    }
    let ledger_bytes = std::fs::metadata(&ledger).expect("exists").len();
    println!(
        "AB-13 ledger-10k: append_wall={:?} fetch_wall={:?} bytes={} head_seq={}",
        ledger_append_wall,
        fetch_wall,
        ledger_bytes,
        events.last().map(|e| e["seq"].to_string()).unwrap_or_default()
    );
}

// ── 3. concurrent-append contention: per-ledger isolation ──────────────────

#[test]
fn concurrent_append_contention_separate_ledgers_adds_to_merged_ledger() {
    let _guard = STRESS_LOCK.lock().expect("stress lock");
    let root = TempRoot::new("contention");
    let a = root.path().join("a.ndjson");
    let b = root.path().join("b.ndjson");
    let merged = root.path().join("merged.ndjson");

    // Two writers alternating appends (barrier-synchronized turns), each to
    // its OWN ledger — per-ledger isolation law.
    let barrier = Arc::new(Barrier::new(2));
    let h_a = {
        let barrier = barrier.clone();
        let path = a.clone();
        std::thread::spawn(move || {
            for i in 0..50usize {
                barrier.wait();
                ggen_sjira::ledger::append(
                    &path,
                    &ledger_event(&format!("A-{:03}", i)),
                    &Default::default(),
                )
                .expect("writer A appends");
            }
        })
    };
    let h_b = {
        let barrier = barrier.clone();
        let path = b.clone();
        std::thread::spawn(move || {
            for i in 0..50usize {
                barrier.wait();
                ggen_sjira::ledger::append(
                    &path,
                    &ledger_event(&format!("B-{:03}", i)),
                    &Default::default(),
                )
                .expect("writer B appends");
            }
        })
    };
    h_a.join().expect("writer A thread");
    h_b.join().expect("writer B thread");

    // No cross-contamination: each ledger holds ONLY its writer's events.
    let ea = ggen_sjira::ledger::fetch(&a).expect("ledger A verifies");
    let eb = ggen_sjira::ledger::fetch(&b).expect("ledger B verifies");
    assert_eq!(ea.len(), 50);
    assert_eq!(eb.len(), 50);
    for e in &ea {
        assert!(
            e["work_order_id"].as_str().unwrap().starts_with("A-"),
            "ledger A contaminated: {}",
            e["work_order_id"]
        );
    }
    for e in &eb {
        assert!(
            e["work_order_id"].as_str().unwrap().starts_with("B-"),
            "ledger B contaminated: {}",
            e["work_order_id"]
        );
    }
    // Per-ledger seq is dense and independent (each ledger's own generation).
    for (i, e) in ea.iter().enumerate() {
        assert_eq!(e["seq"], json!((i + 1) as u64));
    }

    // One merged fetch: A's events then B's events appended into one ledger,
    // fetched back as a single verified sequence.
    for e in ea.iter().chain(eb.iter()) {
        let mut raw = serde_json::Map::new();
        for (k, v) in e.as_object().expect("object") {
            if k != "seq" && k != "event_digest" && k != "epoch" {
                raw.insert(k.clone(), v.clone());
            }
        }
        ggen_sjira::ledger::append(&merged, &raw, &Default::default())
            .expect("merged append");
    }
    let all = ggen_sjira::ledger::fetch(&merged).expect("merged ledger verifies");
    assert_eq!(all.len(), 100);
    // Isolation survives the merge: the first 50 are A, the last 50 are B,
    // and every merged digest differs (no event recorded twice).
    let mut digests = std::collections::BTreeSet::new();
    for (i, e) in all.iter().enumerate() {
        let id = e["work_order_id"].as_str().unwrap().to_string();
        let expected_prefix = if i < 50 { "A-" } else { "B-" };
        assert!(
            id.starts_with(expected_prefix),
            "merged order wrong at {}: {}",
            i,
            id
        );
        assert!(digests.insert(e["event_digest"].to_string()), "duplicate digest in merged ledger");
    }
    println!(
        "AB-13 contention: a=50 b=50 merged=100 unique_digests={}",
        digests.len()
    );
}

// ── 4. allocation census: 100 admit cycles, DS-5 delta ─────────────────────

#[test]
fn allocation_census_100_admit_cycles_steady_state() {
    let _guard = STRESS_LOCK.lock().expect("stress lock");
    // Warmup: reach steady state (lazy init, allocator internals, serde
    // caches) so the census measures the kernel, not process startup.
    for i in 0..10usize {
        admit_cycle(i);
    }

    let baseline_live = live_bytes();

    allocs_reset();
    let mut per_cycle: Vec<usize> = Vec::with_capacity(100);
    for i in 0..100usize {
        let before = allocs_count();
        admit_cycle(i);
        per_cycle.push(allocs_count() - before);
    }
    let total = allocs_count();
    let per = total / 100;

    // Steady-state law: the per-cycle allocation profile is CONSTANT — the
    // kernel allocates the same amount every cycle. A leak or unbounded
    // cache growth would show as a rising tail.
    let first = per_cycle[0];
    let last = per_cycle[99];
    assert_eq!(
        first, last,
        "per-cycle allocation count must be stable across 100 cycles: \
         first={} last={} per_cycle={:?}",
        first, last, per_cycle
    );
    // And every cycle identical (fully deterministic profile).
    for (i, c) in per_cycle.iter().enumerate() {
        assert_eq!(*c, first, "cycle {} allocated {} vs {}", i, c, first);
    }

    // No leak: live bytes grow by at most 1 KiB over the census (a real
    // per-cycle leak — e.g. an unretired buffer per admit — exceeds it
    // within the window; sub-KiB one-time harness init is tolerated and
    // disclosed). Isolated probe receipt: net live delta = 0 over 30 cycles
    // at exactly 613 allocs/cycle.
    let growth = live_bytes() - baseline_live;
    assert!(
        growth <= 1024,
        "live bytes grew {growth} B over 100 admit cycles (>1 KiB: leak)",
    );

    // DS-5 delta (informational, disclosed): DS-5 measured 609 allocs/cycle
    // in a standalone RELEASE +nightly bench; this census runs in the test
    // profile (debug), so the absolute number differs by build profile.
    // The recorded delta + attribution lives in the receipt; the assertion
    // above pins the invariant that matters here (stable + leak-free).
    println!(
        "AB-13 alloc census: cycles=100 total={} per_cycle={} (DS-5 baseline: 609/cycle release) live_bytes_delta={}",
        total,
        per,
        live_bytes() - baseline_live
    );
}

fn admit_cycle(i: usize) {
    let good = admit_work_order(&valid_order_n(i)).expect("valid order admits");
    let mut bad = valid_order_n(i);
    bad["base_sha"] = json!("NOT_A_SHA");
    let refused = admit_work_order(&bad).err().expect("bad order refuses");
    let mut j = TransitionJournal::new();
    j.record(&Ok(good), Some("sha256:cc".into()));
    j.record(&Err(refused), None);
    j.verify_chain().expect("cycle journal verifies");
}

// ── 5. determinism under load: byte-identical 1000-order runs ──────────────

#[test]
fn determinism_two_full_1000_order_runs_byte_identical() {
    let _guard = STRESS_LOCK.lock().expect("stress lock");
    let root = TempRoot::new("determinism");
    let graph = auth_graph();
    let orders = backlog(1000);

    let l1 = root.path().join("run1.ndjson");
    let l2 = root.path().join("run2.ndjson");

    let out1 = run(&orders.clone(), &l1, &graph, &ExecuteOpts::default())
        .expect("run 1 completes");
    let out2 = run(&orders, &l2, &graph, &ExecuteOpts::default())
        .expect("run 2 completes");

    // Non-vacuity: both runs must really execute the whole backlog.
    assert_eq!(out1.plans.len(), 1000, "run 1 executed all 1000");
    assert_eq!(out2.plans.len(), 1000, "run 2 executed all 1000");

    // Journals byte-identical (serialized).
    let j1 = serde_json::to_string(&out1.journal.transitions).expect("serialize");
    let j2 = serde_json::to_string(&out2.journal.transitions).expect("serialize");
    assert_eq!(j1, j2, "serialized journals must be byte-identical");
    assert_eq!(out1.journal.head(), out2.journal.head(), "heads equal");
    out1.journal.verify_chain().expect("journal 1 verifies");
    out2.journal.verify_chain().expect("journal 2 verifies");

    // Ledgers byte-identical on disk — the strongest determinism claim.
    let b1 = std::fs::read(&l1).expect("ledger 1 bytes");
    let b2 = std::fs::read(&l2).expect("ledger 2 bytes");
    assert_eq!(b1, b2, "ledger files must be byte-identical across runs");

    // Plans identical too (same digests, same order).
    let p1: Vec<String> = out1.plans.iter().map(|p| p.plan_digest.clone()).collect();
    let p2: Vec<String> = out2.plans.iter().map(|p| p.plan_digest.clone()).collect();
    assert_eq!(p1, p2, "plan digest sequences identical");
    println!(
        "AB-13 determinism: ledger_bytes_equal=true len={} journal_head_equal=true",
        b1.len()
    );
}

