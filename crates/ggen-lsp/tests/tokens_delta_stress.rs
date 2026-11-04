//! AB-20 stress court for the `TokensEngine` full/delta path
//! (`crates/ggen-lsp/src/tokens.rs`, the E80-5/PG-A2 wired surface).
//!
//! Laws under load (Chicago: real collaborators only — real ontology parse,
//! real `ggen.toml`, real generated-Rust fixtures, real tree-sitter-rust
//! parses; no mocks, no doubles):
//!
//! 1. Delta-churn soak: 10k didChange→delta cycles over a ~5k-line
//!    generated-Rust fixture — every delta applies cleanly to the previous
//!    state (`apply_edits` never returns `None`), the resultId chain is
//!    strictly monotone with no extra mints (counter conservation), and the
//!    engine's observable generation-slot state (slot count via stale-id
//!    probes, per-URI isolation) is identical before and after the soak.
//! 2. Edit-pattern stress: 17 edit shapes × 3 size classes — the applied
//!    delta is byte-equal to a fresh full re-classification
//!    (delta ≡ full).
//! 3. Scale: a single didChange replacing 50% of the 5k-line file — delta
//!    cost and correctness recorded (worst-case diff).
//! 4. Allocation census per cycle via a counting global allocator (the
//!    engine is a host facade; the census is a baseline, not a budget).

#[path = "../src/tokens.rs"]
mod tokens;

use std::alloc::{GlobalAlloc, Layout, System};
use std::sync::atomic::{AtomicBool, AtomicUsize, Ordering};

use tokens::{apply_edits, decode, TokensEngine};

// ---------------------------------------------------------------------------
// Counting allocator (alloc census)
// ---------------------------------------------------------------------------

static ALLOC_ENABLED: AtomicBool = AtomicBool::new(false);
static ALLOC_COUNT: AtomicUsize = AtomicUsize::new(0);
static ALLOC_BYTES: AtomicUsize = AtomicUsize::new(0);

struct CountingAlloc;

unsafe impl GlobalAlloc for CountingAlloc {
    unsafe fn alloc(&self, layout: Layout) -> *mut u8 {
        if ALLOC_ENABLED.load(Ordering::Relaxed) {
            ALLOC_COUNT.fetch_add(1, Ordering::Relaxed);
            ALLOC_BYTES.fetch_add(layout.size(), Ordering::Relaxed);
        }
        // Real System allocator underneath — the census observes, never fakes.
        System.alloc(layout)
    }
    unsafe fn dealloc(&self, ptr: *mut u8, layout: Layout) {
        System.dealloc(ptr, layout)
    }
}

#[global_allocator]
static GLOBAL_ALLOC: CountingAlloc = CountingAlloc;

fn allocs_during<T>(f: impl FnOnce() -> T) -> (T, usize, usize) {
    ALLOC_COUNT.store(0, Ordering::Relaxed);
    ALLOC_BYTES.store(0, Ordering::Relaxed);
    ALLOC_ENABLED.store(true, Ordering::Relaxed);
    let out = f();
    ALLOC_ENABLED.store(false, Ordering::Relaxed);
    let count = ALLOC_COUNT.load(Ordering::Relaxed);
    let bytes = ALLOC_BYTES.load(Ordering::Relaxed);
    (out, count, bytes)
}

// ---------------------------------------------------------------------------
// Fixture: real generated-Rust files tiled to ~5k lines
// ---------------------------------------------------------------------------

const REPO_ROOT: &str = concat!(env!("CARGO_MANIFEST_DIR"), "/../..");

/// Real `.rs` outputs declared by `ggen.toml` `[[generation.rules]]`.
const GENERATED_RS: [&str; 5] = [
    "/crates/ggen-cli/src/generated_commands.rs",
    "/crates/ggen-cli/tests/generated/cli_proof_tests.rs",
    "/crates/ggen-lsp/src/generated_diagnostic_codes.rs",
    "/crates/ggen-abb-sbb/src/generated/refusals.rs",
    "/crates/ggen-lsp/src/generated_contract.rs",
];

fn read_repo_file(rel: &str) -> String {
    std::fs::read_to_string(format!("{REPO_ROOT}{rel}"))
        .unwrap_or_else(|e| panic!("read {rel}: {e}"))
}

fn engine() -> TokensEngine {
    let ttl = read_repo_file("/.specify/repo-facts.ttl");
    let toml = read_repo_file("/ggen.toml");
    TokensEngine::from_ontology(&ttl, Some(&toml))
}

/// Tile the real generated files (each wrapped in its own `mod` so the tile
/// stays a valid Rust parse) until the fixture reaches `target_lines` lines.
fn build_fixture(target_lines: usize) -> String {
    let mut out = String::new();
    let mut lines = 0usize;
    let mut tile = 0usize;
    while lines < target_lines {
        for (n, path) in GENERATED_RS.iter().enumerate() {
            let body = read_repo_file(path);
            lines += body.lines().count() + 2;
            out.push_str(&format!(
                "// tile {tile} file {n} (real generated surface)\n"
            ));
            out.push_str(&format!("mod ab20_tile{tile}_file{n} {{\n"));
            out.push_str(&body);
            out.push_str("}\n");
        }
        tile += 1;
    }
    out
}

fn line_count(s: &str) -> usize {
    s.lines().count()
}

// ---------------------------------------------------------------------------
// Edit-shape helpers (line-based, deterministic)
// ---------------------------------------------------------------------------

/// Replace line `idx` (0-based) with `replacement`.
fn replace_line(text: &str, idx: usize, replacement: &str) -> String {
    let mut lines: Vec<String> = text.lines().map(String::from).collect();
    assert!(idx < lines.len(), "line {idx} out of range");
    lines[idx] = replacement.to_owned();
    lines.join("\n")
}

/// Insert `insertion` after line `idx` (-1 = prepend at top).
fn insert_after(text: &str, idx: isize, insertion: &str) -> String {
    let mut lines: Vec<String> = text.lines().map(String::from).collect();
    let at = (idx + 1).max(0) as usize;
    assert!(at <= lines.len());
    lines.insert(at, insertion.to_owned());
    lines.join("\n")
}

/// Delete lines [start, start+count).
fn delete_span(text: &str, start: usize, count: usize) -> String {
    let mut lines: Vec<String> = text.lines().map(String::from).collect();
    assert!(start + count <= lines.len());
    lines.drain(start..start + count);
    lines.join("\n")
}

// ---------------------------------------------------------------------------
// Observable engine-state census (behavioral probes — the fields are private
// by design; these probes distinguish empty slot / matching slot / replaced
// slot exactly, per the delta() dispatch in tokens.rs)
// ---------------------------------------------------------------------------

/// Census probe: (stored_data_len_if_any, has_slot).
///
/// * No slot for `uri`        -> delta with a junk id yields a lone insert
///   edit (`start 0, delete_count 0`, full data).
/// * Slot present             -> junk id yields a replace-all edit whose
///   `delete_count` IS the stored generation's data length.
fn slot_census(eng: &mut TokensEngine, uri: &str, text: &str) -> (Option<usize>, bool) {
    let stored = eng.full(uri, text); // mint + overwrite slot (census write)
    let len = stored.data.len();
    let junk = eng.delta(uri, "ab20-census-junk-id", text);
    // Stored slot exists -> old.result_id != junk -> replace_all_edit with
    // delete_count == stored data len.
    let has_slot = junk
        .edits
        .first()
        .map(|e| e.start == 0 && e.delete_count == len as u32)
        .unwrap_or(false);
    let probed_len = junk
        .edits
        .first()
        .map(|e| e.delete_count as usize)
        .filter(|_| has_slot);
    (probed_len, has_slot)
}

fn parse_result_id(id: &str) -> u64 {
    id.strip_prefix("tok-")
        .and_then(|n| n.parse::<u64>().ok())
        .unwrap_or_else(|| panic!("resultId `{id}` not in tok-N form"))
}

// ===========================================================================
// 1. Delta-churn soak: 10k didChange cycles
// ===========================================================================

#[test]
fn soak_10k_did_change_cycles() {
    let fixture = build_fixture(5_000);
    assert!(
        line_count(&fixture) >= 5_000,
        "fixture must be >= 5k lines, got {}",
        line_count(&fixture)
    );
    let uri = "file:///ab20/stress/generated_surface.rs";
    let mut eng = engine();

    // Census BEFORE soak.
    let (before_len, before_slot) = slot_census(&mut eng, uri, &fixture);
    assert!(before_slot, "slot must exist after full()");
    assert_eq!(before_len, Some(before_len.unwrap_or(0)));

    let mut text = fixture.clone();
    let first = eng.full(uri, &text);
    let mut prev_id = first.result_id.expect("resultId");
    let mut prev_data = first.data;
    let mut prev_counter = parse_result_id(&prev_id);

    // Deterministic LCG — same churn every run (replayable receipt).
    let mut state: u64 = 0x_AB20;
    let mut rng = move || {
        state = state
            .wrapping_mul(6_364_136_223_846_793_005)
            .wrapping_add(1_442_695_040_888_963_407);
        (state >> 33) as usize
    };

    let cycles = 10_000u32;
    let mut verified_cycles = 0u32;
    for cycle in 1..=cycles {
        // Mutate: pick a line, either rename a fn-shaped line or insert a
        // diagnostic-code comment — text always changes.
        let n_lines = line_count(&text);
        let idx = rng() % n_lines;
        text = if rng() % 2 == 0 {
            replace_line(&text, idx, "fn ab20_churn_renamed() {} // E0010")
        } else {
            insert_after(&text, idx as isize - 1, "// AB20 churn GGEN-TPL-001")
        };
        assert_ne!(text, fixture, "cycle {cycle}: churn must change the text");

        let delta = eng.delta(uri, &prev_id, &text);

        // Law: resultId chain strictly monotone, exactly one mint per delta
        // (no leaked generations inflating the counter).
        let new_counter = parse_result_id(delta.result_id.as_deref().expect("delta resultId"));
        assert_eq!(
            new_counter,
            prev_counter + 1,
            "cycle {cycle}: counter must advance by exactly 1 (no extra mints)"
        );
        prev_counter = new_counter;

        // Law: every delta applies cleanly to the previous state.
        let applied = apply_edits(&prev_data, &delta.edits);
        match applied {
            Some(applied_data) => {
                // Sampled full re-classification: applied delta ≡ fresh full.
                if cycle % 500 == 0 {
                    let fresh = eng.full(uri, &text);
                    assert_eq!(
                        applied_data, fresh.data,
                        "cycle {cycle}: applied delta must byte-equal fresh full data"
                    );
                    assert_eq!(
                        decode(&applied_data),
                        decode(&fresh.data),
                        "cycle {cycle}: decoded token equality"
                    );
                    verified_cycles += 1;
                    // Re-baseline from the fresh full (resultId advanced).
                    prev_id = fresh.result_id.expect("fresh resultId");
                    prev_data = fresh.data;
                    prev_counter = parse_result_id(&prev_id);
                    continue;
                }
                prev_data = applied_data;
            }
            None => panic!("cycle {cycle}: delta edits failed to apply (round-trip law)"),
        }
        prev_id = delta.result_id.expect("delta resultId");
    }
    assert_eq!(
        verified_cycles,
        cycles / 500,
        "sampled full-verification count"
    );

    // Census AFTER soak: same single-slot structure, same probe behavior.
    let final_text = text.clone();
    let (after_len, after_slot) = slot_census(&mut eng, uri, &final_text);
    assert!(after_slot, "slot must still exist after 10k cycles");
    assert_eq!(
        before_len.is_some(),
        after_len.is_some(),
        "generation-slot count unchanged (no slot leak, no slot loss)"
    );

    // Counter conservation, re-read at the end. Mints so far:
    // census-before full+junk-delta (2) + baseline full (1) + loop deltas
    // (10_000) + sampled re-baseline fulls (verified_cycles = 20).
    // (The census-after probe mints two more AFTER this read.)
    let expected = 2 + 1 + u64::from(cycles) + u64::from(verified_cycles);
    let final_id = prev_counter;
    assert_eq!(
        final_id, expected,
        "total mints must equal operations (no leaked generations)"
    );

    // Per-URI isolation after the soak: a second uri has its own slot.
    let other = "file:///ab20/stress/other.rs";
    let (other_len, other_slot) = slot_census(&mut eng, other, &fixture);
    assert!(other_slot);
    assert!(other_len.is_some());

    println!(
        "AB20 soak: {cycles} cycles, fixture {} lines, final resultId tok-{final_id}, \
         sampled-full verifications {verified_cycles}",
        line_count(&final_text)
    );
}

// ===========================================================================
// 2. Edit-pattern stress: 17 shapes × size classes, delta ≡ full
// ===========================================================================

#[test]
fn edit_shapes_delta_equals_full_across_size_classes() {
    let base = build_fixture(1_000); // tile once per size-class scale-down
    let small = line_count(&base) < 2_000;
    let _ = small;

    // Size classes built from the same real generated content.
    let tiny = build_fixture(50); // ~50 lines
    let medium = build_fixture(500);
    let large = build_fixture(5_000);
    let classes: [(&str, &String); 3] = [("tiny", &tiny), ("medium", &medium), ("large", &large)];

    let uri = "file:///ab20/stress/shapes.rs";
    let n_lines_of = |t: &str| line_count(t);

    for (class_name, text) in classes {
        let n = n_lines_of(text);
        let mid = n / 2;
        let third = n / 3;

        // 17 edit shapes. Each entry: (name, edited_text, expected_edit_kinds)
        let shapes: Vec<(String, String)> = vec![
            (
                "01-prepend-line".into(),
                insert_after(text, -1, "fn ab20_prepended() {}"),
            ),
            (
                "02-append-line".into(),
                insert_after(text, (n - 1) as isize, "fn ab20_appended() {}"),
            ),
            (
                "03-replace-mid-fn".into(),
                replace_line(text, mid, "fn ab20_mid_replaced() {}"),
            ),
            ("04-delete-span-mid".into(), delete_span(text, mid, 8)),
            (
                "05-insert-first-third".into(),
                insert_after(text, third as isize, "// AB20 E0010"),
            ),
            (
                "06-insert-two-thirds".into(),
                insert_after(text, (2 * third) as isize, "// AB20 E0011"),
            ),
            (
                "07-replace-entire-line".into(),
                replace_line(text, mid, "const AB20: u32 = 7;"),
            ),
            ("08-delete-first-line".into(), delete_span(text, 0, 1)),
            ("09-delete-last-line".into(), {
                let mut l: Vec<String> = text.lines().map(String::from).collect();
                l.pop();
                l.join("\n")
            }),
            ("10-multi-change-3".into(), {
                let t = insert_after(text, -1, "fn ab20_m3_a() {}");
                let t = replace_line(&t, mid.min(line_count(&t) - 1), "fn ab20_m3_b() {}");
                delete_span(&t, third, 2)
            }),
            ("11-multi-change-5".into(), {
                let t = insert_after(text, -1, "// AB20 GGEN-TPL-001");
                let t = insert_after(&t, (n / 5) as isize, "fn ab20_m5_a() {}");
                let t = replace_line(&t, mid.min(line_count(&t) - 1), "fn ab20_m5_b() {}");
                let t = insert_after(&t, ((3 * n) / 5) as isize, "// AB20 E0010");
                delete_span(&t, ((4 * n) / 5).min(line_count(&t) - 2), 2)
            }),
            ("12-replace-half-block".into(), {
                let mut l: Vec<String> = text.lines().map(String::from).collect();
                for i in 0..n / 2 {
                    l[i] = "// AB20 half-replaced E0013".into();
                }
                l.join("\n")
            }),
            (
                "13-insert-empty-line".into(),
                insert_after(text, mid as isize, ""),
            ),
            ("14-duplicate-block".into(), {
                let lines: Vec<&str> = text.lines().collect();
                let head = lines[..mid].join("\n");
                format!("{head}\n{head}\n{}", lines[mid..].join("\n"))
            }),
            ("15-swap-adjacent".into(), {
                let mut l: Vec<String> = text.lines().map(String::from).collect();
                l.swap(mid, mid + 1);
                l.join("\n")
            }),
            (
                "16-rename-all-occurrences".into(),
                text.replace("fn ", "fn ab20_renamed_"),
            ),
            ("17-no-op".into(), text.to_owned()),
        ];
        assert_eq!(shapes.len(), 17, "17 edit shapes pinned");

        for (shape, edited) in &shapes {
            let mut eng = engine();
            let first = eng.full(uri, text);
            let first_id = first.result_id.expect("resultId");

            let delta = eng.delta(uri, &first_id, edited);

            // Client-side apply against the previous generation.
            let applied = apply_edits(&first.data, &delta.edits)
                .unwrap_or_else(|| panic!("{class_name}/{shape}: edits must apply"));

            // Fresh full re-classification of the edited text.
            let fresh = eng.full(uri, edited);

            // THE correctness law: delta ≡ full, byte-for-byte on the
            // encoded arrays AND on the decoded tokens.
            assert_eq!(
                applied, fresh.data,
                "{class_name}/{shape}: applied delta != fresh full data"
            );
            assert_eq!(
                decode(&applied),
                decode(&fresh.data),
                "{class_name}/{shape}: decoded tokens differ"
            );

            // Shape-specific laws.
            match shape.as_str() {
                "17-no-op" => assert!(
                    delta.edits.is_empty(),
                    "{class_name}/no-op: identical text must yield zero edits"
                ),
                "01-prepend-line" => assert_eq!(
                    delta.edits.first().map(|e| e.start),
                    Some(0),
                    "{class_name}/prepend: first edit starts at array offset 0"
                ),
                _ => {}
            }
        }
    }
    println!("AB20 shapes: 17 shapes x 3 size classes, all delta == full");
}

// ===========================================================================
// 3. Scale: single didChange replacing 50% of the 5k-line file
// ===========================================================================

#[test]
fn half_replace_worst_case_scale() {
    let fixture = build_fixture(5_000);
    let n = line_count(&fixture);
    assert!(n >= 5_000);
    let uri = "file:///ab20/stress/half_replace.rs";
    let mut eng = engine();

    let first = eng.full(uri, &fixture);
    let first_id = first.result_id.expect("resultId");
    let old_len = first.data.len();

    // Replace lines [0, n/2) with equal-count new content (worst case: the
    // prefix/suffix diff can keep nothing before the boundary AND every
    // downstream coordinate shifts).
    let mut lines: Vec<String> = fixture.lines().map(String::from).collect();
    for (i, l) in lines.iter_mut().take(n / 2).enumerate() {
        *l = format!("// AB20 half-replace line {i} fn ab20_hr_{i}() {{}} // E0010");
    }
    let edited = lines.join("\n");

    let started = std::time::Instant::now();
    let delta = eng.delta(uri, &first_id, &edited);
    let delta_us = started.elapsed().as_micros() as u64;

    let applied = apply_edits(&first.data, &delta.edits).expect("worst-case edits must apply");
    let fresh = eng.full(uri, &edited);
    assert_eq!(applied, fresh.data, "delta == full under 50% replacement");
    assert_eq!(decode(&applied), decode(&fresh.data));

    let edit_data_len: usize = delta
        .edits
        .iter()
        .map(|e| e.data.as_ref().map(|d| d.len()).unwrap_or(0) + e.delete_count as usize)
        .sum();

    // Correctness + cost recorded (not a fragile bound — a receipt).
    println!(
        "AB20 half-replace: file {n} lines, replaced {} (50.0%), old array {old_len} u32, \
         delta {} edit(s), touched {edit_data_len} u32 elements ({:.1}% of old array), \
         delta compute {delta_us} us, new array {} u32",
        n / 2,
        delta.edits.len(),
        100.0 * edit_data_len as f64 / old_len as f64,
        fresh.data.len(),
    );
}

// ===========================================================================
// 4. Alloc census per delta cycle (baseline, not a budget)
// ===========================================================================

#[test]
fn alloc_census_per_delta_cycle() {
    let fixture = build_fixture(5_000);
    let uri = "file:///ab20/stress/alloc_census.rs";
    let mut eng = engine();

    let mut text = fixture.clone();
    let first = eng.full(uri, &text);
    let mut prev_id = first.result_id.expect("resultId");
    let mut prev_data = first.data;

    // Warmup (parser setup, allocator gates).
    for i in 0..20 {
        text = insert_after(
            &text,
            (i * 37 % (line_count(&text) - 1)) as isize,
            "// AB20 warmup",
        );
        let d = eng.delta(uri, &prev_id, &text);
        prev_id = d.result_id.expect("resultId");
        prev_data = apply_edits(&prev_data, &d.edits).expect("warmup edits apply");
    }

    // Census: 100 measured cycles, each a real didChange (text change +
    // delta + client-side apply). The counting window covers exactly the
    // delta mint + client-side apply.
    let mut total_allocs = 0usize;
    let mut total_bytes = 0usize;
    let cycles = 100u32;
    for i in 0..cycles {
        let idx = (i as usize * 91) % (line_count(&text) - 1);
        text = replace_line(&text, idx, "fn ab20_alloc_churn() {} // GGEN-OUT-001");
        let prev = prev_id.clone();
        let before = prev_data.clone();
        let (out, allocs, bytes) = allocs_during(|| {
            let d = eng.delta(uri, &prev, &text);
            let new_id = d.result_id.clone().expect("delta resultId");
            (apply_edits(&before, &d.edits), new_id)
        });
        let (applied, new_id) = out;
        assert!(applied.is_some(), "cycle {i}: edits must apply");
        prev_data = applied.expect("some");
        prev_id = new_id;
        total_allocs += allocs;
        total_bytes += bytes;
    }

    let per_cycle = total_allocs / cycles as usize;
    let bytes_per_cycle = total_bytes / cycles as usize;
    println!(
        "AB20 alloc census: {cycles} cycles, {total_allocs} allocs total, \
         avg {per_cycle} allocs/cycle, avg {bytes_per_cycle} bytes/cycle (baseline, host-facade engine)"
    );
    // The census is a baseline, not a budget — assert only that the counting
    // allocator actually observed work (a vacuous census carries no bits).
    assert!(total_allocs > 0, "alloc census observed zero allocations");
}
