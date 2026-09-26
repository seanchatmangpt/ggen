//! Committed performance regression bounds (see `bench/receipt.json` for the recorded
//! criterion numbers). Bounds are ratios measured under the same load in the same
//! process, so they hold on a contended host or in a debug build; the absolute bound is
//! deliberately loose and only catches order-of-magnitude regressions.

use ggen_abb_sbb::*;
use std::time::{Duration, Instant};

const RECEIPT: &str = include_str!("../bench/receipt.json");

/// Admission must digest the graph once: admit / graph_digest stays well under 2
/// (measured ~2.06 before the single-digest fix, ~1.1 after).
const MAX_ADMIT_OVER_DIGEST: f64 = 1.6;
/// Manufacture is linear in artifacts: 64 artifacts vs 16 must stay under 8x (4x ideal).
const MAX_MANUFACTURE_64_OVER_16: f64 = 8.0;
/// Order-of-magnitude guard for one 64x64 admission in any build profile.
const MAX_ADMIT_64X64: Duration = Duration::from_secs(2);

/// Interleaved medians: each round times `a` then `b` back to back, so both see the same
/// ambient load (parallel tests, other processes) and the ratio stays meaningful.
fn paired_medians<A: FnMut(), B: FnMut()>(
    rounds: usize, mut a: A, mut b: B,
) -> (Duration, Duration) {
    let mut ta = Vec::with_capacity(rounds);
    let mut tb = Vec::with_capacity(rounds);
    for _ in 0..rounds {
        let t = Instant::now();
        a();
        ta.push(t.elapsed());
        let t = Instant::now();
        b();
        tb.push(t.elapsed());
    }
    ta.sort();
    tb.sort();
    (ta[rounds / 2], tb[rounds / 2])
}

/// Interleaved minima: the least-contended sample of each side.
fn paired_mins<A: FnMut(), B: FnMut()>(rounds: usize, a: A, b: B) -> (Duration, Duration) {
    let mut a = a;
    let mut b = b;
    let mut ma = Duration::MAX;
    let mut mb = Duration::MAX;
    for _ in 0..rounds {
        let t = Instant::now();
        a();
        ma = ma.min(t.elapsed());
        let t = Instant::now();
        b();
        mb = mb.min(t.elapsed());
    }
    (ma, mb)
}

fn req(g: &EaGraph, sbb: &str) -> Request {
    Request {
        abb: "abb:event-ingest".into(),
        sbb: sbb.into(),
        requested_authority: Authority::Construct,
        expected_graph_digest: Some(g.digest()),
    }
}

#[test]
fn committed_receipt_records_the_enforced_bounds() {
    let r: serde_json::Value = serde_json::from_str(RECEIPT).unwrap();
    assert_eq!(r["schema"], "ggen.abb-sbb.bench-receipt.v1");
    assert_eq!(
        r["bounds"]["max_admit_over_digest"].as_f64(),
        Some(MAX_ADMIT_OVER_DIGEST)
    );
    assert_eq!(
        r["bounds"]["max_manufacture_64_over_16"].as_f64(),
        Some(MAX_MANUFACTURE_64_OVER_16)
    );
    assert_eq!(
        r["bounds"]["max_admit_64x64_ms"].as_u64(),
        Some(MAX_ADMIT_64X64.as_millis() as u64)
    );
    // The receipt names the exact commit it measured (out of subject, C21) and its src tree.
    let measured = r["subject"]["measured_commit"].as_str().unwrap();
    assert!(
        measured.len() == 40 && measured.bytes().all(|b| b.is_ascii_hexdigit()),
        "measured_commit {measured:?} is not a full sha"
    );
    assert!(r["subject"]["measured_src_tree"].as_str().unwrap().len() == 40);
    let obs = r["observed_ratios"]["admit_over_digest_64x64"]
        .as_f64()
        .unwrap();
    assert!(
        obs < MAX_ADMIT_OVER_DIGEST,
        "recorded ratio {obs} already violates the bound"
    );
}

/// Structural, load-independent witness: one admission (and one plan, one replay)
/// computes the graph digest exactly once. Counted on this thread only, so parallel
/// tests and host load cannot flake it (the timing ratio below used to fail spuriously
/// at load ~296 and hide real mutant kills behind it).
#[test]
fn admission_digests_the_graph_once() {
    let g = synthetic_graph(64, 64);
    let r = req(&g, "sbb:ingest-0063");
    let before = graph_digests_computed();
    let ad = admit(&g, &r).unwrap();
    assert_eq!(graph_digests_computed() - before, 1, "admit");
    let before = graph_digests_computed();
    plan(&g, "abb:event-ingest", Authority::Construct).unwrap();
    assert_eq!(
        graph_digests_computed() - before,
        1,
        "plan over 64 candidates"
    );
    let gen = Generator {
        id: "ggen-abb-sbb".into(),
        version: "26.9.26".into(),
    };
    let m = manufacture(&ad, &gen).unwrap();
    let before = graph_digests_computed();
    replay(&m.receipt, &g, &gen).unwrap();
    assert_eq!(graph_digests_computed() - before, 1, "replay");
}

/// Timing twin of the structural witness: admit / digest at 64x64 stays under the
/// committed bound. Minimum of interleaved rounds (the least-contended sample of each)
/// is used, and the bound must hold in one of three attempts, so a burst of host load
/// cannot fail it while a real second digest (ratio ~2.06) still does.
#[test]
fn admission_cost_is_one_digest() {
    let g = synthetic_graph(64, 64);
    let r = req(&g, "sbb:ingest-0063");
    let mut seen = Vec::new();
    for _ in 0..3 {
        let (digest, admit_t) = paired_mins(
            9,
            || {
                std::hint::black_box(g.digest());
            },
            || {
                std::hint::black_box(admit(&g, &r).unwrap());
            },
        );
        let ratio = admit_t.as_secs_f64() / digest.as_secs_f64();
        eprintln!("digest={digest:?} admit={admit_t:?} ratio={ratio:.3}");
        assert!(admit_t < MAX_ADMIT_64X64, "admit 64x64 took {admit_t:?}");
        if ratio < MAX_ADMIT_OVER_DIGEST {
            return;
        }
        seen.push(ratio);
    }
    panic!("admit/digest >= {MAX_ADMIT_OVER_DIGEST} in 3 attempts: {seen:?}");
}

#[test]
fn manufacture_scales_linearly_in_artifacts() {
    let gen = Generator {
        id: "ggen-abb-sbb".into(),
        version: "26.9.26".into(),
    };
    let small = synthetic_graph(1, 16);
    let large = synthetic_graph(1, 64);
    let a_small = admit(&small, &req(&small, "sbb:ingest-0000")).unwrap();
    let a_large = admit(&large, &req(&large, "sbb:ingest-0000")).unwrap();
    let (t_small, t_large) = paired_medians(
        15,
        || {
            std::hint::black_box(manufacture(&a_small, &gen).unwrap());
        },
        || {
            std::hint::black_box(manufacture(&a_large, &gen).unwrap());
        },
    );
    let ratio = t_large.as_secs_f64() / t_small.as_secs_f64();
    eprintln!("manufacture16={t_small:?} manufacture64={t_large:?} ratio={ratio:.3}");
    assert!(ratio < MAX_MANUFACTURE_64_OVER_16, "64/16 = {ratio:.3}");
}
