//! Chicago-style courts for the blast-radius gate (reverse-80/20 §3).
//! Real collaborators: the `VecGraph` fixture impl over real data; assertions
//! on final state (radius membership, verdict, receipt digests), never mocks.

use ggen_engine::blast_radius::*;

/// Fixture: a -> b -> c -> protected_def (sync.rs); d -> trait T (subtypes s1, s2).
fn fixture_graph() -> VecGraph {
    VecGraph {
        refs: vec![
            ("a".into(), "b".into()),
            ("b".into(), "c".into()),
            ("c".into(), "protected_def".into()),
            ("d".into(), "T".into()),
        ],
        subtypes: vec![("T".into(), "s1".into()), ("T".into(), "s2".into())],
        defs: vec![
            loc("app.rs", "a"),
            loc("ggen-engine/src/sync.rs", "b"),
            loc("app.rs", "c"),
            loc("app.rs", "d"),
            loc("lib.rs", "T"),
            loc("lib.rs", "s1"),
            loc("lib.rs", "s2"),
        ],
    }
}

fn loc(path: &str, symbol: &str) -> SymbolLoc {
    SymbolLoc {
        path: path.into(),
        symbol: symbol.into(),
    }
}

#[test]
fn radius_reaches_transitive_callers_over_fixture_graph() {
    let g = fixture_graph();
    let r = compute("c", &g, DEFAULT_MAX_DEPTH);
    // callers* of c: b, a. No subtypes.
    assert!(r.symbols.contains_key("b"));
    assert!(r.symbols.contains_key("a"));
    // callees are NOT in the blast radius (direction: callers*, outward).
    assert!(!r.symbols.contains_key("protected_def"));
    assert!(!r.truncated);
    assert_eq!(r.symbols.len(), 2);
}

#[test]
fn radius_includes_subtype_callers() {
    let g = fixture_graph();
    let r = compute("T", &g, DEFAULT_MAX_DEPTH);
    assert!(r.symbols.contains_key("s1"));
    assert!(r.symbols.contains_key("s2"));
    assert!(!r.truncated);
}

#[test]
fn admit_write_refuses_on_protected_path_intersection() {
    let g = fixture_graph();
    // Writing `c` impacts its callers: `b` (depth 1, defined in sync.rs) and
    // `a` (depth 2). The b hit is a protected-surface violation.
    let plan = WritePlan::new(vec![PlannedWrite {
        path: "app.rs".into(),
        symbols: vec!["c".into()],
    }]);
    let protected = vec![
        "ggen-engine/src/sync.rs".to_string(),
        "ggen-abb-sbb/src/lib.rs".to_string(),
    ];
    let v = admit_write(&plan, &g, &protected);
    match &v {
        Admission::Refused {
            violations,
            receipt,
            ..
        } => {
            assert_eq!(violations.len(), 1);
            let viol = &violations[0];
            assert_eq!(viol.symbol, "c");
            assert_eq!(viol.radius_symbol, "b");
            assert_eq!(viol.protected_path, "ggen-engine/src/sync.rs");
            assert_eq!(viol.depth, 1);
            assert_eq!(receipt.verdict, "REFUSED");
        }
        other => panic!("expected REFUSED, got {other:?}"),
    }
    // Receipt chain verifies: blake3(plan ++ radius ++ verdict).
    let (pd, rd) = match &v {
        Admission::Refused {
            plan_digest,
            receipt,
            ..
        } => (plan_digest.clone(), receipt.radius_digest.clone()),
        _ => unreachable!(),
    };
    let mut h = blake3::Hasher::new();
    h.update(pd.as_bytes());
    h.update(rd.as_bytes());
    h.update(b"REFUSED");
    let recomputed = h.finalize().to_hex().to_string();
    match &v {
        Admission::Refused { receipt, .. } => {
            assert_eq!(receipt.receipt_digest, recomputed);
        }
        _ => unreachable!(),
    }
}

#[test]
fn admit_write_passes_clean_plan_with_slice_payload() {
    let g = fixture_graph();
    // Writing `d` (no callers reference d; d only references T): empty radius,
    // clean ALIVE with the slice payload attached.
    let plan = WritePlan::new(vec![PlannedWrite {
        path: "app.rs".into(),
        symbols: vec!["d".into()],
    }]);
    let protected = vec![
        "ggen-engine/src/sync.rs".to_string(),
        "ggen-abb-sbb/src/lib.rs".to_string(),
    ];
    let v = admit_write(&plan, &g, &protected);
    match &v {
        Admission::Alive {
            radius,
            receipt,
            plan_digest,
            ..
        } => {
            // Slice carries the written symbol itself plus its (empty) radius.
            assert_eq!(radius.symbols.len(), 1);
            assert_eq!(radius.symbols.get("d"), Some(&RadiusOrigin::Written));
            assert!(!radius.truncated);
            assert_eq!(receipt.verdict, "ALIVE");
            assert_eq!(receipt.depth_cap, DEFAULT_MAX_DEPTH);
            // plan_digest is stable: same plan -> same digest.
            let v2 = admit_write(&plan, &g, &protected);
            match v2 {
                Admission::Alive {
                    plan_digest: pd2, ..
                } => {
                    assert_eq!(*plan_digest, pd2);
                }
                other => panic!("expected ALIVE on replay, got {other:?}"),
            }
        }
        other => panic!("expected ALIVE, got {other:?}"),
    }
}

#[test]
fn depth_cap_truncates_and_degrades_alive_to_partial() {
    let g = fixture_graph();
    // Writing `c` with cap 1: `b` enters at depth 1; b's own expansion is
    // refused by the cap (1 >= 1), so `a` (depth 2) is never reached.
    let plan = WritePlan {
        writes: vec![PlannedWrite {
            path: "app.rs".into(),
            symbols: vec!["c".into()],
        }],
        max_depth: Some(1),
    };
    // Protected list excludes sync.rs (where b lives) so no violation fires;
    // the truncation itself is what we are testing.
    let protected = vec!["ggen-abb-sbb/src/lib.rs".to_string()];
    let v = admit_write(&plan, &g, &protected);
    match &v {
        Admission::Partial { radius, reason, .. } => {
            assert!(radius.truncated);
            assert!(radius.symbols.contains_key("b"));
            assert!(!radius.symbols.contains_key("a")); // beyond the cap
            assert!(reason.contains("lower bound"));
        }
        other => panic!("expected PARTIAL, got {other:?}"),
    }
    // The truncated radius never certifies ALIVE — PARTIAL is the point.
    assert!(!matches!(v, Admission::Alive { .. }));
}

#[test]
fn direct_write_to_protected_path_refuses_at_depth_zero() {
    let g = fixture_graph();
    let plan = WritePlan::new(vec![PlannedWrite {
        path: "ggen-abb-sbb/src/lib.rs".into(),
        symbols: vec!["abb_kernel".into()],
    }]);
    let protected = vec![
        "ggen-engine/src/sync.rs".to_string(),
        "ggen-abb-sbb/src/lib.rs".to_string(),
    ];
    let v = admit_write(&plan, &g, &protected);
    match &v {
        Admission::Refused {
            violations,
            receipt,
            ..
        } => {
            assert_eq!(violations.len(), 1);
            assert_eq!(violations[0].depth, 0);
            assert_eq!(violations[0].protected_path, "ggen-abb-sbb/src/lib.rs");
            assert_eq!(receipt.verdict, "REFUSED");
        }
        other => panic!("expected REFUSED, got {other:?}"),
    }
}
