//! Kourani Stage-1 decomposition tests: the five hand-authored WF-nets from
//! the task (sequence, XOR-choice, AND-parallel, loop, and one non-separable
//! net that must be refused), plus the differential round-trip
//! `L(N) == L(decompose(N)) == L(recompose(decompose(N)))`.

use std::collections::BTreeSet;

use powl2_decompose::language::language_upto;
use powl2_decompose::{convert, recompose, Powl, RefusalReason, Trace, WfNet};

type Arc = (&'static str, &'static str);

/// Build a WF-net from string literals; labels default to the transition id,
/// unless the id starts with `tau` (silent `τ`).
fn net(
    places: &[&str], transitions: &[&str], pt: &[Arc], tp: &[Arc], source: &str, sink: &str,
) -> WfNet {
    let ts = transitions.iter().map(|t| {
        let label = if t.starts_with("tau") {
            None
        } else {
            Some((*t).to_string())
        };
        ((*t).to_string(), label)
    });
    WfNet::new(
        places.iter().map(|p| (*p).to_string()),
        ts,
        pt.iter().map(|(p, t)| ((*p).to_string(), (*t).to_string())),
        tp.iter().map(|(t, p)| ((*t).to_string(), (*p).to_string())),
        source,
        sink,
    )
    .expect("valid WF-net")
}

fn lang(traces: &[&[&str]]) -> BTreeSet<Trace> {
    traces
        .iter()
        .map(|t| t.iter().map(|s| (*s).to_string()).collect())
        .collect()
}

/// The core differential check: decomposition admits, and all three
/// independent computations of the bounded language agree.
fn assert_admits_and_roundtrips(net: &WfNet, max_len: usize) -> Powl {
    let model = convert(net).expect("separable net must be admitted");

    let net_lang = language_upto(net, max_len);
    let powl_lang = model.language_upto(max_len);
    assert_eq!(
        net_lang, powl_lang,
        "correctness: L(decompose(N)) must equal L(N)"
    );

    let recomposed = recompose(&model);
    let re_lang = language_upto(&recomposed, max_len);
    assert_eq!(
        re_lang, net_lang,
        "round-trip: L(recompose(decompose(N))) must equal L(N)"
    );

    model
}

// ── 1. Sequence: a then b ──────────────────────────────────────────────────────

fn sequence_net() -> WfNet {
    net(
        &["source", "p1", "sink"],
        &["a", "b"],
        &[("source", "a"), ("p1", "b")],
        &[("a", "p1"), ("b", "sink")],
        "source",
        "sink",
    )
}

#[test]
fn sequence_decomposes_to_partial_order() {
    let n = sequence_net();
    let model = assert_admits_and_roundtrips(&n, 8);
    assert!(
        matches!(model, Powl::PartialOrder { .. }),
        "a total order is a partial order"
    );
    assert_eq!(language_upto(&n, 8), lang(&[&["a", "b"]]));
}

// ── 2. Exclusive choice: a xor b ────────────────────────────────────────────────

fn xor_net() -> WfNet {
    net(
        &["source", "sink"],
        &["a", "b"],
        &[("source", "a"), ("source", "b")],
        &[("a", "sink"), ("b", "sink")],
        "source",
        "sink",
    )
}

#[test]
fn xor_decomposes_to_choice_graph() {
    let n = xor_net();
    let model = assert_admits_and_roundtrips(&n, 8);
    assert!(
        matches!(model, Powl::Choice { .. }),
        "an exclusive choice is a choice graph"
    );
    assert_eq!(language_upto(&n, 8), lang(&[&["a"], &["b"]]));
}

// ── 3. AND-parallel: a || b (silent split/join) ─────────────────────────────────

fn and_net() -> WfNet {
    net(
        &["source", "p1", "p2", "p3", "p4", "sink"],
        &["tau_split", "a", "b", "tau_join"],
        &[
            ("source", "tau_split"),
            ("p1", "a"),
            ("p2", "b"),
            ("p3", "tau_join"),
            ("p4", "tau_join"),
        ],
        &[
            ("tau_split", "p1"),
            ("tau_split", "p2"),
            ("a", "p3"),
            ("b", "p4"),
            ("tau_join", "sink"),
        ],
        "source",
        "sink",
    )
}

#[test]
fn and_decomposes_to_partial_order_with_concurrency() {
    let n = and_net();
    let model = assert_admits_and_roundtrips(&n, 8);
    assert!(matches!(model, Powl::PartialOrder { .. }));
    // both interleavings of the two concurrent labelled transitions
    assert_eq!(language_upto(&n, 8), lang(&[&["a", "b"], &["b", "a"]]));
}

// ── 4. Loop: a, then c any number of times, then b ──────────────────────────────

fn loop_net() -> WfNet {
    net(
        &["source", "p1", "sink"],
        &["a", "c", "b"],
        &[("source", "a"), ("p1", "c"), ("p1", "b")],
        &[("a", "p1"), ("c", "p1"), ("b", "sink")],
        "source",
        "sink",
    )
}

#[test]
fn loop_decomposes_to_cyclic_choice_graph() {
    let n = loop_net();
    let model = assert_admits_and_roundtrips(&n, 8);
    assert!(
        matches!(model, Powl::Choice { .. }),
        "a loop is a cyclic choice graph"
    );
    let expected = lang(&[
        &["a", "b"],
        &["a", "c", "b"],
        &["a", "c", "c", "b"],
        &["a", "c", "c", "c", "b"],
        &["a", "c", "c", "c", "c", "b"],
        &["a", "c", "c", "c", "c", "c", "b"],
        &["a", "c", "c", "c", "c", "c", "c", "b"],
    ]);
    assert_eq!(language_upto(&n, 8), expected);
}

// ── 5. Non-separable: non-free-choice long-term dependency ───────────────────────

/// `A` forks to `p1`,`p2`; `p1` chooses `B`(→p3) xor `C`(→p4); the joins `E`
/// (needs p2,p3) and `F` (needs p2,p4) share `p2` but not their full pre-set,
/// so the net is *not* free-choice — hence not separable (Def 3.13 corollary).
fn non_separable_net() -> WfNet {
    net(
        &["p0", "p1", "p2", "p3", "p4", "p5"],
        &["A", "B", "C", "E", "F"],
        &[
            ("p0", "A"),
            ("p1", "B"),
            ("p1", "C"),
            ("p2", "E"),
            ("p3", "E"),
            ("p2", "F"),
            ("p4", "F"),
        ],
        &[
            ("A", "p1"),
            ("A", "p2"),
            ("B", "p3"),
            ("C", "p4"),
            ("E", "p5"),
            ("F", "p5"),
        ],
        "p0",
        "p5",
    )
}

#[test]
fn non_separable_net_is_refused_with_receipt() {
    let n = non_separable_net();
    assert!(
        !n.is_free_choice(),
        "the witness net is deliberately non-free-choice"
    );

    let refusal = convert(&n).expect_err("non-separable net must be refused, not approximated");
    assert!(!refusal.separable);
    assert!(matches!(
        refusal.reason,
        RefusalReason::NonFreeChoice { .. }
    ));
    // Receipt is the input net's content address.
    assert_eq!(refusal.net_hash, n.content_hash());
    assert_eq!(refusal.net_hash.len(), 64, "BLAKE3 hex receipt");
    // Doctrine payoff: the refusal renders as a receipted reason.
    let rendered = refusal.to_string();
    assert!(rendered.contains("REFUSED (non-separable"));
    assert!(rendered.contains(&refusal.net_hash));
}

// ── determinism ──────────────────────────────────────────────────────────────

#[test]
fn decomposition_is_deterministic() {
    for n in [sequence_net(), xor_net(), and_net(), loop_net()] {
        let a = convert(&n).unwrap();
        let b = convert(&n).unwrap();
        assert_eq!(a, b, "decomposition must be byte-deterministic");
    }
}

/// The five nets collectively exercise every POWL 2.0 construct and the
/// refusal boundary — a compact separable/non-separable admission matrix.
#[test]
fn admission_matrix() {
    assert!(convert(&sequence_net()).is_ok());
    assert!(convert(&xor_net()).is_ok());
    assert!(convert(&and_net()).is_ok());
    assert!(convert(&loop_net()).is_ok());
    assert!(convert(&non_separable_net()).is_err());
}
