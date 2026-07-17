//! Bounded token-game language of a [`WfNet`] — the independent oracle the
//! differential round-trip checks the POWL semantics against.
//!
//! A safe WF-net's marking is a *set* of places. Starting from `[source]`,
//! we DFS over enabled firings up to `max_len` transitions; whenever the
//! marking is exactly `[sink]` we record the label-sequence (silent `τ`
//! transitions elided). For acyclic nets and a large-enough bound this is the
//! full language; for cyclic nets it is the length-`≤max_len` prefix language
//! of terminating runs — which is precisely what [`crate::Powl::language_upto`]
//! also computes.

use std::collections::BTreeSet;

use crate::net::WfNet;
use crate::powl::Language;

/// The bounded language of `net`: all firing-sequence label traces of length
/// `≤ max_len` that lead from `[source]` to `[sink]`.
#[must_use]
pub fn language_upto(net: &WfNet, max_len: usize) -> Language {
    let mut out = Language::new();
    let start: BTreeSet<String> = [net.source().to_string()].into_iter().collect();
    let sink_marking: BTreeSet<String> = [net.sink().to_string()].into_iter().collect();
    let mut trace = Vec::new();
    // Guard against silent-only cycles inflating a single run without adding
    // labels: bound the number of *fired transitions*, not the trace length.
    explore(net, &start, &sink_marking, &mut trace, 0, max_len, &mut out);
    out
}

fn explore(
    net: &WfNet, marking: &BTreeSet<String>, sink_marking: &BTreeSet<String>,
    trace: &mut Vec<String>, fired: usize, max_len: usize, out: &mut Language,
) {
    if marking == sink_marking {
        out.insert(trace.clone());
        // The sink is a proper terminal (sink• = ∅ in a WF-net); no enabled
        // transition remains once the marking is exactly [sink], so we stop.
        return;
    }
    if fired >= 2 * max_len + 2 {
        // Fired-transition budget: enough to expose any labelled trace of
        // length ≤ max_len even when interleaved with silent steps.
        return;
    }
    for t in net.transitions().keys() {
        let pre = net.pre_trans(t);
        if !pre.is_subset(marking) {
            continue;
        }
        let mut next: BTreeSet<String> = marking.difference(&pre).cloned().collect();
        next.extend(net.post_trans(t));
        let label = net.label(t);
        let pushed = label.is_some();
        if let Some(a) = label {
            if trace.len() + 1 > max_len {
                continue;
            }
            trace.push(a);
        }
        explore(net, &next, sink_marking, trace, fired + 1, max_len, out);
        if pushed {
            trace.pop();
        }
    }
}
