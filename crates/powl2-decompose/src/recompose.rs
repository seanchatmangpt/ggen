//! Recomposition: POWL 2.0 → safe & sound WF-net (the "relatively
//! straightforward" inverse noted by Kourani et al. §1). Used by the
//! differential round-trip test: `L(recompose(decompose(N))) == L(N)`.
//!
//! - A **leaf** `t` becomes `source → t → sink`.
//! - A **partial order** becomes a marked-graph fork/join: a silent AND-split
//!   seeds every child, per-child silent gates enforce the cover (Hasse)
//!   relation of `≺`, and a silent AND-join collects them.
//! - A **choice graph** becomes a state machine: each graph edge is a silent
//!   transition splicing one child's exit place to the next child's entry
//!   place, so a single token walks exactly one `▷→…→□` path (cycles
//!   included).

use std::collections::{BTreeMap, BTreeSet};

use crate::net::{Label, WfNet};
use crate::powl::{ChoiceGraph, GNode, Powl};

/// Recompose a POWL 2.0 model into an equivalent safe & sound WF-net.
#[must_use]
pub fn recompose(model: &Powl) -> WfNet {
    let mut b = Builder::default();
    let (src, snk) = b.build(model);
    b.finish(src, snk)
}

#[derive(Default)]
struct Builder {
    counter: usize,
    places: BTreeSet<String>,
    transitions: BTreeMap<String, Label>,
    pt: BTreeSet<(String, String)>,
    tp: BTreeSet<(String, String)>,
}

impl Builder {
    fn id(&mut self, stem: &str) -> String {
        let id = format!("{stem}#{}", self.counter);
        self.counter += 1;
        id
    }

    fn place(&mut self, stem: &str) -> String {
        let id = self.id(stem);
        self.places.insert(id.clone());
        id
    }

    fn trans(&mut self, stem: &str, label: Label) -> String {
        let id = self.id(stem);
        self.transitions.insert(id.clone(), label);
        id
    }

    fn arc_pt(&mut self, p: &str, t: &str) {
        self.pt.insert((p.to_string(), t.to_string()));
    }
    fn arc_tp(&mut self, t: &str, p: &str) {
        self.tp.insert((t.to_string(), p.to_string()));
    }

    /// Build the sub-net for `model`, returning its `(source, sink)` places.
    fn build(&mut self, model: &Powl) -> (String, String) {
        match model {
            Powl::Leaf(label) => {
                let s = self.place("s");
                let k = self.place("k");
                let t = self.trans("t", label.clone());
                self.arc_pt(&s, &t);
                self.arc_tp(&t, &k);
                (s, k)
            }
            Powl::PartialOrder { children, order } => self.build_partial_order(children, order),
            Powl::Choice { children, graph } => self.build_choice(children, graph),
            Powl::ExternalCut { region, .. } => self.build(region),
        }
    }

    fn build_partial_order(
        &mut self, children: &[Powl], order: &BTreeSet<(usize, usize)>,
    ) -> (String, String) {
        let n = children.len();
        let subs: Vec<(String, String)> = children.iter().map(|c| self.build(c)).collect();
        let cover = cover_relation(order, n);

        let s = self.place("po_s");
        let k = self.place("po_k");
        let init = self.trans("po_init", None);
        let fini = self.trans("po_fini", None);
        self.arc_pt(&s, &init);
        self.arc_tp(&fini, &k);

        // Per-child ready/post places + gating transitions.
        let ready: Vec<String> = (0..n).map(|_| self.place("po_ready")).collect();
        let post: Vec<String> = (0..n).map(|_| self.place("po_post")).collect();
        // One edge place per cover pair (j -> i).
        let mut edge_place: BTreeMap<(usize, usize), String> = BTreeMap::new();
        for &(j, i) in &cover {
            let ep = self.place("po_edge");
            edge_place.insert((j, i), ep);
        }

        for i in 0..n {
            self.arc_tp(&init, &ready[i]);
            let go = self.trans("po_go", None);
            self.arc_pt(&ready[i], &go);
            // consume incoming cover edges (predecessors)
            for (&(j, ii), ep) in &edge_place {
                if ii == i {
                    let _ = j;
                    self.arc_pt(ep, &go);
                }
            }
            self.arc_tp(&go, &subs[i].0);

            let fin = self.trans("po_fin", None);
            self.arc_pt(&subs[i].1, &fin);
            self.arc_tp(&fin, &post[i]);
            // produce outgoing cover edges (successors)
            for (&(j, ii), ep) in &edge_place {
                if j == i {
                    let _ = ii;
                    self.arc_tp(&fin, ep);
                }
            }
            self.arc_pt(&post[i], &fini);
        }

        (s, k)
    }

    fn build_choice(&mut self, children: &[Powl], graph: &ChoiceGraph) -> (String, String) {
        let subs: Vec<(String, String)> = children.iter().map(|c| self.build(c)).collect();
        let s = self.place("cg_s");
        let k = self.place("cg_k");

        let out_place = |b: &Self, node: GNode| -> String {
            match node {
                GNode::Start => s.clone(),
                GNode::Child(i) => subs[i].1.clone(),
                GNode::End => {
                    let _ = b;
                    k.clone()
                }
            }
        };
        let in_place = |node: GNode| -> String {
            match node {
                GNode::Start => s.clone(),
                GNode::Child(i) => subs[i].0.clone(),
                GNode::End => k.clone(),
            }
        };

        for (u, v) in &graph.edges {
            let from = out_place(self, *u);
            let to = in_place(*v);
            let e = self.trans("cg_e", None);
            self.arc_pt(&from, &e);
            self.arc_tp(&e, &to);
        }

        (s, k)
    }

    fn finish(self, source: String, sink: String) -> WfNet {
        WfNet::new(
            self.places,
            self.transitions,
            self.pt,
            self.tp,
            source,
            sink,
        )
        .expect("recomposition yields a valid safe & sound WF-net")
    }
}

/// Cover (Hasse) relation of a transitively-closed strict partial order:
/// `(i,j)` with no intermediate `k` such that `i≺k≺j`.
fn cover_relation(order: &BTreeSet<(usize, usize)>, n: usize) -> BTreeSet<(usize, usize)> {
    let mut cover = BTreeSet::new();
    for &(i, j) in order {
        let has_mid =
            (0..n).any(|k| k != i && k != j && order.contains(&(i, k)) && order.contains(&(k, j)));
        if !has_mid {
            cover.insert((i, j));
        }
    }
    cover
}
