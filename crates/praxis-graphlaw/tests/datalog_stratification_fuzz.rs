//! Independent-oracle fuzzing for Datalog stratification (targets the
//! specifically named low-confidence gap: only hand-picked adversarial
//! cycle topologies -- far-cycle, diamond, disjoint-cycles -- were tested
//! against `praxis_graphlaw::datalog::validate_rules`'s Bellman-Ford-relaxation
//! implementation; "untested doesn't mean correct" for the general case).
//!
//! This builds a SEPARATE, from-scratch reference stratification checker
//! (Tarjan's SCC algorithm: a program is stratifiable iff no strongly
//! connected component of its predicate-dependency graph contains a
//! negative edge) and cross-checks it against the real engine across
//! thousands of randomly generated rule sets -- a disagreement between two
//! independently-implemented algorithms on random input is a much stronger
//! signal than either algorithm agreeing with itself on hand-picked cases.

use praxis_graphlaw::datalog::validate_rules;
use praxis_graphlaw::triples::{BodyLiteral, Rule, Triple};
use proptest::prelude::*;
use std::collections::{HashMap, HashSet};

fn pred(n: usize) -> String {
    format!("http://example.org/P{}", n)
}

/// A generated program: `edges[i] = (from_predicate, to_predicate, negated)`
/// meaning "a rule with head `to_predicate` has a body literal over
/// `from_predicate`, negated or not." All rules share one body variable
/// `?x` plus a guaranteed positive `Base` literal, so every generated rule
/// is trivially SAFE (per `validate_rules`'s safety check) -- this fuzzer
/// targets the STRATIFICATION decision specifically, not safety-rejection.
fn build_rules(num_preds: usize, edges: &[(usize, usize, bool)]) -> Vec<Rule> {
    let mut rules = Vec::new();
    // Group edges by target predicate (each target gets exactly one rule
    // whose body is: Base(x), plus one literal per incoming edge).
    let mut by_target: HashMap<usize, Vec<(usize, bool)>> = HashMap::new();
    for &(from, to, negated) in edges {
        by_target.entry(to).or_default().push((from, negated));
    }
    for p in 0..num_preds {
        let mut body = vec![BodyLiteral {
            negated: false,
            pattern: Triple::from(
                "?x".to_string(),
                pred(num_preds + 100),
                "http://example.org/true".to_string(),
            ), // "Base"
        }];
        if let Some(incoming) = by_target.get(&p) {
            for &(from, negated) in incoming {
                body.push(BodyLiteral {
                    negated,
                    pattern: Triple::from(
                        "?x".to_string(),
                        pred(from),
                        "http://example.org/true".to_string(),
                    ),
                });
            }
        }
        rules.push(Rule {
            head: Triple::from(
                "?x".to_string(),
                pred(p),
                "http://example.org/true".to_string(),
            ),
            body,
        });
    }
    rules
}

/// Independent reference stratification checker: Tarjan's SCC algorithm
/// over the predicate-dependency graph, then reject iff any SCC contains a
/// negative edge between two of its own members (including a negative
/// self-loop).
fn independent_stratifiable(num_preds: usize, edges: &[(usize, usize, bool)]) -> bool {
    let mut adj: Vec<Vec<usize>> = vec![Vec::new(); num_preds];
    for &(from, to, _) in edges {
        adj[from].push(to); // edge direction: body-predicate -> head-predicate (dependency)
    }

    // Tarjan's SCC.
    let mut index_counter = 0usize;
    let mut stack = Vec::new();
    let mut on_stack = vec![false; num_preds];
    let mut indices: Vec<Option<usize>> = vec![None; num_preds];
    let mut lowlink = vec![0usize; num_preds];
    let mut sccs: Vec<Vec<usize>> = Vec::new();

    // Tarjan helper: argument count follows the textbook algorithm state.
    #[allow(clippy::too_many_arguments, clippy::ptr_arg)]
    fn strongconnect(
        v: usize, adj: &Vec<Vec<usize>>, index_counter: &mut usize, stack: &mut Vec<usize>,
        on_stack: &mut Vec<bool>, indices: &mut Vec<Option<usize>>, lowlink: &mut Vec<usize>,
        sccs: &mut Vec<Vec<usize>>,
    ) {
        indices[v] = Some(*index_counter);
        lowlink[v] = *index_counter;
        *index_counter += 1;
        stack.push(v);
        on_stack[v] = true;

        for &w in &adj[v] {
            if indices[w].is_none() {
                strongconnect(
                    w,
                    adj,
                    index_counter,
                    stack,
                    on_stack,
                    indices,
                    lowlink,
                    sccs,
                );
                lowlink[v] = lowlink[v].min(lowlink[w]);
            } else if on_stack[w] {
                lowlink[v] = lowlink[v].min(indices[w].unwrap());
            }
        }

        if lowlink[v] == indices[v].unwrap() {
            let mut component = Vec::new();
            loop {
                let w = stack.pop().unwrap();
                on_stack[w] = false;
                component.push(w);
                if w == v {
                    break;
                }
            }
            sccs.push(component);
        }
    }

    for v in 0..num_preds {
        if indices[v].is_none() {
            strongconnect(
                v,
                &adj,
                &mut index_counter,
                &mut stack,
                &mut on_stack,
                &mut indices,
                &mut lowlink,
                &mut sccs,
            );
        }
    }

    // Reject if any SCC (size > 1, or size 1 with a self-loop) contains a
    // negative edge between two of its own members.
    for scc in &sccs {
        let members: HashSet<usize> = scc.iter().copied().collect();
        let is_self_looped =
            scc.len() == 1 && edges.iter().any(|&(f, t, _)| f == scc[0] && t == scc[0]);
        if scc.len() > 1 || is_self_looped {
            for &(from, to, negated) in edges {
                if negated && members.contains(&from) && members.contains(&to) {
                    return false;
                }
            }
        }
    }
    true
}

proptest! {
    #![proptest_config(ProptestConfig::with_cases(2000))]
    #[test]
    fn nk_stratification_fuzz(
        num_preds in 3usize..8,
        raw_edges in prop::collection::vec((0usize..8, 0usize..8, any::<bool>()), 3..20),
    ) {
        // Clip edge endpoints into range and drop any edge referencing an
        // out-of-range predicate for this run's num_preds.
        let edges: Vec<(usize, usize, bool)> = raw_edges
            .into_iter()
            .filter(|&(f, t, _)| f < num_preds && t < num_preds)
            .collect();

        let rules = build_rules(num_preds, &edges);
        let real_result = validate_rules(&rules, &HashMap::new());
        let real_stratifiable = real_result.is_ok();
        let oracle_stratifiable = independent_stratifiable(num_preds, &edges);

        prop_assert_eq!(
            real_stratifiable, oracle_stratifiable,
            "DISAGREEMENT: num_preds={} edges={:?} -- real engine stratifiable={:?}, independent SCC oracle stratifiable={}",
            num_preds, edges, real_result, oracle_stratifiable
        );
    }
}
