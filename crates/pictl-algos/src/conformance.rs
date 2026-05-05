use pictl_types::*;
use std::cmp::Reverse;
use std::collections::{BinaryHeap, HashMap, HashSet};

/// Token replay conformance checking.
///
/// For each trace: simulate a token multiset through the DFG model.
/// Fitness = 1.0 - missing / (consumed + missing)
pub fn check_conformance_token_replay(
    log: &EventLog,
    model: &DFG,
    activity_key: &str,
) -> Result<ConformanceResult> {
    // Build adjacency: source -> set of targets
    let mut successors: HashMap<&str, HashSet<&str>> = HashMap::new();
    let start_set: HashSet<&str> = model.start_activities.iter().map(|s| s.as_str()).collect();
    let end_set: HashSet<&str> = model.end_activities.iter().map(|s| s.as_str()).collect();

    for edge in &model.edges {
        successors
            .entry(edge.source.as_str())
            .or_default()
            .insert(edge.target.as_str());
    }

    let total_traces = log.traces.len();
    let mut total_produced: usize = 0;
    let mut total_consumed: usize = 0;
    let mut total_missing: usize = 0;
    let mut total_remaining: usize = 0;
    let mut fitting_traces: usize = 0;

    for trace in &log.traces {
        let activities: Vec<String> = trace
            .events
            .iter()
            .filter_map(|e| e.get_activity(activity_key))
            .collect();

        if activities.is_empty() {
            fitting_traces += 1;
            continue;
        }

        // Token replay on DFG:
        // Each DFG edge (a,b) corresponds to a "place" p_{a,b}.
        // Firing activity a produces a token in every place p_{a,x} for each successor x.
        // Firing activity b consumes a token from p_{a,b} for the preceding activity a.
        // Virtual start place feeds all start activities; virtual end place is consumed by end activities.
        //
        // Counters track total produced, consumed, and missing tokens (per Rozinat & van der Aalst).
        let mut marking: HashMap<String, usize> = HashMap::new();
        let mut produced: usize = 0;
        let mut consumed: usize = 0;
        let mut missing: usize = 0;

        // Fire start: produce initial token
        produced += 1;
        // Consume token to fire first activity
        // The first activity consumes from the virtual start place
        if start_set.contains(activities[0].as_str()) {
            consumed += 1; // token present — no missing
        } else {
            missing += 1;
            consumed += 1; // artificial token needed
        }

        for (i, activity) in activities.iter().enumerate() {
            // Produce tokens on outgoing DFG edges from this activity
            if let Some(succs) = successors.get(activity.as_str()) {
                for succ in succs.iter() {
                    let place = format!("p_{}_{}", activity, succ);
                    *marking.entry(place).or_insert(0) += 1;
                    produced += 1;
                }
            }

            // Consume token for next activity (if there is one)
            if i + 1 < activities.len() {
                let next = &activities[i + 1];
                let place = format!("p_{}_{}", activity, next);
                let available = marking.get(&place).copied().unwrap_or(0);
                if available > 0 {
                    *marking.get_mut(&place).unwrap() -= 1;
                    consumed += 1;
                } else {
                    // No token available on this edge — the edge is not in the model
                    missing += 1;
                    consumed += 1;
                }
            }
        }

        // Final activity should end at a sink — check end_set
        let last_act = &activities[activities.len() - 1];
        if end_set.contains(last_act.as_str()) {
            // Proper end — no extra missing
        } else {
            // Trace ends mid-model; remaining tokens will be counted
        }

        // Remaining tokens: any unconsumed tokens in the marking
        let remaining: usize = marking.values().sum();

        total_produced += produced;
        total_consumed += consumed;
        total_missing += missing;
        total_remaining += remaining;

        let trace_fitness =
            TokenReplayResult::calculate_fitness(produced, consumed, missing, remaining);
        if (trace_fitness - 1.0_f64).abs() < 1e-9 {
            fitting_traces += 1;
        }
    }

    let fitness = TokenReplayResult::calculate_fitness(
        total_produced,
        total_consumed,
        total_missing,
        total_remaining,
    );
    let deviating = total_traces - fitting_traces;

    Ok(ConformanceResult::new(
        fitness,
        total_traces,
        fitting_traces,
        deviating,
    ))
}

/// Alignment step in a synchronous product alignment
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct AlignmentStep {
    /// Activity in the trace (None = model move)
    pub log_activity: Option<String>,
    /// Activity in the model (None = log move)
    pub model_activity: Option<String>,
    /// 0 = sync move, 1 = log or model move
    pub cost: usize,
}

/// Full alignment result for a single trace
#[derive(Debug, Clone)]
pub struct TraceAlignment {
    pub steps: Vec<AlignmentStep>,
    pub total_cost: usize,
}

/// Alignment-based conformance checking (synchronous product / Dijkstra).
///
/// Log moves cost 1, model moves cost 1, sync moves cost 0.
pub fn check_conformance_alignment(
    log: &EventLog,
    model: &PetriNet,
    activity_key: &str,
) -> Result<ConformanceResult> {
    let total_traces = log.traces.len();
    let mut fitting_traces = 0usize;
    let mut total_cost = 0usize;

    // Build transition index: label -> transition id
    let label_to_tid: HashMap<&str, &str> = model
        .transitions
        .iter()
        .filter(|t| !t.invisible)
        .map(|t| (t.label.as_str(), t.id.as_str()))
        .collect();

    // Build a simple reachable marking graph from the Petri net.
    // For alignment we use a simplified state: (trace_position, current_place_multiset).
    // Since full Petri net reachability is expensive and this is a library implementation,
    // we use a linearised model: sequence of activities derived from the net topology.
    let model_sequence = derive_model_sequence(model);

    for trace in &log.traces {
        let trace_acts: Vec<String> = trace
            .events
            .iter()
            .filter_map(|e| e.get_activity(activity_key))
            .collect();

        let alignment = align_trace_to_model(&trace_acts, &model_sequence, &label_to_tid);
        total_cost += alignment.total_cost;
        if alignment.total_cost == 0 {
            fitting_traces += 1;
        }
    }

    // Normalise fitness: 1.0 when cost == 0, decreasing with cost
    let max_cost = log.traces.iter().map(|t| t.len()).sum::<usize>() + model_sequence.len();
    let fitness = if max_cost == 0 {
        1.0
    } else {
        (1.0 - total_cost as f64 / max_cost as f64).clamp(0.0, 1.0)
    };

    let deviating = total_traces - fitting_traces;
    Ok(ConformanceResult::new(
        fitness,
        total_traces,
        fitting_traces,
        deviating,
    ))
}

/// Derive a canonical activity sequence from the Petri net by topological traversal.
fn derive_model_sequence(model: &PetriNet) -> Vec<String> {
    // Build adjacency: place_id -> transition_ids that consume from it
    // and transition_id -> place_ids that it produces into.
    let mut place_to_trans: HashMap<&str, Vec<&str>> = HashMap::new();
    let mut trans_to_place: HashMap<&str, Vec<&str>> = HashMap::new();

    for arc in &model.arcs {
        // Determine if source is a place or transition
        let src_is_place = model.places.iter().any(|p| p.id == arc.source);
        if src_is_place {
            place_to_trans
                .entry(arc.source.as_str())
                .or_default()
                .push(arc.target.as_str());
        } else {
            trans_to_place
                .entry(arc.source.as_str())
                .or_default()
                .push(arc.target.as_str());
        }
    }

    // Find source place (initial_marking > 0 or named "source"/"start")
    let start_places: Vec<&str> = model
        .places
        .iter()
        .filter(|p| p.initial_marking > 0 || p.id.contains("source") || p.id.contains("start"))
        .map(|p| p.id.as_str())
        .collect();

    // BFS/DFS over transitions
    let mut visited_trans: HashSet<&str> = HashSet::new();
    let mut queue: Vec<&str> = Vec::new();
    let mut sequence: Vec<String> = Vec::new();

    for sp in &start_places {
        if let Some(trans) = place_to_trans.get(sp) {
            for t in trans {
                if visited_trans.insert(t) {
                    queue.push(t);
                }
            }
        }
    }

    while let Some(tid) = queue.first().cloned() {
        queue.remove(0);
        if let Some(t) = model.transitions.iter().find(|t| t.id == tid) {
            if !t.invisible {
                sequence.push(t.label.clone());
            }
            if let Some(out_places) = trans_to_place.get(tid) {
                for op in out_places {
                    if let Some(next_trans) = place_to_trans.get(op) {
                        for nt in next_trans {
                            if visited_trans.insert(nt) {
                                queue.push(nt);
                            }
                        }
                    }
                }
            }
        }
    }

    sequence
}

/// Align a trace to a model sequence using Dijkstra on the synchronous product.
///
/// State: (trace_idx, model_idx)
/// Moves:
///   - sync move: trace[i] == model[j]  -> (i+1, j+1), cost 0
///   - log move:  consume trace[i]       -> (i+1, j),   cost 1
///   - model move: consume model[j]      -> (i, j+1),   cost 1
fn align_trace_to_model(
    trace: &[String],
    model_seq: &[String],
    _label_to_tid: &HashMap<&str, &str>,
) -> TraceAlignment {
    let n = trace.len();
    let m = model_seq.len();

    // dist[i][j] = minimum cost to reach state (i, j)
    let mut dist = vec![vec![usize::MAX; m + 1]; n + 1];
    // prev[i][j] = (pi, pj, step)
    let mut prev: Vec<Vec<Option<(usize, usize, AlignmentStep)>>> = vec![vec![None; m + 1]; n + 1];

    dist[0][0] = 0;

    // Priority queue: (cost, trace_idx, model_idx)
    let mut heap: BinaryHeap<Reverse<(usize, usize, usize)>> = BinaryHeap::new();
    heap.push(Reverse((0, 0, 0)));

    while let Some(Reverse((cost, ti, mi))) = heap.pop() {
        if cost > dist[ti][mi] {
            continue;
        }
        if ti == n && mi == m {
            break;
        }

        // Sync move
        if ti < n && mi < m && trace[ti] == model_seq[mi] {
            let new_cost = cost;
            if new_cost < dist[ti + 1][mi + 1] {
                dist[ti + 1][mi + 1] = new_cost;
                prev[ti + 1][mi + 1] = Some((
                    ti,
                    mi,
                    AlignmentStep {
                        log_activity: Some(trace[ti].clone()),
                        model_activity: Some(model_seq[mi].clone()),
                        cost: 0,
                    },
                ));
                heap.push(Reverse((new_cost, ti + 1, mi + 1)));
            }
        }

        // Log move (consume trace event, no model progress)
        if ti < n {
            let new_cost = cost + 1;
            if new_cost < dist[ti + 1][mi] {
                dist[ti + 1][mi] = new_cost;
                prev[ti + 1][mi] = Some((
                    ti,
                    mi,
                    AlignmentStep {
                        log_activity: Some(trace[ti].clone()),
                        model_activity: None,
                        cost: 1,
                    },
                ));
                heap.push(Reverse((new_cost, ti + 1, mi)));
            }
        }

        // Model move (skip model transition, no trace progress)
        if mi < m {
            let new_cost = cost + 1;
            if new_cost < dist[ti][mi + 1] {
                dist[ti][mi + 1] = new_cost;
                prev[ti][mi + 1] = Some((
                    ti,
                    mi,
                    AlignmentStep {
                        log_activity: None,
                        model_activity: Some(model_seq[mi].clone()),
                        cost: 1,
                    },
                ));
                heap.push(Reverse((new_cost, ti, mi + 1)));
            }
        }
    }

    // Reconstruct path
    let total_cost = dist[n][m];
    let mut steps = Vec::new();
    let mut ci = n;
    let mut cj = m;
    while let Some((pi, pj, step)) = prev[ci][cj].clone() {
        steps.push(step);
        ci = pi;
        cj = pj;
    }
    steps.reverse();

    TraceAlignment {
        steps,
        total_cost: if total_cost == usize::MAX {
            n + m
        } else {
            total_cost
        },
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::collections::HashMap;

    fn make_event(activity: &str) -> Event {
        let mut attrs = HashMap::new();
        attrs.insert(
            "concept:name".to_string(),
            AttributeValue::String(activity.to_string()),
        );
        Event::new(attrs)
    }

    fn make_trace(case_id: &str, activities: &[&str]) -> Trace {
        Trace::new(
            case_id.to_string(),
            activities.iter().map(|a| make_event(a)).collect(),
        )
    }

    fn make_dfg(edges: &[(&str, &str)], starts: &[&str], ends: &[&str]) -> DFG {
        let mut dfg = DFG::new();
        let mut nodes: HashSet<String> = HashSet::new();
        for (s, t) in edges {
            nodes.insert(s.to_string());
            nodes.insert(t.to_string());
            dfg.edges
                .push(DFGEdge::new(s.to_string(), t.to_string(), 1));
        }
        for n in nodes {
            dfg.nodes.push(DFGNode::new(n, 1));
        }
        dfg.start_activities = starts.iter().map(|s| s.to_string()).collect();
        dfg.end_activities = ends.iter().map(|s| s.to_string()).collect();
        dfg
    }

    #[test]
    fn test_token_replay_conforming_trace_fitness_1() {
        // Model: A -> B -> C
        let log = EventLog::new(vec![make_trace("c1", &["A", "B", "C"])], HashMap::new());
        let model = make_dfg(&[("A", "B"), ("B", "C")], &["A"], &["C"]);

        let result = check_conformance_token_replay(&log, &model, "concept:name").unwrap();
        // A perfectly conforming trace should yield fitness 1.0
        assert!(
            result.fitness >= 0.99,
            "Expected fitness ~1.0, got {}",
            result.fitness
        );
        assert_eq!(result.fitting_traces, 1);
        assert_eq!(result.deviating_traces, 0);
    }

    #[test]
    fn test_token_replay_nonconforming_trace_fitness_less_than_1() {
        // Model: A -> B -> C, but trace has extra event D
        let log = EventLog::new(
            vec![make_trace("c1", &["A", "B", "D", "C"])],
            HashMap::new(),
        );
        let model = make_dfg(&[("A", "B"), ("B", "C")], &["A"], &["C"]);

        let result = check_conformance_token_replay(&log, &model, "concept:name").unwrap();
        // D is not in the model, so fitness should be < 1.0
        assert!(
            result.fitness < 1.0,
            "Expected fitness < 1.0, got {}",
            result.fitness
        );
    }

    #[test]
    fn test_alignment_conforming_trace_zero_cost() {
        // Simple linear Petri net: source -> t_A -> p1 -> t_B -> sink
        let mut net = PetriNet::new();
        net.places.push(PetriNetPlace {
            id: "source".to_string(),
            label: "source".to_string(),
            initial_marking: 1,
        });
        net.places.push(PetriNetPlace {
            id: "p1".to_string(),
            label: "p1".to_string(),
            initial_marking: 0,
        });
        net.places.push(PetriNetPlace {
            id: "sink".to_string(),
            label: "sink".to_string(),
            initial_marking: 0,
        });
        net.transitions.push(PetriNetTransition::new(
            "t_A".to_string(),
            "A".to_string(),
            false,
        ));
        net.transitions.push(PetriNetTransition::new(
            "t_B".to_string(),
            "B".to_string(),
            false,
        ));
        net.arcs
            .push(PetriNetArc::new("source".to_string(), "t_A".to_string(), 1));
        net.arcs
            .push(PetriNetArc::new("t_A".to_string(), "p1".to_string(), 1));
        net.arcs
            .push(PetriNetArc::new("p1".to_string(), "t_B".to_string(), 1));
        net.arcs
            .push(PetriNetArc::new("t_B".to_string(), "sink".to_string(), 1));

        let log = EventLog::new(vec![make_trace("c1", &["A", "B"])], HashMap::new());

        let result = check_conformance_alignment(&log, &net, "concept:name").unwrap();
        assert_eq!(
            result.fitting_traces, 1,
            "Conforming trace should have zero alignment cost"
        );
    }

    #[test]
    fn test_alignment_nonconforming_trace_has_cost() {
        // Same linear net: A -> B, but trace is A -> B -> C (extra C)
        let mut net = PetriNet::new();
        net.places.push(PetriNetPlace {
            id: "source".to_string(),
            label: "source".to_string(),
            initial_marking: 1,
        });
        net.places.push(PetriNetPlace {
            id: "p1".to_string(),
            label: "p1".to_string(),
            initial_marking: 0,
        });
        net.places.push(PetriNetPlace {
            id: "sink".to_string(),
            label: "sink".to_string(),
            initial_marking: 0,
        });
        net.transitions.push(PetriNetTransition::new(
            "t_A".to_string(),
            "A".to_string(),
            false,
        ));
        net.transitions.push(PetriNetTransition::new(
            "t_B".to_string(),
            "B".to_string(),
            false,
        ));
        net.arcs
            .push(PetriNetArc::new("source".to_string(), "t_A".to_string(), 1));
        net.arcs
            .push(PetriNetArc::new("t_A".to_string(), "p1".to_string(), 1));
        net.arcs
            .push(PetriNetArc::new("p1".to_string(), "t_B".to_string(), 1));
        net.arcs
            .push(PetriNetArc::new("t_B".to_string(), "sink".to_string(), 1));

        let log = EventLog::new(vec![make_trace("c1", &["A", "B", "C"])], HashMap::new());

        let result = check_conformance_alignment(&log, &net, "concept:name").unwrap();
        assert!(
            result.fitness < 1.0,
            "Non-conforming trace should have fitness < 1.0"
        );
        assert_eq!(result.deviating_traces, 1);
    }
}
