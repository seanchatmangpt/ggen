use pictl_types::*;
use std::collections::HashMap;

/// Inductive Miner - discovers structured process models recursively
/// Simplified single-pass version focused on core control flow discovery
pub fn discover_streaming_dfg(log: &EventLog, activity_key: &str) -> Result<DFG> {
    let mut dfg = DFG::new();
    let mut node_map: HashMap<String, usize> = HashMap::new();
    let mut edge_counts: HashMap<(usize, usize), usize> = HashMap::new();
    let mut start_activities: HashMap<String, usize> = HashMap::new();
    let mut end_activities: HashMap<String, usize> = HashMap::new();

    // Columnar-style single pass: collect activities in order
    for trace in &log.traces {
        let activities: Vec<String> = trace
            .events
            .iter()
            .filter_map(|e| e.get_activity(activity_key))
            .collect();

        if activities.is_empty() {
            continue;
        }

        // Infer nodes and assign IDs in columnar fashion
        for (i, activity) in activities.iter().enumerate() {
            let node_id = *node_map.entry(activity.clone()).or_insert_with(|| {
                let id = dfg.nodes.len();
                dfg.nodes.push(DFGNode::new(activity.clone(), 0));
                id
            });

            // Increment frequency (branchless)
            dfg.nodes[node_id].frequency += 1;

            // Record start activity (first event in trace)
            if i == 0 {
                *start_activities.entry(activity.clone()).or_insert(0) += 1;
            }

            // Record end activity (last event in trace)
            if i == activities.len() - 1 {
                *end_activities.entry(activity.clone()).or_insert(0) += 1;
            }

            // Branchless edge collection (no branching, just increment counter)
            if i + 1 < activities.len() {
                let from_id = node_id;
                let to_activity = &activities[i + 1];
                let to_id = *node_map.entry(to_activity.clone()).or_insert_with(|| {
                    let id = dfg.nodes.len();
                    dfg.nodes.push(DFGNode::new(to_activity.clone(), 0));
                    id
                });

                *edge_counts.entry((from_id, to_id)).or_insert(0) += 1;
            }
        }
    }

    // Materialize edges from aggregated counts
    for ((from_id, to_id), frequency) in edge_counts.iter() {
        let from = &dfg.nodes[*from_id].activity;
        let to = &dfg.nodes[*to_id].activity;
        dfg.edges
            .push(DFGEdge::new(from.clone(), to.clone(), *frequency));
    }

    // Assign start/end activities
    dfg.start_activities = start_activities.keys().cloned().collect();
    dfg.end_activities = end_activities.keys().cloned().collect();

    Ok(dfg)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_inductive_miner_sequence() {
        let mut attrs_a = std::collections::HashMap::new();
        attrs_a.insert(
            "concept:name".to_string(),
            AttributeValue::String("A".to_string()),
        );

        let mut attrs_b = std::collections::HashMap::new();
        attrs_b.insert(
            "concept:name".to_string(),
            AttributeValue::String("B".to_string()),
        );

        let log = EventLog::new(
            vec![Trace::new(
                "case1".to_string(),
                vec![Event::new(attrs_a), Event::new(attrs_b)],
            )],
            std::collections::HashMap::new(),
        );

        let dfg = discover_streaming_dfg(&log, "concept:name").unwrap();
        assert_eq!(dfg.nodes.len(), 2);
        assert_eq!(dfg.edges.len(), 1);
        assert_eq!(dfg.start_activities.len(), 1);
        assert_eq!(dfg.end_activities.len(), 1);
    }
}
