use pictl_types::*;
use std::collections::HashMap;

/// Heuristic Miner - discovers DFG based on activity frequencies and directly-follows relationships
/// Time complexity: O(n + m) where n = events, m = edges
/// Space complexity: O(k + e) where k = unique activities, e = edges
pub fn discover_heuristic(log: &EventLog, activity_key: &str) -> Result<DFG> {
    let mut dfg = DFG::new();
    let mut node_map: HashMap<String, usize> = HashMap::new();
    let mut edge_counts: HashMap<(usize, usize), usize> = HashMap::new();
    let mut start_activities: HashMap<String, usize> = HashMap::new();
    let mut end_activities: HashMap<String, usize> = HashMap::new();

    // Single pass: collect all relationships
    for trace in &log.traces {
        let activities: Vec<String> = trace
            .events
            .iter()
            .filter_map(|e| e.get_activity(activity_key))
            .collect();

        if activities.is_empty() {
            continue;
        }

        // Record start activity
        *start_activities.entry(activities[0].clone()).or_insert(0) += 1;

        // Record end activity
        *end_activities
            .entry(activities[activities.len() - 1].clone())
            .or_insert(0) += 1;

        // Process nodes and edges
        for (i, activity) in activities.iter().enumerate() {
            let node_id = *node_map.entry(activity.clone()).or_insert_with(|| {
                let id = dfg.nodes.len();
                dfg.nodes.push(DFGNode::new(activity.clone(), 0));
                id
            });

            // Increment node frequency
            dfg.nodes[node_id].frequency += 1;

            // Record directly-follows edges (branchless: check all consecutive pairs)
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

    // Build edges from collected relationships
    for ((from_id, to_id), frequency) in edge_counts.iter() {
        let from = &dfg.nodes[*from_id].activity;
        let to = &dfg.nodes[*to_id].activity;
        dfg.edges
            .push(DFGEdge::new(from.clone(), to.clone(), *frequency));
    }

    // Set start and end activities in DFG
    dfg.start_activities = start_activities.keys().cloned().collect();
    dfg.end_activities = end_activities.keys().cloned().collect();

    Ok(dfg)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_heuristic_miner_simple() {
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

        let mut attrs_c = std::collections::HashMap::new();
        attrs_c.insert(
            "concept:name".to_string(),
            AttributeValue::String("C".to_string()),
        );

        let log = EventLog::new(
            vec![Trace::new(
                "case1".to_string(),
                vec![
                    Event::new(attrs_a.clone()),
                    Event::new(attrs_b.clone()),
                    Event::new(attrs_c.clone()),
                ],
            )],
            std::collections::HashMap::new(),
        );

        let dfg = discover_heuristic(&log, "concept:name").unwrap();
        assert_eq!(dfg.nodes.len(), 3);
        assert_eq!(dfg.edges.len(), 2); // A→B, B→C
        assert!(dfg.start_activities.contains(&"A".to_string()));
        assert!(dfg.end_activities.contains(&"C".to_string()));
    }

    #[test]
    fn test_heuristic_miner_parallel() {
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
            vec![
                Trace::new(
                    "case1".to_string(),
                    vec![Event::new(attrs_a.clone()), Event::new(attrs_b.clone())],
                ),
                Trace::new(
                    "case2".to_string(),
                    vec![Event::new(attrs_a.clone()), Event::new(attrs_b.clone())],
                ),
            ],
            std::collections::HashMap::new(),
        );

        let dfg = discover_heuristic(&log, "concept:name").unwrap();
        assert_eq!(dfg.nodes.len(), 2);
        assert_eq!(dfg.nodes[0].frequency, 2); // A appears twice
        assert_eq!(dfg.nodes[1].frequency, 2); // B appears twice
    }
}
