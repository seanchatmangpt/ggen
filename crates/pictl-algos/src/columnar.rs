//! Columnar encoding helpers — eliminate the 43-occurrence boilerplate in wasm4pm.
//!
//! All discovery algorithms share the same three operations:
//!   1. Encode activities as u32 IDs and count edges  (`build_edge_counts`)
//!   2. Materialize typed DFGEdges from integer counts (`materialize_dfg_edges`)
//!   3. Extract start/end activities from traces       (`extract_start_end`)
//!
//! When the `bcinr` feature is enabled, the Jaccard similarity helper delegates
//! to `bcinr::api::bitset::jaccard_u64_slices` for constant-latency execution.

use hashbrown::HashMap;
use pictl_types::{AttributeValue, DFGEdge, EventLog};

/// Result of encoding an event log into columnar (integer-keyed) form.
pub struct ColumnarEdgeCounts {
    /// Integer-keyed edge frequencies: (from_id, to_id) → count
    pub edge_counts: HashMap<(u32, u32), usize>,
    /// Vocabulary: position = activity ID, value = activity name
    pub vocab: Vec<String>,
    /// Start activities: activity name → trace count
    pub start_activities: HashMap<String, usize>,
    /// End activities: activity name → trace count
    pub end_activities: HashMap<String, usize>,
}

/// Single-pass columnar encoding of an event log.
///
/// Returns integer-keyed edge counts plus vocab and start/end activity maps.
/// Silently skips events that lack the `activity_key` attribute.
pub fn build_edge_counts(log: &EventLog, activity_key: &str) -> ColumnarEdgeCounts {
    let mut vocab: Vec<String> = Vec::new();
    let mut activity_ids: HashMap<String, u32> = HashMap::new();
    let mut edge_counts: HashMap<(u32, u32), usize> = HashMap::new();
    let mut start_activities: HashMap<String, usize> = HashMap::new();
    let mut end_activities: HashMap<String, usize> = HashMap::new();

    for trace in &log.traces {
        // Collect activity IDs for this trace, skipping events without the key.
        // Inline the interning to avoid a mutable closure capturing `vocab`.
        let mut ids: Vec<u32> = Vec::new();
        for event in &trace.events {
            if let Some(AttributeValue::String(name)) = event.attributes.get(activity_key) {
                let id = if let Some(&id) = activity_ids.get(name.as_str()) {
                    id
                } else {
                    let id = vocab.len() as u32;
                    vocab.push(name.clone());
                    activity_ids.insert(name.clone(), id);
                    id
                };
                ids.push(id);
            }
        }

        if ids.is_empty() {
            continue;
        }

        // Start / end activities — safe to read vocab here, no closure holds a borrow
        let first_name = vocab[ids[0] as usize].clone();
        *start_activities.entry(first_name).or_insert(0) += 1;
        let last_name = vocab[*ids.last().unwrap() as usize].clone();
        *end_activities.entry(last_name).or_insert(0) += 1;

        // Count directly-follows edges
        for pair in ids.windows(2) {
            *edge_counts.entry((pair[0], pair[1])).or_insert(0) += 1;
        }
    }

    ColumnarEdgeCounts {
        edge_counts,
        vocab,
        start_activities,
        end_activities,
    }
}

/// Materialize `DFGEdge` values from integer-keyed edge counts and a vocabulary.
///
/// This is the inner loop that was repeated verbatim across 11 discovery files.
pub fn materialize_dfg_edges(
    edge_counts: &HashMap<(u32, u32), usize>, vocab: &[String],
) -> Vec<DFGEdge> {
    edge_counts
        .iter()
        .map(|(&(from_id, to_id), &frequency)| {
            DFGEdge::new(
                vocab[from_id as usize].clone(),
                vocab[to_id as usize].clone(),
                frequency,
            )
        })
        .collect()
}

/// Extract start and end activity lists from a `ColumnarEdgeCounts` result.
pub fn extract_start_end(counts: &ColumnarEdgeCounts) -> (Vec<String>, Vec<String>) {
    let starts: Vec<String> = counts.start_activities.keys().cloned().collect();
    let ends: Vec<String> = counts.end_activities.keys().cloned().collect();
    (starts, ends)
}

/// Jaccard similarity between two activity-bitset slices.
///
/// With the `bcinr` feature: delegates to the branchless `jaccard_u64_slices`
/// (constant-latency, no branches). Without: identical portable fallback.
#[inline]
pub fn jaccard_similarity(a: &[u64], b: &[u64]) -> f32 {
    debug_assert_eq!(a.len(), b.len(), "bitset slices must have equal length");
    #[cfg(feature = "bcinr")]
    {
        bcinr::bitset::jaccard_u64_slices(a, b)
    }
    #[cfg(not(feature = "bcinr"))]
    {
        let mut intersection = 0u32;
        let mut union = 0u32;
        for i in 0..a.len() {
            intersection += (a[i] & b[i]).count_ones();
            union += (a[i] | b[i]).count_ones();
        }
        if union == 0 {
            1.0
        } else {
            intersection as f32 / union as f32
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use pictl_types::{Event, EventLog, Trace};
    use std::collections::HashMap as StdHashMap;

    fn make_log(traces: Vec<Vec<&str>>) -> EventLog {
        let traces = traces
            .into_iter()
            .enumerate()
            .map(|(i, activities)| {
                let events = activities
                    .into_iter()
                    .map(|a| {
                        let mut attrs = StdHashMap::new();
                        attrs.insert(
                            "concept:name".to_string(),
                            AttributeValue::String(a.to_string()),
                        );
                        Event::new(attrs)
                    })
                    .collect();
                Trace::new(format!("case{i}"), events)
            })
            .collect();
        EventLog::new(traces, StdHashMap::new())
    }

    #[test]
    fn test_build_edge_counts_basic() {
        let log = make_log(vec![vec!["A", "B", "C"], vec!["A", "B"]]);
        let counts = build_edge_counts(&log, "concept:name");

        assert_eq!(counts.vocab.len(), 3); // A, B, C
        assert_eq!(counts.edge_counts.values().sum::<usize>(), 3);
        // A→B appears twice, B→C once
        let a_id = counts.vocab.iter().position(|v| v == "A").unwrap() as u32;
        let b_id = counts.vocab.iter().position(|v| v == "B").unwrap() as u32;
        let c_id = counts.vocab.iter().position(|v| v == "C").unwrap() as u32;
        assert_eq!(counts.edge_counts[&(a_id, b_id)], 2);
        assert_eq!(counts.edge_counts[&(b_id, c_id)], 1);
    }

    #[test]
    fn test_start_end_activities() {
        let log = make_log(vec![vec!["A", "B", "C"]]);
        let counts = build_edge_counts(&log, "concept:name");
        let (starts, ends) = extract_start_end(&counts);
        assert_eq!(starts, vec!["A"]);
        assert_eq!(ends, vec!["C"]);
    }

    #[test]
    fn test_materialize_dfg_edges() {
        let mut edge_counts = HashMap::new();
        edge_counts.insert((0u32, 1u32), 3usize);
        let vocab = vec!["A".to_string(), "B".to_string()];
        let edges = materialize_dfg_edges(&edge_counts, &vocab);
        assert_eq!(edges.len(), 1);
        assert_eq!(edges[0].source, "A");
        assert_eq!(edges[0].target, "B");
        assert_eq!(edges[0].frequency, 3);
    }

    #[test]
    fn test_jaccard_identical() {
        let a = vec![0b1111u64];
        assert!((jaccard_similarity(&a, &a) - 1.0).abs() < f32::EPSILON);
    }

    #[test]
    fn test_jaccard_disjoint() {
        let a = vec![0b0101u64];
        let b = vec![0b1010u64];
        assert!((jaccard_similarity(&a, &b) - 0.0).abs() < f32::EPSILON);
    }
}
