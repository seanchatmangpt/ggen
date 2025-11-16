//! Workflow Analytics and Process Mining Integration
//!
//! Tracks marketplace and university research implementation workflows,
//! discovers process patterns, and generates analytics reports.

use chrono::{DateTime, Utc};
use serde::{Deserialize, Serialize};
use std::collections::HashMap;

/// Workflow event types for tracking marketplace and research implementations
#[derive(Debug, Clone, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub enum WorkflowEventType {
    // University Research Workflow
    PaperSubmitted,
    DepartmentOnboarded,
    PilotStarted,
    CodeGenerated,
    TestsRun,
    SecurityAudit,
    BenchmarkCompleted,
    DocumentationGenerated,
    MarketplacePublished,
    AdoptionIncreased,

    // Package Maturity Workflow
    PackageCreated,
    MaturityAssessed,
    FeedbackGenerated,
    ImprovementsMade,
    ReleasePublished,
    DownloadsIncreased,
    CitationsAdded,

    // RevOps/GTM Workflow
    SubscriptionStarted,
    QuarterlyReviewScheduled,
    MetricsCollected,
    ReportGenerated,
    PartnershipEstablished,
    RevenueRecognized,
}

/// Workflow event representing a single activity in a process
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct WorkflowEvent {
    /// Unique event ID
    pub event_id: String,
    /// Case ID (e.g., paper_id, package_id, department_id)
    pub case_id: String,
    /// Event type/activity
    pub event_type: WorkflowEventType,
    /// Timestamp of event
    pub timestamp: DateTime<Utc>,
    /// Resource/actor performing the event
    pub resource: String,
    /// Optional attributes (e.g., maturity_score: 72)
    pub attributes: HashMap<String, String>,
}

/// Workflow trace representing a complete process execution
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct WorkflowTrace {
    pub case_id: String,
    pub events: Vec<WorkflowEvent>,
    pub start_time: DateTime<Utc>,
    pub end_time: DateTime<Utc>,
}

impl WorkflowTrace {
    /// Calculate total duration in minutes
    pub fn duration_minutes(&self) -> i64 {
        (self.end_time - self.start_time).num_minutes()
    }

    /// Get sequence of activities
    pub fn activity_sequence(&self) -> Vec<WorkflowEventType> {
        self.events.iter().map(|e| e.event_type.clone()).collect()
    }

    /// Get resource allocation
    pub fn resources_used(&self) -> Vec<String> {
        let mut resources: Vec<_> = self.events.iter().map(|e| e.resource.clone()).collect();
        resources.sort();
        resources.dedup();
        resources
    }
}

/// Workflow log containing multiple traces
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct WorkflowLog {
    pub name: String,
    pub traces: Vec<WorkflowTrace>,
}

impl WorkflowLog {
    pub fn new(name: &str) -> Self {
        Self {
            name: name.to_string(),
            traces: Vec::new(),
        }
    }

    pub fn add_event(&mut self, event: WorkflowEvent) {
        // Find or create trace for case_id
        if let Some(trace) = self.traces.iter_mut().find(|t| t.case_id == event.case_id) {
            if event.timestamp > trace.end_time {
                trace.end_time = event.timestamp;
            }
            trace.events.push(event);
        } else {
            let trace = WorkflowTrace {
                case_id: event.case_id.clone(),
                events: vec![event.clone()],
                start_time: event.timestamp,
                end_time: event.timestamp,
            };
            self.traces.push(trace);
        }
    }

    /// Get process statistics
    pub fn statistics(&self) -> ProcessStatistics {
        let variant_counts = self.variant_frequencies();
        let duration_stats = self.duration_statistics();

        ProcessStatistics {
            total_cases: self.traces.len(),
            total_events: self.traces.iter().map(|t| t.events.len()).sum(),
            unique_activities: self.unique_activities(),
            average_duration_minutes: duration_stats.0,
            median_duration_minutes: duration_stats.1,
            variant_count: variant_counts.len(),
            most_common_variant: variant_counts
                .iter()
                .max_by_key(|(_, count)| **count)
                .map(|(v, _)| v.clone()),
        }
    }

    fn unique_activities(&self) -> usize {
        let mut activities = std::collections::HashSet::new();
        for trace in &self.traces {
            for event in &trace.events {
                activities.insert(&event.event_type);
            }
        }
        activities.len()
    }

    fn variant_frequencies(&self) -> HashMap<String, usize> {
        let mut variants: HashMap<String, usize> = HashMap::new();
        for trace in &self.traces {
            let sequence = trace.activity_sequence();
            let variant_str = format!(
                "{:?}",
                sequence
                    .iter()
                    .map(|e| format!("{:?}", e))
                    .collect::<Vec<_>>()
            );
            *variants.entry(variant_str).or_insert(0) += 1;
        }
        variants
    }

    fn duration_statistics(&self) -> (f64, f64) {
        if self.traces.is_empty() {
            return (0.0, 0.0);
        }

        let mut durations: Vec<i64> = self.traces.iter().map(|t| t.duration_minutes()).collect();

        let average = durations.iter().sum::<i64>() as f64 / durations.len() as f64;

        durations.sort();
        let median = if durations.len() % 2 == 0 {
            (durations[durations.len() / 2 - 1] + durations[durations.len() / 2]) as f64 / 2.0
        } else {
            durations[durations.len() / 2] as f64
        };

        (average, median)
    }

    /// Discover directly-follows relationships (edges in process graph)
    pub fn discover_dfg(&self) -> DirectlyFollowsGraph {
        let mut follows: HashMap<(WorkflowEventType, WorkflowEventType), usize> = HashMap::new();
        let mut activity_frequencies: HashMap<WorkflowEventType, usize> = HashMap::new();

        for trace in &self.traces {
            let activities = trace.activity_sequence();

            // Count activity occurrences
            for activity in &activities {
                *activity_frequencies.entry(activity.clone()).or_insert(0) += 1;
            }

            // Count directly-follows relationships
            for window in activities.windows(2) {
                let (from, to) = (&window[0], &window[1]);
                *follows.entry((from.clone(), to.clone())).or_insert(0) += 1;
            }
        }

        DirectlyFollowsGraph {
            activities: activity_frequencies,
            follows,
        }
    }
}

/// Process Statistics Summary
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ProcessStatistics {
    pub total_cases: usize,
    pub total_events: usize,
    pub unique_activities: usize,
    pub average_duration_minutes: f64,
    pub median_duration_minutes: f64,
    pub variant_count: usize,
    pub most_common_variant: Option<String>,
}

/// Directly-Follows Graph for process visualization
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct DirectlyFollowsGraph {
    /// Activity frequencies
    pub activities: HashMap<WorkflowEventType, usize>,
    /// Follows relationships: (from, to) -> frequency
    pub follows: HashMap<(WorkflowEventType, WorkflowEventType), usize>,
}

impl DirectlyFollowsGraph {
    /// Get edges sorted by frequency (80/20: top edges cover 80% of paths)
    pub fn pareto_edges(
        &self, percentile: f32,
    ) -> Vec<((WorkflowEventType, WorkflowEventType), usize)> {
        let total_follows: usize = self.follows.values().sum();
        let threshold = total_follows as f32 * percentile;

        let mut edges: Vec<_> = self.follows.iter().map(|(k, v)| (k.clone(), *v)).collect();

        edges.sort_by_key(|(_k, v)| std::cmp::Reverse(*v));

        let mut cumulative = 0;
        edges
            .into_iter()
            .take_while(|(_k, v)| {
                cumulative += v;
                cumulative as f32 <= threshold
            })
            .collect()
    }

    /// Export as Mermaid flowchart for visualization
    pub fn to_mermaid(&self) -> String {
        let mut mermaid = String::from("graph TD\n");

        for ((from, to), freq) in &self.follows {
            if *freq > 0 {
                let from_str = format!("{:?}", from).replace(" ", "");
                let to_str = format!("{:?}", to).replace(" ", "");
                mermaid.push_str(&format!("  {}[\"{}({})\"]\n", from_str, from_str, freq));
                mermaid.push_str(&format!("  {} -->|{}| {}\n", from_str, freq, to_str));
            }
        }

        mermaid
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_workflow_log_creation() {
        let mut log = WorkflowLog::new("test-workflow");
        assert_eq!(log.traces.len(), 0);

        let event = WorkflowEvent {
            event_id: "1".to_string(),
            case_id: "case1".to_string(),
            event_type: WorkflowEventType::PaperSubmitted,
            timestamp: Utc::now(),
            resource: "researcher1".to_string(),
            attributes: HashMap::new(),
        };

        log.add_event(event);
        assert_eq!(log.traces.len(), 1);
    }

    #[test]
    fn test_dfg_discovery() {
        let mut log = WorkflowLog::new("test");
        let now = Utc::now();

        for i in 0..2 {
            log.add_event(WorkflowEvent {
                event_id: format!("e{}", i),
                case_id: "case1".to_string(),
                event_type: WorkflowEventType::PaperSubmitted,
                timestamp: now,
                resource: "r1".to_string(),
                attributes: HashMap::new(),
            });

            log.add_event(WorkflowEvent {
                event_id: format!("e{}", i + 100),
                case_id: "case1".to_string(),
                event_type: WorkflowEventType::CodeGenerated,
                timestamp: now,
                resource: "r2".to_string(),
                attributes: HashMap::new(),
            });
        }

        let dfg = log.discover_dfg();
        assert!(dfg.follows.len() > 0);
    }
}
