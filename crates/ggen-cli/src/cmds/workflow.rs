//! Workflow Analytics Commands - clap-noun-verb v5.3.0 Migration
//!
//! Commands for tracking and analyzing marketplace and university research workflows
//! using process mining techniques with the v5.3.0 #[verb("verb", "noun")] pattern.

use chrono::{DateTime, Utc};
use clap_noun_verb::{NounVerbError, Result};
use clap_noun_verb_macros::verb;
use serde::{Deserialize, Serialize};
use serde_json::Value;
use std::collections::{BTreeMap, BTreeSet};
use std::fs;
use std::fs::File;
use std::io::BufWriter;
use std::path::{Path, PathBuf};

// ============================================================================
// Output Types
// ============================================================================

#[derive(Serialize)]
struct WorkflowInitOutput {
    workflow_name: String,
    path: String,
    status: String,
}

#[derive(Serialize)]
struct WorkflowAnalysisOutput {
    workflow_name: String,
    total_cases: usize,
    total_events: usize,
    unique_activities: usize,
    average_duration_minutes: f64,
    median_duration_minutes: f64,
    variant_count: usize,
    most_common_variant: Option<String>,
}

#[derive(Serialize)]
struct WorkflowDiscoveryOutput {
    workflow_name: String,
    total_edges: usize,
    pareto_edges: usize,
    graph_mermaid: String,
    top_paths: Vec<String>,
}

#[derive(Serialize, Deserialize, Clone)]
struct WorkflowEvent {
    case_id: String,
    activity: String,
    resource: Option<String>,
    timestamp: DateTime<Utc>,
}

#[derive(Serialize, Deserialize)]
struct WorkflowFile {
    name: String,
    r#type: String,
    events: Vec<WorkflowEvent>,
    created_at: DateTime<Utc>,
    metadata: BTreeMap<String, String>,
}

fn execution_error(msg: impl Into<String>) -> NounVerbError {
    clap_noun_verb::NounVerbError::execution_error(msg.into())
}

fn ensure_parent(path: &Path) -> Result<()> {
    if let Some(parent) = path.parent() {
        fs::create_dir_all(parent).map_err(|e| {
            execution_error(format!(
                "Failed to create directory {}: {e}",
                parent.display()
            ))
        })?;
    }
    Ok(())
}

fn persist_workflow(path: &Path, workflow: &WorkflowFile) -> Result<()> {
    ensure_parent(path)?;
    let file = File::create(path).map_err(|e| {
        execution_error(format!(
            "Failed to create workflow file {}: {e}",
            path.display()
        ))
    })?;
    let writer = BufWriter::new(file);
    serde_json::to_writer_pretty(writer, workflow).map_err(|e| {
        execution_error(format!(
            "Failed to serialize workflow {}: {e}",
            path.display()
        ))
    })
}

fn load_workflow(path: &Path) -> Result<WorkflowFile> {
    let data = fs::read_to_string(path).map_err(|e| {
        execution_error(format!(
            "Failed to read workflow file {}: {e}",
            path.display()
        ))
    })?;
    serde_json::from_str(&data).map_err(|e| {
        execution_error(format!(
            "Workflow file {} is invalid JSON: {e}",
            path.display()
        ))
    })
}

fn compute_durations_minutes(workflow: &WorkflowFile) -> Vec<f64> {
    // **FMEA Fix**: Use BTreeMap for deterministic iteration order
    let mut by_case: BTreeMap<&str, Vec<&WorkflowEvent>> = BTreeMap::new();
    for event in &workflow.events {
        by_case.entry(&event.case_id).or_default().push(event);
    }

    let mut durations = Vec::new();
    for events in by_case.values_mut() {
        events.sort_by_key(|e| e.timestamp);
        if let (Some(first), Some(last)) = (events.first(), events.last()) {
            let duration = last
                .timestamp
                .signed_duration_since(first.timestamp)
                .num_seconds() as f64
                / 60.0;
            durations.push(duration.max(0.0));
        }
    }

    durations
}

fn average(values: &[f64]) -> f64 {
    if values.is_empty() {
        return 0.0;
    }
    values.iter().copied().sum::<f64>() / values.len() as f64
}

fn median(mut values: Vec<f64>) -> f64 {
    if values.is_empty() {
        return 0.0;
    }
    // Sort with proper handling of NaN values (NaN sorts last)
    values.sort_by(|a, b| a.partial_cmp(b).unwrap_or(std::cmp::Ordering::Equal));
    let mid = values.len() / 2;
    if values.len() % 2 == 0 {
        (values[mid - 1] + values[mid]) / 2.0
    } else {
        values[mid]
    }
}

fn analysis_from_workflow(workflow: &WorkflowFile) -> WorkflowAnalysisOutput {
    let mut unique_cases = BTreeSet::new();
    let mut unique_activities = BTreeSet::new();
    // **FMEA Fix**: Use BTreeMap for deterministic iteration order
    let mut variants: BTreeMap<String, usize> = BTreeMap::new();
    let mut by_case: BTreeMap<&str, Vec<&WorkflowEvent>> = BTreeMap::new();

    for event in &workflow.events {
        unique_cases.insert(event.case_id.as_str());
        unique_activities.insert(event.activity.as_str());
        by_case.entry(&event.case_id).or_default().push(event);
    }

    for events in by_case.values_mut() {
        events.sort_by_key(|e| e.timestamp);
        let variant = events
            .iter()
            .map(|e| e.activity.as_str())
            .collect::<Vec<_>>()
            .join("→");
        *variants.entry(variant).or_default() += 1;
    }

    let durations = compute_durations_minutes(workflow);
    let variant_count = variants.len();
    let most_common_variant = variants
        .into_iter()
        .max_by_key(|(_, count)| *count)
        .map(|(variant, _)| variant);

    WorkflowAnalysisOutput {
        workflow_name: workflow.name.clone(),
        total_cases: unique_cases.len(),
        total_events: workflow.events.len(),
        unique_activities: unique_activities.len(),
        average_duration_minutes: average(&durations),
        median_duration_minutes: median(durations),
        variant_count,
        most_common_variant,
    }
}

fn build_edges(workflow: &WorkflowFile) -> BTreeMap<(String, String), usize> {
    // **FMEA Fix**: Use BTreeMap for deterministic iteration order
    let mut edges: BTreeMap<(String, String), usize> = BTreeMap::new();
    let mut by_case: BTreeMap<&str, Vec<&WorkflowEvent>> = BTreeMap::new();

    for event in &workflow.events {
        by_case.entry(&event.case_id).or_default().push(event);
    }

    for events in by_case.values_mut() {
        events.sort_by_key(|e| e.timestamp);
        for window in events.windows(2) {
            if let [from, to] = window {
                *edges
                    .entry((from.activity.clone(), to.activity.clone()))
                    .or_default() += 1;
            }
        }
    }

    edges
}

fn mermaid_from_edges(edges: &BTreeMap<(String, String), usize>) -> String {
    let mut nodes: BTreeSet<&str> = BTreeSet::new();
    let mut lines = String::from("graph TD\n");

    let mut sorted_edges: Vec<_> = edges.iter().collect();
    sorted_edges.sort_by(|a, b| b.1.cmp(a.1).then_with(|| a.0.cmp(b.0)));

    for ((from, to), count) in sorted_edges {
        nodes.insert(from);
        nodes.insert(to);
        lines.push_str(&format!("    \"{from}\" -->|{count}| \"{to}\"\n"));
    }

    for node in nodes {
        lines.push_str(&format!("    \"{node}\";\n"));
    }

    lines
}

fn top_paths_from_variants(workflow: &WorkflowFile) -> Vec<String> {
    // **FMEA Fix**: Use BTreeMap for deterministic iteration order
    let mut variants: BTreeMap<String, usize> = BTreeMap::new();
    let mut by_case: BTreeMap<&str, Vec<&WorkflowEvent>> = BTreeMap::new();

    for event in &workflow.events {
        by_case.entry(&event.case_id).or_default().push(event);
    }

    for events in by_case.values_mut() {
        events.sort_by_key(|e| e.timestamp);
        let variant = events
            .iter()
            .map(|e| e.activity.as_str())
            .collect::<Vec<_>>()
            .join("→");
        *variants.entry(variant).or_default() += 1;
    }

    let mut variant_list: Vec<(String, usize)> = variants.into_iter().collect();
    variant_list.sort_by(|a, b| b.1.cmp(&a.1).then_with(|| a.0.cmp(&b.0)));
    variant_list
        .into_iter()
        .take(3)
        .map(|(v, count)| format!("{v} ({count} cases)"))
        .collect()
}

fn pareto_edge_count(edges: &BTreeMap<(String, String), usize>) -> usize {
    let mut counts: Vec<usize> = edges.values().copied().collect();
    counts.sort_unstable_by(|a, b| b.cmp(a));
    let total: usize = counts.iter().sum();
    if total == 0 {
        return 0;
    }

    let threshold = (total as f64 * 0.8).ceil() as usize;
    let mut running = 0usize;
    let mut edge_count = 0usize;
    for count in counts {
        running += count;
        edge_count += 1;
        if running >= threshold {
            break;
        }
    }
    edge_count
}

fn discovery_from_workflow(workflow: &WorkflowFile) -> WorkflowDiscoveryOutput {
    let edges = build_edges(workflow);
    let graph_mermaid = mermaid_from_edges(&edges);
    let top_paths = top_paths_from_variants(workflow);
    let pareto_edges = pareto_edge_count(&edges);

    WorkflowDiscoveryOutput {
        workflow_name: workflow.name.clone(),
        total_edges: edges.len(),
        pareto_edges,
        graph_mermaid,
        top_paths,
    }
}

// ============================================================================
// Verb Functions
// ============================================================================

/// Initialize a new workflow for tracking
///
/// # Usage
///
/// ```bash
/// # Create university research workflow
/// ggen workflow init --name "university-research" --type research
///
/// # Create package maturity workflow
/// ggen workflow init --name "package-maturity" --type maturity
///
/// # Create RevOps workflow
/// ggen workflow init --name "revops-pipeline" --type revops
/// ```
#[verb("init", "workflow")]
fn init(
    name: String, workflow_type: Option<String>, output_dir: Option<PathBuf>,
) -> Result<WorkflowInitOutput> {
    let workflow_type = workflow_type.unwrap_or_else(|| "research".to_string());
    let output_dir = output_dir
        .unwrap_or_else(|| std::env::current_dir().unwrap_or_else(|_| PathBuf::from(".")));
    let workflow_path = output_dir.join(".workflows").join(format!("{}.json", name));
    let path_str = workflow_path.display().to_string();

    if workflow_path.exists() {
        return Err(execution_error(format!(
            "Workflow file already exists at {}",
            workflow_path.display()
        )));
    }

    let workflow = WorkflowFile {
        name: name.clone(),
        r#type: workflow_type,
        events: Vec::new(),
        created_at: Utc::now(),
        metadata: BTreeMap::new(),
    };

    persist_workflow(&workflow_path, &workflow)?;

    Ok(WorkflowInitOutput {
        workflow_name: name.clone(),
        path: path_str,
        status: "Workflow initialized - ready to track events".to_string(),
    })
}

/// Analyze workflow events and generate statistics
///
/// # Usage
///
/// ```bash
/// # Analyze workflow
/// ggen workflow analyze --workflow-file workflow.json
///
/// # Show summary
/// ggen workflow analyze --workflow-file workflow.json --summary
/// ```
#[verb("analyze", "workflow")]
fn analyze(workflow_file: String, summary: bool) -> Result<WorkflowAnalysisOutput> {
    let path = Path::new(&workflow_file);
    if summary {
        // summary currently returns same detailed output but keeps future room for slimmer view
    }

    let workflow = load_workflow(path)?;
    Ok(analysis_from_workflow(&workflow))
}

/// Discover process patterns and generate visualization
///
/// # Usage
///
/// ```bash
/// # Discover process patterns
/// ggen workflow discover --workflow-file workflow.json
///
/// # Export as Mermaid diagram
/// ggen workflow discover --workflow-file workflow.json --export mermaid
///
/// # Show 80/20 critical path
/// ggen workflow discover --workflow-file workflow.json --pareto
/// ```
#[verb("discover", "workflow")]
fn discover(
    workflow_file: String, export_format: Option<String>, pareto: bool,
) -> Result<WorkflowDiscoveryOutput> {
    let _export_format = export_format;
    let workflow = load_workflow(Path::new(&workflow_file))?;
    let discovery = discovery_from_workflow(&workflow);

    if pareto && discovery.pareto_edges == 0 && discovery.total_edges > 0 {
        return Err(execution_error(
            "Pareto analysis requested but no edges found in workflow",
        ));
    }

    Ok(discovery)
}

/// Track workflow event
///
/// # Usage
///
/// ```bash
/// # Record event
/// ggen workflow event --workflow-file workflow.json \
///   --case-id workflow-123 \
///   --activity "CodeGenerated" \
///   --resource "researcher-1"
/// ```
#[verb("event", "workflow")]
fn event(
    workflow_file: String, case_id: String, activity: String, resource: Option<String>,
) -> Result<serde_json::Value> {
    let path = Path::new(&workflow_file);
    let mut workflow = load_workflow(path)?;
    let event = WorkflowEvent {
        case_id,
        activity,
        resource,
        timestamp: Utc::now(),
    };
    workflow.events.push(event);
    persist_workflow(path, &workflow)?;

    Ok(serde_json::json!({
        "status": "Event recorded",
        "events": workflow.events.len(),
        "workflow": workflow.name,
        "timestamp": workflow.events.last().map(|e| e.timestamp.to_rfc3339()),
    }))
}

/// Generate workflow report
///
/// # Usage
///
/// ```bash
/// # Generate HTML report
/// ggen workflow report --workflow-file workflow.json --format html --output report.html
///
/// # Generate JSON report
/// ggen workflow report --workflow-file workflow.json --format json --output report.json
/// ```
#[verb("report", "workflow")]
fn report(
    workflow_file: String, format: Option<String>, output: Option<String>,
) -> Result<serde_json::Value> {
    let format = format.unwrap_or_else(|| "html".to_string());
    let output_path = output.unwrap_or_else(|| match format.as_str() {
        "json" => "workflow-report.json".to_string(),
        _ => "workflow-report.html".to_string(),
    });

    let path = Path::new(&workflow_file);
    let workflow = load_workflow(path)?;
    let analysis = analysis_from_workflow(&workflow);
    let discovery = discovery_from_workflow(&workflow);
    let report_path = PathBuf::from(&output_path);
    ensure_parent(&report_path)?;

    match format.as_str() {
        "json" => {
            let payload = serde_json::json!({
                "workflow": workflow.name,
                "analysis": analysis,
                "discovery": discovery,
            });
            let file = File::create(&report_path).map_err(|e| {
                execution_error(format!(
                    "Failed to create report file {}: {e}",
                    report_path.display()
                ))
            })?;
            let writer = BufWriter::new(file);
            serde_json::to_writer_pretty(writer, &payload).map_err(|e| {
                execution_error(format!(
                    "Failed to serialize report to {}: {e}",
                    report_path.display()
                ))
            })?;
        }
        "html" => {
            let html = format!(
                "<!DOCTYPE html><html><head><title>Workflow Report - {}</title></head>\
                <body><h1>Workflow Report - {}</h1>\
                <section><h2>Analysis</h2>\
                <ul>\
                <li>Total cases: {}</li>\
                <li>Total events: {}</li>\
                <li>Unique activities: {}</li>\
                <li>Average duration (minutes): {:.2}</li>\
                <li>Median duration (minutes): {:.2}</li>\
                <li>Variant count: {}</li>\
                <li>Most common variant: {}</li>\
                </ul>\
                </section>\
                <section><h2>Discovery</h2>\
                <p>Total edges: {}</p>\
                <p>Pareto edges: {}</p>\
                <pre>{}</pre>\
                <ul>{}</ul>\
                </section>\
                </body></html>",
                workflow.name,
                workflow.name,
                analysis.total_cases,
                analysis.total_events,
                analysis.unique_activities,
                analysis.average_duration_minutes,
                analysis.median_duration_minutes,
                analysis.variant_count,
                analysis
                    .most_common_variant
                    .clone()
                    .unwrap_or_else(|| "N/A".to_string()),
                discovery.total_edges,
                discovery.pareto_edges,
                discovery.graph_mermaid,
                discovery
                    .top_paths
                    .iter()
                    .map(|p| format!("<li>{p}</li>"))
                    .collect::<Vec<_>>()
                    .join(""),
            );

            fs::write(&report_path, html).map_err(|e| {
                execution_error(format!(
                    "Failed to write HTML report to {}: {e}",
                    report_path.display()
                ))
            })?;
        }
        other => {
            return Err(execution_error(format!(
                "Unsupported report format: {other}"
            )));
        }
    }

    let json_value = serde_json::json!({
        "status": "Report generated",
        "path": report_path.display().to_string(),
        "format": format
    });

    // Convert JSON value to Object - json! macro always produces valid JSON
    Ok(Value::Object(
        json_value
            .as_object()
            .expect("json! macro always produces valid object")
            .clone(),
    ))
}

#[cfg(test)]
mod tests {
    use super::*;

    fn sample_workflow() -> WorkflowFile {
        let ts = |days: i64| Utc::now() + chrono::Duration::days(days);
        WorkflowFile {
            name: "sample".to_string(),
            r#type: "research".to_string(),
            events: vec![
                WorkflowEvent {
                    case_id: "case-1".to_string(),
                    activity: "Submitted".to_string(),
                    resource: Some("alice".to_string()),
                    timestamp: ts(0),
                },
                WorkflowEvent {
                    case_id: "case-1".to_string(),
                    activity: "Reviewed".to_string(),
                    resource: Some("bob".to_string()),
                    timestamp: ts(1),
                },
                WorkflowEvent {
                    case_id: "case-2".to_string(),
                    activity: "Submitted".to_string(),
                    resource: Some("carol".to_string()),
                    timestamp: ts(0),
                },
                WorkflowEvent {
                    case_id: "case-2".to_string(),
                    activity: "Reviewed".to_string(),
                    resource: Some("dave".to_string()),
                    timestamp: ts(2),
                },
            ],
            created_at: Utc::now(),
            metadata: BTreeMap::new(),
        }
    }

    #[test]
    fn computes_analysis_metrics() {
        let workflow = sample_workflow();
        let analysis = analysis_from_workflow(&workflow);
        assert_eq!(analysis.workflow_name, "sample");
        assert_eq!(analysis.total_cases, 2);
        assert_eq!(analysis.total_events, 4);
        assert_eq!(analysis.unique_activities, 2);
        assert_eq!(analysis.variant_count, 1);
        assert!(analysis.average_duration_minutes > 0.0);
        assert!(analysis.median_duration_minutes > 0.0);
        assert!(analysis.most_common_variant.is_some());
    }

    #[test]
    fn counts_unique_variants_not_cases() {
        let ts = |days: i64| Utc::now() + chrono::Duration::days(days);
        let workflow = WorkflowFile {
            name: "variants".to_string(),
            r#type: "research".to_string(),
            events: vec![
                WorkflowEvent {
                    case_id: "case-1".to_string(),
                    activity: "Submitted".to_string(),
                    resource: None,
                    timestamp: ts(0),
                },
                WorkflowEvent {
                    case_id: "case-1".to_string(),
                    activity: "Reviewed".to_string(),
                    resource: None,
                    timestamp: ts(1),
                },
                WorkflowEvent {
                    case_id: "case-2".to_string(),
                    activity: "Submitted".to_string(),
                    resource: None,
                    timestamp: ts(0),
                },
                WorkflowEvent {
                    case_id: "case-2".to_string(),
                    activity: "Approved".to_string(),
                    resource: None,
                    timestamp: ts(1),
                },
                WorkflowEvent {
                    case_id: "case-3".to_string(),
                    activity: "Submitted".to_string(),
                    resource: None,
                    timestamp: ts(0),
                },
                WorkflowEvent {
                    case_id: "case-3".to_string(),
                    activity: "Reviewed".to_string(),
                    resource: None,
                    timestamp: ts(1),
                },
            ],
            created_at: Utc::now(),
            metadata: BTreeMap::new(),
        };

        let analysis = analysis_from_workflow(&workflow);
        assert_eq!(analysis.variant_count, 2);
        assert_eq!(analysis.total_cases, 3);
    }

    #[test]
    fn builds_discovery_graph() {
        let workflow = sample_workflow();
        let discovery = discovery_from_workflow(&workflow);
        assert_eq!(discovery.total_edges, 1);
        assert_eq!(discovery.pareto_edges, 1);
        assert!(discovery.graph_mermaid.contains("Submitted"));
        assert!(!discovery.top_paths.is_empty());
    }
}
