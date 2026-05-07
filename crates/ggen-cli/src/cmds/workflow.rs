//! Workflow Analytics Commands - Process Mining Integration
//!
//! Commands for tracking and analyzing marketplace and university research workflows
//! using process mining techniques.

use clap_noun_verb::Result as VerbResult;
use clap_noun_verb_macros::verb;
use serde::{Deserialize, Serialize};
use std::collections::{HashMap, HashSet};
use std::path::{Path, PathBuf};

// ============================================================================
// 80/20 Process Mining Types (Replacing deleted pictl_types)
// ============================================================================

#[derive(Serialize, Deserialize, Clone, Debug)]
#[serde(untagged)]
pub enum AttributeValue {
    String(String),
    Date(String),
    Int(i64),
    Float(f64),
    Boolean(bool),
}

#[derive(Serialize, Deserialize, Clone, Debug)]
pub struct Event {
    pub attributes: HashMap<String, AttributeValue>,
}

impl Event {
    pub fn new(attributes: HashMap<String, AttributeValue>) -> Self {
        Self { attributes }
    }
}

#[derive(Serialize, Deserialize, Clone, Debug)]
pub struct Trace {
    pub case_id: String,
    pub events: Vec<Event>,
}

impl Trace {
    pub fn new(case_id: String, events: Vec<Event>) -> Self {
        Self { case_id, events }
    }
}

#[derive(Serialize, Deserialize, Clone, Debug, Default)]
pub struct EventLog {
    pub traces: Vec<Trace>,
    pub extensions: HashMap<String, String>,
}

impl EventLog {
    pub fn new(traces: Vec<Trace>, extensions: HashMap<String, String>) -> Self {
        Self { traces, extensions }
    }
    pub fn len(&self) -> usize {
        self.traces.len()
    }
    pub fn event_count(&self) -> usize {
        self.traces.iter().map(|t| t.events.len()).sum()
    }
    pub fn get_activities(&self, key: &str) -> HashSet<String> {
        let mut acts = HashSet::new();
        for t in &self.traces {
            for e in &t.events {
                if let Some(AttributeValue::String(s)) = e.attributes.get(key) {
                    acts.insert(s.clone());
                }
            }
        }
        acts
    }
}

#[derive(Debug, Default)]
pub struct PetriNet {
    pub places: Vec<Place>,
    pub transitions: Vec<Transition>,
}

#[derive(Debug)]
pub struct Place {
    pub id: String,
    pub label: String,
}

#[derive(Debug)]
pub struct Transition {
    pub id: String,
    pub label: String,
}

// 80/20 Discovery: Extracts unique activities as transitions and creates places between them
// simulating a simple Directly-Follows Graph (DFG) represented as a PetriNet
fn discover_dfg(log: &EventLog, key: &str) -> PetriNet {
    let mut net = PetriNet::default();
    let activities = log.get_activities(key);
    
    // Add transitions for each activity
    for (i, act) in activities.iter().enumerate() {
        net.transitions.push(Transition {
            id: format!("t{}", i),
            label: act.clone(),
        });
    }
    
    // Add simple places connecting them (just a linear flow simulation for 80/20)
    for i in 0..activities.len() {
        net.places.push(Place {
            id: format!("p{}", i),
            label: format!("State {}", i),
        });
    }
    net
}


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
}

#[derive(Serialize)]
struct WorkflowDiscoveryOutput {
    workflow_name: String,
    total_places: usize,
    total_transitions: usize,
    graph_mermaid: String,
}

// ============================================================================
// Verb Functions
// ============================================================================

/// Initialize a new workflow for tracking
#[verb]
fn init(
    name: String, workflow_type: Option<String>, output_dir: Option<PathBuf>,
) -> VerbResult<WorkflowInitOutput> {
    let _workflow_type = workflow_type.unwrap_or_else(|| "research".to_string());
    let output_dir = output_dir.unwrap_or_else(|| PathBuf::from("."));
    let workflow_path = output_dir.join(format!("{}.json", name));

    let log = EventLog::new(Vec::new(), std::collections::HashMap::new());
    let log_json = serde_json::to_string_pretty(&log).unwrap();
    std::fs::write(&workflow_path, log_json).map_err(|e| {
        clap_noun_verb::NounVerbError::execution_error(format!(
            "Failed to create workflow file: {}",
            e
        ))
    })?;

    Ok(WorkflowInitOutput {
        workflow_name: name,
        path: workflow_path.to_string_lossy().to_string(),
        status: "Workflow initialized - ready to track events".to_string(),
    })
}

/// Analyze workflow events and generate statistics
#[verb]
fn analyze(workflow_file: String) -> VerbResult<WorkflowAnalysisOutput> {
    let log = load_log(&workflow_file)?;

    Ok(WorkflowAnalysisOutput {
        workflow_name: workflow_file,
        total_cases: log.len(),
        total_events: log.event_count(),
        unique_activities: log.get_activities("concept:name").len(),
        average_duration_minutes: 0.0, // 80/20 simplification
    })
}

/// Discover process patterns and generate visualization
#[verb]
fn discover(workflow_file: String) -> VerbResult<WorkflowDiscoveryOutput> {
    let log = load_log(&workflow_file)?;
    let petri_net = discover_dfg(&log, "concept:name");

    Ok(WorkflowDiscoveryOutput {
        workflow_name: workflow_file,
        total_places: petri_net.places.len(),
        total_transitions: petri_net.transitions.len(),
        graph_mermaid: generate_mermaid(&petri_net),
    })
}

/// Synthesize a Semantic OS Law from a workflow log
#[verb]
fn synthesize(
    workflow_file: String, law_id: String, name: String, output: Option<String>,
) -> VerbResult<serde_json::Value> {
    let log = load_log(&workflow_file)?;
    let petri_net = discover_dfg(&log, "concept:name");

    let ttl = petri_net_to_ttl(&petri_net, &law_id, &name);
    let output_path = PathBuf::from(output.unwrap_or_else(|| format!("{}.ttl", law_id)));

    std::fs::write(&output_path, ttl).map_err(|e| {
        clap_noun_verb::NounVerbError::execution_error(format!("Failed to write law file: {}", e))
    })?;

    Ok(serde_json::json!({
        "status": "success",
        "law_id": law_id,
        "output_path": output_path.to_string_lossy().to_string(),
        "transitions_discovered": petri_net.transitions.len(),
    }))
}

/// Track workflow event
#[verb]
fn event(
    workflow_file: String, case_id: String, activity: String, resource: Option<String>,
) -> VerbResult<serde_json::Value> {
    let mut log = load_log(&workflow_file)?;
    let timestamp = chrono::Utc::now().to_rfc3339();

    append_event(&mut log, &case_id, &activity, &timestamp, resource);

    let log_json = serde_json::to_string_pretty(&log).unwrap();
    std::fs::write(&workflow_file, log_json).map_err(|e| {
        clap_noun_verb::NounVerbError::execution_error(format!(
            "Failed to write workflow file: {}",
            e
        ))
    })?;

    Ok(serde_json::json!({
        "status": "Event recorded",
        "timestamp": timestamp,
        "case_id": case_id,
        "activity": activity,
    }))
}

/// Generate workflow report
#[verb]
fn report(
    workflow_file: String, format: Option<String>, output: Option<String>,
) -> VerbResult<serde_json::Value> {
    let log = load_log(&workflow_file)?;
    let _format = format.unwrap_or_else(|| "html".to_string());
    let _output = output.unwrap_or_else(|| "workflow-report.html".to_string());

    let report_content = format!(
        "<html><body><h1>Workflow Report</h1><p>Cases: {}</p><p>Events: {}</p></body></html>",
        log.len(),
        log.event_count()
    );

    std::fs::write(&_output, report_content).map_err(|e| {
        clap_noun_verb::NounVerbError::execution_error(format!(
            "Failed to write report file: {}",
            e
        ))
    })?;

    Ok(serde_json::json!({
        "status": "Report generated",
        "path": _output,
        "format": _format,
        "total_cases": log.len(),
    }))
}

// ============================================================================
// Helper Functions
// ============================================================================

fn load_log(path: &str) -> VerbResult<EventLog> {
    if !Path::new(path).exists() {
        return Ok(EventLog::new(Vec::new(), std::collections::HashMap::new()));
    }
    let content = std::fs::read_to_string(path).map_err(|e| {
        clap_noun_verb::NounVerbError::execution_error(format!(
            "Failed to read workflow file: {}",
            e
        ))
    })?;
    serde_json::from_str(&content).map_err(|e| {
        clap_noun_verb::NounVerbError::execution_error(format!("Failed to parse EventLog: {}", e))
    })
}

fn generate_mermaid(net: &PetriNet) -> String {
    let mut mermaid = "graph TD\n".to_string();
    for p in &net.places {
        let label = if p.label.is_empty() { &p.id } else { &p.label };
        mermaid.push_str(&format!("  P{}[\"{}\"]\n", p.id, label));
    }
    for t in &net.transitions {
        let label = if t.label.is_empty() { &t.id } else { &t.label };
        mermaid.push_str(&format!("  T{}[\"{}\"]\n", t.id, label));
    }
    mermaid
}

fn petri_net_to_ttl(net: &PetriNet, law_id: &str, name: &str) -> String {
    let mut ttl = format!(
        r#"@prefix sos: <http://seanchatmangpt.com/sos#> .
@prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .

<{law_id}> a sos:Law ;
    rdfs:label "{name}" ;
    sos:implementation "petri-net" .
"#
    );

    for p in &net.places {
        ttl.push_str(&format!(
            "<{law_id}/place/{id}> a sos:Place ; rdfs:label \"{label}\" .\n",
            id = p.id,
            label = p.label
        ));
    }
    for t in &net.transitions {
        ttl.push_str(&format!(
            "<{law_id}/transition/{id}> a sos:Transition ; rdfs:label \"{label}\" .\n",
            id = t.id,
            label = t.label
        ));
    }
    ttl
}

fn append_event(
    log: &mut EventLog, case_id: &str, activity: &str, timestamp: &str, resource: Option<String>,
) {
    let mut attrs = std::collections::HashMap::new();
    attrs.insert(
        "concept:name".to_string(),
        AttributeValue::String(activity.to_string()),
    );
    attrs.insert(
        "time:timestamp".to_string(),
        AttributeValue::Date(timestamp.to_string()),
    );
    if let Some(r) = resource {
        attrs.insert("org:resource".to_string(), AttributeValue::String(r));
    }
    let new_event = Event::new(attrs);

    if let Some(trace) = log.traces.iter_mut().find(|t| t.case_id == case_id) {
        trace.events.push(new_event);
    } else {
        log.traces
            .push(Trace::new(case_id.to_string(), vec![new_event]));
    }
}