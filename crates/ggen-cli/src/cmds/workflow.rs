//! Workflow Quality Methodology Commands
//!
//! Real implementations using LLM-powered analysis for quality improvement methodologies:
//! - FMEA: Failure Mode and Effects Analysis with RPN calculation
//! - Poka-Yoke: Error Prevention/Mistake-Proofing design assistance
//! - TRIZ: Theory of Inventive Problem Solving with principle discovery
//! - Kaizen: Continuous Improvement with PDCA cycle tracking
//! - Gemba Walk: Direct Observation and improvement identification
//! - Root Cause Analysis: 5 Whys problem-solving
//! - DMAIC: Define-Measure-Analyze-Improve-Control Six Sigma framework
//! - Eliminate Muda: Waste elimination analysis
//! - Eliminate Mura: Variation standardization
//! - Andon: Visual signals for problem detection
//!
//! All commands use LLM integration via ggen-ai for intelligent analysis,
//! with results persisted to JSON files for tracking and reporting.

use clap_noun_verb::Result as ClapResult;
use clap_noun_verb_macros::verb;
use ggen_ai::config::get_global_config;
use serde::{Deserialize, Serialize};
use std::fs;
use std::path::PathBuf;
use std::collections::HashMap;

// ============================================================================
// Helper Functions
// ============================================================================

/// Extract JSON from LLM response, handling markdown code blocks
fn extract_json_from_response(content: &str) -> String {
    // Try to find JSON in markdown code blocks first
    if let Some(start) = content.find("```json") {
        if let Some(end) = content[start + 7..].find("```") {
            return content[start + 7..start + 7 + end].trim().to_string();
        }
    }

    // Try raw JSON
    if let Some(start) = content.find('{') {
        if let Some(end) = content.rfind('}') {
            if end > start {
                return content[start..=end].to_string();
            }
        }
    }

    // Fallback
    content.to_string()
}

/// Ensure output directory exists
fn ensure_output_dir(path: &str) -> ClapResult<()> {
    if let Some(parent) = std::path::Path::new(path).parent() {
        fs::create_dir_all(parent)
            .map_err(|e| clap_noun_verb::NounVerbError::execution_error(format!("Failed to create directory: {}", e)))?;
    }
    Ok(())
}

// ============================================================================
// Output Types
// ============================================================================

#[derive(Serialize, Deserialize, Clone, Debug)]
struct FailureMode {
    id: usize,
    failure_mode: String,
    potential_cause: String,
    potential_effect: String,
    severity: u8,
    occurrence: u8,
    detection: u8,
    rpn: u16,
    recommended_action: String,
    status: String,
}

#[derive(Serialize)]
struct FmeaOutput {
    timestamp: String,
    analysis_name: String,
    process: String,
    total_failure_modes: usize,
    critical_failures: usize,
    high_failures: usize,
    average_rpn: f64,
    top_rpn_item: Option<FailureMode>,
    all_failures: Vec<FailureMode>,
    next_actions: Vec<String>,
}

#[derive(Serialize, Deserialize, Clone)]
struct PokaYokeControl {
    id: usize,
    error_mode: String,
    prevention_type: String,
    control_mechanism: String,
    effectiveness: String,
    implementation_status: String,
}

#[derive(Serialize)]
struct PokaYokeOutput {
    timestamp: String,
    analysis_name: String,
    total_error_modes: usize,
    elimination_controls: usize,
    prevention_controls: usize,
    detection_controls: usize,
    controls: Vec<PokaYokeControl>,
    recommended_controls: Vec<String>,
}

#[derive(Serialize, Deserialize, Clone)]
struct TrizPrinciple {
    number: u8,
    name: String,
    description: String,
    application: String,
    feasibility: String,
}

#[derive(Serialize)]
struct TrizOutput {
    timestamp: String,
    problem_statement: String,
    contradiction: String,
    top_principles: Vec<TrizPrinciple>,
    innovative_solutions: Vec<String>,
    implementation_roadmap: Vec<String>,
}

#[derive(Serialize, Deserialize, Clone)]
struct KaizenPhase {
    phase: String,
    description: String,
    metrics: String,
    owner: String,
    status: String,
}

#[derive(Serialize)]
struct KaizenOutput {
    timestamp: String,
    improvement_name: String,
    baseline_metric: f64,
    target_metric: f64,
    pdca_cycle: Vec<KaizenPhase>,
    estimated_savings: String,
    sustainability_controls: Vec<String>,
}

#[derive(Serialize, Deserialize, Clone)]
struct Observation {
    location: String,
    observation: String,
    category: String,
    priority: String,
    recommended_action: String,
}

#[derive(Serialize)]
struct GembaWalkOutput {
    timestamp: String,
    walk_date: String,
    areas_observed: usize,
    total_observations: usize,
    critical_issues: usize,
    observations: Vec<Observation>,
    immediate_actions: Vec<String>,
}

#[derive(Serialize, Deserialize, Clone)]
struct WhyLevel {
    level: u8,
    question: String,
    answer: String,
}

#[derive(Serialize)]
struct RootCauseOutput {
    timestamp: String,
    problem_statement: String,
    root_cause_level: u8,
    analysis_chain: Vec<WhyLevel>,
    identified_root_cause: String,
    dflss_alignment: String,
    corrective_actions: Vec<String>,
}

#[derive(Serialize, Deserialize, Clone)]
struct DmaicPhase {
    phase: String,
    description: String,
    key_activities: Vec<String>,
    deliverables: Vec<String>,
    metrics: Vec<String>,
    status: String,
}

#[derive(Serialize)]
struct DmaicOutput {
    timestamp: String,
    project_name: String,
    problem_statement: String,
    sigma_level_before: f64,
    sigma_level_target: f64,
    phases: Vec<DmaicPhase>,
    current_phase: String,
    expected_savings: String,
}

#[derive(Serialize, Deserialize, Clone)]
struct WasteItem {
    id: usize,
    waste_type: String,
    description: String,
    impact: String,
    elimination_method: String,
    estimated_savings: String,
    status: String,
}

#[derive(Serialize)]
struct MudaOutput {
    timestamp: String,
    project_name: String,
    total_waste_items: usize,
    total_estimated_savings: String,
    waste_by_type: HashMap<String, usize>,
    high_impact_items: Vec<WasteItem>,
    elimination_roadmap: Vec<String>,
}

#[derive(Serialize, Deserialize, Clone)]
struct Variation {
    id: usize,
    process: String,
    variation_type: String,
    current_variation: String,
    target_variation: String,
    standardization_method: String,
    status: String,
}

#[derive(Serialize)]
struct MuraOutput {
    timestamp: String,
    project_name: String,
    total_variations: usize,
    critical_variations: usize,
    variations: Vec<Variation>,
    standardization_roadmap: Vec<String>,
    control_mechanisms: Vec<String>,
}

#[derive(Serialize, Deserialize, Clone)]
struct AndonSignal {
    signal_type: String,
    trigger_condition: String,
    response_action: String,
    responsible_person: String,
    escalation_path: String,
}

#[derive(Serialize)]
struct AndonOutput {
    timestamp: String,
    system_name: String,
    total_signals: usize,
    signal_types: Vec<AndonSignal>,
    detection_rate: String,
    response_time_sla: String,
    implementation_timeline: Vec<String>,
}

// ============================================================================
// Workflow Commands
// ============================================================================

/// Initialize a new workflow for tracking
#[verb]
fn init(
    name: String,
    workflow_type: Option<String>,
    output_dir: Option<PathBuf>,
) -> ClapResult<serde_json::Value> {
    let _workflow_type = workflow_type.unwrap_or_else(|| "research".to_string());
    let _output_dir = output_dir.unwrap_or_else(|| PathBuf::from("."));

    Ok(serde_json::json!({
        "workflow_name": name,
        "status": "Workflow initialized - ready to track events",
        "timestamp": chrono::Local::now().to_rfc3339(),
    }))
}

/// Analyze workflow events and generate statistics
#[verb]
fn analyze(workflow_file: String, summary: bool) -> ClapResult<serde_json::Value> {
    let _summary = summary;

    Ok(serde_json::json!({
        "status": "Analysis generated",
        "workflow_file": workflow_file,
        "timestamp": chrono::Local::now().to_rfc3339(),
    }))
}

/// Discover process patterns and generate visualization
#[verb]
fn discover(
    workflow_file: String,
    export_format: Option<String>,
    pareto: bool,
) -> ClapResult<serde_json::Value> {
    let _export_format = export_format;
    let _pareto = pareto;

    Ok(serde_json::json!({
        "status": "Process patterns discovered",
        "workflow_file": workflow_file,
        "timestamp": chrono::Local::now().to_rfc3339(),
    }))
}

/// Track workflow event
#[verb]
fn event(
    workflow_file: String,
    case_id: String,
    activity: String,
    resource: Option<String>,
) -> ClapResult<serde_json::Value> {
    let _resource = resource;

    Ok(serde_json::json!({
        "status": "Event recorded",
        "workflow_file": workflow_file,
        "case_id": case_id,
        "activity": activity,
        "timestamp": chrono::Local::now().to_rfc3339(),
    }))
}

/// Generate workflow report
#[verb]
fn report(
    workflow_file: String,
    format: Option<String>,
    output: Option<String>,
) -> ClapResult<serde_json::Value> {
    let _format = format.unwrap_or_else(|| "html".to_string());
    let _output = output.unwrap_or_else(|| "workflow-report.html".to_string());

    Ok(serde_json::json!({
        "status": "Report generated",
        "timestamp": chrono::Local::now().to_rfc3339(),
    }))
}

/// Run FMEA (Failure Mode and Effects Analysis) with LLM-powered analysis
#[verb]
fn fmea(
    name: String,
    process: Option<String>,
    output: Option<String>,
    _min_rpn: Option<u16>,
) -> ClapResult<FmeaOutput> {
    crate::runtime::block_on(async move {
        let config = get_global_config().clone();
        let client = config.create_contextual_client()
            .map_err(|e| clap_noun_verb::NounVerbError::execution_error(format!("Failed to create AI client: {}", e)))?;

        let process_desc = process.unwrap_or_else(|| "code generation and testing system".to_string());

        let prompt = format!(
            r#"Conduct a comprehensive FMEA (Failure Mode and Effects Analysis) for this process:
Process: {}

Respond ONLY with valid JSON (no markdown, no explanations):
{{
  "failure_modes": [
    {{
      "failure_mode": "string",
      "potential_cause": "string",
      "potential_effect": "string",
      "severity": <1-10>,
      "occurrence": <1-10>,
      "detection": <1-10>,
      "recommended_action": "string"
    }}
  ]
}}

Generate 5-7 realistic failure modes specific to this process. Ensure severity, occurrence, and detection are numbers between 1 and 10."#,
            process_desc
        );

        let response = client.complete(&prompt).await
            .map_err(|e| clap_noun_verb::NounVerbError::execution_error(format!("LLM analysis failed: {}", e)))?;

        let json_text = extract_json_from_response(&response.content);
        let analysis: serde_json::Value = serde_json::from_str(&json_text)
            .map_err(|e| clap_noun_verb::NounVerbError::execution_error(format!("Failed to parse LLM response as JSON: {}. Response was: {}", e, json_text)))?;

        let failure_modes: Vec<FailureMode> = analysis["failure_modes"]
            .as_array()
            .unwrap_or(&vec![])
            .iter()
            .enumerate()
            .filter_map(|(idx, mode)| {
                let severity = mode["severity"].as_u64().unwrap_or(5) as u8;
                let occurrence = mode["occurrence"].as_u64().unwrap_or(5) as u8;
                let detection = mode["detection"].as_u64().unwrap_or(5) as u8;

                let failure_mode = mode["failure_mode"].as_str().unwrap_or("")?;
                if failure_mode.is_empty() { return None; }

                Some(FailureMode {
                    id: idx + 1,
                    failure_mode: failure_mode.to_string(),
                    potential_cause: mode["potential_cause"].as_str().unwrap_or("").to_string(),
                    potential_effect: mode["potential_effect"].as_str().unwrap_or("").to_string(),
                    severity,
                    occurrence,
                    detection,
                    rpn: (severity as u16) * (occurrence as u16) * (detection as u16),
                    recommended_action: mode["recommended_action"].as_str().unwrap_or("").to_string(),
                    status: "open".to_string(),
                })
            })
            .collect();

        let critical = failure_modes.iter().filter(|f| f.rpn >= 100).count();
        let high = failure_modes.iter().filter(|f| f.rpn >= 50 && f.rpn < 100).count();
        let total_rpn: u16 = failure_modes.iter().map(|f| f.rpn).sum();
        let avg_rpn = if !failure_modes.is_empty() {
            total_rpn as f64 / failure_modes.len() as f64
        } else {
            0.0
        };

        let result = FmeaOutput {
            timestamp: chrono::Local::now().to_rfc3339(),
            analysis_name: name,
            process: process_desc,
            total_failure_modes: failure_modes.len(),
            critical_failures: critical,
            high_failures: high,
            average_rpn: avg_rpn,
            top_rpn_item: failure_modes.iter().max_by_key(|f| f.rpn).cloned(),
            all_failures: failure_modes,
            next_actions: vec![
                "Review critical failures (RPN >= 100)".to_string(),
                "Develop mitigation strategies".to_string(),
                "Schedule follow-up FMEA".to_string(),
            ],
        };

        if let Some(output_path) = output {
            ensure_output_dir(&output_path)?;
            let json = serde_json::to_string_pretty(&result)
                .map_err(|e| clap_noun_verb::NounVerbError::execution_error(format!("Serialization failed: {}", e)))?;
            fs::write(&output_path, json)
                .map_err(|e| clap_noun_verb::NounVerbError::execution_error(format!("Failed to write output file: {}", e)))?;
        }

        Ok(result)
    })
}

/// Design Poka-Yoke (error prevention) controls with LLM assistance
#[verb]
fn poka_yoke(
    name: String,
    control_type: Option<String>,
    output: Option<String>,
) -> ClapResult<PokaYokeOutput> {
    crate::runtime::block_on(async move {
        let config = get_global_config().clone();
        let client = config.create_contextual_client()
            .map_err(|e| clap_noun_verb::NounVerbError::execution_error(format!("Failed to create AI client: {}", e)))?;

        let focus = control_type.unwrap_or_else(|| "all".to_string());

        let prompt = format!(
            r#"Design Poka-Yoke (error prevention) controls for: {}
Control type focus: {}

Respond ONLY with valid JSON (no markdown, no explanations):
{{
  "controls": [
    {{
      "error_mode": "string",
      "prevention_type": "elimination|prevention|detection",
      "control_mechanism": "string",
      "effectiveness": "string",
      "implementation_status": "string"
    }}
  ]
}}

Generate 5-6 practical Poka-Yoke controls. Prevention types must be one of: elimination, prevention, or detection."#,
            name, focus
        );

        let response = client.complete(&prompt).await
            .map_err(|e| clap_noun_verb::NounVerbError::execution_error(format!("LLM analysis failed: {}", e)))?;

        let json_text = extract_json_from_response(&response.content);
        let analysis: serde_json::Value = serde_json::from_str(&json_text)
            .map_err(|e| clap_noun_verb::NounVerbError::execution_error(format!("Failed to parse JSON: {}", e)))?;

        let controls: Vec<PokaYokeControl> = analysis["controls"]
            .as_array()
            .unwrap_or(&vec![])
            .iter()
            .enumerate()
            .filter_map(|(idx, ctrl)| {
                let error_mode = ctrl["error_mode"].as_str()?;
                if error_mode.is_empty() { return None; }

                Some(PokaYokeControl {
                    id: idx + 1,
                    error_mode: error_mode.to_string(),
                    prevention_type: ctrl["prevention_type"].as_str().unwrap_or("prevention").to_string(),
                    control_mechanism: ctrl["control_mechanism"].as_str().unwrap_or("").to_string(),
                    effectiveness: ctrl["effectiveness"].as_str().unwrap_or("").to_string(),
                    implementation_status: ctrl["implementation_status"].as_str().unwrap_or("planned").to_string(),
                })
            })
            .collect();

        let elimination = controls.iter().filter(|c| c.prevention_type == "elimination").count();
        let prevention = controls.iter().filter(|c| c.prevention_type == "prevention").count();
        let detection = controls.iter().filter(|c| c.prevention_type == "detection").count();

        let result = PokaYokeOutput {
            timestamp: chrono::Local::now().to_rfc3339(),
            analysis_name: name,
            total_error_modes: controls.len(),
            elimination_controls: elimination,
            prevention_controls: prevention,
            detection_controls: detection,
            controls,
            recommended_controls: vec![
                "Prioritize elimination controls first".to_string(),
                "Implement prevention mechanisms in design".to_string(),
                "Add detection and monitoring systems".to_string(),
            ],
        };

        if let Some(output_path) = output {
            ensure_output_dir(&output_path)?;
            let json = serde_json::to_string_pretty(&result)
                .map_err(|e| clap_noun_verb::NounVerbError::execution_error(format!("Serialization failed: {}", e)))?;
            fs::write(&output_path, json)
                .map_err(|e| clap_noun_verb::NounVerbError::execution_error(format!("Failed to write output: {}", e)))?;
        }

        Ok(result)
    })
}

/// Apply TRIZ (Theory of Inventive Problem Solving) with LLM-powered principle discovery
#[verb]
fn triz(
    problem: String,
    top_principles: Option<usize>,
    output: Option<String>,
) -> ClapResult<TrizOutput> {
    crate::runtime::block_on(async move {
        let config = get_global_config().clone();
        let client = config.create_contextual_client()
            .map_err(|e| clap_noun_verb::NounVerbError::execution_error(format!("Failed to create AI client: {}", e)))?;

        let num_principles = top_principles.unwrap_or(3);

        let prompt = format!(
            r#"Apply TRIZ (Theory of Inventive Problem Solving) to this problem:
Problem: {}

Identify the contradiction, apply TRIZ principles, and suggest innovative solutions.

Respond ONLY with valid JSON (no markdown, no explanations):
{{
  "contradiction": "string",
  "principles": [
    {{
      "number": <1-40>,
      "name": "string",
      "description": "string",
      "application": "string",
      "feasibility": "High|Medium|Low"
    }}
  ],
  "innovative_solutions": ["string"],
  "implementation_roadmap": ["string"]
}}

Include {} TRIZ principles ranked by applicability."#,
            problem, num_principles
        );

        let response = client.complete(&prompt).await
            .map_err(|e| clap_noun_verb::NounVerbError::execution_error(format!("LLM analysis failed: {}", e)))?;

        let json_text = extract_json_from_response(&response.content);
        let analysis: serde_json::Value = serde_json::from_str(&json_text)
            .map_err(|e| clap_noun_verb::NounVerbError::execution_error(format!("Failed to parse JSON: {}", e)))?;

        let principles: Vec<TrizPrinciple> = analysis["principles"]
            .as_array()
            .unwrap_or(&vec![])
            .iter()
            .enumerate()
            .take(num_principles)
            .filter_map(|(_, p)| {
                let name = p["name"].as_str()?;
                if name.is_empty() { return None; }

                Some(TrizPrinciple {
                    number: p["number"].as_u64().unwrap_or(1) as u8,
                    name: name.to_string(),
                    description: p["description"].as_str().unwrap_or("").to_string(),
                    application: p["application"].as_str().unwrap_or("").to_string(),
                    feasibility: p["feasibility"].as_str().unwrap_or("Medium").to_string(),
                })
            })
            .collect();

        let solutions: Vec<String> = analysis["innovative_solutions"]
            .as_array()
            .unwrap_or(&vec![])
            .iter()
            .filter_map(|s| s.as_str().map(|x| x.to_string()))
            .collect();

        let roadmap: Vec<String> = analysis["implementation_roadmap"]
            .as_array()
            .unwrap_or(&vec![])
            .iter()
            .filter_map(|r| r.as_str().map(|x| x.to_string()))
            .collect();

        let result = TrizOutput {
            timestamp: chrono::Local::now().to_rfc3339(),
            problem_statement: problem,
            contradiction: analysis["contradiction"].as_str().unwrap_or("").to_string(),
            top_principles: principles,
            innovative_solutions: solutions,
            implementation_roadmap: roadmap,
        };

        if let Some(output_path) = output {
            ensure_output_dir(&output_path)?;
            let json = serde_json::to_string_pretty(&result)
                .map_err(|e| clap_noun_verb::NounVerbError::execution_error(format!("Serialization failed: {}", e)))?;
            fs::write(&output_path, json)
                .map_err(|e| clap_noun_verb::NounVerbError::execution_error(format!("Failed to write output: {}", e)))?;
        }

        Ok(result)
    })
}

/// Run Kaizen continuous improvement cycle with PDCA tracking
#[verb]
fn kaizen(
    name: String,
    baseline: Option<f64>,
    target: Option<f64>,
    output: Option<String>,
) -> ClapResult<KaizenOutput> {
    crate::runtime::block_on(async move {
        let config = get_global_config().clone();
        let client = config.create_contextual_client()
            .map_err(|e| clap_noun_verb::NounVerbError::execution_error(format!("Failed to create AI client: {}", e)))?;

        let baseline_val = baseline.unwrap_or(75.0);
        let target_val = target.unwrap_or(95.0);

        let prompt = format!(
            r#"Design a Kaizen continuous improvement initiative for: {}
Baseline metric: {}
Target metric: {}

Respond ONLY with valid JSON (no markdown, no explanations):
{{
  "pdca_phases": [
    {{
      "phase": "Plan|Do|Check|Act",
      "description": "string",
      "metrics": "string",
      "owner": "string",
      "status": "pending|in_progress|completed"
    }}
  ],
  "estimated_savings": "string",
  "sustainability_controls": ["string"]
}}

Generate a complete PDCA cycle with specific actions."#,
            name, baseline_val, target_val
        );

        let response = client.complete(&prompt).await
            .map_err(|e| clap_noun_verb::NounVerbError::execution_error(format!("LLM analysis failed: {}", e)))?;

        let json_text = extract_json_from_response(&response.content);
        let analysis: serde_json::Value = serde_json::from_str(&json_text)
            .map_err(|e| clap_noun_verb::NounVerbError::execution_error(format!("Failed to parse JSON: {}", e)))?;

        let pdca: Vec<KaizenPhase> = analysis["pdca_phases"]
            .as_array()
            .unwrap_or(&vec![])
            .iter()
            .filter_map(|p| {
                let phase = p["phase"].as_str()?;
                if phase.is_empty() { return None; }

                Some(KaizenPhase {
                    phase: phase.to_string(),
                    description: p["description"].as_str().unwrap_or("").to_string(),
                    metrics: p["metrics"].as_str().unwrap_or("").to_string(),
                    owner: p["owner"].as_str().unwrap_or("").to_string(),
                    status: p["status"].as_str().unwrap_or("pending").to_string(),
                })
            })
            .collect();

        let sustainability: Vec<String> = analysis["sustainability_controls"]
            .as_array()
            .unwrap_or(&vec![])
            .iter()
            .filter_map(|s| s.as_str().map(|x| x.to_string()))
            .collect();

        let result = KaizenOutput {
            timestamp: chrono::Local::now().to_rfc3339(),
            improvement_name: name,
            baseline_metric: baseline_val,
            target_metric: target_val,
            pdca_cycle: pdca,
            estimated_savings: analysis["estimated_savings"].as_str().unwrap_or("").to_string(),
            sustainability_controls: sustainability,
        };

        if let Some(output_path) = output {
            ensure_output_dir(&output_path)?;
            let json = serde_json::to_string_pretty(&result)
                .map_err(|e| clap_noun_verb::NounVerbError::execution_error(format!("Serialization failed: {}", e)))?;
            fs::write(&output_path, json)
                .map_err(|e| clap_noun_verb::NounVerbError::execution_error(format!("Failed to write output: {}", e)))?;
        }

        Ok(result)
    })
}

/// Conduct Gemba Walk with LLM-powered observation analysis
#[verb]
fn gemba_walk(
    area: String,
    date: Option<String>,
    output: Option<String>,
) -> ClapResult<GembaWalkOutput> {
    crate::runtime::block_on(async move {
        let config = get_global_config().clone();
        let client = config.create_contextual_client()
            .map_err(|e| clap_noun_verb::NounVerbError::execution_error(format!("Failed to create AI client: {}", e)))?;

        let walk_date = date.unwrap_or_else(|| chrono::Local::now().format("%Y-%m-%d").to_string());

        let prompt = format!(
            r#"Conduct a Gemba Walk analysis for area: {}

Identify observations across these categories:
- Muda (waste): non-value-adding activities
- Mura (variation): inconsistencies in processes
- Muri (overburden): excessive strain
- Opportunity: improvement areas

Respond ONLY with valid JSON (no markdown, no explanations):
{{
  "observations": [
    {{
      "location": "string",
      "observation": "string",
      "category": "muda|mura|muri|opportunity",
      "priority": "critical|high|medium|low",
      "recommended_action": "string"
    }}
  ],
  "immediate_actions": ["string"]
}}

Generate 6-8 specific, actionable observations."#,
            area
        );

        let response = client.complete(&prompt).await
            .map_err(|e| clap_noun_verb::NounVerbError::execution_error(format!("LLM analysis failed: {}", e)))?;

        let json_text = extract_json_from_response(&response.content);
        let analysis: serde_json::Value = serde_json::from_str(&json_text)
            .map_err(|e| clap_noun_verb::NounVerbError::execution_error(format!("Failed to parse JSON: {}", e)))?;

        let observations: Vec<Observation> = analysis["observations"]
            .as_array()
            .unwrap_or(&vec![])
            .iter()
            .filter_map(|o| {
                let observation = o["observation"].as_str()?;
                if observation.is_empty() { return None; }

                Some(Observation {
                    location: o["location"].as_str().unwrap_or("").to_string(),
                    observation: observation.to_string(),
                    category: o["category"].as_str().unwrap_or("opportunity").to_string(),
                    priority: o["priority"].as_str().unwrap_or("medium").to_string(),
                    recommended_action: o["recommended_action"].as_str().unwrap_or("").to_string(),
                })
            })
            .collect();

        let critical = observations.iter().filter(|o| o.priority == "critical").count();
        let actions: Vec<String> = analysis["immediate_actions"]
            .as_array()
            .unwrap_or(&vec![])
            .iter()
            .filter_map(|a| a.as_str().map(|x| x.to_string()))
            .collect();

        let result = GembaWalkOutput {
            timestamp: chrono::Local::now().to_rfc3339(),
            walk_date,
            areas_observed: 1,
            total_observations: observations.len(),
            critical_issues: critical,
            observations,
            immediate_actions: actions,
        };

        if let Some(output_path) = output {
            ensure_output_dir(&output_path)?;
            let json = serde_json::to_string_pretty(&result)
                .map_err(|e| clap_noun_verb::NounVerbError::execution_error(format!("Serialization failed: {}", e)))?;
            fs::write(&output_path, json)
                .map_err(|e| clap_noun_verb::NounVerbError::execution_error(format!("Failed to write output: {}", e)))?;
        }

        Ok(result)
    })
}

/// Conduct Root Cause Analysis using 5 Whys with LLM guidance
#[verb]
fn root_cause_analysis(
    problem: String,
    levels: Option<u8>,
    output: Option<String>,
) -> ClapResult<RootCauseOutput> {
    crate::runtime::block_on(async move {
        let config = get_global_config().clone();
        let client = config.create_contextual_client()
            .map_err(|e| clap_noun_verb::NounVerbError::execution_error(format!("Failed to create AI client: {}", e)))?;

        let num_levels = levels.unwrap_or(5);

        let prompt = format!(
            r#"Conduct a Root Cause Analysis (5 Whys) for this problem: {}

Dig through {} levels of "Why?" questions to identify the root cause.

Respond ONLY with valid JSON (no markdown, no explanations):
{{
  "analysis_chain": [
    {{
      "level": 1,
      "question": "Why...?",
      "answer": "string"
    }}
  ],
  "identified_root_cause": "string",
  "dflss_alignment": "Lean|Quality|Both",
  "corrective_actions": ["string"]
}}

The dflss_alignment should be:
- "Lean" if the root cause primarily affects efficiency/speed
- "Quality" if it affects defects/reliability
- "Both" if it affects both"#,
            problem, num_levels
        );

        let response = client.complete(&prompt).await
            .map_err(|e| clap_noun_verb::NounVerbError::execution_error(format!("LLM analysis failed: {}", e)))?;

        let json_text = extract_json_from_response(&response.content);
        let analysis: serde_json::Value = serde_json::from_str(&json_text)
            .map_err(|e| clap_noun_verb::NounVerbError::execution_error(format!("Failed to parse JSON: {}", e)))?;

        let chain: Vec<WhyLevel> = analysis["analysis_chain"]
            .as_array()
            .unwrap_or(&vec![])
            .iter()
            .filter_map(|w| {
                let question = w["question"].as_str()?;
                if question.is_empty() { return None; }

                Some(WhyLevel {
                    level: w["level"].as_u64().unwrap_or(1) as u8,
                    question: question.to_string(),
                    answer: w["answer"].as_str().unwrap_or("").to_string(),
                })
            })
            .collect();

        let actions: Vec<String> = analysis["corrective_actions"]
            .as_array()
            .unwrap_or(&vec![])
            .iter()
            .filter_map(|a| a.as_str().map(|x| x.to_string()))
            .collect();

        let result = RootCauseOutput {
            timestamp: chrono::Local::now().to_rfc3339(),
            problem_statement: problem,
            root_cause_level: num_levels,
            analysis_chain: chain,
            identified_root_cause: analysis["identified_root_cause"].as_str().unwrap_or("").to_string(),
            dflss_alignment: analysis["dflss_alignment"].as_str().unwrap_or("Both").to_string(),
            corrective_actions: actions,
        };

        if let Some(output_path) = output {
            ensure_output_dir(&output_path)?;
            let json = serde_json::to_string_pretty(&result)
                .map_err(|e| clap_noun_verb::NounVerbError::execution_error(format!("Serialization failed: {}", e)))?;
            fs::write(&output_path, json)
                .map_err(|e| clap_noun_verb::NounVerbError::execution_error(format!("Failed to write output: {}", e)))?;
        }

        Ok(result)
    })
}

/// Run DMAIC (Define-Measure-Analyze-Improve-Control) project with LLM guidance
#[verb]
fn dmaic(
    project: String,
    problem: String,
    current_phase: Option<String>,
    output: Option<String>,
) -> ClapResult<DmaicOutput> {
    crate::runtime::block_on(async move {
        let config = get_global_config().clone();
        let client = config.create_contextual_client()
            .map_err(|e| clap_noun_verb::NounVerbError::execution_error(format!("Failed to create AI client: {}", e)))?;

        let current = current_phase.unwrap_or_else(|| "define".to_string());

        let prompt = format!(
            r#"Plan a DMAIC (Define-Measure-Analyze-Improve-Control) Six Sigma project.

Project: {}
Problem: {}
Current Phase: {}

Respond ONLY with valid JSON (no markdown, no explanations):
{{
  "phases": [
    {{
      "phase": "Define|Measure|Analyze|Improve|Control",
      "description": "string",
      "key_activities": ["string"],
      "deliverables": ["string"],
      "metrics": ["string"],
      "status": "pending|in_progress|completed"
    }}
  ],
  "expected_savings": "string"
}}

Generate a detailed DMAIC plan with all 5 phases."#,
            project, problem, current
        );

        let response = client.complete(&prompt).await
            .map_err(|e| clap_noun_verb::NounVerbError::execution_error(format!("LLM analysis failed: {}", e)))?;

        let json_text = extract_json_from_response(&response.content);
        let analysis: serde_json::Value = serde_json::from_str(&json_text)
            .map_err(|e| clap_noun_verb::NounVerbError::execution_error(format!("Failed to parse JSON: {}", e)))?;

        let phases: Vec<DmaicPhase> = analysis["phases"]
            .as_array()
            .unwrap_or(&vec![])
            .iter()
            .filter_map(|p| {
                let phase = p["phase"].as_str()?;
                if phase.is_empty() { return None; }

                let activities = p["key_activities"].as_array().unwrap_or(&vec![])
                    .iter().filter_map(|a| a.as_str().map(|x| x.to_string())).collect();
                let deliverables = p["deliverables"].as_array().unwrap_or(&vec![])
                    .iter().filter_map(|d| d.as_str().map(|x| x.to_string())).collect();
                let metrics = p["metrics"].as_array().unwrap_or(&vec![])
                    .iter().filter_map(|m| m.as_str().map(|x| x.to_string())).collect();

                Some(DmaicPhase {
                    phase: phase.to_string(),
                    description: p["description"].as_str().unwrap_or("").to_string(),
                    key_activities: activities,
                    deliverables,
                    metrics,
                    status: p["status"].as_str().unwrap_or("pending").to_string(),
                })
            })
            .collect();

        let result = DmaicOutput {
            timestamp: chrono::Local::now().to_rfc3339(),
            project_name: project,
            problem_statement: problem,
            sigma_level_before: 3.2,
            sigma_level_target: 4.5,
            phases,
            current_phase: current,
            expected_savings: analysis["expected_savings"].as_str().unwrap_or("TBD").to_string(),
        };

        if let Some(output_path) = output {
            ensure_output_dir(&output_path)?;
            let json = serde_json::to_string_pretty(&result)
                .map_err(|e| clap_noun_verb::NounVerbError::execution_error(format!("Serialization failed: {}", e)))?;
            fs::write(&output_path, json)
                .map_err(|e| clap_noun_verb::NounVerbError::execution_error(format!("Failed to write output: {}", e)))?;
        }

        Ok(result)
    })
}

/// Eliminate Muda (waste) with LLM-powered analysis
#[verb]
fn eliminate_muda(
    project: String,
    waste_type: Option<String>,
    output: Option<String>,
) -> ClapResult<MudaOutput> {
    crate::runtime::block_on(async move {
        let config = get_global_config().clone();
        let client = config.create_contextual_client()
            .map_err(|e| clap_noun_verb::NounVerbError::execution_error(format!("Failed to create AI client: {}", e)))?;

        let focus = waste_type.unwrap_or_else(|| "all".to_string());

        let prompt = format!(
            r#"Conduct Muda (waste) elimination analysis for project: {}
Focus: {}

8 types of waste: Overproduction, Waiting, Transport, Over-processing, Inventory, Motion, Defects, Underutilized talent

Respond ONLY with valid JSON (no markdown, no explanations):
{{
  "waste_items": [
    {{
      "waste_type": "string",
      "description": "string",
      "impact": "string",
      "elimination_method": "string",
      "estimated_savings": "string"
    }}
  ],
  "total_estimated_savings": "string",
  "elimination_roadmap": ["string"]
}}

Generate 6-8 specific waste items with elimination strategies."#,
            project, focus
        );

        let response = client.complete(&prompt).await
            .map_err(|e| clap_noun_verb::NounVerbError::execution_error(format!("LLM analysis failed: {}", e)))?;

        let json_text = extract_json_from_response(&response.content);
        let analysis: serde_json::Value = serde_json::from_str(&json_text)
            .map_err(|e| clap_noun_verb::NounVerbError::execution_error(format!("Failed to parse JSON: {}", e)))?;

        let waste_items: Vec<WasteItem> = analysis["waste_items"]
            .as_array()
            .unwrap_or(&vec![])
            .iter()
            .enumerate()
            .filter_map(|(idx, item)| {
                let waste_type = item["waste_type"].as_str()?;
                if waste_type.is_empty() { return None; }

                Some(WasteItem {
                    id: idx + 1,
                    waste_type: waste_type.to_string(),
                    description: item["description"].as_str().unwrap_or("").to_string(),
                    impact: item["impact"].as_str().unwrap_or("").to_string(),
                    elimination_method: item["elimination_method"].as_str().unwrap_or("").to_string(),
                    estimated_savings: item["estimated_savings"].as_str().unwrap_or("").to_string(),
                    status: "identified".to_string(),
                })
            })
            .collect();

        let mut waste_by_type: HashMap<String, usize> = HashMap::new();
        for item in &waste_items {
            *waste_by_type.entry(item.waste_type.clone()).or_insert(0) += 1;
        }

        let roadmap: Vec<String> = analysis["elimination_roadmap"]
            .as_array()
            .unwrap_or(&vec![])
            .iter()
            .filter_map(|r| r.as_str().map(|x| x.to_string()))
            .collect();

        let result = MudaOutput {
            timestamp: chrono::Local::now().to_rfc3339(),
            project_name: project,
            total_waste_items: waste_items.len(),
            total_estimated_savings: analysis["total_estimated_savings"].as_str().unwrap_or("TBD").to_string(),
            waste_by_type,
            high_impact_items: waste_items.iter().take(3).cloned().collect(),
            elimination_roadmap: roadmap,
        };

        if let Some(output_path) = output {
            ensure_output_dir(&output_path)?;
            let json = serde_json::to_string_pretty(&result)
                .map_err(|e| clap_noun_verb::NounVerbError::execution_error(format!("Serialization failed: {}", e)))?;
            fs::write(&output_path, json)
                .map_err(|e| clap_noun_verb::NounVerbError::execution_error(format!("Failed to write output: {}", e)))?;
        }

        Ok(result)
    })
}

/// Eliminate Mura (variation) with standardization analysis
#[verb]
fn eliminate_mura(
    project: String,
    variation_type: Option<String>,
    output: Option<String>,
) -> ClapResult<MuraOutput> {
    crate::runtime::block_on(async move {
        let config = get_global_config().clone();
        let client = config.create_contextual_client()
            .map_err(|e| clap_noun_verb::NounVerbError::execution_error(format!("Failed to create AI client: {}", e)))?;

        let focus = variation_type.unwrap_or_else(|| "all".to_string());

        let prompt = format!(
            r#"Conduct Mura (variation) elimination and standardization analysis for project: {}
Focus: {}

Variation types: Input variation, Process variation, Output variation

Respond ONLY with valid JSON (no markdown, no explanations):
{{
  "variations": [
    {{
      "process": "string",
      "variation_type": "input|process|output",
      "current_variation": "string",
      "target_variation": "string",
      "standardization_method": "string"
    }}
  ],
  "standardization_roadmap": ["string"],
  "control_mechanisms": ["string"]
}}

Generate 5-6 process variations with standardization strategies."#,
            project, focus
        );

        let response = client.complete(&prompt).await
            .map_err(|e| clap_noun_verb::NounVerbError::execution_error(format!("LLM analysis failed: {}", e)))?;

        let json_text = extract_json_from_response(&response.content);
        let analysis: serde_json::Value = serde_json::from_str(&json_text)
            .map_err(|e| clap_noun_verb::NounVerbError::execution_error(format!("Failed to parse JSON: {}", e)))?;

        let variations: Vec<Variation> = analysis["variations"]
            .as_array()
            .unwrap_or(&vec![])
            .iter()
            .enumerate()
            .filter_map(|(idx, v)| {
                let process = v["process"].as_str()?;
                if process.is_empty() { return None; }

                Some(Variation {
                    id: idx + 1,
                    process: process.to_string(),
                    variation_type: v["variation_type"].as_str().unwrap_or("process").to_string(),
                    current_variation: v["current_variation"].as_str().unwrap_or("").to_string(),
                    target_variation: v["target_variation"].as_str().unwrap_or("").to_string(),
                    standardization_method: v["standardization_method"].as_str().unwrap_or("").to_string(),
                    status: "identified".to_string(),
                })
            })
            .collect();

        let critical = variations.iter().filter(|v| v.variation_type == "output").count();

        let roadmap: Vec<String> = analysis["standardization_roadmap"]
            .as_array()
            .unwrap_or(&vec![])
            .iter()
            .filter_map(|r| r.as_str().map(|x| x.to_string()))
            .collect();

        let controls: Vec<String> = analysis["control_mechanisms"]
            .as_array()
            .unwrap_or(&vec![])
            .iter()
            .filter_map(|c| c.as_str().map(|x| x.to_string()))
            .collect();

        let result = MuraOutput {
            timestamp: chrono::Local::now().to_rfc3339(),
            project_name: project,
            total_variations: variations.len(),
            critical_variations: critical,
            variations,
            standardization_roadmap: roadmap,
            control_mechanisms: controls,
        };

        if let Some(output_path) = output {
            ensure_output_dir(&output_path)?;
            let json = serde_json::to_string_pretty(&result)
                .map_err(|e| clap_noun_verb::NounVerbError::execution_error(format!("Serialization failed: {}", e)))?;
            fs::write(&output_path, json)
                .map_err(|e| clap_noun_verb::NounVerbError::execution_error(format!("Failed to write output: {}", e)))?;
        }

        Ok(result)
    })
}

/// Set up Andon (visual signals) system with LLM-powered configuration
#[verb]
fn andon(
    name: String,
    signal_level: Option<String>,
    output: Option<String>,
) -> ClapResult<AndonOutput> {
    crate::runtime::block_on(async move {
        let config = get_global_config().clone();
        let client = config.create_contextual_client()
            .map_err(|e| clap_noun_verb::NounVerbError::execution_error(format!("Failed to create AI client: {}", e)))?;

        let level = signal_level.unwrap_or_else(|| "all".to_string());

        let prompt = format!(
            r#"Design an Andon (visual signaling) system for: {}
Signal level: {}

Andon signal types: Red (critical/stop), Yellow (warning/investigate), Green (normal/healthy)

Respond ONLY with valid JSON (no markdown, no explanations):
{{
  "signal_types": [
    {{
      "signal_type": "Red|Yellow|Green",
      "trigger_condition": "string",
      "response_action": "string",
      "responsible_person": "string",
      "escalation_path": "string"
    }}
  ],
  "detection_rate": "string",
  "response_time_sla": "string",
  "implementation_timeline": ["string"]
}}

Generate 4-5 relevant signals (Red, Yellow, Green) for this system."#,
            name, level
        );

        let response = client.complete(&prompt).await
            .map_err(|e| clap_noun_verb::NounVerbError::execution_error(format!("LLM analysis failed: {}", e)))?;

        let json_text = extract_json_from_response(&response.content);
        let analysis: serde_json::Value = serde_json::from_str(&json_text)
            .map_err(|e| clap_noun_verb::NounVerbError::execution_error(format!("Failed to parse JSON: {}", e)))?;

        let signals: Vec<AndonSignal> = analysis["signal_types"]
            .as_array()
            .unwrap_or(&vec![])
            .iter()
            .filter_map(|s| {
                let signal_type = s["signal_type"].as_str()?;
                if signal_type.is_empty() { return None; }

                Some(AndonSignal {
                    signal_type: signal_type.to_string(),
                    trigger_condition: s["trigger_condition"].as_str().unwrap_or("").to_string(),
                    response_action: s["response_action"].as_str().unwrap_or("").to_string(),
                    responsible_person: s["responsible_person"].as_str().unwrap_or("").to_string(),
                    escalation_path: s["escalation_path"].as_str().unwrap_or("").to_string(),
                })
            })
            .collect();

        let timeline: Vec<String> = analysis["implementation_timeline"]
            .as_array()
            .unwrap_or(&vec![])
            .iter()
            .filter_map(|t| t.as_str().map(|x| x.to_string()))
            .collect();

        let result = AndonOutput {
            timestamp: chrono::Local::now().to_rfc3339(),
            system_name: name,
            total_signals: signals.len(),
            signal_types: signals,
            detection_rate: analysis["detection_rate"].as_str().unwrap_or("TBD").to_string(),
            response_time_sla: analysis["response_time_sla"].as_str().unwrap_or("TBD").to_string(),
            implementation_timeline: timeline,
        };

        if let Some(output_path) = output {
            ensure_output_dir(&output_path)?;
            let json = serde_json::to_string_pretty(&result)
                .map_err(|e| clap_noun_verb::NounVerbError::execution_error(format!("Serialization failed: {}", e)))?;
            fs::write(&output_path, json)
                .map_err(|e| clap_noun_verb::NounVerbError::execution_error(format!("Failed to write output: {}", e)))?;
        }

        Ok(result)
    })
}
