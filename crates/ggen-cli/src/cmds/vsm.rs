//! Value Stream Mapping Commands
//!
//! CLI commands for end-to-end value stream visualization, bottleneck detection,
//! and efficiency analysis from ontology design to deployed code.

use clap_noun_verb::Result;
use clap_noun_verb_macros::verb;
use serde::Serialize;

// ============================================================================
// Output Types (all must derive Serialize for JSON output)
// ============================================================================

#[derive(Serialize)]
struct AnalyzeOutput {
    stream_name: String,
    total_lead_time_ms: f64,
    process_efficiency: f64,
    efficiency_grade: char,
    stage_count: usize,
    bottleneck_count: usize,
    critical_bottleneck_count: usize,
    improvement_potential_ms: f64,
    improvement_potential_pct: f64,
    recommendations_count: usize,
}

#[derive(Serialize)]
struct VisualizeOutput {
    visualization_generated: bool,
    output_format: String,
    charts_included: Vec<String>,
    summary_stats: String,
}

#[derive(Serialize)]
struct BottlenecksOutput {
    total_bottlenecks: usize,
    critical_count: usize,
    high_count: usize,
    total_delay_ms: f64,
    improvement_potential_ms: f64,
    top_bottlenecks: Vec<BottleneckInfo>,
}

#[derive(Serialize)]
struct BottleneckInfo {
    stage: String,
    bottleneck_type: String,
    severity: String,
    impact_ms: f64,
    delay_percentage: f64,
}

#[derive(Serialize)]
struct EfficiencyOutput {
    overall_efficiency: f64,
    efficiency_grade: char,
    value_added_ratio: f64,
    best_stage: Option<String>,
    worst_stage: Option<String>,
    stage_efficiencies: Vec<StageEfficiency>,
}

#[derive(Serialize)]
struct StageEfficiency {
    stage: String,
    efficiency: f64,
    grade: char,
}

#[derive(Serialize)]
struct SwimLaneOutput {
    total_lanes: usize,
    total_handoffs: usize,
    average_handoff_time_ms: f64,
    most_loaded: Option<StakeholderLoad>,
    least_loaded: Option<StakeholderLoad>,
    overloaded_count: usize,
}

#[derive(Serialize)]
struct StakeholderLoad {
    role: String,
    utilization: f64,
}

#[derive(Serialize)]
struct MoonshotOutput {
    future_state_name: String,
    target_date: String,
    moonshot_targets_count: usize,
    initiatives_count: usize,
    overall_progress: f64,
    targets: Vec<MoonshotTargetInfo>,
}

#[derive(Serialize)]
struct MoonshotTargetInfo {
    name: String,
    metric_name: String,
    current_baseline: f64,
    target_value: f64,
    improvement_factor: f64,
    unit: String,
}

// ============================================================================
// Verb Functions (the actual CLI commands)
// ============================================================================

/// Analyze complete value stream and generate comprehensive report
#[verb]
fn analyze(stream_name: Option<String>) -> Result<AnalyzeOutput> {
    // For now, generate a demo value stream
    // In production, this would load from a file or database
    let stream = create_demo_stream(stream_name.unwrap_or_else(|| "semantic-generation".to_string()));

    let report = ggen_domain::vsm::AnalysisReport::generate(&stream, None);

    Ok(AnalyzeOutput {
        stream_name: stream.name.clone(),
        total_lead_time_ms: report.overall_metrics.total_lead_time_ms,
        process_efficiency: report.overall_metrics.overall_efficiency,
        efficiency_grade: report.efficiency.overall_efficiency
            .to_string()
            .chars()
            .next()
            .unwrap_or('F'),
        stage_count: report.overall_metrics.stage_count,
        bottleneck_count: report.bottleneck_summary.total_bottlenecks,
        critical_bottleneck_count: report.bottleneck_summary.critical_count,
        improvement_potential_ms: report.bottleneck_summary.total_improvement_potential_ms,
        improvement_potential_pct: if report.overall_metrics.total_lead_time_ms > 0.0 {
            (report.bottleneck_summary.total_improvement_potential_ms
                / report.overall_metrics.total_lead_time_ms)
                * 100.0
        } else {
            0.0
        },
        recommendations_count: report.recommendations.len(),
    })
}

/// Detect and analyze bottlenecks in the value stream
#[verb]
fn bottlenecks(stream_name: Option<String>, top_n: Option<usize>) -> Result<BottlenecksOutput> {
    let stream = create_demo_stream(stream_name.unwrap_or_else(|| "semantic-generation".to_string()));

    let top = top_n.unwrap_or(5);
    let top_bottlenecks: Vec<BottleneckInfo> = stream
        .bottlenecks
        .top_bottlenecks(top)
        .into_iter()
        .map(|b| BottleneckInfo {
            stage: b.stage.to_string(),
            bottleneck_type: b.bottleneck_type.to_string(),
            severity: b.severity.to_string(),
            impact_ms: b.impact_ms,
            delay_percentage: b.delay_percentage,
        })
        .collect();

    Ok(BottlenecksOutput {
        total_bottlenecks: stream.bottlenecks.bottlenecks.len(),
        critical_count: stream.bottlenecks.critical().len(),
        high_count: stream.bottlenecks.high_severity().len(),
        total_delay_ms: stream.bottlenecks.total_delay_ms,
        improvement_potential_ms: stream.bottlenecks.total_improvement_potential_ms,
        top_bottlenecks,
    })
}

/// Calculate process efficiency ratios across all stages
#[verb]
fn efficiency(stream_name: Option<String>) -> Result<EfficiencyOutput> {
    let stream = create_demo_stream(stream_name.unwrap_or_else(|| "semantic-generation".to_string()));

    let analysis = ggen_domain::vsm::EfficiencyAnalysis::from_stream(&stream);

    let stage_efficiencies: Vec<StageEfficiency> = stream
        .stages
        .iter()
        .map(|s| StageEfficiency {
            stage: s.stage_type.to_string(),
            efficiency: s.metrics.process_efficiency,
            grade: s.metrics.efficiency_grade(),
        })
        .collect();

    Ok(EfficiencyOutput {
        overall_efficiency: analysis.overall_efficiency,
        efficiency_grade: if analysis.overall_efficiency >= 0.9 {
            'A'
        } else if analysis.overall_efficiency >= 0.8 {
            'B'
        } else if analysis.overall_efficiency >= 0.7 {
            'C'
        } else if analysis.overall_efficiency >= 0.6 {
            'D'
        } else {
            'F'
        },
        value_added_ratio: analysis.value_added_ratio,
        best_stage: analysis.best_stage.map(|s| s.to_string()),
        worst_stage: analysis.worst_stage.map(|s| s.to_string()),
        stage_efficiencies,
    })
}

/// Analyze swim lanes and stakeholder touchpoints
#[verb]
fn swimlanes(stream_name: Option<String>) -> Result<SwimLaneOutput> {
    let stream = create_demo_stream(stream_name.unwrap_or_else(|| "semantic-generation".to_string()));

    let most_loaded = stream
        .swim_lanes
        .most_loaded_stakeholder()
        .map(|(role, util)| StakeholderLoad {
            role: role.to_string(),
            utilization: util,
        });

    let least_loaded = stream
        .swim_lanes
        .least_loaded_stakeholder()
        .map(|(role, util)| StakeholderLoad {
            role: role.to_string(),
            utilization: util,
        });

    Ok(SwimLaneOutput {
        total_lanes: stream.swim_lanes.lanes.len(),
        total_handoffs: stream.swim_lanes.total_handoffs,
        average_handoff_time_ms: stream.swim_lanes.average_handoff_time_ms,
        most_loaded,
        least_loaded,
        overloaded_count: stream.swim_lanes.overloaded_stakeholders().len(),
    })
}

/// Show moonshot 2028 targets and future state mapping
#[verb]
fn moonshot() -> Result<MoonshotOutput> {
    let future_state = ggen_domain::vsm::FutureState::moonshot_2028();

    let targets: Vec<MoonshotTargetInfo> = future_state
        .moonshot_targets
        .iter()
        .map(|t| MoonshotTargetInfo {
            name: t.name.clone(),
            metric_name: t.metric_name.clone(),
            current_baseline: t.current_baseline,
            target_value: t.target_value,
            improvement_factor: t.improvement_factor,
            unit: t.unit.clone(),
        })
        .collect();

    Ok(MoonshotOutput {
        future_state_name: future_state.name.clone(),
        target_date: future_state.target_date.format("%Y-%m-%d").to_string(),
        moonshot_targets_count: future_state.moonshot_targets.len(),
        initiatives_count: future_state.initiatives.len(),
        overall_progress: 0.0, // Would calculate from current metrics
        targets,
    })
}

/// Generate visualization data for dashboards
#[verb]
fn visualize(stream_name: Option<String>, format: Option<String>) -> Result<VisualizeOutput> {
    let stream = create_demo_stream(stream_name.unwrap_or_else(|| "semantic-generation".to_string()));

    let viz_data = ggen_domain::vsm::VisualizationData::from_value_stream(&stream);

    let output_format = format.unwrap_or_else(|| "json".to_string());

    let charts_included = vec![
        "lead_time_chart".to_string(),
        "efficiency_chart".to_string(),
        "bottleneck_chart".to_string(),
        "swim_lane_view".to_string(),
        "flow_diagram".to_string(),
    ];

    Ok(VisualizeOutput {
        visualization_generated: true,
        output_format,
        charts_included,
        summary_stats: viz_data.summary_stats.total_stages.to_string(),
    })
}

// ============================================================================
// Helper Functions
// ============================================================================

/// Create a demo value stream for testing
fn create_demo_stream(name: String) -> ggen_domain::vsm::ValueStream {
    use ggen_domain::vsm::{Stage, StageType, ValueStream};
    use std::time::Duration;

    let mut stream = ValueStream::new(name);
    stream.start();

    // Ontology Design stage
    let mut stage1 = Stage::new(StageType::OntologyDesign);
    stage1.start();
    stage1.record_process_time(Duration::from_millis(5000));
    stage1.metrics.timing.lead_time_ms = 8000.0;
    stage1.metrics.timing.wait_time_ms = 3000.0;
    stage1.metrics.process_efficiency = 0.625; // 5000/8000
    stage1.end();

    // Template Creation stage
    let mut stage2 = Stage::new(StageType::TemplateCreation);
    stage2.start();
    stage2.record_process_time(Duration::from_millis(7000));
    stage2.metrics.timing.lead_time_ms = 10000.0;
    stage2.metrics.timing.wait_time_ms = 3000.0;
    stage2.metrics.process_efficiency = 0.7;
    stage2.end();

    // Data Binding stage
    let mut stage3 = Stage::new(StageType::DataBinding);
    stage3.start();
    stage3.record_process_time(Duration::from_millis(3000));
    stage3.metrics.timing.lead_time_ms = 5000.0;
    stage3.metrics.timing.wait_time_ms = 2000.0;
    stage3.metrics.process_efficiency = 0.6;
    stage3.end();

    // Template Processing stage (bottleneck)
    let mut stage4 = Stage::new(StageType::TemplateProcessing);
    stage4.start();
    stage4.record_process_time(Duration::from_millis(4000));
    stage4.metrics.timing.lead_time_ms = 15000.0; // Slow!
    stage4.metrics.timing.wait_time_ms = 11000.0;
    stage4.metrics.process_efficiency = 0.267; // Low efficiency
    stage4.queue_length = 25; // High queue
    stage4.end();

    // Code Generation stage
    let mut stage5 = Stage::new(StageType::CodeGeneration);
    stage5.start();
    stage5.record_process_time(Duration::from_millis(6000));
    stage5.metrics.timing.lead_time_ms = 8000.0;
    stage5.metrics.timing.wait_time_ms = 2000.0;
    stage5.metrics.process_efficiency = 0.75;
    stage5.end();

    // Validation stage (approval bottleneck)
    let mut stage6 = Stage::new(StageType::Validation);
    stage6.start();
    stage6.record_process_time(Duration::from_millis(2000));
    stage6.metrics.timing.lead_time_ms = 12000.0;
    stage6.metrics.timing.wait_time_ms = 10000.0;
    stage6.metrics.process_efficiency = 0.167;
    stage6.approval_required = true;
    stage6.record_approval_time(Duration::from_millis(8000));
    stage6.end();

    // Testing stage
    let mut stage7 = Stage::new(StageType::Testing);
    stage7.start();
    stage7.record_process_time(Duration::from_millis(8000));
    stage7.metrics.timing.lead_time_ms = 10000.0;
    stage7.metrics.timing.wait_time_ms = 2000.0;
    stage7.metrics.process_efficiency = 0.8;
    stage7.end();

    // Deployment stage
    let mut stage8 = Stage::new(StageType::Deployment);
    stage8.start();
    stage8.record_process_time(Duration::from_millis(5000));
    stage8.metrics.timing.lead_time_ms = 7000.0;
    stage8.metrics.timing.wait_time_ms = 2000.0;
    stage8.metrics.process_efficiency = 0.714;
    stage8.end();

    stream.add_stage(stage1);
    stream.add_stage(stage2);
    stream.add_stage(stage3);
    stream.add_stage(stage4);
    stream.add_stage(stage5);
    stream.add_stage(stage6);
    stream.add_stage(stage7);
    stream.add_stage(stage8);

    stream.end();
    stream
}
