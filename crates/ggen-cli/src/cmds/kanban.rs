// Kanban Commands - Pull system for ontology evolution
//
// This module implements Kanban workflow commands using clap-noun-verb v3.4.0 #[verb] pattern

use clap_noun_verb::Result as NounVerbResult;
use clap_noun_verb_macros::verb;
use serde::Serialize;
use std::path::PathBuf;

// ============================================================================
// Output Types
// ============================================================================

#[derive(Serialize)]
struct InitOutput {
    board_path: String,
    max_concurrent_modifications: usize,
    wip_limits: std::collections::HashMap<String, usize>,
}

#[derive(Serialize)]
struct StatusOutput {
    total_wip: usize,
    backlog: usize,
    analysis: usize,
    transformation: usize,
    validation: usize,
    generation: usize,
    done: usize,
    bottlenecks: Vec<BottleneckInfo>,
}

#[derive(Serialize)]
struct BottleneckInfo {
    stage: String,
    utilization: f64,
}

#[derive(Serialize)]
struct CardListOutput {
    cards: Vec<CardInfo>,
    total: usize,
    stage: Option<String>,
}

#[derive(Serialize)]
struct CardInfo {
    id: String,
    title: String,
    status: String,
    priority: String,
    stage: String,
    rdf_changes: usize,
    affected_templates: usize,
    age_hours: f64,
}

#[derive(Serialize)]
struct CardShowOutput {
    id: String,
    title: String,
    description: String,
    status: String,
    priority: String,
    stage: String,
    rdf_changes: Vec<String>,
    affected_templates: Vec<String>,
    affected_files: Vec<String>,
    created_at: String,
    updated_at: String,
    cycle_times: std::collections::HashMap<String, f64>,
}

#[derive(Serialize)]
struct CardCreateOutput {
    id: String,
    title: String,
    priority: String,
}

#[derive(Serialize)]
struct CardMoveOutput {
    id: String,
    from_stage: String,
    to_stage: String,
}

#[derive(Serialize)]
struct PullOutput {
    pulled_card_id: Option<String>,
    stage: String,
}

#[derive(Serialize)]
struct MetricsOutput {
    lead_time_avg: f64,
    throughput: f64,
    wip: usize,
    flow_efficiency: f64,
    completed_count: usize,
    blocked_count: usize,
    bottlenecks: Vec<BottleneckAnalysis>,
}

#[derive(Serialize)]
struct BottleneckAnalysis {
    stage: String,
    severity: String,
    reason: String,
    recommendation: String,
}

#[derive(Serialize)]
struct FlowDiagramOutput {
    data_points: usize,
    average_wip: f64,
    trends: std::collections::HashMap<String, String>,
    csv_path: Option<String>,
    json_path: Option<String>,
}

#[derive(Serialize)]
struct DemandSignalOutput {
    signal_id: String,
    signal_type: String,
    priority: String,
    pending_count: usize,
}

// ============================================================================
// Verb Functions
// ============================================================================

/// Initialize a new Kanban board for ontology evolution
#[verb]
fn init(
    #[arg(long, help = "Maximum concurrent template modifications", default_value = "3")]
    max_concurrent: usize,
    #[arg(long, help = "Path to save board state", default_value = ".ggen/kanban.json")]
    board_path: String,
) -> NounVerbResult<InitOutput> {
    use ggen_domain::kanban::{KanbanConfig, KanbanBoard};
    use std::fs;

    let config = KanbanConfig {
        max_concurrent_modifications: max_concurrent,
        ..Default::default()
    };

    let board = KanbanBoard::new(config.wip_limits.clone());

    // Save board to file
    let board_json = serde_json::to_string_pretty(&board).map_err(|e| {
        clap_noun_verb::NounVerbError::execution_error(format!("Failed to serialize board: {}", e))
    })?;

    // Create directory if needed
    if let Some(parent) = std::path::Path::new(&board_path).parent() {
        fs::create_dir_all(parent).map_err(|e| {
            clap_noun_verb::NounVerbError::execution_error(format!("Failed to create directory: {}", e))
        })?;
    }

    fs::write(&board_path, board_json).map_err(|e| {
        clap_noun_verb::NounVerbError::execution_error(format!("Failed to write board: {}", e))
    })?;

    Ok(InitOutput {
        board_path,
        max_concurrent_modifications: max_concurrent,
        wip_limits: config.wip_limits,
    })
}

/// Show Kanban board status
#[verb]
fn status(
    #[arg(long, help = "Path to board state", default_value = ".ggen/kanban.json")]
    board_path: String,
) -> NounVerbResult<StatusOutput> {
    use ggen_domain::kanban::{KanbanBoard, WorkflowStage};

    let board = load_board(&board_path)?;

    let bottlenecks: Vec<BottleneckInfo> = board
        .bottleneck_stages()
        .into_iter()
        .map(|(stage, utilization)| BottleneckInfo {
            stage: stage.as_str().to_string(),
            utilization,
        })
        .collect();

    Ok(StatusOutput {
        total_wip: board.total_wip(),
        backlog: board.get_stage(WorkflowStage::Backlog).map(|s| s.wip_count()).unwrap_or(0),
        analysis: board.get_stage(WorkflowStage::Analysis).map(|s| s.wip_count()).unwrap_or(0),
        transformation: board.get_stage(WorkflowStage::Transformation).map(|s| s.wip_count()).unwrap_or(0),
        validation: board.get_stage(WorkflowStage::Validation).map(|s| s.wip_count()).unwrap_or(0),
        generation: board.get_stage(WorkflowStage::Generation).map(|s| s.wip_count()).unwrap_or(0),
        done: board.get_stage(WorkflowStage::Done).map(|s| s.wip_count()).unwrap_or(0),
        bottlenecks,
    })
}

/// List workflow cards
#[verb]
fn list(
    #[arg(long, help = "Filter by stage")]
    stage: Option<String>,
    #[arg(long, help = "Filter by status")]
    status: Option<String>,
    #[arg(long, help = "Path to cards file", default_value = ".ggen/kanban-cards.json")]
    cards_path: String,
) -> NounVerbResult<CardListOutput> {
    use ggen_domain::kanban::card::{RdfChangeTracker, CardStatus};

    let tracker = load_card_tracker(&cards_path)?;

    let mut cards: Vec<CardInfo> = Vec::new();

    for card in tracker.active_cards() {
        // Apply filters
        if let Some(ref s) = stage {
            if card.stage.as_str() != s {
                continue;
            }
        }

        if let Some(ref st) = status {
            let card_status = format!("{:?}", card.status).to_lowercase();
            if card_status != st.to_lowercase() {
                continue;
            }
        }

        cards.push(CardInfo {
            id: card.id.clone(),
            title: card.title.clone(),
            status: format!("{:?}", card.status),
            priority: format!("{:?}", card.priority),
            stage: card.stage.as_str().to_string(),
            rdf_changes: card.rdf_changes.len(),
            affected_templates: card.affected_templates.len(),
            age_hours: card.age_hours(),
        });
    }

    let total = cards.len();

    Ok(CardListOutput {
        cards,
        total,
        stage,
    })
}

/// Show detailed card information
#[verb]
fn show(
    card_id: String,
    #[arg(long, help = "Path to cards file", default_value = ".ggen/kanban-cards.json")]
    cards_path: String,
) -> NounVerbResult<CardShowOutput> {
    use ggen_domain::kanban::card::RdfChangeTracker;

    let tracker = load_card_tracker(&cards_path)?;

    let card = tracker.get_card(&card_id).ok_or_else(|| {
        clap_noun_verb::NounVerbError::execution_error(format!("Card not found: {}", card_id))
    })?;

    Ok(CardShowOutput {
        id: card.id.clone(),
        title: card.title.clone(),
        description: card.description.clone(),
        status: format!("{:?}", card.status),
        priority: format!("{:?}", card.priority),
        stage: card.stage.as_str().to_string(),
        rdf_changes: card.rdf_changes.iter().map(|c| format!("{:?}", c)).collect(),
        affected_templates: card.affected_templates.clone(),
        affected_files: card.affected_files.clone(),
        created_at: card.created_at.to_rfc3339(),
        updated_at: card.updated_at.to_rfc3339(),
        cycle_times: card.cycle_times.clone(),
    })
}

/// Create a new workflow card
#[verb]
fn create(
    title: String,
    description: String,
    #[arg(long, help = "Priority: low, medium, high, critical", default_value = "medium")]
    priority: String,
    #[arg(long, help = "Path to cards file", default_value = ".ggen/kanban-cards.json")]
    cards_path: String,
    #[arg(long, help = "Path to board state", default_value = ".ggen/kanban.json")]
    board_path: String,
) -> NounVerbResult<CardCreateOutput> {
    use ggen_domain::kanban::card::{WorkflowCard, Priority, RdfChangeTracker};
    use ggen_domain::kanban::{KanbanBoard, WorkflowStage};

    let priority_enum = match priority.to_lowercase().as_str() {
        "low" => Priority::Low,
        "medium" => Priority::Medium,
        "high" => Priority::High,
        "critical" => Priority::Critical,
        _ => return Err(clap_noun_verb::NounVerbError::execution_error(
            "Invalid priority. Use: low, medium, high, or critical".to_string()
        )),
    };

    let card = WorkflowCard::new(title.clone(), description, priority_enum);
    let card_id = card.id.clone();

    // Load or create tracker
    let mut tracker = load_card_tracker(&cards_path).unwrap_or_else(|_| RdfChangeTracker::new());
    tracker.add_card(card);

    // Save tracker
    save_card_tracker(&cards_path, &tracker)?;

    // Add card to board
    let mut board = load_board(&board_path)?;
    board.stages.get_mut(&WorkflowStage::Backlog)
        .ok_or_else(|| clap_noun_verb::NounVerbError::execution_error("Backlog stage not found".to_string()))?
        .add_card(card_id.clone())
        .map_err(|e| clap_noun_verb::NounVerbError::execution_error(format!("Failed to add card: {}", e)))?;

    save_board(&board_path, &board)?;

    Ok(CardCreateOutput {
        id: card_id,
        title,
        priority,
    })
}

/// Pull a card from previous stage (demand-driven)
#[verb]
fn pull(
    #[arg(long, help = "Stage to pull into")]
    stage: String,
    #[arg(long, help = "Path to board state", default_value = ".ggen/kanban.json")]
    board_path: String,
) -> NounVerbResult<PullOutput> {
    use ggen_domain::kanban::{KanbanBoard, WorkflowStage};

    let mut board = load_board(&board_path)?;

    let stage_enum = parse_stage(&stage)?;

    let pulled_card_id = board.pull_card(stage_enum).map_err(|e| {
        clap_noun_verb::NounVerbError::execution_error(format!("Failed to pull card: {}", e))
    })?;

    save_board(&board_path, &board)?;

    Ok(PullOutput {
        pulled_card_id,
        stage,
    })
}

/// Calculate and show flow metrics
#[verb]
fn metrics(
    #[arg(long, help = "Path to board state", default_value = ".ggen/kanban.json")]
    board_path: String,
    #[arg(long, help = "Path to cards file", default_value = ".ggen/kanban-cards.json")]
    cards_path: String,
) -> NounVerbResult<MetricsOutput> {
    use ggen_domain::kanban::{KanbanBoard, metrics::{EfficiencyCalculator, Bottleneck}};
    use ggen_domain::kanban::card::RdfChangeTracker;

    let board = load_board(&board_path)?;
    let tracker = load_card_tracker(&cards_path)?;

    let cards: Vec<_> = tracker.active_cards().into_iter().cloned().collect();
    let flow_metrics = EfficiencyCalculator::calculate(&board, &cards);

    let bottlenecks: Vec<BottleneckAnalysis> = EfficiencyCalculator::identify_bottlenecks(&flow_metrics)
        .into_iter()
        .map(|b| BottleneckAnalysis {
            stage: b.stage,
            severity: format!("{:?}", b.severity),
            reason: b.reason,
            recommendation: b.recommendation,
        })
        .collect();

    Ok(MetricsOutput {
        lead_time_avg: flow_metrics.lead_time_avg,
        throughput: flow_metrics.throughput,
        wip: flow_metrics.wip,
        flow_efficiency: flow_metrics.flow_efficiency,
        completed_count: flow_metrics.completed_count,
        blocked_count: flow_metrics.blocked_count,
        bottlenecks,
    })
}

/// Generate cumulative flow diagram
#[verb]
fn diagram(
    #[arg(long, help = "Export as CSV")]
    csv: Option<String>,
    #[arg(long, help = "Export as JSON")]
    json: Option<String>,
    #[arg(long, help = "Path to flow diagram data", default_value = ".ggen/kanban-cfd.json")]
    diagram_path: String,
) -> NounVerbResult<FlowDiagramOutput> {
    use ggen_domain::kanban::flow_diagram::CumulativeFlowDiagram;

    let cfd = load_flow_diagram(&diagram_path)?;

    let trends: std::collections::HashMap<String, String> = cfd
        .stage_trends()
        .into_iter()
        .map(|(stage, trend)| (stage.as_str().to_string(), format!("{:?}", trend)))
        .collect();

    // Export CSV if requested
    let csv_path = if let Some(path) = csv {
        let csv_data = cfd.to_csv();
        std::fs::write(&path, csv_data).map_err(|e| {
            clap_noun_verb::NounVerbError::execution_error(format!("Failed to write CSV: {}", e))
        })?;
        Some(path)
    } else {
        None
    };

    // Export JSON if requested
    let json_path = if let Some(path) = json {
        let json_data = cfd.to_json().map_err(|e| {
            clap_noun_verb::NounVerbError::execution_error(format!("Failed to serialize JSON: {}", e))
        })?;
        std::fs::write(&path, json_data).map_err(|e| {
            clap_noun_verb::NounVerbError::execution_error(format!("Failed to write JSON: {}", e))
        })?;
        Some(path)
    } else {
        None
    };

    Ok(FlowDiagramOutput {
        data_points: cfd.get_data_points().len(),
        average_wip: cfd.average_wip(),
        trends,
        csv_path,
        json_path,
    })
}

/// Signal demand for code generation
#[verb]
fn signal(
    description: String,
    #[arg(long, help = "Priority: low, medium, high, critical", default_value = "medium")]
    priority: String,
    #[arg(long, help = "Signal type: request, schema_change, dependency, template_usage", default_value = "request")]
    signal_type: String,
    #[arg(long, help = "Path to demand system state", default_value = ".ggen/kanban-demand.json")]
    demand_path: String,
) -> NounVerbResult<DemandSignalOutput> {
    use ggen_domain::kanban::demand::{DemandSignal, DemandSignalType, DemandSource, PullSystem};
    use ggen_domain::kanban::card::Priority;

    let priority_enum = match priority.to_lowercase().as_str() {
        "low" => Priority::Low,
        "medium" => Priority::Medium,
        "high" => Priority::High,
        "critical" => Priority::Critical,
        _ => return Err(clap_noun_verb::NounVerbError::execution_error(
            "Invalid priority".to_string()
        )),
    };

    let signal_type_enum = match signal_type.to_lowercase().as_str() {
        "request" => DemandSignalType::ExplicitRequest {
            description: description.clone(),
            affected_templates: vec![],
        },
        "schema_change" => DemandSignalType::SchemaChange {
            change_type: description.clone(),
            ontology_uri: "http://ggen.dev/ontology#".to_string(),
        },
        _ => return Err(clap_noun_verb::NounVerbError::execution_error(
            "Invalid signal type".to_string()
        )),
    };

    let signal = DemandSignal::new(signal_type_enum, priority_enum, DemandSource::User);
    let signal_id = signal.id.clone();

    // Load or create pull system
    let mut pull_system = load_pull_system(&demand_path).unwrap_or_else(|_| PullSystem::new(3));
    pull_system.signal_demand(signal);

    save_pull_system(&demand_path, &pull_system)?;

    Ok(DemandSignalOutput {
        signal_id,
        signal_type,
        priority,
        pending_count: pull_system.pending_demand_count(),
    })
}

// ============================================================================
// Helper Functions
// ============================================================================

fn load_board(path: &str) -> NounVerbResult<ggen_domain::kanban::KanbanBoard> {
    let content = std::fs::read_to_string(path).map_err(|e| {
        clap_noun_verb::NounVerbError::execution_error(format!("Failed to read board: {}", e))
    })?;

    serde_json::from_str(&content).map_err(|e| {
        clap_noun_verb::NounVerbError::execution_error(format!("Failed to parse board: {}", e))
    })
}

fn save_board(path: &str, board: &ggen_domain::kanban::KanbanBoard) -> NounVerbResult<()> {
    let content = serde_json::to_string_pretty(board).map_err(|e| {
        clap_noun_verb::NounVerbError::execution_error(format!("Failed to serialize board: {}", e))
    })?;

    std::fs::write(path, content).map_err(|e| {
        clap_noun_verb::NounVerbError::execution_error(format!("Failed to write board: {}", e))
    })?;

    Ok(())
}

fn load_card_tracker(path: &str) -> NounVerbResult<ggen_domain::kanban::card::RdfChangeTracker> {
    let content = std::fs::read_to_string(path).map_err(|e| {
        clap_noun_verb::NounVerbError::execution_error(format!("Failed to read cards: {}", e))
    })?;

    serde_json::from_str(&content).map_err(|e| {
        clap_noun_verb::NounVerbError::execution_error(format!("Failed to parse cards: {}", e))
    })
}

fn save_card_tracker(path: &str, tracker: &ggen_domain::kanban::card::RdfChangeTracker) -> NounVerbResult<()> {
    let content = serde_json::to_string_pretty(tracker).map_err(|e| {
        clap_noun_verb::NounVerbError::execution_error(format!("Failed to serialize cards: {}", e))
    })?;

    // Create directory if needed
    if let Some(parent) = std::path::Path::new(path).parent() {
        std::fs::create_dir_all(parent).map_err(|e| {
            clap_noun_verb::NounVerbError::execution_error(format!("Failed to create directory: {}", e))
        })?;
    }

    std::fs::write(path, content).map_err(|e| {
        clap_noun_verb::NounVerbError::execution_error(format!("Failed to write cards: {}", e))
    })?;

    Ok(())
}

fn load_flow_diagram(path: &str) -> NounVerbResult<ggen_domain::kanban::flow_diagram::CumulativeFlowDiagram> {
    let content = std::fs::read_to_string(path).map_err(|e| {
        clap_noun_verb::NounVerbError::execution_error(format!("Failed to read flow diagram: {}", e))
    })?;

    serde_json::from_str(&content).map_err(|e| {
        clap_noun_verb::NounVerbError::execution_error(format!("Failed to parse flow diagram: {}", e))
    })
}

fn load_pull_system(path: &str) -> NounVerbResult<ggen_domain::kanban::demand::PullSystem> {
    let content = std::fs::read_to_string(path).map_err(|e| {
        clap_noun_verb::NounVerbError::execution_error(format!("Failed to read pull system: {}", e))
    })?;

    serde_json::from_str(&content).map_err(|e| {
        clap_noun_verb::NounVerbError::execution_error(format!("Failed to parse pull system: {}", e))
    })
}

fn save_pull_system(path: &str, pull_system: &ggen_domain::kanban::demand::PullSystem) -> NounVerbResult<()> {
    let content = serde_json::to_string_pretty(pull_system).map_err(|e| {
        clap_noun_verb::NounVerbError::execution_error(format!("Failed to serialize pull system: {}", e))
    })?;

    // Create directory if needed
    if let Some(parent) = std::path::Path::new(path).parent() {
        std::fs::create_dir_all(parent).map_err(|e| {
            clap_noun_verb::NounVerbError::execution_error(format!("Failed to create directory: {}", e))
        })?;
    }

    std::fs::write(path, content).map_err(|e| {
        clap_noun_verb::NounVerbError::execution_error(format!("Failed to write pull system: {}", e))
    })?;

    Ok(())
}

fn parse_stage(stage: &str) -> NounVerbResult<ggen_domain::kanban::WorkflowStage> {
    use ggen_domain::kanban::WorkflowStage;

    match stage.to_lowercase().as_str() {
        "backlog" => Ok(WorkflowStage::Backlog),
        "analysis" => Ok(WorkflowStage::Analysis),
        "transformation" => Ok(WorkflowStage::Transformation),
        "validation" => Ok(WorkflowStage::Validation),
        "generation" => Ok(WorkflowStage::Generation),
        "done" => Ok(WorkflowStage::Done),
        _ => Err(clap_noun_verb::NounVerbError::execution_error(
            format!("Invalid stage: {}. Use: backlog, analysis, transformation, validation, generation, done", stage)
        )),
    }
}
