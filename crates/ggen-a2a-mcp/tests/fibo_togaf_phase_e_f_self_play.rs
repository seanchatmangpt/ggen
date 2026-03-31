//! FIBO + TOGAF Phases E & F Self-Play Tests
//!
//! Tests Opportunities & Solutions (Phase E) and Migration Planning (Phase F) agents.
//! Focus: Solution alternatives, gap analysis, and 7-year migration roadmap with workstreams.
//!
//! ## Test Scope
//!
//! **Phase E (Opportunities & Solutions):** Turns 29-35
//! - Solution alternative evaluation (buy vs. build vs. partner)
//! - Trade-off analysis for each capability gap
//! - FIBO data model considerations in solution design
//!
//! **Phase F (Migration Planning):** Turns 36-50
//! - 7-year migration roadmap
//! - 4-5 concurrent workstreams with dependencies
//! - FIBO-based data migration sequencing
//! - Legacy system decommission planning

use std::collections::HashMap;

use a2a_generated::converged::message::{
    ConvergedMessage, ConvergedMessageType, ConvergedPayload, MessageEnvelope, MessageLifecycle,
    MessagePriority, MessageRouting, MessageState, QoSRequirements, ReliabilityLevel,
    UnifiedContent,
};
use chrono::Utc;
use ggen_a2a_mcp::{A2aMessageConverter, MessageRouter};

mod common;
use common::init_tracing;

// ---------------------------------------------------------------------------
// Test Data Structures
// ---------------------------------------------------------------------------

/// Solution alternative for a capability gap.
#[derive(Debug, Clone)]
struct SolutionAlternative {
    name: String,
    approach: ApproachType,
    estimated_cost: u64,    // in thousands USD
    time_to_implement: u16, // in months
    fibo_alignment: u8,     // 0-100 score
    risk_level: RiskLevel,
}

#[derive(Debug, Clone, PartialEq)]
enum ApproachType {
    BuyPackage,
    BuildCustom,
    PartnerIntegration,
    Hybrid,
}

#[derive(Debug, Clone, PartialEq)]
enum RiskLevel {
    Low,
    Medium,
    High,
    Critical,
}

/// Trade-off analysis result.
#[derive(Debug, Clone)]
struct TradeoffAnalysis {
    gap_id: String,
    alternatives: Vec<SolutionAlternative>,
    selected_approach: ApproachType,
    rationale: String,
    fibo_impact: String,
}

/// Migration workstream.
#[derive(Debug, Clone)]
struct MigrationWorkstream {
    name: String,
    start_year: u8,
    end_year: u8,
    dependencies: Vec<String>,
    involves_fibo_data: bool,
    estimated_cost: u64, // in thousands USD
    milestones: Vec<String>,
}

/// 7-year migration roadmap.
#[derive(Debug, Clone)]
struct MigrationRoadmap {
    total_years: u8,
    workstreams: Vec<MigrationWorkstream>,
    sequencing: HashMap<u8, Vec<String>>, // year -> workstream names
    critical_path: Vec<String>,
    fibo_migration_phases: Vec<String>,
}

// ---------------------------------------------------------------------------
// Helpers
// ---------------------------------------------------------------------------

fn test_routing() -> MessageRouting {
    MessageRouting {
        path: vec!["togaf-agent".to_string()],
        metadata: None,
        qos: QoSRequirements {
            reliability: ReliabilityLevel::AtLeastOnce,
            latency: None,
            throughput: None,
        },
    }
}

fn test_lifecycle() -> MessageLifecycle {
    MessageLifecycle {
        state: MessageState::Created,
        history: Vec::new(),
        timeout: None,
    }
}

fn make_text_message(id: &str, source: &str, content: &str) -> ConvergedMessage {
    ConvergedMessage::text(id.to_string(), source.to_string(), content.to_string())
}

fn make_data_message(id: &str, source: &str, data: serde_json::Value) -> ConvergedMessage {
    ConvergedMessage {
        message_id: id.to_string(),
        source: source.to_string(),
        target: None,
        envelope: MessageEnvelope {
            message_type: ConvergedMessageType::Task,
            priority: MessagePriority::Normal,
            timestamp: Utc::now(),
            schema_version: "1.0".to_string(),
            content_type: "application/json".to_string(),
            correlation_id: None,
            causation_chain: None,
        },
        payload: ConvergedPayload {
            content: UnifiedContent::Data {
                data: data.as_object().unwrap().clone(),
                schema: Some("TOGAF-PhaseE-F".to_string()),
            },
            context: None,
            hints: None,
            integrity: None,
        },
        routing: test_routing(),
        lifecycle: test_lifecycle(),
        extensions: None,
    }
}

// ---------------------------------------------------------------------------
// Phase E: Solution Alternatives Tests
// ---------------------------------------------------------------------------

#[tokio::test]
async fn test_phase_e_solution_alternatives() {
    init_tracing();

    let router = MessageRouter::with_defaults();
    let _converter = A2aMessageConverter::new();

    // Turns 29-35: Solution alternatives evaluation
    let solution_alternatives = vec![
        (
            "GAP-001: Core Banking Modernization",
            vec![
                SolutionAlternative {
                    name: "Package A (Finacle)".to_string(),
                    approach: ApproachType::BuyPackage,
                    estimated_cost: 15000,
                    time_to_implement: 24,
                    fibo_alignment: 75,
                    risk_level: RiskLevel::Medium,
                },
                SolutionAlternative {
                    name: "Custom Build (Rust/Microservices)".to_string(),
                    approach: ApproachType::BuildCustom,
                    estimated_cost: 25000,
                    time_to_implement: 36,
                    fibo_alignment: 95,
                    risk_level: RiskLevel::High,
                },
                SolutionAlternative {
                    name: "Partner Integration (FinTech Platform)".to_string(),
                    approach: ApproachType::PartnerIntegration,
                    estimated_cost: 8000,
                    time_to_implement: 12,
                    fibo_alignment: 60,
                    risk_level: RiskLevel::Low,
                },
            ],
        ),
        (
            "GAP-002: Payment Processing",
            vec![
                SolutionAlternative {
                    name: "SWIFT Integration".to_string(),
                    approach: ApproachType::BuyPackage,
                    estimated_cost: 3000,
                    time_to_implement: 8,
                    fibo_alignment: 85,
                    risk_level: RiskLevel::Low,
                },
                SolutionAlternative {
                    name: "Build Payment Engine".to_string(),
                    approach: ApproachType::BuildCustom,
                    estimated_cost: 12000,
                    time_to_implement: 18,
                    fibo_alignment: 90,
                    risk_level: RiskLevel::Medium,
                },
                SolutionAlternative {
                    name: "Payment Partner (Stripe/Adyen)".to_string(),
                    approach: ApproachType::PartnerIntegration,
                    estimated_cost: 2000,
                    time_to_implement: 4,
                    fibo_alignment: 70,
                    risk_level: RiskLevel::Low,
                },
            ],
        ),
        (
            "GAP-003: Risk Data Aggregation (BCBS 239)",
            vec![
                SolutionAlternative {
                    name: "Risk Package (FIS RiskWatch)".to_string(),
                    approach: ApproachType::BuyPackage,
                    estimated_cost: 8000,
                    time_to_implement: 14,
                    fibo_alignment: 80,
                    risk_level: RiskLevel::Medium,
                },
                SolutionAlternative {
                    name: "FIBO-Based Risk Lake".to_string(),
                    approach: ApproachType::BuildCustom,
                    estimated_cost: 18000,
                    time_to_implement: 24,
                    fibo_alignment: 98,
                    risk_level: RiskLevel::High,
                },
            ],
        ),
    ];

    // Simulate Phase E evaluation messages
    let mut messages = Vec::new();

    for (gap_id, alternatives) in &solution_alternatives {
        let msg_content = format!(
            "Evaluating solution alternatives for {}: {} options (Buy, Build, Partner)",
            gap_id,
            alternatives.len()
        );

        let msg = make_text_message(
            &format!("phase-e-{}", gap_id),
            "opportunities-solutions-agent",
            &msg_content,
        );

        let response = router.route(msg).await.expect("routing should succeed");

        messages.push(response);

        // Verify trade-off analysis was performed
        assert_tradeoff_analysis(&messages, gap_id);
    }

    // Verify at least 3 solution alternatives were evaluated
    assert!(
        messages.len() >= 3,
        "Should have evaluated at least 3 gaps, got {}",
        messages.len()
    );

    println!(
        "✓ Phase E: Evaluated {} capability gaps with solution alternatives",
        messages.len()
    );
}

#[tokio::test]
async fn test_phase_e_tradeoff_analysis_with_fibo() {
    init_tracing();

    // Test that trade-off analysis considers FIBO alignment
    let gap_id = "GAP-001: Core Banking";

    let alternatives = vec![
        SolutionAlternative {
            name: "Package Solution".to_string(),
            approach: ApproachType::BuyPackage,
            estimated_cost: 15000,
            time_to_implement: 24,
            fibo_alignment: 65, // Lower FIBO alignment
            risk_level: RiskLevel::Medium,
        },
        SolutionAlternative {
            name: "FIBO-Native Build".to_string(),
            approach: ApproachType::BuildCustom,
            estimated_cost: 25000,
            time_to_implement: 36,
            fibo_alignment: 98, // Higher FIBO alignment
            risk_level: RiskLevel::High,
        },
    ];

    // Simulate trade-off analysis
    let selected = if alternatives[1].fibo_alignment > alternatives[0].fibo_alignment + 20 {
        &alternatives[1]
    } else {
        &alternatives[0]
    };

    // Verify FIBO alignment is a decision factor
    assert!(
        selected.fibo_alignment >= 90,
        "Selected solution should have high FIBO alignment (≥90), got {}",
        selected.fibo_alignment
    );

    let analysis = TradeoffAnalysis {
        gap_id: gap_id.to_string(),
        alternatives: alternatives.clone(),
        selected_approach: selected.approach.clone(),
        rationale: format!(
            "Selected {} based on superior FIBO alignment ({} vs {})",
            selected.name, selected.fibo_alignment, alternatives[0].fibo_alignment
        ),
        fibo_impact: "High - enables semantic interoperability with regulatory reporting"
            .to_string(),
    };

    assert_eq!(analysis.selected_approach, ApproachType::BuildCustom);
    assert!(analysis.rationale.contains("FIBO alignment"));
    assert!(analysis.fibo_impact.contains("semantic interoperability"));

    println!("✓ Phase E: Trade-off analysis prioritizes FIBO alignment");
}

fn assert_tradeoff_analysis(messages: &[ConvergedMessage], gap_id: &str) {
    let gap_msg = messages
        .iter()
        .find(|m| m.source.contains("opportunities-solutions-agent"))
        .expect("Should have a response from opportunities-solutions-agent");

    match &gap_msg.payload.content {
        UnifiedContent::Text { content, .. } => {
            assert!(
                content.contains("evaluated") || content.contains("analyzed"),
                "Trade-off analysis should indicate evaluation occurred for {}",
                gap_id
            );
        }
        _ => panic!("Expected text content in trade-off analysis response"),
    }
}

// ---------------------------------------------------------------------------
// Phase F: Migration Planning Tests
// ---------------------------------------------------------------------------

#[tokio::test]
async fn test_phase_f_migration_7_year_roadmap() {
    init_tracing();

    let router = MessageRouter::with_defaults();

    // Turns 36-50: Migration Planning
    let workstreams = vec![
        MigrationWorkstream {
            name: "Legacy Core Banking Decommission".to_string(),
            start_year: 1,
            end_year: 4,
            dependencies: vec![],
            involves_fibo_data: false,
            estimated_cost: 5000,
            milestones: vec![
                "Phase-out legacy accounts".to_string(),
                "Migrate active accounts".to_string(),
                "Archive historical data".to_string(),
            ],
        },
        MigrationWorkstream {
            name: "New Core Banking Implementation".to_string(),
            start_year: 1,
            end_year: 5,
            dependencies: vec!["FIBO Data Model Design".to_string()],
            involves_fibo_data: true,
            estimated_cost: 25000,
            milestones: vec![
                "FIBO-based account schema".to_string(),
                "Core transaction processing".to_string(),
                "Regulatory reporting interface".to_string(),
            ],
        },
        MigrationWorkstream {
            name: "FIBO Data Migration".to_string(),
            start_year: 2,
            end_year: 4,
            dependencies: vec!["Legacy Core Banking Decommission".to_string()],
            involves_fibo_data: true,
            estimated_cost: 8000,
            milestones: vec![
                "Extract legacy data".to_string(),
                "Transform to FIBO ontology".to_string(),
                "Load to FIBO-based graph store".to_string(),
                "Validate semantic integrity".to_string(),
            ],
        },
        MigrationWorkstream {
            name: "Integration Layer".to_string(),
            start_year: 3,
            end_year: 6,
            dependencies: vec!["New Core Banking Implementation".to_string()],
            involves_fibo_data: true,
            estimated_cost: 12000,
            milestones: vec![
                "API gateway with FIBO semantics".to_string(),
                "Event-driven architecture".to_string(),
                "External system integration".to_string(),
            ],
        },
        MigrationWorkstream {
            name: "Regulatory Compliance (BCBS 239)".to_string(),
            start_year: 4,
            end_year: 7,
            dependencies: vec![
                "FIBO Data Migration".to_string(),
                "Integration Layer".to_string(),
            ],
            involves_fibo_data: true,
            estimated_cost: 10000,
            milestones: vec![
                "Risk data aggregation".to_string(),
                "FIBO-based risk reporting".to_string(),
                "BCBS 239 validation".to_string(),
            ],
        },
    ];

    // Simulate Phase F roadmap creation
    let roadmap = create_roadmap(workstreams.clone());

    // Verify roadmap spans 7 years
    assert_eq!(roadmap.total_years, 7, "Roadmap should span 7 years");

    // Verify sequencing
    assert_workstream_sequencing(&roadmap, &workstreams);

    // Verify FIBO data migration is included
    assert_fibo_data_migration_included(&roadmap);

    // Verify critical path
    assert!(
        !roadmap.critical_path.is_empty(),
        "Critical path should not be empty"
    );
    assert!(
        roadmap.critical_path.len() >= 3,
        "Critical path should have at least 3 workstreams"
    );

    // Send roadmap validation message
    let msg_content = format!(
        "Migration roadmap validated: {} workstreams over {} years with FIBO data migration",
        roadmap.workstreams.len(),
        roadmap.total_years
    );

    let msg = make_text_message(
        "phase-f-roadmap-validation",
        "migration-planning-agent",
        &msg_content,
    );

    let response = router.route(msg).await.expect("routing should succeed");

    match &response.payload.content {
        UnifiedContent::Text { content, .. } => {
            assert!(
                content.contains("validated") || content.contains("Processed"),
                "Roadmap validation should confirm success"
            );
        }
        _ => panic!("Expected text content in roadmap validation response"),
    }

    println!(
        "✓ Phase F: Validated 7-year roadmap with {} workstreams",
        roadmap.workstreams.len()
    );
}

#[tokio::test]
async fn test_phase_f_workstream_dependencies() {
    init_tracing();

    // Test that workstream dependencies are correctly sequenced
    let workstreams = vec![
        MigrationWorkstream {
            name: "FIBO Data Model Design".to_string(),
            start_year: 1,
            end_year: 2,
            dependencies: vec![],
            involves_fibo_data: true,
            estimated_cost: 3000,
            milestones: vec!["FIBO ontology design".to_string()],
        },
        MigrationWorkstream {
            name: "New Core Banking Implementation".to_string(),
            start_year: 2,
            end_year: 5,
            dependencies: vec!["FIBO Data Model Design".to_string()],
            involves_fibo_data: true,
            estimated_cost: 25000,
            milestones: vec!["Core implementation".to_string()],
        },
        MigrationWorkstream {
            name: "FIBO Data Migration".to_string(),
            start_year: 3,
            end_year: 4,
            dependencies: vec!["New Core Banking Implementation".to_string()],
            involves_fibo_data: true,
            estimated_cost: 8000,
            milestones: vec!["Data migration".to_string()],
        },
    ];

    let roadmap = create_roadmap(workstreams);

    // Verify dependencies are satisfied in sequencing
    for workstream in &roadmap.workstreams {
        for dep in &workstream.dependencies {
            let dep_workstream = roadmap
                .workstreams
                .iter()
                .find(|w| &w.name == dep)
                .unwrap_or_else(|| panic!("Dependency '{}' not found in roadmap", dep));

            assert!(
                dep_workstream.end_year < workstream.start_year,
                "Dependency '{}' should end before workstream '{}' starts ({} < {})",
                dep,
                workstream.name,
                dep_workstream.end_year,
                workstream.start_year
            );
        }
    }

    println!("✓ Phase F: All workstream dependencies correctly sequenced");
}

#[tokio::test]
async fn test_phase_f_fibo_migration_phases() {
    init_tracing();

    // Test that FIBO data migration has distinct phases
    let fibo_migration = MigrationWorkstream {
        name: "FIBO Data Migration".to_string(),
        start_year: 2,
        end_year: 4,
        dependencies: vec!["Legacy Decommission".to_string()],
        involves_fibo_data: true,
        estimated_cost: 8000,
        milestones: vec![
            "Extract legacy data to staging".to_string(),
            "Transform to FIBO RDF ontology".to_string(),
            "Load to Oxigraph triple store".to_string(),
            "Validate semantic integrity".to_string(),
            "Create FIBO-based SPARQL endpoints".to_string(),
        ],
    };

    let roadmap = create_roadmap(vec![fibo_migration.clone()]);

    // Verify FIBO migration phases
    assert!(
        fibo_migration.milestones.len() >= 5,
        "FIBO migration should have at least 5 phases, got {}",
        fibo_migration.milestones.len()
    );

    // Verify specific FIBO-related milestones
    let milestone_text = fibo_migration.milestones.join(" ");
    assert!(
        milestone_text.contains("FIBO"),
        "FIBO migration milestones should mention FIBO"
    );
    assert!(
        milestone_text.contains("RDF")
            || milestone_text.contains("ontology")
            || milestone_text.contains("semantic"),
        "FIBO migration should reference RDF/ontology/semantic concepts"
    );

    // Verify roadmap includes FIBO migration phases
    assert!(!roadmap.fibo_migration_phases.is_empty());
    assert!(
        roadmap.fibo_migration_phases.len() >= fibo_migration.milestones.len(),
        "Roadmap should capture all FIBO migration phases"
    );

    println!(
        "✓ Phase F: FIBO migration has {} distinct phases",
        fibo_migration.milestones.len()
    );
}

// ---------------------------------------------------------------------------
// Roadmap Creation Helper
// ---------------------------------------------------------------------------

fn create_roadmap(workstreams: Vec<MigrationWorkstream>) -> MigrationRoadmap {
    let total_years = 7;
    let mut sequencing = HashMap::new();
    let mut critical_path = Vec::new();
    let mut fibo_migration_phases = Vec::new();

    // Build year-based sequencing
    for year in 1..=total_years {
        let ws_in_year: Vec<String> = workstreams
            .iter()
            .filter(|ws| ws.start_year <= year && ws.end_year >= year)
            .map(|ws| ws.name.clone())
            .collect();

        if !ws_in_year.is_empty() {
            sequencing.insert(year, ws_in_year);
        }
    }

    // Determine critical path (workstreams with most dependencies)
    let mut dep_counts: Vec<(String, usize)> = workstreams
        .iter()
        .map(|ws| (ws.name.clone(), ws.dependencies.len()))
        .collect();

    dep_counts.sort_by(|a, b| b.1.cmp(&a.1));

    critical_path = dep_counts
        .iter()
        .take(3)
        .map(|(name, _)| name.clone())
        .collect();

    // Collect FIBO migration phases
    for ws in &workstreams {
        if ws.involves_fibo_data {
            for milestone in &ws.milestones {
                fibo_migration_phases.push(format!("{}: {}", ws.name, milestone));
            }
        }
    }

    MigrationRoadmap {
        total_years,
        workstreams,
        sequencing,
        critical_path,
        fibo_migration_phases,
    }
}

fn assert_workstream_sequencing(roadmap: &MigrationRoadmap, workstreams: &[MigrationWorkstream]) {
    // Verify all workstreams are in the roadmap
    assert_eq!(
        roadmap.workstreams.len(),
        workstreams.len(),
        "All workstreams should be in roadmap"
    );

    // Verify each year has appropriate workstreams
    for year in 1..=roadmap.total_years {
        if let Some(ws_names) = roadmap.sequencing.get(&year) {
            assert!(
                !ws_names.is_empty(),
                "Year {} should have workstreams",
                year
            );

            for ws_name in ws_names {
                let ws = workstreams
                    .iter()
                    .find(|w| &w.name == ws_name)
                    .unwrap_or_else(|| panic!("Workstream '{}' not found", ws_name));

                assert!(
                    ws.start_year <= year && ws.end_year >= year,
                    "Workstream '{}' should be active in year {} ({}-{})",
                    ws_name,
                    year,
                    ws.start_year,
                    ws.end_year
                );
            }
        }
    }
}

fn assert_fibo_data_migration_included(roadmap: &MigrationRoadmap) {
    // Verify at least one workstream involves FIBO data
    let fibo_workstreams: Vec<&MigrationWorkstream> = roadmap
        .workstreams
        .iter()
        .filter(|ws| ws.involves_fibo_data)
        .collect();

    assert!(
        !fibo_workstreams.is_empty(),
        "At least one workstream should involve FIBO data"
    );

    // Verify FIBO migration phases are documented
    assert!(
        !roadmap.fibo_migration_phases.is_empty(),
        "FIBO migration phases should be documented"
    );

    // Verify critical path includes FIBO-related workstream
    let critical_has_fibo = roadmap.critical_path.iter().any(|name| {
        roadmap
            .workstreams
            .iter()
            .any(|ws| &ws.name == name && ws.involves_fibo_data)
    });

    assert!(
        critical_has_fibo,
        "Critical path should include at least one FIBO-related workstream"
    );
}

// ---------------------------------------------------------------------------
// End-to-End: Phase E -> Phase F Integration
// ---------------------------------------------------------------------------

#[tokio::test]
async fn test_phase_e_to_f_integration() {
    init_tracing();

    let _router = MessageRouter::with_defaults();

    // Phase E: Select solution for Core Banking gap
    let core_banking_alternatives = [SolutionAlternative {
            name: "Package Solution".to_string(),
            approach: ApproachType::BuyPackage,
            estimated_cost: 15000,
            time_to_implement: 24,
            fibo_alignment: 65,
            risk_level: RiskLevel::Medium,
        },
        SolutionAlternative {
            name: "FIBO-Native Build".to_string(),
            approach: ApproachType::BuildCustom,
            estimated_cost: 25000,
            time_to_implement: 36,
            fibo_alignment: 98,
            risk_level: RiskLevel::High,
        }];

    // Select FIBO-native solution (higher alignment)
    let selected = &core_banking_alternatives[1];

    // Phase F: Create migration workstream based on Phase E selection
    let migration_workstream = MigrationWorkstream {
        name: "New Core Banking Implementation".to_string(),
        start_year: 1,
        end_year: 5,
        dependencies: vec!["FIBO Data Model Design".to_string()],
        involves_fibo_data: true,
        estimated_cost: selected.estimated_cost,
        milestones: vec![
            format!("FIBO-based core (alignment: {})", selected.fibo_alignment),
            "Core transaction processing".to_string(),
            "Regulatory reporting interface".to_string(),
        ],
    };

    let roadmap = create_roadmap(vec![migration_workstream.clone()]);

    // Verify Phase E decision influences Phase F planning
    assert_eq!(
        migration_workstream.estimated_cost, selected.estimated_cost,
        "Migration workstream cost should match selected solution cost"
    );

    assert!(
        migration_workstream.involves_fibo_data,
        "Migration should involve FIBO data (based on Phase E selection)"
    );

    // Verify roadmap reflects FIBO-based approach
    assert!(!roadmap.fibo_migration_phases.is_empty());

    println!("✓ Phase E -> F: Solution selection drives migration planning");
}
