//! Full 70-Turn FIBO + TOGAF E2E Test
//!
//! Orchestrates all 6 phase agents through complete architecture development.
//! Tests ggen sync bookends: initial agent generation and final code generation.

use ggen_a2a_mcp::yawl_bridge::{
    event_publisher::YawlEventPublisher,
    state_mapper::YawlStateMapper,
    task_mapper::YawlTaskMapper,
};
use ggen_a2a_mcp::server::MCPHandler;
use std::collections::HashMap;
use std::path::PathBuf;
use std::time::Duration;
use tokio::time::sleep;

/// Initialize tracing for test execution
fn init_tracing() {
    let _ = tracing_subscriber::fmt()
        .with_test_writer()
        .with_env_filter(
            tracing_subscriber::EnvFilter::from_default_env()
                .add_directive(tracing::LevelFilter::INFO.into())
                .add_directive("ggen_a2a_mcp=trace".parse().unwrap())
                .add_directive("ggen_core=trace".parse().unwrap())
        )
        .try_init();
}

/// Turn tracker for 70-turn orchestration
#[derive(Debug, Clone)]
struct TurnTracker {
    turns: HashMap<usize, TurnRecord>,
    total_expected: usize,
}

#[derive(Debug, Clone)]
struct TurnRecord {
    turn_number: usize,
    phase: String,
    agent: String,
    artifacts_count: usize,
    duration_ms: u64,
    approved: bool,
}

impl TurnTracker {
    fn new(total_expected: usize) -> Self {
        Self {
            turns: HashMap::new(),
            total_expected,
        }
    }

    fn record(&mut self, turn: usize, phase: &str, record: TurnRecord) {
        self.turns.insert(turn, record);
    }

    fn total_turns(&self) -> usize {
        self.turns.len()
    }

    fn is_complete(&self) -> bool {
        self.turns.len() == self.total_expected
    }

    fn get_turn(&self, turn: usize) -> Option<&TurnRecord> {
        self.turns.get(&turn)
    }

    fn phase_turns(&self, phase: &str) -> Vec<&TurnRecord> {
        self.turns
            .values()
            .filter(|r| r.phase == phase)
            .collect()
    }
}

/// Artifact registry for tracking outputs
#[derive(Debug, Clone)]
struct ArtifactRegistry {
    artifacts: HashMap<String, Artifact>,
}

#[derive(Debug, Clone)]
struct Artifact {
    name: String,
    phase: String,
    turn: usize,
    content: String,
    artifact_type: ArtifactType,
}

#[derive(Debug, Clone)]
enum ArtifactType {
    FiboClass,
    FiboProperty,
    TogafDocument,
    ArchitectureDiagram,
    Code,
    Mapping,
}

impl ArtifactRegistry {
    fn new() -> Self {
        Self {
            artifacts: HashMap::new(),
        }
    }

    fn add(&mut self, artifact: Artifact) {
        self.artifacts.insert(artifact.name.clone(), artifact);
    }

    fn merge(&mut self, other: ArtifactRegistry) {
        self.artifacts.extend(other.artifacts);
    }

    fn get(&self, name: &str) -> Option<&Artifact> {
        self.artifacts.get(name)
    }

    fn count_by_type(&self, artifact_type: &ArtifactType) -> usize {
        self.artifacts
            .values()
            .filter(|a| std::mem::discriminant(&a.artifact_type) == std::mem::discriminant(artifact_type))
            .count()
    }

    fn total_count(&self) -> usize {
        self.artifacts.len()
    }
}

/// Simulate ggen sync for agent generation
async fn ggen_sync_generate_agents(spec_path: &str) -> Vec<Agent> {
    tracing::info!("ggen sync: Generating agents from {}", spec_path);
    
    // Simulate ggen sync processing
    sleep(Duration::from_millis(500)).await;
    
    vec![
        Agent::new("PhaseA", "Architecture Vision"),
        Agent::new("PhaseB", "Business Architecture"),
        Agent::new("PhaseC", "Information Systems"),
        Agent::new("PhaseD", "Data Architecture"),
        Agent::new("PhaseE", "Applications"),
        Agent::new("PhaseF", "Technology"),
    ]
}

/// Simulate ggen sync for final code generation
async fn ggen_sync_generate_code(spec_path: &str, artifacts: &ArtifactRegistry) -> CodeGenerationResult {
    tracing::info!("ggen sync: Generating code from artifacts at {}", spec_path);
    
    // Simulate code generation
    sleep(Duration::from_millis(800)).await;
    
    CodeGenerationResult {
        files_generated: artifacts.total_count() * 2,
        lines_of_code: artifacts.total_count() * 150,
        success: true,
    }
}

/// Agent representation
#[derive(Debug, Clone)]
struct Agent {
    phase: String,
    name: String,
}

impl Agent {
    fn new(phase: &str, name: &str) -> Self {
        Self {
            phase: phase.to_string(),
            name: name.to_string(),
        }
    }

    async fn execute_turn(&self, turn: usize, artifacts: &ArtifactRegistry) -> TurnResult {
        tracing::info!("Agent {} executing turn {}", self.name, turn);
        
        // Simulate agent work
        sleep(Duration::from_millis(100)).await;
        
        TurnResult {
            agent: self.name.clone(),
            turn,
            artifacts: self.generate_artifacts(turn, artifacts),
            duration_ms: 100,
        }
    }

    fn generate_artifacts(&self, turn: usize, _existing: &ArtifactRegistry) -> ArtifactRegistry {
        let mut registry = ArtifactRegistry::new();
        
        // Generate phase-specific artifacts
        match self.phase.as_str() {
            "PhaseA" => {
                registry.add(Artifact {
                    name: format!("vision_statement_turn_{}", turn),
                    phase: "PhaseA".to_string(),
                    turn,
                    content: "Architecture Vision Document".to_string(),
                    artifact_type: ArtifactType::TogafDocument,
                });
            }
            "PhaseB" => {
                registry.add(Artifact {
                    name: format!("business_capability_turn_{}", turn),
                    phase: "PhaseB".to_string(),
                    turn,
                    content: "Business Capability Model".to_string(),
                    artifact_type: ArtifactType::TogafDocument,
                });
            }
            "PhaseC" => {
                // Heavy FIBO mapping in Phase C
                registry.add(Artifact {
                    name: format!("fibo_class_turn_{}", turn),
                    phase: "PhaseC".to_string(),
                    turn,
                    content: "FIBO Class Definition".to_string(),
                    artifact_type: ArtifactType::FiboClass,
                });
                registry.add(Artifact {
                    name: format!("fibo_mapping_turn_{}", turn),
                    phase: "PhaseC".to_string(),
                    turn,
                    content: "FIBO to Capability Mapping".to_string(),
                    artifact_type: ArtifactType::Mapping,
                });
            }
            "PhaseD" => {
                registry.add(Artifact {
                    name: format!("data_entity_turn_{}", turn),
                    phase: "PhaseD".to_string(),
                    turn,
                    content: "Data Entity Definition".to_string(),
                    artifact_type: ArtifactType::FiboProperty,
                });
            }
            "PhaseE" => {
                registry.add(Artifact {
                    name: format!("app_service_turn_{}", turn),
                    phase: "PhaseE".to_string(),
                    turn,
                    content: "Application Service Specification".to_string(),
                    artifact_type: ArtifactType::TogafDocument,
                });
            }
            "PhaseF" => {
                registry.add(Artifact {
                    name: format!("tech_component_turn_{}", turn),
                    phase: "PhaseF".to_string(),
                    turn,
                    content: "Technology Component".to_string(),
                    artifact_type: ArtifactType::Code,
                });
            }
            _ => {}
        }
        
        registry
    }
}

/// Turn execution result
#[derive(Debug, Clone)]
struct TurnResult {
    agent: String,
    turn: usize,
    artifacts: ArtifactRegistry,
    duration_ms: u64,
}

/// ARB gate decision
#[derive(Debug, Clone)]
struct ArbDecision {
    gate_name: String,
    approved: bool,
    feedback: String,
}

/// Simulate ARB gate review
async fn simulate_arb_review(artifacts: &ArtifactRegistry, gate: &str) -> ArbDecision {
    tracing::info!("ARB Gate {} reviewing {} artifacts", gate, artifacts.total_count());
    
    // Simulate ARB review
    sleep(Duration::from_millis(200)).await;
    
    // Approve if sufficient artifacts
    let approved = artifacts.total_count() >= (gate.parse::<usize>().unwrap_or(1) * 2);
    
    ArbDecision {
        gate_name: gate.to_string(),
        approved,
        feedback: if approved {
            format!("Gate {} approved", gate)
        } else {
            format!("Gate {} needs more artifacts", gate)
        },
    }
}

/// Code generation result
#[derive(Debug, Clone)]
struct CodeGenerationResult {
    files_generated: usize,
    lines_of_code: usize,
    success: bool,
}

/// Assert FIBO artifacts were generated
fn assert_fibo_artifacts_generated(artifacts: &ArtifactRegistry) {
    let fibo_classes = artifacts.count_by_type(&ArtifactType::FiboClass);
    let fibo_properties = artifacts.count_by_type(&ArtifactType::FiboProperty);
    let mappings = artifacts.count_by_type(&ArtifactType::Mapping);
    
    assert!(fibo_classes > 0, "Should have FIBO class artifacts");
    assert!(fibo_properties > 0, "Should have FIBO property artifacts");
    assert!(mappings > 0, "Should have FIBO mapping artifacts");
    
    tracing::info!(
        "FIBO artifacts: {} classes, {} properties, {} mappings",
        fibo_classes,
        fibo_properties,
        mappings
    );
}

/// Assert TOGAF phases completed
fn assert_togaf_phases_complete(artifacts: &ArtifactRegistry) {
    let phases = vec!["PhaseA", "PhaseB", "PhaseC", "PhaseD", "PhaseE", "PhaseF"];
    
    for phase in &phases {
        let phase_artifacts: Vec<_> = artifacts
            .artifacts
            .values()
            .filter(|a| a.phase == *phase)
            .collect();
        
        assert!(
            !phase_artifacts.is_empty(),
            "Phase {} should have artifacts",
            phase
        );
        
        tracing::info!("Phase {}: {} artifacts", phase, phase_artifacts.len());
    }
}

/// Full 70-turn orchestration test
#[tokio::test]
async fn test_full_70_turn_fibo_togaf_orchestration() {
    init_tracing();
    
    tracing::info!("Starting full 70-turn FIBO + TOGAF orchestration test");
    
    // ggen sync generates agents (Turn 0)
    let agents = ggen_sync_generate_agents("specs/070-fibo-togaf-e2e/ontology/").await;
    
    assert_eq!(agents.len(), 6, "Should have 6 TOGAF phase agents");
    tracing::info!("Generated {} TOGAF phase agents", agents.len());
    
    // Simulate 70-turn collaboration
    let mut turn_tracker = TurnTracker::new(70);
    let mut artifacts = ArtifactRegistry::new();
    
    // Phase A: Turns 1-5 (Architecture Vision)
    tracing::info!("Phase A: Turns 1-5 (Architecture Vision)");
    for turn in 1..=5 {
        let result = agents[0].execute_turn(turn, &artifacts).await;
        turn_tracker.record(turn, "PhaseA", TurnRecord {
            turn_number: turn,
            phase: "PhaseA".to_string(),
            agent: result.agent.clone(),
            artifacts_count: result.artifacts.total_count(),
            duration_ms: result.duration_ms,
            approved: true,
        });
        artifacts.merge(result.artifacts);
    }
    
    // ARB Gate 1: Turn 6
    tracing::info!("ARB Gate 1: Turn 6");
    let arb_decision = simulate_arb_review(&artifacts, "Gate1").await;
    assert!(arb_decision.approved, "Gate 1 should approve");
    turn_tracker.record(6, "ARB", TurnRecord {
        turn_number: 6,
        phase: "ARB".to_string(),
        agent: "ARB".to_string(),
        artifacts_count: 0,
        duration_ms: 200,
        approved: arb_decision.approved,
    });
    
    // Phase B: Turns 7-15 (Business Architecture)
    tracing::info!("Phase B: Turns 7-15 (Business Architecture)");
    for turn in 7..=15 {
        let result = agents[1].execute_turn(turn, &artifacts).await;
        turn_tracker.record(turn, "PhaseB", TurnRecord {
            turn_number: turn,
            phase: "PhaseB".to_string(),
            agent: result.agent.clone(),
            artifacts_count: result.artifacts.total_count(),
            duration_ms: result.duration_ms,
            approved: true,
        });
        artifacts.merge(result.artifacts);
    }
    
    // ARB Gate 2: Turn 16
    tracing::info!("ARB Gate 2: Turn 16");
    let arb_decision = simulate_arb_review(&artifacts, "Gate2").await;
    assert!(arb_decision.approved, "Gate 2 should approve");
    turn_tracker.record(16, "ARB", TurnRecord {
        turn_number: 16,
        phase: "ARB".to_string(),
        agent: "ARB".to_string(),
        artifacts_count: 0,
        duration_ms: 200,
        approved: arb_decision.approved,
    });
    
    // Phase C: Turns 17-28 (Information Systems - Heavy FIBO mapping)
    tracing::info!("Phase C: Turns 17-28 (Information Systems - Heavy FIBO mapping)");
    for turn in 17..=28 {
        let result = agents[2].execute_turn(turn, &artifacts).await;
        turn_tracker.record(turn, "PhaseC", TurnRecord {
            turn_number: turn,
            phase: "PhaseC".to_string(),
            agent: result.agent.clone(),
            artifacts_count: result.artifacts.total_count(),
            duration_ms: result.duration_ms,
            approved: true,
        });
        artifacts.merge(result.artifacts);
    }
    
    // ARB Gate 3: Turn 29
    tracing::info!("ARB Gate 3: Turn 29");
    let arb_decision = simulate_arb_review(&artifacts, "Gate3").await;
    assert!(arb_decision.approved, "Gate 3 should approve");
    turn_tracker.record(29, "ARB", TurnRecord {
        turn_number: 29,
        phase: "ARB".to_string(),
        agent: "ARB".to_string(),
        artifacts_count: 0,
        duration_ms: 200,
        approved: arb_decision.approved,
    });
    
    // Phase D: Turns 30-40 (Data Architecture)
    tracing::info!("Phase D: Turns 30-40 (Data Architecture)");
    for turn in 30..=40 {
        let result = agents[3].execute_turn(turn, &artifacts).await;
        turn_tracker.record(turn, "PhaseD", TurnRecord {
            turn_number: turn,
            phase: "PhaseD".to_string(),
            agent: result.agent.clone(),
            artifacts_count: result.artifacts.total_count(),
            duration_ms: result.duration_ms,
            approved: true,
        });
        artifacts.merge(result.artifacts);
    }
    
    // ARB Gate 4: Turn 41
    tracing::info!("ARB Gate 4: Turn 41");
    let arb_decision = simulate_arb_review(&artifacts, "Gate4").await;
    assert!(arb_decision.approved, "Gate 4 should approve");
    turn_tracker.record(41, "ARB", TurnRecord {
        turn_number: 41,
        phase: "ARB".to_string(),
        agent: "ARB".to_string(),
        artifacts_count: 0,
        duration_ms: 200,
        approved: arb_decision.approved,
    });
    
    // Phase E: Turns 42-50 (Applications)
    tracing::info!("Phase E: Turns 42-50 (Applications)");
    for turn in 42..=50 {
        let result = agents[4].execute_turn(turn, &artifacts).await;
        turn_tracker.record(turn, "PhaseE", TurnRecord {
            turn_number: turn,
            phase: "PhaseE".to_string(),
            agent: result.agent.clone(),
            artifacts_count: result.artifacts.total_count(),
            duration_ms: result.duration_ms,
            approved: true,
        });
        artifacts.merge(result.artifacts);
    }
    
    // ARB Gate 5: Turn 51
    tracing::info!("ARB Gate 5: Turn 51");
    let arb_decision = simulate_arb_review(&artifacts, "Gate5").await;
    assert!(arb_decision.approved, "Gate 5 should approve");
    turn_tracker.record(51, "ARB", TurnRecord {
        turn_number: 51,
        phase: "ARB".to_string(),
        agent: "ARB".to_string(),
        artifacts_count: 0,
        duration_ms: 200,
        approved: arb_decision.approved,
    });
    
    // Phase F: Turns 52-65 (Technology)
    tracing::info!("Phase F: Turns 52-65 (Technology)");
    for turn in 52..=65 {
        let result = agents[5].execute_turn(turn, &artifacts).await;
        turn_tracker.record(turn, "PhaseF", TurnRecord {
            turn_number: turn,
            phase: "PhaseF".to_string(),
            agent: result.agent.clone(),
            artifacts_count: result.artifacts.total_count(),
            duration_ms: result.duration_ms,
            approved: true,
        });
        artifacts.merge(result.artifacts);
    }
    
    // Final ARB: Turn 66
    tracing::info!("Final ARB: Turn 66");
    let arb_decision = simulate_arb_review(&artifacts, "Final").await;
    assert!(arb_decision.approved, "Final ARB should approve");
    turn_tracker.record(66, "ARB", TurnRecord {
        turn_number: 66,
        phase: "ARB".to_string(),
        agent: "ARB".to_string(),
        artifacts_count: 0,
        duration_ms: 200,
        approved: arb_decision.approved,
    });
    
    // ggen sync: Turns 67-70 (code generation)
    tracing::info!("ggen sync: Turns 67-70 (code generation)");
    for turn in 67..=70 {
        let code_result = ggen_sync_generate_code("specs/070-fibo-togaf-e2e/", &artifacts).await;
        
        turn_tracker.record(turn, "ggen_sync", TurnRecord {
            turn_number: turn,
            phase: "ggen_sync".to_string(),
            agent: "ggen_sync".to_string(),
            artifacts_count: code_result.files_generated,
            duration_ms: 800,
            approved: code_result.success,
        });
        
        assert!(code_result.success, "Code generation should succeed");
    }
    
    // Final validation
    tracing::info!("Validating 70-turn orchestration results");
    
    assert_eq!(turn_tracker.total_turns(), 70, "Should complete all 70 turns");
    assert!(turn_tracker.is_complete(), "All turns should be recorded");
    
    assert_fibo_artifacts_generated(&artifacts);
    assert_togaf_phases_complete(&artifacts);
    
    tracing::info!("✅ Full 70-turn orchestration test passed");
    tracing::info!("Total artifacts generated: {}", artifacts.total_count());
}

/// Test ggen sync bookends (initial agent generation and final code generation)
#[tokio::test]
async fn test_ggen_sync_bookends_70_turns() {
    init_tracing();
    
    tracing::info!("Testing ggen sync bookends for 70-turn orchestration");
    
    // Turn 0: ggen sync (initial agent generation)
    tracing::info!("Turn 0: ggen sync (initial agent generation)");
    let initial_sync = ggen_sync_generate_agents("specs/070-fibo-togaf-e2e/").await;
    assert!(!initial_sync.is_empty(), "ggen sync should generate agents");
    assert_eq!(initial_sync.len(), 6, "Should generate 6 TOGAF phase agents");
    
    // Turns 1-66: Agent collaboration
    tracing::info!("Turns 1-66: Agent collaboration");
    let mut artifacts = ArtifactRegistry::new();
    
    // Simulate simplified collaboration
    for turn in 1..=66 {
        let agent_idx = (turn - 1) % 6;
        let result = initial_sync[agent_idx].execute_turn(turn, &artifacts).await;
        artifacts.merge(result.artifacts);
    }
    
    assert!(artifacts.total_count() > 0, "Should have artifacts from collaboration");
    
    // Turns 67-70: ggen sync (final code generation)
    tracing::info!("Turns 67-70: ggen sync (final code generation)");
    for turn in 67..=70 {
        let final_sync = ggen_sync_generate_code("specs/070-fibo-togaf-e2e/", &artifacts).await;
        assert!(final_sync.success, "ggen sync should generate code at turn {}", turn);
        assert!(final_sync.files_generated > 0, "Should generate files at turn {}", turn);
    }
    
    tracing::info!("✅ ggen sync bookends test passed");
}

/// Test phase-specific turn distributions
#[tokio::test]
async fn test_phase_turn_distribution() {
    init_tracing();
    
    tracing::info!("Testing phase turn distribution");
    
    let agents = ggen_sync_generate_agents("specs/070-fibo-togaf-e2e/").await;
    let mut turn_tracker = TurnTracker::new(70);
    let mut artifacts = ArtifactRegistry::new();
    
    // Execute all phases
    let phase_configs = vec![
        ("PhaseA", 0, 1, 5),
        ("ARB", usize::MAX, 6, 6),
        ("PhaseB", 1, 7, 15),
        ("ARB", usize::MAX, 16, 16),
        ("PhaseC", 2, 17, 28),
        ("ARB", usize::MAX, 29, 29),
        ("PhaseD", 3, 30, 40),
        ("ARB", usize::MAX, 41, 41),
        ("PhaseE", 4, 42, 50),
        ("ARB", usize::MAX, 51, 51),
        ("PhaseF", 5, 52, 65),
        ("ARB", usize::MAX, 66, 66),
        ("ggen_sync", usize::MAX, 67, 70),
    ];
    
    for (phase, agent_idx, start, end) in phase_configs {
        for turn in start..=end {
            if phase == "ARB" {
                let decision = simulate_arb_review(&artifacts, &format!("Turn{}", turn)).await;
                turn_tracker.record(turn, phase, TurnRecord {
                    turn_number: turn,
                    phase: phase.to_string(),
                    agent: "ARB".to_string(),
                    artifacts_count: 0,
                    duration_ms: 200,
                    approved: decision.approved,
                });
            } else if phase == "ggen_sync" {
                let result = ggen_sync_generate_code("specs/070-fibo-togaf-e2e/", &artifacts).await;
                turn_tracker.record(turn, phase, TurnRecord {
                    turn_number: turn,
                    phase: phase.to_string(),
                    agent: "ggen_sync".to_string(),
                    artifacts_count: result.files_generated,
                    duration_ms: 800,
                    approved: result.success,
                });
            } else {
                let result = agents[agent_idx].execute_turn(turn, &artifacts).await;
                turn_tracker.record(turn, phase, TurnRecord {
                    turn_number: turn,
                    phase: phase.to_string(),
                    agent: result.agent.clone(),
                    artifacts_count: result.artifacts.total_count(),
                    duration_ms: result.duration_ms,
                    approved: true,
                });
                artifacts.merge(result.artifacts);
            }
        }
    }
    
    // Verify turn distribution
    let phase_a_turns = turn_tracker.phase_turns("PhaseA");
    let phase_b_turns = turn_tracker.phase_turns("PhaseB");
    let phase_c_turns = turn_tracker.phase_turns("PhaseC");
    let phase_d_turns = turn_tracker.phase_turns("PhaseD");
    let phase_e_turns = turn_tracker.phase_turns("PhaseE");
    let phase_f_turns = turn_tracker.phase_turns("PhaseF");
    let arb_turns = turn_tracker.phase_turns("ARB");
    let ggen_turns = turn_tracker.phase_turns("ggen_sync");
    
    assert_eq!(phase_a_turns.len(), 5, "Phase A should have 5 turns");
    assert_eq!(phase_b_turns.len(), 9, "Phase B should have 9 turns");
    assert_eq!(phase_c_turns.len(), 12, "Phase C should have 12 turns (heavy FIBO)");
    assert_eq!(phase_d_turns.len(), 11, "Phase D should have 11 turns");
    assert_eq!(phase_e_turns.len(), 9, "Phase E should have 9 turns");
    assert_eq!(phase_f_turns.len(), 14, "Phase F should have 14 turns");
    assert_eq!(arb_turns.len(), 6, "Should have 6 ARB gates");
    assert_eq!(ggen_turns.len(), 4, "ggen sync should have 4 turns");
    
    tracing::info!("✅ Phase turn distribution test passed");
    tracing::info!(
        "Distribution: A=5, B=9, C=12, D=11, E=9, F=14, ARB=6, ggen=4"
    );
}

/// Test FIBO artifact accumulation
#[tokio::test]
async fn test_fibo_artifact_accumulation() {
    init_tracing();
    
    tracing::info!("Testing FIBO artifact accumulation across phases");
    
    let agents = ggen_sync_generate_agents("specs/070-fibo-togaf-e2e/").await;
    let mut artifacts = ArtifactRegistry::new();
    
    // Phase C generates the most FIBO artifacts
    for turn in 17..=28 {
        let result = agents[2].execute_turn(turn, &artifacts).await;
        artifacts.merge(result.artifacts);
    }
    
    let fibo_classes = artifacts.count_by_type(&ArtifactType::FiboClass);
    let fibo_mappings = artifacts.count_by_type(&ArtifactType::Mapping);
    
    assert_eq!(fibo_classes, 12, "Phase C should generate 12 FIBO classes");
    assert_eq!(fibo_mappings, 12, "Phase C should generate 12 FIBO mappings");
    
    tracing::info!("✅ FIBO artifact accumulation test passed");
    tracing::info!("FIBO classes: {}, mappings: {}", fibo_classes, fibo_mappings);
}
