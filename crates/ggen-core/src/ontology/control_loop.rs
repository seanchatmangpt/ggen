use serde::{Deserialize, Serialize};
/// Autonomous Control Loop: Closed-Loop Ontology Evolution
///
/// Implements the complete feedback loop:
/// Observe → Detect → Propose → Validate → Promote → Record → Repeat
///
/// Runs autonomously without human intervention in the editing loop.
use std::sync::Arc;
use std::time::Duration;
use tokio::sync::RwLock;

use crate::ontology::delta_proposer::DeltaSigmaProposer;
#[cfg(test)]
use crate::ontology::pattern_miner::ObservationSource;
use crate::ontology::pattern_miner::{MinerConfig, Observation, PatternMiner};
use crate::ontology::promotion::AtomicSnapshotPromoter;
use crate::ontology::sigma_runtime::{SigmaReceipt, SigmaRuntime, SigmaSnapshot};
use crate::ontology::validators::{CompositeValidator, Invariant, ValidationContext};

/// Telemetry for a control loop iteration
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct IterationTelemetry {
    pub iteration: usize,
    pub timestamp_ms: u64,
    pub observation_count: usize,
    pub patterns_detected: usize,
    pub proposals_generated: usize,
    pub proposals_validated: usize,
    pub proposals_promoted: usize,
    pub total_duration_ms: u64,
}

/// Control loop state machine
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum LoopState {
    Idle,
    Observing,
    Detecting,
    Proposing,
    Validating,
    Promoting,
    Recording,
    Error,
}

/// Autonomous control loop configuration
#[derive(Debug, Clone)]
pub struct ControlLoopConfig {
    /// Interval between iterations (milliseconds)
    pub iteration_interval_ms: u64,

    /// Max iterations before stopping (None = infinite)
    pub max_iterations: Option<usize>,

    /// Enable automatic promotion of valid proposals
    pub auto_promote: bool,

    /// Sector to evolve
    pub sector: String,

    /// Min confidence to proceed with proposal
    pub min_proposal_confidence: f64,

    /// Miner configuration
    pub miner_config: MinerConfig,
}

impl Default for ControlLoopConfig {
    fn default() -> Self {
        Self {
            iteration_interval_ms: 5000,
            max_iterations: None,
            auto_promote: true,
            sector: "support".to_string(),
            min_proposal_confidence: 0.75,
            miner_config: MinerConfig::default(),
        }
    }
}

/// The autonomous control loop
pub struct AutonomousControlLoop {
    config: ControlLoopConfig,
    state: Arc<RwLock<LoopState>>,
    sigma_runtime: Arc<RwLock<SigmaRuntime>>,
    promoter: Arc<AtomicSnapshotPromoter>,
    pattern_miner: Arc<RwLock<PatternMiner>>,
    proposer: Arc<dyn DeltaSigmaProposer>,
    validator: Arc<CompositeValidator>,
    telemetry: Arc<RwLock<Vec<IterationTelemetry>>>,
}

impl AutonomousControlLoop {
    pub fn new(
        config: ControlLoopConfig, initial_snapshot: SigmaSnapshot,
        proposer: Arc<dyn DeltaSigmaProposer>, validator: Arc<CompositeValidator>,
    ) -> Self {
        let sigma_runtime = SigmaRuntime::new(initial_snapshot.clone());
        let miner_config = config.miner_config.clone();

        Self {
            config,
            state: Arc::new(RwLock::new(LoopState::Idle)),
            sigma_runtime: Arc::new(RwLock::new(sigma_runtime)),
            promoter: Arc::new(AtomicSnapshotPromoter::new(Arc::new(initial_snapshot))),
            pattern_miner: Arc::new(RwLock::new(PatternMiner::new(miner_config))),
            proposer,
            validator,
            telemetry: Arc::new(RwLock::new(Vec::new())),
        }
    }

    /// Get current loop state
    pub async fn state(&self) -> LoopState {
        *self.state.read().await
    }

    /// Get telemetry
    pub async fn telemetry(&self) -> Vec<IterationTelemetry> {
        self.telemetry.read().await.clone()
    }

    /// Feed observation into the system
    pub async fn observe(&self, obs: Observation) {
        let mut miner = self.pattern_miner.write().await;
        miner.add_observation(obs);
    }

    /// Run one iteration of the control loop
    async fn iterate(&self) -> Result<IterationTelemetry, String> {
        let start_ms = get_time_ms();
        let mut telemetry = IterationTelemetry {
            iteration: self.telemetry.read().await.len(),
            timestamp_ms: start_ms,
            observation_count: 0,
            patterns_detected: 0,
            proposals_generated: 0,
            proposals_validated: 0,
            proposals_promoted: 0,
            total_duration_ms: 0,
        };

        // 1. OBSERVE: Already done via .observe() calls
        let miner = self.pattern_miner.read().await;
        telemetry.observation_count = miner.observation_count();
        drop(miner);

        // 2. DETECT: Run pattern mining
        *self.state.write().await = LoopState::Detecting;
        let mut miner = self.pattern_miner.write().await;
        let patterns = miner
            .mine()
            .map_err(|e| format!("Pattern mining failed: {}", e))?;
        telemetry.patterns_detected = patterns.len();
        drop(miner);

        if patterns.is_empty() {
            *self.state.write().await = LoopState::Recording;
            return Ok(telemetry);
        }

        // 3. PROPOSE: Generate ΔΣ² proposals
        *self.state.write().await = LoopState::Proposing;
        let current_snapshot = self
            .promoter
            .get_current()
            .map_err(|e| format!("Failed to get current snapshot: {}", e))?;
        let proposals = self
            .proposer
            .propose_deltas(patterns, current_snapshot.snapshot(), &self.config.sector)
            .await
            .map_err(|e| format!("Proposal generation failed: {}", e))?;

        telemetry.proposals_generated = proposals.len();

        // Filter by confidence
        let valid_proposals: Vec<_> = proposals
            .iter()
            .filter(|p| p.confidence >= self.config.min_proposal_confidence)
            .cloned()
            .collect();

        // 4. VALIDATE: Check invariants (Q)
        *self.state.write().await = LoopState::Validating;
        for proposal in &valid_proposals {
            let current_snap = self
                .promoter
                .get_current()
                .map_err(|e| format!("Failed to get current snapshot: {}", e))?;

            // Apply proposal changes to create new snapshot
            let mut new_triples = current_snap.snapshot().triples.as_ref().clone();

            // Remove triples (for now, just filter out matching subjects)
            for triple_pattern in &proposal.triples_to_remove {
                new_triples.retain(|stmt| !stmt.subject.contains(triple_pattern));
            }

            // Add new triples
            for triple_str in &proposal.triples_to_add {
                // Parse simple triple format: "subject predicate object"
                let parts: Vec<&str> = triple_str.split_whitespace().collect();
                if parts.len() >= 3 {
                    new_triples.push(crate::ontology::sigma_runtime::Statement {
                        subject: parts[0].to_string(),
                        predicate: parts[1].to_string(),
                        object: parts[2].to_string(),
                        graph: None,
                    });
                }
            }

            let new_snap = SigmaSnapshot::new(
                Some(current_snap.snapshot().id.clone()),
                new_triples,
                format!("{}_updated", current_snap.snapshot().version),
                "sig_updated".to_string(),
                current_snap.snapshot().metadata.clone(),
            );

            let ctx = ValidationContext {
                proposal: proposal.clone(),
                current_snapshot: current_snap.snapshot(),
                expected_new_snapshot: Arc::new(new_snap),
                sector: self.config.sector.clone(),
                invariants: vec![
                    Invariant::NoRetrocausation,
                    Invariant::TypeSoundness,
                    Invariant::SLOPreservation,
                ],
            };

            let (static_ev, dynamic_ev, perf_ev) = self
                .validator
                .validate_all(&ctx)
                .await
                .map_err(|e| format!("Validation failed: {}", e))?;

            telemetry.proposals_validated += 1;

            // Check if all validations passed
            if static_ev.passed && dynamic_ev.passed && perf_ev.passed {
                // 5. PROMOTE: Move to current
                *self.state.write().await = LoopState::Promoting;

                if self.config.auto_promote {
                    // Create the promoted snapshot with applied changes
                    let current_snap_for_promote = self.promoter.get_current().map_err(|e| {
                        format!("Failed to get current snapshot for promotion: {}", e)
                    })?;

                    let mut promoted_triples =
                        current_snap_for_promote.snapshot().triples.as_ref().clone();

                    // Apply the same changes for promotion
                    for triple_pattern in &proposal.triples_to_remove {
                        promoted_triples.retain(|stmt| !stmt.subject.contains(triple_pattern));
                    }

                    for triple_str in &proposal.triples_to_add {
                        let parts: Vec<&str> = triple_str.split_whitespace().collect();
                        if parts.len() >= 3 {
                            promoted_triples.push(crate::ontology::sigma_runtime::Statement {
                                subject: parts[0].to_string(),
                                predicate: parts[1].to_string(),
                                object: parts[2].to_string(),
                                graph: None,
                            });
                        }
                    }

                    let promoted_snapshot = SigmaSnapshot::new(
                        Some(current_snap_for_promote.snapshot().id.clone()),
                        promoted_triples,
                        format!(
                            "{}_v{}",
                            current_snap_for_promote.snapshot().version,
                            telemetry.iteration
                        ),
                        "promoted_sig".to_string(),
                        current_snap_for_promote.snapshot().metadata.clone(),
                    );

                    let _promotion_result = self
                        .promoter
                        .promote(Arc::new(promoted_snapshot))
                        .expect("Failed to promote snapshot");

                    telemetry.proposals_promoted += 1;

                    // 6. RECORD: Store receipt
                    let receipt = SigmaReceipt::new(
                        Default::default(),
                        Some(current_snap.snapshot().id.clone()),
                        format!("Proposal: {}", proposal.id),
                    );

                    let mut runtime = self.sigma_runtime.write().await;
                    runtime.record_receipt(receipt);
                }
            }
        }

        // 7. RECORD: Update telemetry
        *self.state.write().await = LoopState::Recording;
        telemetry.total_duration_ms = get_time_ms() - start_ms;

        Ok(telemetry)
    }

    /// Run the control loop
    pub async fn run(&self) -> Result<(), String> {
        *self.state.write().await = LoopState::Idle;

        let mut iteration = 0;
        loop {
            // Check max iterations
            if let Some(max) = self.config.max_iterations {
                if iteration >= max {
                    break;
                }
            }

            // Run iteration
            match self.iterate().await {
                Ok(telemetry) => {
                    self.telemetry.write().await.push(telemetry);
                }
                Err(e) => {
                    *self.state.write().await = LoopState::Error;
                    return Err(e);
                }
            }

            iteration += 1;

            // Wait before next iteration
            tokio::time::sleep(Duration::from_millis(self.config.iteration_interval_ms)).await;
        }

        *self.state.write().await = LoopState::Idle;
        Ok(())
    }

    /// Run with bounded iterations
    pub async fn run_bounded(&self, max_iters: usize) -> Result<(), String> {
        // Note: we can't modify self.config (it's not mutable), so we'll track manually

        for _ in 0..max_iters {
            match self.iterate().await {
                Ok(telemetry) => {
                    self.telemetry.write().await.push(telemetry);
                }
                Err(e) => {
                    *self.state.write().await = LoopState::Error;
                    return Err(e);
                }
            }

            tokio::time::sleep(Duration::from_millis(self.config.iteration_interval_ms)).await;
        }

        *self.state.write().await = LoopState::Idle;
        Ok(())
    }

    /// Get current snapshot
    pub fn current_snapshot(&self) -> Arc<SigmaSnapshot> {
        self.promoter
            .get_current()
            .expect("Failed to get current snapshot - lock poisoned")
            .snapshot()
    }
}

/// Get current time in milliseconds
fn get_time_ms() -> u64 {
    use std::time::{SystemTime, UNIX_EPOCH};

    SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .unwrap_or_default()
        .as_millis() as u64
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ontology::delta_proposer::MockLLMProposer;
    use crate::ontology::validators::{
        MockDynamicValidator, MockPerformanceValidator, MockStaticValidator,
    };

    fn create_test_loop() -> AutonomousControlLoop {
        let snapshot = SigmaSnapshot::new(
            None,
            vec![],
            "1.0.0".to_string(),
            "sig".to_string(),
            Default::default(),
        );

        let proposer: Arc<dyn DeltaSigmaProposer> =
            Arc::new(MockLLMProposer::new(Default::default()));

        let static_v: Arc<dyn crate::ontology::validators::StaticValidator> =
            Arc::new(MockStaticValidator);
        let dynamic_v: Arc<dyn crate::ontology::validators::DynamicValidator> =
            Arc::new(MockDynamicValidator);
        let perf_v: Arc<dyn crate::ontology::validators::PerformanceValidator> =
            Arc::new(MockPerformanceValidator::new(1000, 1024 * 100));

        let validator = Arc::new(CompositeValidator::new(static_v, dynamic_v, perf_v));

        let config = ControlLoopConfig {
            iteration_interval_ms: 100,
            ..Default::default()
        };

        AutonomousControlLoop::new(config, snapshot, proposer, validator)
    }

    #[tokio::test]
    async fn test_control_loop_creation() {
        let loop_sys = create_test_loop();
        assert_eq!(loop_sys.state().await, LoopState::Idle);
    }

    #[tokio::test]
    async fn test_control_loop_observation() {
        let loop_sys = create_test_loop();

        let obs = Observation {
            entity: "test_entity".to_string(),
            properties: [("type".to_string(), "test".to_string())]
                .iter()
                .cloned()
                .collect(),
            timestamp: 1000,
            source: ObservationSource::Data,
        };

        loop_sys.observe(obs).await;

        let miner = loop_sys.pattern_miner.read().await;
        assert_eq!(miner.observations.len(), 1);
    }

    #[tokio::test]
    async fn test_control_loop_iteration() {
        let loop_sys = create_test_loop();

        // Add some observations
        for i in 0..3 {
            let obs = Observation {
                entity: format!("entity_{}", i),
                properties: [("type".to_string(), "test".to_string())]
                    .iter()
                    .cloned()
                    .collect(),
                timestamp: 1000 + i as u64,
                source: ObservationSource::Data,
            };
            loop_sys.observe(obs).await;
        }

        // Run one iteration
        let telemetry = loop_sys.iterate().await.unwrap();
        assert_eq!(telemetry.observation_count, 3);
    }

    #[tokio::test]
    async fn test_control_loop_bounded_run() {
        let loop_sys = create_test_loop();

        // Add observations
        for i in 0..5 {
            let obs = Observation {
                entity: format!("entity_{}", i),
                properties: [("type".to_string(), "test".to_string())]
                    .iter()
                    .cloned()
                    .collect(),
                timestamp: 1000 + i as u64,
                source: ObservationSource::Data,
            };
            loop_sys.observe(obs).await;
        }

        // Run 1 iteration
        let result = loop_sys.run_bounded(1).await;
        assert!(result.is_ok());

        let telemetry = loop_sys.telemetry().await;
        assert_eq!(telemetry.len(), 1);
    }

    #[tokio::test]
    async fn test_control_loop_state_transitions() {
        let loop_sys = create_test_loop();

        // Add observations
        for i in 0..3 {
            let obs = Observation {
                entity: format!("entity_{}", i),
                properties: [("type".to_string(), "test".to_string())]
                    .iter()
                    .cloned()
                    .collect(),
                timestamp: 1000 + i as u64,
                source: ObservationSource::Data,
            };
            loop_sys.observe(obs).await;
        }

        loop_sys.iterate().await.unwrap();

        let final_state = loop_sys.state().await;
        assert_eq!(final_state, LoopState::Recording);
    }
}
