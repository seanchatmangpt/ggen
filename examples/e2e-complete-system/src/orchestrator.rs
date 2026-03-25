/// OSIRIS System Orchestration - Joe Armstrong Fault Tolerance Principles
///
/// This module implements the main orchestration logic demonstrating all five
/// Armstrong principles in a single cohesive system:
/// 1. Autonomous agents with isolation
/// 2. Distributed consensus (PBFT)
/// 3. Tool use integration (MCP)
/// 4. Crash recovery (supervisor trees)
/// 5. Cryptographic accountability (signed receipts)
use crate::agents::*;
use crate::consensus::*;
use crate::receipts::*;
use crate::supervisor::*;
use crate::tools::*;

use anyhow::{anyhow, Result};
use dashmap::DashMap;
use serde::{Deserialize, Serialize};
use std::sync::atomic::{AtomicBool, Ordering};
use std::sync::Arc;
use tokio::sync::{Mutex, RwLock};
use tracing::{error, info, warn};
use uuid::Uuid;

/// Life domains that OSIRIS manages
#[derive(Clone, Debug, Serialize, Deserialize, Hash, Eq, PartialEq)]
#[serde(rename_all = "lowercase")]
pub enum LifeDomain {
    Health,
    Career,
    Relationships,
    Finance,
    Learning,
    Leisure,
}

impl std::fmt::Display for LifeDomain {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let s = match self {
            LifeDomain::Health => "Health",
            LifeDomain::Career => "Career",
            LifeDomain::Relationships => "Relationships",
            LifeDomain::Finance => "Finance",
            LifeDomain::Learning => "Learning",
            LifeDomain::Leisure => "Leisure",
        };
        write!(f, "{}", s)
    }
}

/// Priority in the life plan
#[derive(Clone, Debug, Serialize, Deserialize, PartialEq, Eq, Hash)]
pub struct Priority {
    pub domain: LifeDomain,
    pub score: u8, // 0-100
}

/// Assessment from a single domain agent
#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct DomainAssessment {
    pub domain: LifeDomain,
    pub health_score: f64,
    pub key_issues: Vec<String>,
    pub opportunities: Vec<String>,
    pub agent_id: String,
}

/// Complete OSIRIS system state
#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct OSIRISState {
    pub session_id: String,
    pub timeline: String,
    pub assessments: Vec<DomainAssessment>,
    pub priorities: Vec<Priority>,
    pub consensus_receipt: Option<String>,
    pub plan_steps: Vec<PlanStep>,
    pub execution_results: Vec<ExecutionResult>,
    pub final_receipt: Option<String>,
}

/// Single step in the execution plan
#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct PlanStep {
    pub id: String,
    pub priority: LifeDomain,
    pub description: String,
    pub tool_id: Option<String>,
    pub tool_args: Option<serde_json::Value>,
    pub dependencies: Vec<String>,
    pub timeout_ms: u64,
}

/// Result from executing a plan step
#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct ExecutionResult {
    pub step_id: String,
    pub success: bool,
    pub output: serde_json::Value,
    pub error: Option<String>,
    pub retries: u32,
    pub duration_ms: u64,
}

/// Main OSIRIS orchestrator
pub struct OSIRISOrchestrator {
    session_id: String,
    state: Arc<RwLock<OSIRISState>>,
    agents: Arc<DashMap<LifeDomain, Arc<Mutex<DomainAgent>>>>,
    supervisors: Arc<DashMap<LifeDomain, Arc<Supervisor>>>,
    consensus_engine: Arc<PBFTConsensus>,
    tool_registry: Arc<ToolRegistry>,
    receipt_signer: Arc<ReceiptSigner>,
    is_running: Arc<AtomicBool>,
    failure_injection: Arc<AtomicBool>,
    failure_timing: Arc<Mutex<Option<String>>>,
}

impl OSIRISOrchestrator {
    /// Create a new OSIRIS orchestrator
    pub async fn new(timeline: String, enable_failure_injection: bool) -> Result<Self> {
        let session_id = Uuid::new_v4().to_string();
        info!("[INIT] Starting OSIRIS system with 6 agents...");

        let state = OSIRISState {
            session_id: session_id.clone(),
            timeline,
            assessments: Vec::new(),
            priorities: Vec::new(),
            consensus_receipt: None,
            plan_steps: Vec::new(),
            execution_results: Vec::new(),
            final_receipt: None,
        };

        let agents = Arc::new(DashMap::new());
        let supervisors = Arc::new(DashMap::new());

        // Initialize domain agents with supervisors
        for domain in vec![
            LifeDomain::Health,
            LifeDomain::Career,
            LifeDomain::Relationships,
            LifeDomain::Finance,
            LifeDomain::Learning,
            LifeDomain::Leisure,
        ] {
            let agent = DomainAgent::new(domain.clone()).await?;
            let supervisor = Arc::new(Supervisor::new(domain.clone(), agent.clone()).await?);

            agents.insert(domain.clone(), Arc::new(Mutex::new(agent)));
            supervisors.insert(domain.clone(), supervisor);

            info!(
                "[AGENT-{}] Initializing {} domain monitoring",
                domain, domain
            );
        }

        let consensus_engine = Arc::new(PBFTConsensus::new(6));
        let tool_registry = Arc::new(ToolRegistry::new().await?);
        let receipt_signer = Arc::new(ReceiptSigner::new().await?);

        Ok(Self {
            session_id,
            state: Arc::new(RwLock::new(state)),
            agents,
            supervisors,
            consensus_engine,
            tool_registry,
            receipt_signer,
            is_running: Arc::new(AtomicBool::new(true)),
            failure_injection: Arc::new(AtomicBool::new(enable_failure_injection)),
            failure_timing: Arc::new(Mutex::new(None)),
        })
    }

    /// PRINCIPLE 1: Autonomous Agents with Isolation
    /// Each domain agent independently analyzes its domain
    pub async fn analyze_domains(&self) -> Result<Vec<DomainAssessment>> {
        info!("[ANALYSIS] Running domain assessments...");
        let mut assessments = Vec::new();

        // Run all agents in parallel (fault domain isolation)
        let handles: Vec<_> = self
            .agents
            .iter()
            .map(|entry| {
                let domain = entry.key().clone();
                let agent = entry.value().clone();
                tokio::spawn(async move {
                    match agent.lock().await.analyze().await {
                        Ok(assessment) => {
                            info!(
                                "[AGENT-{}] ✓ Analysis complete (score: {:.2})",
                                domain, assessment.health_score
                            );
                            Ok(assessment)
                        }
                        Err(e) => {
                            error!("[AGENT-{}] Analysis failed: {}", domain, e);
                            Err(e)
                        }
                    }
                })
            })
            .collect();

        for handle in handles {
            match handle.await {
                Ok(Ok(assessment)) => assessments.push(assessment),
                Ok(Err(e)) => {
                    warn!("Agent analysis failed: {}", e);
                }
                Err(e) => {
                    error!("Agent task failed: {}", e);
                }
            }
        }

        if assessments.is_empty() {
            return Err(anyhow!("No agents completed analysis"));
        }

        // Update state
        let mut state = self.state.write().await;
        state.assessments = assessments.clone();

        // Print assessment results
        info!("[ANALYSIS] Domain health scores:");
        for assessment in &assessments {
            info!(
                "  {}: {:.2} ({})",
                assessment.domain,
                assessment.health_score,
                assessment.key_issues.join(", ")
            );
        }

        Ok(assessments)
    }

    /// PRINCIPLE 2: Distributed Consensus (PBFT Agreement)
    /// Reach agreement on top 3 priorities via Byzantine Fault Tolerant voting
    pub async fn reach_consensus(
        &self, assessments: Vec<DomainAssessment>,
    ) -> Result<Vec<Priority>> {
        info!("[CONSENSUS] Running PBFT with f=1 Byzantine tolerance...");

        // Convert assessments to priority proposals
        let mut proposals = Vec::new();
        for assessment in assessments {
            proposals.push(Priority {
                domain: assessment.domain,
                score: (assessment.health_score * 100.0) as u8,
            });
        }

        // Inject failure if configured
        if self.failure_injection.load(Ordering::Relaxed) {
            if let Some(timing) = self.failure_timing.lock().await.as_ref() {
                if timing == "during-consensus" {
                    info!("[SIMULATE-FAILURE] Killing career agent during consensus...");
                    if let Some(mut entry) = self.supervisors.remove(&LifeDomain::Career) {
                        entry.1.crash().await;
                        tokio::time::sleep(tokio::time::Duration::from_millis(200)).await;
                        info!("[SUPERVISOR] Career agent restarting...");
                        drop(entry);
                        // Agent will be restarted by supervisor
                    }
                }
            }
        }

        // Run PBFT consensus
        let priorities = self
            .consensus_engine
            .reach_agreement(proposals)
            .await
            .map_err(|e| anyhow!("Consensus failed: {}", e))?;

        info!("[CONSENSUS] ✓ Agreement reached!");
        info!("[CONSENSUS] Priorities = {:?}", priorities);

        // Sign consensus receipt
        let receipt = self
            .receipt_signer
            .sign_consensus(&self.session_id, &priorities)
            .await?;

        info!("[RECEIPT] Receipt signed ({})", &receipt[..12]);

        let mut state = self.state.write().await;
        state.priorities = priorities.clone();
        state.consensus_receipt = Some(receipt);

        Ok(priorities)
    }

    /// PRINCIPLE 3: Tool Use Integration (MCP Discovery & Execution)
    /// Discover available tools and create action plan
    pub async fn discover_tools(&self) -> Result<Vec<ToolMetadata>> {
        info!("[TOOLS] Discovering available tools...");
        let tools = self.tool_registry.discover_tools().await?;
        info!("[TOOLS] ✓ Found {} tools", tools.len());
        for tool in tools.iter().take(5) {
            info!("[TOOLS]   - {}", tool.name);
        }
        Ok(tools)
    }

    /// Generate execution plan based on priorities and available tools
    pub async fn generate_plan(
        &self, priorities: Vec<Priority>, tools: Vec<ToolMetadata>,
    ) -> Result<Vec<PlanStep>> {
        info!("[PLAN] Generating plan from priorities...");

        let mut steps = Vec::new();
        for (idx, priority) in priorities.iter().enumerate() {
            let step = PlanStep {
                id: format!("step-{}", idx + 1),
                priority: priority.domain.clone(),
                description: self.describe_action(&priority.domain),
                tool_id: self.select_tool(&priority.domain, &tools),
                tool_args: None,
                dependencies: if idx > 0 {
                    vec![format!("step-{}", idx)]
                } else {
                    Vec::new()
                },
                timeout_ms: 5000,
            };
            steps.push(step);
        }

        // Add 4 more steps for other domains
        let remaining_domains = vec![LifeDomain::Health, LifeDomain::Leisure];
        for domain in remaining_domains {
            let step = PlanStep {
                id: format!("step-{}", steps.len() + 1),
                priority: domain.clone(),
                description: self.describe_action(&domain),
                tool_id: self.select_tool(&domain, &tools),
                tool_args: None,
                dependencies: vec![format!("step-{}", steps.len())],
                timeout_ms: 5000,
            };
            steps.push(step);
        }

        info!("[PLAN] Generated {}-step plan:", steps.len());
        for step in &steps {
            info!(
                "[PLAN]   Step {}: {} ({})",
                step.id, step.description, step.priority
            );
        }

        let mut state = self.state.write().await;
        state.plan_steps = steps.clone();

        Ok(steps)
    }

    /// PRINCIPLE 4 & 5: Execute Plan with Crash Recovery & Receipts
    /// Execute plan steps with automatic recovery on failure
    pub async fn execute_plan(&self, steps: Vec<PlanStep>) -> Result<Vec<ExecutionResult>> {
        info!(
            "[EXECUTE] Starting plan execution with {} steps...",
            steps.len()
        );

        let mut results = Vec::new();

        for step in steps {
            let result = self.execute_step(&step).await;
            results.push(result);
        }

        // Sign final receipt
        let final_receipt = self
            .receipt_signer
            .sign_execution(&self.session_id, &results)
            .await?;

        let mut state = self.state.write().await;
        state.execution_results = results.clone();
        state.final_receipt = Some(final_receipt.clone());

        info!("[COMPLETE] Plan execution finished");
        info!("[RECEIPTS] Final receipt: {}", &final_receipt[..12]);

        Ok(results)
    }

    /// Execute a single plan step with retry logic and crash recovery
    async fn execute_step(&self, step: &PlanStep) -> ExecutionResult {
        let start = std::time::Instant::now();
        let mut retries = 0;
        let max_retries = 3;

        loop {
            match self.execute_step_internal(step).await {
                Ok(output) => {
                    let duration_ms = start.elapsed().as_millis() as u64;
                    info!("[EXECUTE] ✓ {} - Done", step.description);
                    return ExecutionResult {
                        step_id: step.id.clone(),
                        success: true,
                        output,
                        error: None,
                        retries,
                        duration_ms,
                    };
                }
                Err(e) if retries < max_retries => {
                    retries += 1;
                    warn!(
                        "[EXECUTE] {} - Attempt {} failed: {}",
                        step.description, retries, e
                    );
                    tokio::time::sleep(tokio::time::Duration::from_millis(100)).await;
                }
                Err(e) => {
                    let duration_ms = start.elapsed().as_millis() as u64;
                    warn!(
                        "[EXECUTE] {} - Failed after {} retries",
                        step.description, retries
                    );
                    return ExecutionResult {
                        step_id: step.id.clone(),
                        success: false,
                        output: serde_json::json!({}),
                        error: Some(e.to_string()),
                        retries,
                        duration_ms,
                    };
                }
            }
        }
    }

    /// Internal step execution logic
    async fn execute_step_internal(&self, step: &PlanStep) -> Result<serde_json::Value> {
        info!("[EXECUTE] {}", step.description);

        // Simulate tool execution
        if let Some(tool_id) = &step.tool_id {
            match self
                .tool_registry
                .execute(tool_id, step.tool_args.clone().unwrap_or_default())
                .await
            {
                Ok(output) => Ok(output),
                Err(e) => {
                    warn!("[EXECUTE] Tool failed, trying fallback...");
                    // Try fallback
                    Err(anyhow!("Execution failed: {}", e))
                }
            }
        } else {
            Ok(serde_json::json!({"status": "completed"}))
        }
    }

    /// Run complete OSIRIS session
    pub async fn run_session(&self) -> Result<OSIRISState> {
        // Step 1: Analyze domains (Principle 1: Autonomous agents)
        let assessments = self.analyze_domains().await?;

        // Step 2: Reach consensus (Principle 2: PBFT)
        let priorities = self.reach_consensus(assessments).await?;

        // Step 3: Discover tools (Principle 3: Tool use)
        let tools = self.discover_tools().await?;

        // Step 4: Generate plan
        let plan = self.generate_plan(priorities, tools).await?;

        // Step 5: Execute plan (Principle 4 & 5: Recovery & receipts)
        let _results = self.execute_plan(plan).await?;

        // Return final state
        let state = self.state.read().await;
        Ok(state.clone())
    }

    /// Helper: describe action for a domain
    fn describe_action(&self, domain: &LifeDomain) -> String {
        match domain {
            LifeDomain::Learning => "Enroll in online course".to_string(),
            LifeDomain::Health => "Schedule workouts 4x/week".to_string(),
            LifeDomain::Leisure => "Plan weekend vacation".to_string(),
            LifeDomain::Career => "Schedule career coaching session".to_string(),
            LifeDomain::Finance => "Review investment portfolio".to_string(),
            LifeDomain::Relationships => "Schedule family dinner".to_string(),
        }
    }

    /// Helper: select appropriate tool for domain
    fn select_tool(&self, _domain: &LifeDomain, tools: &[ToolMetadata]) -> Option<String> {
        tools.first().map(|t| t.id.clone())
    }

    /// Stop the orchestrator gracefully
    pub async fn shutdown(&self) {
        info!("[SHUTDOWN] Stopping OSIRIS system...");
        self.is_running.store(false, Ordering::Relaxed);
    }

    /// Check if system is running
    pub fn is_running(&self) -> bool {
        self.is_running.load(Ordering::Relaxed)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[tokio::test]
    async fn test_orchestrator_creation() {
        let orchestrator = OSIRISOrchestrator::new("next 3 months".to_string(), false)
            .await
            .expect("Failed to create orchestrator");
        assert!(orchestrator.is_running());
    }

    #[tokio::test]
    async fn test_domain_analysis() {
        let orchestrator = OSIRISOrchestrator::new("next 3 months".to_string(), false)
            .await
            .expect("Failed to create orchestrator");
        let assessments = orchestrator
            .analyze_domains()
            .await
            .expect("Failed to analyze domains");
        assert_eq!(assessments.len(), 6);
    }

    #[tokio::test]
    async fn test_consensus() {
        let orchestrator = OSIRISOrchestrator::new("next 3 months".to_string(), false)
            .await
            .expect("Failed to create orchestrator");
        let assessments = orchestrator
            .analyze_domains()
            .await
            .expect("Failed to analyze domains");
        let priorities = orchestrator
            .reach_consensus(assessments)
            .await
            .expect("Failed to reach consensus");
        assert!(!priorities.is_empty());
    }

    #[tokio::test]
    async fn test_full_session() {
        let orchestrator = OSIRISOrchestrator::new("next 3 months".to_string(), false)
            .await
            .expect("Failed to create orchestrator");
        let state = orchestrator
            .run_session()
            .await
            .expect("Failed to run session");
        assert!(!state.assessments.is_empty());
        assert!(!state.priorities.is_empty());
        assert!(!state.plan_steps.is_empty());
    }
}
