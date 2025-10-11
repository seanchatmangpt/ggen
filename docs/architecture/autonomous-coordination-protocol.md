<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->
**Table of Contents**

- [Autonomous Agent Coordination Protocol](#autonomous-agent-coordination-protocol)
  - [1. Overview](#1-overview)
  - [2. Agent Types and Roles](#2-agent-types-and-roles)
    - [2.1 Agent Hierarchy](#21-agent-hierarchy)
    - [2.2 Agent Capabilities](#22-agent-capabilities)
  - [3. Message Protocol](#3-message-protocol)
    - [3.1 Message Types](#31-message-types)
    - [3.2 Message Routing](#32-message-routing)
  - [4. Consensus Mechanisms](#4-consensus-mechanisms)
    - [4.1 Proposal Voting](#41-proposal-voting)
    - [4.2 Conflict Resolution](#42-conflict-resolution)
  - [5. Coordination Patterns](#5-coordination-patterns)
    - [5.1 Work Distribution](#51-work-distribution)
    - [5.2 Failure Recovery](#52-failure-recovery)
  - [6. Agent Implementation Template](#6-agent-implementation-template)
  - [7. Coordination Guarantees](#7-coordination-guarantees)
    - [Safety Properties](#safety-properties)
    - [Liveness Properties](#liveness-properties)
    - [Performance Properties](#performance-properties)
  - [8. Testing Strategy](#8-testing-strategy)
    - [Unit Tests](#unit-tests)
    - [Integration Tests](#integration-tests)
    - [Chaos Tests](#chaos-tests)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

# Autonomous Agent Coordination Protocol

**Document**: Autonomous Agent Coordination Protocol v1.0
**Date**: 2025-10-10
**Purpose**: Define communication, coordination, and consensus mechanisms for autonomous AI agents

## 1. Overview

This protocol enables autonomous AI agents to coordinate graph evolution without human intervention. It defines message formats, consensus mechanisms, conflict resolution, and failure recovery.

## 2. Agent Types and Roles

### 2.1 Agent Hierarchy

```
┌─────────────────────────────────────────┐
│   Graph Evolution Coordinator           │
│   - Final decision authority            │
│   - Conflict resolution                 │
│   - Resource allocation                 │
└────────────┬────────────────────────────┘
             │
     ┌───────┴───────┬────────────────────┐
     │               │                    │
┌────▼────┐   ┌──────▼──────┐   ┌────────▼────────┐
│ NL      │   │ Trace       │   │ Doc             │
│ Parser  │   │ Analyzer    │   │ Extractor       │
│ Agents  │   │ Agents      │   │ Agents          │
└────┬────┘   └──────┬──────┘   └────────┬────────┘
     │               │                    │
     └───────┬───────┴────────────────────┘
             │
     ┌───────▼────────┐
     │   Validator    │
     │   Agents       │
     └────────────────┘
```

### 2.2 Agent Capabilities

```rust
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct AgentCapabilities {
    pub agent_type: AgentType,
    pub specializations: Vec<Specialization>,
    pub max_concurrent_tasks: usize,
    pub supported_operations: Vec<OperationType>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum AgentType {
    Coordinator,
    NLParser,
    TraceAnalyzer,
    DocExtractor,
    Validator,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum Specialization {
    ClassExtraction,
    PropertyInference,
    RelationshipMapping,
    ConstraintGeneration,
    QueryOptimization,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum OperationType {
    AddClass,
    AddProperty,
    AddRelationship,
    AddConstraint,
    ModifyClass,
    ModifyProperty,
    DeleteEntity,
}
```

## 3. Message Protocol

### 3.1 Message Types

```rust
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum AgentMessage {
    // Task assignment from coordinator
    TaskAssignment {
        task_id: String,
        assigned_to: String,
        task_type: TaskType,
        payload: TaskPayload,
        priority: Priority,
        deadline: Option<DateTime<Utc>>,
    },

    // Task result from agent
    TaskResult {
        task_id: String,
        agent_id: String,
        result: TaskResultType,
        metadata: TaskMetadata,
    },

    // Proposal for graph change
    ChangeProposal {
        proposal_id: String,
        proposer_id: String,
        operations: Vec<GraphOperation>,
        justification: String,
        confidence: f64,
    },

    // Vote on proposal
    ProposalVote {
        proposal_id: String,
        voter_id: String,
        vote: Vote,
        reasoning: String,
    },

    // Conflict notification
    ConflictDetected {
        conflict_id: String,
        detector_id: String,
        proposals: Vec<String>, // Conflicting proposal IDs
        conflict_type: ConflictType,
    },

    // Heartbeat for liveness
    Heartbeat {
        agent_id: String,
        status: AgentStatus,
        current_load: f64,
        capabilities: AgentCapabilities,
    },

    // Request for assistance
    AssistanceRequest {
        request_id: String,
        requester_id: String,
        issue: String,
        context: serde_json::Value,
    },
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum Vote {
    Approve,
    Reject,
    Abstain,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum ConflictType {
    OverlappingChanges,
    InconsistentProposals,
    ResourceContention,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum AgentStatus {
    Idle,
    Busy,
    Overloaded,
    Degraded,
    Offline,
}
```

### 3.2 Message Routing

```rust
pub struct MessageRouter {
    agents: HashMap<String, AgentHandle>,
    coordinator: CoordinatorHandle,
    message_log: Arc<RwLock<Vec<LoggedMessage>>>,
}

impl MessageRouter {
    pub async fn route_message(&self, msg: AgentMessage) -> Result<()> {
        // Log all messages for audit trail
        self.log_message(&msg).await?;

        match &msg {
            AgentMessage::TaskAssignment { assigned_to, .. } => {
                let agent = self.agents.get(assigned_to)
                    .ok_or(GgenAiError::AgentNotFound)?;
                agent.send(msg).await?;
            }

            AgentMessage::TaskResult { .. } => {
                self.coordinator.send(msg).await?;
            }

            AgentMessage::ChangeProposal { .. } => {
                // Broadcast to all validators
                self.broadcast_to_validators(msg).await?;
            }

            AgentMessage::ProposalVote { .. } => {
                self.coordinator.send(msg).await?;
            }

            AgentMessage::ConflictDetected { .. } => {
                self.coordinator.send(msg).await?;
            }

            AgentMessage::Heartbeat { agent_id, .. } => {
                self.coordinator.update_agent_status(agent_id, &msg).await?;
            }

            AgentMessage::AssistanceRequest { .. } => {
                self.coordinator.send(msg).await?;
            }
        }

        Ok(())
    }

    async fn broadcast_to_validators(&self, msg: AgentMessage) -> Result<()> {
        for (id, agent) in &self.agents {
            if agent.agent_type() == AgentType::Validator {
                agent.send(msg.clone()).await?;
            }
        }
        Ok(())
    }
}
```

## 4. Consensus Mechanisms

### 4.1 Proposal Voting

```rust
pub struct ConsensusEngine {
    quorum_threshold: f64, // e.g., 0.66 for 2/3 majority
    timeout: Duration,
    active_proposals: Arc<RwLock<HashMap<String, ProposalState>>>,
}

#[derive(Debug, Clone)]
pub struct ProposalState {
    pub proposal: ChangeProposal,
    pub votes: Vec<ProposalVote>,
    pub created_at: DateTime<Utc>,
    pub status: ProposalStatus,
}

#[derive(Debug, Clone, PartialEq)]
pub enum ProposalStatus {
    Pending,
    Approved,
    Rejected,
    Expired,
}

impl ConsensusEngine {
    pub async fn submit_proposal(&self, proposal: ChangeProposal) -> Result<String> {
        let proposal_id = proposal.proposal_id.clone();

        let state = ProposalState {
            proposal,
            votes: Vec::new(),
            created_at: Utc::now(),
            status: ProposalStatus::Pending,
        };

        self.active_proposals.write().await.insert(proposal_id.clone(), state);

        // Start timeout timer
        self.start_timeout_timer(&proposal_id).await;

        Ok(proposal_id)
    }

    pub async fn record_vote(&self, vote: ProposalVote) -> Result<ProposalStatus> {
        let mut proposals = self.active_proposals.write().await;

        let state = proposals.get_mut(&vote.proposal_id)
            .ok_or(GgenAiError::ProposalNotFound)?;

        // Check if already voted
        if state.votes.iter().any(|v| v.voter_id == vote.voter_id) {
            return Err(GgenAiError::DuplicateVote);
        }

        state.votes.push(vote);

        // Check if consensus reached
        let status = self.check_consensus(state)?;
        state.status = status.clone();

        Ok(status)
    }

    fn check_consensus(&self, state: &ProposalState) -> Result<ProposalStatus> {
        let total_votes = state.votes.len();
        let approve_votes = state.votes.iter().filter(|v| matches!(v.vote, Vote::Approve)).count();
        let reject_votes = state.votes.iter().filter(|v| matches!(v.vote, Vote::Reject)).count();

        // Need quorum to make decision
        let quorum_met = (total_votes as f64 / self.get_total_validators() as f64) >= self.quorum_threshold;

        if !quorum_met {
            return Ok(ProposalStatus::Pending);
        }

        // Majority of quorum votes
        if approve_votes as f64 / total_votes as f64 > 0.5 {
            Ok(ProposalStatus::Approved)
        } else if reject_votes as f64 / total_votes as f64 > 0.5 {
            Ok(ProposalStatus::Rejected)
        } else {
            Ok(ProposalStatus::Pending)
        }
    }

    async fn start_timeout_timer(&self, proposal_id: &str) {
        let proposal_id = proposal_id.to_string();
        let proposals = self.active_proposals.clone();
        let timeout = self.timeout;

        tokio::spawn(async move {
            tokio::time::sleep(timeout).await;

            let mut props = proposals.write().await;
            if let Some(state) = props.get_mut(&proposal_id) {
                if state.status == ProposalStatus::Pending {
                    state.status = ProposalStatus::Expired;
                }
            }
        });
    }

    fn get_total_validators(&self) -> usize {
        // This would be provided by coordinator
        5 // Example: 5 validator agents
    }
}
```

### 4.2 Conflict Resolution

```rust
pub struct ConflictResolver {
    resolution_strategy: ResolutionStrategy,
    coordinator_client: Box<dyn LlmClient>,
}

#[derive(Debug, Clone)]
pub enum ResolutionStrategy {
    FirstComeFirstServed,
    HighestConfidence,
    CoordinatorDecision,
    Merge,
}

impl ConflictResolver {
    pub async fn resolve_conflict(&self, conflict: ConflictDetected) -> Result<Resolution> {
        match self.resolution_strategy {
            ResolutionStrategy::FirstComeFirstServed => {
                self.resolve_fcfs(&conflict).await
            }
            ResolutionStrategy::HighestConfidence => {
                self.resolve_by_confidence(&conflict).await
            }
            ResolutionStrategy::CoordinatorDecision => {
                self.resolve_by_coordinator(&conflict).await
            }
            ResolutionStrategy::Merge => {
                self.resolve_by_merge(&conflict).await
            }
        }
    }

    async fn resolve_fcfs(&self, conflict: &ConflictDetected) -> Result<Resolution> {
        // Accept first proposal, reject others
        Ok(Resolution {
            accepted: vec![conflict.proposals[0].clone()],
            rejected: conflict.proposals[1..].to_vec(),
            reasoning: "First-come-first-served policy".to_string(),
        })
    }

    async fn resolve_by_confidence(&self, conflict: &ConflictDetected) -> Result<Resolution> {
        // Accept proposal with highest confidence
        // (Would need to fetch proposal details)
        unimplemented!()
    }

    async fn resolve_by_coordinator(&self, conflict: &ConflictDetected) -> Result<Resolution> {
        // Use AI coordinator to decide
        let prompt = format!(
            "Resolve this conflict between graph change proposals:\n\
             Conflict Type: {:?}\n\
             Proposals: {:?}\n\
             Which proposal(s) should be accepted and why?",
            conflict.conflict_type,
            conflict.proposals
        );

        let response = self.coordinator_client.complete(&prompt).await?;

        // Parse AI decision
        self.parse_resolution_decision(&response.content)
    }

    async fn resolve_by_merge(&self, conflict: &ConflictDetected) -> Result<Resolution> {
        // Attempt to merge non-conflicting parts of proposals
        let prompt = format!(
            "Merge these conflicting graph change proposals:\n\
             {:?}\n\
             Output a single merged proposal that satisfies both intents.",
            conflict.proposals
        );

        let response = self.coordinator_client.complete(&prompt).await?;

        Ok(Resolution {
            accepted: vec!["merged_proposal".to_string()],
            rejected: Vec::new(),
            reasoning: response.content,
        })
    }

    fn parse_resolution_decision(&self, decision: &str) -> Result<Resolution> {
        // Parse AI coordinator's decision
        // This would use structured output or JSON parsing
        unimplemented!()
    }
}

#[derive(Debug, Clone)]
pub struct Resolution {
    pub accepted: Vec<String>, // Accepted proposal IDs
    pub rejected: Vec<String>, // Rejected proposal IDs
    pub reasoning: String,
}
```

## 5. Coordination Patterns

### 5.1 Work Distribution

```rust
pub struct WorkDistributor {
    agents: HashMap<String, AgentHandle>,
    load_balancer: LoadBalancer,
}

#[derive(Debug, Clone)]
pub struct LoadBalancer {
    strategy: LoadBalancingStrategy,
}

#[derive(Debug, Clone)]
pub enum LoadBalancingStrategy {
    RoundRobin,
    LeastLoaded,
    CapabilityMatching,
}

impl WorkDistributor {
    pub async fn assign_task(&self, task: Task) -> Result<String> {
        // Select best agent for task
        let agent_id = self.load_balancer.select_agent(&self.agents, &task)?;

        let agent = self.agents.get(&agent_id)
            .ok_or(GgenAiError::AgentNotFound)?;

        // Send task assignment
        let msg = AgentMessage::TaskAssignment {
            task_id: task.id.clone(),
            assigned_to: agent_id.clone(),
            task_type: task.task_type,
            payload: task.payload,
            priority: task.priority,
            deadline: task.deadline,
        };

        agent.send(msg).await?;

        Ok(agent_id)
    }
}

impl LoadBalancer {
    pub fn select_agent(&self, agents: &HashMap<String, AgentHandle>, task: &Task) -> Result<String> {
        match self.strategy {
            LoadBalancingStrategy::RoundRobin => {
                self.select_round_robin(agents)
            }
            LoadBalancingStrategy::LeastLoaded => {
                self.select_least_loaded(agents)
            }
            LoadBalancingStrategy::CapabilityMatching => {
                self.select_by_capability(agents, task)
            }
        }
    }

    fn select_round_robin(&self, agents: &HashMap<String, AgentHandle>) -> Result<String> {
        // Simple round-robin selection
        agents.keys().next()
            .cloned()
            .ok_or(GgenAiError::NoAgentsAvailable)
    }

    fn select_least_loaded(&self, agents: &HashMap<String, AgentHandle>) -> Result<String> {
        agents.iter()
            .min_by_key(|(_, agent)| agent.current_load())
            .map(|(id, _)| id.clone())
            .ok_or(GgenAiError::NoAgentsAvailable)
    }

    fn select_by_capability(&self, agents: &HashMap<String, AgentHandle>, task: &Task) -> Result<String> {
        // Find agent with matching specialization
        agents.iter()
            .filter(|(_, agent)| agent.supports_task(task))
            .min_by_key(|(_, agent)| agent.current_load())
            .map(|(id, _)| id.clone())
            .ok_or(GgenAiError::NoAgentsAvailable)
    }
}
```

### 5.2 Failure Recovery

```rust
pub struct FailureRecoveryManager {
    retry_policy: RetryPolicy,
    task_queue: Arc<RwLock<TaskQueue>>,
    dead_letter_queue: Arc<RwLock<Vec<FailedTask>>>,
}

#[derive(Debug, Clone)]
pub struct RetryPolicy {
    pub max_retries: u32,
    pub backoff_strategy: BackoffStrategy,
    pub retry_on: Vec<ErrorType>,
}

#[derive(Debug, Clone)]
pub enum BackoffStrategy {
    Linear(Duration),
    Exponential { base: Duration, multiplier: u32 },
    Constant(Duration),
}

impl FailureRecoveryManager {
    pub async fn handle_failure(&self, task_id: &str, error: &str) -> Result<RecoveryAction> {
        // Get task from queue
        let mut queue = self.task_queue.write().await;
        let task = queue.get_mut(task_id)
            .ok_or(GgenAiError::TaskNotFound)?;

        task.retry_count += 1;

        if task.retry_count > self.retry_policy.max_retries {
            // Move to dead letter queue
            self.move_to_dlq(task.clone()).await?;
            return Ok(RecoveryAction::GiveUp);
        }

        // Calculate backoff
        let backoff = self.calculate_backoff(task.retry_count)?;

        Ok(RecoveryAction::Retry {
            delay: backoff,
            task_id: task_id.to_string(),
        })
    }

    fn calculate_backoff(&self, retry_count: u32) -> Result<Duration> {
        match &self.retry_policy.backoff_strategy {
            BackoffStrategy::Constant(duration) => Ok(*duration),
            BackoffStrategy::Linear(base) => {
                Ok(*base * retry_count)
            }
            BackoffStrategy::Exponential { base, multiplier } => {
                Ok(*base * (*multiplier).pow(retry_count - 1))
            }
        }
    }

    async fn move_to_dlq(&self, task: Task) -> Result<()> {
        let mut dlq = self.dead_letter_queue.write().await;
        dlq.push(FailedTask {
            task,
            final_error: "Max retries exceeded".to_string(),
            failed_at: Utc::now(),
        });
        Ok(())
    }
}

#[derive(Debug, Clone)]
pub enum RecoveryAction {
    Retry { delay: Duration, task_id: String },
    ReassignToOtherAgent { task_id: String, new_agent: String },
    GiveUp,
}

#[derive(Debug, Clone)]
pub struct FailedTask {
    pub task: Task,
    pub final_error: String,
    pub failed_at: DateTime<Utc>,
}
```

## 6. Agent Implementation Template

```rust
#[async_trait]
pub trait AutonomousAgent: Send + Sync {
    /// Get agent's unique ID
    fn agent_id(&self) -> &str;

    /// Get agent's type
    fn agent_type(&self) -> AgentType;

    /// Get agent's capabilities
    fn capabilities(&self) -> AgentCapabilities;

    /// Process incoming message
    async fn process_message(&mut self, msg: AgentMessage) -> Result<()>;

    /// Perform assigned task
    async fn execute_task(&self, task: Task) -> Result<TaskResultType>;

    /// Vote on a proposal
    async fn vote_on_proposal(&self, proposal: &ChangeProposal) -> Result<ProposalVote>;

    /// Report current status
    async fn heartbeat(&self) -> Result<AgentStatus>;

    /// Shutdown gracefully
    async fn shutdown(&mut self) -> Result<()>;
}

// Example implementation
pub struct NLParserAgent {
    id: String,
    llm_client: Box<dyn LlmClient>,
    message_rx: mpsc::Receiver<AgentMessage>,
    message_tx: mpsc::Sender<AgentMessage>,
    current_load: Arc<AtomicU32>,
}

#[async_trait]
impl AutonomousAgent for NLParserAgent {
    fn agent_id(&self) -> &str {
        &self.id
    }

    fn agent_type(&self) -> AgentType {
        AgentType::NLParser
    }

    fn capabilities(&self) -> AgentCapabilities {
        AgentCapabilities {
            agent_type: AgentType::NLParser,
            specializations: vec![
                Specialization::ClassExtraction,
                Specialization::PropertyInference,
                Specialization::RelationshipMapping,
            ],
            max_concurrent_tasks: 5,
            supported_operations: vec![
                OperationType::AddClass,
                OperationType::AddProperty,
                OperationType::AddRelationship,
            ],
        }
    }

    async fn process_message(&mut self, msg: AgentMessage) -> Result<()> {
        match msg {
            AgentMessage::TaskAssignment { task_id, task_type, payload, .. } => {
                let result = self.execute_task(Task {
                    id: task_id.clone(),
                    task_type,
                    payload,
                    priority: Priority::Normal,
                    deadline: None,
                    retry_count: 0,
                }).await?;

                // Send result back
                self.message_tx.send(AgentMessage::TaskResult {
                    task_id,
                    agent_id: self.id.clone(),
                    result,
                    metadata: TaskMetadata::default(),
                }).await?;
            }
            _ => {
                tracing::warn!("Unexpected message type for NL Parser agent");
            }
        }
        Ok(())
    }

    async fn execute_task(&self, task: Task) -> Result<TaskResultType> {
        self.current_load.fetch_add(1, Ordering::SeqCst);

        // Parse natural language input
        let nl_input = task.payload.get("input")
            .and_then(|v| v.as_str())
            .ok_or(GgenAiError::InvalidTaskPayload)?;

        // Use LLM to extract graph operations
        let prompt = format!(
            "Extract RDF graph operations from this natural language input:\n\
             {}\n\
             Output as structured operations (AddClass, AddProperty, etc.)",
            nl_input
        );

        let response = self.llm_client.complete(&prompt).await?;

        // Parse operations from response
        let operations = self.parse_operations(&response.content)?;

        self.current_load.fetch_sub(1, Ordering::SeqCst);

        Ok(TaskResultType::GraphOperations(operations))
    }

    async fn vote_on_proposal(&self, proposal: &ChangeProposal) -> Result<ProposalVote> {
        // Evaluate proposal using LLM
        let prompt = format!(
            "Evaluate this graph change proposal:\n\
             Operations: {:?}\n\
             Justification: {}\n\
             Should this be approved? Answer with Approve/Reject and reasoning.",
            proposal.operations,
            proposal.justification
        );

        let response = self.llm_client.complete(&prompt).await?;

        let vote = if response.content.contains("Approve") {
            Vote::Approve
        } else {
            Vote::Reject
        };

        Ok(ProposalVote {
            proposal_id: proposal.proposal_id.clone(),
            voter_id: self.id.clone(),
            vote,
            reasoning: response.content,
        })
    }

    async fn heartbeat(&self) -> Result<AgentStatus> {
        let load = self.current_load.load(Ordering::SeqCst);

        if load == 0 {
            Ok(AgentStatus::Idle)
        } else if load < 5 {
            Ok(AgentStatus::Busy)
        } else {
            Ok(AgentStatus::Overloaded)
        }
    }

    async fn shutdown(&mut self) -> Result<()> {
        tracing::info!("NL Parser agent {} shutting down", self.id);
        // Cleanup resources
        Ok(())
    }
}
```

## 7. Coordination Guarantees

### Safety Properties
1. **At-most-once graph mutation**: No change applied twice
2. **Atomic commits**: All or nothing for each graph delta
3. **Consistency**: Graph always in valid state
4. **Isolation**: Concurrent changes don't interfere

### Liveness Properties
1. **Progress guarantee**: Every valid proposal eventually decided
2. **Deadlock freedom**: No circular dependencies in conflict resolution
3. **Starvation freedom**: All agents eventually get tasks

### Performance Properties
1. **Low latency**: < 100ms for message routing
2. **High throughput**: > 100 messages/sec
3. **Scalability**: Linear scaling with agent count
4. **Fault tolerance**: Survive single agent failures

## 8. Testing Strategy

### Unit Tests
- Message serialization/deserialization
- Consensus algorithm correctness
- Conflict resolution logic
- Load balancing strategies

### Integration Tests
- Multi-agent coordination scenarios
- Concurrent proposal handling
- Failure recovery flows
- End-to-end task execution

### Chaos Tests
- Random agent failures
- Network partition simulation
- Message loss scenarios
- Overload conditions

---

**Protocol Version**: 1.0.0
**Status**: Design Complete
**Compatibility**: Backwards compatible with ggen-ai v1.x
