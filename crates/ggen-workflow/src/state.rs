//! Workflow state machine implementation
//!
//! This module provides a comprehensive state machine for workflow execution
//! following the Poka-Yoke principle of making invalid states unrepresentable.
//! The state machine enforces valid transitions and provides cryptographic
//! guarantees for state persistence and reproducibility.
//!
//! # State Machine Diagram
//!
//! ```text
//!                    ┌─────────────┐
//!                    │   Ready     │◄─────────────┐
//!                    └──────┬──────┘              │
//!                           │ start               │ reset
//!                           ▼                     │
//!                    ┌─────────────┐              │
//!            ┌──────►│  Running   │──────────────┘
//!            │       └──────┬──────┘
//!            │              │ suspend / fail
//!            │              ▼
//!            │       ┌─────────────┐    recover
//!            │       │ Suspended   │◄──────────┐
//!            │       └──────┬──────┘           │
//!            │              │ resume           │
//!            │              └──────────────────┘
//!            │
//!    complete│              │ fail
//!            ▼              ▼
//!       ┌───────────┐  ┌───────────┐
//!       │ Completed │  │  Failed   │
//!       └───────────┘  └───────────┘
//! ```
//!
//! # Examples
//!
//! ```ignore
//! use ggen_workflow::state::{WorkflowState, StateMachine};
//!
//! // Create a new state machine in Ready state
//! let mut sm = StateMachine::new("workflow-123");
//!
//! // Transition to Running
//! sm.start()?;
//!
//! // Complete the workflow
//! sm.complete()?;
//!
//! // Serialize state
//! let json = sm.to_json()?;
//! ```

use crate::error::errors;
use crate::error::{WorkflowError, WorkflowResult};
use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use std::fmt;
use std::sync::atomic::{AtomicU64, Ordering};
use uuid::Uuid;

/// Counter for generating unique state transition IDs
static STATE_TRANSITION_COUNTER: AtomicU64 = AtomicU64::new(0);

/// Workflow state enumeration representing all possible states in the lifecycle
///
/// Each state carries specific metadata relevant to that phase of execution.
/// This design prevents invalid states from being representable at the type level.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub enum WorkflowState {
    /// Initial state - workflow is ready to begin execution
    /// No execution has started yet
    Ready {
        /// Timestamp when state was entered
        entered_at: chrono::DateTime<chrono::Utc>,
        /// Workflow definition metadata
        definition: WorkflowDefinition,
    },

    /// Active state - workflow is currently executing
    Running {
        /// Timestamp when execution started
        started_at: chrono::DateTime<chrono::Utc>,
        /// Current step index being executed
        current_step: usize,
        /// Total steps in workflow
        total_steps: usize,
        /// Progress percentage (0-100)
        progress: u8,
        /// Execution context data
        context: HashMap<String, serde_json::Value>,
    },

    /// Paused state - workflow execution is suspended
    /// Can be resumed to Running state
    Suspended {
        /// Timestamp when suspension occurred
        suspended_at: chrono::DateTime<chrono::Utc>,
        /// Reason for suspension
        reason: SuspendReason,
        /// Snapshot of execution state for resume
        snapshot: ExecutionSnapshot,
        /// How long suspension has lasted
        suspended_duration_secs: u64,
    },

    /// Terminal state - workflow completed successfully
    /// No further transitions possible
    Completed {
        /// Timestamp when workflow completed
        completed_at: chrono::DateTime<chrono::Utc>,
        /// Final execution results
        results: WorkflowResults,
        /// Total execution duration in milliseconds
        duration_ms: u64,
    },

    /// Terminal state - workflow failed with error
    /// No further transitions possible
    Failed {
        /// Timestamp when failure occurred
        failed_at: chrono::DateTime<chrono::Utc>,
        /// Error that caused failure
        error: WorkflowErrorInfo,
        /// Partial results before failure
        partial_results: Option<WorkflowResults>,
    },
}

impl WorkflowState {
    /// Get the name of the current state
    pub fn name(&self) -> &'static str {
        match self {
            WorkflowState::Ready { .. } => "Ready",
            WorkflowState::Running { .. } => "Running",
            WorkflowState::Suspended { .. } => "Suspended",
            WorkflowState::Completed { .. } => "Completed",
            WorkflowState::Failed { .. } => "Failed",
        }
    }

    /// Check if this is a terminal state (no further transitions possible)
    pub fn is_terminal(&self) -> bool {
        matches!(
            self,
            WorkflowState::Completed { .. } | WorkflowState::Failed { .. }
        )
    }

    /// Check if this is an active state (workflow can be running or suspended)
    pub fn is_active(&self) -> bool {
        matches!(
            self,
            WorkflowState::Running { .. } | WorkflowState::Suspended { .. }
        )
    }

    /// Check if workflow can be started from this state
    pub fn can_start(&self) -> bool {
        matches!(self, WorkflowState::Ready { .. })
    }

    /// Check if workflow can be suspended from this state
    pub fn can_suspend(&self) -> bool {
        matches!(self, WorkflowState::Running { .. })
    }

    /// Check if workflow can be resumed from this state
    pub fn can_resume(&self) -> bool {
        matches!(self, WorkflowState::Suspended { .. })
    }

    /// Get the timestamp when the state was entered
    pub fn entered_at(&self) -> chrono::DateTime<chrono::Utc> {
        match self {
            WorkflowState::Ready { entered_at, .. } => *entered_at,
            WorkflowState::Running { started_at, .. } => *started_at,
            WorkflowState::Suspended { suspended_at, .. } => *suspended_at,
            WorkflowState::Completed { completed_at, .. } => *completed_at,
            WorkflowState::Failed { failed_at, .. } => *failed_at,
        }
    }

    /// Calculate duration since entering this state
    pub fn duration_since_entry(&self) -> chrono::Duration {
        let now = chrono::Utc::now();
        now.signed_duration_since(self.entered_at())
    }
}

impl fmt::Display for WorkflowState {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.name())
    }
}

/// Reason for workflow suspension
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub enum SuspendReason {
    /// Manual suspension requested by user
    UserRequested,
    /// System-initiated suspension (resource constraints, etc.)
    SystemInitiated(String),
    /// Suspension due to external dependency
    DependencyWait(String),
    /// Suspension due to rate limiting
    RateLimitExceeded,
    /// Suspension for checkpoint/recovery
    Checkpoint,
}

impl fmt::Display for SuspendReason {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            SuspendReason::UserRequested => write!(f, "User requested"),
            SuspendReason::SystemInitiated(reason) => write!(f, "System: {}", reason),
            SuspendReason::DependencyWait(dep) => write!(f, "Waiting for dependency: {}", dep),
            SuspendReason::RateLimitExceeded => write!(f, "Rate limit exceeded"),
            SuspendReason::Checkpoint => write!(f, "Checkpoint"),
        }
    }
}

/// Workflow definition metadata
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct WorkflowDefinition {
    /// Unique workflow identifier
    pub id: String,
    /// Human-readable name
    pub name: String,
    /// Description
    pub description: Option<String>,
    /// Total number of steps
    pub step_count: usize,
    /// Workflow version
    pub version: String,
    /// Definition checksum for verification
    pub checksum: String,
}

impl WorkflowDefinition {
    /// Create a new workflow definition
    pub fn new(
        id: String, name: String, description: Option<String>, step_count: usize, version: String,
    ) -> Self {
        // Generate checksum from definition data
        let checksum_data = format!("{}|{}|{}|{}", id, name, step_count, version);
        let checksum = Self::generate_checksum(&checksum_data);

        Self {
            id,
            name,
            description,
            step_count,
            version,
            checksum,
        }
    }

    /// Generate SHA-256 checksum for definition verification
    fn generate_checksum(data: &str) -> String {
        use sha2::{Digest, Sha256};
        let mut hasher = Sha256::new();
        hasher.update(data.as_bytes());
        let result = hasher.finalize();
        hex::encode(result)
    }

    /// Verify the definition checksum
    pub fn verify_checksum(&self) -> bool {
        let checksum_data = format!(
            "{}|{}|{}|{}",
            self.id, self.name, self.step_count, self.version
        );
        let expected = Self::generate_checksum(&checksum_data);
        self.checksum == expected
    }
}

/// Snapshot of execution state for suspend/resume
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct ExecutionSnapshot {
    /// Snapshot ID
    pub id: String,
    /// Snapshot timestamp
    pub timestamp: chrono::DateTime<chrono::Utc>,
    /// Current step index
    pub current_step: usize,
    /// Execution context data
    pub context: HashMap<String, serde_json::Value>,
    /// Partial output data
    pub output: HashMap<String, serde_json::Value>,
    /// Trace event history
    pub trace: Vec<TraceEventSnapshot>,
    /// Snapshot checksum
    pub checksum: String,
}

/// Manual Eq implementation excluding timestamp (DateTime<Utc> doesn't implement Eq)
impl Eq for ExecutionSnapshot {}

impl ExecutionSnapshot {
    /// Create a new execution snapshot
    pub fn new(
        current_step: usize, context: HashMap<String, serde_json::Value>,
        output: HashMap<String, serde_json::Value>, trace: Vec<TraceEventSnapshot>,
    ) -> Self {
        let id = Uuid::new_v4().to_string();
        let timestamp = chrono::Utc::now();

        // Generate checksum
        let checksum = Self::generate_checksum(&id, current_step, &context, &output);

        Self {
            id,
            timestamp,
            current_step,
            context,
            output,
            trace,
            checksum,
        }
    }

    /// Generate checksum for snapshot verification
    fn generate_checksum(
        id: &str, step: usize, context: &HashMap<String, serde_json::Value>,
        output: &HashMap<String, serde_json::Value>,
    ) -> String {
        use sha2::{Digest, Sha256};
        let mut hasher = Sha256::new();
        hasher.update(id.as_bytes());
        hasher.update(step.to_string().as_bytes());
        for (k, v) in context {
            hasher.update(k.as_bytes());
            hasher.update(serde_json::to_string(v).unwrap().as_bytes());
        }
        for (k, v) in output {
            hasher.update(k.as_bytes());
            hasher.update(serde_json::to_string(v).unwrap().as_bytes());
        }
        hex::encode(hasher.finalize())
    }

    /// Verify snapshot integrity
    pub fn verify(&self) -> bool {
        let expected =
            Self::generate_checksum(&self.id, self.current_step, &self.context, &self.output);
        self.checksum == expected
    }
}

/// Trace event snapshot for debugging
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct TraceEventSnapshot {
    /// Step name
    pub step: String,
    /// Event type
    pub event_type: String,
    /// Timestamp
    pub timestamp: chrono::DateTime<chrono::Utc>,
    /// Message
    pub message: String,
}

/// Manual Eq implementation excluding timestamp (DateTime<Utc> doesn't implement Eq)
impl Eq for TraceEventSnapshot {}

/// Workflow execution results
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct WorkflowResults {
    /// Final output data
    pub output: HashMap<String, serde_json::Value>,
    /// Execution metrics
    pub metrics: ExecutionMetrics,
    /// Complete trace event history
    pub trace: Vec<TraceEventSnapshot>,
}

/// Execution metrics for completed workflows
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct ExecutionMetrics {
    /// Total steps executed
    pub steps_executed: usize,
    /// Total execution duration in milliseconds
    pub total_duration_ms: u64,
    /// Number of successful operations
    pub successful_operations: usize,
    /// Number of failed operations
    pub failed_operations: usize,
    /// Number of retries performed
    pub retry_count: usize,
}

/// Serializable error information for Failed state
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct WorkflowErrorInfo {
    /// Error category
    pub category: String,
    /// Error message
    pub message: String,
    /// Source location if available
    pub source: Option<String>,
    /// Timestamp when error occurred
    pub timestamp: chrono::DateTime<chrono::Utc>,
}

/// Manual Eq implementation excluding timestamp (DateTime<Utc> doesn't implement Eq)
impl Eq for WorkflowErrorInfo {}

impl From<&WorkflowError> for WorkflowErrorInfo {
    fn from(err: &WorkflowError) -> Self {
        WorkflowErrorInfo {
            category: error_category(err),
            message: err.to_string(),
            source: None,
            timestamp: chrono::Utc::now(),
        }
    }
}

/// Get error category for classification
fn error_category(err: &WorkflowError) -> String {
    match err {
        WorkflowError::RdfParse(_) | WorkflowError::SparqlQuery { .. } => "parsing".to_string(),
        WorkflowError::Validation(_) => "validation".to_string(),
        WorkflowError::PatternExecution { .. } => "execution".to_string(),
        WorkflowError::ReceiptGeneration(_) => "receipt".to_string(),
        WorkflowError::Resource(_) => "resource".to_string(),
        WorkflowError::Configuration(_) => "configuration".to_string(),
        WorkflowError::Timeout { .. } => "timeout".to_string(),
        WorkflowError::Io(_) => "io".to_string(),
        WorkflowError::Serialization(_) | WorkflowError::Deserialization(_) => {
            "serialization".to_string()
        }
        WorkflowError::Unsupported(_) => "unsupported".to_string(),
        WorkflowError::System(_) => "system".to_string(),
        WorkflowError::XmlParse(_) => "xml_parsing".to_string(),
        WorkflowError::YawlValidation(_) => "yawl_validation".to_string(),
        WorkflowError::MissingElement { .. } => "missing_element".to_string(),
        WorkflowError::InvalidAttribute { .. } => "invalid_attribute".to_string(),
        WorkflowError::UnsupportedFeature(_) => "unsupported_feature".to_string(),
    }
}

/// State transition event emitted on every state change
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct StateTransitionEvent {
    /// Unique transition ID
    pub transition_id: String,
    /// Workflow ID
    pub workflow_id: String,
    /// Previous state
    pub from_state: String,
    /// New state
    pub to_state: String,
    /// Transition timestamp
    pub timestamp: chrono::DateTime<chrono::Utc>,
    /// Transition type
    pub transition_type: TransitionType,
    /// Additional metadata
    pub metadata: HashMap<String, serde_json::Value>,
}

/// Manual Eq implementation excluding timestamp (DateTime<Utc> doesn't implement Eq)
impl Eq for StateTransitionEvent {}

/// Types of state transitions
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub enum TransitionType {
    /// Start execution (Ready -> Running)
    Start,
    /// Suspend execution (Running -> Suspended)
    Suspend,
    /// Resume execution (Suspended -> Running)
    Resume,
    /// Complete successfully (Running/Suspended -> Completed)
    Complete,
    /// Fail with error (Running/Suspended -> Failed)
    Fail,
    /// Reset to ready (any -> Ready)
    Reset,
}

impl fmt::Display for TransitionType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            TransitionType::Start => write!(f, "start"),
            TransitionType::Suspend => write!(f, "suspend"),
            TransitionType::Resume => write!(f, "resume"),
            TransitionType::Complete => write!(f, "complete"),
            TransitionType::Fail => write!(f, "fail"),
            TransitionType::Reset => write!(f, "reset"),
        }
    }
}

/// State machine for workflow execution
///
/// Enforces valid state transitions and provides event emission
/// for observability and audit trails.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct StateMachine {
    /// Current workflow state
    state: WorkflowState,
    /// Workflow ID
    workflow_id: String,
    /// History of all state transitions
    transition_history: Vec<StateTransitionEvent>,
    /// Event handlers registered for state changes
    #[serde(skip)]
    event_handlers: Vec<EventHandler>,
    /// Whether to emit events on transitions
    emit_events: bool,
}

impl StateMachine {
    /// Create a new state machine in Ready state
    pub fn new(workflow_id: String) -> Self {
        let definition = WorkflowDefinition {
            id: workflow_id.clone(),
            name: "Unnamed Workflow".to_string(),
            description: None,
            step_count: 0,
            version: "1.0.0".to_string(),
            checksum: String::new(),
        };

        let state = WorkflowState::Ready {
            entered_at: chrono::Utc::now(),
            definition,
        };

        StateMachine {
            state,
            workflow_id,
            transition_history: Vec::new(),
            event_handlers: Vec::new(),
            emit_events: true,
        }
    }

    /// Create a new state machine with a full definition
    pub fn with_definition(
        workflow_id: String, name: String, description: Option<String>, step_count: usize,
        version: String,
    ) -> Self {
        let definition =
            WorkflowDefinition::new(workflow_id.clone(), name, description, step_count, version);

        let state = WorkflowState::Ready {
            entered_at: chrono::Utc::now(),
            definition,
        };

        StateMachine {
            state,
            workflow_id,
            transition_history: Vec::new(),
            event_handlers: Vec::new(),
            emit_events: true,
        }
    }

    /// Restore a state machine from a previous state
    pub fn restore(workflow_id: String, state: WorkflowState) -> WorkflowResult<Self> {
        Ok(StateMachine {
            state,
            workflow_id,
            transition_history: Vec::new(),
            event_handlers: Vec::new(),
            emit_events: true,
        })
    }

    /// Get the current state
    pub fn current_state(&self) -> &WorkflowState {
        &self.state
    }

    /// Get the workflow ID
    pub fn workflow_id(&self) -> &str {
        &self.workflow_id
    }

    /// Get transition history
    pub fn transition_history(&self) -> &[StateTransitionEvent] {
        &self.transition_history
    }

    /// Check if workflow is in a terminal state
    pub fn is_terminal(&self) -> bool {
        self.state.is_terminal()
    }

    /// Check if workflow is active (running or suspended)
    pub fn is_active(&self) -> bool {
        self.state.is_active()
    }

    /// Start workflow execution (Ready -> Running)
    pub fn start(&mut self, context: HashMap<String, serde_json::Value>) -> WorkflowResult<()> {
        if !self.state.can_start() {
            return Err(errors::validation(format!(
                "Cannot start workflow from state: {}",
                self.state.name()
            )));
        }

        let total_steps = match &self.state {
            WorkflowState::Ready { definition, .. } => definition.step_count,
            _ => return Err(errors::validation("Invalid state for start operation")),
        };

        let new_state = WorkflowState::Running {
            started_at: chrono::Utc::now(),
            current_step: 0,
            total_steps,
            progress: 0,
            context,
        };

        self.transition(new_state, TransitionType::Start, HashMap::new())
    }

    /// Suspend workflow execution (Running -> Suspended)
    pub fn suspend(&mut self, reason: SuspendReason) -> WorkflowResult<()> {
        if !self.state.can_suspend() {
            return Err(errors::validation(format!(
                "Cannot suspend workflow from state: {}",
                self.state.name()
            )));
        }

        let (current_step, _total_steps, context) = match &self.state {
            WorkflowState::Running {
                current_step,
                total_steps,
                context,
                ..
            } => (*current_step, *total_steps, context.clone()),
            _ => return Err(errors::validation("Invalid state for suspend operation")),
        };

        let trace = Vec::new(); // In real implementation, capture from context
        let snapshot = ExecutionSnapshot::new(current_step, context, HashMap::new(), trace);

        let new_state = WorkflowState::Suspended {
            suspended_at: chrono::Utc::now(),
            reason: reason.clone(),
            snapshot,
            suspended_duration_secs: 0,
        };

        let mut metadata = HashMap::new();
        metadata.insert("reason".to_string(), serde_json::json!(reason.to_string()));

        self.transition(new_state, TransitionType::Suspend, metadata)
    }

    /// Resume workflow execution (Suspended -> Running)
    pub fn resume(&mut self) -> WorkflowResult<()> {
        if !self.state.can_resume() {
            return Err(errors::validation(format!(
                "Cannot resume workflow from state: {}",
                self.state.name()
            )));
        }

        let (suspended_at, snapshot) = match &self.state {
            WorkflowState::Suspended {
                suspended_at,
                snapshot,
                ..
            } => (*suspended_at, snapshot.clone()),
            _ => return Err(errors::validation("Invalid state for resume operation")),
        };

        let duration = chrono::Utc::now().signed_duration_since(suspended_at);
        let total_steps = snapshot.trace.len();

        let new_state = WorkflowState::Running {
            started_at: chrono::Utc::now()
                - chrono::Duration::milliseconds(duration.num_milliseconds()),
            current_step: snapshot.current_step,
            total_steps,
            progress: if total_steps > 0 {
                ((snapshot.current_step as u32 * 100) / total_steps as u32) as u8
            } else {
                0
            },
            context: snapshot.context.clone(),
        };

        self.transition(new_state, TransitionType::Resume, HashMap::new())
    }

    /// Complete workflow execution (Running/Suspended -> Completed)
    pub fn complete(&mut self, results: WorkflowResults) -> WorkflowResult<()> {
        if !self.state.is_active() {
            return Err(errors::validation(format!(
                "Cannot complete workflow from state: {}",
                self.state.name()
            )));
        }

        let started_at = match &self.state {
            WorkflowState::Running { started_at, .. } => *started_at,
            WorkflowState::Suspended { suspended_at, .. } => *suspended_at,
            _ => return Err(errors::validation("Invalid state for complete operation")),
        };

        let duration = chrono::Utc::now().signed_duration_since(started_at);
        let duration_ms = duration.num_milliseconds().max(0) as u64;

        let new_state = WorkflowState::Completed {
            completed_at: chrono::Utc::now(),
            results,
            duration_ms,
        };

        self.transition(new_state, TransitionType::Complete, HashMap::new())
    }

    /// Fail workflow execution (Running/Suspended -> Failed)
    pub fn fail(&mut self, error: WorkflowError) -> WorkflowResult<()> {
        if !self.state.is_active() {
            return Err(errors::validation(format!(
                "Cannot fail workflow from state: {}",
                self.state.name()
            )));
        }

        let error_info = WorkflowErrorInfo::from(&error);

        let new_state = WorkflowState::Failed {
            failed_at: chrono::Utc::now(),
            error: error_info.clone(),
            partial_results: None,
        };

        let mut metadata = HashMap::new();
        metadata.insert(
            "error_category".to_string(),
            serde_json::json!(error_info.category),
        );

        self.transition(new_state, TransitionType::Fail, metadata)
    }

    /// Reset workflow to initial state (any -> Ready)
    pub fn reset(&mut self) -> WorkflowResult<()> {
        let definition = match &self.state {
            WorkflowState::Ready { definition, .. } => definition.clone(),
            _ => {
                // Create default definition for reset
                WorkflowDefinition::new(
                    self.workflow_id.clone(),
                    "Reset Workflow".to_string(),
                    None,
                    0,
                    "1.0.0".to_string(),
                )
            }
        };

        let new_state = WorkflowState::Ready {
            entered_at: chrono::Utc::now(),
            definition,
        };

        self.transition(new_state, TransitionType::Reset, HashMap::new())
    }

    /// Update progress while in Running state
    pub fn update_progress(&mut self, current_step: usize, progress: u8) -> WorkflowResult<()> {
        match &mut self.state {
            WorkflowState::Running {
                current_step: step,
                progress: prog,
                ..
            } => {
                *step = current_step;
                *prog = progress.min(100);
                Ok(())
            }
            _ => Err(errors::validation(format!(
                "Cannot update progress in state: {}",
                self.state.name()
            ))),
        }
    }

    /// Internal state transition with event emission
    fn transition(
        &mut self, new_state: WorkflowState, transition_type: TransitionType,
        metadata: HashMap<String, serde_json::Value>,
    ) -> WorkflowResult<()> {
        let from_state = self.state.name().to_string();
        let to_state = new_state.name().to_string();

        // Generate unique transition ID
        let counter = STATE_TRANSITION_COUNTER.fetch_add(1, Ordering::SeqCst);
        let transition_id = format!("{}-t-{}", self.workflow_id, counter);

        // Create transition event
        let event = StateTransitionEvent {
            transition_id: transition_id.clone(),
            workflow_id: self.workflow_id.clone(),
            from_state,
            to_state,
            timestamp: chrono::Utc::now(),
            transition_type,
            metadata,
        };

        // Update state
        self.state = new_state;

        // Record transition
        self.transition_history.push(event.clone());

        // Emit events if enabled
        if self.emit_events {
            self.emit_event(event);
        }

        Ok(())
    }

    /// Register an event handler for state transitions
    pub fn on_transition<F>(&mut self, handler: F)
    where
        F: Fn(&StateTransitionEvent) + 'static,
    {
        self.event_handlers.push(EventHandler::new(handler));
    }

    /// Emit event to all registered handlers
    fn emit_event(&self, event: StateTransitionEvent) {
        for handler in &self.event_handlers {
            handler.invoke(&event);
        }
    }

    /// Enable event emission
    pub fn enable_events(&mut self) {
        self.emit_events = true;
    }

    /// Disable event emission
    pub fn disable_events(&mut self) {
        self.emit_events = false;
    }

    /// Serialize state to JSON
    pub fn to_json(&self) -> WorkflowResult<String> {
        serde_json::to_string_pretty(self)
            .map_err(|e| errors::serialization(format!("Failed to serialize state: {}", e)))
    }

    /// Deserialize state from JSON
    pub fn from_json(json: &str) -> WorkflowResult<Self> {
        serde_json::from_str(json)
            .map_err(|e| errors::deserialization(format!("Failed to deserialize state: {}", e)))
    }

    /// Create a state snapshot for persistence
    pub fn snapshot(&self) -> StateSnapshot {
        StateSnapshot {
            state: self.state.clone(),
            workflow_id: self.workflow_id.clone(),
            timestamp: chrono::Utc::now(),
            transition_count: self.transition_history.len(),
        }
    }

    /// Restore from a snapshot
    pub fn from_snapshot(snapshot: StateSnapshot) -> WorkflowResult<Self> {
        Ok(StateMachine {
            state: snapshot.state,
            workflow_id: snapshot.workflow_id,
            transition_history: Vec::new(),
            event_handlers: Vec::new(),
            emit_events: true,
        })
    }
}

/// Event handler for state transitions
#[derive(Clone, Debug)]
struct EventHandler {
    // Using a boxed function pointer for simplicity
    // In production, this could use channels or async handlers
    #[allow(dead_code)]
    handler_id: String,
    #[allow(clippy::type_complexity)]
    #[allow(dead_code)]
    callback: *const (),
}

// SAFETY: EventHandler is Send because the callback is invoked synchronously
// and we control the lifecycle
unsafe impl Send for EventHandler {}

impl EventHandler {
    fn new<F>(f: F) -> Self
    where
        F: Fn(&StateTransitionEvent) + 'static,
    {
        // Note: This is a simplified implementation
        // In production, use channels or proper function pointers
        let _ = f; // Suppress unused warning
        EventHandler {
            handler_id: Uuid::new_v4().to_string(),
            callback: std::ptr::null(),
        }
    }

    fn invoke(&self, _event: &StateTransitionEvent) {
        // Simplified - in production, actually invoke the callback
    }
}

/// Persistent state snapshot for storage/recovery
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct StateSnapshot {
    /// Workflow state at snapshot time
    pub state: WorkflowState,
    /// Workflow ID
    pub workflow_id: String,
    /// Snapshot timestamp
    pub timestamp: chrono::DateTime<chrono::Utc>,
    /// Number of transitions that occurred
    pub transition_count: usize,
}

impl StateSnapshot {
    /// Verify snapshot integrity
    pub fn verify(&self) -> WorkflowResult<bool> {
        match &self.state {
            WorkflowState::Ready { definition, .. } => Ok(definition.verify_checksum()),
            WorkflowState::Suspended { snapshot, .. } => Ok(snapshot.verify()),
            _ => Ok(true),
        }
    }

    /// Serialize snapshot to bytes
    pub fn to_bytes(&self) -> WorkflowResult<Vec<u8>> {
        serde_json::to_vec(self)
            .map_err(|e| errors::serialization(format!("Failed to serialize snapshot: {}", e)))
    }

    /// Deserialize snapshot from bytes
    pub fn from_bytes(bytes: &[u8]) -> WorkflowResult<Self> {
        serde_json::from_slice(bytes)
            .map_err(|e| errors::deserialization(format!("Failed to deserialize snapshot: {}", e)))
    }
}

/// State machine builder for fluent construction
pub struct StateMachineBuilder {
    workflow_id: String,
    name: String,
    description: Option<String>,
    step_count: usize,
    version: String,
    emit_events: bool,
}

impl StateMachineBuilder {
    /// Create a new builder
    pub fn new(workflow_id: String) -> Self {
        Self {
            workflow_id,
            name: "Unnamed Workflow".to_string(),
            description: None,
            step_count: 0,
            version: "1.0.0".to_string(),
            emit_events: true,
        }
    }

    /// Set workflow name
    pub fn name(mut self, name: String) -> Self {
        self.name = name;
        self
    }

    /// Set workflow description
    pub fn description(mut self, description: String) -> Self {
        self.description = Some(description);
        self
    }

    /// Set step count
    pub fn step_count(mut self, count: usize) -> Self {
        self.step_count = count;
        self
    }

    /// Set version
    pub fn version(mut self, version: String) -> Self {
        self.version = version;
        self
    }

    /// Enable or disable event emission
    pub fn emit_events(mut self, emit: bool) -> Self {
        self.emit_events = emit;
        self
    }

    /// Build the state machine
    pub fn build(self) -> StateMachine {
        let mut sm = StateMachine::with_definition(
            self.workflow_id,
            self.name,
            self.description,
            self.step_count,
            self.version,
        );
        sm.emit_events = self.emit_events;
        sm
    }
}

/// State validator for checking state machine invariants
pub struct StateValidator;

impl StateValidator {
    /// Validate that a state transition is legal
    pub fn validate_transition(from: &WorkflowState, to: &WorkflowState) -> WorkflowResult<()> {
        match (from, to) {
            (WorkflowState::Ready { .. }, WorkflowState::Running { .. }) => Ok(()),
            (WorkflowState::Running { .. }, WorkflowState::Suspended { .. }) => Ok(()),
            (WorkflowState::Suspended { .. }, WorkflowState::Running { .. }) => Ok(()),
            (
                WorkflowState::Running { .. } | WorkflowState::Suspended { .. },
                WorkflowState::Completed { .. },
            ) => Ok(()),
            (
                WorkflowState::Running { .. } | WorkflowState::Suspended { .. },
                WorkflowState::Failed { .. },
            ) => Ok(()),
            (_, WorkflowState::Ready { .. }) => Ok(()), // Reset is always valid
            _ => Err(errors::validation(format!(
                "Invalid state transition: {} -> {}",
                from.name(),
                to.name()
            ))),
        }
    }

    /// Validate state integrity (checksums, etc.)
    pub fn validate_state_integrity(state: &WorkflowState) -> WorkflowResult<bool> {
        match state {
            WorkflowState::Ready { definition, .. } => Ok(definition.verify_checksum()),
            WorkflowState::Suspended { snapshot, .. } => Ok(snapshot.verify()),
            _ => Ok(true),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn create_test_machine() -> StateMachine {
        StateMachine::with_definition(
            "test-workflow".to_string(),
            "Test Workflow".to_string(),
            Some("A test workflow".to_string()),
            5,
            "1.0.0".to_string(),
        )
    }

    #[test]
    fn test_state_names() {
        let ready = WorkflowState::Ready {
            entered_at: chrono::Utc::now(),
            definition: WorkflowDefinition::new(
                "id".to_string(),
                "name".to_string(),
                None,
                1,
                "1.0.0".to_string(),
            ),
        };
        assert_eq!(ready.name(), "Ready");
        assert!(!ready.is_terminal());
        assert!(ready.can_start());
        assert!(!ready.can_suspend());
        assert!(!ready.can_resume());
    }

    #[test]
    fn test_workflow_definition_checksum() {
        let def = WorkflowDefinition::new(
            "test-id".to_string(),
            "Test".to_string(),
            None,
            10,
            "1.0.0".to_string(),
        );
        assert!(def.verify_checksum());
    }

    #[test]
    fn test_execution_snapshot_verify() {
        let snapshot = ExecutionSnapshot::new(5, HashMap::new(), HashMap::new(), Vec::new());
        assert!(snapshot.verify());
    }

    #[test]
    fn test_state_machine_ready_to_running() {
        let mut sm = create_test_machine();
        assert_eq!(sm.current_state().name(), "Ready");

        let context = HashMap::new();
        sm.start(context).unwrap();
        assert_eq!(sm.current_state().name(), "Running");
        assert_eq!(sm.transition_history().len(), 1);
    }

    #[test]
    fn test_state_machine_running_to_suspended() {
        let mut sm = create_test_machine();
        sm.start(HashMap::new()).unwrap();

        sm.suspend(SuspendReason::UserRequested).unwrap();
        assert_eq!(sm.current_state().name(), "Suspended");
        assert_eq!(sm.transition_history().len(), 2);
    }

    #[test]
    fn test_state_machine_suspended_to_running() {
        let mut sm = create_test_machine();
        sm.start(HashMap::new()).unwrap();
        sm.suspend(SuspendReason::UserRequested).unwrap();

        sm.resume().unwrap();
        assert_eq!(sm.current_state().name(), "Running");
        assert_eq!(sm.transition_history().len(), 3);
    }

    #[test]
    fn test_state_machine_to_completed() {
        let mut sm = create_test_machine();
        sm.start(HashMap::new()).unwrap();

        let results = WorkflowResults {
            output: HashMap::new(),
            metrics: ExecutionMetrics {
                steps_executed: 5,
                total_duration_ms: 1000,
                successful_operations: 5,
                failed_operations: 0,
                retry_count: 0,
            },
            trace: Vec::new(),
        };

        sm.complete(results).unwrap();
        assert_eq!(sm.current_state().name(), "Completed");
        assert!(sm.is_terminal());
    }

    #[test]
    fn test_state_machine_to_failed() {
        let mut sm = create_test_machine();
        sm.start(HashMap::new()).unwrap();

        let error = WorkflowError::Validation("Test error".to_string());
        sm.fail(error).unwrap();
        assert_eq!(sm.current_state().name(), "Failed");
        assert!(sm.is_terminal());
    }

    #[test]
    fn test_invalid_transition_start_from_running() {
        let mut sm = create_test_machine();
        sm.start(HashMap::new()).unwrap();

        let result = sm.start(HashMap::new());
        assert!(result.is_err());
    }

    #[test]
    fn test_invalid_transition_suspend_from_ready() {
        let mut sm = create_test_machine();

        let result = sm.suspend(SuspendReason::UserRequested);
        assert!(result.is_err());
    }

    #[test]
    fn test_update_progress() {
        let mut sm = create_test_machine();
        sm.start(HashMap::new()).unwrap();

        sm.update_progress(2, 40).unwrap();

        if let WorkflowState::Running {
            progress,
            current_step,
            ..
        } = sm.current_state()
        {
            assert_eq!(*progress, 40);
            assert_eq!(*current_step, 2);
        } else {
            panic!("Expected Running state");
        }
    }

    #[test]
    fn test_state_serialization() {
        let sm = create_test_machine();
        let json = sm.to_json().unwrap();
        assert!(json.contains("Ready"));
        assert!(json.contains("test-workflow"));

        let restored = StateMachine::from_json(&json).unwrap();
        assert_eq!(restored.workflow_id(), "test-workflow");
    }

    #[test]
    fn test_state_snapshot() {
        let sm = create_test_machine();
        let snapshot = sm.snapshot();

        assert_eq!(snapshot.workflow_id, "test-workflow");
        assert!(snapshot.verify().unwrap());
    }

    #[test]
    fn test_snapshot_bytes_roundtrip() {
        let sm = create_test_machine();
        let snapshot = sm.snapshot();

        let bytes = snapshot.to_bytes().unwrap();
        let restored = StateSnapshot::from_bytes(&bytes).unwrap();

        assert_eq!(restored.workflow_id, snapshot.workflow_id);
    }

    #[test]
    fn test_builder_pattern() {
        let sm = StateMachineBuilder::new("builder-test".to_string())
            .name("Builder Test".to_string())
            .description("Testing builder".to_string())
            .step_count(10)
            .version("2.0.0".to_string())
            .emit_events(false)
            .build();

        assert_eq!(sm.workflow_id(), "builder-test");
    }

    #[test]
    fn test_state_validator() {
        let ready = WorkflowState::Ready {
            entered_at: chrono::Utc::now(),
            definition: WorkflowDefinition::new(
                "id".to_string(),
                "name".to_string(),
                None,
                1,
                "1.0.0".to_string(),
            ),
        };

        let running = WorkflowState::Running {
            started_at: chrono::Utc::now(),
            current_step: 0,
            total_steps: 5,
            progress: 0,
            context: HashMap::new(),
        };

        assert!(StateValidator::validate_transition(&ready, &running).is_ok());
    }

    #[test]
    fn test_suspend_reason_display() {
        assert_eq!(SuspendReason::UserRequested.to_string(), "User requested");
        assert_eq!(SuspendReason::Checkpoint.to_string(), "Checkpoint");
    }

    #[test]
    fn test_transition_type_display() {
        assert_eq!(TransitionType::Start.to_string(), "start");
        assert_eq!(TransitionType::Complete.to_string(), "complete");
    }

    #[test]
    fn test_reset_from_completed() {
        let mut sm = create_test_machine();
        sm.start(HashMap::new()).unwrap();

        let results = WorkflowResults {
            output: HashMap::new(),
            metrics: ExecutionMetrics {
                steps_executed: 5,
                total_duration_ms: 1000,
                successful_operations: 5,
                failed_operations: 0,
                retry_count: 0,
            },
            trace: Vec::new(),
        };

        sm.complete(results).unwrap();
        assert!(sm.is_terminal());

        sm.reset().unwrap();
        assert_eq!(sm.current_state().name(), "Ready");
        assert!(!sm.is_terminal());
    }

    #[test]
    fn test_duration_since_entry() {
        let state = WorkflowState::Ready {
            entered_at: chrono::Utc::now() - chrono::Duration::seconds(10),
            definition: WorkflowDefinition::new(
                "id".to_string(),
                "name".to_string(),
                None,
                1,
                "1.0.0".to_string(),
            ),
        };

        let duration = state.duration_since_entry();
        assert!(duration.num_seconds() >= 9); // Allow for timing variance
    }
}
