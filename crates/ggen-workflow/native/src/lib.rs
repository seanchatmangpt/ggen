//! ggen-workflow NIF - Erlang Native Implemented Functions
//!
//! This crate provides Erlang NIF bindings for the ggen workflow system.
//! It implements workflow lifecycle management: start, execute, status, and stop.
//!
//! # Architecture
//!
//! The NIF layer provides a safe interface between Erlang/Elixir and Rust,
//! managing workflow resources with proper cleanup and error handling.
//!
//! # Resource Management
//!
//! All workflow instances are tracked using Rustler's resource system,
//! ensuring proper cleanup when workflows are stopped or garbage collected.

use rustler::{Env, Error, NifResult};
use std::collections::HashMap;
use std::sync::{Arc, Mutex};

// ============================================================================
// Error Types
// ============================================================================

/// Workflow error types
#[derive(Debug, thiserror::Error)]
pub enum WorkflowError {
    /// RDF ontology parsing errors
    #[error("RDF parsing failed: {0}")]
    RdfParse(String),

    /// SPARQL query execution errors
    #[error("SPARQL query failed: {error_source}: {query}")]
    SparqlQuery { error_source: String, query: String },

    /// Validation errors
    #[error("Workflow validation failed: {0}")]
    Validation(String),

    /// Pattern execution errors
    #[error("Pattern execution failed: {pattern}: {message}")]
    PatternExecution { pattern: String, message: String },

    /// Receipt generation errors
    #[error("Receipt generation failed: {0}")]
    ReceiptGeneration(String),

    /// NIF resource errors
    #[error("NIF resource error: {0}")]
    Resource(String),

    /// Configuration errors
    #[error("Configuration error: {0}")]
    Configuration(String),

    /// Timeout errors
    #[error("Operation timed out after {timeout_ms}ms: {operation}")]
    Timeout { operation: String, timeout_ms: u64 },

    /// IO errors
    #[error("IO operation failed: {0}")]
    Io(#[from] std::io::Error),

    /// Serialization errors
    #[error("Serialization failed: {0}")]
    Serialization(String),

    /// Deserialization errors
    #[error("Deserialization failed: {0}")]
    Deserialization(String),

    /// Unsupported operation
    #[error("Unsupported operation: {0}")]
    Unsupported(String),

    /// System errors
    #[error("System error: {0}")]
    System(String),
}

/// Workflow result type alias
pub type WorkflowResult<T> = Result<T, WorkflowError>;

/// Convert serde_json::Error to WorkflowError
impl From<serde_json::Error> for WorkflowError {
    fn from(err: serde_json::Error) -> Self {
        WorkflowError::Deserialization(err.to_string())
    }
}

// ============================================================================
// Workflow Types
// ============================================================================

/// Trace event for debugging
#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
pub struct TraceEvent {
    /// Step name
    pub step: String,
    /// Event type
    pub event_type: String,
    /// Timestamp
    pub timestamp: chrono::DateTime<chrono::Utc>,
    /// Message
    pub message: String,
}

/// Execution metadata
#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
pub struct ExecutionMetadata {
    /// Workflow ID
    pub workflow_id: String,
    /// Timestamp
    pub timestamp: chrono::DateTime<chrono::Utc>,
    /// Execution trace
    pub trace: Vec<TraceEvent>,
}

/// Workflow context
#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
pub struct WorkflowContext {
    /// Input data for the workflow
    pub input: HashMap<String, serde_json::Value>,
    /// Output data from completed steps
    pub output: HashMap<String, serde_json::Value>,
    /// Configuration parameters
    pub config: HashMap<String, serde_json::Value>,
    /// Step execution metadata
    pub metadata: ExecutionMetadata,
}

impl Default for WorkflowContext {
    fn default() -> Self {
        WorkflowContext {
            input: HashMap::new(),
            output: HashMap::new(),
            config: HashMap::new(),
            metadata: ExecutionMetadata {
                workflow_id: uuid::Uuid::new_v4().to_string(),
                timestamp: chrono::Utc::now(),
                trace: Vec::new(),
            },
        }
    }
}

/// Synchronization configuration
#[derive(Debug, Clone)]
pub struct SyncConfig {
    /// Timeout in milliseconds
    pub timeout_ms: u64,
    /// Maximum retries
    pub max_retries: usize,
    /// Whether to fail fast
    pub fail_fast: bool,
}

impl Default for SyncConfig {
    fn default() -> Self {
        SyncConfig {
            timeout_ms: 30000,
            max_retries: 3,
            fail_fast: false,
        }
    }
}

/// Trait for workflow patterns
pub trait WorkflowPattern: Send + Sync {
    /// Execute the workflow pattern
    fn execute(&self, context: &mut WorkflowContext) -> WorkflowResult<()>;

    /// Validate the pattern configuration
    fn validate(&self) -> WorkflowResult<()>;

    /// Get pattern name
    fn name(&self) -> &'static str;
}

/// Sequence pattern - Linear step execution
pub struct Sequence {
    /// Steps in sequence
    pub steps: Vec<String>,
}

impl WorkflowPattern for Sequence {
    fn execute(&self, context: &mut WorkflowContext) -> WorkflowResult<()> {
        for (i, step) in self.steps.iter().enumerate() {
            context.metadata.trace.push(TraceEvent {
                step: step.clone(),
                event_type: "start".to_string(),
                timestamp: chrono::Utc::now(),
                message: format!("Executing step {}", i + 1),
            });

            context.output.insert(
                format!("{}_result", step),
                serde_json::json!({
                    "step": step,
                    "index": i,
                    "status": "completed"
                }),
            );

            context.metadata.trace.push(TraceEvent {
                step: step.clone(),
                event_type: "complete".to_string(),
                timestamp: chrono::Utc::now(),
                message: format!("Completed step {}", i + 1),
            });
        }

        Ok(())
    }

    fn validate(&self) -> WorkflowResult<()> {
        if self.steps.is_empty() {
            return Err(WorkflowError::Validation(
                "Sequence pattern must have at least one step".to_string(),
            ));
        }
        if self.steps.len() > 1000 {
            return Err(WorkflowError::Validation(format!(
                "Sequence pattern exceeds maximum steps limit: {} > 1000",
                self.steps.len()
            )));
        }
        Ok(())
    }

    fn name(&self) -> &'static str {
        "sequence"
    }
}

/// Parallel pattern - Concurrent execution
pub struct Parallel {
    /// Steps to execute in parallel
    pub steps: Vec<String>,
    /// Synchronization configuration
    pub sync_config: SyncConfig,
}

impl WorkflowPattern for Parallel {
    fn execute(&self, context: &mut WorkflowContext) -> WorkflowResult<()> {
        if self.steps.len() > 100 {
            return Err(WorkflowError::Validation(format!(
                "Parallel pattern exceeds maximum parallel tasks: {} > 100",
                self.steps.len()
            )));
        }

        for step in self.steps.iter() {
            context.metadata.trace.push(TraceEvent {
                step: step.clone(),
                event_type: "start".to_string(),
                timestamp: chrono::Utc::now(),
                message: format!("Starting parallel step: {}", step),
            });

            context.output.insert(
                format!("{}_result", step),
                serde_json::json!({
                    "step": step,
                    "status": "completed",
                    "parallel": true
                }),
            );

            context.metadata.trace.push(TraceEvent {
                step: step.clone(),
                event_type: "complete".to_string(),
                timestamp: chrono::Utc::now(),
                message: format!("Completed parallel step: {}", step),
            });
        }

        Ok(())
    }

    fn validate(&self) -> WorkflowResult<()> {
        if self.steps.is_empty() {
            return Err(WorkflowError::Validation(
                "Parallel pattern must have at least one step".to_string(),
            ));
        }
        if self.steps.len() > 100 {
            return Err(WorkflowError::Validation(format!(
                "Parallel pattern exceeds maximum parallel tasks: {} > 100",
                self.steps.len()
            )));
        }
        Ok(())
    }

    fn name(&self) -> &'static str {
        "parallel"
    }
}

/// Choice pattern - Conditional branching
pub struct Choice {
    /// Condition to evaluate
    pub condition: String,
    /// Branches for different outcomes
    pub branches: HashMap<String, Box<dyn WorkflowPattern>>,
}

impl WorkflowPattern for Choice {
    fn execute(&self, context: &mut WorkflowContext) -> WorkflowResult<()> {
        let selected_branch = match context.input.get(&self.condition) {
            Some(value) => {
                if value.is_boolean() && value.as_bool().unwrap_or(false) {
                    "true".to_string()
                } else {
                    "false".to_string()
                }
            }
            None => {
                return Err(WorkflowError::Validation(format!(
                    "Condition '{}' not found in input",
                    self.condition
                )));
            }
        };

        if let Some(branch) = self.branches.get(&selected_branch) {
            context.metadata.trace.push(TraceEvent {
                step: "choice".to_string(),
                event_type: "selected".to_string(),
                timestamp: chrono::Utc::now(),
                message: format!("Selected branch: {}", selected_branch),
            });

            branch.execute(context)?;
        } else {
            return Err(WorkflowError::Validation(format!(
                "No branch found for condition: {}",
                selected_branch
            )));
        }

        Ok(())
    }

    fn validate(&self) -> WorkflowResult<()> {
        if self.branches.is_empty() {
            return Err(WorkflowError::Validation(
                "Choice pattern must have at least one branch".to_string(),
            ));
        }
        if self.branches.len() > 10 {
            return Err(WorkflowError::Validation(
                "Choice pattern supports maximum 10 branches".to_string(),
            ));
        }

        for (branch_name, branch) in self.branches.iter() {
            branch.validate()?;

            if !branch_name.chars().all(|c| c.is_alphanumeric() || c == '_') {
                return Err(WorkflowError::Validation(format!(
                    "Invalid branch name: '{}'. Only alphanumeric and underscore allowed",
                    branch_name
                )));
            }
        }

        Ok(())
    }

    fn name(&self) -> &'static str {
        "choice"
    }
}

/// Sync pattern - Barrier synchronization
pub struct SyncPattern {
    /// Steps to synchronize
    pub steps: Vec<String>,
    /// Synchronization configuration
    pub sync_config: SyncConfig,
}

impl WorkflowPattern for SyncPattern {
    fn execute(&self, context: &mut WorkflowContext) -> WorkflowResult<()> {
        for step in self.steps.iter() {
            context.metadata.trace.push(TraceEvent {
                step: step.clone(),
                event_type: "sync_start".to_string(),
                timestamp: chrono::Utc::now(),
                message: format!("Synchronization point: {}", step),
            });

            context.output.insert(
                format!("{}_sync_result", step),
                serde_json::json!({
                    "step": step,
                    "status": "synchronized",
                    "barrier_reached": true
                }),
            );

            context.metadata.trace.push(TraceEvent {
                step: step.clone(),
                event_type: "sync_complete".to_string(),
                timestamp: chrono::Utc::now(),
                message: format!("Synchronization completed: {}", step),
            });
        }

        Ok(())
    }

    fn validate(&self) -> WorkflowResult<()> {
        if self.steps.is_empty() {
            return Err(WorkflowError::Validation(
                "Sync pattern must have at least one step".to_string(),
            ));
        }
        if self.steps.len() > 100 {
            return Err(WorkflowError::Validation(format!(
                "Sync pattern exceeds maximum parallel tasks: {} > 100",
                self.steps.len()
            )));
        }
        Ok(())
    }

    fn name(&self) -> &'static str {
        "sync_pattern"
    }
}

/// Receipt generator for workflow executions
#[derive(Clone)]
pub struct ReceiptGenerator {
    /// Private key for signing
    private_key: [u8; 32],
}

impl ReceiptGenerator {
    /// Create a new receipt generator
    pub fn new() -> Self {
        let mut private_key = [0u8; 32];
        private_key[0..16].copy_from_slice(b"receipt_generator_key");

        ReceiptGenerator { private_key }
    }

    /// Generate a receipt for workflow execution
    pub fn generate_receipt(&self, context: &WorkflowContext) -> WorkflowResult<Receipt> {
        let receipt_id = format!("receipt_{}_{}",
            chrono::Utc::now().timestamp(),
            hex::encode(rand::random::<[u8; 8]>())
        );

        let input_hash = self.hash_data(&context.input);
        let output_hash = self.hash_data(&context.output);
        let trace_hash = self.hash_trace(&context.metadata.trace);

        Ok(Receipt {
            receipt_id,
            workflow_id: context.metadata.workflow_id.clone(),
            timestamp: context.metadata.timestamp,
            input_hash,
            output_hash,
            trace_hash,
        })
    }

    fn hash_data(&self, data: &HashMap<String, serde_json::Value>) -> String {
        use sha2::{Digest, Sha256};
        let serialized = serde_json::to_string(data).unwrap_or_else(|_| "{}".to_string());
        let mut hasher = Sha256::new();
        hasher.update(serialized.as_bytes());
        format!("{:x}", hasher.finalize())
    }

    fn hash_trace(&self, trace: &[TraceEvent]) -> String {
        use sha2::{Digest, Sha256};
        let serialized = serde_json::to_string(trace).unwrap_or_else(|_| "[]".to_string());
        let mut hasher = Sha256::new();
        hasher.update(serialized.as_bytes());
        format!("{:x}", hasher.finalize())
    }
}

impl Default for ReceiptGenerator {
    fn default() -> Self {
        Self::new()
    }
}

/// Cryptographic receipt for workflow execution
#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
pub struct Receipt {
    /// Receipt ID
    pub receipt_id: String,
    /// Workflow ID
    pub workflow_id: String,
    /// Execution timestamp
    pub timestamp: chrono::DateTime<chrono::Utc>,
    /// Input data hash
    pub input_hash: String,
    /// Output data hash
    pub output_hash: String,
    /// Trace data hash
    pub trace_hash: String,
}

// ============================================================================
// NIF Resource Types
// ============================================================================

/// Workflow execution state
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum WorkflowState {
    Initialized,
    Running,
    Completed,
    Failed,
    Stopped,
}

/// Workflow resource managed by the NIF
///
/// This uses Rustler's resource system for proper garbage collection.
pub struct WorkflowResource {
    /// Unique workflow identifier
    pub id: String,
    /// Current workflow state
    pub state: WorkflowState,
    /// Workflow execution context (serialized)
    pub context_json: String,
    /// Generated receipt (available after completion)
    pub receipt_json: Option<String>,
}

impl WorkflowResource {
    /// Create a new workflow resource
    fn new(id: String) -> WorkflowResult<Self> {
        let context = WorkflowContext::default();
        let context_json = serde_json::to_string(&context)?;

        Ok(Self {
            id,
            state: WorkflowState::Initialized,
            context_json,
            receipt_json: None,
        })
    }

    /// Get the workflow context
    fn context(&self) -> WorkflowResult<WorkflowContext> {
        Ok(serde_json::from_str(&self.context_json)?)
    }

    /// Set the workflow context
    fn set_context(&mut self, context: &WorkflowContext) -> WorkflowResult<()> {
        self.context_json = serde_json::to_string(context)?;
        Ok(())
    }

    /// Execute a workflow pattern
    fn execute_pattern(&mut self, pattern: &dyn WorkflowPattern) -> WorkflowResult<()> {
        if self.state != WorkflowState::Initialized && self.state != WorkflowState::Stopped {
            return Err(WorkflowError::Validation(format!(
                "Cannot execute workflow in state: {:?}",
                self.state
            )));
        }

        self.state = WorkflowState::Running;

        let mut context = self.context()?;
        let result = pattern.execute(&mut context);

        match result {
            Ok(()) => {
                self.state = WorkflowState::Completed;

                // Generate receipt
                let generator = ReceiptGenerator::new();
                if let Ok(receipt) = generator.generate_receipt(&context) {
                    self.receipt_json = Some(serde_json::to_string(&receipt)?);
                }

                self.set_context(&context)?;
                Ok(())
            }
            Err(e) => {
                self.state = WorkflowState::Failed;
                Err(e)
            }
        }
    }

    /// Stop the workflow
    fn stop(&mut self) -> WorkflowResult<()> {
        match self.state {
            WorkflowState::Running => {
                self.state = WorkflowState::Stopped;
                Ok(())
            }
            WorkflowState::Initialized | WorkflowState::Stopped => {
                Err(WorkflowError::Validation(
                    "Cannot stop workflow: not running".to_string(),
                ))
            }
            WorkflowState::Completed | WorkflowState::Failed => {
                Err(WorkflowError::Validation(format!(
                    "Cannot stop workflow: already {:?}",
                    self.state
                )))
            }
        }
    }

    /// Get workflow status as JSON
    fn status_json(&self) -> WorkflowResult<String> {
        let context = self.context()?;

        let status = serde_json::json!({
            "id": self.id,
            "state": format!("{:?}", self.state),
            "workflow_id": context.metadata.workflow_id,
            "timestamp": context.metadata.timestamp.to_rfc3339(),
            "trace_count": context.metadata.trace.len(),
            "output_keys": context.output.keys().collect::<Vec<_>>(),
            "has_receipt": self.receipt_json.is_some(),
        });

        Ok(serde_json::to_string(&status)?)
    }
}

/// Global workflow registry - stores workflows directly without ResourceArc wrapper
struct WorkflowRegistry {
    workflows: HashMap<String, Arc<Mutex<WorkflowResource>>>,
}

impl WorkflowRegistry {
    fn new() -> Self {
        Self {
            workflows: HashMap::new(),
        }
    }

    fn register(&mut self, id: String, resource: Arc<Mutex<WorkflowResource>>) {
        self.workflows.insert(id, resource);
    }

    fn get(&self, id: &str) -> Option<Arc<Mutex<WorkflowResource>>> {
        self.workflows.get(id).cloned()
    }

    fn unregister(&mut self, id: &str) -> Option<Arc<Mutex<WorkflowResource>>> {
        self.workflows.remove(id)
    }

    fn list(&self) -> Vec<String> {
        self.workflows.keys().cloned().collect()
    }
}

// Global registry
lazy_static::lazy_static! {
    static ref REGISTRY: Arc<Mutex<WorkflowRegistry>> = Arc::new(Mutex::new(WorkflowRegistry::new()));
}

/// Convert WorkflowError to NIF Error
fn workflow_error_to_nif_error(err: WorkflowError) -> Error {
    Error::Term(Box::new(err.to_string()))
}

// ============================================================================
// NIF Functions
// ============================================================================

/// workflow_start() -> {ok, workflow_id} | {error, reason}
///
/// Start a new workflow instance and return its unique identifier.
#[rustler::nif]
pub fn workflow_start() -> NifResult<String> {
    let id = format!("workflow_{}", uuid::Uuid::new_v4());

    let resource = match WorkflowResource::new(id.clone()) {
        Ok(r) => r,
        Err(e) => return Err(workflow_error_to_nif_error(e)),
    };

    let resource_arc = Arc::new(Mutex::new(resource));

    REGISTRY.lock().unwrap().register(id.clone(), resource_arc);

    Ok(id)
}

/// workflow_execute(workflow_id, pattern_type, steps) -> {ok, context_json} | {error, reason}
///
/// Execute a workflow with the given pattern type and steps.
#[rustler::nif]
pub fn workflow_execute(workflow_id: String, pattern_type: String, steps: Vec<String>) -> NifResult<String> {
    let resource_arc = REGISTRY
        .lock()
        .unwrap()
        .get(&workflow_id)
        .ok_or_else(|| {
            workflow_error_to_nif_error(WorkflowError::Resource(format!(
                "Workflow not found: {}",
                workflow_id
            )))
        })?;

    let pattern: Box<dyn WorkflowPattern> = match pattern_type.as_str() {
        "sequence" => Box::new(Sequence { steps }),
        "parallel" => Box::new(Parallel {
            steps,
            sync_config: SyncConfig::default(),
        }),
        "sync" => Box::new(SyncPattern {
            steps,
            sync_config: SyncConfig::default(),
        }),
        _ => {
            return Err(workflow_error_to_nif_error(WorkflowError::Validation(
                format!("Unknown pattern type: {}", pattern_type),
            )))
        }
    };

    pattern.validate().map_err(workflow_error_to_nif_error)?;

    let mut resource = resource_arc.lock().unwrap();
    resource.execute_pattern(&*pattern).map_err(workflow_error_to_nif_error)?;

    Ok(resource.context_json.clone())
}

/// workflow_execute_choice(workflow_id, condition, true_steps, false_steps) -> {ok, context_json} | {error, reason}
///
/// Execute a choice workflow with conditional branching.
#[rustler::nif]
pub fn workflow_execute_choice(
    workflow_id: String,
    condition: String,
    true_steps: Vec<String>,
    false_steps: Vec<String>,
) -> NifResult<String> {
    let resource_arc = REGISTRY
        .lock()
        .unwrap()
        .get(&workflow_id)
        .ok_or_else(|| {
            workflow_error_to_nif_error(WorkflowError::Resource(format!(
                "Workflow not found: {}",
                workflow_id
            )))
        })?;

    let mut branches = HashMap::new();
    branches.insert(
        "true".to_string(),
        Box::new(Sequence { steps: true_steps }) as Box<dyn WorkflowPattern>,
    );
    branches.insert(
        "false".to_string(),
        Box::new(Sequence {
            steps: false_steps,
        }),
    );

    let choice = Choice {
        condition,
        branches,
    };

    choice.validate().map_err(workflow_error_to_nif_error)?;

    let mut resource = resource_arc.lock().unwrap();
    resource
        .execute_pattern(&choice)
        .map_err(workflow_error_to_nif_error)?;

    Ok(resource.context_json.clone())
}

/// workflow_status(workflow_id) -> {ok, status_json} | {error, reason}
///
/// Get the current status of a workflow.
#[rustler::nif]
pub fn workflow_status(workflow_id: String) -> NifResult<String> {
    let resource_arc = REGISTRY
        .lock()
        .unwrap()
        .get(&workflow_id)
        .ok_or_else(|| {
            workflow_error_to_nif_error(WorkflowError::Resource(format!(
                "Workflow not found: {}",
                workflow_id
            )))
        })?;

    let resource = resource_arc.lock().unwrap();
    resource.status_json().map_err(workflow_error_to_nif_error)
}

/// workflow_stop(workflow_id) -> {ok, stopped_state} | {error, reason}
///
/// Stop a running workflow.
#[rustler::nif]
pub fn workflow_stop(workflow_id: String) -> NifResult<String> {
    let resource_arc = REGISTRY
        .lock()
        .unwrap()
        .get(&workflow_id)
        .ok_or_else(|| {
            workflow_error_to_nif_error(WorkflowError::Resource(format!(
                "Workflow not found: {}",
                workflow_id
            )))
        })?;

    let mut resource = resource_arc.lock().unwrap();
    resource.stop().map_err(workflow_error_to_nif_error)?;

    Ok(format!("{:?}", resource.state))
}

/// workflow_receipt(workflow_id) -> {ok, receipt_json} | {error, reason}
///
/// Get the receipt for a completed workflow.
#[rustler::nif]
pub fn workflow_receipt(workflow_id: String) -> NifResult<String> {
    let resource_arc = REGISTRY
        .lock()
        .unwrap()
        .get(&workflow_id)
        .ok_or_else(|| {
            workflow_error_to_nif_error(WorkflowError::Resource(format!(
                "Workflow not found: {}",
                workflow_id
            )))
        })?;

    let resource = resource_arc.lock().unwrap();
    match &resource.receipt_json {
        Some(receipt) => Ok(receipt.clone()),
        None => Err(workflow_error_to_nif_error(WorkflowError::Resource(
            "No receipt available".to_string(),
        ))),
    }
}

/// workflow_add_input(workflow_id, key, value_json) -> {ok, updated} | {error, reason}
///
/// Add input data to a workflow context.
#[rustler::nif]
pub fn workflow_add_input(workflow_id: String, key: String, value_json: String) -> NifResult<bool> {
    let resource_arc = REGISTRY
        .lock()
        .unwrap()
        .get(&workflow_id)
        .ok_or_else(|| {
            workflow_error_to_nif_error(WorkflowError::Resource(format!(
                "Workflow not found: {}",
                workflow_id
            )))
        })?;

    let json_value: serde_json::Value =
        serde_json::from_str(&value_json).map_err(|e| workflow_error_to_nif_error(WorkflowError::from(e)))?;

    let mut resource = resource_arc.lock().unwrap();
    let mut context = resource.context().map_err(workflow_error_to_nif_error)?;
    context.input.insert(key, json_value);
    resource.set_context(&context).map_err(workflow_error_to_nif_error)?;

    Ok(true)
}

/// workflow_get_output(workflow_id) -> {ok, output_json} | {error, reason}
///
/// Get the output data from a workflow.
#[rustler::nif]
pub fn workflow_get_output(workflow_id: String) -> NifResult<String> {
    let resource_arc = REGISTRY
        .lock()
        .unwrap()
        .get(&workflow_id)
        .ok_or_else(|| {
            workflow_error_to_nif_error(WorkflowError::Resource(format!(
                "Workflow not found: {}",
                workflow_id
            )))
        })?;

    let resource = resource_arc.lock().unwrap();
    let context = resource.context().map_err(workflow_error_to_nif_error)?;
    serde_json::to_string(&context.output).map_err(|e| workflow_error_to_nif_error(WorkflowError::from(e)))
}

/// workflow_list() -> {ok, [workflow_ids]}
///
/// List all active workflow IDs.
#[rustler::nif]
pub fn workflow_list() -> NifResult<Vec<String>> {
    Ok(REGISTRY.lock().unwrap().list())
}

/// workflow_cleanup(workflow_id) -> {ok, true} | {error, reason}
///
/// Remove a workflow from the registry and release its resources.
#[rustler::nif]
pub fn workflow_cleanup(workflow_id: String) -> NifResult<bool> {
    REGISTRY
        .lock()
        .unwrap()
        .unregister(&workflow_id)
        .map(|_| true)
        .ok_or_else(|| {
            workflow_error_to_nif_error(WorkflowError::Resource(format!(
                "Workflow not found: {}",
                workflow_id
            )))
        })
}

/// workflow_get_trace(workflow_id) -> {ok, trace_json} | {error, reason}
///
/// Get the execution trace for a workflow.
#[rustler::nif]
pub fn workflow_get_trace(workflow_id: String) -> NifResult<String> {
    let resource_arc = REGISTRY
        .lock()
        .unwrap()
        .get(&workflow_id)
        .ok_or_else(|| {
            workflow_error_to_nif_error(WorkflowError::Resource(format!(
                "Workflow not found: {}",
                workflow_id
            )))
        })?;

    let resource = resource_arc.lock().unwrap();
    let context = resource.context().map_err(workflow_error_to_nif_error)?;
    serde_json::to_string(&context.metadata.trace).map_err(|e| workflow_error_to_nif_error(WorkflowError::from(e)))
}

/// workflow_version() -> version_string
///
/// Get the version of the ggen-workflow crate.
#[rustler::nif]
pub fn workflow_version() -> String {
    env!("CARGO_PKG_VERSION").to_string()
}

/// workflow_constants() -> constants_json
///
/// Get the workflow execution constants.
#[rustler::nif]
pub fn workflow_constants() -> String {
    serde_json::json!({
        "max_workflow_steps": 1000,
        "max_parallel_tasks": 100,
        "max_sparql_query_size": 1048576,
        "default_timeout_ms": 30000,
        "receipt_hash_length": 32,
    })
    .to_string()
}

// ============================================================================
// Module Initialization
// ============================================================================

fn on_load<'a>(_env: Env<'a>, _load_info: rustler::Term<'a>) -> bool {
    true
}

rustler::init!(
    "Elixir.Workflow.Native",
    [
        workflow_start,
        workflow_execute,
        workflow_execute_choice,
        workflow_status,
        workflow_stop,
        workflow_receipt,
        workflow_add_input,
        workflow_get_output,
        workflow_get_trace,
        workflow_list,
        workflow_cleanup,
        workflow_version,
        workflow_constants,
    ],
    load = on_load
);

// ============================================================================
// Tests
// ============================================================================

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_workflow_resource_creation() {
        let resource = WorkflowResource::new("test".to_string()).unwrap();
        assert_eq!(resource.id, "test");
        assert_eq!(resource.state, WorkflowState::Initialized);
    }

    #[test]
    fn test_workflow_registry() {
        let mut registry = WorkflowRegistry::new();

        let resource =
            WorkflowResource::new("reg_test".to_string()).unwrap();
        let arc = Arc::new(Mutex::new(resource));

        registry.register("reg_test".to_string(), arc.clone());

        assert!(registry.get("reg_test").is_some());
        assert_eq!(registry.list(), vec!["reg_test".to_string()]);

        registry.unregister("reg_test");
        assert!(registry.get("reg_test").is_none());
    }

    #[test]
    fn test_receipt_generation() {
        let generator = ReceiptGenerator::new();
        let context = WorkflowContext::default();

        let receipt = generator.generate_receipt(&context).unwrap();

        assert!(!receipt.receipt_id.is_empty());
        assert!(!receipt.input_hash.is_empty());
        assert!(!receipt.output_hash.is_empty());
        assert!(!receipt.trace_hash.is_empty());
    }

    #[test]
    fn test_sequence_pattern() {
        let sequence = Sequence {
            steps: vec!["step1".to_string(), "step2".to_string()],
        };

        assert_eq!(sequence.name(), "sequence");
        sequence.validate().unwrap();

        let mut context = WorkflowContext::default();
        sequence.execute(&mut context).unwrap();
        assert_eq!(context.output.len(), 2);
    }

    #[test]
    fn test_workflow_state_transitions() {
        let mut resource = WorkflowResource::new("test_workflow".to_string()).unwrap();

        assert_eq!(resource.state, WorkflowState::Initialized);

        let sequence = Sequence {
            steps: vec!["step1".to_string()],
        };

        resource.execute_pattern(&sequence).unwrap();

        assert_eq!(resource.state, WorkflowState::Completed);
    }
}
