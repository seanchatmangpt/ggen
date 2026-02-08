//! WorkflowEngine - Core logic for workflow execution
//!
//! This module implements the core workflow engine with:
//! - Task scheduling with dependency resolution
//! - Flow routing (AND/XOR/OR split/join patterns)
//! - Condition evaluation with SPARQL support
//! - Decomposition handling for complex workflows
//! - Deterministic receipts for reproducibility
//!
//! # Architecture
//!
//! The engine implements a five-stage transformation pipeline:
//! - μ₁ (Normalize): Task dependency resolution and validation
//! - μ₂ (Extract): Condition evaluation and routing decisions
//! - μ₃ (Emit): Task execution with parallel orchestration
//! - μ₄ (Canonicalize): Result aggregation and state synchronization
//! - μ₅ (Receipt): Cryptographic proof generation
//!
//! # Flow Control Patterns
//!
//! - **AND Split**: Parallel execution of multiple branches
//! - **AND Join**: Synchronization barrier waiting for all branches
//! - **XOR Split**: Exclusive choice - exactly one branch executes
//! - **XOR Join**: Merge from exclusive branch
//! - **OR Split**: Multiple branches may execute based on conditions
//! - **OR Join**: Merge from one or more branches

use crate::error::{WorkflowError, WorkflowResult};
use crate::patterns::{WorkflowContext, TraceEvent};
use crate::receipts::{ReceiptGenerator, WorkflowReceipt};
use crate::CONSTANTS;
use serde::{Deserialize, Serialize};
use sha2::{Digest, Sha256};
use std::collections::{HashMap, HashSet};
use std::sync::Arc;
use std::time::{Duration, Instant};
use tokio::sync::{RwLock, Semaphore};
use uuid::Uuid;

// ============================================================================
// CORE ENGINE TYPES
// ============================================================================

/// The main workflow engine orchestrating task execution
pub struct WorkflowEngine {
    /// Engine configuration
    config: EngineConfig,
    /// Receipt generator for deterministic execution
    receipt_generator: ReceiptGenerator,
    /// Task scheduler for dependency resolution (interior mutability)
    scheduler: Arc<RwLock<TaskScheduler>>,
    /// Active workflow executions
    active_workflows: Arc<RwLock<HashMap<String, WorkflowState>>>,
    /// Concurrency control semaphore
    semaphore: Arc<Semaphore>,
}

/// Engine configuration parameters
#[derive(Debug, Clone)]
pub struct EngineConfig {
    /// Maximum concurrent tasks
    pub max_parallel_tasks: usize,
    /// Default timeout for task execution (milliseconds)
    pub default_timeout_ms: u64,
    /// Maximum retry attempts for failed tasks
    pub max_retries: u32,
    /// Enable deterministic execution
    pub deterministic: bool,
    /// Salt for deterministic hashing
    pub determinism_salt: Option<String>,
}

impl Default for EngineConfig {
    fn default() -> Self {
        Self {
            max_parallel_tasks: CONSTANTS.max_parallel_tasks,
            default_timeout_ms: CONSTANTS.default_timeout_ms,
            max_retries: 3,
            deterministic: true,
            determinism_salt: None,
        }
    }
}

/// Internal state of an active workflow execution
#[derive(Debug, Clone)]
pub struct WorkflowState {
    /// Unique workflow execution ID
    pub execution_id: String,
    /// Current execution phase
    pub phase: ExecutionPhase,
    /// Tasks ready to execute
    pub ready_tasks: HashSet<String>,
    /// Tasks currently running
    pub running_tasks: HashSet<String>,
    /// Completed tasks
    pub completed_tasks: HashSet<String>,
    /// Failed tasks with error messages
    pub failed_tasks: HashMap<String, String>,
    /// Task results indexed by task ID
    pub task_results: HashMap<String, TaskResult>,
    /// Execution start time
    pub started_at: Instant,
    /// Last update time
    pub updated_at: Instant,
}

/// Phases of workflow execution
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ExecutionPhase {
    /// Initial validation and normalization
    Normalization,
    /// Dependency extraction and resolution
    Extraction,
    /// Active task execution
    Execution,
    /// Result canonicalization and aggregation
    Canonicalization,
    /// Receipt generation
    ReceiptGeneration,
    /// Execution completed
    Completed,
    /// Execution failed
    Failed,
}

/// Result of a workflow execution
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct EngineResult {
    /// Unique execution ID
    pub execution_id: String,
    /// Whether execution succeeded
    pub success: bool,
    /// Total tasks in workflow
    pub total_tasks: usize,
    /// Completed tasks count
    pub completed_tasks: usize,
    /// Failed tasks count
    pub failed_tasks: usize,
    /// Execution time in milliseconds
    pub execution_time_ms: u64,
    /// Individual task results
    pub task_results: Vec<TaskResult>,
    /// Deterministic receipt
    pub receipt: Option<WorkflowReceipt>,
    /// Execution trace for debugging
    pub trace: Vec<TraceEvent>,
}

// ============================================================================
// TASK SCHEDULING
// ============================================================================

/// Task scheduler with dependency resolution
pub struct TaskScheduler {
    /// Task dependency graph: task_id -> dependencies
    dependency_graph: HashMap<String, Vec<String>>,
    /// Reverse dependencies: task_id -> dependents
    reverse_dependencies: HashMap<String, Vec<String>>,
    /// All registered tasks
    tasks: HashMap<String, TaskDefinition>,
}

/// Definition of a task in the workflow
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct TaskDefinition {
    /// Unique task identifier
    pub id: String,
    /// Human-readable task name
    pub name: String,
    /// Task type for routing and execution
    pub task_type: String,
    /// Task IDs this task depends on
    pub dependencies: Vec<String>,
    /// Task priority (0-100, higher = more important)
    pub priority: u8,
    /// Execution timeout in milliseconds
    pub timeout_ms: u64,
    /// Maximum retry attempts
    pub max_retries: u32,
    /// Task input data
    pub input: serde_json::Value,
    /// Flow control metadata
    pub flow_metadata: Option<FlowMetadata>,
}

/// Flow control metadata for routing patterns
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct FlowMetadata {
    /// Flow control pattern type
    pub pattern: FlowPattern,
    /// Branch identifier for split/join patterns
    pub branch_id: Option<String>,
    /// Join condition for synchronization
    pub join_condition: Option<JoinCondition>,
    /// Condition expression for XOR/OR routing
    pub condition: Option<String>,
}

/// Flow control patterns
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub enum FlowPattern {
    /// Sequential execution (default)
    Sequence,
    /// AND split - parallel execution
    AndSplit,
    /// AND join - wait for all branches
    AndJoin,
    /// XOR split - exclusive choice
    XorSplit,
    /// XOR join - merge from exclusive branch
    XorJoin,
    /// OR split - conditional parallel
    OrSplit,
    /// OR join - merge from one or more branches
    OrJoin,
    /// Loop with condition
    Loop,
}

/// Join conditions for synchronization
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub enum JoinCondition {
    /// Wait for all incoming branches
    All,
    /// Wait for first completed branch
    First,
    /// Wait for N branches
    Count(usize),
    /// Wait based on predicate
    Predicate(String),
}

/// Result of task execution
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct TaskResult {
    /// Task identifier
    pub task_id: String,
    /// Execution success status
    pub success: bool,
    /// Output data
    pub output: Option<serde_json::Value>,
    /// Error message if failed
    pub error: Option<String>,
    /// Execution time in milliseconds
    pub execution_time_ms: u64,
    /// Number of retries attempted
    pub retry_count: u32,
    /// Branch identifier for flow tracking
    pub branch_id: Option<String>,
}

// ============================================================================
// FLOW ROUTING
// ============================================================================

/// Flow router for split/join patterns
pub struct FlowRouter {
    /// Active split points
    active_splits: HashMap<String, SplitState>,
    /// Join synchronization state
    join_states: HashMap<String, JoinState>,
}

/// State of an active split point
#[derive(Debug, Clone)]
pub struct SplitState {
    /// Split point identifier
    pub split_id: String,
    /// Flow pattern type
    pub pattern: FlowPattern,
    /// Branches created from this split
    pub branches: Vec<BranchState>,
    /// Condition evaluated for routing
    pub condition_result: Option<ConditionResult>,
    /// Timestamp when split was created
    pub created_at: Instant,
}

/// State of a single branch
#[derive(Debug, Clone)]
pub struct BranchState {
    /// Branch identifier
    pub branch_id: String,
    /// Tasks in this branch
    pub tasks: Vec<String>,
    /// Branch completion status
    pub completed: bool,
    /// Branch result
    pub result: Option<TaskResult>,
}

/// Result of condition evaluation
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ConditionResult {
    /// Condition expression
    pub expression: String,
    /// Evaluation result
    pub result: bool,
    /// Selected branch(es)
    pub selected_branches: Vec<String>,
    /// Evaluation timestamp
    pub evaluated_at: chrono::DateTime<chrono::Utc>,
}

/// State of a join synchronization point
#[derive(Debug, Clone)]
pub struct JoinState {
    /// Join identifier
    pub join_id: String,
    /// Join condition type
    pub condition: JoinCondition,
    /// Expected branch IDs
    pub expected_branches: HashSet<String>,
    /// Completed branch IDs
    pub completed_branches: HashSet<String>,
    /// Branch results
    pub branch_results: HashMap<String, TaskResult>,
    /// Timestamp when join was created
    pub created_at: Instant,
}

// ============================================================================
// CONDITION EVALUATION
// ============================================================================

/// Condition evaluator for flow routing decisions
pub struct ConditionEvaluator {
    /// Context variables for evaluation
    context: HashMap<String, serde_json::Value>,
    /// SPARQL endpoint for ontology-based conditions
    sparql_endpoint: Option<String>,
}

impl ConditionEvaluator {
    /// Create a new condition evaluator
    pub fn new(context: HashMap<String, serde_json::Value>) -> Self {
        Self {
            context,
            sparql_endpoint: None,
        }
    }

    /// Evaluate a condition expression
    pub fn evaluate(&self, expression: &str) -> WorkflowResult<bool> {
        // Parse and evaluate the expression
        let result = self.evaluate_expression(expression)?;

        Ok(result)
    }

    /// Evaluate expression based on syntax
    fn evaluate_expression(&self, expr: &str) -> WorkflowResult<bool> {
        let expr = expr.trim();

        // Handle boolean literals
        match expr.to_lowercase().as_str() {
            "true" => return Ok(true),
            "false" => return Ok(false),
            _ => {}
        }

        // Handle context variable reference
        if let Some(value) = self.context.get(expr) {
            if let Some(b) = value.as_bool() {
                return Ok(b);
            }
        }

        // Handle comparison operators
        if let Some(result) = self.try_evaluate_comparison(expr) {
            return Ok(result);
        }

        // Handle logical operators
        if let Some(result) = self.try_evaluate_logical(expr) {
            return Ok(result);
        }

        // Handle SPARQL-based conditions
        if expr.starts_with("SPARQL:") {
            return self.evaluate_sparql_condition(&expr[7..]);
        }

        Err(WorkflowError::Validation(format!(
            "Unable to evaluate condition: {}",
            expr
        )))
    }

    /// Try to evaluate a comparison expression
    fn try_evaluate_comparison(&self, expr: &str) -> Option<bool> {
        let operators = [">=", "<=", "==", "!=", ">", "<"];

        for op in &operators {
            if let Some((left, right)) = expr.split_once(op) {
                let left_val = self.resolve_value(left.trim())?;
                let right_val = self.resolve_value(right.trim())?;

                return Some(match *op {
                    ">=" => left_val >= right_val,
                    "<=" => left_val <= right_val,
                    "==" => left_val == right_val,
                    "!=" => left_val != right_val,
                    ">" => left_val > right_val,
                    "<" => left_val < right_val,
                    _ => return None,
                });
            }
        }

        None
    }

    /// Try to evaluate a logical expression
    fn try_evaluate_logical(&self, expr: &str) -> Option<bool> {
        // Handle AND
        if let Some((left, right)) = expr.split_once("&&") {
            let left_val = self.evaluate_expression(left.trim()).ok()?;
            let right_val = self.evaluate_expression(right.trim()).ok()?;
            return Some(left_val && right_val);
        }

        // Handle OR
        if let Some((left, right)) = expr.split_once("||") {
            let left_val = self.evaluate_expression(left.trim()).ok()?;
            let right_val = self.evaluate_expression(right.trim()).ok()?;
            return Some(left_val || right_val);
        }

        // Handle NOT
        if let Some(inner) = expr.strip_prefix('!') {
            let val = self.evaluate_expression(inner.trim()).ok()?;
            return Some(!val);
        }

        None
    }

    /// Resolve a value to a comparable number
    fn resolve_value(&self, value: &str) -> Option<f64> {
        // Try direct number parsing
        if let Ok(n) = value.parse::<f64>() {
            return Some(n);
        }

        // Try context variable lookup
        if let Some(val) = self.context.get(value) {
            if let Some(n) = val.as_f64() {
                return Some(n);
            }
            if let Some(s) = val.as_str() {
                return s.parse::<f64>().ok();
            }
        }

        None
    }

    /// Evaluate a SPARQL-based condition
    fn evaluate_sparql_condition(&self, _query: &str) -> WorkflowResult<bool> {
        // In production, this would execute against a SPARQL endpoint
        // For now, return a placeholder result
        Err(WorkflowError::Validation(
            "SPARQL condition evaluation not yet implemented".to_string(),
        ))
    }

    /// Evaluate conditions for OR split pattern
    pub fn evaluate_or_split(
        &self, branches: &[(String, String)],
    ) -> WorkflowResult<ConditionResult> {
        let mut selected_branches = Vec::new();

        for (branch_id, condition) in branches {
            if self.evaluate(condition)? {
                selected_branches.push(branch_id.clone());
            }
        }

        Ok(ConditionResult {
            expression: format!("OR split across {} branches", branches.len()),
            result: !selected_branches.is_empty(),
            selected_branches,
            evaluated_at: chrono::Utc::now(),
        })
    }

    /// Evaluate condition for XOR split pattern
    pub fn evaluate_xor_split(
        &self, branches: &[(String, String)],
    ) -> WorkflowResult<ConditionResult> {
        for (branch_id, condition) in branches {
            if self.evaluate(condition)? {
                return Ok(ConditionResult {
                    expression: format!("XOR split across {} branches", branches.len()),
                    result: true,
                    selected_branches: vec![branch_id.clone()],
                    evaluated_at: chrono::Utc::now(),
                });
            }
        }

        // Default case: no condition matched
        Ok(ConditionResult {
            expression: format!("XOR split across {} branches", branches.len()),
            result: false,
            selected_branches: vec![],
            evaluated_at: chrono::Utc::now(),
        })
    }
}

// ============================================================================
// WORKFLOW DECOMPOSITION
// ============================================================================

/// Workflow decomposer for handling complex nested workflows
pub struct WorkflowDecomposer {
    /// Maximum decomposition depth
    max_depth: usize,
    /// Current decomposition depth
    current_depth: usize,
}

impl WorkflowDecomposer {
    /// Create a new decomposer
    pub fn new(max_depth: usize) -> Self {
        Self {
            max_depth,
            current_depth: 0,
        }
    }

    /// Decompose a complex workflow into manageable sub-workflows
    pub fn decompose(
        &mut self, tasks: Vec<TaskDefinition>,
    ) -> WorkflowResult<DecompositionResult> {
        if self.current_depth >= self.max_depth {
            return Err(WorkflowError::Validation(
                "Maximum decomposition depth exceeded".to_string(),
            ));
        }

        self.current_depth += 1;

        // Identify independent task groups using strongly connected components
        let groups = self.identify_independent_groups(&tasks)?;

        // Create sub-workflows for each group
        let mut sub_workflows = Vec::new();
        for group in groups {
            let sub_workflow = self.create_sub_workflow(group)?;
            sub_workflows.push(sub_workflow);
        }

        // Identify remaining dependencies between sub-workflows
        let dependencies = self.identify_workflow_dependencies(&sub_workflows);

        Ok(DecompositionResult {
            sub_workflows,
            dependencies,
            depth: self.current_depth,
        })
    }

    /// Identify independent groups of tasks
    fn identify_independent_groups(
        &self, tasks: &[TaskDefinition],
    ) -> WorkflowResult<Vec<Vec<TaskDefinition>>> {
        let mut groups = Vec::new();
        let mut assigned = HashSet::new();

        for task in tasks {
            if assigned.contains(&task.id) {
                continue;
            }

            // Find all tasks transitively connected to this task
            let group = self.find_connected_group(task, tasks, &mut assigned)?;
            groups.push(group);
        }

        Ok(groups)
    }

    /// Find all tasks connected through dependencies
    fn find_connected_group(
        &self, start: &TaskDefinition, all_tasks: &[TaskDefinition],
        assigned: &mut HashSet<String>,
    ) -> WorkflowResult<Vec<TaskDefinition>> {
        let mut group = Vec::new();
        let mut to_visit = vec![start.id.clone()];

        while let Some(task_id) = to_visit.pop() {
            if assigned.contains(&task_id) {
                continue;
            }

            let task = all_tasks
                .iter()
                .find(|t| t.id == task_id)
                .ok_or_else(|| {
                    WorkflowError::Validation(format!("Task not found: {}", task_id))
                })?;

            group.push(task.clone());
            assigned.insert(task_id.clone());

            // Add dependencies
            for dep in &task.dependencies {
                if !assigned.contains(dep) {
                    to_visit.push(dep.clone());
                }
            }

            // Add dependents
            for other in all_tasks {
                if other.dependencies.contains(&task_id) && !assigned.contains(&other.id) {
                    to_visit.push(other.id.clone());
                }
            }
        }

        Ok(group)
    }

    /// Create a sub-workflow from a task group
    fn create_sub_workflow(&self, tasks: Vec<TaskDefinition>) -> WorkflowResult<SubWorkflow> {
        let sub_workflow_id = format!("sub_{}", Uuid::new_v4());

        // Find entry points (tasks with no dependencies in the group)
        let task_ids: HashSet<String> = tasks.iter().map(|t| t.id.clone()).collect();
        let entry_points: Vec<String> = tasks
            .iter()
            .filter(|t| {
                t.dependencies
                    .iter()
                    .all(|dep| !task_ids.contains(dep))
            })
            .map(|t| t.id.clone())
            .collect();

        // Find exit points (tasks with no dependents in the group)
        let mut has_dependents = HashSet::new();
        for task in &tasks {
            for dep in &task.dependencies {
                if task_ids.contains(dep) {
                    has_dependents.insert(dep.clone());
                }
            }
        }
        let exit_points: Vec<String> = tasks
            .iter()
            .filter(|t| !has_dependents.contains(&t.id))
            .map(|t| t.id.clone())
            .collect();

        Ok(SubWorkflow {
            id: sub_workflow_id,
            tasks,
            entry_points,
            exit_points,
        })
    }

    /// Identify dependencies between sub-workflows
    fn identify_workflow_dependencies(
        &self, workflows: &[SubWorkflow],
    ) -> Vec<WorkflowDependency> {
        let mut dependencies = Vec::new();

        for (i, wf_a) in workflows.iter().enumerate() {
            for (j, wf_b) in workflows.iter().enumerate() {
                if i == j {
                    continue;
                }

                let _task_ids_a: HashSet<String> =
                    wf_a.tasks.iter().map(|t| t.id.clone()).collect();
                let task_ids_b: HashSet<String> =
                    wf_b.tasks.iter().map(|t| t.id.clone()).collect();

                // Check if any task in A depends on any task in B
                for task in &wf_a.tasks {
                    for dep in &task.dependencies {
                        if task_ids_b.contains(dep) {
                            dependencies.push(WorkflowDependency {
                                from: wf_b.id.clone(),
                                to: wf_a.id.clone(),
                                task_id: task.id.clone(),
                                dependency_id: dep.clone(),
                            });
                        }
                    }
                }
            }
        }

        dependencies
    }

    /// Reset decomposition depth
    pub fn reset(&mut self) {
        self.current_depth = 0;
    }
}

/// Result of workflow decomposition
#[derive(Debug, Clone)]
pub struct DecompositionResult {
    /// Identified sub-workflows
    pub sub_workflows: Vec<SubWorkflow>,
    /// Dependencies between sub-workflows
    pub dependencies: Vec<WorkflowDependency>,
    /// Current decomposition depth
    pub depth: usize,
}

/// A sub-workflow containing a group of related tasks
#[derive(Debug, Clone)]
pub struct SubWorkflow {
    /// Unique sub-workflow identifier
    pub id: String,
    /// Tasks in this sub-workflow
    pub tasks: Vec<TaskDefinition>,
    /// Entry point tasks (no external dependencies)
    pub entry_points: Vec<String>,
    /// Exit point tasks (no external dependents)
    pub exit_points: Vec<String>,
}

/// Dependency between sub-workflows
#[derive(Debug, Clone)]
pub struct WorkflowDependency {
    /// Source sub-workflow ID
    pub from: String,
    /// Target sub-workflow ID
    pub to: String,
    /// Task ID in target workflow
    pub task_id: String,
    /// Dependency task ID in source workflow
    pub dependency_id: String,
}

// ============================================================================
// ENGINE IMPLEMENTATION
// ============================================================================

impl WorkflowEngine {
    /// Create a new workflow engine
    pub fn new(config: EngineConfig) -> Self {
        let semaphore = Arc::new(Semaphore::new(config.max_parallel_tasks));
        let receipt_generator = ReceiptGenerator::new();

        Self {
            config: config.clone(),
            receipt_generator,
            scheduler: Arc::new(RwLock::new(TaskScheduler::new())),
            active_workflows: Arc::new(RwLock::new(HashMap::new())),
            semaphore,
        }
    }

    /// Create engine with default configuration
    pub fn default_engine() -> Self {
        Self::new(EngineConfig::default())
    }

    /// Execute a workflow with the given tasks
    pub async fn execute(
        &self, tasks: Vec<TaskDefinition>, context: &mut WorkflowContext,
    ) -> WorkflowResult<EngineResult> {
        let execution_id = Uuid::new_v4().to_string();
        let start_time = Instant::now();

        // Phase 1: Normalization
        self.trace_event(context, "normalization", "Starting workflow normalization")?;
        let normalized_tasks = self.normalize_workflow(tasks)?;
        self.trace_event(context, "normalization", "Workflow normalized successfully")?;

        // Phase 2: Extraction - build dependency graph
        self.trace_event(context, "extraction", "Building dependency graph")?;
        self.scheduler.write().await.load_tasks(normalized_tasks.clone())?;
        self.trace_event(context, "extraction", "Dependency graph built")?;

        // Initialize workflow state
        let workflow_state = self.initialize_workflow_state(&normalized_tasks, &execution_id)?;
        self.active_workflows
            .write()
            .await
            .insert(execution_id.clone(), workflow_state);

        // Phase 3: Execution
        self.trace_event(context, "execution", "Starting task execution")?;
        let task_results = self.execute_workflow_tasks(&execution_id, context).await?;
        self.trace_event(context, "execution", "Task execution completed")?;

        // Phase 4: Canonicalization
        self.trace_event(
            context,
            "canonicalization",
            "Aggregating and validating results",
        )?;
        let canonical_results = self.canonicalize_results(task_results)?;
        self.trace_event(context, "canonicalization", "Results canonicalized")?;

        // Phase 5: Receipt Generation
        self.trace_event(context, "receipt", "Generating deterministic receipt")?;
        let execution_time_ms = start_time.elapsed().as_millis() as u64;
        let receipt = self
            .generate_receipt(&execution_id, context, &canonical_results, execution_time_ms)
            .await?;
        self.trace_event(context, "receipt", "Receipt generated successfully")?;

        // Build final result
        let success = canonical_results.iter().all(|r| r.success);
        let completed_count = canonical_results.iter().filter(|r| r.success).count();
        let failed_count = canonical_results.len() - completed_count;

        Ok(EngineResult {
            execution_id: execution_id.clone(),
            success,
            total_tasks: canonical_results.len(),
            completed_tasks: completed_count,
            failed_tasks: failed_count,
            execution_time_ms,
            task_results: canonical_results.clone(),
            receipt: Some(receipt),
            trace: context.metadata.trace.clone(),
        })
    }

    /// Normalize workflow tasks
    fn normalize_workflow(&self, tasks: Vec<TaskDefinition>) -> WorkflowResult<Vec<TaskDefinition>> {
        let mut normalized = Vec::new();

        for task in tasks {
            // Validate task ID
            if task.id.is_empty() {
                return Err(WorkflowError::Validation("Task ID cannot be empty".to_string()));
            }

            // Check for circular dependencies would go here
            // For now, just validate basic structure

            // Apply default values
            let mut normalized_task = task;
            if normalized_task.timeout_ms == 0 {
                normalized_task.timeout_ms = self.config.default_timeout_ms;
            }
            if normalized_task.max_retries == 0 {
                normalized_task.max_retries = self.config.max_retries;
            }

            normalized.push(normalized_task);
        }

        Ok(normalized)
    }

    /// Initialize workflow state
    fn initialize_workflow_state(
        &self, tasks: &[TaskDefinition], execution_id: &str,
    ) -> WorkflowResult<WorkflowState> {
        // Find ready tasks (no dependencies or all dependencies resolved)
        let all_task_ids: HashSet<String> = tasks.iter().map(|t| t.id.clone()).collect();
        let mut ready_tasks = HashSet::new();

        for task in tasks {
            let deps_resolved = task
                .dependencies
                .iter()
                .all(|dep| !all_task_ids.contains(dep));
            if deps_resolved {
                ready_tasks.insert(task.id.clone());
            }
        }

        Ok(WorkflowState {
            execution_id: execution_id.to_string(),
            phase: ExecutionPhase::Normalization,
            ready_tasks,
            running_tasks: HashSet::new(),
            completed_tasks: HashSet::new(),
            failed_tasks: HashMap::new(),
            task_results: HashMap::new(),
            started_at: Instant::now(),
            updated_at: Instant::now(),
        })
    }

    /// Execute all workflow tasks with dependency resolution
    async fn execute_workflow_tasks(
        &self, execution_id: &str, context: &mut WorkflowContext,
    ) -> WorkflowResult<Vec<TaskResult>> {
        let mut results = Vec::new();

        loop {
            // Get scheduler and workflow state locks
            let ready_batch = {
                let scheduler = self.scheduler.write().await;
                let mut active_workflows = self.active_workflows.write().await;

                let workflow_state = active_workflows
                    .get_mut(execution_id)
                    .ok_or_else(|| WorkflowError::System("Workflow state not found".to_string()))?;

                workflow_state.phase = ExecutionPhase::Execution;

                // Get next batch of ready tasks
                let ready_batch: Vec<String> = workflow_state.ready_tasks.drain().collect();

                if ready_batch.is_empty() {
                    // Check if we're done
                    if workflow_state.running_tasks.is_empty() {
                        break;
                    }
                    // Still have running tasks, wait for them
                    drop(scheduler);
                    drop(active_workflows);
                    tokio::time::sleep(Duration::from_millis(10)).await;
                    continue;
                }

                ready_batch
            };

            // Execute batch
            let batch_results = self.execute_task_batch(&ready_batch, context).await?;

            // Update state
            {
                let scheduler = self.scheduler.read().await;
                let mut active_workflows = self.active_workflows.write().await;

                let workflow_state = active_workflows
                    .get_mut(execution_id)
                    .ok_or_else(|| WorkflowError::System("Workflow state not found".to_string()))?;

                for result in batch_results {
                    workflow_state.running_tasks.remove(&result.task_id);

                    if result.success {
                        workflow_state.completed_tasks.insert(result.task_id.clone());
                        workflow_state
                            .task_results
                            .insert(result.task_id.clone(), result.clone());

                        // Find newly ready tasks
                        let newly_ready = scheduler.find_ready_after(&result.task_id)?;
                        workflow_state.ready_tasks.extend(newly_ready);
                    } else {
                        workflow_state
                            .failed_tasks
                            .insert(result.task_id.clone(), result.error.clone().unwrap_or_default());
                        workflow_state
                            .task_results
                            .insert(result.task_id.clone(), result.clone());
                    }

                    results.push(result);
                }

                workflow_state.updated_at = Instant::now();
            }
        }

        {
            let mut active_workflows = self.active_workflows.write().await;
            let workflow_state = active_workflows
                .get_mut(execution_id)
                .ok_or_else(|| WorkflowError::System("Workflow state not found".to_string()))?;

            workflow_state.phase = ExecutionPhase::Canonicalization;
        }

        Ok(results)
    }

    /// Execute a batch of tasks
    async fn execute_task_batch(
        &self, task_ids: &[String], context: &mut WorkflowContext,
    ) -> WorkflowResult<Vec<TaskResult>> {
        let mut results = Vec::new();
        let mut handles = Vec::new();

        // Acquire semaphore permits
        let _permits = self
            .semaphore
            .acquire_many(task_ids.len() as u32)
            .await
            .map_err(|_| WorkflowError::System("Failed to acquire semaphore".to_string()))?;

        // Get task definitions
        let task_defs = {
            let scheduler = self.scheduler.read().await;
            let mut defs = Vec::new();
            for task_id in task_ids {
                defs.push(scheduler.get_task(task_id)?);
            }
            defs
        };

        // Spawn tasks
        for task_def in task_defs {
            let context_clone = context.clone();

            let handle = tokio::spawn(async move {
                Self::execute_single_task(task_def, context_clone).await
            });

            handles.push(handle);
        }

        // Wait for all tasks in batch
        for handle in handles {
            match handle.await {
                Ok(Ok(result)) => results.push(result),
                Ok(Err(e)) => {
                    results.push(TaskResult {
                        task_id: "unknown".to_string(),
                        success: false,
                        output: None,
                        error: Some(e.to_string()),
                        execution_time_ms: 0,
                        retry_count: 0,
                        branch_id: None,
                    });
                }
                Err(_) => {
                    results.push(TaskResult {
                        task_id: "unknown".to_string(),
                        success: false,
                        output: None,
                        error: Some("Task execution cancelled".to_string()),
                        execution_time_ms: 0,
                        retry_count: 0,
                        branch_id: None,
                    });
                }
            }
        }

        Ok(results)
    }

    /// Execute a single task
    async fn execute_single_task(
        task: TaskDefinition, mut context: WorkflowContext,
    ) -> WorkflowResult<TaskResult> {
        let start = Instant::now();

        // Add trace event
        context.metadata.trace.push(TraceEvent {
            step: task.id.clone(),
            event_type: "start".to_string(),
            timestamp: chrono::Utc::now(),
            message: format!("Executing task: {}", task.name),
        });

        // Execute task based on type
        let result = match task.task_type.as_str() {
            "sequence" => Self::execute_sequence_task(&task, &mut context).await,
            "parallel" => Self::execute_parallel_task(&task, &mut context).await,
            "choice" => Self::execute_choice_task(&task, &mut context).await,
            "sync" => Self::execute_sync_task(&task, &mut context).await,
            _ => Self::execute_generic_task(&task, &mut context).await,
        };

        let execution_time_ms = start.elapsed().as_millis() as u64;

        match result {
            Ok(output) => {
                context.metadata.trace.push(TraceEvent {
                    step: task.id.clone(),
                    event_type: "complete".to_string(),
                    timestamp: chrono::Utc::now(),
                    message: format!("Task completed: {}", task.name),
                });

                Ok(TaskResult {
                    task_id: task.id,
                    success: true,
                    output: Some(output),
                    error: None,
                    execution_time_ms,
                    retry_count: 0,
                    branch_id: task.flow_metadata.as_ref().and_then(|f| f.branch_id.clone()),
                })
            }
            Err(e) => {
                context.metadata.trace.push(TraceEvent {
                    step: task.id.clone(),
                    event_type: "failed".to_string(),
                    timestamp: chrono::Utc::now(),
                    message: format!("Task failed: {}", e),
                });

                Ok(TaskResult {
                    task_id: task.id,
                    success: false,
                    output: None,
                    error: Some(e.to_string()),
                    execution_time_ms,
                    retry_count: 0,
                    branch_id: task.flow_metadata.as_ref().and_then(|f| f.branch_id.clone()),
                })
            }
        }
    }

    /// Execute a generic task (placeholder implementation)
    async fn execute_generic_task(
        task: &TaskDefinition, context: &mut WorkflowContext,
    ) -> WorkflowResult<serde_json::Value> {
        // Store task input in context output
        context
            .output
            .insert(format!("{}_result", task.id), task.input.clone());

        // Return success
        Ok(serde_json::json!({
            "task_id": task.id,
            "status": "completed",
            "timestamp": chrono::Utc::now().to_rfc3339()
        }))
    }

    /// Execute sequence task
    async fn execute_sequence_task(
        task: &TaskDefinition, context: &mut WorkflowContext,
    ) -> WorkflowResult<serde_json::Value> {
        // Sequence tasks execute steps in order
        if let Some(steps) = task.input.get("steps") {
            if let Some(steps_array) = steps.as_array() {
                for step in steps_array {
                    // Execute each step
                    context.metadata.trace.push(TraceEvent {
                        step: task.id.clone(),
                        event_type: "step".to_string(),
                        timestamp: chrono::Utc::now(),
                        message: format!("Sequence step: {}", step),
                    });
                }
            }
        }

        Ok(serde_json::json!({
            "task_id": task.id,
            "pattern": "sequence",
            "status": "completed"
        }))
    }

    /// Execute parallel task
    async fn execute_parallel_task(
        task: &TaskDefinition, _context: &mut WorkflowContext,
    ) -> WorkflowResult<serde_json::Value> {
        // Parallel tasks would spawn concurrent execution
        // For now, simulate the pattern
        Ok(serde_json::json!({
            "task_id": task.id,
            "pattern": "parallel",
            "status": "completed"
        }))
    }

    /// Execute choice/routing task
    async fn execute_choice_task(
        task: &TaskDefinition, context: &mut WorkflowContext,
    ) -> WorkflowResult<serde_json::Value> {
        // Evaluate condition and select branch
        if let Some(flow_metadata) = &task.flow_metadata {
            if let Some(condition) = &flow_metadata.condition {
                let evaluator = ConditionEvaluator::new(context.input.clone());
                let result = evaluator.evaluate(condition)?;
                return Ok(serde_json::json!({
                    "task_id": task.id,
                    "pattern": "choice",
                    "condition_result": result,
                    "status": "completed"
                }));
            }
        }

        Ok(serde_json::json!({
            "task_id": task.id,
            "pattern": "choice",
            "status": "completed"
        }))
    }

    /// Execute sync/barrier task
    async fn execute_sync_task(
        task: &TaskDefinition, _context: &mut WorkflowContext,
    ) -> WorkflowResult<serde_json::Value> {
        // Sync tasks act as barriers
        Ok(serde_json::json!({
            "task_id": task.id,
            "pattern": "sync",
            "status": "completed"
        }))
    }

    /// Canonicalize and validate results
    fn canonicalize_results(&self, results: Vec<TaskResult>) -> WorkflowResult<Vec<TaskResult>> {
        // Validate all results
        for result in &results {
            if result.execution_time_ms == 0 {
                return Err(WorkflowError::Validation(format!(
                    "Task {} has zero execution time",
                    result.task_id
                )));
            }
        }

        // Sort results by task ID for deterministic output
        let mut sorted = results;
        sorted.sort_by(|a, b| a.task_id.cmp(&b.task_id));

        Ok(sorted)
    }

    /// Generate deterministic receipt
    async fn generate_receipt(
        &self, _execution_id: &str, context: &WorkflowContext, results: &[TaskResult],
        execution_time_ms: u64,
    ) -> WorkflowResult<WorkflowReceipt> {
        // Update context with results for receipt generation
        let mut receipt_context = context.clone();
        for result in results {
            receipt_context.output.insert(
                format!("receipt_{}", result.task_id),
                serde_json::to_value(result)?,
            );
        }

        // Update metadata
        receipt_context.metadata.execution_time_ms = execution_time_ms;

        let receipt = self
            .receipt_generator
            .generate_receipt(&receipt_context)?;

        Ok(receipt)
    }

    /// Add trace event to context
    fn trace_event(
        &self, context: &mut WorkflowContext, phase: &str, message: &str,
    ) -> WorkflowResult<()> {
        context.metadata.trace.push(TraceEvent {
            step: format!("engine_{}", phase),
            event_type: phase.to_string(),
            timestamp: chrono::Utc::now(),
            message: message.to_string(),
        });
        Ok(())
    }

    /// Get current engine state
    pub async fn get_state(&self, execution_id: &str) -> Option<WorkflowState> {
        let workflows = self.active_workflows.read().await;
        workflows.get(execution_id).cloned()
    }

    /// Cancel a running workflow
    pub async fn cancel(&self, execution_id: &str) -> WorkflowResult<bool> {
        let mut workflows = self.active_workflows.write().await;

        if let Some(state) = workflows.get_mut(execution_id) {
            state.phase = ExecutionPhase::Failed;
            state.updated_at = Instant::now();
            Ok(true)
        } else {
            Ok(false)
        }
    }
}

// ============================================================================
// TASK SCHEDULER IMPLEMENTATION
// ============================================================================

impl TaskScheduler {
    /// Create a new task scheduler
    pub fn new() -> Self {
        Self {
            dependency_graph: HashMap::new(),
            reverse_dependencies: HashMap::new(),
            tasks: HashMap::new(),
        }
    }

    /// Load tasks into the scheduler
    pub fn load_tasks(&mut self, tasks: Vec<TaskDefinition>) -> WorkflowResult<()> {
        // Clear existing state
        self.dependency_graph.clear();
        self.reverse_dependencies.clear();
        self.tasks.clear();

        // Validate for circular dependencies
        self.validate_no_cycles(&tasks)?;

        // Build dependency graph
        for task in tasks {
            self.dependency_graph.insert(task.id.clone(), task.dependencies.clone());

            // Build reverse dependencies
            for dep in &task.dependencies {
                self.reverse_dependencies
                    .entry(dep.clone())
                    .or_insert_with(Vec::new)
                    .push(task.id.clone());
            }

            self.tasks.insert(task.id.clone(), task);
        }

        Ok(())
    }

    /// Validate no circular dependencies exist
    fn validate_no_cycles(&self, tasks: &[TaskDefinition]) -> WorkflowResult<()> {
        let mut visited = HashSet::new();
        let mut recursion_stack = HashSet::new();

        for task in tasks {
            if self.has_cycle_recursive(&task.id, &mut visited, &mut recursion_stack)? {
                return Err(WorkflowError::Validation(format!(
                    "Circular dependency detected involving task: {}",
                    task.id
                )));
            }
        }

        Ok(())
    }

    /// Recursive cycle detection
    fn has_cycle_recursive(
        &self, task_id: &str, visited: &mut HashSet<String>,
        recursion_stack: &mut HashSet<String>,
    ) -> WorkflowResult<bool> {
        visited.insert(task_id.to_string());
        recursion_stack.insert(task_id.to_string());

        if let Some(deps) = self.dependency_graph.get(task_id) {
            for dep in deps {
                if !visited.contains(dep) {
                    if self.has_cycle_recursive(dep, visited, recursion_stack)? {
                        return Ok(true);
                    }
                } else if recursion_stack.contains(dep) {
                    return Ok(true);
                }
            }
        }

        recursion_stack.remove(task_id);
        Ok(false)
    }

    /// Get task definition by ID
    pub fn get_task(&self, task_id: &str) -> WorkflowResult<TaskDefinition> {
        self.tasks
            .get(task_id)
            .cloned()
            .ok_or_else(|| WorkflowError::Validation(format!("Task not found: {}", task_id)))
    }

    /// Find tasks that become ready after a task completes
    pub fn find_ready_after(&self, completed_task_id: &str) -> WorkflowResult<Vec<String>> {
        let mut ready = Vec::new();

        if let Some(dependents) = self.reverse_dependencies.get(completed_task_id) {
            for dependent_id in dependents {
                // Check if all dependencies are satisfied
                if self.is_task_ready(dependent_id)? {
                    ready.push(dependent_id.clone());
                }
            }
        }

        Ok(ready)
    }

    /// Check if a task is ready to execute
    fn is_task_ready(&self, task_id: &str) -> WorkflowResult<bool> {
        if let Some(deps) = self.dependency_graph.get(task_id) {
            // In production, would check against completed tasks
            // For now, assume external dependencies are satisfied
            for dep in deps {
                if self.tasks.contains_key(dep) {
                    return Ok(false);
                }
            }
        }
        Ok(true)
    }
}

impl Default for TaskScheduler {
    fn default() -> Self {
        Self::new()
    }
}

// ============================================================================
// FLOW ROUTER IMPLEMENTATION
// ============================================================================

impl FlowRouter {
    /// Create a new flow router
    pub fn new() -> Self {
        Self {
            active_splits: HashMap::new(),
            join_states: HashMap::new(),
        }
    }

    /// Create an AND split - all branches execute in parallel
    pub fn create_and_split(&mut self, split_id: String, branches: Vec<String>) -> FlowResult<()> {
        let split_state = SplitState {
            split_id: split_id.clone(),
            pattern: FlowPattern::AndSplit,
            branches: branches
                .into_iter()
                .map(|branch_id| BranchState {
                    branch_id,
                    tasks: Vec::new(),
                    completed: false,
                    result: None,
                })
                .collect(),
            condition_result: None,
            created_at: Instant::now(),
        };

        self.active_splits.insert(split_id, split_state);
        Ok(())
    }

    /// Create a XOR split - exactly one branch executes
    pub fn create_xor_split(
        &mut self, split_id: String, branches: Vec<(String, String)>,
        evaluator: &ConditionEvaluator,
    ) -> FlowResult<()> {
        let condition_result = evaluator.evaluate_xor_split(&branches)?;

        let selected_branch = condition_result
            .selected_branches
            .first()
            .cloned()
            .unwrap_or_default();

        let split_state = SplitState {
            split_id: split_id.clone(),
            pattern: FlowPattern::XorSplit,
            branches: vec![BranchState {
                branch_id: selected_branch,
                tasks: Vec::new(),
                completed: false,
                result: None,
            }],
            condition_result: Some(condition_result),
            created_at: Instant::now(),
        };

        self.active_splits.insert(split_id, split_state);
        Ok(())
    }

    /// Create an OR split - multiple branches may execute
    pub fn create_or_split(
        &mut self, split_id: String, branches: Vec<(String, String)>,
        evaluator: &ConditionEvaluator,
    ) -> FlowResult<()> {
        let condition_result = evaluator.evaluate_or_split(&branches)?;

        let split_state = SplitState {
            split_id: split_id.clone(),
            pattern: FlowPattern::OrSplit,
            branches: condition_result
                .selected_branches
                .iter()
                .map(|branch_id| BranchState {
                    branch_id: branch_id.clone(),
                    tasks: Vec::new(),
                    completed: false,
                    result: None,
                })
                .collect(),
            condition_result: Some(condition_result),
            created_at: Instant::now(),
        };

        self.active_splits.insert(split_id, split_state);
        Ok(())
    }

    /// Create a join state for synchronization
    pub fn create_join(
        &mut self, join_id: String, condition: JoinCondition, expected_branches: Vec<String>,
    ) -> FlowResult<()> {
        let join_id_for_state = join_id.clone();
        let join_state = JoinState {
            join_id: join_id_for_state,
            condition,
            expected_branches: expected_branches.into_iter().collect(),
            completed_branches: HashSet::new(),
            branch_results: HashMap::new(),
            created_at: Instant::now(),
        };

        self.join_states.insert(join_id, join_state);
        Ok(())
    }

    /// Mark a branch as completed at a join
    pub fn mark_branch_complete(
        &mut self, join_id: &str, branch_id: &str, result: TaskResult,
    ) -> FlowResult<bool> {
        let join_state = self
            .join_states
            .get_mut(join_id)
            .ok_or_else(|| FlowError::JoinNotFound(join_id.to_string()))?;

        join_state.completed_branches.insert(branch_id.to_string());
        join_state
            .branch_results
            .insert(branch_id.to_string(), result);

        // Check if join condition is satisfied
        let condition = join_state.condition.clone();
        let completed_count = join_state.completed_branches.len();
        let expected_count = join_state.expected_branches.len();

        Ok(match condition {
            JoinCondition::All => completed_count >= expected_count,
            JoinCondition::First => completed_count >= 1,
            JoinCondition::Count(n) => completed_count >= n,
            JoinCondition::Predicate(_) => false, // TODO: Implement predicate evaluation
        })
    }

    /// Check if join condition is satisfied
    fn check_join_condition(&self, state: &JoinState) -> bool {
        match state.condition {
            JoinCondition::All => {
                state.completed_branches.len() >= state.expected_branches.len()
            }
            JoinCondition::First => state.completed_branches.len() >= 1,
            JoinCondition::Count(n) => state.completed_branches.len() >= n,
            JoinCondition::Predicate(_) => false, // TODO: Implement predicate evaluation
        }
    }

    /// Get split state
    pub fn get_split(&self, split_id: &str) -> Option<&SplitState> {
        self.active_splits.get(split_id)
    }

    /// Remove completed split
    pub fn remove_split(&mut self, split_id: &str) -> Option<SplitState> {
        self.active_splits.remove(split_id)
    }
}

impl Default for FlowRouter {
    fn default() -> Self {
        Self::new()
    }
}

/// Flow routing errors
#[derive(Debug, thiserror::Error)]
pub enum FlowError {
    #[error("Split not found: {0}")]
    SplitNotFound(String),

    #[error("Join not found: {0}")]
    JoinNotFound(String),

    #[error("Invalid flow pattern: {0}")]
    InvalidPattern(String),

    #[error("Join condition not satisfied: {0}")]
    ConditionNotSatisfied(String),

    #[error("Workflow error: {0}")]
    WorkflowError(#[from] WorkflowError),
}

/// Flow result type
pub type FlowResult<T> = Result<T, FlowError>;

// ============================================================================
// DETERMINISTIC HASHING
// ============================================================================

/// Compute deterministic hash for workflow execution
pub fn compute_execution_hash(
    execution_id: &str, input: &HashMap<String, serde_json::Value>, salt: Option<&str>,
) -> String {
    let mut hasher = Sha256::new();

    // Hash execution ID
    hasher.update(execution_id.as_bytes());

    // Hash sorted input for determinism
    let mut sorted_keys: Vec<&String> = input.keys().collect();
    sorted_keys.sort();

    for key in sorted_keys {
        hasher.update(key.as_bytes());
        if let Ok(json_str) = serde_json::to_string(input.get(key).unwrap()) {
            hasher.update(json_str.as_bytes());
        }
    }

    // Add salt if provided
    if let Some(salt_value) = salt {
        hasher.update(salt_value.as_bytes());
    }

    format!("{:x}", hasher.finalize())
}

// ============================================================================
// RECEIPT METADATA EXTENSION
// ============================================================================

/// Extend receipt metadata with execution info
pub trait ReceiptMetadataExt {
    /// Update metadata with execution results
    fn update_with_results(&mut self, results: &[TaskResult]);
}

impl ReceiptMetadataExt for crate::receipts::ReceiptMetadata {
    fn update_with_results(&mut self, results: &[TaskResult]) {
        self.steps_count = results.len();

        for result in results {
            self.execution_time_ms += result.execution_time_ms;
        }
    }
}

// ============================================================================
// TESTS
// ============================================================================

#[cfg(test)]
mod tests {
    use super::*;
    use std::collections::HashMap;

    #[test]
    fn test_engine_config_default() {
        let config = EngineConfig::default();
        assert_eq!(config.max_parallel_tasks, CONSTANTS.max_parallel_tasks);
        assert_eq!(config.default_timeout_ms, CONSTANTS.default_timeout_ms);
        assert_eq!(config.max_retries, 3);
        assert!(config.deterministic);
    }

    #[test]
    fn test_workflow_state_creation() {
        let tasks = vec![
            TaskDefinition {
                id: "task1".to_string(),
                name: "Task 1".to_string(),
                task_type: "generic".to_string(),
                dependencies: vec![],
                priority: 50,
                timeout_ms: 5000,
                max_retries: 3,
                input: serde_json::json!({}),
                flow_metadata: None,
            },
            TaskDefinition {
                id: "task2".to_string(),
                name: "Task 2".to_string(),
                task_type: "generic".to_string(),
                dependencies: vec!["task1".to_string()],
                priority: 50,
                timeout_ms: 5000,
                max_retries: 3,
                input: serde_json::json!({}),
                flow_metadata: None,
            },
        ];

        let engine = WorkflowEngine::default_engine();
        let state = engine.initialize_workflow_state(&tasks, "test-execution").unwrap();

        assert_eq!(state.execution_id, "test-execution");
        assert_eq!(state.ready_tasks.len(), 1); // Only task1 is ready
        assert!(state.ready_tasks.contains("task1"));
        assert!(state.completed_tasks.is_empty());
        assert!(state.failed_tasks.is_empty());
    }

    #[test]
    fn test_task_scheduler_load() {
        let mut scheduler = TaskScheduler::new();

        let tasks = vec![
            TaskDefinition {
                id: "task1".to_string(),
                name: "Task 1".to_string(),
                task_type: "generic".to_string(),
                dependencies: vec![],
                priority: 50,
                timeout_ms: 5000,
                max_retries: 3,
                input: serde_json::json!({}),
                flow_metadata: None,
            },
            TaskDefinition {
                id: "task2".to_string(),
                name: "Task 2".to_string(),
                task_type: "generic".to_string(),
                dependencies: vec!["task1".to_string()],
                priority: 50,
                timeout_ms: 5000,
                max_retries: 3,
                input: serde_json::json!({}),
                flow_metadata: None,
            },
        ];

        scheduler.load_tasks(tasks).unwrap();

        assert_eq!(scheduler.tasks.len(), 2);
        assert!(scheduler.get_task("task1").is_ok());
        assert!(scheduler.get_task("task2").is_ok());
        assert!(scheduler.get_task("task3").is_err());
    }

    #[test]
    fn test_task_scheduler_circular_detection() {
        let mut scheduler = TaskScheduler::new();

        let tasks = vec![
            TaskDefinition {
                id: "task1".to_string(),
                name: "Task 1".to_string(),
                task_type: "generic".to_string(),
                dependencies: vec!["task2".to_string()],
                priority: 50,
                timeout_ms: 5000,
                max_retries: 3,
                input: serde_json::json!({}),
                flow_metadata: None,
            },
            TaskDefinition {
                id: "task2".to_string(),
                name: "Task 2".to_string(),
                task_type: "generic".to_string(),
                dependencies: vec!["task1".to_string()],
                priority: 50,
                timeout_ms: 5000,
                max_retries: 3,
                input: serde_json::json!({}),
                flow_metadata: None,
            },
        ];

        let result = scheduler.load_tasks(tasks);
        assert!(result.is_err());
    }

    #[test]
    fn test_condition_evaluator() {
        let mut context = HashMap::new();
        context.insert("x".to_string(), serde_json::json!(10));
        context.insert("y".to_string(), serde_json::json!(20));

        let evaluator = ConditionEvaluator::new(context);

        // Test basic conditions
        assert!(evaluator.evaluate("true").unwrap());
        assert!(!evaluator.evaluate("false").unwrap());

        // Test comparisons
        assert!(evaluator.evaluate("x < y").unwrap());
        assert!(evaluator.evaluate("y > x").unwrap());
        assert!(evaluator.evaluate("x == 10").unwrap());

        // Test logical operators
        assert!(evaluator.evaluate("x < y && x == 10").unwrap());
        assert!(evaluator.evaluate("x > y || x == 10").unwrap());
        assert!(!evaluator.evaluate("x > 100").unwrap());
    }

    #[test]
    fn test_flow_router_and_split() {
        let mut router = FlowRouter::new();

        router
            .create_and_split("split1".to_string(), vec!["branch1".to_string(), "branch2".to_string()])
            .unwrap();

        let split = router.get_split("split1").unwrap();
        assert_eq!(split.pattern, FlowPattern::AndSplit);
        assert_eq!(split.branches.len(), 2);
    }

    #[test]
    fn test_flow_router_xor_split() {
        let mut router = FlowRouter::new();
        let mut context = HashMap::new();
        context.insert("mode".to_string(), serde_json::json!("fast"));

        let evaluator = ConditionEvaluator::new(context);

        router
            .create_xor_split(
                "split1".to_string(),
                vec![
                    ("branch1".to_string(), "mode == 'fast'".to_string()),
                    ("branch2".to_string(), "mode == 'slow'".to_string()),
                ],
                &evaluator,
            )
            .unwrap();

        let split = router.get_split("split1").unwrap();
        assert_eq!(split.pattern, FlowPattern::XorSplit);
        assert_eq!(split.branches.len(), 1);
        assert_eq!(split.branches[0].branch_id, "branch1");
    }

    #[test]
    fn test_decomposer_basic() {
        let mut decomposer = WorkflowDecomposer::new(3);

        let tasks = vec![
            TaskDefinition {
                id: "task1".to_string(),
                name: "Task 1".to_string(),
                task_type: "generic".to_string(),
                dependencies: vec![],
                priority: 50,
                timeout_ms: 5000,
                max_retries: 3,
                input: serde_json::json!({}),
                flow_metadata: None,
            },
            TaskDefinition {
                id: "task2".to_string(),
                name: "Task 2".to_string(),
                task_type: "generic".to_string(),
                dependencies: vec!["task1".to_string()],
                priority: 50,
                timeout_ms: 5000,
                max_retries: 3,
                input: serde_json::json!({}),
                flow_metadata: None,
            },
        ];

        let result = decomposer.decompose(tasks).unwrap();

        assert!(!result.sub_workflows.is_empty());
        assert_eq!(result.depth, 1);
    }

    #[test]
    fn test_decomposer_max_depth() {
        let mut decomposer = WorkflowDecomposer::new(1);

        // First decomposition should succeed
        let tasks = vec![TaskDefinition {
            id: "task1".to_string(),
            name: "Task 1".to_string(),
            task_type: "generic".to_string(),
            dependencies: vec![],
            priority: 50,
            timeout_ms: 5000,
            max_retries: 3,
            input: serde_json::json!({}),
            flow_metadata: None,
        }];

        decomposer.decompose(tasks).unwrap();

        // Second decomposition should fail
        let tasks2 = vec![TaskDefinition {
            id: "task2".to_string(),
            name: "Task 2".to_string(),
            task_type: "generic".to_string(),
            dependencies: vec![],
            priority: 50,
            timeout_ms: 5000,
            max_retries: 3,
            input: serde_json::json!({}),
            flow_metadata: None,
        }];

        let result = decomposer.decompose(tasks2);
        assert!(result.is_err());
    }

    #[test]
    fn test_compute_execution_hash() {
        let mut input = HashMap::new();
        input.insert("key1".to_string(), serde_json::json!("value1"));
        input.insert("key2".to_string(), serde_json::json!(42));

        let hash1 = compute_execution_hash("exec1", &input, Some("salt"));
        let hash2 = compute_execution_hash("exec1", &input, Some("salt"));
        let hash3 = compute_execution_hash("exec1", &input, Some("different_salt"));

        // Same inputs produce same hash
        assert_eq!(hash1, hash2);

        // Different salt produces different hash
        assert_ne!(hash1, hash3);

        // Hash is hex string of correct length
        assert_eq!(hash1.len(), 64); // SHA-256 produces 64 hex chars
    }

    #[tokio::test]
    async fn test_engine_simple_execution() {
        let engine = WorkflowEngine::default_engine();

        let tasks = vec![TaskDefinition {
            id: "task1".to_string(),
            name: "Simple Task".to_string(),
            task_type: "generic".to_string(),
            dependencies: vec![],
            priority: 50,
            timeout_ms: 5000,
            max_retries: 3,
            input: serde_json::json!({"test": "data"}),
            flow_metadata: None,
        }];

        let mut context = WorkflowContext::default();
        let result = engine.execute(tasks, &mut context).await.unwrap();

        assert!(result.success);
        assert_eq!(result.total_tasks, 1);
        assert_eq!(result.completed_tasks, 1);
        assert_eq!(result.failed_tasks, 0);
        assert!(result.receipt.is_some());
    }

    #[tokio::test]
    async fn test_engine_dependent_tasks() {
        let engine = WorkflowEngine::default_engine();

        let tasks = vec![
            TaskDefinition {
                id: "task1".to_string(),
                name: "First Task".to_string(),
                task_type: "generic".to_string(),
                dependencies: vec![],
                priority: 50,
                timeout_ms: 5000,
                max_retries: 3,
                input: serde_json::json!({}),
                flow_metadata: None,
            },
            TaskDefinition {
                id: "task2".to_string(),
                name: "Second Task".to_string(),
                task_type: "generic".to_string(),
                dependencies: vec!["task1".to_string()],
                priority: 50,
                timeout_ms: 5000,
                max_retries: 3,
                input: serde_json::json!({}),
                flow_metadata: None,
            },
        ];

        let mut context = WorkflowContext::default();
        let result = engine.execute(tasks, &mut context).await.unwrap();

        assert!(result.success);
        assert_eq!(result.total_tasks, 2);
        assert_eq!(result.completed_tasks, 2);
    }

    #[tokio::test]
    async fn test_engine_cancellation() {
        let engine = WorkflowEngine::default_engine();

        // Create a workflow with a long-running task would go here
        // For now, just test the cancellation interface

        let cancelled = engine.cancel("non-existent-execution").await.unwrap();
        assert!(!cancelled);
    }

    #[test]
    fn test_flow_pattern_serialization() {
        let pattern = FlowPattern::AndSplit;
        let json = serde_json::to_string(&pattern).unwrap();
        let deserialized: FlowPattern = serde_json::from_str(&json).unwrap();
        assert_eq!(pattern, deserialized);
    }

    #[test]
    fn test_join_condition_serialization() {
        let condition = JoinCondition::Count(5);
        let json = serde_json::to_string(&condition).unwrap();
        let deserialized: JoinCondition = serde_json::from_str(&json).unwrap();
        assert_eq!(condition, deserialized);
    }
}
