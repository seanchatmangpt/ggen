//! YAWL to A2A task converter.
//!
//! This module provides conversion from YAWL workflow specifications to
//! A2A tasks with proper dependency mapping and priority handling.

use a2a_generated::task::{Task, TaskPriority, TaskStatus};
use crate::a2a::error::{A2AIntegrationError, IntegrationResult};
use crate::a2a::gateway::SplitJoinBehavior;
use crate::template::{FlowContext, TaskContext, TemplateContext};
use serde::{Deserialize, Serialize};
use std::collections::{HashMap, HashSet};
use std::time::Duration;
use uuid::Uuid;

/// Configuration for YAWL to A2A task conversion.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct YawlToA2AConfig {
    /// Default task timeout in seconds
    pub default_timeout_secs: u64,
    /// Default task priority
    pub default_priority: TaskPriority,
    /// Whether to preserve YAWL task IDs in metadata
    pub preserve_yawl_ids: bool,
    /// Custom task type prefix
    pub task_type_prefix: String,
    /// Whether to auto-generate task dependencies from flows
    pub auto_dependencies: bool,
}

impl Default for YawlToA2AConfig {
    fn default() -> Self {
        Self {
            default_timeout_secs: 300,
            default_priority: TaskPriority::Normal,
            preserve_yawl_ids: true,
            task_type_prefix: "yawl".to_string(),
            auto_dependencies: true,
        }
    }
}

impl YawlToA2AConfig {
    /// Create a new config with custom timeout.
    pub fn with_timeout(mut self, timeout_secs: u64) -> Self {
        self.default_timeout_secs = timeout_secs;
        self
    }

    /// Set default task priority.
    pub fn with_priority(mut self, priority: TaskPriority) -> Self {
        self.default_priority = priority;
        self
    }

    /// Enable or disable YAWL ID preservation.
    pub fn with_preserve_yawl_ids(mut self, preserve: bool) -> Self {
        self.preserve_yawl_ids = preserve;
        self
    }

    /// Set task type prefix.
    pub fn with_task_type_prefix(mut self, prefix: impl Into<String>) -> Self {
        self.task_type_prefix = prefix.into();
        self
    }

    /// Enable or disable automatic dependency generation.
    pub fn with_auto_dependencies(mut self, enabled: bool) -> Self {
        self.auto_dependencies = enabled;
        self
    }
}

/// Converted task with metadata.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ConvertedTask {
    /// The A2A task
    pub task: Task,
    /// Original YAWL task ID
    pub yawl_task_id: String,
    /// Task type based on YAWL classification
    pub yawl_task_type: YawlTaskType,
    /// Split behavior for gateway handling
    pub split_behavior: SplitJoinBehavior,
    /// Join behavior for gateway handling
    pub join_behavior: SplitJoinBehavior,
}

/// YAWL task types as defined in the specification.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub enum YawlTaskType {
    /// Atomic task (single unit of work)
    Atomic,
    /// Composite task (contains sub-workflow)
    Composite,
    /// Multiple instance task (parallel execution)
    MultipleInstance,
    /// Condition task (gateway/decision point)
    Condition,
}

/// Task dependency information.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct TaskDependency {
    /// Task that depends on another
    pub task_id: String,
    /// Task that is being depended on
    pub depends_on: String,
    /// Flow condition (if any)
    pub condition: Option<String>,
}

/// Converter from YAWL to A2A tasks.
pub struct TaskConverter {
    config: YawlToA2AConfig,
}

impl TaskConverter {
    /// Create a new task converter with default config.
    pub fn new() -> Self {
        Self {
            config: YawlToA2AConfig::default(),
        }
    }

    /// Create a new task converter with custom config.
    pub fn with_config(config: YawlToA2AConfig) -> Self {
        Self { config }
    }

    /// Convert a YAWL template context to A2A tasks.
    ///
    /// This method performs the following:
    /// 1. Converts each YAWL task to an A2A task
    /// 2. Builds dependency graph from flows
    /// 3. Validates for circular dependencies
    /// 4. Returns converted tasks with metadata
    pub fn convert(&self, ctx: &TemplateContext) -> IntegrationResult<Vec<ConvertedTask>> {
        // Validate context
        ctx.validate()
            .map_err(|e| A2AIntegrationError::conversion(format!("Invalid template context: {}", e)))?;

        let mut converted_tasks = Vec::new();
        let mut task_map = HashMap::new();

        // First pass: convert all tasks
        for yawl_task in &ctx.tasks {
            let task_type = self.classify_task(yawl_task);
            let split_behavior = self.parse_split_behavior(yawl_task);
            let join_behavior = self.parse_join_behavior(yawl_task);

            let a2a_task = self.create_a2a_task(
                yawl_task,
                &task_type,
                ctx,
                &split_behavior,
            )?;

            task_map.insert(yawl_task.id.clone(), a2a_task.id.clone());

            let converted = ConvertedTask {
                yawl_task_id: yawl_task.id.clone(),
                task: a2a_task,
                yawl_task_type: task_type,
                split_behavior,
                join_behavior,
            };

            converted_tasks.push(converted);
        }

        // Second pass: add dependencies from flows
        if self.config.auto_dependencies {
            self.apply_flow_dependencies(&mut converted_tasks, ctx, &task_map)?;
        }

        // Validate no circular dependencies
        self.validate_no_circular_deps(&converted_tasks)?;

        Ok(converted_tasks)
    }

    /// Classify a YAWL task into its type.
    fn classify_task(&self, task: &TaskContext) -> YawlTaskType {
        if task.decomposition_id.is_some() {
            YawlTaskType::Composite
        } else if task.split_type.contains("OR") || task.join_type.contains("OR") {
            YawlTaskType::MultipleInstance
        } else if !task.is_auto {
            YawlTaskType::Condition
        } else {
            YawlTaskType::Atomic
        }
    }

    /// Parse split behavior from task context.
    fn parse_split_behavior(&self, task: &TaskContext) -> SplitJoinBehavior {
        match task.split_type.as_str() {
            "AND" => SplitJoinBehavior::And,
            "XOR" => SplitJoinBehavior::Xor,
            "OR" => SplitJoinBehavior::Or,
            _ => SplitJoinBehavior::Xor, // Default to XOR
        }
    }

    /// Parse join behavior from task context.
    fn parse_join_behavior(&self, task: &TaskContext) -> SplitJoinBehavior {
        match task.join_type.as_str() {
            "AND" => SplitJoinBehavior::And,
            "XOR" => SplitJoinBehavior::Xor,
            "OR" => SplitJoinBehavior::Or,
            _ => SplitJoinBehavior::Xor, // Default to XOR
        }
    }

    /// Create an A2A task from YAWL task context.
    fn create_a2a_task(
        &self,
        yawl_task: &TaskContext,
        task_type: &YawlTaskType,
        ctx: &TemplateContext,
        split_behavior: &SplitJoinBehavior,
    ) -> IntegrationResult<Task> {
        let task_id = format!("{}-{}", ctx.workflow_name, yawl_task.id);
        let task_type_str = format!("{}:{}", self.config.task_type_prefix, task_type.as_ref());

        // Build input from context variables
        let mut input = serde_json::json!({
            "yawl_task_id": yawl_task.id,
            "yawl_task_name": yawl_task.name,
            "workflow_name": ctx.workflow_name,
            "split_type": yawl_task.split_type,
            "join_type": yawl_task.join_type,
            "is_automated": yawl_task.is_auto,
        });

        // Add workflow variables to input
        let workflow_vars: HashMap<String, serde_json::Value> = ctx
            .variables
            .iter()
            .filter(|v| v.scope == "workflow")
            .map(|v| {
                let value = v.default.as_ref().map(|d| serde_json::json!(d))
                    .unwrap_or_else(|| serde_json::Value::Null);
                (v.name.clone(), value)
            })
            .collect();

        if let Some(obj) = input.as_object_mut() {
            obj.insert("workflow_variables".to_string(), serde_json::json!(workflow_vars));
        }

        // Add task-specific variables
        let task_vars: HashMap<String, serde_json::Value> = ctx
            .variables
            .iter()
            .filter(|v| v.scope == yawl_task.id)
            .map(|v| {
                let value = v.default.as_ref().map(|d| serde_json::json!(d))
                    .unwrap_or_else(|| serde_json::Value::Null);
                (v.name.clone(), value)
            })
            .collect();

        if let Some(obj) = input.as_object_mut() {
            obj.insert("task_variables".to_string(), serde_json::json!(task_vars));
        }

        // Create task
        let mut task = Task::new(task_id, yawl_task.name.clone(), task_type_str, input);

        // Set timeout
        task.timeout = Duration::from_secs(self.config.default_timeout_secs);

        // Set status based on split behavior (XOR split tasks start ready)
        task.status = if matches!(split_behavior, SplitJoinBehavior::Xor) {
            TaskStatus::Ready
        } else {
            TaskStatus::Pending
        };

        // Set priority
        task.priority = self.config.default_priority.clone();

        // Add metadata
        if self.config.preserve_yawl_ids {
            task.metadata.insert("yawl_task_id".to_string(), yawl_task.id.clone());
            task.metadata.insert("yawl_workflow".to_string(), ctx.workflow_name.clone());
            task.metadata.insert("yawl_version".to_string(), ctx.version.clone());
        }

        // Add decomposition ID for composite tasks
        if let Some(decomp_id) = &yawl_task.decomposition_id {
            task.metadata.insert("decomposition_id".to_string(), decomp_id.clone());
        }

        Ok(task)
    }

    /// Apply flow-based dependencies to converted tasks.
    fn apply_flow_dependencies(
        &self,
        tasks: &mut [ConvertedTask],
        ctx: &TemplateContext,
        task_map: &HashMap<String, String>,
    ) -> IntegrationResult<()> {
        // Build a mapping from YAWL task ID to converted task index
        let yawl_to_index: HashMap<String, usize> = tasks
            .iter()
            .enumerate()
            .map(|(i, t)| (t.yawl_task_id.clone(), i))
            .collect();

        // Process each flow
        for flow in &ctx.flows {
            // Find the target task in converted tasks
            if let Some(&target_idx) = yawl_to_index.get(&flow.target) {
                // Add source as dependency
                if let Some(source_a2a_id) = task_map.get(&flow.source) {
                    tasks[target_idx].task.dependencies.push(source_a2a_id.clone());

                    // Add condition to metadata if present
                    if let Some(condition) = &flow.condition {
                        let key = format!("dep_condition_{}", flow.source);
                        tasks[target_idx].task.metadata.insert(key, condition.clone());
                    }
                }
            }
        }

        Ok(())
    }

    /// Validate that there are no circular dependencies.
    fn validate_no_circular_deps(&self, tasks: &[ConvertedTask]) -> IntegrationResult<()> {
        // Build dependency graph
        let dep_graph: HashMap<&str, Vec<&str>> = tasks
            .iter()
            .map(|t| {
                let deps: Vec<&str> = t
                    .task
                    .dependencies
                    .iter()
                    .map(|d| d.as_str())
                    .collect();
                (t.task.id.as_str(), deps)
            })
            .collect();

        // Check for cycles using DFS
        let mut visited = HashSet::new();
        let mut rec_stack = HashSet::new();
        let mut path = Vec::new();

        for task_id in dep_graph.keys() {
            if !visited.contains(task_id) {
                if let Some(cycle) = self.detect_cycle(
                    task_id,
                    &dep_graph,
                    &mut visited,
                    &mut rec_stack,
                    &mut path,
                ) {
                    return Err(A2AIntegrationError::circular_dependency(cycle));
                }
            }
        }

        Ok(())
    }

    /// Detect cycle using DFS.
    fn detect_cycle<'a>(
        &self,
        node: &'a str,
        graph: &HashMap<&'a str, Vec<&'a str>>,
        visited: &mut HashSet<&'a str>,
        rec_stack: &mut HashSet<&'a str>,
        path: &mut Vec<&'a str>,
    ) -> Option<String> {
        visited.insert(node);
        rec_stack.insert(node);
        path.push(node);

        if let Some(deps) = graph.get(node) {
            for dep in deps {
                if !visited.contains(dep) {
                    if let Some(cycle) = self.detect_cycle(dep, graph, visited, rec_stack, path) {
                        return Some(cycle);
                    }
                } else if rec_stack.contains(dep) {
                    // Found a cycle - construct the cycle string
                    let cycle_start = path.iter().position(|p| p == dep).unwrap_or(0);
                    let cycle: Vec<&str> = path[cycle_start..].iter().cloned().chain(Some(dep)).collect();
                    return Some(cycle.join(" -> ")));
                }
            }
        }

        rec_stack.remove(node);
        path.pop();
        None
    }

    /// Extract dependency information from flows.
    pub fn extract_dependencies(&self, ctx: &TemplateContext) -> Vec<TaskDependency> {
        let mut dependencies = Vec::new();

        for flow in &ctx.flows {
            dependencies.push(TaskDependency {
                task_id: flow.target.clone(),
                depends_on: flow.source.clone(),
                condition: flow.condition.clone(),
            });
        }

        dependencies
    }

    /// Get task IDs in topological order.
    pub fn topological_order(&self, ctx: &TemplateContext) -> IntegrationResult<Vec<String>> {
        let dependencies = self.extract_dependencies(ctx);

        // Build adjacency list and in-degree count
        let mut adj_list: HashMap<String, Vec<String>> = HashMap::new();
        let mut in_degree: HashMap<String, usize> = HashMap::new();

        // Initialize with all tasks
        for task in &ctx.tasks {
            adj_list.insert(task.id.clone(), Vec::new());
            in_degree.insert(task.id.clone(), 0);
        }

        // Build graph from dependencies
        for dep in &dependencies {
            adj_list
                .entry(dep.depends_on.clone())
                .or_insert_with(Vec::new)
                .push(dep.task_id.clone());
            *in_degree.entry(dep.task_id.clone()).or_insert(0) += 1;
        }

        // Kahn's algorithm for topological sort
        let mut queue: Vec<String> = in_degree
            .iter()
            .filter(|(_, &deg)| deg == 0)
            .map(|(id, _)| id.clone())
            .collect();

        let mut result = Vec::new();

        while let Some(node) = queue.pop() {
            result.push(node.clone());

            if let Some(neighbors) = adj_list.get(&node) {
                for neighbor in neighbors {
                    if let Some(deg) = in_degree.get_mut(neighbor) {
                        *deg -= 1;
                        if *deg == 0 {
                            queue.push(neighbor.clone());
                        }
                    }
                }
            }
        }

        // Check for cycle
        if result.len() != ctx.tasks.len() {
            return Err(A2AIntegrationError::circular_dependency(
                "Cycle detected in task dependencies".to_string(),
            ));
        }

        Ok(result)
    }
}

impl Default for TaskConverter {
    fn default() -> Self {
        Self::new()
    }
}

impl YawlTaskType {
    /// Get string representation of task type.
    pub fn as_ref(&self) -> &str {
        match self {
            Self::Atomic => "atomic",
            Self::Composite => "composite",
            Self::MultipleInstance => "multiple_instance",
            Self::Condition => "condition",
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn create_test_context() -> TemplateContext {
        TemplateContext {
            workflow_name: "test-workflow".to_string(),
            description: "Test workflow".to_string(),
            version: "1.0.0".to_string(),
            tasks: vec![
                TaskContext {
                    id: "task1".to_string(),
                    name: "First Task".to_string(),
                    split_type: "XOR".to_string(),
                    join_type: "XOR".to_string(),
                    is_auto: true,
                    decomposition_id: None,
                },
                TaskContext {
                    id: "task2".to_string(),
                    name: "Second Task".to_string(),
                    split_type: "AND".to_string(),
                    join_type: "AND".to_string(),
                    is_auto: true,
                    decomposition_id: None,
                },
            ],
            flows: vec![
                FlowContext {
                    source: "task1".to_string(),
                    target: "task2".to_string(),
                    condition: None,
                    predicate: None,
                    is_default: true,
                },
            ],
            input_condition: None,
            output_condition: None,
            variables: vec![],
        }
    }

    #[test]
    fn test_converter_creation() {
        let converter = TaskConverter::new();
        assert_eq!(converter.config.default_timeout_secs, 300);
    }

    #[test]
    fn test_convert_basic_workflow() {
        let converter = TaskConverter::new();
        let ctx = create_test_context();

        let result = converter.convert(&ctx);
        assert!(result.is_ok());

        let tasks = result.unwrap();
        assert_eq!(tasks.len(), 2);
        assert_eq!(tasks[0].yawl_task_id, "task1");
        assert_eq!(tasks[1].yawl_task_id, "task2");

        // Check dependency was added
        assert_eq!(tasks[1].task.dependencies.len(), 1);
    }

    #[test]
    fn test_task_classification() {
        let converter = TaskConverter::new();

        // Atomic task
        let atomic = TaskContext {
            id: "atomic".to_string(),
            name: "Atomic".to_string(),
            split_type: "XOR".to_string(),
            join_type: "XOR".to_string(),
            is_auto: true,
            decomposition_id: None,
        };
        assert_eq!(converter.classify_task(&atomic), YawlTaskType::Atomic);

        // Composite task
        let composite = TaskContext {
            id: "composite".to_string(),
            name: "Composite".to_string(),
            split_type: "XOR".to_string(),
            join_type: "XOR".to_string(),
            is_auto: true,
            decomposition_id: Some("subworkflow".to_string()),
        };
        assert_eq!(converter.classify_task(&composite), YawlTaskType::Composite);
    }

    #[test]
    fn test_parse_split_join_behavior() {
        let converter = TaskConverter::new();

        let task_and = TaskContext {
            id: "test".to_string(),
            name: "Test".to_string(),
            split_type: "AND".to_string(),
            join_type: "AND".to_string(),
            is_auto: true,
            decomposition_id: None,
        };

        assert_eq!(
            converter.parse_split_behavior(&task_and),
            SplitJoinBehavior::And
        );
        assert_eq!(
            converter.parse_join_behavior(&task_and),
            SplitJoinBehavior::And
        );
    }

    #[test]
    fn test_topological_order() {
        let converter = TaskConverter::new();
        let ctx = create_test_context();

        let order = converter.topological_order(&ctx).unwrap();
        assert_eq!(order, vec!["task1", "task2"]);
    }

    #[test]
    fn test_config_defaults() {
        let config = YawlToA2AConfig::default();
        assert_eq!(config.default_timeout_secs, 300);
        assert_eq!(config.default_priority, TaskPriority::Normal);
        assert!(config.preserve_yawl_ids);
        assert_eq!(config.task_type_prefix, "yawl");
        assert!(config.auto_dependencies);
    }

    #[test]
    fn test_config_builder() {
        let config = YawlToA2AConfig::default()
            .with_timeout(600)
            .with_priority(TaskPriority::High)
            .with_preserve_yawl_ids(false)
            .with_task_type_prefix("custom")
            .with_auto_dependencies(false);

        assert_eq!(config.default_timeout_secs, 600);
        assert_eq!(config.default_priority, TaskPriority::High);
        assert!(!config.preserve_yawl_ids);
        assert_eq!(config.task_type_prefix, "custom");
        assert!(!config.auto_dependencies);
    }

    #[test]
    fn test_extract_dependencies() {
        let converter = TaskConverter::new();
        let ctx = create_test_context();

        let deps = converter.extract_dependencies(&ctx);
        assert_eq!(deps.len(), 1);
        assert_eq!(deps[0].task_id, "task2");
        assert_eq!(deps[0].depends_on, "task1");
    }

    #[test]
    fn test_no_circular_dependencies() {
        let converter = TaskConverter::new();
        let ctx = create_test_context();

        let tasks = converter.convert(&ctx).unwrap();
        // Should not error
        assert!(converter.validate_no_circular_deps(&tasks).is_ok());
    }

    #[test]
    fn test_circular_dependency_detection() {
        let converter = TaskConverter::new();

        // Create tasks with circular dependency
        let task1 = ConvertedTask {
            yawl_task_id: "task1".to_string(),
            task: Task::new(
                "task1".to_string(),
                "Task 1".to_string(),
                "test".to_string(),
                serde_json::json!({}),
            )
            .with_dependency("task2".to_string()),
            yawl_task_type: YawlTaskType::Atomic,
            split_behavior: SplitJoinBehavior::Xor,
            join_behavior: SplitJoinBehavior::Xor,
        };

        let task2 = ConvertedTask {
            yawl_task_id: "task2".to_string(),
            task: Task::new(
                "task2".to_string(),
                "Task 2".to_string(),
                "test".to_string(),
                serde_json::json!({}),
            )
            .with_dependency("task1".to_string()),
            yawl_task_type: YawlTaskType::Atomic,
            split_behavior: SplitJoinBehavior::Xor,
            join_behavior: SplitJoinBehavior::Xor,
        };

        let result = converter.validate_no_circular_deps(&[task1, task2]);
        assert!(result.is_err());
        assert!(matches!(result.unwrap_err(), A2AIntegrationError::CircularDependency { .. }));
    }
}
