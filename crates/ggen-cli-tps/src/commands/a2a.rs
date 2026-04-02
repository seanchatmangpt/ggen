use crate::error::{CliError, Result};
use clap::Subcommand;
use serde::{Deserialize, Serialize};
use std::path::PathBuf;

/// Subcommands for agent-to-agent (A2A) task management.
///
/// Provides commands for creating, managing, and tracking tasks in a multi-agent
/// system. Tasks represent work units that can transition through states (created,
/// running, blocked, completed, failed) and can be assigned to specific agents.
///
/// # Variants
///
/// - `Create`: Create a new task
/// - `Transition`: Move task between states
/// - `Status`: Check task status
/// - `List`: Display all tasks with optional filtering
/// - `Validate`: Validate task state machine
/// - `AddArtifact`: Attach output artifacts to tasks
#[derive(Debug, Subcommand)]
pub enum A2aCommands {
    /// Create a new task
    Create {
        /// Task title
        #[clap(short, long)]
        title: String,

        /// Task description
        #[clap(short, long)]
        description: Option<String>,

        /// Agent ID creating the task
        #[clap(short = 'a', long)]
        agent: String,

        /// Assign to agent ID
        #[clap(short = 's', long)]
        assign_to: Option<String>,

        /// Output file
        #[clap(short, long)]
        output: PathBuf,
    },

    /// Transition task state
    Transition {
        /// Task file
        #[clap(short, long)]
        task: PathBuf,

        /// Target state (running, blocked, completed, failed)
        #[clap(short = 's', long)]
        state: TaskStateArg,

        /// Failure reason (required if state is failed)
        #[clap(short, long)]
        reason: Option<String>,
    },

    /// Show task status
    Status {
        /// Task file
        #[clap(short, long)]
        task: PathBuf,

        /// Output format (json, text)
        #[clap(short, long, default_value = "text")]
        format: OutputFormat,
    },

    /// List all tasks in directory
    List {
        /// Directory containing tasks
        #[clap(short, long, default_value = ".")]
        directory: PathBuf,

        /// Filter by state
        #[clap(short = 's', long)]
        state: Option<TaskStateArg>,

        /// Output format
        #[clap(short, long, default_value = "text")]
        format: OutputFormat,
    },

    /// Validate task state machine
    Validate {
        /// Task file
        #[clap(short, long)]
        task: PathBuf,
    },

    /// Add artifact to task
    AddArtifact {
        /// Task file
        #[clap(short, long)]
        task: PathBuf,

        /// Artifact name
        #[clap(short, long)]
        name: String,

        /// Artifact content file
        #[clap(short, long)]
        content: PathBuf,

        /// Artifact type (input, output, intermediate)
        #[clap(short = 't', long)]
        artifact_type: ArtifactTypeArg,
    },
}

/// Task lifecycle states.
///
/// Represents the possible states a task can transition through.
#[derive(Debug, Clone, clap::ValueEnum)]
pub enum TaskStateArg {
    /// Task has been created but not yet started
    Created,
    /// Task is currently executing
    Running,
    /// Task is waiting on external dependencies
    Blocked,
    /// Task has finished successfully
    Completed,
    /// Task has failed and requires intervention
    Failed,
}

/// Types of task artifacts.
///
/// Classifies artifacts attached to tasks by their role in processing.
#[derive(Debug, Clone, clap::ValueEnum)]
pub enum ArtifactTypeArg {
    /// Input data provided to the task
    Input,
    /// Output produced by the task
    Output,
    /// Intermediate results generated during execution
    Intermediate,
}

/// Output format for A2A commands.
#[derive(Debug, Clone, clap::ValueEnum)]
pub enum OutputFormat {
    /// JSON format output
    Json,
    /// Human-readable text format
    Text,
}

/// Represents a task in the agent-to-agent task management system.
///
/// A task is a unit of work that can be assigned to agents, tracked through
/// various states, and produce artifacts as output.
#[derive(Debug, Serialize, Deserialize)]
pub struct Task {
    /// Unique task identifier (format: "task-{timestamp}")
    pub id: String,
    /// Current state of the task (created, running, blocked, completed, failed)
    pub state: String,
    /// Brief title describing the task
    pub title: String,
    /// Detailed description of task purpose and requirements
    pub description: Option<String>,
    /// Agent ID this task is assigned to
    pub assigned_to: Option<String>,
    /// Agent ID that created this task
    pub created_by: String,
    /// RFC 3339 timestamp when task was created
    pub created_at: String,
    /// RFC 3339 timestamp of last status update
    pub updated_at: String,
    /// RFC 3339 timestamp when task reached terminal state
    pub completed_at: Option<String>,
    /// Reason for failure if task failed
    pub failure_reason: Option<String>,
    /// Artifacts produced or used by this task
    pub artifacts: Vec<TaskArtifact>,
}

/// An artifact associated with a task.
///
/// Artifacts are outputs or inputs attached to tasks for tracking and audit purposes.
#[derive(Debug, Serialize, Deserialize)]
pub struct TaskArtifact {
    /// Name of the artifact
    pub name: String,
    /// Type of artifact (input, output, intermediate)
    pub artifact_type: String,
    /// Content data of the artifact
    pub content: String,
}

impl A2aCommands {
    /// Execute the A2A command asynchronously.
    ///
    /// Dispatches to the appropriate handler based on the command variant.
    pub async fn execute(self) -> Result<()> {
        match self {
            Self::Create {
                title,
                description,
                agent,
                assign_to,
                output,
            } => Self::create_task(title, description, agent, assign_to, output).await,
            Self::Transition {
                task,
                state,
                reason,
            } => Self::transition_state(task, state, reason).await,
            Self::Status { task, format } => Self::show_status(task, format).await,
            Self::List {
                directory,
                state,
                format,
            } => Self::list_tasks(directory, state, format).await,
            Self::Validate { task } => Self::validate_task(task).await,
            Self::AddArtifact {
                task,
                name,
                content,
                artifact_type,
            } => Self::add_artifact(task, name, content, artifact_type).await,
        }
    }

    /// Create a new task.
    ///
    /// Generates a task with initial state "created" and writes it to a JSON file.
    async fn create_task(
        title: String, description: Option<String>, agent: String, assign_to: Option<String>,
        output: PathBuf,
    ) -> Result<()> {
        let task_id = format!("task-{}", chrono::Utc::now().timestamp());
        let now = chrono::Utc::now().to_rfc3339();

        let task = Task {
            id: task_id.clone(),
            state: "created".to_string(),
            title: title.clone(),
            description,
            assigned_to: assign_to,
            created_by: agent,
            created_at: now.clone(),
            updated_at: now,
            completed_at: None,
            failure_reason: None,
            artifacts: Vec::new(),
        };

        let json = serde_json::to_string_pretty(&task)?;
        tokio::fs::write(&output, json).await?;

        println!("✓ Task created: {}", task_id);
        println!("  Title: {}", title);
        println!("  State: created");
        println!("  File: {}", output.display());

        Ok(())
    }

    /// Transition a task to a new state.
    ///
    /// Updates the task's state and validates the state machine transition.
    /// Records completion timestamps for terminal states.
    async fn transition_state(
        task_path: PathBuf, state: TaskStateArg, reason: Option<String>,
    ) -> Result<()> {
        let content = tokio::fs::read_to_string(&task_path).await?;
        let mut task: Task = serde_json::from_str(&content)?;

        let old_state = task.state.clone();
        let new_state = match state {
            TaskStateArg::Created => "created",
            TaskStateArg::Running => "running",
            TaskStateArg::Blocked => "blocked",
            TaskStateArg::Completed => "completed",
            TaskStateArg::Failed => "failed",
        }
        .to_string();

        // Validate state transition
        Self::validate_transition(&old_state, &new_state)?;

        if new_state == "failed" && reason.is_none() {
            return Err(CliError::TaskStateError(
                "Failure reason required for failed state".to_string(),
            ));
        }

        task.state = new_state.clone();
        task.updated_at = chrono::Utc::now().to_rfc3339();

        if new_state == "completed" || new_state == "failed" {
            task.completed_at = Some(chrono::Utc::now().to_rfc3339());
        }

        if new_state == "failed" {
            task.failure_reason = reason.clone();
        }

        let json = serde_json::to_string_pretty(&task)?;
        tokio::fs::write(&task_path, json).await?;

        println!("✓ Task state transitioned");
        println!("  {} → {}", old_state, new_state);
        if let Some(r) = reason {
            println!("  Reason: {}", r);
        }

        Ok(())
    }

    /// Display detailed task status.
    ///
    /// Shows all task metadata and artifacts in the specified format.
    async fn show_status(task_path: PathBuf, format: OutputFormat) -> Result<()> {
        let content = tokio::fs::read_to_string(&task_path).await?;
        let task: Task = serde_json::from_str(&content)?;

        match format {
            OutputFormat::Json => {
                let json = serde_json::to_string_pretty(&task)?;
                println!("{}", json);
            }
            OutputFormat::Text => {
                println!("Task Status");
                println!("===========");
                println!("ID: {}", task.id);
                println!("Title: {}", task.title);
                println!("State: {}", task.state);
                println!("Created By: {}", task.created_by);
                if let Some(assigned) = &task.assigned_to {
                    println!("Assigned To: {}", assigned);
                }
                println!("Created: {}", task.created_at);
                println!("Updated: {}", task.updated_at);
                if let Some(completed) = &task.completed_at {
                    println!("Completed: {}", completed);
                }
                if let Some(reason) = &task.failure_reason {
                    println!("Failure Reason: {}", reason);
                }
                if !task.artifacts.is_empty() {
                    println!("\nArtifacts:");
                    for artifact in &task.artifacts {
                        println!("  - {} [{}]", artifact.name, artifact.artifact_type);
                    }
                }
            }
        }

        Ok(())
    }

    /// List all tasks in a directory with optional state filtering.
    ///
    /// Scans directory for task JSON files and displays summary information.
    async fn list_tasks(
        directory: PathBuf, state_filter: Option<TaskStateArg>, format: OutputFormat,
    ) -> Result<()> {
        let mut tasks = Vec::new();

        let mut entries = tokio::fs::read_dir(&directory).await?;
        while let Some(entry) = entries.next_entry().await? {
            let path = entry.path();
            if path.extension().and_then(|s| s.to_str()) == Some("json") {
                if let Ok(content) = tokio::fs::read_to_string(&path).await {
                    if let Ok(task) = serde_json::from_str::<Task>(&content) {
                        if let Some(ref filter) = state_filter {
                            let filter_state = match filter {
                                TaskStateArg::Created => "created",
                                TaskStateArg::Running => "running",
                                TaskStateArg::Blocked => "blocked",
                                TaskStateArg::Completed => "completed",
                                TaskStateArg::Failed => "failed",
                            };
                            if task.state == filter_state {
                                tasks.push(task);
                            }
                        } else {
                            tasks.push(task);
                        }
                    }
                }
            }
        }

        match format {
            OutputFormat::Json => {
                let json = serde_json::to_string_pretty(&tasks)?;
                println!("{}", json);
            }
            OutputFormat::Text => {
                println!("Tasks ({})", tasks.len());
                println!("==========");
                for task in tasks {
                    println!(
                        "{} [{}] {} (by {})",
                        task.id, task.state, task.title, task.created_by
                    );
                }
            }
        }

        Ok(())
    }

    /// Validate a task against the state machine schema.
    ///
    /// Checks that required fields are present and state transitions are valid.
    async fn validate_task(task_path: PathBuf) -> Result<()> {
        let content = tokio::fs::read_to_string(&task_path).await?;
        let task: Task = serde_json::from_str(&content)?;

        // Validate required fields
        if task.id.is_empty() {
            return Err(CliError::Validation("Task ID is empty".to_string()));
        }

        if task.title.is_empty() {
            return Err(CliError::Validation("Task title is empty".to_string()));
        }

        if task.created_by.is_empty() {
            return Err(CliError::Validation("Task creator is empty".to_string()));
        }

        // Validate state
        let valid_states = ["created", "running", "blocked", "completed", "failed"];
        if !valid_states.contains(&task.state.as_str()) {
            return Err(CliError::Validation(format!(
                "Invalid state: {}",
                task.state
            )));
        }

        // Validate terminal states
        if task.state == "failed" && task.failure_reason.is_none() {
            return Err(CliError::Validation(
                "Failed task must have failure reason".to_string(),
            ));
        }

        if (task.state == "completed" || task.state == "failed") && task.completed_at.is_none() {
            return Err(CliError::Validation(
                "Terminal state must have completion timestamp".to_string(),
            ));
        }

        println!("✓ Task validation passed");
        println!("  ID: {}", task.id);
        println!("  State: {}", task.state);

        Ok(())
    }

    /// Add an artifact to a task.
    ///
    /// Attaches an artifact file to a task and updates its modification timestamp.
    async fn add_artifact(
        task_path: PathBuf, name: String, content_path: PathBuf, artifact_type: ArtifactTypeArg,
    ) -> Result<()> {
        let task_content = tokio::fs::read_to_string(&task_path).await?;
        let mut task: Task = serde_json::from_str(&task_content)?;

        let artifact_content = tokio::fs::read_to_string(&content_path).await?;

        let artifact_type_str = match artifact_type {
            ArtifactTypeArg::Input => "input",
            ArtifactTypeArg::Output => "output",
            ArtifactTypeArg::Intermediate => "intermediate",
        }
        .to_string();

        let artifact = TaskArtifact {
            name: name.clone(),
            artifact_type: artifact_type_str.clone(),
            content: artifact_content,
        };

        task.artifacts.push(artifact);
        task.updated_at = chrono::Utc::now().to_rfc3339();

        let json = serde_json::to_string_pretty(&task)?;
        tokio::fs::write(&task_path, json).await?;

        println!("✓ Artifact added to task");
        println!("  Name: {}", name);
        println!("  Type: {}", artifact_type_str);

        Ok(())
    }

    /// Validate that a task state transition is allowed.
    ///
    /// Checks against the valid state machine transitions.
    fn validate_transition(from: &str, to: &str) -> Result<()> {
        let valid_transitions = [
            ("created", "running"),
            ("created", "failed"),
            ("running", "blocked"),
            ("running", "completed"),
            ("running", "failed"),
            ("blocked", "running"),
            ("blocked", "failed"),
        ];

        if valid_transitions.contains(&(from, to)) {
            Ok(())
        } else {
            Err(CliError::TaskStateError(format!(
                "Invalid state transition: {} → {}",
                from, to
            )))
        }
    }
}
