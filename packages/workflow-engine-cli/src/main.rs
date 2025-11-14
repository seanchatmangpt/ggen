use anyhow::Result;
use clap::{Parser, Subcommand};
use tracing::{info, Level};
use tracing_subscriber::FmtSubscriber;
use workflow_engine::prelude::*;

#[derive(Parser)]
#[command(name = "workflow-engine")]
#[command(about = "BPMN workflow execution and orchestration", long_about = None)]
#[command(version)]
struct Cli {
    #[command(subcommand)]
    noun: Noun,
}

#[derive(Subcommand)]
enum Noun {
    /// Manage BPMN workflow definitions
    Workflow {
        #[command(subcommand)]
        verb: WorkflowVerb,
    },
    /// Manage workflow process instances
    Process {
        #[command(subcommand)]
        verb: ProcessVerb,
    },
    /// Manage workflow tasks
    Task {
        #[command(subcommand)]
        verb: TaskVerb,
    },
    /// Track and analyze process instances
    Instance {
        #[command(subcommand)]
        verb: InstanceVerb,
    },
}

#[derive(Subcommand)]
enum WorkflowVerb {
    /// Create a new workflow definition
    Create {
        /// Path to BPMN file
        #[arg(short, long)]
        file: String,
        /// Workflow name
        #[arg(short, long)]
        name: String,
        /// Workflow version
        #[arg(short, long)]
        version: String,
    },
    /// Deploy a workflow to the execution engine
    Deploy {
        /// Workflow ID to deploy
        #[arg(short, long)]
        workflow_id: String,
        /// Target environment
        #[arg(short, long, default_value = "production")]
        environment: String,
        /// Validate before deployment
        #[arg(long)]
        validate: bool,
    },
    /// List all deployed workflows
    List {
        /// Filter by status
        #[arg(short, long)]
        status: Option<String>,
        /// Filter expression
        #[arg(short, long)]
        filter: Option<String>,
        /// Maximum results
        #[arg(short, long, default_value = "20")]
        limit: usize,
    },
    /// Validate BPMN workflow definition
    Validate {
        /// Path to BPMN file
        #[arg(short, long)]
        file: String,
        /// Strict validation
        #[arg(long)]
        strict: bool,
    },
    /// Create a new version of a workflow
    Version {
        /// Workflow ID
        #[arg(short, long)]
        workflow_id: String,
        /// New version number
        #[arg(short, long)]
        new_version: String,
    },
}

#[derive(Subcommand)]
enum ProcessVerb {
    /// Start a new process instance
    Start {
        /// Workflow ID
        #[arg(short, long)]
        workflow_id: String,
        /// Process variables as JSON
        #[arg(short, long)]
        variables: String,
    },
    /// Pause a running process
    Pause {
        /// Instance ID
        #[arg(short, long)]
        instance_id: String,
        /// Reason for pausing
        #[arg(short, long)]
        reason: Option<String>,
    },
    /// Resume a paused process
    Resume {
        /// Instance ID
        #[arg(short, long)]
        instance_id: String,
    },
    /// Abort a process instance
    Abort {
        /// Instance ID
        #[arg(short, long)]
        instance_id: String,
        /// Reason for aborting
        #[arg(short, long)]
        reason: Option<String>,
    },
    /// Get process status
    Status {
        /// Instance ID
        #[arg(short, long)]
        instance_id: String,
        /// Include task details
        #[arg(long)]
        include_tasks: bool,
    },
}

#[derive(Subcommand)]
enum TaskVerb {
    /// Assign a task to a user
    Assign {
        /// Task ID
        #[arg(short, long)]
        task_id: String,
        /// Assignee user ID
        #[arg(short, long)]
        assignee: String,
        /// Task priority (0-100)
        #[arg(short, long, default_value = "50")]
        priority: u8,
    },
    /// Complete a task
    Complete {
        /// Task ID
        #[arg(short, long)]
        task_id: String,
        /// Output variables as JSON
        #[arg(short, long)]
        variables: String,
    },
    /// Delegate a task to another user
    Delegate {
        /// Task ID
        #[arg(short, long)]
        task_id: String,
        /// Source user
        #[arg(short, long)]
        from: String,
        /// Target user
        #[arg(short, long)]
        to: String,
    },
    /// Escalate an overdue task
    Escalate {
        /// Task ID
        #[arg(short, long)]
        task_id: String,
        /// Escalation target
        #[arg(short, long)]
        to: String,
        /// Escalation reason
        #[arg(short, long)]
        reason: String,
    },
    /// Retry a failed service task
    Retry {
        /// Task ID
        #[arg(short, long)]
        task_id: String,
        /// Maximum retry attempts
        #[arg(short, long, default_value = "3")]
        max_attempts: u32,
    },
}

#[derive(Subcommand)]
enum InstanceVerb {
    /// List process instances
    List {
        /// Workflow ID filter
        #[arg(short, long)]
        workflow_id: Option<String>,
        /// State filter
        #[arg(short, long)]
        state: Option<String>,
        /// Maximum results
        #[arg(short, long, default_value = "50")]
        limit: usize,
    },
    /// Show instance details
    Show {
        /// Instance ID
        #[arg(short, long)]
        instance_id: String,
        /// Include task details
        #[arg(long)]
        include_tasks: bool,
        /// Include variables
        #[arg(long)]
        include_variables: bool,
    },
    /// Show execution trace
    Trace {
        /// Instance ID
        #[arg(short, long)]
        instance_id: String,
        /// Output format (tree, list)
        #[arg(short, long, default_value = "tree")]
        format: String,
    },
    /// Show execution history
    History {
        /// Instance ID
        #[arg(short, long)]
        instance_id: String,
    },
    /// Show performance metrics
    Metrics {
        /// Instance ID
        #[arg(short, long)]
        instance_id: String,
        /// Include task durations
        #[arg(long)]
        include_task_durations: bool,
    },
}

#[tokio::main]
async fn main() -> Result<()> {
    // Initialize tracing
    let subscriber = FmtSubscriber::builder()
        .with_max_level(Level::INFO)
        .finish();
    tracing::subscriber::set_global_default(subscriber)?;

    let cli = Cli::parse();

    match cli.noun {
        Noun::Workflow { verb } => handle_workflow_verb(verb).await?,
        Noun::Process { verb } => handle_process_verb(verb).await?,
        Noun::Task { verb } => handle_task_verb(verb).await?,
        Noun::Instance { verb } => handle_instance_verb(verb).await?,
    }

    Ok(())
}

async fn handle_workflow_verb(verb: WorkflowVerb) -> Result<()> {
    match verb {
        WorkflowVerb::Create { file, name, version } => {
            info!("Creating workflow: {} v{} from {}", name, version, file);
            let workflow = WorkflowManager::create_from_file(&file, &name, &version)?;
            println!("Workflow created: {}", workflow.id);
        }
        WorkflowVerb::Deploy { workflow_id, environment, validate } => {
            info!("Deploying workflow {} to {}", workflow_id, environment);
            if validate {
                WorkflowManager::validate(&workflow_id)?;
            }
            WorkflowManager::deploy(&workflow_id, &environment)?;
            println!("Workflow deployed successfully");
        }
        WorkflowVerb::List { status, filter, limit } => {
            info!("Listing workflows");
            let workflows = WorkflowManager::list(status.as_deref(), filter.as_deref(), limit)?;
            println!("Found {} workflows:", workflows.len());
            for wf in workflows {
                println!("  {} - {} ({})", wf.id, wf.name, wf.version);
            }
        }
        WorkflowVerb::Validate { file, strict } => {
            info!("Validating workflow: {}", file);
            WorkflowManager::validate_file(&file, strict)?;
            println!("Validation successful");
        }
        WorkflowVerb::Version { workflow_id, new_version } => {
            info!("Creating new version: {} -> {}", workflow_id, new_version);
            let new_wf = WorkflowManager::create_version(&workflow_id, &new_version)?;
            println!("New version created: {}", new_wf.id);
        }
    }
    Ok(())
}

async fn handle_process_verb(verb: ProcessVerb) -> Result<()> {
    match verb {
        ProcessVerb::Start { workflow_id, variables } => {
            info!("Starting process for workflow: {}", workflow_id);
            let vars: serde_json::Value = serde_json::from_str(&variables)?;
            let instance = ProcessExecutor::start(&workflow_id, vars).await?;
            println!("Process started: {}", instance.id);
        }
        ProcessVerb::Pause { instance_id, reason } => {
            info!("Pausing process: {}", instance_id);
            ProcessExecutor::pause(&instance_id, reason.as_deref()).await?;
            println!("Process paused");
        }
        ProcessVerb::Resume { instance_id } => {
            info!("Resuming process: {}", instance_id);
            ProcessExecutor::resume(&instance_id).await?;
            println!("Process resumed");
        }
        ProcessVerb::Abort { instance_id, reason } => {
            info!("Aborting process: {}", instance_id);
            ProcessExecutor::abort(&instance_id, reason.as_deref()).await?;
            println!("Process aborted");
        }
        ProcessVerb::Status { instance_id, include_tasks } => {
            info!("Getting status for: {}", instance_id);
            let status = ProcessExecutor::get_status(&instance_id, include_tasks).await?;
            println!("Process Status:");
            println!("  State: {}", status.state);
            println!("  Started: {}", status.start_time);
            if let Some(tasks) = status.tasks {
                println!("  Active Tasks: {}", tasks.len());
            }
        }
    }
    Ok(())
}

async fn handle_task_verb(verb: TaskVerb) -> Result<()> {
    match verb {
        TaskVerb::Assign { task_id, assignee, priority } => {
            info!("Assigning task {} to {}", task_id, assignee);
            TaskExecutor::assign(&task_id, &assignee, priority).await?;
            println!("Task assigned");
        }
        TaskVerb::Complete { task_id, variables } => {
            info!("Completing task: {}", task_id);
            let vars: serde_json::Value = serde_json::from_str(&variables)?;
            TaskExecutor::complete(&task_id, vars).await?;
            println!("Task completed");
        }
        TaskVerb::Delegate { task_id, from, to } => {
            info!("Delegating task {} from {} to {}", task_id, from, to);
            TaskExecutor::delegate(&task_id, &from, &to).await?;
            println!("Task delegated");
        }
        TaskVerb::Escalate { task_id, to, reason } => {
            info!("Escalating task {} to {}", task_id, to);
            TaskExecutor::escalate(&task_id, &to, &reason).await?;
            println!("Task escalated");
        }
        TaskVerb::Retry { task_id, max_attempts } => {
            info!("Retrying task {} (max {} attempts)", task_id, max_attempts);
            TaskExecutor::retry(&task_id, max_attempts).await?;
            println!("Task retry initiated");
        }
    }
    Ok(())
}

async fn handle_instance_verb(verb: InstanceVerb) -> Result<()> {
    match verb {
        InstanceVerb::List { workflow_id, state, limit } => {
            info!("Listing instances");
            let instances = InstanceTracker::list(workflow_id.as_deref(), state.as_deref(), limit).await?;
            println!("Found {} instances:", instances.len());
            for inst in instances {
                println!("  {} - {} ({})", inst.id, inst.workflow_id, inst.state);
            }
        }
        InstanceVerb::Show { instance_id, include_tasks, include_variables } => {
            info!("Showing instance: {}", instance_id);
            let details = InstanceTracker::show(&instance_id, include_tasks, include_variables).await?;
            println!("Instance: {}", details.id);
            println!("  Workflow: {}", details.workflow_id);
            println!("  State: {}", details.state);
            println!("  Started: {}", details.start_time);
        }
        InstanceVerb::Trace { instance_id, format } => {
            info!("Tracing instance: {}", instance_id);
            let trace = InstanceTracker::trace(&instance_id, &format).await?;
            println!("Execution Trace:\n{}", trace);
        }
        InstanceVerb::History { instance_id } => {
            info!("Getting history for: {}", instance_id);
            let history = InstanceTracker::history(&instance_id).await?;
            println!("Execution History ({} events):", history.len());
            for event in history {
                println!("  {} - {}", event.timestamp, event.description);
            }
        }
        InstanceVerb::Metrics { instance_id, include_task_durations } => {
            info!("Getting metrics for: {}", instance_id);
            let metrics = InstanceTracker::metrics(&instance_id, include_task_durations).await?;
            println!("Performance Metrics:");
            println!("  Duration: {}ms", metrics.duration_ms);
            println!("  Tasks Completed: {}", metrics.tasks_completed);
        }
    }
    Ok(())
}
