use clap::{Parser, Subcommand};
use core_domain::{InMemoryJobRepository, InMemoryTaskRepository, Job, Task};
use scheduler::JobScheduler;
use std::sync::Arc;
use uuid::Uuid;

// ============================================================================
// CLI COMMANDS
// ============================================================================

#[derive(Parser)]
#[command(name = "lifecycle-demo")]
#[command(about = "Advanced Lifecycle Demo - Multi-crate job orchestration", long_about = None)]
struct Cli {
    #[command(subcommand)]
    command: Commands,
}

#[derive(Subcommand)]
enum Commands {
    /// Create a new job
    Create {
        /// Job name
        #[arg(short, long)]
        name: String,

        /// Number of tasks
        #[arg(short, long, default_value = "3")]
        tasks: usize,
    },
    /// List all jobs
    List,
    /// Get job status
    Status {
        /// Job ID
        #[arg(short, long)]
        job_id: String,
    },
    /// Start a job
    Start {
        /// Job ID
        #[arg(short, long)]
        job_id: String,
    },
    /// Pause a job
    Pause {
        /// Job ID
        #[arg(short, long)]
        job_id: String,
    },
    /// Resume a job
    Resume {
        /// Job ID
        #[arg(short, long)]
        job_id: String,
    },
    /// Execute complete workflow
    Execute {
        /// Job ID
        #[arg(short, long)]
        job_id: String,
    },
    /// Demo workflow
    Demo,
}

// ============================================================================
// MAIN
// ============================================================================

#[tokio::main]
async fn main() -> anyhow::Result<()> {
    tracing_subscriber::fmt::init();

    let cli = Cli::parse();

    // Create repositories and scheduler
    let job_repo = Arc::new(InMemoryJobRepository::new());
    let task_repo = Arc::new(InMemoryTaskRepository::new());
    let scheduler = JobScheduler::new(job_repo, task_repo);

    match cli.command {
        Commands::Create { name, tasks } => {
            let task_list: Vec<Task> = (0..tasks)
                .map(|i| Task::new(format!("Task {}", i + 1)))
                .collect();

            let job = Job::new(name, task_list);
            let submitted = scheduler.submit_job(job).await?;

            println!("Job created: {}", submitted.id);
            println!("Tasks: {}", submitted.tasks.len());
            println!("Status: {:?}", submitted.status);
        }
        Commands::List => {
            let jobs = scheduler.list_jobs().await?;
            println!("Total jobs: {}", jobs.len());
            for job in jobs {
                println!("  - {} ({}) [{}]", job.name, job.id, job.status);
            }
        }
        Commands::Status { job_id } => {
            let id = Uuid::parse_str(&job_id)?;
            let status = scheduler.get_job_status(id).await?;
            println!("Job {} status: {:?}", id, status);
        }
        Commands::Start { job_id } => {
            let id = Uuid::parse_str(&job_id)?;
            let job = scheduler.start_job(id).await?;
            println!("Job started: {} [{}]", job.name, job.status);
        }
        Commands::Pause { job_id } => {
            let id = Uuid::parse_str(&job_id)?;
            let job = scheduler.pause_job(id).await?;
            println!("Job paused: {} [{}]", job.name, job.status);
        }
        Commands::Resume { job_id } => {
            let id = Uuid::parse_str(&job_id)?;
            let job = scheduler.resume_job(id).await?;
            println!("Job resumed: {} [{}]", job.name, job.status);
        }
        Commands::Execute { job_id } => {
            let id = Uuid::parse_str(&job_id)?;
            let job = scheduler.execute_workflow(id).await?;
            println!("Workflow executed: {}", job.name);
            println!("Final status: {:?}", job.status);
            println!("Completed tasks: {}/{}",
                job.tasks.iter().filter(|t| t.status == core_domain::TaskStatus::Completed).count(),
                job.tasks.len());
        }
        Commands::Demo => {
            println!("=== Advanced Lifecycle Demo ===\n");

            // Create a job with 3 tasks
            println!("1. Creating job with 3 tasks...");
            let tasks = vec![
                Task::new("Database Migration"),
                Task::new("Cache Invalidation"),
                Task::new("Service Restart"),
            ];
            let job = Job::new("Production Deployment", tasks);
            let submitted = scheduler.submit_job(job).await?;
            println!("   Job ID: {}", submitted.id);
            println!("   Status: {:?}\n", submitted.status);

            // Start the job
            println!("2. Starting job...");
            let _started = scheduler.start_job(submitted.id).await?;
            println!("   Status: Running\n");

            // Process all tasks
            println!("3. Processing tasks...");
            scheduler.process_job_tasks(submitted.id).await?;
            println!("   All tasks processed\n");

            // Complete the job
            println!("4. Completing job...");
            let completed = scheduler.complete_job(submitted.id).await?;
            println!("   Final Status: {:?}", completed.status);
            println!("   Completed Tasks: {}/{}",
                completed.tasks.iter().filter(|t| t.status == core_domain::TaskStatus::Completed).count(),
                completed.tasks.len());
        }
    }

    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_cli_parser() {
        let args = vec!["cli", "create", "--name", "TestJob", "--tasks", "5"];
        let cli = Cli::try_parse_from(args).unwrap();
        match cli.command {
            Commands::Create { name, tasks } => {
                assert_eq!(name, "TestJob");
                assert_eq!(tasks, 5);
            }
            _ => panic!("Wrong command"),
        }
    }

    #[test]
    fn test_demo_command_parsing() {
        let args = vec!["cli", "demo"];
        let cli = Cli::try_parse_from(args).unwrap();
        match cli.command {
            Commands::Demo => {}
            _ => panic!("Wrong command"),
        }
    }
}
