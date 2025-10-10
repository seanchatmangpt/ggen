//! # ggen-agents - Ultrathink Multi-Agent CLI
//!
//! Command-line interface for the ultrathink multi-agent system.
//! Demonstrates the 80/20 principle by focusing on the most valuable agent interactions.

use clap::{Parser, Subcommand};
use ggen_agents::{
    agents::{create_all_agents, get_agent_by_name, list_agent_names},
    coordination::{AgentCoordinator, create_ggen_development_plan},
    core::{AgentContext, ExecutionContext, ExecutionState, SerializableTask},
    protocols::Message,
};
use std::collections::HashMap;
use tokio::sync::mpsc;
use uuid::Uuid;

/// Ultrathink multi-agent system for ggen
#[derive(Parser)]
#[command(name = "ggen-agents")]
#[command(about = "Ultrathink multi-agent system for ggen development")]
#[command(version)]
struct Cli {
    #[command(subcommand)]
    command: Commands,
}

#[derive(Subcommand)]
enum Commands {
    /// List all available agents
    ListAgents,
    /// Execute a specific agent
    Execute {
        /// Agent name to execute
        agent: String,
        /// Input data as JSON
        #[arg(short, long)]
        input: Option<String>,
    },
    /// Run the full development workflow
    Workflow {
        /// Project specification as JSON
        #[arg(short, long)]
        spec: Option<String>,
    },
    /// Demonstrate agent coordination
    Demo,
}

#[tokio::main]
async fn main() -> Result<(), Box<dyn std::error::Error>> {
    // Initialize structured logging
    tracing_subscriber::fmt()
        .with_env_filter(tracing_subscriber::EnvFilter::from_default_env())
        .init();

    let cli = Cli::parse();

    match cli.command {
        Commands::ListAgents => {
            println!("🤖 Available Agents:");
            println!("===================");
            for agent_name in list_agent_names() {
                println!("  • {}", agent_name);
            }
        }

        Commands::Execute { agent, input } => {
            let input_data = input
                .as_ref()
                .and_then(|s| serde_json::from_str(s).ok())
                .unwrap_or_else(|| serde_json::json!({}));

            execute_single_agent(&agent, input_data).await?;
        }

        Commands::Workflow { spec } => {
            let project_spec = spec
                .as_ref()
                .and_then(|s| serde_json::from_str(s).ok())
                .unwrap_or_else(|| serde_json::json!({
                    "distributed": true,
                    "stateful": true,
                    "network_sensitive": true,
                    "security_sensitive": true,
                    "real_time": false,
                    "concurrent_access": true
                }));

            execute_workflow(project_spec).await?;
        }

        Commands::Demo => {
            run_demo().await?;
        }
    }

    Ok(())
}

/// Execute a single agent
async fn execute_single_agent(agent_name: &str, input: serde_json::Value) -> Result<(), Box<dyn std::error::Error>> {
    let mut agent = get_agent_by_name(agent_name)
        .ok_or_else(|| format!("Agent '{}' not found", agent_name))?;

    // Create minimal context for agent initialization
    let (message_tx, _message_rx) = mpsc::unbounded_channel();
    let shared_state = std::sync::Arc::new(tokio::sync::RwLock::new(HashMap::new()));

    let context = AgentContext {
        agent_id: agent.id(),
        config: HashMap::new(),
        shared_state,
        message_tx,
    };

    // Initialize agent
    agent.initialize(&context).await?;

    // Create execution context
    let execution_context = ExecutionContext {
        task: SerializableTask {
            task_id: "demo_task".to_string(),
            agent_id: agent.id().to_string(),
            description: "Demo execution".to_string(),
            input: input.clone(),
            output_schema: serde_json::json!({}),
            priority: 50,
            timeout_seconds: Some(30),
        },
        state: ExecutionState::Running,
        input,
        shared_state: std::sync::Arc::new(tokio::sync::RwLock::new(HashMap::new())),
        result_tx: mpsc::unbounded_channel().0,
    };

    // Execute agent
    let result = agent.execute(&execution_context).await?;

    println!("✅ Agent '{}' executed successfully", agent_name);
    println!("📊 Execution time: {}ms", result.duration_ms);
    println!("📋 Result: {}", serde_json::to_string_pretty(&result.output)?);

    Ok(())
}

/// Execute the full development workflow using multiple agents
async fn execute_workflow(project_spec: serde_json::Value) -> Result<(), Box<dyn std::error::Error>> {
    println!("🚀 Executing ultrathink development workflow...");

    // Create agent coordinator
    let coordinator = AgentCoordinator::new();

    // Create development plan
    let plan = create_ggen_development_plan();

    // Execute plan (this would normally involve multiple agents)
    let results = coordinator.execute_plan(plan).await?;

    println!("✅ Workflow completed successfully");
    println!("📊 Results: {} tasks executed", results.len());

    for (task_id, result) in results {
        println!("  • {}: {}ms", task_id, result.duration_ms);
    }

    Ok(())
}

/// Demonstrate agent capabilities
async fn run_demo() -> Result<(), Box<dyn std::error::Error>> {
    println!("🎭 Ultrathink Multi-Agent Demo");
    println!("==============================");

    // Demonstrate London BDD agent
    println!("\n1️⃣  London BDD Agent Demo:");
    let london_bdd_input = serde_json::json!({
        "requirements": "A CLI tool that processes text files and generates reports"
    });
    execute_single_agent("london-bdd", london_bdd_input).await?;

    // Demonstrate Byzantene agent
    println!("\n2️⃣  Byzantene Agent Demo:");
    let byzantene_input = serde_json::json!({
        "distributed": true,
        "stateful": true,
        "network_sensitive": true,
        "security_sensitive": true,
        "real_time": false,
        "concurrent_access": true
    });
    execute_single_agent("byzantene", byzantene_input).await?;

    println!("\n🎉 Demo completed! This demonstrates the 80/20 principle:");
    println!("   • 20% of agent interactions deliver 80% of the value");
    println!("   • London BDD ensures test-driven development");
    println!("   • Byzantene ensures fault-tolerant distributed systems");

    Ok(())
}
