//! End-to-end agent workflow example
//!
//! Demonstrates complete integration of:
//! - A2A agents with task state machines
//! - OSIRIS domains with goal tracking
//! - MCP tools with autonomous discovery and planning
//! - Byzantine consensus for multi-agent coordination

use e2e_agent_workflow::{
    orchestration::OrchestrationEngine,
    Result,
};
use colored::Colorize;
use std::io::Write;

#[tokio::main]
async fn main() -> Result<()> {
    // Initialize logging
    tracing_subscriber::fmt()
        .with_max_level(tracing::Level::INFO)
        .init();

    println!("\n{}", "=".repeat(80));
    println!("{}", "End-to-End Agent Workflow Example".bold());
    println!("{}", "Integrating A2A, OSIRIS Domains, and MCP Tools".italic());
    println!("{}", "=".repeat(80).dimmed());

    // Create orchestration engine
    println!("\n{} Creating orchestration engine...", "→".green());
    let mut engine = OrchestrationEngine::new();

    // Initialize with 3 agents and 8 tools
    println!("{} Initializing with 3 agents and 8 tools...", "→".green());
    engine.initialize(3, 8)?;
    println!("  {} Initialization complete", "✓".green());

    // Execute complete workflow
    println!("\n{} Executing workflow phases:", "→".green());
    println!("  1. Initialization");
    println!("  2. Planning");
    println!("  3. Execution");
    println!("  4. Analysis");
    println!("  5. Learning");

    std::io::stdout().flush()?;

    let result = engine.execute_workflow()?;

    // Display results
    println!("\n{}", "=".repeat(80));
    println!("{}", "Workflow Execution Results".bold());
    println!("{}", "=".repeat(80).dimmed());

    println!("\n{} Workflow Status: {}", "✓".green(),
        if result.success { "SUCCESS".green() } else { "FAILED".red() });

    println!("\n{} Agent Statistics:", "📊".cyan());
    println!("  Created: {}", result.agents_created);
    println!("  Ready: {}", result.agents_ready);
    println!("  Tasks Completed: {}", result.tasks_completed);

    println!("\n{} Tool Registry:", "🔧".cyan());
    println!("  Available Tools: {}", result.tools_available);

    println!("\n{} Life Domain Balance:", "⚖️ ".cyan());
    let balance = &result.domain_balance;
    print_domain_balance(balance);

    println!("\n{} Workflow Summary:", "📋".cyan());
    let summary = &result.workflow_summary;
    println!("  ID: {}", summary.id);
    println!("  Phase: {}", summary.phase);
    println!("  Error Count: {}", summary.error_count);

    println!("\n{} Execution Events: {}", "📝".cyan(), result.events_logged);

    // Print execution log
    println!("\n{}", "Execution Log:".underline());
    for (i, event) in engine.get_log().iter().enumerate() {
        println!("  [{}] {} - {} ({})",
            i + 1,
            event.timestamp.format("%H:%M:%S").to_string().dimmed(),
            event.description,
            event.status.yellow()
        );
    }

    println!("\n{}", "=".repeat(80));
    println!("{} Example completed successfully!", "✓".green().bold());
    println!("{}", "=".repeat(80));

    Ok(())
}

fn print_domain_balance(balance: &e2e_agent_workflow::domain::DomainBalance) {
    let domains = vec![
        ("Health", balance.health),
        ("Work", balance.work),
        ("Relationships", balance.relationships),
        ("Finances", balance.finances),
        ("Learning", balance.learning),
        ("Creativity", balance.creativity),
        ("Spirituality", balance.spirituality),
        ("Recreation", balance.recreation),
    ];

    for (name, value) in domains {
        let bar = create_progress_bar(value);
        let color_value = format!("{:.2}", value);
        let colored = if value >= 0.7 {
            color_value.green()
        } else if value >= 0.4 {
            color_value.yellow()
        } else {
            color_value.red()
        };
        println!("  {:<15} {} {}", name, bar, colored);
    }

    println!("  {:<15} Overall: {:.2}", "", balance.overall_balance);
}

fn create_progress_bar(value: f64) -> String {
    let filled = (value * 20.0) as usize;
    let empty = 20 - filled;
    let bar = format!("[{}{}]", "█".repeat(filled), "░".repeat(empty));
    bar.cyan().to_string()
}
