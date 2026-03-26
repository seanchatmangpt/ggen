//! OSIRIS Life Domains Example - Autonomous Agent Management
//!
//! Demonstrates Wave 4 capabilities:
//! - 6 autonomous domain agents
//! - Autonomous reasoning and goal setting
//! - Domain consensus and balancing
//! - Self-improvement through outcome tracking
//! - MCP tool integration

use osiris_life_domains::*;
use tracing::info;

#[tokio::main]
async fn main() -> anyhow::Result<()> {
    // Initialize logging
    tracing_subscriber::fmt()
        .with_max_level(tracing::Level::INFO)
        .init();

    info!("Starting OSIRIS Life Domains Example");

    // Create and initialize the system
    let system = LifeDomainsSystem::new().await;
    system.initialize().await?;

    info!("=== Initial System Status ===");
    let status = system.get_system_status().await?;
    println!("{}", serde_json::to_string_pretty(&status)?);

    info!("=== Running Autonomous Reasoning Cycle ===");
    system.reasoning_cycle().await?;

    info!("=== Balancing Domains ===");
    system.balance_domains().await?;

    info!("=== Final System Status ===");
    let final_status = system.get_system_status().await?;
    println!("{}", serde_json::to_string_pretty(&final_status)?);

    info!("OSIRIS Life Domains Example completed successfully");
    Ok(())
}
