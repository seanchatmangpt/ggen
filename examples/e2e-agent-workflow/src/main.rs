//! E2E Agent Workflow Example - Main entry point

use e2e_agent_workflow::{AgentPool, DomainManager, LifeDomain, initialize_standard_tools};

#[tokio::main]
async fn main() {
    println!("=== End-to-End Agent Workflow Example ===\n");

    // Initialize agent pool
    let pool = AgentPool::new(6);
    println!("Created agent pool with {} agents\n", pool.all().len());

    // Initialize domain manager
    let mut domain_mgr = DomainManager::new();
    for domain in &[
        LifeDomain::Health,
        LifeDomain::Mental,
        LifeDomain::Financial,
        LifeDomain::Social,
        LifeDomain::Career,
        LifeDomain::Learning,
    ] {
        let goal = e2e_agent_workflow::DomainGoal::new(
            format!("Achieve goals in {:?}", domain),
            *domain,
            8,
        );
        domain_mgr.add_goal(goal);
    }
    println!("Initialized {} domain goals\n", domain_mgr.all_goals().len());

    // Initialize tool manager
    let tool_mgr = initialize_standard_tools();
    println!("Discovered {} MCP tools\n", tool_mgr.available_tools().len());

    // Print system health
    let health = pool.health_summary();
    println!("System Health:");
    println!("  Total agents: {}", health.total_agents);
    println!("  Healthy: {}", health.healthy_count);
    println!("  System health: {:.1}%\n", health.system_health_percentage);

    // Print domain balance
    let balance = domain_mgr.calculate_balance_score();
    println!("Domain Balance Score: {:.1}%\n", balance);

    println!("=== System initialized successfully ===");
}
