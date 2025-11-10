use workflow_engine::prelude::*;

#[tokio::main]
async fn main() -> Result<()> {
    println!("=== Parallel Gateway Example ===\n");

    // Create a workflow with parallel gateway
    let workflow = WorkflowManager::create_from_file(
        "examples/bpmn/parallel-gateway.bpmn",
        "Parallel Approval Workflow",
        "1.0.0"
    )?;

    println!("Created workflow: {} ({})", workflow.name, workflow.id);

    // Deploy
    WorkflowManager::deploy(&workflow.id, "production")?;
    println!("Deployed workflow\n");

    // Start process with multiple approvers
    let variables = serde_json::json!({
        "amount": 10000,
        "requester": "john.doe@example.com",
        "approvers": ["manager@example.com", "finance@example.com"]
    });

    let instance = ProcessExecutor::start(&workflow.id, variables).await?;
    println!("Started parallel approval process: {}\n", instance.id);

    // Simulate parallel task completion
    println!("Completing parallel tasks:");

    // Task 1: Manager approval
    let task1_vars = serde_json::json!({"approved": true, "comments": "Approved by manager"});
    TaskExecutor::complete("task-manager", task1_vars).await?;
    println!("  ✓ Manager approval completed");

    // Task 2: Finance approval (runs in parallel)
    let task2_vars = serde_json::json!({"approved": true, "comments": "Approved by finance"});
    TaskExecutor::complete("task-finance", task2_vars).await?;
    println!("  ✓ Finance approval completed\n");

    // Get execution trace
    let trace = InstanceTracker::trace(&instance.id, "tree").await?;
    println!("Execution trace:\n{}", trace);

    // Get final status
    let status = ProcessExecutor::get_status(&instance.id, true).await?;
    println!("\nFinal status: {}", status.state);

    Ok(())
}
