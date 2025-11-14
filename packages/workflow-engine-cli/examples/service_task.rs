use workflow_engine::prelude::*;

#[tokio::main]
async fn main() -> Result<()> {
    println!("=== Service Task Example ===\n");

    // Create a workflow with service task
    let workflow = WorkflowManager::create_from_file(
        "examples/bpmn/service-task.bpmn",
        "Service Task Workflow",
        "1.0.0"
    )?;

    println!("Created workflow: {} ({})", workflow.name, workflow.id);

    // Deploy the workflow
    WorkflowManager::deploy(&workflow.id, "production")?;
    println!("Deployed workflow to production\n");

    // Start a process instance
    let variables = serde_json::json!({
        "api_endpoint": "https://api.example.com/data",
        "method": "GET"
    });

    let instance = ProcessExecutor::start(&workflow.id, variables).await?;
    println!("Started process instance: {}", instance.id);
    println!("State: {}\n", instance.state);

    // Wait for service task completion
    tokio::time::sleep(tokio::time::Duration::from_secs(2)).await;

    // Check status
    let status = ProcessExecutor::get_status(&instance.id, true).await?;
    println!("Process status: {}", status.state);

    // Get metrics
    let metrics = InstanceTracker::metrics(&instance.id, true).await?;
    println!("\nMetrics:");
    println!("  Duration: {}ms", metrics.duration_ms);
    println!("  Tasks completed: {}", metrics.tasks_completed);

    Ok(())
}
