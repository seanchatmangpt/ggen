use workflow_engine::prelude::*;

#[tokio::main]
async fn main() -> Result<()> {
    println!("=== Event-Driven Workflow Example ===\n");

    // Create workflow with event handling
    let workflow = WorkflowManager::create_from_file(
        "examples/bpmn/event-driven.bpmn",
        "Event-Driven Order Process",
        "1.0.0"
    )?;

    println!("Created workflow: {}\n", workflow.name);

    // Deploy
    WorkflowManager::deploy(&workflow.id, "production")?;

    // Start process that waits for payment event
    let variables = serde_json::json!({
        "order_id": "ORD-12345",
        "amount": 99.99,
        "customer_id": "CUST-001"
    });

    let instance = ProcessExecutor::start(&workflow.id, variables).await?;
    println!("Started order process: {}", instance.id);
    println!("Waiting for payment event...\n");

    // Simulate waiting for external event
    tokio::time::sleep(tokio::time::Duration::from_secs(2)).await;

    // Simulate payment received event
    println!("Payment received event triggered");

    // Process continues after event
    println!("Processing order fulfillment\n");

    // Check execution history
    let history = InstanceTracker::history(&instance.id).await?;
    println!("Execution history:");
    for event in history {
        println!("  {} - {}", event.timestamp, event.description);
    }

    Ok(())
}
