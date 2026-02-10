//! Packet Routing Example
//!
//! Demonstrates work order routing between agents using the A2A transport.
//! Tasks flow through multiple agents with message passing.

use ggen_a2a::{
    transport::{Envelope, TaskMessage, Transport},
    Task, TaskState,
};
use std::sync::Arc;
use tokio::time::{sleep, Duration};
use uuid::Uuid;

#[tokio::main]
async fn main() -> Result<(), Box<dyn std::error::Error>> {
    println!("=== Packet Routing Example ===\n");
    println!("Demonstrating work order routing through agents\n");

    // Create transport layer
    let transport = Arc::new(Transport::new());
    println!("✓ Transport layer initialized\n");

    // Register agents
    println!("Registering agents:");
    let mut coordinator_rx = transport.register_agent("coordinator".to_string()).await?;
    println!("  - coordinator");

    let mut parser_rx = transport.register_agent("parser".to_string()).await?;
    println!("  - parser");

    let mut validator_rx = transport.register_agent("validator".to_string()).await?;
    println!("  - validator");

    let mut generator_rx = transport.register_agent("generator".to_string()).await?;
    println!("  - generator\n");

    // Create work order
    let task = Task::new(
        "Generate API from OpenAPI spec".to_string(),
        "coordinator".to_string(),
    )
    .with_description("Parse, validate, and generate code from spec.yaml".to_string());

    let task_id = task.id;
    transport.store_task(task.clone()).await?;

    println!("Created work order:");
    println!("  ID: {}", task_id);
    println!("  Title: {}", task.title);
    println!("  State: {:?}\n", task.state);

    // Start agent workers
    let t = transport.clone();
    tokio::spawn(async move {
        parser_agent(parser_rx, t).await;
    });

    let t = transport.clone();
    tokio::spawn(async move {
        validator_agent(validator_rx, t).await;
    });

    let t = transport.clone();
    tokio::spawn(async move {
        generator_agent(generator_rx, t).await;
    });

    // Coordinator initiates routing
    println!("Step 1: Coordinator → Parser");
    transport
        .send(
            "coordinator".to_string(),
            "parser".to_string(),
            TaskMessage::Assign {
                task_id,
                agent_id: "parser".to_string(),
            },
        )
        .await?;

    // Wait for processing
    sleep(Duration::from_millis(500)).await;

    // Check task progress
    let task = transport.get_task(task_id).await?;
    println!("  Task state: {:?}", task.state);
    println!("  Assigned to: {:?}", task.assigned_to);

    // Route to validator
    println!("\nStep 2: Parser → Validator");
    transport
        .send(
            "parser".to_string(),
            "validator".to_string(),
            TaskMessage::Assign {
                task_id,
                agent_id: "validator".to_string(),
            },
        )
        .await?;

    sleep(Duration::from_millis(500)).await;

    // Route to generator
    println!("\nStep 3: Validator → Generator");
    transport
        .send(
            "validator".to_string(),
            "generator".to_string(),
            TaskMessage::Assign {
                task_id,
                agent_id: "generator".to_string(),
            },
        )
        .await?;

    sleep(Duration::from_millis(500)).await;

    // Final status
    let final_task = transport.get_task(task_id).await?;
    println!("\n=== Final Status ===");
    println!("Task ID: {}", final_task.id);
    println!("State: {:?}", final_task.state);
    println!("Assigned to: {:?}", final_task.assigned_to);

    if final_task.is_terminal() {
        println!("✓ Work order completed");
        if let Some(duration) = final_task.duration() {
            println!("Duration: {:?}", duration);
        }
    }

    // Stats
    println!("\n=== Transport Stats ===");
    println!("Active agents: {}", transport.agent_count().await);
    println!("Total tasks: {}", transport.task_count().await);

    println!("\n✓ Packet routing example completed");
    Ok(())
}

async fn parser_agent(mut rx: tokio::sync::mpsc::UnboundedReceiver<Envelope>, transport: Arc<Transport>) {
    while let Some(envelope) = rx.recv().await {
        if let TaskMessage::Assign { task_id, agent_id } = envelope.message {
            println!("  [Parser] Received task {}", task_id);

            // Update task
            let mut task = transport.get_task(task_id).await.unwrap();
            task.assigned_to = Some(agent_id);
            task.state = TaskState::Running;
            transport.update_task(task).await.unwrap();

            // Simulate parsing
            sleep(Duration::from_millis(200)).await;
            println!("  [Parser] Completed parsing");
        }
    }
}

async fn validator_agent(mut rx: tokio::sync::mpsc::UnboundedReceiver<Envelope>, transport: Arc<Transport>) {
    while let Some(envelope) = rx.recv().await {
        if let TaskMessage::Assign { task_id, agent_id } = envelope.message {
            println!("  [Validator] Received task {}", task_id);

            // Update task
            let mut task = transport.get_task(task_id).await.unwrap();
            task.assigned_to = Some(agent_id);
            transport.update_task(task).await.unwrap();

            // Simulate validation
            sleep(Duration::from_millis(200)).await;
            println!("  [Validator] Validation passed");
        }
    }
}

async fn generator_agent(mut rx: tokio::sync::mpsc::UnboundedReceiver<Envelope>, transport: Arc<Transport>) {
    while let Some(envelope) = rx.recv().await {
        if let TaskMessage::Assign { task_id, agent_id } = envelope.message {
            println!("  [Generator] Received task {}", task_id);

            // Update task
            let mut task = transport.get_task(task_id).await.unwrap();
            task.assigned_to = Some(agent_id);
            transport.update_task(task).await.unwrap();

            // Simulate generation
            sleep(Duration::from_millis(200)).await;

            // Complete task
            task.state = TaskState::Completed;
            task.completed_at = Some(chrono::Utc::now());
            transport.update_task(task).await.unwrap();

            println!("  [Generator] Code generation complete");
        }
    }
}
