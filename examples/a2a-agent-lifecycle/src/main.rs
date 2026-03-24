//! Agent Lifecycle Example - Demonstrates state machines, task management, and messaging

use a2a_agent_lifecycle::{
    Agent, AgentState, Message, MessageRouter, MessageType, Task, TaskPriority,
    AgentBridge, Tool,
};
use anyhow::Result;

#[tokio::main]
async fn main() -> Result<()> {
    println!("=== Agent Lifecycle State Machine Example ===\n");

    // Example 1: Agent state transitions
    println!("--- Example 1: Agent State Transitions ---");
    example_state_transitions()?;

    println!("\n--- Example 2: Task Management ---");
    example_task_management()?;

    println!("\n--- Example 3: Message Routing ---");
    example_message_routing()?;

    println!("\n--- Example 4: Agent Bridging ---");
    example_agent_bridging()?;

    println!("\n=== All examples completed successfully ===");
    Ok(())
}

fn example_state_transitions() -> Result<()> {
    let mut agent = Agent::new("WorkerAgent");
    println!("Created agent: {} ({})", agent.name, agent.id);
    println!("Initial state: {}", agent.state_code());

    // Transition through lifecycle
    agent.mark_ready()?;
    println!("After initialization: {}", agent.state_code());

    agent.mark_processing()?;
    println!("After accepting task: {}", agent.state_code());

    agent.mark_idle()?;
    println!("After completing task: {}", agent.state_code());

    // Process another task
    agent.mark_processing()?;
    agent.mark_idle()?;
    println!("Completed second task: {}", agent.state_code());

    // Show state history
    println!("\nState transitions ({}):", agent.state_history().len());
    for (i, transition) in agent.state_history().iter().enumerate() {
        println!(
            "  {} {} → {} (trigger: {})",
            i + 1,
            transition.from.code(),
            transition.to.code(),
            transition.trigger
        );
    }

    println!(
        "\nAgent uptime: {}ms, State transitions: {}",
        agent.uptime_ms(),
        agent.state_history().len()
    );

    Ok(())
}

fn example_task_management() -> Result<()> {
    let mut task1 = Task::new("GenerateReport", TaskPriority::High)
        .with_description("Generate quarterly report");

    let mut task2 = Task::new("SendNotifications", TaskPriority::Normal)
        .with_description("Send notifications to stakeholders");

    // Task1 must complete before task2
    task2.add_dependency(&task1.id);

    println!("Task 1: {} [{}]", task1.name, task1.priority_code());
    println!("  Status: {}", task1.status_code());
    println!(
        "  ID: {} (depends_on: {})",
        task1.id,
        if task1.depends_on.is_empty() {
            "none".to_string()
        } else {
            format!("{} tasks", task1.depends_on.len())
        }
    );

    println!("\nTask 2: {} [{}]", task2.name, task2.priority_code());
    println!("  Status: {}", task2.status_code());
    println!(
        "  Depends on: {}",
        if task2.depends_on.is_empty() {
            "nothing".to_string()
        } else {
            "Task1".to_string()
        }
    );

    // Execute Task 1
    task1.mark_in_progress("agent-1")?;
    println!("\nTask 1 assigned to agent-1, status: {}", task1.status_code());

    // Task 2 cannot run yet (dependency not satisfied)
    let mut completed = std::collections::HashSet::new();
    println!(
        "Task 2 ready to run: {}",
        task2.dependencies_satisfied(&completed)
    );

    task1.mark_completed()?;
    completed.insert(task1.id.clone());
    println!("Task 1 completed");
    println!(
        "Task 2 ready to run: {}",
        task2.dependencies_satisfied(&completed)
    );

    // Add artifact to task
    let artifact_id = task1.create_artifact(
        "report.pdf",
        "document",
        serde_json::json!({
            "title": "Q4 Report",
            "pages": 42
        }),
    );
    println!("\nArtifact created: {} ({})", artifact_id, "report.pdf");

    if let Some(exec_time) = task1.execution_time_ms() {
        println!("Task 1 execution time: {}ms", exec_time);
    }

    Ok(())
}

fn example_message_routing() -> Result<()> {
    let mut router = MessageRouter::new();

    // Register agents
    router.register_agent("coordinator");
    router.register_agent("worker-1");
    router.register_agent("worker-2");

    println!("Registered 3 agents: coordinator, worker-1, worker-2");

    // Send direct message
    let msg1 = Message::new(
        MessageType::TaskRequest,
        "coordinator",
        Some("worker-1".to_string()),
        serde_json::json!({
            "task_id": "task-001",
            "description": "Process data"
        }),
    );
    router.send(msg1.clone())?;
    println!("\nDirect message: coordinator → worker-1 ({})", msg1.message_type.code());

    // Send broadcast message
    let msg2 = Message::new(
        MessageType::StateTransition,
        "coordinator",
        None,
        serde_json::json!({
            "old_state": "initializing",
            "new_state": "ready"
        }),
    );
    router.send(msg2.clone())?;
    println!("Broadcast message: coordinator → all ({})", msg2.message_type.code());

    // Receive messages
    println!("\nQueue status:");
    println!("  worker-1: {} messages", router.queue_len("worker-1")?);
    println!("  worker-2: {} messages", router.queue_len("worker-2")?);

    // Process message on worker-1
    if let Some(msg) = router.receive("worker-1")? {
        println!(
            "\nworker-1 received: {} from {}",
            msg.message_type.code(),
            msg.source
        );
    }

    // Get router statistics
    let stats = router.stats();
    println!(
        "\nRouter stats: {} total messages, avg latency: {}ms, {} pending",
        stats.total_messages, stats.avg_latency_ms, stats.pending_messages
    );

    // Message ordering test
    println!("\nMessage ordering test:");
    for i in 1..=5 {
        let msg = Message::new(
            MessageType::TaskResult,
            "worker-2",
            Some("coordinator".to_string()),
            serde_json::json!({"seq": i}),
        );
        router.send(msg)?;
    }

    println!("Sent 5 messages in sequence, coordinator queue:");
    for _ in 0..5 {
        if let Some(msg) = router.receive("coordinator")? {
            let seq = msg.payload.get("seq").unwrap();
            println!("  Received seq: {}", seq);
        }
    }

    Ok(())
}

fn example_agent_bridging() -> Result<()> {
    println!("Creating agent bridge...");

    let mut bridge = AgentBridge::new("agent-001", "DataProcessorAgent");

    // Register tools
    let get_state_tool = Tool {
        name: "get_state".to_string(),
        description: "Get current agent state".to_string(),
        input_schema: serde_json::json!({
            "type": "object",
            "properties": {},
            "required": []
        }),
        output_schema: serde_json::json!({
            "type": "object",
            "properties": {
                "agent_id": { "type": "string" },
                "state": { "type": "string" }
            }
        }),
    };

    let execute_task_tool = Tool {
        name: "execute_task".to_string(),
        description: "Execute a task on the agent".to_string(),
        input_schema: serde_json::json!({
            "type": "object",
            "properties": {
                "task_id": { "type": "string" }
            },
            "required": ["task_id"]
        }),
        output_schema: serde_json::json!({
            "type": "object",
            "properties": {
                "status": { "type": "string" },
                "result": { "type": "string" }
            }
        }),
    };

    bridge.register_tool(get_state_tool)?;
    bridge.register_tool(execute_task_tool)?;

    println!("Registered {} tools", bridge.tool_count());

    // Execute tools
    println!("\nExecuting tools:");

    let exec1 = bridge.execute_tool(
        "get_state",
        serde_json::json!({}),
    )?;
    println!("  Tool: {} → success: {}", exec1.tool_name, exec1.success);

    let exec2 = bridge.execute_tool_safe(
        "execute_task",
        serde_json::json!({
            "task_id": "task-123"
        }),
    );
    println!("  Tool: {} → success: {}", exec2.tool_name, exec2.success);

    let exec3 = bridge.execute_tool_safe(
        "execute_task",
        serde_json::json!({}), // Missing required field
    );
    println!(
        "  Tool: {} → success: {} (expected failure)",
        exec3.tool_name, exec3.success
    );

    println!(
        "\nBridge statistics: {} executions, success rate: {:.1}%",
        bridge.execution_count(),
        bridge.success_rate() * 100.0
    );

    Ok(())
}

// Helper trait to get priority code
trait TaskExt {
    fn priority_code(&self) -> &'static str;
    fn status_code(&self) -> &'static str;
}

impl TaskExt for Task {
    fn priority_code(&self) -> &'static str {
        match self.priority {
            TaskPriority::Low => "LOW",
            TaskPriority::Normal => "NORMAL",
            TaskPriority::High => "HIGH",
            TaskPriority::Critical => "CRITICAL",
        }
    }

    fn status_code(&self) -> &'static str {
        self.status().code()
    }
}
