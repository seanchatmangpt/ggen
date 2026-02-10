//! A2A Task State Machine Example
//!
//! Demonstrates task lifecycle through state machine with guards.
//! Only valid transitions are allowed. Terminal states are final.

use ggen_a2a::{
    state_machine::{StateTransition, TaskStateMachine},
    Artifact, ArtifactType, Task, TaskState,
};

fn main() -> Result<(), Box<dyn std::error::Error>> {
    println!("=== A2A Task State Machine Example ===\n");

    // Create task
    let mut task = Task::new(
        "Implement authentication system".to_string(),
        "product-owner".to_string(),
    )
    .with_description("Add OAuth2 and JWT authentication to API".to_string())
    .with_metadata("priority".to_string(), "high".to_string())
    .with_metadata("sprint".to_string(), "Sprint-23".to_string());

    println!("Created task:");
    println!("  ID: {}", task.id);
    println!("  Title: {}", task.title);
    println!("  Created by: {}", task.created_by);
    println!("  Initial state: {:?}\n", task.state);

    // Show possible transitions
    let possible = TaskStateMachine::possible_transitions(task.state);
    println!("Possible transitions from Created:");
    for state in &possible {
        println!("  → {:?}", state);
    }
    println!();

    // Attempt invalid transition (should fail)
    println!("Attempting invalid transition (Created → Completed):");
    let invalid_transition = StateTransition::new(
        TaskState::Completed,
        "product-owner".to_string(),
    );
    match TaskStateMachine::transition(&mut task, invalid_transition) {
        Ok(_) => println!("  ✗ Unexpectedly succeeded"),
        Err(e) => println!("  ✓ Correctly rejected: {}", e),
    }
    println!();

    // Attempt transition without assignment (should fail)
    println!("Attempting transition to Running without assignment:");
    let no_assign_transition = StateTransition::new(
        TaskState::Running,
        "developer-1".to_string(),
    );
    match TaskStateMachine::transition(&mut task, no_assign_transition) {
        Ok(_) => println!("  ✗ Unexpectedly succeeded"),
        Err(e) => println!("  ✓ Correctly rejected: {}", e),
    }
    println!();

    // Assign task
    println!("Assigning task to developer-1");
    task.assigned_to = Some("developer-1".to_string());
    println!("  Assigned to: {}\n", task.assigned_to.as_ref().unwrap());

    // Valid transition: Created → Running
    println!("Transition: Created → Running");
    let start_transition = StateTransition::new(
        TaskState::Running,
        "developer-1".to_string(),
    );
    TaskStateMachine::transition(&mut task, start_transition)?;
    println!("  ✓ State: {:?}", task.state);
    println!("  Updated at: {}\n", task.updated_at);

    // Add input artifacts
    println!("Adding input artifacts:");
    let spec_artifact = Artifact::text(
        "requirements.md".to_string(),
        ArtifactType::Input,
        "OAuth2 and JWT implementation requirements".to_string(),
    )
    .with_metadata("version".to_string(), "1.0".to_string());

    task.artifacts.insert("spec".to_string(), spec_artifact);
    println!("  + requirements.md (Input)\n");

    // Simulate blocking
    println!("Transition: Running → Blocked (waiting for API keys)");
    let block_transition = StateTransition::new(
        TaskState::Blocked,
        "developer-1".to_string(),
    )
    .with_reason("Waiting for production API keys from DevOps".to_string());

    TaskStateMachine::transition(&mut task, block_transition)?;
    println!("  ✓ State: {:?}", task.state);
    println!("  Reason: Waiting for production API keys\n");

    // Unblock
    println!("Transition: Blocked → Running (API keys received)");
    let unblock_transition = StateTransition::new(
        TaskState::Running,
        "developer-1".to_string(),
    );
    TaskStateMachine::transition(&mut task, unblock_transition)?;
    println!("  ✓ State: {:?}\n", task.state);

    // Add output artifacts
    println!("Adding output artifacts:");
    let code_artifact = Artifact::file(
        "auth.rs".to_string(),
        ArtifactType::Output,
        std::path::PathBuf::from("src/auth/auth.rs"),
    )
    .with_metadata("lines".to_string(), "342".to_string())
    .with_hash("abc123def456".to_string());

    task.artifacts.insert("code".to_string(), code_artifact);
    println!("  + auth.rs (Output, 342 lines)\n");

    // Complete task
    println!("Transition: Running → Completed");
    let complete_transition = StateTransition::new(
        TaskState::Completed,
        "developer-1".to_string(),
    );
    TaskStateMachine::transition(&mut task, complete_transition)?;
    println!("  ✓ State: {:?}", task.state);
    println!("  ✓ Terminal state reached");
    println!("  Completed at: {:?}", task.completed_at);
    if let Some(duration) = task.duration() {
        println!("  Duration: {:?}\n", duration);
    }

    // Attempt transition from terminal state (should fail)
    println!("Attempting transition from terminal state:");
    let invalid_from_terminal = StateTransition::new(
        TaskState::Running,
        "developer-1".to_string(),
    );
    match TaskStateMachine::transition(&mut task, invalid_from_terminal) {
        Ok(_) => println!("  ✗ Unexpectedly succeeded"),
        Err(e) => println!("  ✓ Correctly rejected: {}", e),
    }
    println!();

    // Task summary
    println!("=== Task Summary ===");
    println!("ID: {}", task.id);
    println!("State: {:?}", task.state);
    println!("Terminal: {}", task.is_terminal());
    println!("Artifacts: {}", task.artifacts.len());
    for (name, artifact) in &task.artifacts {
        println!("  - {}: {:?}", name, artifact.artifact_type);
    }
    println!("Metadata:");
    for (key, value) in &task.metadata {
        println!("  - {}: {}", key, value);
    }

    println!("\n✓ A2A task state machine example completed");
    println!("Key insight: State machine enforces valid transitions and guards");

    Ok(())
}
