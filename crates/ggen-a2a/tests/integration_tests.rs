//! Integration tests for A2A task state machine protocol

use ggen_a2a::{
    Artifact, ArtifactType, StateTransition, Task, TaskMessage, TaskState, TaskStateMachine,
    Transport,
};

#[tokio::test]
async fn test_full_task_lifecycle() {
    // Arrange: Create task
    let mut task = Task::new("Build feature".to_string(), "agent-1".to_string())
        .with_assignment("agent-2".to_string())
        .with_description("Implement new feature".to_string());

    assert_eq!(task.state, TaskState::Created);

    // Act & Assert: Transition to Running
    let transition = StateTransition::new(TaskState::Running, "agent-2".to_string());
    TaskStateMachine::transition(&mut task, transition).unwrap();
    assert_eq!(task.state, TaskState::Running);

    // Act & Assert: Transition to Completed
    let transition = StateTransition::new(TaskState::Completed, "agent-2".to_string());
    TaskStateMachine::transition(&mut task, transition).unwrap();
    assert_eq!(task.state, TaskState::Completed);
    assert!(task.completed_at.is_some());
    assert!(task.duration().is_some());
}

#[tokio::test]
async fn test_task_blocking_and_recovery() {
    // Arrange: Create running task
    let mut task = Task::new("Blocked task".to_string(), "agent-1".to_string())
        .with_assignment("agent-1".to_string());

    let transition = StateTransition::new(TaskState::Running, "agent-1".to_string());
    TaskStateMachine::transition(&mut task, transition).unwrap();

    // Act: Block the task
    let transition = StateTransition::new(TaskState::Blocked, "agent-1".to_string())
        .with_reason("Waiting for dependency".to_string());
    TaskStateMachine::transition(&mut task, transition).unwrap();
    assert_eq!(task.state, TaskState::Blocked);

    // Act: Unblock and continue
    let transition = StateTransition::new(TaskState::Running, "agent-1".to_string());
    TaskStateMachine::transition(&mut task, transition).unwrap();
    assert_eq!(task.state, TaskState::Running);

    // Act: Complete
    let transition = StateTransition::new(TaskState::Completed, "agent-1".to_string());
    TaskStateMachine::transition(&mut task, transition).unwrap();
    assert_eq!(task.state, TaskState::Completed);
}

#[tokio::test]
async fn test_task_failure_with_reason() {
    // Arrange
    let mut task = Task::new("Failing task".to_string(), "agent-1".to_string())
        .with_assignment("agent-1".to_string());

    let transition = StateTransition::new(TaskState::Running, "agent-1".to_string());
    TaskStateMachine::transition(&mut task, transition).unwrap();

    // Act: Fail the task
    let transition = StateTransition::new(TaskState::Failed, "agent-1".to_string())
        .with_reason("Connection timeout".to_string());
    TaskStateMachine::transition(&mut task, transition).unwrap();

    // Assert
    assert_eq!(task.state, TaskState::Failed);
    assert_eq!(
        task.failure_reason,
        Some("Connection timeout".to_string())
    );
    assert!(task.completed_at.is_some());
}

#[tokio::test]
async fn test_transport_agent_communication() {
    // Arrange
    let transport = Transport::new();
    let mut rx1 = transport
        .register_agent("agent-1".to_string())
        .await
        .unwrap();
    let mut rx2 = transport
        .register_agent("agent-2".to_string())
        .await
        .unwrap();

    // Act: Agent 1 creates a task and sends to Agent 2
    let task = Task::new("Test task".to_string(), "agent-1".to_string())
        .with_assignment("agent-2".to_string());
    let task_id = task.id;

    transport.store_task(task.clone()).await.unwrap();

    let message = TaskMessage::Assign {
        task_id,
        agent_id: "agent-2".to_string(),
    };
    transport
        .send("agent-1".to_string(), "agent-2".to_string(), message)
        .await
        .unwrap();

    // Assert: Agent 2 receives the assignment
    let envelope = rx2.recv().await.unwrap();
    assert_eq!(envelope.from, "agent-1");
    assert!(matches!(envelope.message, TaskMessage::Assign { .. }));

    // Act: Agent 2 starts the task and notifies
    let mut task = transport.get_task(task_id).await.unwrap();
    let transition = StateTransition::new(TaskState::Running, "agent-2".to_string());
    TaskStateMachine::transition(&mut task, transition).unwrap();
    transport.update_task(task.clone()).await.unwrap();

    let notification = TaskMessage::StateChanged {
        task_id,
        old_state: TaskState::Created,
        new_state: TaskState::Running,
    };
    transport
        .send("agent-2".to_string(), "agent-1".to_string(), notification)
        .await
        .unwrap();

    // Assert: Agent 1 receives the notification
    let envelope = rx1.recv().await.unwrap();
    assert!(matches!(
        envelope.message,
        TaskMessage::StateChanged { .. }
    ));
}

#[tokio::test]
async fn test_task_with_artifacts() {
    // Arrange: Create task with input artifact
    let input_artifact = Artifact::text(
        "input.txt".to_string(),
        ArtifactType::Input,
        "Input data".to_string(),
    );

    let mut task = Task::new("Process data".to_string(), "agent-1".to_string())
        .with_assignment("agent-1".to_string())
        .with_artifact("input".to_string(), input_artifact);

    // Act: Run task and add output artifact
    let transition = StateTransition::new(TaskState::Running, "agent-1".to_string());
    TaskStateMachine::transition(&mut task, transition).unwrap();

    let output_artifact = Artifact::text(
        "output.txt".to_string(),
        ArtifactType::Output,
        "Processed data".to_string(),
    );
    task.artifacts.insert("output".to_string(), output_artifact);

    // Act: Complete task
    let transition = StateTransition::new(TaskState::Completed, "agent-1".to_string());
    TaskStateMachine::transition(&mut task, transition).unwrap();

    // Assert
    assert_eq!(task.state, TaskState::Completed);
    assert_eq!(task.artifacts.len(), 2);
    assert!(task.artifacts.contains_key("input"));
    assert!(task.artifacts.contains_key("output"));
}

#[tokio::test]
async fn test_broadcast_state_change() {
    // Arrange: Multiple agents listening
    let transport = Transport::new();
    let mut rx1 = transport
        .register_agent("agent-1".to_string())
        .await
        .unwrap();
    let mut rx2 = transport
        .register_agent("agent-2".to_string())
        .await
        .unwrap();
    let mut rx3 = transport
        .register_agent("agent-3".to_string())
        .await
        .unwrap();

    // Act: Agent 1 broadcasts completion
    let task_id = uuid::Uuid::new_v4();
    let message = TaskMessage::Completed {
        task_id,
        result: "Success".to_string(),
    };
    transport
        .broadcast("agent-1".to_string(), message)
        .await
        .unwrap();

    // Assert: All other agents receive the broadcast
    let envelope2 = rx2.recv().await.unwrap();
    let envelope3 = rx3.recv().await.unwrap();

    assert!(matches!(envelope2.message, TaskMessage::Completed { .. }));
    assert!(matches!(envelope3.message, TaskMessage::Completed { .. }));

    // Agent 1 should not receive own broadcast
    assert!(rx1.try_recv().is_err());
}

#[tokio::test]
async fn test_query_tasks_by_state() {
    // Arrange: Create tasks in different states
    let transport = Transport::new();

    let task1 = Task::new("Task 1".to_string(), "agent-1".to_string());
    let mut task2 = Task::new("Task 2".to_string(), "agent-1".to_string())
        .with_assignment("agent-1".to_string());
    let mut task3 = Task::new("Task 3".to_string(), "agent-1".to_string())
        .with_assignment("agent-1".to_string());

    // Set different states
    let transition = StateTransition::new(TaskState::Running, "agent-1".to_string());
    TaskStateMachine::transition(&mut task2, transition).unwrap();

    let transition = StateTransition::new(TaskState::Running, "agent-1".to_string());
    TaskStateMachine::transition(&mut task3, transition).unwrap();
    let transition = StateTransition::new(TaskState::Completed, "agent-1".to_string());
    TaskStateMachine::transition(&mut task3, transition).unwrap();

    transport.store_task(task1).await.unwrap();
    transport.store_task(task2).await.unwrap();
    transport.store_task(task3).await.unwrap();

    // Act: Query tasks by state
    let created_tasks = transport
        .list_tasks_by_state(TaskState::Created)
        .await
        .unwrap();
    let running_tasks = transport
        .list_tasks_by_state(TaskState::Running)
        .await
        .unwrap();
    let completed_tasks = transport
        .list_tasks_by_state(TaskState::Completed)
        .await
        .unwrap();

    // Assert
    assert_eq!(created_tasks.len(), 1);
    assert_eq!(running_tasks.len(), 1);
    assert_eq!(completed_tasks.len(), 1);
}

#[tokio::test]
async fn test_hierarchical_tasks() {
    // Arrange: Create parent and child tasks
    let parent = Task::new("Parent task".to_string(), "agent-1".to_string());
    let parent_id = parent.id;

    let mut child1 = Task::new("Child task 1".to_string(), "agent-1".to_string())
        .with_assignment("agent-2".to_string());
    child1.parent_id = Some(parent_id);

    let mut child2 = Task::new("Child task 2".to_string(), "agent-1".to_string())
        .with_assignment("agent-3".to_string());
    child2.parent_id = Some(parent_id);

    // Act: Complete child tasks
    let transition = StateTransition::new(TaskState::Running, "agent-2".to_string());
    TaskStateMachine::transition(&mut child1, transition).unwrap();
    let transition = StateTransition::new(TaskState::Completed, "agent-2".to_string());
    TaskStateMachine::transition(&mut child1, transition).unwrap();

    let transition = StateTransition::new(TaskState::Running, "agent-3".to_string());
    TaskStateMachine::transition(&mut child2, transition).unwrap();
    let transition = StateTransition::new(TaskState::Completed, "agent-3".to_string());
    TaskStateMachine::transition(&mut child2, transition).unwrap();

    // Assert
    assert_eq!(child1.parent_id, Some(parent_id));
    assert_eq!(child2.parent_id, Some(parent_id));
    assert_eq!(child1.state, TaskState::Completed);
    assert_eq!(child2.state, TaskState::Completed);
}

#[tokio::test]
async fn test_task_dependencies() {
    // Arrange: Create tasks with dependencies
    let task1 = Task::new("Task 1".to_string(), "agent-1".to_string());
    let task1_id = task1.id;

    let task2 = Task::new("Task 2".to_string(), "agent-1".to_string()).with_dependency(task1_id);

    // Assert: Dependencies are recorded
    assert_eq!(task2.dependencies.len(), 1);
    assert_eq!(task2.dependencies[0], task1_id);
}

#[tokio::test]
async fn test_terminal_state_immutability() {
    // Arrange: Complete a task
    let mut task = Task::new("Terminal test".to_string(), "agent-1".to_string())
        .with_assignment("agent-1".to_string());

    let transition = StateTransition::new(TaskState::Running, "agent-1".to_string());
    TaskStateMachine::transition(&mut task, transition).unwrap();

    let transition = StateTransition::new(TaskState::Completed, "agent-1".to_string());
    TaskStateMachine::transition(&mut task, transition).unwrap();

    // Act: Try to transition from terminal state
    let transition = StateTransition::new(TaskState::Running, "agent-1".to_string());
    let result = TaskStateMachine::transition(&mut task, transition);

    // Assert: Transition should fail
    assert!(result.is_err());
    assert_eq!(task.state, TaskState::Completed);
}
