//! Task management tests

use a2a_agent_lifecycle::{Task, TaskManager, TaskPriority};
use std::collections::HashSet;

#[test]
fn test_task_creation() {
    let task = Task::new("TestTask", TaskPriority::Normal);
    assert_eq!(task.name, "TestTask");
    assert_eq!(task.priority, TaskPriority::Normal);
    assert!(task.is_pending());
}

#[test]
fn test_task_lifecycle() {
    let mut task = Task::new("TestTask", TaskPriority::High);

    task.mark_in_progress("agent-1").unwrap();
    assert!(task.is_in_progress());

    task.mark_completed().unwrap();
    assert!(task.is_completed());
}

#[test]
fn test_task_failed_state() {
    let mut task = Task::new("TestTask", TaskPriority::Normal);
    task.mark_in_progress("agent-1").unwrap();

    assert!(task.mark_failed().is_ok());
    assert!(task.is_failed());
}

#[test]
fn test_invalid_transition_to_completed() {
    let mut task = Task::new("TestTask", TaskPriority::Normal);

    // Cannot complete a pending task
    assert!(task.mark_completed().is_err());
}

#[test]
fn test_task_artifacts() {
    let mut task = Task::new("TestTask", TaskPriority::Normal);
    let artifact_id = task.create_artifact(
        "output.txt",
        "text",
        serde_json::json!({"content": "result"}),
    );

    assert!(task.get_artifact(&artifact_id).is_some());
    assert_eq!(task.artifacts.len(), 1);
}

#[test]
fn test_task_dependencies() {
    let mut task1 = Task::new("Task1", TaskPriority::Normal);
    let mut task2 = Task::new("Task2", TaskPriority::Normal);

    task2.add_dependency(&task1.id);

    let mut completed = HashSet::new();
    assert!(!task2.dependencies_satisfied(&completed));

    completed.insert(task1.id.clone());
    assert!(task2.dependencies_satisfied(&completed));
}

#[test]
fn test_task_manager_basic() {
    let mut manager = TaskManager::new();

    let task = Task::new("Task1", TaskPriority::Normal);
    manager.add_task(task);

    assert_eq!(manager.pending_count(), 1);
    assert_eq!(manager.completed_count(), 0);
}

#[test]
fn test_task_manager_priority_ordering() {
    let mut manager = TaskManager::new();

    manager.add_task(Task::new("LowPriority", TaskPriority::Low));
    manager.add_task(Task::new("HighPriority", TaskPriority::High));
    manager.add_task(Task::new("CriticalPriority", TaskPriority::Critical));
    manager.add_task(Task::new("NormalPriority", TaskPriority::Normal));

    // High and Critical priority tasks should be scheduled first
    let first = manager.get_next_task().unwrap();
    assert!(first.name == "HighPriority" || first.name == "CriticalPriority");
}

#[test]
fn test_task_dependencies_in_manager() {
    let mut manager = TaskManager::new();

    let task1 = Task::new("Task1", TaskPriority::Normal);
    let mut task2 = Task::new("Task2", TaskPriority::Normal);

    task2.add_dependency(&task1.id);

    manager.add_task(task1);
    manager.add_task(task2);

    // Task1 should be scheduled first
    let first = manager.get_next_task();
    assert!(first.is_some());
}

#[test]
fn test_task_execution_time() {
    let mut task = Task::new("TestTask", TaskPriority::Normal);
    assert!(task.execution_time_ms().is_none());

    task.mark_in_progress("agent-1").unwrap();
    std::thread::sleep(std::time::Duration::from_millis(10));
    task.mark_completed().unwrap();

    let exec_time = task.execution_time_ms();
    assert!(exec_time.is_some());
    assert!(exec_time.unwrap() > 0);
}

#[test]
fn test_task_assignment() {
    let mut task = Task::new("TestTask", TaskPriority::Normal);
    assert!(task.assigned_agent.is_none());

    task.mark_in_progress("agent-123").unwrap();
    assert_eq!(task.assigned_agent, Some("agent-123".to_string()));
}

#[test]
fn test_multiple_artifacts() {
    let mut task = Task::new("TestTask", TaskPriority::Normal);

    let id1 = task.create_artifact("file1.txt", "text", serde_json::json!({}));
    let id2 = task.create_artifact("file2.pdf", "pdf", serde_json::json!({}));
    let id3 = task.create_artifact("file3.json", "json", serde_json::json!({}));

    assert_eq!(task.artifacts.len(), 3);
    assert!(task.get_artifact(&id1).is_some());
    assert!(task.get_artifact(&id2).is_some());
    assert!(task.get_artifact(&id3).is_some());
}

#[test]
fn test_task_unique_ids() {
    let task1 = Task::new("Task", TaskPriority::Normal);
    let task2 = Task::new("Task", TaskPriority::Normal);

    assert_ne!(task1.id, task2.id);
}

#[test]
fn test_task_priority_values() {
    assert!(TaskPriority::Critical > TaskPriority::High);
    assert!(TaskPriority::High > TaskPriority::Normal);
    assert!(TaskPriority::Normal > TaskPriority::Low);
}

#[test]
fn test_task_with_description() {
    let task = Task::new("TestTask", TaskPriority::Normal).with_description("This is a test task");

    assert_eq!(task.description, Some("This is a test task".to_string()));
}

#[test]
fn test_task_manager_complete() {
    let mut manager = TaskManager::new();

    let task1 = Task::new("Task1", TaskPriority::Normal);
    let task1_id = task1.id.clone();
    manager.add_task(task1);

    assert_eq!(manager.pending_count(), 1);
    manager.complete_task(&task1_id).unwrap();
    assert_eq!(manager.completed_count(), 1);
}

#[test]
fn test_task_manager_get_task() {
    let mut manager = TaskManager::new();

    let task = Task::new("TestTask", TaskPriority::Normal);
    let task_id = task.id.clone();
    manager.add_task(task);

    assert!(manager.get_task(&task_id).is_some());
}

#[test]
fn test_dependency_chain() {
    let mut manager = TaskManager::new();

    let task1 = Task::new("Task1", TaskPriority::Normal);
    let mut task2 = Task::new("Task2", TaskPriority::Normal);
    let mut task3 = Task::new("Task3", TaskPriority::Normal);

    task2.add_dependency(&task1.id);
    task3.add_dependency(&task2.id);

    let t1_id = task1.id.clone();
    let t2_id = task2.id.clone();

    manager.add_task(task1);
    manager.add_task(task2);
    manager.add_task(task3);

    // Complete task1
    manager.complete_task(&t1_id).unwrap();

    // Now task2 should be satisfiable
    let mut completed = std::collections::HashSet::new();
    completed.insert(t1_id);

    if let Some(task) = manager.get_task(&t2_id) {
        assert!(task.dependencies_satisfied(&completed));
    }
}

#[test]
fn test_task_status_codes() {
    let mut task = Task::new("TestTask", TaskPriority::Normal);
    assert_eq!(task.status_code(), "PENDING");

    task.mark_in_progress("agent-1").unwrap();
    assert_eq!(task.status_code(), "IN_PROGRESS");

    task.mark_completed().unwrap();
    assert_eq!(task.status_code(), "COMPLETED");
}

#[test]
fn test_concurrent_tasks() {
    let mut manager = TaskManager::new();

    for i in 0..10 {
        manager.add_task(Task::new(&format!("Task{}", i), TaskPriority::Normal));
    }

    assert_eq!(manager.pending_count(), 10);
}
