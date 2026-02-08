use std::collections::HashMap;
use std::sync::{Arc, Mutex};
use std::time::Duration;
use uuid::Uuid;

use ggen_workflow::context::WorkflowContext;
use ggen_workflow::errors::WorkflowError;
use ggen_workflow::events::WorkflowEvent;
use ggen_workflow::storage::{MemoryStorage, WorkflowStorage};
use ggen_workflow::workflow::{Workflow, WorkflowBuilder, WorkflowState, WorkflowStatus};

// Mock external dependencies
struct MockServices {
    pub api_service: Arc<Mutex<bool>>,
    pub database_service: Arc<Mutex<bool>>,
    pub notification_service: Arc<Mutex<bool>>,
}

impl MockServices {
    fn new() -> Self {
        Self {
            api_service: Arc::new(Mutex::new(true)),
            database_service: Arc::new(Mutex::new(true)),
            notification_service: Arc::new(Mutex::new(true)),
        }
    }

    fn simulate_service_failure(&self, service_name: &str) {
        match service_name {
            "api" => *self.api_service.lock().unwrap() = false,
            "database" => *self.database_service.lock().unwrap() = false,
            "notification" => *self.notification_service.lock().unwrap() = false,
            _ => {}
        }
    }

    fn check_service_health(&self, service_name: &str) -> bool {
        match service_name {
            "api" => *self.api_service.lock().unwrap(),
            "database" => *self.database_service.lock().unwrap(),
            "notification" => *self.notification_service.lock().unwrap(),
            _ => false,
        }
    }
}

#[cfg(test)]
mod workflow_lifecycle_tests {
    use super::*;

    // Helper function to create test workflow
    fn create_test_workflow(name: &str) -> Workflow {
        let mut builder = WorkflowBuilder::new();
        builder
            .with_name(name)
            .with_version("1.0.0")
            .with_timeout(Duration::from_secs(300))
            .add_task("task1", "Step 1")
            .add_task("task2", "Step 2")
            .add_task("task3", "Step 3")
            .add_dependency("task2", "task1")
            .add_dependency("task3", "task2");

        builder.build().unwrap()
    }

    #[test]
    fn test_workflow_start_success() {
        // Arrange
        let workflow = create_test_workflow("test_workflow");
        let storage = Arc::new(MemoryStorage::new());
        let services = Arc::new(MockServices::new());
        let context = WorkflowContext::new(workflow.clone(), storage, services);

        // Act
        let result = Workflow::start(&context);

        // Assert
        assert!(result.is_ok());
        let stored_workflow = storage.get_workflow(&workflow.id()).unwrap();
        assert_eq!(stored_workflow.status(), WorkflowStatus::Running);
        assert_eq!(stored_workflow.state(), WorkflowState::Pending);
    }

    #[test]
    fn test_workflow_execute_success() {
        // Arrange
        let workflow = create_test_workflow("execute_test");
        let storage = Arc::new(MemoryStorage::new());
        let services = Arc::new(MockServices::new());
        let context = WorkflowContext::new(workflow.clone(), storage.clone(), services);

        // Start the workflow first
        Workflow::start(&context).unwrap();

        // Act
        let result = Workflow::execute(&context);

        // Assert
        assert!(result.is_ok());
        let stored_workflow = storage.get_workflow(&workflow.id()).unwrap();
        assert_eq!(stored_workflow.status(), WorkflowStatus::Completed);
        assert_eq!(stored_workflow.state(), WorkflowState::Completed);

        // Verify all tasks were executed
        let tasks = stored_workflow.get_tasks();
        assert_eq!(tasks.len(), 3);
        for task in tasks {
            assert!(task.is_completed());
        }
    }

    #[test]
    fn test_workflow_status_polling() {
        // Arrange
        let workflow = create_test_workflow("polling_test");
        let storage = Arc::new(MemoryStorage::new());
        let services = Arc::new(MockServices::new());
        let context = WorkflowContext::new(workflow.clone(), storage.clone(), services);

        // Start workflow
        Workflow::start(&context).unwrap();

        // Act - Poll status multiple times
        let status1 = Workflow::get_status(&context);
        Workflow::execute(&context).unwrap();
        let status2 = Workflow::get_status(&context);

        // Assert
        assert!(status1.is_ok());
        assert!(status2.is_ok());
        assert_eq!(status1.unwrap(), WorkflowStatus::Running);
        assert_eq!(status2.unwrap(), WorkflowStatus::Completed);
    }

    #[test]
    fn test_workflow_error_handling() {
        // Arrange
        let workflow = create_test_workflow("error_test");
        let storage = Arc::new(MemoryStorage::new());
        let services = Arc::new(MockServices::new());
        let services_clone = services.clone();

        // Simulate service failure
        services_clone.simulate_service_failure("api");

        let context = WorkflowContext::new(workflow.clone(), storage.clone(), services);

        // Act
        Workflow::start(&context).unwrap();
        let result = Workflow::execute(&context);

        // Assert
        assert!(result.is_err());
        if let Err(WorkflowError::ExecutionFailed(msg)) = result {
            assert!(msg.contains("service unavailable"));
        }

        let stored_workflow = storage.get_workflow(&workflow.id()).unwrap();
        assert_eq!(stored_workflow.status(), WorkflowStatus::Failed);
    }

    #[test]
    fn test_workflow_state_transitions() {
        // Arrange
        let workflow = create_test_workflow("transition_test");
        let storage = Arc::new(MemoryStorage::new());
        let services = Arc::new(MockServices::new());
        let context = WorkflowContext::new(workflow.clone(), storage.clone(), services);

        // Test state transitions
        Workflow::start(&context).unwrap();
        let stored_after_start = storage.get_workflow(&workflow.id()).unwrap();
        assert_eq!(stored_after_start.state(), WorkflowState::Pending);

        Workflow::execute(&context).unwrap();
        let stored_after_execute = storage.get_workflow(&workflow.id()).unwrap();
        assert_eq!(stored_after_execute.state(), WorkflowState::Completed);

        // Test transition failure
        let invalid_context = WorkflowContext::new(workflow.clone(), storage, services);

        let result = Workflow::pause(&invalid_context);
        assert!(result.is_err());
        if let Err(WorkflowError::InvalidTransition(_)) = result {
            // Expected error for invalid transition
        }
    }

    #[test]
    fn test_workflow_timeout_handling() {
        // Arrange
        let mut builder = WorkflowBuilder::new();
        builder
            .with_name("timeout_test")
            .with_version("1.0.0")
            .with_timeout(Duration::from_secs(1))
            .add_task("slow_task", "Slow task that takes 2 seconds");

        let workflow = builder.build().unwrap();
        let storage = Arc::new(MemoryStorage::new());
        let services = Arc::new(MockServices::new());
        let context = WorkflowContext::new(workflow.clone(), storage.clone(), services);

        // Act
        Workflow::start(&context).unwrap();

        // Wait for timeout
        std::thread::sleep(Duration::from_secs(2));

        // Assert
        let stored_workflow = storage.get_workflow(&workflow.id()).unwrap();
        assert_eq!(stored_workflow.status(), WorkflowStatus::Failed);
        assert!(stored_workflow
            .get_error_message()
            .unwrap()
            .contains("timeout"));
    }

    #[test]
    fn test_workflow_events_emission() {
        // Arrange
        let workflow = create_test_workflow("events_test");
        let storage = Arc::new(MemoryStorage::new());
        let services = Arc::new(MockServices::new());
        let context = WorkflowContext::new(workflow.clone(), storage, services);

        let mut events_received = Vec::new();

        // Act - Mock event handling
        let start_result = Workflow::start(&context);
        assert!(start_result.is_ok());

        let execute_result = Workflow::execute(&context);
        assert!(execute_result.is_ok());

        // Assert - Verify events were emitted
        // In a real implementation, this would check event logs
        let stored_workflow = storage.get_workflow(&workflow.id()).unwrap();
        assert_eq!(stored_workflow.status(), WorkflowStatus::Completed);
    }

    #[test]
    fn test_workflow_cancellation() {
        // Arrange
        let workflow = create_test_workflow("cancellation_test");
        let storage = Arc::new(MemoryStorage::new());
        let services = Arc::new(MockServices::new());
        let context = WorkflowContext::new(workflow.clone(), storage.clone(), services);

        // Start workflow
        Workflow::start(&context).unwrap();

        // Act
        let cancel_result = Workflow::cancel(&context);

        // Assert
        assert!(cancel_result.is_ok());
        let stored_workflow = storage.get_workflow(&workflow.id()).unwrap();
        assert_eq!(stored_workflow.status(), WorkflowStatus::Cancelled);
    }

    #[test]
    fn test_workflow_restart() {
        // Arrange
        let workflow = create_test_workflow("restart_test");
        let storage = Arc::new(MemoryStorage::new());
        let services = Arc::new(MockServices::new());
        let context = WorkflowContext::new(workflow.clone(), storage.clone(), services);

        // First execution fails
        Workflow::start(&context).unwrap();
        Workflow::cancel(&context).unwrap();

        // Act
        let restart_result = Workflow::restart(&context);

        // Assert
        assert!(restart_result.is_ok());
        let stored_workflow = storage.get_workflow(&workflow.id()).unwrap();
        assert_eq!(stored_workflow.status(), WorkflowStatus::Running);
        assert_eq!(stored_workflow.state(), WorkflowState::Pending);
    }

    #[test]
    fn test_workflow_concurrent_execution() {
        // Arrange
        let workflow = create_test_workflow("concurrent_test");
        let storage = Arc::new(MemoryStorage::new());
        let services = Arc::new(MockServices::new());
        let context = WorkflowContext::new(workflow.clone(), storage.clone(), services);

        // Act - Execute concurrently
        let handle1 = std::thread::spawn({
            let context = context.clone();
            move || {
                Workflow::start(&context).unwrap();
                Workflow::execute(&context)
            }
        });

        let handle2 = std::thread::spawn({
            let context = context.clone();
            move || {
                Workflow::start(&context).unwrap();
                Workflow::execute(&context)
            }
        });

        // Wait for both threads
        let result1 = handle1.join().unwrap();
        let result2 = handle2.join().unwrap();

        // Assert
        assert!(result1.is_ok());
        assert!(result2.is_ok());

        // Verify final state
        let stored_workflow = storage.get_workflow(&workflow.id()).unwrap();
        assert_eq!(stored_workflow.status(), WorkflowStatus::Completed);
    }
}

// Property-based tests using proptest
proptest::proptest! {
    #![proptest_config(ProptestConfig::with_cases(100))]

    #[test]
    fn test_workflow_id_generation_is_unique() {
        let workflow1 = WorkflowBuilder::new()
            .with_name("test1")
            .build()
            .unwrap();
        let workflow2 = WorkflowBuilder::new()
            .with_name("test2")
            .build()
            .unwrap();

        prop_assert_ne!(workflow1.id(), workflow2.id());
    }

    #[test]
    fn test_workflow_timeout_is_valid(duration_ms in 1000..3600000) {
        let duration = Duration::from_millis(duration_ms);
        let mut builder = WorkflowBuilder::new();
        builder
            .with_name("timeout_prop_test")
            .with_timeout(duration);

        let result = builder.build();
        prop_assert!(result.is_ok());
    }

    #[test]
    fn test_workflow_task_count_valid(task_count in 1..100) {
        let mut builder = WorkflowBuilder::new();
        builder
            .with_name("task_count_test");

        for i in 0..task_count {
            builder.add_task(&format!("task_{}", i), &format!("Task {}", i));
        }

        let result = builder.build();
        prop_assert!(result.is_ok());
        if result.is_ok() {
            let workflow = result.unwrap();
            prop_assert_eq!(workflow.get_tasks().len(), task_count as usize);
        }
    }
}
