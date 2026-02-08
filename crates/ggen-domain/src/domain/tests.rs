//! Domain configuration tests

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_domain_config_builder() {
        let config = DomainConfigBuilder::new()
            .with_environment("test")
            .with_max_concurrency(8)
            .with_agent_id("test-agent")
            .with_agent_type("workflow")
            .with_max_tasks(5)
            .build();

        assert_eq!(config.environment.name, "test");
        assert_eq!(config.environment.max_concurrency, 8);
        assert_eq!(config.a2a.agent.id, "test-agent");
        assert_eq!(config.a2a.agent.agent_type, "workflow");
        assert_eq!(config.a2a.agent.max_concurrent_tasks, 5);
    }

    #[test]
    fn test_default_domain_config() {
        let config = DomainConfig::default();
        assert_eq!(config.environment.name, "development");
        assert_eq!(config.environment.max_concurrency, 4);
    }

    #[test]
    fn test_domain_config_validation() {
        let config = DomainConfig::default();
        let validation = config.validate();
        assert!(validation.valid);
        assert_eq!(validation.score, 100.0);
    }

    #[test]
    fn test_agent_creation() {
        let mut agent = Agent::new("agent-001", "Test Agent", AgentType::Workflow);
        agent.add_capability("data-processing");
        agent.update_status(AgentStatus::Running);
        agent.update_health_score(95.5);

        assert_eq!(agent.id, "agent-001");
        assert_eq!(agent.name, "Test Agent");
        assert_eq!(agent.agent_type, AgentType::Workflow);
        assert_eq!(agent.status, AgentStatus::Running);
        assert_eq!(agent.health_score, 95.5);
        assert!(agent.capabilities.contains(&"data-processing".to_string()));
        assert!(agent.can_accept_tasks());
    }

    #[test]
    fn test_task_creation() {
        let mut task = Task::new("task-001", "Test Task", "data-processing");
        task.set_executor("agent-001");
        task.update_progress(0.5);
        task.add_dependency("task-002");

        assert_eq!(task.id, "task-001");
        assert_eq!(task.name, "Test Task");
        assert_eq!(task.task_type, "data-processing");
        assert_eq!(task.progress, 0.5);
        assert_eq!(task.executor, Some("agent-001".to_string()));
        assert!(task.dependencies.contains(&"task-002".to_string()));
        assert!(!task.is_ready());
    }

    #[test]
    fn test_message_creation() {
        let mut message = Message::new(
            "msg-001",
            MessageType::TaskUpdate,
            "agent-001",
            "agent-002",
            "Task Update",
            "Task progress updated",
        );
        message.add_header("priority", "high");
        message.set_payload(serde_json::json!({"progress": 50}).as_object().unwrap().clone());

        assert_eq!(message.id, "msg-001");
        assert_eq!(message.message_type, MessageType::TaskUpdate);
        assert_eq!(message.from, "agent-001");
        assert_eq!(message.to, "agent-002");
        assert_eq!(message.subject, "Task Update");
        assert_eq!(message.body, "Task progress updated");
        assert_eq!(message.headers.get("priority"), Some(&"high".to_string()));
        assert!(message.payload.is_some());
    }

    #[test]
    fn test_event_creation() {
        let mut event = Event::new("evt-001", EventType::TaskStarted, "agent-001");
        event.set_data(serde_json::json!({
            "task_id": "task-001",
            "timestamp": "2026-02-05T10:00:00Z"
        }).as_object().unwrap().clone());

        assert_eq!(event.id, "evt-001");
        assert_eq!(event.event_type, EventType::TaskStarted);
        assert_eq!(event.source, "agent-001");
        assert!(event.data.is_some());
    }

    #[test]
    fn test_error_creation() {
        let error = Error::new(
            "error-001",
            "TaskTimeoutError",
            "Task execution exceeded timeout limit",
            "TIMEOUT",
            ErrorSeverity::Error,
        );
        error.set_related_task("task-001");

        assert_eq!(error.id, "error-001");
        assert_eq!(error.name, "TaskTimeoutError");
        assert_eq!(error.description, "Task execution exceeded timeout limit");
        assert_eq!(error.code, "TIMEOUT");
        assert_eq!(error.severity, ErrorSeverity::Error);
        assert_eq!(error.related_task, Some("task-001".to_string()));
    }
}