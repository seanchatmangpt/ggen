#[cfg(test)]
mod tests {
    use osiris_autonomic::{
        uuid::Uuid, AutonomicRefusalSystem, Priority, RefusalContext, RefusalType, RiskLevel,
        Urgency,
    };

    #[tokio::test]
    async fn test_autonomic_system_creation() {
        let system = AutonomicRefusalSystem::new();
        assert!(system.is_ok());
    }

    #[tokio::test]
    async fn test_autonomic_system_start_stop() {
        let system = AutonomicRefusalSystem::new().unwrap();

        // Start the system
        let result = system.start().await;
        assert!(result.is_ok());

        // Stop the system
        let result = system.stop().await;
        assert!(result.is_ok());
    }

    #[tokio::test]
    async fn test_process_request_without_running_system() {
        let system = AutonomicRefusalSystem::new().unwrap();

        let context = create_test_context();
        let result = system.process_request(context).await;
        // Should fail because system is not running
        assert!(result.is_err());
    }

    #[tokio::test]
    async fn test_process_request_with_running_system() {
        let system = AutonomicRefusalSystem::new().unwrap();
        system.start().await.unwrap();

        let context = create_test_context();
        let result = system.process_request(context).await;

        // Should succeed
        assert!(result.is_ok());

        let decision = result.unwrap();
        // Default decision should be Accept
        assert!(matches!(decision.refusal_type, RefusalType::Accept));

        system.stop().await.unwrap();
    }

    fn create_test_context() -> RefusalContext {
        RefusalContext {
            request_id: Uuid::new_v4(),
            request_type: "test-request".to_string(),
            requested_by: "test-user".to_string(),
            timestamp: chrono::Utc::now(),
            priority: Priority::Normal,
            urgency: Urgency::Routine,
            risk_assessment: RiskLevel::Minimal,
            user_context: std::collections::HashMap::new(),
            system_state: std::collections::HashMap::new(),
        }
    }
}
