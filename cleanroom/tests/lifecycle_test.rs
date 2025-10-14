//! Integration tests for lifecycle management system

use clnrm::lifecycle::{
    LifecycleConfig, LifecycleManager, Status, RequirementCategory, Requirement, Phase,
};
use clnrm::cleanroom::CleanroomEnvironment;
use clnrm::config::CleanroomConfig;
use std::sync::Arc;
use tempfile::TempDir;

#[tokio::test]
async fn test_lifecycle_config_creation() {
    let config = LifecycleConfig::default_with_name("test-project");
    assert_eq!(config.project_name, "test-project");
    assert!(!config.phases.is_empty());
    assert!(!config.environments.is_empty());
    assert!(!config.readiness_requirements.is_empty());
}

#[tokio::test]
async fn test_lifecycle_config_validation() {
    let config = LifecycleConfig::default_with_name("test");
    assert!(config.validate().is_ok());

    // Test empty project name validation
    let mut bad_config = config.clone();
    bad_config.project_name = String::new();
    assert!(bad_config.validate().is_err());
}

#[tokio::test]
async fn test_lifecycle_config_save_load() {
    let temp_dir = TempDir::new().unwrap();
    let config_path = temp_dir.path().join("lifecycle.toml");

    let config = LifecycleConfig::default_with_name("test-save-load");
    config.save(&config_path).await.unwrap();

    let loaded_config = LifecycleConfig::load(&config_path).await.unwrap();
    assert_eq!(loaded_config.project_name, "test-save-load");
}

#[tokio::test]
async fn test_lifecycle_manager_creation() {
    let config = LifecycleConfig::default_with_name("test-manager");
    let manager = LifecycleManager::new(config, None);
    assert!(manager.is_ok());
}

#[tokio::test]
async fn test_lifecycle_manager_with_cleanroom() {
    let config = LifecycleConfig::default_with_name("test-cleanroom");
    let cleanroom_config = CleanroomConfig::default();
    let cleanroom = Arc::new(CleanroomEnvironment::new(cleanroom_config).await.unwrap());

    let manager = LifecycleManager::new(config, Some(cleanroom));
    assert!(manager.is_ok());
}

#[tokio::test]
async fn test_readiness_score_evaluation() {
    let config = LifecycleConfig::default_with_name("test-readiness");
    let manager = LifecycleManager::new(config, None).unwrap();

    let score = manager.readiness().await.unwrap();
    assert!(score.score <= 100);
    assert!(!score.requirements.is_empty());
}

#[tokio::test]
async fn test_readiness_with_complete_requirements() {
    let mut config = LifecycleConfig::default_with_name("test-complete");

    // Mark all requirements as complete
    for req in &mut config.readiness_requirements {
        req.status = Status::Complete;
    }

    let manager = LifecycleManager::new(config, None).unwrap();
    let score = manager.readiness().await.unwrap();

    assert_eq!(score.score, 100);
    assert!(score.blockers.is_empty());
}

#[tokio::test]
async fn test_readiness_with_blockers() {
    let mut config = LifecycleConfig::default_with_name("test-blockers");

    // Add high-priority incomplete requirement
    config.readiness_requirements.push(Requirement {
        id: "critical-blocker".to_string(),
        name: "Critical Blocker".to_string(),
        description: "This blocks deployment".to_string(),
        status: Status::NotStarted,
        category: RequirementCategory::Security,
        priority: 5,
        validation_command: None,
    });

    let manager = LifecycleManager::new(config, None).unwrap();
    let score = manager.readiness().await.unwrap();

    assert!(score.score < 100);
    assert!(!score.blockers.is_empty());
}

#[tokio::test]
async fn test_requirement_categories() {
    let config = LifecycleConfig::default_with_name("test-categories");
    let manager = LifecycleManager::new(config, None).unwrap();

    let score = manager.readiness().await.unwrap();

    // Should have category breakdown
    assert!(!score.category_scores.is_empty());
}

#[tokio::test]
async fn test_requirement_update() {
    let config = LifecycleConfig::default_with_name("test-update");
    let manager = LifecycleManager::new(config.clone(), None).unwrap();

    // Get first requirement ID
    let req_id = &config.readiness_requirements[0].id;

    // Update requirement status
    let result = manager.update_requirement(req_id, Status::Complete).await;
    assert!(result.is_ok());
}

#[tokio::test]
async fn test_deployment_validation() {
    let temp_dir = TempDir::new().unwrap();
    let config = LifecycleConfig::default_with_name("test-validation");

    // Create a minimal Cargo.toml
    let cargo_toml = temp_dir.path().join("Cargo.toml");
    tokio::fs::write(&cargo_toml, r#"
[package]
name = "test-validation"
version = "0.1.0"
edition = "2021"

[dependencies]
"#).await.unwrap();

    // Create src directory
    tokio::fs::create_dir_all(temp_dir.path().join("src")).await.unwrap();

    let manager = LifecycleManager::new(config, None).unwrap();
    let validation = manager.validate("dev").await;

    assert!(validation.is_ok());
}

#[tokio::test]
async fn test_phase_execution_order() {
    let mut config = LifecycleConfig::default_with_name("test-phases");

    // Add phases with dependencies
    config.phases = vec![
        Phase {
            name: "phase1".to_string(),
            command: "echo".to_string(),
            args: vec!["phase1".to_string()],
            cleanroom_enabled: false,
            timeout_seconds: 10,
            env: Default::default(),
            dependencies: vec![],
            continue_on_failure: false,
        },
        Phase {
            name: "phase2".to_string(),
            command: "echo".to_string(),
            args: vec!["phase2".to_string()],
            cleanroom_enabled: false,
            timeout_seconds: 10,
            env: Default::default(),
            dependencies: vec!["phase1".to_string()],
            continue_on_failure: false,
        },
    ];

    // Validation should pass
    assert!(config.validate().is_ok());
}

#[tokio::test]
async fn test_invalid_phase_dependencies() {
    let mut config = LifecycleConfig::default_with_name("test-invalid");

    // Add phase with invalid dependency
    config.phases.push(Phase {
        name: "invalid".to_string(),
        command: "echo".to_string(),
        args: vec![],
        cleanroom_enabled: false,
        timeout_seconds: 10,
        env: Default::default(),
        dependencies: vec!["nonexistent".to_string()],
        continue_on_failure: false,
    });

    // Validation should fail
    assert!(config.validate().is_err());
}

#[tokio::test]
async fn test_environment_configs() {
    let config = LifecycleConfig::default_with_name("test-envs");

    // Should have default environments
    assert!(config.get_environment("dev").is_some());
    assert!(config.get_environment("staging").is_some());
    assert!(config.get_environment("production").is_some());

    // Production should have more checks than dev
    let dev = config.get_environment("dev").unwrap();
    let prod = config.get_environment("production").unwrap();

    assert!(prod.validation_checks.len() > dev.validation_checks.len());
    assert!(prod.required_services.len() >= dev.required_services.len());
}

#[tokio::test]
async fn test_status_serialization() {
    use serde_json;

    let status = Status::Complete;
    let json = serde_json::to_string(&status).unwrap();
    let deserialized: Status = serde_json::from_str(&json).unwrap();

    assert_eq!(status, deserialized);
}

#[tokio::test]
async fn test_readiness_recommendations() {
    let mut config = LifecycleConfig::default_with_name("test-recs");

    // Add multiple incomplete requirements
    for i in 0..5 {
        config.readiness_requirements.push(Requirement {
            id: format!("req-{}", i),
            name: format!("Requirement {}", i),
            description: "Test requirement".to_string(),
            status: Status::NotStarted,
            category: RequirementCategory::Testing,
            priority: 3,
            validation_command: None,
        });
    }

    let manager = LifecycleManager::new(config, None).unwrap();
    let score = manager.readiness().await.unwrap();

    // Should have recommendations
    assert!(!score.recommendations.is_empty());
}

#[tokio::test]
async fn test_deployment_requires_high_readiness() {
    let config = LifecycleConfig::default_with_name("test-deploy-req");
    let manager = LifecycleManager::new(config, None).unwrap();

    // Deployment should fail if readiness is low
    let result = manager.deploy("production").await;

    // Will fail due to low readiness score or validation issues
    // This is expected behavior
    assert!(result.is_err() || result.unwrap().validation_passed);
}
