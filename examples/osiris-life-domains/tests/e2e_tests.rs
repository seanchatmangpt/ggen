//! End-to-end workflow tests

use osiris_life_domains::*;

#[tokio::test]
async fn test_complete_reasoning_cycle() {
    let system = LifeDomainsSystem::new().await;
    system.initialize().await.unwrap();
    
    let result = system.reasoning_cycle().await;
    assert!(result.is_ok());
}

#[tokio::test]
async fn test_complete_balance_cycle() {
    let system = LifeDomainsSystem::new().await;
    system.initialize().await.unwrap();
    
    let result = system.balance_domains().await;
    assert!(result.is_ok());
}

#[tokio::test]
async fn test_full_system_workflow() {
    let system = LifeDomainsSystem::new().await;
    
    // Initialize
    assert!(system.initialize().await.is_ok());
    
    // Get initial status
    let initial_status = system.get_system_status().await.unwrap();
    assert!(initial_status["domain_statuses"].is_object());
    
    // Run reasoning cycle
    assert!(system.reasoning_cycle().await.is_ok());
    
    // Balance domains
    assert!(system.balance_domains().await.is_ok());
    
    // Get final status
    let final_status = system.get_system_status().await.unwrap();
    assert!(final_status["domain_statuses"].is_object());
}

#[tokio::test]
async fn test_system_status_structure() {
    let system = LifeDomainsSystem::new().await;
    system.initialize().await.unwrap();
    
    let status = system.get_system_status().await.unwrap();
    assert!(status["domain_statuses"].is_object());
    assert!(status["balance"].is_object());
    assert!(status["timestamp"].is_string());
}

#[tokio::test]
async fn test_multiple_reasoning_cycles() {
    let system = LifeDomainsSystem::new().await;
    system.initialize().await.unwrap();
    
    for _ in 0..3 {
        assert!(system.reasoning_cycle().await.is_ok());
    }
}

#[tokio::test]
async fn test_interleaved_operations() {
    let system = LifeDomainsSystem::new().await;
    system.initialize().await.unwrap();
    
    // Interleave operations
    system.reasoning_cycle().await.unwrap();
    system.balance_domains().await.unwrap();
    system.reasoning_cycle().await.unwrap();
    system.balance_domains().await.unwrap();
    
    let status = system.get_system_status().await.unwrap();
    assert!(status["domain_statuses"].is_object());
}

#[tokio::test]
async fn test_all_agents_have_health_scores() {
    let system = LifeDomainsSystem::new().await;
    system.initialize().await.unwrap();
    
    let status = system.get_system_status().await.unwrap();
    let domains = status["domain_statuses"].as_object().unwrap();
    
    for (name, domain_status) in domains {
        assert!(
            domain_status["health_score"].is_f64(),
            "{} missing health_score",
            name
        );
    }
}

#[tokio::test]
async fn test_system_maintains_state_consistency() {
    let system = LifeDomainsSystem::new().await;
    system.initialize().await.unwrap();
    
    // Get status multiple times - should be consistent structure
    for _ in 0..5 {
        let status = system.get_system_status().await.unwrap();
        let domains = status["domain_statuses"].as_object().unwrap();
        assert_eq!(domains.len(), 6);
    }
}

#[tokio::test]
async fn test_system_responds_to_imbalances() {
    let system = LifeDomainsSystem::new().await;
    system.initialize().await.unwrap();
    
    // Run multiple cycles to let system detect and respond to imbalances
    system.reasoning_cycle().await.unwrap();
    system.balance_domains().await.unwrap();
    
    let status = system.get_system_status().await.unwrap();
    assert!(status["balance"].is_object());
}
