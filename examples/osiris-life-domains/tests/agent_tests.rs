//! Agent lifecycle and coordination tests

use osiris_life_domains::*;

#[tokio::test]
async fn test_life_domains_initialization() {
    let system = LifeDomainsSystem::new().await;
    let result = system.initialize().await;
    assert!(result.is_ok());
}

#[tokio::test]
async fn test_system_has_six_agents() {
    let system = LifeDomainsSystem::new().await;
    system.initialize().await.unwrap();
    
    let status = system.get_system_status().await.unwrap();
    let domains = status["domain_statuses"].as_object().unwrap();
    assert_eq!(domains.len(), 6);
}

#[tokio::test]
async fn test_all_domains_present() {
    let system = LifeDomainsSystem::new().await;
    system.initialize().await.unwrap();
    
    let status = system.get_system_status().await.unwrap();
    let domains = status["domain_statuses"].as_object().unwrap();
    
    assert!(domains.contains_key("health"));
    assert!(domains.contains_key("career"));
    assert!(domains.contains_key("relationships"));
    assert!(domains.contains_key("finance"));
    assert!(domains.contains_key("learning"));
    assert!(domains.contains_key("spirituality"));
}

#[tokio::test]
async fn test_health_scores_computed() {
    let system = LifeDomainsSystem::new().await;
    system.initialize().await.unwrap();
    
    let status = system.get_system_status().await.unwrap();
    let domains = status["domain_statuses"].as_object().unwrap();
    
    for domain in domains.values() {
        let health_score = domain["health_score"].as_f64();
        assert!(health_score.is_some());
        let score = health_score.unwrap();
        assert!(score >= 0.0 && score <= 1.0);
    }
}

#[tokio::test]
async fn test_system_status_has_timestamp() {
    let system = LifeDomainsSystem::new().await;
    system.initialize().await.unwrap();
    
    let status = system.get_system_status().await.unwrap();
    assert!(status["timestamp"].is_string());
}

#[tokio::test]
async fn test_agent_status_includes_goals() {
    let agent = HealthAgent::new("health");
    let status = agent.get_status().await.unwrap();
    assert!(status.goals.is_empty()); // Initially empty
}

#[tokio::test]
async fn test_agent_status_includes_actions() {
    let agent = CareerAgent::new("career");
    let status = agent.get_status().await.unwrap();
    assert!(!status.current_actions.is_empty());
}

#[tokio::test]
async fn test_agent_status_includes_outcomes() {
    let agent = LearningAgent::new("learning");
    let status = agent.get_status().await.unwrap();
    assert!(!status.recent_outcomes.is_empty());
}
