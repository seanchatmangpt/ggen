//! Domain creation and status tests

use osiris_life_domains::*;

#[tokio::test]
async fn test_health_agent_creation() {
    let agent = HealthAgent::new("health");
    let status = agent.get_status().await.unwrap();
    assert_eq!(status.domain_id, "health");
    assert!(status.health_score >= 0.0 && status.health_score <= 1.0);
}

#[tokio::test]
async fn test_health_agent_exercise_action() {
    let agent = HealthAgent::new("health");
    let result = agent.execute_action("workout").await.unwrap();
    assert!(result.contains("30-minute"));
}

#[tokio::test]
async fn test_health_agent_recommendations() {
    let agent = HealthAgent::new("health");
    let recommendations = agent.recommend_improvements().await.unwrap();
    assert!(!recommendations.is_empty());
}

#[tokio::test]
async fn test_career_agent_creation() {
    let agent = CareerAgent::new("career");
    let status = agent.get_status().await.unwrap();
    assert_eq!(status.domain_id, "career");
}

#[tokio::test]
async fn test_career_agent_skill_learning() {
    let agent = CareerAgent::new("career");
    let result = agent.execute_action("learn_skill").await.unwrap();
    assert!(result.contains("skill"));
}

#[tokio::test]
async fn test_relationship_agent_creation() {
    let agent = RelationshipAgent::new("relationships");
    let status = agent.get_status().await.unwrap();
    assert_eq!(status.domain_id, "relationships");
}

#[tokio::test]
async fn test_relationship_agent_quality_time() {
    let agent = RelationshipAgent::new("relationships");
    let result = agent.execute_action("quality_time").await.unwrap();
    assert!(result.contains("quality time"));
}

#[tokio::test]
async fn test_finance_agent_creation() {
    let agent = FinanceAgent::new("finance");
    let status = agent.get_status().await.unwrap();
    assert_eq!(status.domain_id, "finance");
}

#[tokio::test]
async fn test_finance_agent_savings() {
    let agent = FinanceAgent::new("finance");
    let result = agent.execute_action("increase_savings").await.unwrap();
    assert!(result.contains("savings"));
}

#[tokio::test]
async fn test_learning_agent_creation() {
    let agent = LearningAgent::new("learning");
    let status = agent.get_status().await.unwrap();
    assert_eq!(status.domain_id, "learning");
}

#[tokio::test]
async fn test_learning_agent_course() {
    let agent = LearningAgent::new("learning");
    let result = agent.execute_action("complete_course").await.unwrap();
    assert!(result.contains("course"));
}

#[tokio::test]
async fn test_spirituality_agent_creation() {
    let agent = SpiritualityAgent::new("spirituality");
    let status = agent.get_status().await.unwrap();
    assert_eq!(status.domain_id, "spirituality");
}

#[tokio::test]
async fn test_spirituality_agent_meditation() {
    let agent = SpiritualityAgent::new("spirituality");
    let result = agent.execute_action("meditate_20min").await.unwrap();
    assert!(result.contains("meditation"));
}

#[tokio::test]
async fn test_agent_goal_setting() {
    let agent = HealthAgent::new("health");
    let goals = vec!["increase_exercise".to_string(), "improve_sleep".to_string()];
    let result = agent.set_goals(goals).await;
    assert!(result.is_ok());
}

#[tokio::test]
async fn test_agent_resource_allocation() {
    let agent = CareerAgent::new("career");
    let result = agent.allocate_resources(0.3).await;
    assert!(result.is_ok());
}
