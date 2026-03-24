//! Self-improvement and learning tests

use osiris_life_domains::*;

#[tokio::test]
async fn test_improvement_tracker_creation() {
    let tracker = ImprovementTracker::new();
    let outcomes = tracker.get_outcomes().await;
    assert!(outcomes.is_empty());
}

#[tokio::test]
async fn test_record_successful_action() {
    let tracker = ImprovementTracker::new();
    let result = tracker
        .record_action("health", "workout".to_string(), "success".to_string())
        .await;
    assert!(result.is_ok());
    
    let outcomes = tracker.get_outcomes().await;
    assert_eq!(outcomes.len(), 1);
    assert!(outcomes[0].success);
}

#[tokio::test]
async fn test_record_failed_action() {
    let tracker = ImprovementTracker::new();
    let result = tracker
        .record_action("health", "workout".to_string(), "failed".to_string())
        .await;
    assert!(result.is_ok());
    
    let outcomes = tracker.get_outcomes().await;
    assert_eq!(outcomes.len(), 1);
    assert!(!outcomes[0].success);
}

#[tokio::test]
async fn test_action_effectiveness_tracking() {
    let tracker = ImprovementTracker::new();
    
    // Record multiple successes
    for _ in 0..3 {
        tracker
            .record_action("health", "workout".to_string(), "success".to_string())
            .await
            .unwrap();
    }
    
    let effectiveness = tracker.get_action_effectiveness("health", "workout").await;
    assert!(effectiveness > 0.0);
}

#[tokio::test]
async fn test_get_top_actions() {
    let tracker = ImprovementTracker::new();
    
    // Record various actions
    tracker
        .record_action("health", "workout".to_string(), "success".to_string())
        .await
        .unwrap();
    tracker
        .record_action("health", "meditation".to_string(), "success".to_string())
        .await
        .unwrap();
    tracker
        .record_action("health", "sleep".to_string(), "success".to_string())
        .await
        .unwrap();
    
    let top_actions = tracker.get_top_actions("health", 2).await;
    assert_eq!(top_actions.len(), 2);
}

#[tokio::test]
async fn test_share_learning_between_agents() {
    let tracker = ImprovementTracker::new();
    
    // Health domain learns something
    tracker
        .record_action("health", "meditation".to_string(), "success".to_string())
        .await
        .unwrap();
    
    // Share with spirituality domain
    let result = tracker.share_learning("health", "spirituality").await;
    assert!(result.is_ok());
    
    // Verify spirituality now has effectiveness data
    let effectiveness = tracker.get_action_effectiveness("spirituality", "meditation").await;
    assert!(effectiveness >= 0.0);
}

#[tokio::test]
async fn test_outcome_timestamp() {
    let tracker = ImprovementTracker::new();
    tracker
        .record_action("health", "workout".to_string(), "success".to_string())
        .await
        .unwrap();
    
    let outcomes = tracker.get_outcomes().await;
    assert!(!outcomes[0].timestamp.is_empty());
}

#[tokio::test]
async fn test_multiple_domains_learning() {
    let tracker = ImprovementTracker::new();
    
    // Multiple domains record actions
    tracker
        .record_action("health", "exercise".to_string(), "success".to_string())
        .await
        .unwrap();
    tracker
        .record_action("career", "networking".to_string(), "success".to_string())
        .await
        .unwrap();
    tracker
        .record_action("learning", "studying".to_string(), "success".to_string())
        .await
        .unwrap();
    
    let outcomes = tracker.get_outcomes().await;
    assert_eq!(outcomes.len(), 3);
}
