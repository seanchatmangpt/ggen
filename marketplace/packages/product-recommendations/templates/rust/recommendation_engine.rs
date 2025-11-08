// Product Recommendations - Real-Time Recommendation Engine (Rust)
// High-performance collaborative filtering with in-memory caching

use std::collections::{HashMap, HashSet};
use std::sync::Arc;
use tokio::sync::RwLock;

#[derive(Debug, Clone)]
pub struct UserBehavior {
    pub user_id: String,
    pub product_id: String,
    pub behavior_type: BehaviorType,
    pub timestamp: i64,
    pub score: f64,
}

#[derive(Debug, Clone, PartialEq)]
pub enum BehaviorType {
    View,
    Click,
    AddToCart,
    Purchase,
    Rating(u8),
}

impl BehaviorType {
    pub fn weight(&self) -> f64 {
        match self {
            BehaviorType::View => 1.0,
            BehaviorType::Click => 2.0,
            BehaviorType::AddToCart => 5.0,
            BehaviorType::Purchase => 10.0,
            BehaviorType::Rating(stars) => *stars as f64 * 2.0,
        }
    }
}

#[derive(Debug, Clone)]
pub struct Recommendation {
    pub product_id: String,
    pub score: f64,
    pub reason: String,
}

pub struct RecommendationEngine {
    user_behaviors: Arc<RwLock<HashMap<String, Vec<UserBehavior>>>>,
    product_similarities: Arc<RwLock<HashMap<String, Vec<(String, f64)>>>>,
}

impl RecommendationEngine {
    pub fn new() -> Self {
        Self {
            user_behaviors: Arc::new(RwLock::new(HashMap::new())),
            product_similarities: Arc::new(RwLock::new(HashMap::new())),
        }
    }

    /// Record user behavior for real-time recommendations
    pub async fn record_behavior(&self, behavior: UserBehavior) {
        let mut behaviors = self.user_behaviors.write().await;
        behaviors.entry(behavior.user_id.clone())
            .or_insert_with(Vec::new)
            .push(behavior);
    }

    /// Generate personalized recommendations using collaborative filtering
    pub async fn recommend_for_user(
        &self,
        user_id: &str,
        limit: usize,
    ) -> Vec<Recommendation> {
        let behaviors = self.user_behaviors.read().await;

        // Get user's purchase/interaction history
        let user_history = match behaviors.get(user_id) {
            Some(history) => history,
            None => return Vec::new(),
        };

        let user_products: HashSet<String> = user_history.iter()
            .map(|b| b.product_id.clone())
            .collect();

        // Find similar users (collaborative filtering)
        let similar_users = self.find_similar_users(user_id, &behaviors).await;

        // Aggregate recommendations from similar users
        let mut product_scores: HashMap<String, f64> = HashMap::new();

        for (similar_user_id, similarity_score) in similar_users.iter().take(10) {
            if let Some(similar_user_history) = behaviors.get(similar_user_id) {
                for behavior in similar_user_history {
                    // Skip products user already interacted with
                    if user_products.contains(&behavior.product_id) {
                        continue;
                    }

                    let weighted_score = behavior.behavior_type.weight() * similarity_score;
                    *product_scores.entry(behavior.product_id.clone())
                        .or_insert(0.0) += weighted_score;
                }
            }
        }

        // Sort by score and return top N
        let mut recommendations: Vec<Recommendation> = product_scores
            .into_iter()
            .map(|(product_id, score)| Recommendation {
                product_id,
                score,
                reason: "Users with similar tastes also liked".to_string(),
            })
            .collect();

        recommendations.sort_by(|a, b| b.score.partial_cmp(&a.score).unwrap());
        recommendations.truncate(limit);

        recommendations
    }

    /// Find users with similar behavior patterns
    async fn find_similar_users(
        &self,
        user_id: &str,
        behaviors: &HashMap<String, Vec<UserBehavior>>,
    ) -> Vec<(String, f64)> {
        let user_history = match behaviors.get(user_id) {
            Some(history) => history,
            None => return Vec::new(),
        };

        let user_products: HashSet<&String> = user_history.iter()
            .map(|b| &b.product_id)
            .collect();

        // Calculate Jaccard similarity with other users
        let mut similarities: Vec<(String, f64)> = Vec::new();

        for (other_user_id, other_history) in behaviors.iter() {
            if other_user_id == user_id {
                continue;
            }

            let other_products: HashSet<&String> = other_history.iter()
                .map(|b| &b.product_id)
                .collect();

            let intersection = user_products.intersection(&other_products).count();
            let union = user_products.union(&other_products).count();

            if union > 0 {
                let similarity = intersection as f64 / union as f64;
                if similarity > 0.1 {  // Minimum similarity threshold
                    similarities.push((other_user_id.clone(), similarity));
                }
            }
        }

        similarities.sort_by(|a, b| b.1.partial_cmp(&a.1).unwrap());
        similarities
    }

    /// Generate "frequently bought together" recommendations
    pub async fn recommend_bundle(
        &self,
        product_id: &str,
        limit: usize,
    ) -> Vec<Recommendation> {
        let behaviors = self.user_behaviors.read().await;

        // Find users who purchased this product
        let mut co_purchased: HashMap<String, usize> = HashMap::new();

        for user_history in behaviors.values() {
            let purchased_this = user_history.iter()
                .any(|b| b.product_id == product_id && matches!(b.behavior_type, BehaviorType::Purchase));

            if purchased_this {
                for behavior in user_history {
                    if behavior.product_id != product_id && matches!(behavior.behavior_type, BehaviorType::Purchase) {
                        *co_purchased.entry(behavior.product_id.clone()).or_insert(0) += 1;
                    }
                }
            }
        }

        let mut recommendations: Vec<Recommendation> = co_purchased
            .into_iter()
            .map(|(pid, count)| Recommendation {
                product_id: pid,
                score: count as f64,
                reason: format!("Frequently bought with this product ({} times)", count),
            })
            .collect();

        recommendations.sort_by(|a, b| b.score.partial_cmp(&a.score).unwrap());
        recommendations.truncate(limit);

        recommendations
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[tokio::test]
    async fn test_collaborative_filtering() {
        let engine = RecommendationEngine::new();

        // User A likes Product 1 and 2
        engine.record_behavior(UserBehavior {
            user_id: "user_a".to_string(),
            product_id: "product_1".to_string(),
            behavior_type: BehaviorType::Purchase,
            timestamp: 1000,
            score: 10.0,
        }).await;
        engine.record_behavior(UserBehavior {
            user_id: "user_a".to_string(),
            product_id: "product_2".to_string(),
            behavior_type: BehaviorType::Purchase,
            timestamp: 1100,
            score: 10.0,
        }).await;

        // User B likes Product 1 and 3
        engine.record_behavior(UserBehavior {
            user_id: "user_b".to_string(),
            product_id: "product_1".to_string(),
            behavior_type: BehaviorType::Purchase,
            timestamp: 1200,
            score: 10.0,
        }).await;
        engine.record_behavior(UserBehavior {
            user_id: "user_b".to_string(),
            product_id: "product_3".to_string(),
            behavior_type: BehaviorType::Purchase,
            timestamp: 1300,
            score: 10.0,
        }).await;

        // User A should get Product 3 recommended (via User B similarity)
        let recommendations = engine.recommend_for_user("user_a", 5).await;

        assert!(!recommendations.is_empty());
        assert_eq!(recommendations[0].product_id, "product_3");
    }

    #[tokio::test]
    async fn test_bundle_recommendations() {
        let engine = RecommendationEngine::new();

        // User C bought Product 1 and 4
        engine.record_behavior(UserBehavior {
            user_id: "user_c".to_string(),
            product_id: "product_1".to_string(),
            behavior_type: BehaviorType::Purchase,
            timestamp: 1400,
            score: 10.0,
        }).await;
        engine.record_behavior(UserBehavior {
            user_id: "user_c".to_string(),
            product_id: "product_4".to_string(),
            behavior_type: BehaviorType::Purchase,
            timestamp: 1500,
            score: 10.0,
        }).await;

        // Bundle recommendation for Product 1 should include Product 4
        let bundles = engine.recommend_bundle("product_1", 5).await;

        assert!(!bundles.is_empty());
        assert_eq!(bundles[0].product_id, "product_4");
    }
}
