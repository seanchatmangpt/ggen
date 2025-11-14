//! Smart recommendation system using collaborative filtering and machine learning
//!
//! This module provides a recommendation engine that uses collaborative filtering
//! algorithms to suggest packages to users based on their preferences, interaction
//! history, and similarity to other users and packages.
//!
//! ## Features
//!
//! - **Collaborative Filtering**: User-based and item-based recommendation algorithms
//! - **Cosine Similarity**: Calculate similarity between users and packages
//! - **Trending Detection**: Identify trending packages in categories
//! - **Complementary Packages**: Find packages frequently used together
//! - **User Preference Learning**: Learn from user interaction history
//!
//! ## Recommendation Types
//!
//! - **SimilarUsers**: Based on users with similar preferences
//! - **SimilarPackages**: Based on package feature similarity
//! - **TrendingInCategory**: Based on trending scores in a category
//! - **FrequentlyUsedTogether**: Based on co-occurrence patterns
//!
//! ## Examples
//!
//! ### Getting User Recommendations
//!
//! ```rust,no_run
//! use ggen_marketplace::recommendations::RecommendationEngine;
//!
//! # fn example() -> anyhow::Result<()> {
//! let mut engine = RecommendationEngine::new();
//!
//! // Initialize with user-package interactions
//! let interactions = vec![
//!     ("user1".to_string(), "pkg1".to_string(), 1.0),
//!     ("user1".to_string(), "pkg2".to_string(), 0.8),
//! ];
//! engine.initialize(interactions)?;
//!
//! // Get recommendations for a user
//! let recommendations = engine.recommend_for_user("user1", 10)?;
//! for rec in recommendations {
//!     println!("{}: {:.2} ({:?})", rec.package_id, rec.score, rec.reason);
//! }
//! # Ok(())
//! # }
//! ```
//!
//! ### Finding Similar Packages
//!
//! ```rust,no_run
//! use ggen_marketplace::recommendations::RecommendationEngine;
//! use ndarray::Array1;
//!
//! # fn example() -> anyhow::Result<()> {
//! let mut engine = RecommendationEngine::new();
//!
//! // Set package features for similarity calculation
//! let features = Array1::from_vec(vec![0.8, 0.6, 0.9, 0.7]);
//! engine.update_package_features("pkg1".to_string(), features);
//!
//! // Find similar packages
//! let similar = engine.find_similar_packages("pkg1", 5)?;
//! # Ok(())
//! # }
//! ```

#![allow(clippy::unwrap_used)] // Safe: f64 scores from cosine similarity are never NaN
use anyhow::{Result, Context};
use ndarray::{Array1, Array2, ArrayView1};
use serde::{Deserialize, Serialize};
use std::collections::HashMap;

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct PackageRecommendation {
    pub package_id: String,
    pub score: f64,
    pub reason: RecommendationReason,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum RecommendationReason {
    SimilarUsers,
    SimilarPackages,
    TrendingInCategory,
    FrequentlyUsedTogether,
}

#[derive(Debug, Clone)]
pub struct UserPreferences {
    pub user_id: String,
    pub installed_packages: Vec<String>,
    pub search_history: Vec<String>,
    pub download_counts: HashMap<String, u32>,
}

pub struct RecommendationEngine {
    // User-item interaction matrix
    interaction_matrix: Array2<f64>,
    user_ids: Vec<String>,
    package_ids: Vec<String>,
    // Package feature vectors for similarity
    package_features: HashMap<String, Array1<f64>>,
    // Category-based trending scores
    trending_scores: HashMap<String, f64>,
}

impl RecommendationEngine {
    pub fn new() -> Self {
        Self {
            interaction_matrix: Array2::zeros((0, 0)),
            user_ids: Vec::new(),
            package_ids: Vec::new(),
            package_features: HashMap::new(),
            trending_scores: HashMap::new(),
        }
    }

    /// Initialize with user-package interaction data
    pub fn initialize(&mut self, interactions: Vec<(String, String, f64)>) -> Result<()> {
        let mut user_set = std::collections::HashSet::new();
        let mut package_set = std::collections::HashSet::new();

        for (user_id, package_id, _) in &interactions {
            user_set.insert(user_id.clone());
            package_set.insert(package_id.clone());
        }

        self.user_ids = user_set.into_iter().collect();
        self.package_ids = package_set.into_iter().collect();

        let n_users = self.user_ids.len();
        let n_packages = self.package_ids.len();

        self.interaction_matrix = Array2::zeros((n_users, n_packages));

        for (user_id, package_id, score) in interactions {
            if let Some(user_idx) = self.user_ids.iter().position(|u| u == &user_id) {
                if let Some(pkg_idx) = self.package_ids.iter().position(|p| p == &package_id) {
                    self.interaction_matrix[[user_idx, pkg_idx]] = score;
                }
            }
        }

        Ok(())
    }

    /// Get recommendations for a user using collaborative filtering
    pub fn recommend_for_user(
        &self,
        user_id: &str,
        n_recommendations: usize,
    ) -> Result<Vec<PackageRecommendation>> {
        let user_idx = self.user_ids.iter()
            .position(|u| u == user_id)
            .context("User not found")?;

        let user_vector = self.interaction_matrix.row(user_idx);
        let mut recommendations = Vec::new();

        // Find similar users using cosine similarity
        let similar_users = self.find_similar_users(user_vector)?;

        // Get packages that similar users liked but current user hasn't tried
        for (similar_user_idx, similarity) in similar_users.iter().take(10) {
            let similar_user_vector = self.interaction_matrix.row(*similar_user_idx);

            for (pkg_idx, &score) in similar_user_vector.iter().enumerate() {
                if score > 0.0 && user_vector[pkg_idx] == 0.0 {
                    let package_id = self.package_ids[pkg_idx].clone();
                    let weighted_score = score * similarity;

                    recommendations.push(PackageRecommendation {
                        package_id,
                        score: weighted_score,
                        reason: RecommendationReason::SimilarUsers,
                    });
                }
            }
        }

        // Sort by score and take top N
        recommendations.sort_by(|a, b| b.score.partial_cmp(&a.score).unwrap());
        recommendations.truncate(n_recommendations);

        Ok(recommendations)
    }

    /// Find similar users using cosine similarity
    fn find_similar_users(&self, user_vector: ArrayView1<f64>) -> Result<Vec<(usize, f64)>> {
        let mut similarities = Vec::new();

        for (idx, other_user) in self.interaction_matrix.axis_iter(ndarray::Axis(0)).enumerate() {
            let similarity = self.cosine_similarity(user_vector, other_user);
            if similarity > 0.0 {
                similarities.push((idx, similarity));
            }
        }

        similarities.sort_by(|a, b| b.1.partial_cmp(&a.1).unwrap());
        Ok(similarities)
    }

    /// Calculate cosine similarity between two vectors
    fn cosine_similarity(&self, a: ArrayView1<f64>, b: ArrayView1<f64>) -> f64 {
        let dot_product: f64 = a.iter().zip(b.iter()).map(|(x, y)| x * y).sum();
        let magnitude_a: f64 = a.iter().map(|x| x * x).sum::<f64>().sqrt();
        let magnitude_b: f64 = b.iter().map(|x| x * x).sum::<f64>().sqrt();

        if magnitude_a == 0.0 || magnitude_b == 0.0 {
            0.0
        } else {
            dot_product / (magnitude_a * magnitude_b)
        }
    }

    /// Find similar packages based on feature vectors
    pub fn find_similar_packages(
        &self,
        package_id: &str,
        n_similar: usize,
    ) -> Result<Vec<PackageRecommendation>> {
        let features = self.package_features.get(package_id)
            .context("Package features not found")?;

        let mut similarities = Vec::new();

        for (other_id, other_features) in &self.package_features {
            if other_id != package_id {
                let similarity = self.cosine_similarity(features.view(), other_features.view());
                if similarity > 0.0 {
                    similarities.push(PackageRecommendation {
                        package_id: other_id.clone(),
                        score: similarity,
                        reason: RecommendationReason::SimilarPackages,
                    });
                }
            }
        }

        similarities.sort_by(|a, b| b.score.partial_cmp(&a.score).unwrap());
        similarities.truncate(n_similar);

        Ok(similarities)
    }

    /// Update package features for similarity calculation
    pub fn update_package_features(&mut self, package_id: String, features: Array1<f64>) {
        self.package_features.insert(package_id, features);
    }

    /// Get trending packages in a category
    pub fn get_trending_packages(
        &self,
        category: &str,
        n_trending: usize,
    ) -> Vec<PackageRecommendation> {
        let prefix = format!("{}_", category);
        let mut trending: Vec<_> = self.trending_scores
            .iter()
            .filter(|(k, _)| k.starts_with(&prefix))
            .map(|(k, v)| PackageRecommendation {
                package_id: k.clone(),
                score: *v,
                reason: RecommendationReason::TrendingInCategory,
            })
            .collect();

        trending.sort_by(|a, b| b.score.partial_cmp(&a.score).unwrap());
        trending.truncate(n_trending);
        trending
    }

    /// Update trending scores based on recent activity
    pub fn update_trending_score(&mut self, package_id: String, score: f64) {
        self.trending_scores
            .entry(package_id)
            .and_modify(|s| *s = (*s * 0.9) + (score * 0.1))
            .or_insert(score);
    }

    /// Find packages frequently used together
    pub fn find_complementary_packages(
        &self,
        package_id: &str,
        n_complementary: usize,
    ) -> Result<Vec<PackageRecommendation>> {
        let pkg_idx = self.package_ids.iter()
            .position(|p| p == package_id)
            .context("Package not found")?;

        let mut co_occurrence = vec![0.0; self.package_ids.len()];

        // Count how often packages appear together in user installations
        for user_row in self.interaction_matrix.axis_iter(ndarray::Axis(0)) {
            if user_row[pkg_idx] > 0.0 {
                for (idx, &score) in user_row.iter().enumerate() {
                    if idx != pkg_idx && score > 0.0 {
                        co_occurrence[idx] += 1.0;
                    }
                }
            }
        }

        let mut complementary: Vec<_> = co_occurrence
            .into_iter()
            .enumerate()
            .filter(|(_, count)| *count > 0.0)
            .map(|(idx, count)| PackageRecommendation {
                package_id: self.package_ids[idx].clone(),
                score: count,
                reason: RecommendationReason::FrequentlyUsedTogether,
            })
            .collect();

        complementary.sort_by(|a, b| b.score.partial_cmp(&a.score).unwrap());
        complementary.truncate(n_complementary);

        Ok(complementary)
    }

    /// Learn user preferences from interaction history
    pub fn learn_user_preferences(&mut self, preferences: UserPreferences) -> Result<()> {
        // Add user if not exists
        if !self.user_ids.contains(&preferences.user_id) {
            self.user_ids.push(preferences.user_id.clone());
            // Expand interaction matrix
            let new_row = Array1::zeros(self.package_ids.len());
            self.interaction_matrix = ndarray::concatenate![
                ndarray::Axis(0),
                self.interaction_matrix,
                new_row.into_shape((1, self.package_ids.len()))?
            ];
        }

        let user_idx = self.user_ids.iter()
            .position(|u| u == &preferences.user_id)
            .unwrap();

        // Update interaction scores based on download counts
        for (package_id, count) in preferences.download_counts {
            if let Some(pkg_idx) = self.package_ids.iter().position(|p| p == &package_id) {
                // Normalize download count to 0-1 scale
                let score = (count as f64).log10().min(1.0);
                self.interaction_matrix[[user_idx, pkg_idx]] = score;
            }
        }

        Ok(())
    }
}

impl Default for RecommendationEngine {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_recommendation_engine() {
        let mut engine = RecommendationEngine::new();

        let interactions = vec![
            ("user1".to_string(), "pkg1".to_string(), 1.0),
            ("user1".to_string(), "pkg2".to_string(), 0.8),
            ("user2".to_string(), "pkg1".to_string(), 0.9),
            ("user2".to_string(), "pkg3".to_string(), 1.0),
        ];

        engine.initialize(interactions).unwrap();
        let recommendations = engine.recommend_for_user("user1", 5).unwrap();

        assert!(!recommendations.is_empty());
    }

    #[test]
    fn test_cosine_similarity() {
        let engine = RecommendationEngine::new();
        let a = Array1::from_vec(vec![1.0, 2.0, 3.0]);
        let b = Array1::from_vec(vec![2.0, 3.0, 4.0]);

        let similarity = engine.cosine_similarity(a.view(), b.view());
        assert!(similarity > 0.9); // Should be high similarity
    }
}
