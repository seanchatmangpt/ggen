//! Custom scoring algorithms for search results
//!
//! This module provides customizable scoring algorithms for ranking search results
//! in the marketplace. It combines multiple factors including relevance, popularity,
//! quality metrics, and recency to produce a final score for each package.
//!
//! ## Scoring Factors
//!
//! The `CustomScorer` combines four weighted factors:
//! - **Relevance**: TF-IDF based text relevance score (default: 50%)
//! - **Popularity**: Download count on logarithmic scale (default: 20%)
//! - **Quality**: User rating normalized to 0-1 (default: 20%)
//! - **Recency**: Time since last update with 1-year decay (default: 10%)
//!
//! ## Examples
//!
//! ### Using Default Scorer
//!
//! ```rust,no_run
//! use ggen_marketplace::search::scoring::CustomScorer;
//! use ggen_marketplace::types::Package;
//!
//! # fn example() {
//! let scorer = CustomScorer::default();
//! let package = /* ... get package ... */;
//! let base_relevance = 0.8; // From search engine
//!
//! let final_score = scorer.score(&package, base_relevance);
//! # }
//! ```
//!
//! ### Custom Weight Configuration
//!
//! ```rust,no_run
//! use ggen_marketplace::search::scoring::CustomScorer;
//!
//! // Emphasize quality over popularity
//! let scorer = CustomScorer::new(
//!     0.4,  // relevance: 40%
//!     0.1,  // popularity: 10%
//!     0.4,  // quality: 40%
//!     0.1,  // recency: 10%
//! );
//! ```

use crate::types::Package;

pub struct CustomScorer {
    relevance_weight: f32,
    popularity_weight: f32,
    quality_weight: f32,
    recency_weight: f32,
}

impl Default for CustomScorer {
    fn default() -> Self {
        Self {
            relevance_weight: 0.5,
            popularity_weight: 0.2,
            quality_weight: 0.2,
            recency_weight: 0.1,
        }
    }
}

impl CustomScorer {
    pub fn new(relevance: f32, popularity: f32, quality: f32, recency: f32) -> Self {
        Self {
            relevance_weight: relevance,
            popularity_weight: popularity,
            quality_weight: quality,
            recency_weight: recency,
        }
    }

    /// Calculate custom score for a package
    pub fn score(&self, package: &Package, base_score: f32) -> f32 {
        let relevance = base_score * self.relevance_weight;

        // Normalize downloads (log scale)
        let popularity = if package.downloads > 0 {
            (package.downloads as f32).log10() / 6.0 // Assume max ~1M downloads
        } else {
            0.0
        } * self.popularity_weight;

        // Normalize rating (0-5 scale)
        let quality = (package.rating / 5.0) * self.quality_weight;

        // Recency score (newer is better, decay over 1 year)
        let duration = chrono::Utc::now() - package.updated_at;
        let days_old = duration.num_days();
        let recency = if days_old >= 0 {
            (1.0 - (days_old as f32 / 365.0).min(1.0)) * self.recency_weight
        } else {
            self.recency_weight
        };

        relevance + popularity + quality + recency
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use chrono::Utc;

    #[test]
    fn test_custom_scoring() {
        let scorer = CustomScorer::default();

        let package = Package {
            id: "test".to_string(),
            name: "test-package".to_string(),
            description: "Test package".to_string(),
            version: "1.0.0".to_string(),
            category: "tools".to_string(),
            language: "rust".to_string(),
            license: "MIT".to_string(),
            tags: vec![],
            downloads: 1000,
            rating: 4.5,
            created_at: Utc::now(),
            updated_at: Utc::now(),
            author: "test".to_string(),
            repository_url: None,
        };

        let score = scorer.score(&package, 1.0);
        assert!(score > 0.0 && score <= 1.0);
    }
}
