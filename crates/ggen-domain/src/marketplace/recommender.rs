//! Package recommendation engine based on sector bundles and validation history
//!
//! Provides intelligent recommendations using collaborative filtering and
//! semantic similarity based on the marketplace ontology.

use serde::{Deserialize, Serialize};

/// Package recommendation with reasoning
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Recommendation {
    /// Recommended package ID
    pub package_id: String,

    /// Recommendation confidence (0-100)
    pub confidence: f64,

    /// Why this package is recommended
    pub reason: RecommendationReason,

    /// Quality score of the package
    pub quality_score: f64,

    /// Sector bundles it belongs to
    pub bundles: Vec<String>,

    /// Complementary packages (work well together)
    pub complements: Vec<String>,
}

/// Reason for recommendation
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum RecommendationReason {
    /// Same sector bundle as user's selections
    SameBundleAffinity { bundle: String },

    /// Complements installed packages
    Complementary { package: String },

    /// Similar category and high quality
    CategorySimilarity { category: String, avg_score: f64 },

    /// Popular in same domain
    PopularInDomain { domain: String },

    /// Trending (recently improved)
    Trending { improvement_rate: f64 },
}

/// Personalized recommendation set
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct RecommendationSet {
    /// Recommendations for the user
    pub recommendations: Vec<Recommendation>,

    /// User's installed packages (for filtering)
    pub installed: Vec<String>,

    /// User's preferred sector bundles
    pub preferred_bundles: Vec<String>,

    /// Total packages evaluated
    pub total_evaluated: usize,

    /// Quality threshold used
    pub quality_threshold: f64,
}

/// Recommendation engine configuration
#[derive(Debug, Clone)]
pub struct RecommenderConfig {
    /// Minimum quality score to recommend (0-100)
    pub min_quality_score: f64,

    /// Maximum number of recommendations
    pub max_recommendations: usize,

    /// Whether to recommend production-ready only
    pub production_ready_only: bool,

    /// Weight for bundle affinity (0-100)
    pub bundle_affinity_weight: f64,

    /// Weight for complementarity (0-100)
    pub complementarity_weight: f64,

    /// Weight for quality (0-100)
    pub quality_weight: f64,

    /// Weight for popularity (0-100)
    pub popularity_weight: f64,
}

impl Default for RecommenderConfig {
    fn default() -> Self {
        Self {
            min_quality_score: 75.0,
            max_recommendations: 10,
            production_ready_only: false,
            bundle_affinity_weight: 30.0,
            complementarity_weight: 25.0,
            quality_weight: 30.0,
            popularity_weight: 15.0,
        }
    }
}

/// Package information for recommendation
#[derive(Debug, Clone)]
pub struct PackageInfo {
    /// Package identifier
    pub id: String,

    /// Quality score
    pub score: f64,

    /// Production-ready status
    pub production_ready: bool,

    /// Category
    pub category: String,

    /// Sector bundles
    pub bundles: Vec<String>,

    /// Related/complementary packages
    pub related: Vec<String>,

    /// Download count
    pub downloads: usize,

    /// Tags
    pub tags: Vec<String>,
}

/// Recommendation engine
pub struct Recommender;

impl Recommender {
    /// Generate recommendations for a user
    pub fn recommend(
        config: &RecommenderConfig,
        installed: Vec<String>,
        preferred_bundles: Vec<String>,
        all_packages: Vec<PackageInfo>,
    ) -> RecommendationSet {
        let mut recommendations = Vec::new();

        // Filter candidates
        let candidates: Vec<_> = all_packages
            .iter()
            .filter(|pkg| {
                !installed.contains(&pkg.id)
                    && (!config.production_ready_only || pkg.production_ready)
                    && pkg.score >= config.min_quality_score
            })
            .collect();

        // Score each candidate
        let mut scored: Vec<_> = candidates
            .iter()
            .map(|pkg| {
                let confidence = Self::calculate_recommendation_score(
                    pkg,
                    &installed,
                    &preferred_bundles,
                    config,
                );
                (pkg, confidence)
            })
            .collect();

        // Sort by confidence
        scored.sort_by(|a, b| b.1.partial_cmp(&a.1).unwrap_or(std::cmp::Ordering::Equal));

        // Build recommendations
        for (pkg, confidence) in scored.iter().take(config.max_recommendations) {
            let reason = Self::determine_reason(pkg, &installed, &preferred_bundles);

            recommendations.push(Recommendation {
                package_id: pkg.id.clone(),
                confidence: *confidence,
                reason,
                quality_score: pkg.score,
                bundles: pkg.bundles.clone(),
                complements: pkg
                    .related
                    .iter()
                    .filter(|r| installed.contains(r))
                    .cloned()
                    .collect(),
            });
        }

        RecommendationSet {
            recommendations,
            installed: installed.clone(),
            preferred_bundles,
            total_evaluated: all_packages.len(),
            quality_threshold: config.min_quality_score,
        }
    }

    /// Calculate recommendation score using weighted factors
    fn calculate_recommendation_score(
        pkg: &PackageInfo,
        installed: &[String],
        preferred_bundles: &[String],
        config: &RecommenderConfig,
    ) -> f64 {
        let mut score = 0.0;

        // Bundle affinity: matching preferred bundles
        let bundle_matches = pkg
            .bundles
            .iter()
            .filter(|b| preferred_bundles.contains(b))
            .count();
        if bundle_matches > 0 {
            score += (bundle_matches as f64 / preferred_bundles.len().max(1) as f64)
                * config.bundle_affinity_weight;
        }

        // Complementarity: related packages installed
        let complementary = pkg
            .related
            .iter()
            .filter(|r| installed.contains(r))
            .count();
        if complementary > 0 {
            score += (complementary as f64 / pkg.related.len().max(1) as f64)
                * config.complementarity_weight;
        }

        // Quality score contribution
        let quality_normalized = (pkg.score / 100.0).min(1.0);
        score += quality_normalized * config.quality_weight;

        // Popularity contribution
        let popularity_normalized = (pkg.downloads as f64 / 10000.0).min(1.0);
        score += popularity_normalized * config.popularity_weight;

        // Normalize to 0-100
        (score / (config.bundle_affinity_weight
            + config.complementarity_weight
            + config.quality_weight
            + config.popularity_weight))
            * 100.0
    }

    /// Determine the primary reason for recommendation
    fn determine_reason(
        pkg: &PackageInfo,
        installed: &[String],
        preferred_bundles: &[String],
    ) -> RecommendationReason {
        // Check for bundle affinity
        for bundle in &pkg.bundles {
            if preferred_bundles.contains(bundle) {
                return RecommendationReason::SameBundleAffinity {
                    bundle: bundle.clone(),
                };
            }
        }

        // Check for complementarity
        for related in &pkg.related {
            if installed.contains(related) {
                return RecommendationReason::Complementary {
                    package: related.clone(),
                };
            }
        }

        // Default to category similarity
        RecommendationReason::CategorySimilarity {
            category: pkg.category.clone(),
            avg_score: pkg.score,
        }
    }

    /// Get complementary packages for a given package
    pub fn get_complements(
        package_id: &str,
        all_packages: Vec<PackageInfo>,
    ) -> Vec<String> {
        all_packages
            .iter()
            .find(|p| p.id == package_id)
            .map(|p| p.related.clone())
            .unwrap_or_default()
    }

    /// Find packages in same bundle
    pub fn find_bundle_peers(
        package_id: &str,
        all_packages: Vec<PackageInfo>,
    ) -> Vec<String> {
        let pkg = all_packages.iter().find(|p| p.id == package_id);

        if let Some(pkg) = pkg {
            all_packages
                .iter()
                .filter(|other| {
                    other.id != package_id
                        && other
                            .bundles
                            .iter()
                            .any(|b| pkg.bundles.contains(b))
                })
                .map(|p| p.id.clone())
                .collect()
        } else {
            Vec::new()
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_recommendation_scoring() {
        let config = RecommenderConfig::default();
        let pkg = PackageInfo {
            id: "test-pkg".to_string(),
            score: 85.0,
            production_ready: true,
            category: "test".to_string(),
            bundles: vec!["sector-enterprise-saas".to_string()],
            related: vec!["crm-tool".to_string()],
            downloads: 1000,
            tags: vec!["enterprise".to_string()],
        };

        let installed = vec!["crm-tool".to_string()];
        let bundles = vec!["sector-enterprise-saas".to_string()];

        let score = Recommender::calculate_recommendation_score(&pkg, &installed, &bundles, &config);
        assert!(score > 0.0 && score <= 100.0);
    }

    #[test]
    fn test_bundle_peers() {
        let all_packages = vec![
            PackageInfo {
                id: "crm-system".to_string(),
                score: 85.0,
                production_ready: true,
                category: "enterprise".to_string(),
                bundles: vec!["sector-enterprise-saas".to_string()],
                related: vec![],
                downloads: 500,
                tags: vec![],
            },
            PackageInfo {
                id: "erp-system".to_string(),
                score: 80.0,
                production_ready: true,
                category: "enterprise".to_string(),
                bundles: vec!["sector-enterprise-saas".to_string()],
                related: vec!["crm-system".to_string()],
                downloads: 400,
                tags: vec![],
            },
        ];

        let peers = Recommender::find_bundle_peers("crm-system", all_packages);
        assert!(peers.contains(&"erp-system".to_string()));
    }
}
