//! Quality Tiers for Marketplace Packages
//!
//! Implements Gold/Silver/Bronze quality tier system for package classification.
//! Quality tiers help users discover high-quality, well-maintained packages.
//!
//! # Tier Definitions (Story 3 - Search & Discover)
//!
//! - **Gold**: FMEA passed + >100 downloads + <30 days old
//! - **Silver**: FMEA passed + 10-100 downloads OR 30-90 days old
//! - **Bronze**: Basic validation only
//!
//! # Usage
//!
//! ```rust,no_run
//! use ggen_domain::marketplace::quality_tiers::{QualityTier, QualityTierCalculator};
//!
//! let calculator = QualityTierCalculator::default();
//! let tier = calculator.calculate(150, true, 15); // Gold tier
//! assert_eq!(tier, QualityTier::Gold);
//! ```

use chrono::{Duration, Utc};
use serde::{Deserialize, Serialize};
use std::fmt;

/// Quality tier classification for marketplace packages
///
/// Tiers are ordered from highest to lowest quality:
/// - Gold > Silver > Bronze > Unrated
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize)]
#[serde(rename_all = "lowercase")]
pub enum QualityTier {
    /// Highest quality - FMEA passed, high downloads, recently updated
    Gold,
    /// Good quality - FMEA passed with moderate activity
    Silver,
    /// Basic quality - passes basic validation
    Bronze,
    /// Not yet rated or doesn't meet Bronze criteria
    Unrated,
}

impl QualityTier {
    /// Get display symbol for the tier
    pub fn symbol(&self) -> &'static str {
        match self {
            QualityTier::Gold => "🥇",
            QualityTier::Silver => "🥈",
            QualityTier::Bronze => "🥉",
            QualityTier::Unrated => "○",
        }
    }

    /// Get tier as a numeric priority (lower = higher priority for sorting)
    pub fn priority(&self) -> u8 {
        match self {
            QualityTier::Gold => 0,
            QualityTier::Silver => 1,
            QualityTier::Bronze => 2,
            QualityTier::Unrated => 3,
        }
    }

    /// Check if this tier is at least as good as the given tier
    pub fn is_at_least(&self, other: QualityTier) -> bool {
        self.priority() <= other.priority()
    }

    /// Parse tier from string (case-insensitive)
    pub fn from_str_loose(s: &str) -> Self {
        match s.to_lowercase().as_str() {
            "gold" => QualityTier::Gold,
            "silver" => QualityTier::Silver,
            "bronze" => QualityTier::Bronze,
            _ => QualityTier::Unrated,
        }
    }
}

impl fmt::Display for QualityTier {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            QualityTier::Gold => write!(f, "Gold"),
            QualityTier::Silver => write!(f, "Silver"),
            QualityTier::Bronze => write!(f, "Bronze"),
            QualityTier::Unrated => write!(f, "Unrated"),
        }
    }
}

impl Default for QualityTier {
    fn default() -> Self {
        QualityTier::Unrated
    }
}

/// Thresholds for quality tier calculation
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct QualityThresholds {
    /// Minimum downloads for Gold tier
    pub gold_min_downloads: u32,
    /// Maximum age in days for Gold tier
    pub gold_max_age_days: u32,
    /// Minimum downloads for Silver tier
    pub silver_min_downloads: u32,
    /// Maximum age in days for Silver tier (lower bound)
    pub silver_max_age_days: u32,
    /// FMEA pass required for Gold
    pub gold_requires_fmea: bool,
    /// FMEA pass required for Silver
    pub silver_requires_fmea: bool,
}

impl Default for QualityThresholds {
    fn default() -> Self {
        Self {
            gold_min_downloads: 100,
            gold_max_age_days: 30,
            silver_min_downloads: 10,
            silver_max_age_days: 90,
            gold_requires_fmea: true,
            silver_requires_fmea: true,
        }
    }
}

/// Quality tier calculator
///
/// Calculates quality tier based on package metrics using configurable thresholds.
#[derive(Debug, Clone)]
pub struct QualityTierCalculator {
    thresholds: QualityThresholds,
}

impl Default for QualityTierCalculator {
    fn default() -> Self {
        Self {
            thresholds: QualityThresholds::default(),
        }
    }
}

impl QualityTierCalculator {
    /// Create a new calculator with custom thresholds
    pub fn with_thresholds(thresholds: QualityThresholds) -> Self {
        Self { thresholds }
    }

    /// Calculate quality tier based on package metrics
    ///
    /// # Arguments
    /// * `downloads` - Total download count
    /// * `fmea_passed` - Whether FMEA validation passed
    /// * `age_days` - Days since last update
    ///
    /// # Returns
    /// The calculated quality tier
    pub fn calculate(&self, downloads: u32, fmea_passed: bool, age_days: u32) -> QualityTier {
        // Gold tier: FMEA passed + >100 downloads + <30 days old
        if (!self.thresholds.gold_requires_fmea || fmea_passed)
            && downloads >= self.thresholds.gold_min_downloads
            && age_days <= self.thresholds.gold_max_age_days
        {
            return QualityTier::Gold;
        }

        // Silver tier: FMEA passed + 10-100 downloads OR 30-90 days old
        if (!self.thresholds.silver_requires_fmea || fmea_passed)
            && downloads >= self.thresholds.silver_min_downloads
            && age_days <= self.thresholds.silver_max_age_days
        {
            return QualityTier::Silver;
        }

        // Bronze tier: Basic validation (any package with >0 downloads)
        if downloads > 0 {
            return QualityTier::Bronze;
        }

        // Unrated: No downloads or doesn't meet any criteria
        QualityTier::Unrated
    }

    /// Calculate tier from package info with timestamp
    ///
    /// # Arguments
    /// * `downloads` - Total download count
    /// * `fmea_passed` - Whether FMEA validation passed
    /// * `last_updated` - ISO 8601 timestamp of last update (e.g., "2024-01-15T10:30:00Z")
    ///
    /// # Returns
    /// The calculated quality tier, or Unrated if timestamp parsing fails
    pub fn calculate_with_timestamp(
        &self, downloads: u32, fmea_passed: bool, last_updated: Option<&str>,
    ) -> QualityTier {
        let age_days = match last_updated {
            Some(ts) => self.parse_age_days(ts).unwrap_or(u32::MAX),
            None => u32::MAX, // No timestamp = treat as very old
        };

        self.calculate(downloads, fmea_passed, age_days)
    }

    /// Parse age in days from ISO 8601 timestamp
    fn parse_age_days(&self, timestamp: &str) -> Option<u32> {
        // Try parsing as ISO 8601
        let parsed = chrono::DateTime::parse_from_rfc3339(timestamp)
            .ok()
            .map(|dt| dt.with_timezone(&Utc));

        if let Some(last_updated) = parsed {
            let now = Utc::now();
            let duration = now.signed_duration_since(last_updated);
            // Negative duration means future date - treat as 0 days
            if duration < Duration::zero() {
                return Some(0);
            }
            // Convert to days, clamping to u32::MAX for very old packages
            Some(duration.num_days().min(i64::from(u32::MAX)) as u32)
        } else {
            // Try parsing as date only (YYYY-MM-DD)
            if let Some(date) = chrono::NaiveDate::parse_from_str(timestamp, "%Y-%m-%d").ok() {
                let today = Utc::now().date_naive();
                let duration = today.signed_duration_since(date);
                if duration < Duration::zero() {
                    return Some(0);
                }
                return Some(duration.num_days().min(i64::from(u32::MAX)) as u32);
            }
            None
        }
    }
}

/// Package quality information
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct PackageQualityInfo {
    /// Package name
    pub name: String,
    /// Calculated quality tier
    pub tier: QualityTier,
    /// Download count
    pub downloads: u32,
    /// Whether FMEA validation passed
    pub fmea_passed: bool,
    /// Age in days since last update
    pub age_days: u32,
    /// Optional quality score (0-100)
    pub quality_score: Option<f64>,
}

impl PackageQualityInfo {
    /// Create a new quality info with calculated tier
    pub fn new(name: String, downloads: u32, fmea_passed: bool, age_days: u32) -> Self {
        let calculator = QualityTierCalculator::default();
        let tier = calculator.calculate(downloads, fmea_passed, age_days);
        let quality_score = Some(Self::compute_quality_score(downloads, fmea_passed, age_days));

        Self {
            name,
            tier,
            downloads,
            fmea_passed,
            age_days,
            quality_score,
        }
    }

    /// Compute a quality score (0-100) based on metrics
    ///
    /// Scoring breakdown:
    /// - Downloads: up to 40 points (log scale)
    /// - FMEA: 30 points if passed
    /// - Freshness: up to 30 points (inverse of age)
    fn compute_quality_score(downloads: u32, fmea_passed: bool, age_days: u32) -> f64 {
        // Downloads score (log scale, max 40 points)
        let download_score = if downloads > 0 {
            (f64::from(downloads).ln() / 10.0 * 40.0).min(40.0)
        } else {
            0.0
        };

        // FMEA score (30 points if passed)
        let fmea_score = if fmea_passed { 30.0 } else { 0.0 };

        // Freshness score (max 30 points, decreases with age)
        let freshness_score = if age_days <= 7 {
            30.0
        } else if age_days <= 30 {
            25.0
        } else if age_days <= 90 {
            15.0
        } else if age_days <= 365 {
            5.0
        } else {
            0.0
        };

        (download_score + fmea_score + freshness_score).min(100.0)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_quality_tier_ordering() {
        // Gold > Silver > Bronze > Unrated
        assert!(QualityTier::Gold < QualityTier::Silver);
        assert!(QualityTier::Silver < QualityTier::Bronze);
        assert!(QualityTier::Bronze < QualityTier::Unrated);
    }

    #[test]
    fn test_quality_tier_priority() {
        assert_eq!(QualityTier::Gold.priority(), 0);
        assert_eq!(QualityTier::Silver.priority(), 1);
        assert_eq!(QualityTier::Bronze.priority(), 2);
        assert_eq!(QualityTier::Unrated.priority(), 3);
    }

    #[test]
    fn test_quality_tier_is_at_least() {
        assert!(QualityTier::Gold.is_at_least(QualityTier::Gold));
        assert!(QualityTier::Gold.is_at_least(QualityTier::Silver));
        assert!(QualityTier::Gold.is_at_least(QualityTier::Bronze));
        assert!(!QualityTier::Bronze.is_at_least(QualityTier::Gold));
    }

    #[test]
    fn test_quality_tier_symbols() {
        assert_eq!(QualityTier::Gold.symbol(), "🥇");
        assert_eq!(QualityTier::Silver.symbol(), "🥈");
        assert_eq!(QualityTier::Bronze.symbol(), "🥉");
        assert_eq!(QualityTier::Unrated.symbol(), "○");
    }

    #[test]
    fn test_quality_tier_from_str_loose() {
        assert_eq!(QualityTier::from_str_loose("gold"), QualityTier::Gold);
        assert_eq!(QualityTier::from_str_loose("GOLD"), QualityTier::Gold);
        assert_eq!(QualityTier::from_str_loose("Silver"), QualityTier::Silver);
        assert_eq!(QualityTier::from_str_loose("BRONZE"), QualityTier::Bronze);
        assert_eq!(QualityTier::from_str_loose("unknown"), QualityTier::Unrated);
    }

    #[test]
    fn test_calculator_gold_tier() {
        let calc = QualityTierCalculator::default();

        // Gold: FMEA passed + >100 downloads + <30 days
        assert_eq!(calc.calculate(150, true, 15), QualityTier::Gold);
        assert_eq!(calc.calculate(100, true, 30), QualityTier::Gold);
        assert_eq!(calc.calculate(500, true, 0), QualityTier::Gold);
    }

    #[test]
    fn test_calculator_silver_tier() {
        let calc = QualityTierCalculator::default();

        // Silver: FMEA passed + 10-100 downloads OR 30-90 days
        assert_eq!(calc.calculate(50, true, 45), QualityTier::Silver);
        assert_eq!(calc.calculate(10, true, 90), QualityTier::Silver);
        assert_eq!(calc.calculate(20, true, 60), QualityTier::Silver);
    }

    #[test]
    fn test_calculator_bronze_tier() {
        let calc = QualityTierCalculator::default();

        // Bronze: Any downloads but doesn't meet Silver/Gold
        assert_eq!(calc.calculate(5, true, 100), QualityTier::Bronze);
        assert_eq!(calc.calculate(1, false, 365), QualityTier::Bronze);
        assert_eq!(calc.calculate(50, false, 45), QualityTier::Bronze); // No FMEA
    }

    #[test]
    fn test_calculator_unrated_tier() {
        let calc = QualityTierCalculator::default();

        // Unrated: No downloads
        assert_eq!(calc.calculate(0, true, 10), QualityTier::Unrated);
        assert_eq!(calc.calculate(0, false, 0), QualityTier::Unrated);
    }

    #[test]
    fn test_calculator_fmea_requirement() {
        let calc = QualityTierCalculator::default();

        // Without FMEA, can't get Gold or Silver (with default thresholds)
        assert_eq!(calc.calculate(200, false, 10), QualityTier::Bronze);
        assert_eq!(calc.calculate(50, false, 45), QualityTier::Bronze);
    }

    #[test]
    fn test_calculator_with_timestamp() {
        let calc = QualityTierCalculator::default();

        // Recent timestamp (should qualify for Gold with high downloads)
        let recent = Utc::now().to_rfc3339();
        assert_eq!(
            calc.calculate_with_timestamp(150, true, Some(&recent)),
            QualityTier::Gold
        );

        // Old timestamp (should be Bronze)
        assert_eq!(
            calc.calculate_with_timestamp(150, true, Some("2020-01-01T00:00:00Z")),
            QualityTier::Bronze
        );

        // No timestamp
        assert_eq!(
            calc.calculate_with_timestamp(150, true, None),
            QualityTier::Bronze
        );
    }

    #[test]
    fn test_calculator_date_only_timestamp() {
        let calc = QualityTierCalculator::default();

        // Test date-only format
        let today = Utc::now().format("%Y-%m-%d").to_string();
        assert_eq!(
            calc.calculate_with_timestamp(150, true, Some(&today)),
            QualityTier::Gold
        );
    }

    #[test]
    fn test_package_quality_info() {
        let info = PackageQualityInfo::new("test-package".to_string(), 150, true, 10);

        assert_eq!(info.name, "test-package");
        assert_eq!(info.tier, QualityTier::Gold);
        assert_eq!(info.downloads, 150);
        assert!(info.fmea_passed);
        assert_eq!(info.age_days, 10);
        assert!(info.quality_score.is_some());
        assert!(info.quality_score.unwrap() > 50.0); // Should have decent score
    }

    #[test]
    fn test_quality_score_computation() {
        // High quality package
        let high = PackageQualityInfo::new("high".to_string(), 1000, true, 5);
        assert!(high.quality_score.unwrap() > 80.0);

        // Medium quality package
        let medium = PackageQualityInfo::new("medium".to_string(), 50, true, 45);
        let medium_score = medium.quality_score.unwrap();
        assert!(medium_score > 40.0 && medium_score < 80.0);

        // Low quality package
        let low = PackageQualityInfo::new("low".to_string(), 1, false, 400);
        assert!(low.quality_score.unwrap() < 30.0);
    }

    #[test]
    fn test_quality_tier_display() {
        assert_eq!(format!("{}", QualityTier::Gold), "Gold");
        assert_eq!(format!("{}", QualityTier::Silver), "Silver");
        assert_eq!(format!("{}", QualityTier::Bronze), "Bronze");
        assert_eq!(format!("{}", QualityTier::Unrated), "Unrated");
    }

    #[test]
    fn test_custom_thresholds() {
        let thresholds = QualityThresholds {
            gold_min_downloads: 50, // Lower threshold
            gold_max_age_days: 60,  // More lenient
            silver_min_downloads: 5,
            silver_max_age_days: 180,
            gold_requires_fmea: false, // Don't require FMEA
            silver_requires_fmea: false,
        };
        let calc = QualityTierCalculator::with_thresholds(thresholds);

        // With relaxed thresholds, this should be Gold
        assert_eq!(calc.calculate(50, false, 60), QualityTier::Gold);
    }

    #[test]
    fn test_quality_tier_serialization() {
        let tier = QualityTier::Gold;
        let json = serde_json::to_string(&tier).unwrap();
        assert_eq!(json, r#""gold""#);

        let deserialized: QualityTier = serde_json::from_str(&json).unwrap();
        assert_eq!(deserialized, QualityTier::Gold);
    }
}
