//! DPMO (Defects Per Million Opportunities) tracking across semantic transformations
//!
//! DPMO is the primary metric in Six Sigma for measuring process quality.
//! This module tracks defects across various stages of RDF ontology generation
//! and semantic transformations.

use ggen_utils::error::{Error, Result};
use serde::{Deserialize, Serialize};
use std::collections::HashMap;

/// DPMO calculator and tracker
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct DpmoCalculator {
    /// Defect tracking records
    pub records: Vec<DefectRecord>,
    /// Summary statistics
    pub summary: DpmoSummary,
}

/// Individual defect tracking record
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct DefectRecord {
    pub id: String,
    pub timestamp: chrono::DateTime<chrono::Utc>,
    pub transformation_stage: TransformationStage,
    pub opportunity_type: OpportunityType,
    pub units_produced: u64,
    pub opportunities_per_unit: u64,
    pub total_opportunities: u64,
    pub defects_found: u64,
    pub dpmo: f64,
    pub sigma_level: f64,
}

/// Stages of semantic transformation
#[derive(Debug, Clone, Copy, Serialize, Deserialize, PartialEq, Eq, Hash)]
pub enum TransformationStage {
    /// Template parsing stage
    Parsing,
    /// RDF/Turtle generation
    Generation,
    /// SHACL validation
    Validation,
    /// Semantic reasoning
    Reasoning,
    /// SPARQL query execution
    Querying,
    /// Serialization to output format
    Serialization,
    /// End-to-end transformation
    EndToEnd,
}

/// Types of opportunities for defects
#[derive(Debug, Clone, Copy, Serialize, Deserialize, PartialEq, Eq, Hash)]
pub enum OpportunityType {
    /// Syntax correctness opportunity
    Syntax,
    /// Schema compliance opportunity
    Schema,
    /// Semantic consistency opportunity
    Semantic,
    /// Data quality opportunity
    DataQuality,
    /// Performance requirement opportunity
    Performance,
    /// Completeness opportunity
    Completeness,
}

/// Summary statistics for DPMO tracking
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct DpmoSummary {
    pub total_units: u64,
    pub total_opportunities: u64,
    pub total_defects: u64,
    pub overall_dpmo: f64,
    pub overall_sigma: f64,
    pub by_stage: HashMap<TransformationStage, StageSummary>,
    pub by_opportunity: HashMap<OpportunityType, OpportunitySummary>,
    pub trend_analysis: TrendAnalysis,
}

/// Summary for a specific transformation stage
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct StageSummary {
    pub stage: TransformationStage,
    pub units: u64,
    pub opportunities: u64,
    pub defects: u64,
    pub dpmo: f64,
    pub sigma_level: f64,
    pub defect_rate_percent: f64,
}

/// Summary for a specific opportunity type
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct OpportunitySummary {
    pub opportunity_type: OpportunityType,
    pub opportunities: u64,
    pub defects: u64,
    pub dpmo: f64,
    pub contribution_percent: f64,
}

/// Trend analysis over time
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct TrendAnalysis {
    pub start_date: Option<chrono::DateTime<chrono::Utc>>,
    pub end_date: Option<chrono::DateTime<chrono::Utc>>,
    pub data_points: u64,
    pub trend_direction: TrendDirection,
    pub improvement_rate: f64, // DPMO reduction per day
    pub is_improving: bool,
}

/// Direction of trend
#[derive(Debug, Clone, Copy, Serialize, Deserialize, PartialEq)]
pub enum TrendDirection {
    Improving,
    Stable,
    Degrading,
}

/// Defect tracker for continuous monitoring
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct DefectTracker {
    calculator: DpmoCalculator,
    targets: QualityTargets,
}

/// Quality targets for monitoring
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct QualityTargets {
    pub target_dpmo: f64,
    pub target_sigma: f64,
    pub alert_threshold_dpmo: f64,
    pub critical_threshold_dpmo: f64,
}

impl DpmoCalculator {
    /// Create new DPMO calculator
    pub fn new() -> Self {
        Self {
            records: Vec::new(),
            summary: DpmoSummary {
                total_units: 0,
                total_opportunities: 0,
                total_defects: 0,
                overall_dpmo: 0.0,
                overall_sigma: 0.0,
                by_stage: HashMap::new(),
                by_opportunity: HashMap::new(),
                trend_analysis: TrendAnalysis {
                    start_date: None,
                    end_date: None,
                    data_points: 0,
                    trend_direction: TrendDirection::Stable,
                    improvement_rate: 0.0,
                    is_improving: false,
                },
            },
        }
    }

    /// Record a defect measurement
    pub fn record_defects(
        &mut self,
        id: String,
        stage: TransformationStage,
        opportunity_type: OpportunityType,
        units_produced: u64,
        opportunities_per_unit: u64,
        defects_found: u64,
    ) -> Result<DefectRecord> {
        if units_produced == 0 {
            return Err(Error::new("Units produced must be greater than 0"));
        }

        if opportunities_per_unit == 0 {
            return Err(Error::new("Opportunities per unit must be greater than 0"));
        }

        let total_opportunities = units_produced * opportunities_per_unit;

        // Calculate DPMO
        let dpmo = if total_opportunities > 0 {
            (defects_found as f64 / total_opportunities as f64) * 1_000_000.0
        } else {
            0.0
        };

        // Calculate sigma level from DPMO
        let sigma_level = Self::dpmo_to_sigma(dpmo);

        let record = DefectRecord {
            id,
            timestamp: chrono::Utc::now(),
            transformation_stage: stage,
            opportunity_type,
            units_produced,
            opportunities_per_unit,
            total_opportunities,
            defects_found,
            dpmo,
            sigma_level,
        };

        self.records.push(record.clone());
        self.update_summary();

        Ok(record)
    }

    /// Convert DPMO to approximate sigma level
    fn dpmo_to_sigma(dpmo: f64) -> f64 {
        // Approximate conversion using standard Six Sigma tables
        // Accounts for 1.5σ shift
        match dpmo {
            d if d <= 3.4 => 6.0,
            d if d <= 233.0 => 5.0 + (233.0 - d) / (233.0 - 3.4) * 1.0,
            d if d <= 6_210.0 => 4.0 + (6_210.0 - d) / (6_210.0 - 233.0) * 1.0,
            d if d <= 66_807.0 => 3.0 + (66_807.0 - d) / (66_807.0 - 6_210.0) * 1.0,
            d if d <= 308_538.0 => 2.0 + (308_538.0 - d) / (308_538.0 - 66_807.0) * 1.0,
            d if d <= 691_462.0 => 1.0 + (691_462.0 - d) / (691_462.0 - 308_538.0) * 1.0,
            _ => 1.0,
        }
    }

    /// Update summary statistics
    fn update_summary(&mut self) {
        // Calculate overall totals
        self.summary.total_units = self.records.iter().map(|r| r.units_produced).sum();
        self.summary.total_opportunities = self.records.iter().map(|r| r.total_opportunities).sum();
        self.summary.total_defects = self.records.iter().map(|r| r.defects_found).sum();

        // Calculate overall DPMO
        self.summary.overall_dpmo = if self.summary.total_opportunities > 0 {
            (self.summary.total_defects as f64 / self.summary.total_opportunities as f64)
                * 1_000_000.0
        } else {
            0.0
        };

        // Calculate overall sigma
        self.summary.overall_sigma = Self::dpmo_to_sigma(self.summary.overall_dpmo);

        // Summary by stage
        self.summary.by_stage.clear();
        for stage in [
            TransformationStage::Parsing,
            TransformationStage::Generation,
            TransformationStage::Validation,
            TransformationStage::Reasoning,
            TransformationStage::Querying,
            TransformationStage::Serialization,
            TransformationStage::EndToEnd,
        ] {
            let stage_records: Vec<&DefectRecord> = self
                .records
                .iter()
                .filter(|r| r.transformation_stage == stage)
                .collect();

            if !stage_records.is_empty() {
                let units: u64 = stage_records.iter().map(|r| r.units_produced).sum();
                let opportunities: u64 = stage_records.iter().map(|r| r.total_opportunities).sum();
                let defects: u64 = stage_records.iter().map(|r| r.defects_found).sum();

                let dpmo = if opportunities > 0 {
                    (defects as f64 / opportunities as f64) * 1_000_000.0
                } else {
                    0.0
                };

                let defect_rate_percent = if opportunities > 0 {
                    (defects as f64 / opportunities as f64) * 100.0
                } else {
                    0.0
                };

                self.summary.by_stage.insert(
                    stage,
                    StageSummary {
                        stage,
                        units,
                        opportunities,
                        defects,
                        dpmo,
                        sigma_level: Self::dpmo_to_sigma(dpmo),
                        defect_rate_percent,
                    },
                );
            }
        }

        // Summary by opportunity type
        self.summary.by_opportunity.clear();
        for opp_type in [
            OpportunityType::Syntax,
            OpportunityType::Schema,
            OpportunityType::Semantic,
            OpportunityType::DataQuality,
            OpportunityType::Performance,
            OpportunityType::Completeness,
        ] {
            let opp_records: Vec<&DefectRecord> = self
                .records
                .iter()
                .filter(|r| r.opportunity_type == opp_type)
                .collect();

            if !opp_records.is_empty() {
                let opportunities: u64 = opp_records.iter().map(|r| r.total_opportunities).sum();
                let defects: u64 = opp_records.iter().map(|r| r.defects_found).sum();

                let dpmo = if opportunities > 0 {
                    (defects as f64 / opportunities as f64) * 1_000_000.0
                } else {
                    0.0
                };

                let contribution_percent = if self.summary.total_defects > 0 {
                    (defects as f64 / self.summary.total_defects as f64) * 100.0
                } else {
                    0.0
                };

                self.summary.by_opportunity.insert(
                    opp_type,
                    OpportunitySummary {
                        opportunity_type: opp_type,
                        opportunities,
                        defects,
                        dpmo,
                        contribution_percent,
                    },
                );
            }
        }

        // Update trend analysis
        self.update_trend_analysis();
    }

    /// Update trend analysis
    fn update_trend_analysis(&mut self) {
        if self.records.len() < 2 {
            return;
        }

        let mut sorted_records = self.records.clone();
        sorted_records.sort_by(|a, b| a.timestamp.cmp(&b.timestamp));

        self.summary.trend_analysis.start_date = sorted_records.first().map(|r| r.timestamp);
        self.summary.trend_analysis.end_date = sorted_records.last().map(|r| r.timestamp);
        self.summary.trend_analysis.data_points = sorted_records.len() as u64;

        // Simple linear regression on DPMO over time
        if let (Some(start), Some(end)) = (
            self.summary.trend_analysis.start_date,
            self.summary.trend_analysis.end_date,
        ) {
            let duration_days = (end - start).num_days() as f64;
            if duration_days > 0.0 {
                let first_dpmo = sorted_records.first().unwrap().dpmo;
                let last_dpmo = sorted_records.last().unwrap().dpmo;

                self.summary.trend_analysis.improvement_rate =
                    (first_dpmo - last_dpmo) / duration_days;

                self.summary.trend_analysis.is_improving =
                    self.summary.trend_analysis.improvement_rate > 0.0;

                self.summary.trend_analysis.trend_direction =
                    if self.summary.trend_analysis.improvement_rate > 100.0 {
                        TrendDirection::Improving
                    } else if self.summary.trend_analysis.improvement_rate < -100.0 {
                        TrendDirection::Degrading
                    } else {
                        TrendDirection::Stable
                    };
            }
        }
    }

    /// Get records for a specific stage
    pub fn get_stage_records(&self, stage: TransformationStage) -> Vec<&DefectRecord> {
        self.records
            .iter()
            .filter(|r| r.transformation_stage == stage)
            .collect()
    }

    /// Get records for a specific opportunity type
    pub fn get_opportunity_records(&self, opp_type: OpportunityType) -> Vec<&DefectRecord> {
        self.records
            .iter()
            .filter(|r| r.opportunity_type == opp_type)
            .collect()
    }
}

impl Default for DpmoCalculator {
    fn default() -> Self {
        Self::new()
    }
}

impl DefectTracker {
    /// Create new defect tracker with quality targets
    pub fn new(targets: QualityTargets) -> Self {
        Self {
            calculator: DpmoCalculator::new(),
            targets,
        }
    }

    /// Create tracker with Six Sigma targets (3.4 DPMO, 6σ)
    pub fn with_six_sigma_targets() -> Self {
        Self::new(QualityTargets {
            target_dpmo: 3.4,
            target_sigma: 6.0,
            alert_threshold_dpmo: 233.0,    // 5σ
            critical_threshold_dpmo: 6_210.0, // 4σ
        })
    }

    /// Create tracker with industry standard targets (6,210 DPMO, 4σ)
    pub fn with_industry_targets() -> Self {
        Self::new(QualityTargets {
            target_dpmo: 6_210.0,
            target_sigma: 4.0,
            alert_threshold_dpmo: 66_807.0,     // 3σ
            critical_threshold_dpmo: 308_538.0, // 2σ
        })
    }

    /// Record defects and check against targets
    pub fn track_defects(
        &mut self,
        id: String,
        stage: TransformationStage,
        opportunity_type: OpportunityType,
        units_produced: u64,
        opportunities_per_unit: u64,
        defects_found: u64,
    ) -> Result<QualityStatus> {
        let record = self.calculator.record_defects(
            id,
            stage,
            opportunity_type,
            units_produced,
            opportunities_per_unit,
            defects_found,
        )?;

        // Determine quality status
        let status = if record.dpmo <= self.targets.target_dpmo {
            QualityStatus::OnTarget
        } else if record.dpmo <= self.targets.alert_threshold_dpmo {
            QualityStatus::Alert
        } else if record.dpmo <= self.targets.critical_threshold_dpmo {
            QualityStatus::Critical
        } else {
            QualityStatus::Unacceptable
        };

        Ok(status)
    }

    /// Get current quality status
    pub fn get_status(&self) -> QualityStatus {
        let dpmo = self.calculator.summary.overall_dpmo;

        if dpmo <= self.targets.target_dpmo {
            QualityStatus::OnTarget
        } else if dpmo <= self.targets.alert_threshold_dpmo {
            QualityStatus::Alert
        } else if dpmo <= self.targets.critical_threshold_dpmo {
            QualityStatus::Critical
        } else {
            QualityStatus::Unacceptable
        }
    }

    /// Get the calculator
    pub fn calculator(&self) -> &DpmoCalculator {
        &self.calculator
    }
}

/// Quality status relative to targets
#[derive(Debug, Clone, Copy, Serialize, Deserialize, PartialEq)]
pub enum QualityStatus {
    /// DPMO at or below target
    OnTarget,
    /// DPMO above target but below alert threshold
    Alert,
    /// DPMO above alert threshold but below critical threshold
    Critical,
    /// DPMO above critical threshold
    Unacceptable,
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_dpmo_calculation() {
        let mut calculator = DpmoCalculator::new();

        let result = calculator.record_defects(
            "test-1".to_string(),
            TransformationStage::Parsing,
            OpportunityType::Syntax,
            1000,  // units
            10,    // opportunities per unit
            5,     // defects
        );

        assert!(result.is_ok());
        let record = result.unwrap();
        assert_eq!(record.total_opportunities, 10_000);
        assert_eq!(record.defects_found, 5);
        assert_eq!(record.dpmo, 500.0); // (5/10000) * 1,000,000
        assert!(record.sigma_level > 4.0); // 500 DPMO is > 4σ
    }

    #[test]
    fn test_sigma_conversion() {
        assert!(DpmoCalculator::dpmo_to_sigma(3.4) >= 5.9);
        assert!(DpmoCalculator::dpmo_to_sigma(233.0) >= 4.9);
        assert!(DpmoCalculator::dpmo_to_sigma(6_210.0) >= 3.9);
    }

    #[test]
    fn test_defect_tracker_status() {
        let mut tracker = DefectTracker::with_six_sigma_targets();

        // Record low defect rate (should be OnTarget)
        let status = tracker
            .track_defects(
                "test-1".to_string(),
                TransformationStage::Generation,
                OpportunityType::Schema,
                1_000_000,
                1,
                2, // 2 DPMO
            )
            .unwrap();

        assert_eq!(status, QualityStatus::OnTarget);
    }

    #[test]
    fn test_stage_summary() {
        let mut calculator = DpmoCalculator::new();

        calculator
            .record_defects(
                "test-1".to_string(),
                TransformationStage::Parsing,
                OpportunityType::Syntax,
                1000,
                10,
                5,
            )
            .unwrap();

        calculator
            .record_defects(
                "test-2".to_string(),
                TransformationStage::Parsing,
                OpportunityType::Syntax,
                1000,
                10,
                3,
            )
            .unwrap();

        assert!(calculator.summary.by_stage.contains_key(&TransformationStage::Parsing));
        let parsing_summary = &calculator.summary.by_stage[&TransformationStage::Parsing];
        assert_eq!(parsing_summary.units, 2000);
        assert_eq!(parsing_summary.defects, 8);
    }
}
