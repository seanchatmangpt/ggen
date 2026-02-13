//! DMAIC cycle implementation for ontology quality improvement
//!
//! DMAIC is the core methodology of Six Sigma quality improvement:
//! - **Define**: Define quality goals and defect types
//! - **Measure**: Collect baseline quality metrics
//! - **Analyze**: Identify root causes of defects
//! - **Improve**: Implement solutions to reduce defects
//! - **Control**: Maintain improvements through monitoring

use ggen_utils::error::{Error, Result};
use serde::{Deserialize, Serialize};
use std::collections::HashMap;

/// Complete DMAIC cycle for ontology quality
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct DmaicCycle {
    pub id: String,
    pub target_ontology: String,
    pub define: DefinePhase,
    pub measure: Option<MeasurePhase>,
    pub analyze: Option<AnalyzePhase>,
    pub improve: Option<ImprovePhase>,
    pub control: Option<ControlPhase>,
    pub current_phase: DmaicPhase,
    pub created_at: chrono::DateTime<chrono::Utc>,
    pub updated_at: chrono::DateTime<chrono::Utc>,
}

/// Current phase in the DMAIC cycle
#[derive(Debug, Clone, Copy, Serialize, Deserialize, PartialEq)]
pub enum DmaicPhase {
    Define,
    Measure,
    Analyze,
    Improve,
    Control,
    Completed,
}

/// Define phase: Establish quality goals and critical-to-quality (CTQ) characteristics
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct DefinePhase {
    /// Critical-to-Quality characteristics for RDF ontology
    pub ctq_characteristics: Vec<CtqCharacteristic>,
    /// Target sigma level (typically 3σ to 6σ)
    pub target_sigma_level: f64,
    /// Maximum acceptable DPMO
    pub target_dpmo: f64,
    /// Defect definitions
    pub defect_types: Vec<DefectType>,
    /// Project charter
    pub charter: ProjectCharter,
}

/// Critical-to-Quality characteristic
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct CtqCharacteristic {
    pub name: String,
    pub description: String,
    pub specification_limits: SpecificationLimits,
    pub importance: Importance,
}

/// Specification limits for a CTQ characteristic
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct SpecificationLimits {
    pub lower: Option<f64>,
    pub upper: Option<f64>,
    pub target: f64,
}

/// Importance level of CTQ characteristic
#[derive(Debug, Clone, Copy, Serialize, Deserialize, PartialEq)]
pub enum Importance {
    Critical,    // Must meet specification
    Major,       // Should meet specification
    Minor,       // Nice to meet specification
}

/// Defect type definition
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct DefectType {
    pub code: String,
    pub name: String,
    pub description: String,
    pub category: DefectCategory,
    pub severity: DefectSeverity,
}

/// Categories of RDF ontology defects
#[derive(Debug, Clone, Copy, Serialize, Deserialize, PartialEq)]
pub enum DefectCategory {
    /// Syntactic errors (invalid RDF/Turtle)
    Syntax,
    /// Semantic inconsistencies
    Semantic,
    /// Schema violations
    Schema,
    /// Data quality issues
    DataQuality,
    /// Performance issues
    Performance,
    /// Completeness issues
    Completeness,
}

/// Severity of defects
#[derive(Debug, Clone, Copy, Serialize, Deserialize, PartialEq, Eq, PartialOrd, Ord)]
pub enum DefectSeverity {
    Critical = 4,
    Major = 3,
    Minor = 2,
    Trivial = 1,
}

/// Project charter for DMAIC cycle
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ProjectCharter {
    pub problem_statement: String,
    pub goal_statement: String,
    pub scope: String,
    pub stakeholders: Vec<String>,
    pub timeline_days: u32,
}

/// Measure phase: Collect baseline quality data
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct MeasurePhase {
    /// Baseline metrics collected
    pub baseline_metrics: HashMap<String, f64>,
    /// Total opportunities for defects
    pub total_opportunities: u64,
    /// Total defects found
    pub total_defects: u64,
    /// Baseline DPMO
    pub baseline_dpmo: f64,
    /// Baseline sigma level
    pub baseline_sigma: f64,
    /// Measurement system analysis results
    pub msa: MeasurementSystemAnalysis,
    pub samples_collected: u64,
    pub collection_period_days: u32,
}

/// Measurement System Analysis to ensure measurement reliability
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct MeasurementSystemAnalysis {
    /// Repeatability (equipment variation)
    pub repeatability_variance: f64,
    /// Reproducibility (operator variation)
    pub reproducibility_variance: f64,
    /// Gage R&R percentage
    pub gage_rr_percent: f64,
    /// Is measurement system acceptable (<30% is good)
    pub is_acceptable: bool,
}

/// Analyze phase: Identify root causes
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct AnalyzePhase {
    /// Root causes identified
    pub root_causes: Vec<RootCause>,
    /// Pareto analysis results (80/20 rule)
    pub pareto_analysis: ParetoAnalysis,
    /// Correlation analysis between factors
    pub correlations: Vec<Correlation>,
    /// Hypothesis tests conducted
    pub hypothesis_tests: Vec<HypothesisTest>,
}

/// Root cause identified through analysis
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct RootCause {
    pub id: String,
    pub description: String,
    pub category: RootCauseCategory,
    /// Percentage of defects attributed to this cause
    pub contribution_percent: f64,
    /// Evidence supporting this root cause
    pub evidence: Vec<String>,
}

/// Categories of root causes (5M+E framework)
#[derive(Debug, Clone, Copy, Serialize, Deserialize, PartialEq)]
pub enum RootCauseCategory {
    /// Man/People (human factors)
    People,
    /// Machine/Technology
    Technology,
    /// Material/Input data
    Material,
    /// Method/Process
    Method,
    /// Measurement
    Measurement,
    /// Environment
    Environment,
}

/// Pareto analysis to identify vital few vs trivial many
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ParetoAnalysis {
    /// Defect categories sorted by frequency
    pub categories: Vec<ParetoCategory>,
    /// Categories contributing to 80% of defects
    pub vital_few: Vec<String>,
}

/// Category in Pareto analysis
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ParetoCategory {
    pub name: String,
    pub count: u64,
    pub percentage: f64,
    pub cumulative_percentage: f64,
}

/// Correlation between variables
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Correlation {
    pub variable_x: String,
    pub variable_y: String,
    /// Pearson correlation coefficient (-1 to 1)
    pub correlation_coefficient: f64,
    /// Statistical significance (p-value)
    pub p_value: f64,
    pub is_significant: bool,
}

/// Statistical hypothesis test
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct HypothesisTest {
    pub test_name: String,
    pub null_hypothesis: String,
    pub alternative_hypothesis: String,
    pub test_statistic: f64,
    pub p_value: f64,
    pub significance_level: f64,
    pub reject_null: bool,
    pub conclusion: String,
}

/// Improve phase: Implement solutions
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ImprovePhase {
    /// Solutions implemented
    pub solutions: Vec<Solution>,
    /// Pilot test results
    pub pilot_results: PilotResults,
    /// Expected improvement
    pub expected_improvement_percent: f64,
}

/// Solution to address root causes
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Solution {
    pub id: String,
    pub description: String,
    /// Root causes this solution addresses
    pub addresses_causes: Vec<String>,
    pub implementation_status: ImplementationStatus,
    pub cost: Option<f64>,
    pub timeline_days: u32,
}

/// Implementation status of solution
#[derive(Debug, Clone, Copy, Serialize, Deserialize, PartialEq)]
pub enum ImplementationStatus {
    Proposed,
    InProgress,
    Piloting,
    Implemented,
    Validated,
}

/// Results from pilot testing
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct PilotResults {
    pub pilot_dpmo: f64,
    pub pilot_sigma: f64,
    pub improvement_percent: f64,
    pub validation_samples: u64,
    pub is_successful: bool,
}

/// Control phase: Sustain improvements
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ControlPhase {
    /// Control plan to maintain improvements
    pub control_plan: ControlPlan,
    /// Control charts for monitoring
    pub control_charts: Vec<String>, // References to SPC chart IDs
    /// Process documentation
    pub documentation: ProcessDocumentation,
    /// Current DPMO after improvements
    pub current_dpmo: f64,
    /// Current sigma level
    pub current_sigma: f64,
}

/// Control plan to sustain improvements
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ControlPlan {
    pub process_steps: Vec<ProcessStep>,
    pub monitoring_frequency: MonitoringFrequency,
    pub response_plan: Vec<ResponseAction>,
}

/// Process step in control plan
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ProcessStep {
    pub step_number: u32,
    pub description: String,
    pub inputs: Vec<String>,
    pub outputs: Vec<String>,
    pub controls: Vec<String>,
    pub metrics: Vec<String>,
}

/// Frequency of monitoring
#[derive(Debug, Clone, Copy, Serialize, Deserialize, PartialEq)]
pub enum MonitoringFrequency {
    Continuous,
    Hourly,
    Daily,
    Weekly,
    Monthly,
}

/// Response action when control limits are breached
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ResponseAction {
    pub trigger: String,
    pub action: String,
    pub responsible: String,
    pub escalation_hours: u32,
}

/// Process documentation
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ProcessDocumentation {
    pub standard_operating_procedures: Vec<String>,
    pub training_materials: Vec<String>,
    pub lessons_learned: Vec<String>,
    pub best_practices: Vec<String>,
}

impl DmaicCycle {
    /// Create new DMAIC cycle in Define phase
    pub fn new(id: String, target_ontology: String, define: DefinePhase) -> Self {
        let now = chrono::Utc::now();
        Self {
            id,
            target_ontology,
            define,
            measure: None,
            analyze: None,
            improve: None,
            control: None,
            current_phase: DmaicPhase::Define,
            created_at: now,
            updated_at: now,
        }
    }

    /// Advance to Measure phase
    pub fn start_measure(&mut self, measure: MeasurePhase) -> Result<()> {
        if self.current_phase != DmaicPhase::Define {
            return Err(Error::new("Can only start Measure phase from Define phase"));
        }
        self.measure = Some(measure);
        self.current_phase = DmaicPhase::Measure;
        self.updated_at = chrono::Utc::now();
        Ok(())
    }

    /// Advance to Analyze phase
    pub fn start_analyze(&mut self, analyze: AnalyzePhase) -> Result<()> {
        if self.current_phase != DmaicPhase::Measure {
            return Err(Error::new("Can only start Analyze phase from Measure phase"));
        }
        if self.measure.is_none() {
            return Err(Error::new("Measure phase must be completed first"));
        }
        self.analyze = Some(analyze);
        self.current_phase = DmaicPhase::Analyze;
        self.updated_at = chrono::Utc::now();
        Ok(())
    }

    /// Advance to Improve phase
    pub fn start_improve(&mut self, improve: ImprovePhase) -> Result<()> {
        if self.current_phase != DmaicPhase::Analyze {
            return Err(Error::new("Can only start Improve phase from Analyze phase"));
        }
        if self.analyze.is_none() {
            return Err(Error::new("Analyze phase must be completed first"));
        }
        self.improve = Some(improve);
        self.current_phase = DmaicPhase::Improve;
        self.updated_at = chrono::Utc::now();
        Ok(())
    }

    /// Advance to Control phase
    pub fn start_control(&mut self, control: ControlPhase) -> Result<()> {
        if self.current_phase != DmaicPhase::Improve {
            return Err(Error::new("Can only start Control phase from Improve phase"));
        }
        if self.improve.is_none() {
            return Err(Error::new("Improve phase must be completed first"));
        }
        self.control = Some(control);
        self.current_phase = DmaicPhase::Control;
        self.updated_at = chrono::Utc::now();
        Ok(())
    }

    /// Complete the DMAIC cycle
    pub fn complete(&mut self) -> Result<()> {
        if self.current_phase != DmaicPhase::Control {
            return Err(Error::new("Can only complete from Control phase"));
        }
        if self.control.is_none() {
            return Err(Error::new("Control phase must be completed first"));
        }
        self.current_phase = DmaicPhase::Completed;
        self.updated_at = chrono::Utc::now();
        Ok(())
    }

    /// Calculate overall improvement from baseline to current
    pub fn calculate_improvement(&self) -> Result<f64> {
        let baseline_dpmo = self
            .measure
            .as_ref()
            .ok_or_else(|| Error::new("Measure phase not completed"))?
            .baseline_dpmo;

        let current_dpmo = self
            .control
            .as_ref()
            .ok_or_else(|| Error::new("Control phase not completed"))?
            .current_dpmo;

        Ok(((baseline_dpmo - current_dpmo) / baseline_dpmo) * 100.0)
    }
}

impl DefinePhase {
    /// Create standard defect types for RDF ontology quality
    pub fn standard_rdf_defects() -> Vec<DefectType> {
        vec![
            DefectType {
                code: "SYN001".to_string(),
                name: "Invalid Turtle Syntax".to_string(),
                description: "RDF/Turtle syntax errors preventing parsing".to_string(),
                category: DefectCategory::Syntax,
                severity: DefectSeverity::Critical,
            },
            DefectType {
                code: "SEM001".to_string(),
                name: "Semantic Inconsistency".to_string(),
                description: "Logical contradictions in ontology".to_string(),
                category: DefectCategory::Semantic,
                severity: DefectSeverity::Major,
            },
            DefectType {
                code: "SCH001".to_string(),
                name: "SHACL Violation".to_string(),
                description: "Violation of SHACL shape constraints".to_string(),
                category: DefectCategory::Schema,
                severity: DefectSeverity::Major,
            },
            DefectType {
                code: "DQ001".to_string(),
                name: "Missing Required Property".to_string(),
                description: "Required property is missing from resource".to_string(),
                category: DefectCategory::DataQuality,
                severity: DefectSeverity::Major,
            },
            DefectType {
                code: "DQ002".to_string(),
                name: "Invalid Property Value".to_string(),
                description: "Property value doesn't match expected type or format".to_string(),
                category: DefectCategory::DataQuality,
                severity: DefectSeverity::Minor,
            },
            DefectType {
                code: "COM001".to_string(),
                name: "Incomplete Metadata".to_string(),
                description: "Recommended metadata fields are missing".to_string(),
                category: DefectCategory::Completeness,
                severity: DefectSeverity::Minor,
            },
            DefectType {
                code: "PERF001".to_string(),
                name: "Inefficient Query Pattern".to_string(),
                description: "RDF structure causes inefficient SPARQL queries".to_string(),
                category: DefectCategory::Performance,
                severity: DefectSeverity::Minor,
            },
        ]
    }

    /// Create standard CTQ characteristics for RDF templates
    pub fn standard_rdf_ctqs() -> Vec<CtqCharacteristic> {
        vec![
            CtqCharacteristic {
                name: "Syntactic Correctness".to_string(),
                description: "Percentage of templates with valid RDF/Turtle syntax".to_string(),
                specification_limits: SpecificationLimits {
                    lower: Some(99.5),
                    upper: None,
                    target: 100.0,
                },
                importance: Importance::Critical,
            },
            CtqCharacteristic {
                name: "SHACL Compliance".to_string(),
                description: "Percentage of templates passing SHACL validation".to_string(),
                specification_limits: SpecificationLimits {
                    lower: Some(95.0),
                    upper: None,
                    target: 100.0,
                },
                importance: Importance::Critical,
            },
            CtqCharacteristic {
                name: "Semantic Consistency".to_string(),
                description: "Percentage of templates with no logical contradictions".to_string(),
                specification_limits: SpecificationLimits {
                    lower: Some(98.0),
                    upper: None,
                    target: 100.0,
                },
                importance: Importance::Major,
            },
            CtqCharacteristic {
                name: "Metadata Completeness".to_string(),
                description: "Percentage of recommended metadata fields present".to_string(),
                specification_limits: SpecificationLimits {
                    lower: Some(80.0),
                    upper: None,
                    target: 95.0,
                },
                importance: Importance::Minor,
            },
        ]
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_dmaic_cycle_progression() {
        let define = DefinePhase {
            ctq_characteristics: DefinePhase::standard_rdf_ctqs(),
            target_sigma_level: 4.0,
            target_dpmo: 6210.0,
            defect_types: DefinePhase::standard_rdf_defects(),
            charter: ProjectCharter {
                problem_statement: "High RDF defect rate".to_string(),
                goal_statement: "Achieve 4σ quality".to_string(),
                scope: "All template metadata".to_string(),
                stakeholders: vec!["Quality Team".to_string()],
                timeline_days: 90,
            },
        };

        let mut cycle = DmaicCycle::new(
            "test-cycle-1".to_string(),
            "http://example.org/ontology".to_string(),
            define,
        );

        assert_eq!(cycle.current_phase, DmaicPhase::Define);

        // Advance to Measure
        let measure = MeasurePhase {
            baseline_metrics: HashMap::new(),
            total_opportunities: 1_000_000,
            total_defects: 10_000,
            baseline_dpmo: 10_000.0,
            baseline_sigma: 3.8,
            msa: MeasurementSystemAnalysis {
                repeatability_variance: 0.15,
                reproducibility_variance: 0.10,
                gage_rr_percent: 25.0,
                is_acceptable: true,
            },
            samples_collected: 1000,
            collection_period_days: 30,
        };

        cycle.start_measure(measure).unwrap();
        assert_eq!(cycle.current_phase, DmaicPhase::Measure);
    }

    #[test]
    fn test_defect_severity_ordering() {
        assert!(DefectSeverity::Critical > DefectSeverity::Major);
        assert!(DefectSeverity::Major > DefectSeverity::Minor);
        assert!(DefectSeverity::Minor > DefectSeverity::Trivial);
    }
}
