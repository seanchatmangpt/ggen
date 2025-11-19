//! Black Belt training data for AI quality agents
//!
//! This module provides structured training data and knowledge management
//! for AI agents that perform Six Sigma quality assurance on RDF ontologies.

use ggen_utils::error::{Error, Result};
use serde::{Deserialize, Serialize};
use std::collections::HashMap;

/// Black Belt training dataset for AI quality agents
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct BlackBeltData {
    pub version: String,
    pub training_examples: Vec<TrainingExample>,
    pub best_practices: Vec<BestPractice>,
    pub common_defects: Vec<DefectPattern>,
    pub root_cause_library: Vec<RootCauseKnowledge>,
    pub solution_patterns: Vec<SolutionPattern>,
    pub statistical_methods: Vec<StatisticalMethod>,
}

/// Training example for AI quality agent
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct TrainingExample {
    pub id: String,
    pub category: TrainingCategory,
    pub input: TrainingInput,
    pub expected_output: TrainingOutput,
    pub difficulty: Difficulty,
    pub tags: Vec<String>,
    pub learning_objectives: Vec<String>,
}

/// Category of training
#[derive(Debug, Clone, Copy, Serialize, Deserialize, PartialEq)]
pub enum TrainingCategory {
    /// Defect detection
    DefectDetection,
    /// Root cause analysis
    RootCauseAnalysis,
    /// Process capability assessment
    CapabilityAssessment,
    /// Statistical analysis
    StatisticalAnalysis,
    /// Improvement recommendation
    ImprovementRecommendation,
    /// Control plan development
    ControlPlanDevelopment,
}

/// Input for training example
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct TrainingInput {
    /// RDF/Turtle content to analyze
    pub rdf_content: String,
    /// Additional context
    pub context: HashMap<String, String>,
    /// Historical data
    pub historical_metrics: Option<Vec<f64>>,
}

/// Expected output for training example
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct TrainingOutput {
    /// Defects that should be identified
    pub identified_defects: Vec<ExpectedDefect>,
    /// Root causes that should be identified
    pub root_causes: Vec<String>,
    /// Quality metrics that should be calculated
    pub quality_metrics: HashMap<String, f64>,
    /// Recommendations that should be made
    pub recommendations: Vec<String>,
}

/// Expected defect in training
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ExpectedDefect {
    pub defect_code: String,
    pub location: String,
    pub severity: String,
    pub description: String,
}

/// Difficulty level of training example
#[derive(Debug, Clone, Copy, Serialize, Deserialize, PartialEq, Eq, PartialOrd, Ord)]
pub enum Difficulty {
    Beginner = 1,
    Intermediate = 2,
    Advanced = 3,
    Expert = 4,
}

/// Best practice for RDF quality
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct BestPractice {
    pub id: String,
    pub title: String,
    pub description: String,
    pub category: BestPracticeCategory,
    pub rationale: String,
    pub examples: Vec<String>,
    pub benefits: Vec<String>,
    pub related_practices: Vec<String>,
}

/// Category of best practice
#[derive(Debug, Clone, Copy, Serialize, Deserialize, PartialEq)]
pub enum BestPracticeCategory {
    /// Schema design
    SchemaDesign,
    /// Validation rules
    Validation,
    /// Performance optimization
    Performance,
    /// Maintainability
    Maintainability,
    /// Testing
    Testing,
    /// Documentation
    Documentation,
}

/// Common defect pattern
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct DefectPattern {
    pub pattern_id: String,
    pub name: String,
    pub description: String,
    /// How to detect this pattern
    pub detection_method: String,
    /// Common causes
    pub typical_causes: Vec<String>,
    /// Example manifestations
    pub examples: Vec<String>,
    /// Prevention strategies
    pub prevention: Vec<String>,
    /// Frequency in production
    pub frequency_score: u32, // 1-10
}

/// Root cause knowledge
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct RootCauseKnowledge {
    pub cause_id: String,
    pub description: String,
    pub category: String,
    /// Symptoms that indicate this root cause
    pub symptoms: Vec<String>,
    /// Methods to verify this is the root cause
    pub verification_methods: Vec<String>,
    /// Solutions that address this root cause
    pub solutions: Vec<String>,
}

/// Solution pattern
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct SolutionPattern {
    pub pattern_id: String,
    pub name: String,
    pub description: String,
    /// Problems this solution addresses
    pub addresses_problems: Vec<String>,
    /// Implementation steps
    pub implementation: Vec<String>,
    /// Expected benefits
    pub benefits: Vec<String>,
    /// Potential drawbacks
    pub trade_offs: Vec<String>,
    /// When to apply this solution
    pub applicability: String,
}

/// Statistical method reference
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct StatisticalMethod {
    pub method_id: String,
    pub name: String,
    pub description: String,
    pub use_cases: Vec<String>,
    pub prerequisites: Vec<String>,
    pub interpretation_guide: String,
    pub example_calculation: Option<String>,
}

/// Quality agent with trained knowledge
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct QualityAgent {
    pub agent_id: String,
    pub belt_level: BeltLevel,
    pub training_data: BlackBeltData,
    pub performance_metrics: AgentPerformanceMetrics,
    pub specializations: Vec<Specialization>,
}

/// Six Sigma belt level
#[derive(Debug, Clone, Copy, Serialize, Deserialize, PartialEq, Eq, PartialOrd, Ord)]
pub enum BeltLevel {
    White,
    Yellow,
    Green,
    Black,
    MasterBlack,
}

/// Specialization area for quality agent
#[derive(Debug, Clone, Copy, Serialize, Deserialize, PartialEq)]
pub enum Specialization {
    /// RDF syntax and parsing
    RdfSyntax,
    /// SHACL validation
    ShaclValidation,
    /// Semantic reasoning
    SemanticReasoning,
    /// Performance optimization
    Performance,
    /// Statistical analysis
    Statistics,
    /// Process improvement
    ProcessImprovement,
}

/// Performance metrics for quality agent
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct AgentPerformanceMetrics {
    /// Accuracy in defect detection (0-100%)
    pub defect_detection_accuracy: f64,
    /// Precision (true positives / (true positives + false positives))
    pub precision: f64,
    /// Recall (true positives / (true positives + false negatives))
    pub recall: f64,
    /// F1 score (harmonic mean of precision and recall)
    pub f1_score: f64,
    /// Root cause analysis accuracy
    pub root_cause_accuracy: f64,
    /// Recommendation acceptance rate
    pub recommendation_acceptance_rate: f64,
    /// Average time to analysis (seconds)
    pub avg_analysis_time: f64,
}

impl BlackBeltData {
    /// Create new training dataset
    pub fn new(version: String) -> Self {
        Self {
            version,
            training_examples: Vec::new(),
            best_practices: Vec::new(),
            common_defects: Vec::new(),
            root_cause_library: Vec::new(),
            solution_patterns: Vec::new(),
            statistical_methods: Vec::new(),
        }
    }

    /// Load standard RDF ontology quality training data
    pub fn load_standard_rdf_training() -> Self {
        let mut data = Self::new("1.0.0".to_string());

        // Add standard best practices
        data.best_practices.extend(Self::standard_best_practices());

        // Add common defect patterns
        data.common_defects.extend(Self::standard_defect_patterns());

        // Add statistical methods
        data.statistical_methods.extend(Self::standard_statistical_methods());

        data
    }

    /// Standard best practices for RDF quality
    fn standard_best_practices() -> Vec<BestPractice> {
        vec![
            BestPractice {
                id: "BP001".to_string(),
                title: "Use Semantic Versioning for Ontologies".to_string(),
                description: "Version ontologies using semantic versioning (major.minor.patch)".to_string(),
                category: BestPracticeCategory::SchemaDesign,
                rationale: "Enables clear communication of backward compatibility and breaking changes".to_string(),
                examples: vec!["ggen:templateVersion \"2.1.0\"".to_string()],
                benefits: vec![
                    "Clear change communication".to_string(),
                    "Easier dependency management".to_string(),
                ],
                related_practices: vec!["BP002".to_string()],
            },
            BestPractice {
                id: "BP002".to_string(),
                title: "Define Explicit SHACL Constraints".to_string(),
                description: "Use SHACL shapes to define explicit validation rules".to_string(),
                category: BestPracticeCategory::Validation,
                rationale: "Prevents data quality issues by enforcing constraints at validation time".to_string(),
                examples: vec![
                    "sh:minCount 1".to_string(),
                    "sh:pattern \"^[a-z]+$\"".to_string(),
                ],
                benefits: vec![
                    "Early defect detection".to_string(),
                    "Self-documenting constraints".to_string(),
                ],
                related_practices: vec!["BP003".to_string()],
            },
            BestPractice {
                id: "BP003".to_string(),
                title: "Provide Rich Metadata".to_string(),
                description: "Include comprehensive metadata for all ontology elements".to_string(),
                category: BestPracticeCategory::Documentation,
                rationale: "Improves discoverability and understanding".to_string(),
                examples: vec![
                    "ggen:templateDescription \"...\"".to_string(),
                    "rdfs:comment \"...\"".to_string(),
                ],
                benefits: vec![
                    "Better searchability".to_string(),
                    "Easier maintenance".to_string(),
                ],
                related_practices: vec!["BP001".to_string()],
            },
        ]
    }

    /// Standard defect patterns
    fn standard_defect_patterns() -> Vec<DefectPattern> {
        vec![
            DefectPattern {
                pattern_id: "DP001".to_string(),
                name: "Missing Required Property".to_string(),
                description: "A required property is not present in the resource".to_string(),
                detection_method: "SHACL validation with sh:minCount constraint".to_string(),
                typical_causes: vec![
                    "Template incomplete".to_string(),
                    "Missing variable binding".to_string(),
                ],
                examples: vec![
                    "ggen:Template missing ggen:templateName".to_string(),
                ],
                prevention: vec![
                    "Use strict SHACL shapes".to_string(),
                    "Validate during generation".to_string(),
                ],
                frequency_score: 8,
            },
            DefectPattern {
                pattern_id: "DP002".to_string(),
                name: "Invalid Property Value Type".to_string(),
                description: "Property value doesn't match expected datatype".to_string(),
                detection_method: "SHACL datatype validation".to_string(),
                typical_causes: vec![
                    "Incorrect type conversion".to_string(),
                    "Missing type annotation".to_string(),
                ],
                examples: vec![
                    "ggen:testCoverage \"ninety\"^^xsd:decimal (should be numeric)".to_string(),
                ],
                prevention: vec![
                    "Strong typing in templates".to_string(),
                    "Input validation".to_string(),
                ],
                frequency_score: 6,
            },
        ]
    }

    /// Standard statistical methods
    fn standard_statistical_methods() -> Vec<StatisticalMethod> {
        vec![
            StatisticalMethod {
                method_id: "SM001".to_string(),
                name: "Process Capability Index (Cpk)".to_string(),
                description: "Measures how well a process meets specifications".to_string(),
                use_cases: vec![
                    "Assess template generation precision".to_string(),
                    "Compare process performance across stages".to_string(),
                ],
                prerequisites: vec![
                    "Process must be stable".to_string(),
                    "Data must be normally distributed".to_string(),
                    "At least 30 samples required".to_string(),
                ],
                interpretation_guide: "Cpk >= 1.33 indicates capable process; >= 2.0 indicates world-class".to_string(),
                example_calculation: Some("Cpk = min((USL - μ)/(3σ), (μ - LSL)/(3σ))".to_string()),
            },
            StatisticalMethod {
                method_id: "SM002".to_string(),
                name: "Statistical Process Control (SPC)".to_string(),
                description: "Monitors process variation using control charts".to_string(),
                use_cases: vec![
                    "Detect special cause variation".to_string(),
                    "Monitor process stability over time".to_string(),
                ],
                prerequisites: vec![
                    "Rational subgroups".to_string(),
                    "Sufficient baseline data".to_string(),
                ],
                interpretation_guide: "Points outside 3σ limits or patterns indicate special causes".to_string(),
                example_calculation: Some("UCL = μ + 3σ, LCL = μ - 3σ".to_string()),
            },
        ]
    }

    /// Add training example
    pub fn add_example(&mut self, example: TrainingExample) {
        self.training_examples.push(example);
    }

    /// Get examples by category
    pub fn get_examples_by_category(&self, category: TrainingCategory) -> Vec<&TrainingExample> {
        self.training_examples
            .iter()
            .filter(|e| e.category == category)
            .collect()
    }

    /// Get examples by difficulty
    pub fn get_examples_by_difficulty(&self, difficulty: Difficulty) -> Vec<&TrainingExample> {
        self.training_examples
            .iter()
            .filter(|e| e.difficulty == difficulty)
            .collect()
    }
}

impl QualityAgent {
    /// Create new quality agent
    pub fn new(agent_id: String, belt_level: BeltLevel) -> Self {
        Self {
            agent_id,
            belt_level,
            training_data: BlackBeltData::load_standard_rdf_training(),
            performance_metrics: AgentPerformanceMetrics::default(),
            specializations: Vec::new(),
        }
    }

    /// Add specialization
    pub fn add_specialization(&mut self, specialization: Specialization) {
        if !self.specializations.contains(&specialization) {
            self.specializations.push(specialization);
        }
    }

    /// Update performance metrics
    pub fn update_metrics(&mut self, metrics: AgentPerformanceMetrics) {
        self.performance_metrics = metrics;
    }

    /// Check if agent qualifies for belt level
    pub fn qualifies_for_belt(&self, target_belt: BeltLevel) -> bool {
        match target_belt {
            BeltLevel::Yellow => {
                self.performance_metrics.defect_detection_accuracy >= 70.0
                    && self.performance_metrics.f1_score >= 0.65
            }
            BeltLevel::Green => {
                self.performance_metrics.defect_detection_accuracy >= 80.0
                    && self.performance_metrics.f1_score >= 0.75
                    && self.performance_metrics.root_cause_accuracy >= 70.0
            }
            BeltLevel::Black => {
                self.performance_metrics.defect_detection_accuracy >= 90.0
                    && self.performance_metrics.f1_score >= 0.85
                    && self.performance_metrics.root_cause_accuracy >= 80.0
                    && self.specializations.len() >= 2
            }
            BeltLevel::MasterBlack => {
                self.performance_metrics.defect_detection_accuracy >= 95.0
                    && self.performance_metrics.f1_score >= 0.90
                    && self.performance_metrics.root_cause_accuracy >= 90.0
                    && self.specializations.len() >= 4
                    && self.performance_metrics.recommendation_acceptance_rate >= 85.0
            }
            BeltLevel::White => true, // Everyone qualifies for White belt
        }
    }
}

impl Default for AgentPerformanceMetrics {
    fn default() -> Self {
        Self {
            defect_detection_accuracy: 0.0,
            precision: 0.0,
            recall: 0.0,
            f1_score: 0.0,
            root_cause_accuracy: 0.0,
            recommendation_acceptance_rate: 0.0,
            avg_analysis_time: 0.0,
        }
    }
}

impl AgentPerformanceMetrics {
    /// Calculate F1 score from precision and recall
    pub fn calculate_f1(&mut self) {
        if self.precision + self.recall > 0.0 {
            self.f1_score = 2.0 * (self.precision * self.recall) / (self.precision + self.recall);
        } else {
            self.f1_score = 0.0;
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_load_standard_training() {
        let data = BlackBeltData::load_standard_rdf_training();

        assert!(!data.best_practices.is_empty());
        assert!(!data.common_defects.is_empty());
        assert!(!data.statistical_methods.is_empty());
    }

    #[test]
    fn test_quality_agent_creation() {
        let agent = QualityAgent::new("agent-001".to_string(), BeltLevel::Green);

        assert_eq!(agent.belt_level, BeltLevel::Green);
        assert!(agent.specializations.is_empty());
    }

    #[test]
    fn test_belt_qualification() {
        let mut agent = QualityAgent::new("agent-001".to_string(), BeltLevel::White);

        // Should not qualify for Green belt initially
        assert!(!agent.qualifies_for_belt(BeltLevel::Green));

        // Update metrics to qualify
        let mut metrics = AgentPerformanceMetrics::default();
        metrics.defect_detection_accuracy = 85.0;
        metrics.precision = 0.80;
        metrics.recall = 0.75;
        metrics.calculate_f1();
        metrics.root_cause_accuracy = 75.0;

        agent.update_metrics(metrics);

        // Should now qualify for Green belt
        assert!(agent.qualifies_for_belt(BeltLevel::Green));
    }

    #[test]
    fn test_f1_calculation() {
        let mut metrics = AgentPerformanceMetrics::default();
        metrics.precision = 0.8;
        metrics.recall = 0.9;
        metrics.calculate_f1();

        // F1 = 2 * (0.8 * 0.9) / (0.8 + 0.9) = 2 * 0.72 / 1.7 ≈ 0.847
        assert!((metrics.f1_score - 0.847).abs() < 0.001);
    }
}
