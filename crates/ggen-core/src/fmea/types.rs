//! Core FMEA types and structures

use serde::{Deserialize, Serialize};
use std::fmt;

/// Severity rating (1-10 scale)
///
/// Represents the seriousness of the effect of a failure mode
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Serialize, Deserialize)]
pub struct Severity(u8);

impl Severity {
    /// Create a new severity rating (1-10)
    pub fn new(value: u8) -> Result<Self, String> {
        if (1..=10).contains(&value) {
            Ok(Self(value))
        } else {
            Err(format!("Severity must be between 1-10, got {}", value))
        }
    }

    /// Get the numeric value
    pub fn value(&self) -> u8 {
        self.0
    }

    /// None/minimal impact (1-2)
    pub const MINIMAL: Self = Self(1);

    /// Minor impact (3-4)
    pub const MINOR: Self = Self(3);

    /// Moderate impact (5-6)
    pub const MODERATE: Self = Self(5);

    /// Major impact (7-8)
    pub const MAJOR: Self = Self(7);

    /// Critical/catastrophic impact (9-10)
    pub const CRITICAL: Self = Self(9);

    /// Get severity category description
    pub fn description(&self) -> &'static str {
        match self.0 {
            1..=2 => "Minimal - Minor inconvenience, no functional impact",
            3..=4 => "Minor - Slight degradation, workaround available",
            5..=6 => "Moderate - Noticeable degradation, requires fix",
            7..=8 => "Major - Significant functional loss",
            9..=10 => "Critical - System failure, data loss, or security breach",
            _ => "Unknown",
        }
    }
}

/// Occurrence/Frequency rating (1-10 scale)
///
/// Represents the likelihood of a failure mode occurring
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Serialize, Deserialize)]
pub struct Occurrence(u8);

impl Occurrence {
    /// Create a new occurrence rating (1-10)
    pub fn new(value: u8) -> Result<Self, String> {
        if (1..=10).contains(&value) {
            Ok(Self(value))
        } else {
            Err(format!("Occurrence must be between 1-10, got {}", value))
        }
    }

    /// Get the numeric value
    pub fn value(&self) -> u8 {
        self.0
    }

    /// Remote/unlikely (1-2)
    pub const REMOTE: Self = Self(1);

    /// Low probability (3-4)
    pub const LOW: Self = Self(3);

    /// Moderate probability (5-6)
    pub const MODERATE: Self = Self(5);

    /// High probability (7-8)
    pub const HIGH: Self = Self(7);

    /// Very high/certain (9-10)
    pub const VERY_HIGH: Self = Self(9);

    /// Get occurrence category description
    pub fn description(&self) -> &'static str {
        match self.0 {
            1..=2 => "Remote - Failure unlikely (<1 in 10,000)",
            3..=4 => "Low - Occasional failure (1 in 1,000)",
            5..=6 => "Moderate - Frequent failure (1 in 100)",
            7..=8 => "High - Repeated failure (1 in 10)",
            9..=10 => "Very High - Failure almost certain (>1 in 2)",
            _ => "Unknown",
        }
    }
}

/// Detection rating (1-10 scale)
///
/// Represents the likelihood of detecting a failure before it causes harm
/// Lower is better (1 = almost certain detection, 10 = cannot detect)
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Serialize, Deserialize)]
pub struct Detection(u8);

impl Detection {
    /// Create a new detection rating (1-10)
    pub fn new(value: u8) -> Result<Self, String> {
        if (1..=10).contains(&value) {
            Ok(Self(value))
        } else {
            Err(format!("Detection must be between 1-10, got {}", value))
        }
    }

    /// Get the numeric value
    pub fn value(&self) -> u8 {
        self.0
    }

    /// Almost certain detection (1-2)
    pub const ALMOST_CERTAIN: Self = Self(1);

    /// High detection likelihood (3-4)
    pub const HIGH: Self = Self(3);

    /// Moderate detection likelihood (5-6)
    pub const MODERATE: Self = Self(5);

    /// Low detection likelihood (7-8)
    pub const LOW: Self = Self(7);

    /// Cannot detect (9-10)
    pub const CANNOT_DETECT: Self = Self(10);

    /// Get detection category description
    pub fn description(&self) -> &'static str {
        match self.0 {
            1..=2 => "Almost Certain - Automated tests/validation always detect",
            3..=4 => "High - Automated tests likely to detect",
            5..=6 => "Moderate - Manual review or testing may detect",
            7..=8 => "Low - Detection difficult, requires expert review",
            9..=10 => "Cannot Detect - No known method to detect before deployment",
            _ => "Unknown",
        }
    }
}

/// Risk Priority Number (RPN)
///
/// Calculated as: Severity × Occurrence × Detection
/// Range: 1-1000
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Serialize, Deserialize)]
pub struct RiskPriorityNumber(u16);

impl RiskPriorityNumber {
    /// Calculate RPN from SOD scores
    pub fn calculate(severity: Severity, occurrence: Occurrence, detection: Detection) -> Self {
        let rpn = severity.value() as u16 * occurrence.value() as u16 * detection.value() as u16;
        Self(rpn)
    }

    /// Get the numeric value
    pub fn value(&self) -> u16 {
        self.0
    }

    /// Get risk level category
    pub fn risk_level(&self) -> RiskLevel {
        match self.0 {
            1..=50 => RiskLevel::Low,
            51..=100 => RiskLevel::Medium,
            101..=200 => RiskLevel::High,
            201..=500 => RiskLevel::VeryHigh,
            _ => RiskLevel::Critical,
        }
    }

    /// Check if RPN requires immediate action
    pub fn requires_immediate_action(&self) -> bool {
        self.0 > 200
    }
}

impl fmt::Display for RiskPriorityNumber {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{} ({})", self.0, self.risk_level())
    }
}

/// Risk level categorization
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum RiskLevel {
    /// Low risk (RPN 1-50)
    Low,
    /// Medium risk (RPN 51-100)
    Medium,
    /// High risk (RPN 101-200)
    High,
    /// Very high risk (RPN 201-500)
    VeryHigh,
    /// Critical risk (RPN 501-1000)
    Critical,
}

impl fmt::Display for RiskLevel {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Low => write!(f, "Low"),
            Self::Medium => write!(f, "Medium"),
            Self::High => write!(f, "High"),
            Self::VeryHigh => write!(f, "Very High"),
            Self::Critical => write!(f, "Critical"),
        }
    }
}

/// Failure mode category
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub enum FailureModeCategory {
    /// RDF schema/ontology changes
    SchemaChange,
    /// SPARQL query execution
    QueryExecution,
    /// Template transformation
    TemplateTransformation,
    /// Code generation
    CodeGeneration,
    /// Validation/verification
    Validation,
    /// Custom category
    Custom(String),
}

impl fmt::Display for FailureModeCategory {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::SchemaChange => write!(f, "Schema Change"),
            Self::QueryExecution => write!(f, "Query Execution"),
            Self::TemplateTransformation => write!(f, "Template Transformation"),
            Self::CodeGeneration => write!(f, "Code Generation"),
            Self::Validation => write!(f, "Validation"),
            Self::Custom(name) => write!(f, "{}", name),
        }
    }
}

/// A specific failure mode with SOD scoring and RPN
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct FailureMode {
    /// Unique identifier
    pub id: String,

    /// Category of failure
    pub category: FailureModeCategory,

    /// Description of the failure mode
    pub description: String,

    /// Potential effects/consequences
    pub effects: Vec<String>,

    /// Potential causes
    pub causes: Vec<String>,

    /// Current controls/preventions
    pub current_controls: Vec<String>,

    /// Severity rating
    pub severity: Severity,

    /// Occurrence rating
    pub occurrence: Occurrence,

    /// Detection rating
    pub detection: Detection,

    /// Calculated RPN
    pub rpn: RiskPriorityNumber,

    /// Recommended actions
    pub recommended_actions: Vec<String>,

    /// Mitigation strategies
    pub mitigations: Vec<MitigationStrategy>,
}

impl FailureMode {
    /// Create a new failure mode
    pub fn new(
        id: String,
        category: FailureModeCategory,
        description: String,
        severity: Severity,
        occurrence: Occurrence,
        detection: Detection,
    ) -> Self {
        let rpn = RiskPriorityNumber::calculate(severity, occurrence, detection);

        Self {
            id,
            category,
            description,
            effects: Vec::new(),
            causes: Vec::new(),
            current_controls: Vec::new(),
            severity,
            occurrence,
            detection,
            rpn,
            recommended_actions: Vec::new(),
            mitigations: Vec::new(),
        }
    }

    /// Add an effect
    pub fn add_effect(mut self, effect: String) -> Self {
        self.effects.push(effect);
        self
    }

    /// Add a cause
    pub fn add_cause(mut self, cause: String) -> Self {
        self.causes.push(cause);
        self
    }

    /// Add a current control
    pub fn add_control(mut self, control: String) -> Self {
        self.current_controls.push(control);
        self
    }

    /// Add a recommended action
    pub fn add_action(mut self, action: String) -> Self {
        self.recommended_actions.push(action);
        self
    }

    /// Add a mitigation strategy
    pub fn add_mitigation(mut self, mitigation: MitigationStrategy) -> Self {
        self.mitigations.push(mitigation);
        self
    }

    /// Calculate revised RPN after mitigations
    pub fn revised_rpn(&self) -> RiskPriorityNumber {
        let mut new_severity = self.severity;
        let mut new_occurrence = self.occurrence;
        let mut new_detection = self.detection;

        for mitigation in &self.mitigations {
            if mitigation.status == MitigationStatus::Implemented {
                match mitigation.impact_on {
                    ImpactTarget::Severity => {
                        new_severity = Severity::new(
                            new_severity.value().saturating_sub(mitigation.reduction)
                        ).unwrap_or(Severity::MINIMAL);
                    }
                    ImpactTarget::Occurrence => {
                        new_occurrence = Occurrence::new(
                            new_occurrence.value().saturating_sub(mitigation.reduction)
                        ).unwrap_or(Occurrence::REMOTE);
                    }
                    ImpactTarget::Detection => {
                        new_detection = Detection::new(
                            new_detection.value().saturating_sub(mitigation.reduction)
                        ).unwrap_or(Detection::ALMOST_CERTAIN);
                    }
                }
            }
        }

        RiskPriorityNumber::calculate(new_severity, new_occurrence, new_detection)
    }
}

/// Mitigation strategy for a failure mode
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct MitigationStrategy {
    /// Description of the mitigation
    pub description: String,

    /// Which SOD component this impacts
    pub impact_on: ImpactTarget,

    /// Expected reduction (1-10)
    pub reduction: u8,

    /// Implementation status
    pub status: MitigationStatus,

    /// Implementation cost (relative)
    pub cost: MitigationCost,
}

/// What the mitigation impacts
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum ImpactTarget {
    /// Reduces severity
    Severity,
    /// Reduces occurrence
    Occurrence,
    /// Improves detection
    Detection,
}

/// Mitigation implementation status
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum MitigationStatus {
    /// Not yet started
    Planned,
    /// Currently being implemented
    InProgress,
    /// Fully implemented
    Implemented,
}

/// Relative cost of mitigation
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum MitigationCost {
    /// Minimal effort
    Low,
    /// Moderate effort
    Medium,
    /// Significant effort
    High,
}

/// Complete FMEA analysis
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct FmeaAnalysis {
    /// Analysis metadata
    pub metadata: AnalysisMetadata,

    /// All identified failure modes
    pub failure_modes: Vec<FailureMode>,
}

impl FmeaAnalysis {
    /// Create a new FMEA analysis
    pub fn new(name: String, description: String) -> Self {
        Self {
            metadata: AnalysisMetadata {
                name,
                description,
                version: crate::fmea::FMEA_VERSION.to_string(),
                created_at: chrono::Utc::now(),
            },
            failure_modes: Vec::new(),
        }
    }

    /// Add a failure mode
    pub fn add_failure_mode(&mut self, failure_mode: FailureMode) {
        self.failure_modes.push(failure_mode);
    }

    /// Get failure modes sorted by RPN (highest first)
    pub fn sorted_by_rpn(&self) -> Vec<&FailureMode> {
        let mut modes: Vec<&FailureMode> = self.failure_modes.iter().collect();
        modes.sort_by(|a, b| b.rpn.cmp(&a.rpn));
        modes
    }

    /// Get critical failure modes (RPN > 200)
    pub fn critical_modes(&self) -> Vec<&FailureMode> {
        self.failure_modes
            .iter()
            .filter(|fm| fm.rpn.requires_immediate_action())
            .collect()
    }

    /// Generate FMEA report
    pub fn generate_report(&self) -> FmeaReport {
        FmeaReport::from_analysis(self)
    }
}

/// Analysis metadata
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct AnalysisMetadata {
    /// Analysis name
    pub name: String,

    /// Description
    pub description: String,

    /// FMEA system version
    pub version: String,

    /// Creation timestamp
    pub created_at: chrono::DateTime<chrono::Utc>,
}

/// FMEA report for human consumption
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct FmeaReport {
    /// Metadata
    pub metadata: AnalysisMetadata,

    /// Total number of failure modes
    pub total_modes: usize,

    /// Number of critical modes
    pub critical_count: usize,

    /// Highest RPN value
    pub max_rpn: u16,

    /// Average RPN
    pub avg_rpn: f64,

    /// Failure modes by category
    pub modes_by_category: Vec<(String, usize)>,

    /// Top 10 highest risk modes
    pub top_risks: Vec<String>,
}

impl FmeaReport {
    /// Create a report from an analysis
    pub fn from_analysis(analysis: &FmeaAnalysis) -> Self {
        let sorted = analysis.sorted_by_rpn();
        let critical_count = analysis.critical_modes().len();

        let max_rpn = sorted.first().map(|fm| fm.rpn.value()).unwrap_or(0);

        let total = analysis.failure_modes.len();
        let sum: u16 = analysis.failure_modes.iter().map(|fm| fm.rpn.value()).sum();
        let avg_rpn = if total > 0 {
            sum as f64 / total as f64
        } else {
            0.0
        };

        let top_risks = sorted
            .iter()
            .take(10)
            .map(|fm| format!("{} (RPN: {})", fm.description, fm.rpn.value()))
            .collect();

        // Count by category
        let mut category_counts: std::collections::HashMap<String, usize> = std::collections::HashMap::new();
        for fm in &analysis.failure_modes {
            *category_counts.entry(fm.category.to_string()).or_insert(0) += 1;
        }
        let modes_by_category: Vec<(String, usize)> = category_counts.into_iter().collect();

        Self {
            metadata: analysis.metadata.clone(),
            total_modes: total,
            critical_count,
            max_rpn,
            avg_rpn,
            modes_by_category,
            top_risks,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_severity_bounds() {
        assert!(Severity::new(0).is_err());
        assert!(Severity::new(11).is_err());
        assert!(Severity::new(5).is_ok());
    }

    #[test]
    fn test_rpn_calculation() {
        let s = Severity::new(9).unwrap();
        let o = Occurrence::new(7).unwrap();
        let d = Detection::new(10).unwrap();

        let rpn = RiskPriorityNumber::calculate(s, o, d);
        assert_eq!(rpn.value(), 630);
        assert!(rpn.requires_immediate_action());
    }

    #[test]
    fn test_risk_levels() {
        let low = RiskPriorityNumber::calculate(
            Severity::new(1).unwrap(),
            Occurrence::new(5).unwrap(),
            Detection::new(5).unwrap(),
        );
        assert_eq!(low.risk_level(), RiskLevel::Low);

        let critical = RiskPriorityNumber::calculate(
            Severity::new(10).unwrap(),
            Occurrence::new(10).unwrap(),
            Detection::new(10).unwrap(),
        );
        assert_eq!(critical.risk_level(), RiskLevel::Critical);
    }

    #[test]
    fn test_failure_mode_builder() {
        let fm = FailureMode::new(
            "FM001".to_string(),
            FailureModeCategory::SchemaChange,
            "Breaking schema change".to_string(),
            Severity::new(9).unwrap(),
            Occurrence::new(5).unwrap(),
            Detection::new(8).unwrap(),
        )
        .add_effect("Downstream queries fail".to_string())
        .add_cause("Property renamed without migration".to_string())
        .add_control("Schema validation".to_string());

        assert_eq!(fm.effects.len(), 1);
        assert_eq!(fm.causes.len(), 1);
        assert_eq!(fm.current_controls.len(), 1);
        assert_eq!(fm.rpn.value(), 360);
    }

    #[test]
    fn test_revised_rpn_with_mitigation() {
        let mut fm = FailureMode::new(
            "FM002".to_string(),
            FailureModeCategory::QueryExecution,
            "Query timeout".to_string(),
            Severity::new(7).unwrap(),
            Occurrence::new(6).unwrap(),
            Detection::new(5).unwrap(),
        );

        let original_rpn = fm.rpn.value();
        assert_eq!(original_rpn, 210);

        fm = fm.add_mitigation(MitigationStrategy {
            description: "Add query timeout configuration".to_string(),
            impact_on: ImpactTarget::Severity,
            reduction: 3,
            status: MitigationStatus::Implemented,
            cost: MitigationCost::Low,
        });

        let revised = fm.revised_rpn();
        assert!(revised.value() < original_rpn);
    }
}
