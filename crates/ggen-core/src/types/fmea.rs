//! FMEA (Failure Mode and Effects Analysis) types for marketplace packages.
//!
//! This module provides types for documenting and validating failure modes
//! in enterprise marketplace packages:
//! - Severity, Occurrence, Detection ratings (1-10 scale)
//! - RPN (Risk Priority Number) calculation and classification
//! - FailureMode entries with controls
//! - FmeaConfig for package.toml [fmea] section

use serde::{Deserialize, Serialize};
use std::collections::HashSet;
use thiserror::Error;

/// Validation errors for FMEA ratings
#[derive(Debug, Error, Clone, PartialEq, Eq)]
pub enum FmeaValidationError {
    #[error("Invalid severity rating {0}: must be between 1 and 10")]
    InvalidSeverity(u8),
    #[error("Invalid occurrence rating {0}: must be between 1 and 10")]
    InvalidOccurrence(u8),
    #[error("Invalid detection rating {0}: must be between 1 and 10")]
    InvalidDetection(u8),
    #[error("Duplicate failure mode ID: {0}")]
    DuplicateId(String),
    #[error("Unmitigated critical failure mode: {0} (RPN {1} > 200)")]
    UnmitigatedCritical(String, u16),
}

/// FMEA Severity rating (1-10)
/// Higher = more severe impact if failure occurs
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
#[serde(transparent)]
pub struct Severity(u8);

impl Severity {
    /// Create a new Severity rating
    pub fn new(value: u8) -> Result<Self, FmeaValidationError> {
        if !(1..=10).contains(&value) {
            return Err(FmeaValidationError::InvalidSeverity(value));
        }
        Ok(Self(value))
    }

    /// Get the raw value
    pub fn value(&self) -> u8 {
        self.0
    }

    /// Get severity level description
    pub fn level(&self) -> &'static str {
        match self.0 {
            1 => "None",
            2..=3 => "Minor",
            4..=6 => "Moderate",
            7..=8 => "High",
            9..=10 => "Critical",
            _ => unreachable!(),
        }
    }
}

impl Default for Severity {
    fn default() -> Self {
        Self(5)
    }
}

/// FMEA Occurrence rating (1-10)
/// Higher = more likely to occur
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
#[serde(transparent)]
pub struct Occurrence(u8);

impl Occurrence {
    /// Create a new Occurrence rating
    pub fn new(value: u8) -> Result<Self, FmeaValidationError> {
        if !(1..=10).contains(&value) {
            return Err(FmeaValidationError::InvalidOccurrence(value));
        }
        Ok(Self(value))
    }

    /// Get the raw value
    pub fn value(&self) -> u8 {
        self.0
    }

    /// Get occurrence level description
    pub fn level(&self) -> &'static str {
        match self.0 {
            1 => "Remote",
            2..=3 => "Low",
            4..=6 => "Moderate",
            7..=8 => "High",
            9..=10 => "Very High",
            _ => unreachable!(),
        }
    }
}

impl Default for Occurrence {
    fn default() -> Self {
        Self(5)
    }
}

/// FMEA Detection rating (1-10)
/// Higher = harder to detect before release
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
#[serde(transparent)]
pub struct Detection(u8);

impl Detection {
    /// Create a new Detection rating
    pub fn new(value: u8) -> Result<Self, FmeaValidationError> {
        if !(1..=10).contains(&value) {
            return Err(FmeaValidationError::InvalidDetection(value));
        }
        Ok(Self(value))
    }

    /// Get the raw value
    pub fn value(&self) -> u8 {
        self.0
    }

    /// Get detection level description
    pub fn level(&self) -> &'static str {
        match self.0 {
            1..=2 => "Almost Certain",
            3..=4 => "High",
            5..=6 => "Moderate",
            7..=8 => "Low",
            9..=10 => "Remote",
            _ => unreachable!(),
        }
    }
}

impl Default for Detection {
    fn default() -> Self {
        Self(5)
    }
}

/// RPN risk classification
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum RpnLevel {
    /// RPN >200: MUST have control, blocks validation
    Critical,
    /// RPN 100-200: SHOULD have control, warning if missing
    High,
    /// RPN <100: MAY have control, informational
    Medium,
}

impl RpnLevel {
    /// Check if this level requires mandatory control
    pub fn requires_control(&self) -> bool {
        matches!(self, RpnLevel::Critical)
    }
}

/// Risk Priority Number = Severity x Occurrence x Detection
/// Range: 1-1000
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Serialize, Deserialize)]
#[serde(transparent)]
pub struct RpnScore(u16);

impl RpnScore {
    /// Calculate RPN from S x O x D
    pub fn calculate(severity: Severity, occurrence: Occurrence, detection: Detection) -> Self {
        let rpn =
            (severity.value() as u16) * (occurrence.value() as u16) * (detection.value() as u16);
        Self(rpn)
    }

    /// Get the raw RPN value
    pub fn value(&self) -> u16 {
        self.0
    }

    /// Get the risk level classification
    pub fn level(&self) -> RpnLevel {
        match self.0 {
            201..=1000 => RpnLevel::Critical,
            100..=200 => RpnLevel::High,
            _ => RpnLevel::Medium,
        }
    }

    /// Check if this RPN requires mandatory control
    pub fn requires_control(&self) -> bool {
        self.level().requires_control()
    }
}

/// Single FMEA failure mode entry
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct FailureModeEntry {
    /// Unique identifier (e.g., "F1", "F2")
    pub id: String,
    /// Description of the failure mode
    pub mode: String,
    /// Severity rating (1-10)
    pub severity: u8,
    /// Occurrence rating (1-10)
    pub occurrence: u8,
    /// Detection rating (1-10)
    pub detection: u8,
    /// Mitigation control (required for Critical, recommended for High)
    #[serde(default)]
    pub control: Option<String>,
}

impl FailureModeEntry {
    /// Calculate the RPN score for this failure mode
    pub fn calculate_rpn(&self) -> Result<RpnScore, FmeaValidationError> {
        let severity = Severity::new(self.severity)?;
        let occurrence = Occurrence::new(self.occurrence)?;
        let detection = Detection::new(self.detection)?;
        Ok(RpnScore::calculate(severity, occurrence, detection))
    }

    /// Check if this failure mode has mitigation control
    pub fn is_mitigated(&self) -> bool {
        self.control
            .as_ref()
            .map(|c| !c.is_empty())
            .unwrap_or(false)
    }

    /// Check if this failure mode requires a control
    pub fn requires_control(&self) -> Result<bool, FmeaValidationError> {
        Ok(self.calculate_rpn()?.requires_control())
    }

    /// Validate this failure mode entry
    pub fn validate(&self) -> Result<(), FmeaValidationError> {
        let rpn = self.calculate_rpn()?;

        if rpn.requires_control() && !self.is_mitigated() {
            return Err(FmeaValidationError::UnmitigatedCritical(
                self.id.clone(),
                rpn.value(),
            ));
        }

        Ok(())
    }
}

/// FMEA configuration section from package.toml
#[derive(Debug, Clone, Default, Serialize, Deserialize)]
pub struct FmeaConfig {
    /// Enable FMEA validation
    #[serde(default)]
    pub enabled: bool,
    /// Minimum coverage percentage (0-100)
    #[serde(default = "default_min_coverage")]
    pub min_coverage: u8,
    /// List of failure mode controls
    #[serde(default)]
    pub controls: Vec<FailureModeEntry>,
}

fn default_min_coverage() -> u8 {
    100
}

impl FmeaConfig {
    pub fn new() -> Self {
        Self::default()
    }

    /// Validate all failure modes in this config
    pub fn validate(&self) -> Result<(), Vec<FmeaValidationError>> {
        let mut errors = Vec::new();
        let mut seen_ids = HashSet::new();

        for fm in &self.controls {
            // Check for duplicate IDs
            if !seen_ids.insert(&fm.id) {
                errors.push(FmeaValidationError::DuplicateId(fm.id.clone()));
            }

            // Validate each failure mode
            if let Err(e) = fm.validate() {
                errors.push(e);
            }
        }

        if errors.is_empty() {
            Ok(())
        } else {
            Err(errors)
        }
    }

    /// Get all critical failure modes (RPN > 200)
    pub fn critical_modes(&self) -> Vec<&FailureModeEntry> {
        self.controls
            .iter()
            .filter(|fm| {
                fm.calculate_rpn()
                    .map(|rpn| rpn.level() == RpnLevel::Critical)
                    .unwrap_or(false)
            })
            .collect()
    }

    /// Get all high-risk failure modes (RPN 100-200)
    pub fn high_risk_modes(&self) -> Vec<&FailureModeEntry> {
        self.controls
            .iter()
            .filter(|fm| {
                fm.calculate_rpn()
                    .map(|rpn| rpn.level() == RpnLevel::High)
                    .unwrap_or(false)
            })
            .collect()
    }

    /// Calculate coverage percentage (mitigated / total critical)
    pub fn coverage_percentage(&self) -> f64 {
        let critical = self.critical_modes();
        if critical.is_empty() {
            return 100.0;
        }
        let mitigated = critical.iter().filter(|fm| fm.is_mitigated()).count();
        (mitigated as f64 / critical.len() as f64) * 100.0
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_severity_valid_range() {
        assert!(Severity::new(1).is_ok());
        assert!(Severity::new(10).is_ok());
        assert!(Severity::new(0).is_err());
        assert!(Severity::new(11).is_err());
    }

    #[test]
    fn test_rpn_calculation() {
        let severity = Severity::new(10).unwrap();
        let occurrence = Occurrence::new(7).unwrap();
        let detection = Detection::new(2).unwrap();

        let rpn = RpnScore::calculate(severity, occurrence, detection);
        assert_eq!(rpn.value(), 140); // 10 * 7 * 2 = 140
        assert_eq!(rpn.level(), RpnLevel::High);
    }

    #[test]
    fn test_rpn_critical_threshold() {
        let severity = Severity::new(9).unwrap();
        let occurrence = Occurrence::new(6).unwrap();
        let detection = Detection::new(4).unwrap();

        let rpn = RpnScore::calculate(severity, occurrence, detection);
        assert_eq!(rpn.value(), 216); // 9 * 6 * 4 = 216
        assert_eq!(rpn.level(), RpnLevel::Critical);
        assert!(rpn.requires_control());
    }

    #[test]
    fn test_failure_mode_validation_passes() {
        let fm = FailureModeEntry {
            id: "F1".into(),
            mode: "Developer edits generated file".into(),
            severity: 9,
            occurrence: 6,
            detection: 4,
            control: Some("DO NOT EDIT header".into()),
        };

        assert!(fm.validate().is_ok());
    }

    #[test]
    fn test_failure_mode_unmitigated_critical_fails() {
        let fm = FailureModeEntry {
            id: "F1".into(),
            mode: "Critical failure without control".into(),
            severity: 10,
            occurrence: 7,
            detection: 3,
            control: None,
        };

        let result = fm.validate();
        assert!(result.is_err());
        match result.unwrap_err() {
            FmeaValidationError::UnmitigatedCritical(id, rpn) => {
                assert_eq!(id, "F1");
                assert!(rpn > 200);
            }
            _ => panic!("Expected UnmitigatedCritical error"),
        }
    }

    #[test]
    fn test_fmea_config_coverage() {
        let config = FmeaConfig {
            enabled: true,
            min_coverage: 100,
            controls: vec![
                FailureModeEntry {
                    id: "F1".into(),
                    mode: "Critical mitigated".into(),
                    severity: 10,
                    occurrence: 7,
                    detection: 3,
                    control: Some("Has control".into()),
                },
                FailureModeEntry {
                    id: "F2".into(),
                    mode: "Low risk".into(),
                    severity: 2,
                    occurrence: 2,
                    detection: 2,
                    control: None,
                },
            ],
        };

        assert_eq!(config.coverage_percentage(), 100.0);
    }

    #[test]
    fn test_fmea_config_duplicate_id() {
        let config = FmeaConfig {
            enabled: true,
            min_coverage: 100,
            controls: vec![
                FailureModeEntry {
                    id: "F1".into(),
                    mode: "First".into(),
                    severity: 5,
                    occurrence: 5,
                    detection: 5,
                    control: None,
                },
                FailureModeEntry {
                    id: "F1".into(),
                    mode: "Second".into(),
                    severity: 5,
                    occurrence: 5,
                    detection: 5,
                    control: None,
                },
            ],
        };

        let result = config.validate();
        assert!(result.is_err());
        let errors = result.unwrap_err();
        assert!(errors
            .iter()
            .any(|e| matches!(e, FmeaValidationError::DuplicateId(_))));
    }
}
