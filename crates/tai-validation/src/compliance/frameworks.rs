//! Compliance frameworks (FISMA, FedRAMP, SOC 2, HIPAA, 21 CFR Part 11, NIST 800-53, DFARS)

use crate::error::Result;
use serde::{Deserialize, Serialize};
use std::fmt;

/// Compliance framework type
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum FrameworkType {
    /// FISMA - Federal Information Security Management Act
    FISMA,
    /// FedRAMP - Federal Risk and Authorization Management Program
    FedRAMP,
    /// SOC 2 - Service Organization Control Type II
    SOC2TypeII,
    /// HIPAA - Health Insurance Portability and Accountability Act
    HIPAA,
    /// 21 CFR Part 11 - FDA Regulations for Electronic Records
    CFRPart11,
    /// NIST 800-53 - Security and Privacy Controls
    NIST80053,
    /// DFARS - Defense Federal Acquisition Regulation Supplement
    DFARS,
}

impl fmt::Display for FrameworkType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            FrameworkType::FISMA => write!(f, "FISMA"),
            FrameworkType::FedRAMP => write!(f, "FedRAMP"),
            FrameworkType::SOC2TypeII => write!(f, "SOC 2 Type II"),
            FrameworkType::HIPAA => write!(f, "HIPAA"),
            FrameworkType::CFRPart11 => write!(f, "21 CFR Part 11"),
            FrameworkType::NIST80053 => write!(f, "NIST 800-53"),
            FrameworkType::DFARS => write!(f, "DFARS"),
        }
    }
}

/// Compliance audit result
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ComplianceResult {
    /// Framework audited
    pub framework: FrameworkType,
    /// Total controls
    pub total_controls: usize,
    /// Compliant controls
    pub compliant_controls: usize,
    /// Non-compliant controls
    pub non_compliant_controls: usize,
    /// In-progress controls
    pub in_progress_controls: usize,
    /// Not applicable controls
    pub not_applicable_controls: usize,
    /// Violations found
    pub violations: Vec<ComplianceViolation>,
    /// Audit timestamp (ISO 8601)
    pub audit_timestamp: String,
    /// Compliance percentage (0-100)
    pub compliance_percentage: f64,
}

impl ComplianceResult {
    /// Check if system is fully compliant
    pub fn is_compliant(&self) -> bool {
        self.non_compliant_controls == 0 && self.violations.is_empty()
    }

    /// Get high-severity violations
    pub fn high_severity_violations(&self) -> Vec<&ComplianceViolation> {
        self.violations
            .iter()
            .filter(|v| v.severity == "High")
            .collect()
    }

    /// Get remediation summary
    pub fn remediation_summary(&self) -> String {
        if self.is_compliant() {
            return format!("System is fully compliant with {}", self.framework);
        }

        let high_count = self.high_severity_violations().len();
        let medium_count = self
            .violations
            .iter()
            .filter(|v| v.severity == "Medium")
            .count();
        let low_count = self
            .violations
            .iter()
            .filter(|v| v.severity == "Low")
            .count();

        format!(
            "{}: {} violations found ({} high, {} medium, {} low)",
            self.framework,
            self.violations.len(),
            high_count,
            medium_count,
            low_count
        )
    }
}

/// Compliance violation details
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ComplianceViolation {
    /// Control ID (e.g., "SI-4" for NIST 800-53)
    pub control_id: String,
    /// Control description
    pub control_description: String,
    /// What failed
    pub violation_description: String,
    /// Remediation steps
    pub remediation: Vec<String>,
    /// Severity: High, Medium, Low
    pub severity: String,
    /// Target remediation date
    pub target_date: String,
}

/// Compliance framework validator
#[derive(Debug)]
pub struct ComplianceFramework {
    framework_type: FrameworkType,
}

impl ComplianceFramework {
    /// Create FISMA compliance framework
    pub fn fisma() -> Self {
        Self {
            framework_type: FrameworkType::FISMA,
        }
    }

    /// Create FedRAMP compliance framework
    pub fn fedramp() -> Self {
        Self {
            framework_type: FrameworkType::FedRAMP,
        }
    }

    /// Create SOC 2 Type II compliance framework
    pub fn soc2_type_ii() -> Self {
        Self {
            framework_type: FrameworkType::SOC2TypeII,
        }
    }

    /// Create HIPAA compliance framework
    pub fn hipaa() -> Self {
        Self {
            framework_type: FrameworkType::HIPAA,
        }
    }

    /// Create 21 CFR Part 11 compliance framework
    pub fn cfr_part_11() -> Self {
        Self {
            framework_type: FrameworkType::CFRPart11,
        }
    }

    /// Create NIST 800-53 compliance framework
    pub fn nist_800_53() -> Self {
        Self {
            framework_type: FrameworkType::NIST80053,
        }
    }

    /// Create DFARS compliance framework
    pub fn dfars() -> Self {
        Self {
            framework_type: FrameworkType::DFARS,
        }
    }

    /// Get framework type
    pub fn framework_type(&self) -> FrameworkType {
        self.framework_type
    }

    /// Run compliance audit (stub - to be implemented)
    pub async fn audit(&self) -> Result<ComplianceResult> {
        // This will be implemented with actual control evaluation
        Ok(ComplianceResult {
            framework: self.framework_type,
            total_controls: 0,
            compliant_controls: 0,
            non_compliant_controls: 0,
            in_progress_controls: 0,
            not_applicable_controls: 0,
            violations: Vec::new(),
            audit_timestamp: chrono::Utc::now().to_rfc3339(),
            compliance_percentage: 100.0,
        })
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_framework_creation() {
        let fisma = ComplianceFramework::fisma();
        assert_eq!(fisma.framework_type(), FrameworkType::FISMA);

        let fedramp = ComplianceFramework::fedramp();
        assert_eq!(fedramp.framework_type(), FrameworkType::FedRAMP);

        let soc2 = ComplianceFramework::soc2_type_ii();
        assert_eq!(soc2.framework_type(), FrameworkType::SOC2TypeII);
    }

    #[test]
    fn test_compliance_result_compliant() {
        let result = ComplianceResult {
            framework: FrameworkType::FISMA,
            total_controls: 100,
            compliant_controls: 100,
            non_compliant_controls: 0,
            in_progress_controls: 0,
            not_applicable_controls: 0,
            violations: Vec::new(),
            audit_timestamp: "2026-01-25T12:00:00Z".to_string(),
            compliance_percentage: 100.0,
        };
        assert!(result.is_compliant());
    }

    #[test]
    fn test_compliance_result_non_compliant() {
        let result = ComplianceResult {
            framework: FrameworkType::FISMA,
            total_controls: 100,
            compliant_controls: 95,
            non_compliant_controls: 5,
            in_progress_controls: 0,
            not_applicable_controls: 0,
            violations: vec![ComplianceViolation {
                control_id: "SI-4".to_string(),
                control_description: "System Monitoring".to_string(),
                violation_description: "No monitoring configured".to_string(),
                remediation: vec!["Enable monitoring".to_string()],
                severity: "High".to_string(),
                target_date: "2026-02-01".to_string(),
            }],
            audit_timestamp: "2026-01-25T12:00:00Z".to_string(),
            compliance_percentage: 95.0,
        };
        assert!(!result.is_compliant());
        assert_eq!(result.high_severity_violations().len(), 1);
    }

    #[test]
    fn test_framework_type_display() {
        assert_eq!(FrameworkType::FISMA.to_string(), "FISMA");
        assert_eq!(FrameworkType::FedRAMP.to_string(), "FedRAMP");
        assert_eq!(FrameworkType::SOC2TypeII.to_string(), "SOC 2 Type II");
        assert_eq!(FrameworkType::HIPAA.to_string(), "HIPAA");
        assert_eq!(FrameworkType::CFRPart11.to_string(), "21 CFR Part 11");
        assert_eq!(FrameworkType::NIST80053.to_string(), "NIST 800-53");
        assert_eq!(FrameworkType::DFARS.to_string(), "DFARS");
    }
}
