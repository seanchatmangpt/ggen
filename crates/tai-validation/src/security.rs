//! Automated security scanning (SAST, DAST, dependency scanning)

use crate::error::Result;
use serde::{Deserialize, Serialize};

/// Security scanner for vulnerabilities
#[derive(Debug)]
pub struct SecurityScanner {
    scan_type: ScanType,
}

/// Type of security scan
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ScanType {
    /// Static Application Security Testing
    SAST,
    /// Dynamic Application Security Testing
    DAST,
    /// Dependency scanning
    DependencyScan,
    /// Software Bill of Materials generation
    SBOM,
}

/// Security vulnerability
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Vulnerability {
    /// Vulnerability ID (CVE, etc.)
    pub id: String,
    /// Vulnerability title
    pub title: String,
    /// Severity: Critical, High, Medium, Low
    pub severity: String,
    /// Detailed description
    pub description: String,
    /// Affected component
    pub affected_component: String,
    /// Remediation steps
    pub remediation: Vec<String>,
    /// Discovery date (ISO 8601)
    pub discovered_at: String,
}

/// Security scan result
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct SecurityScanResult {
    /// Scan type
    pub scan_type: String,
    /// Total vulnerabilities found
    pub total_vulnerabilities: usize,
    /// Critical vulnerabilities
    pub critical_count: usize,
    /// High severity vulnerabilities
    pub high_count: usize,
    /// Medium severity vulnerabilities
    pub medium_count: usize,
    /// Low severity vulnerabilities
    pub low_count: usize,
    /// Vulnerabilities found
    pub vulnerabilities: Vec<Vulnerability>,
    /// Scan timestamp (ISO 8601)
    pub scanned_at: String,
    /// Is compliant (no critical/high vulns)
    pub is_compliant: bool,
}

impl SecurityScanResult {
    /// Check if scan is compliant
    pub fn is_compliant(&self) -> bool {
        self.critical_count == 0 && self.high_count == 0
    }

    /// Get critical vulnerabilities
    pub fn critical_vulnerabilities(&self) -> Vec<&Vulnerability> {
        self.vulnerabilities
            .iter()
            .filter(|v| v.severity == "Critical")
            .collect()
    }

    /// Get summary
    pub fn summary(&self) -> String {
        format!(
            "Scan found: {} total ({} critical, {} high, {} medium, {} low)",
            self.total_vulnerabilities, self.critical_count, self.high_count, self.medium_count, self.low_count
        )
    }
}

impl SecurityScanner {
    /// Get scan type
    pub fn scan_type(&self) -> ScanType {
        self.scan_type
    }

    /// Create new SAST scanner
    pub fn sast() -> Self {
        Self {
            scan_type: ScanType::SAST,
        }
    }

    /// Create new DAST scanner
    pub fn dast() -> Self {
        Self {
            scan_type: ScanType::DAST,
        }
    }

    /// Create new dependency scanner
    pub fn dependency_scan() -> Self {
        Self {
            scan_type: ScanType::DependencyScan,
        }
    }

    /// Create new SBOM generator
    pub fn sbom() -> Self {
        Self {
            scan_type: ScanType::SBOM,
        }
    }

    /// Run security scan (stub - to be implemented)
    pub async fn scan(&self) -> Result<SecurityScanResult> {
        Ok(SecurityScanResult {
            scan_type: format!("{:?}", self.scan_type),
            total_vulnerabilities: 0,
            critical_count: 0,
            high_count: 0,
            medium_count: 0,
            low_count: 0,
            vulnerabilities: Vec::new(),
            scanned_at: chrono::Utc::now().to_rfc3339(),
            is_compliant: true,
        })
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_scanner_creation() {
        let sast = SecurityScanner::sast();
        assert_eq!(sast.scan_type, ScanType::SAST);

        let dast = SecurityScanner::dast();
        assert_eq!(dast.scan_type, ScanType::DAST);

        let deps = SecurityScanner::dependency_scan();
        assert_eq!(deps.scan_type, ScanType::DependencyScan);
    }

    #[test]
    fn test_scan_result_compliant() {
        let result = SecurityScanResult {
            scan_type: "SAST".to_string(),
            total_vulnerabilities: 0,
            critical_count: 0,
            high_count: 0,
            medium_count: 0,
            low_count: 0,
            vulnerabilities: Vec::new(),
            scanned_at: chrono::Utc::now().to_rfc3339(),
            is_compliant: true,
        };
        assert!(result.is_compliant());
    }

    #[test]
    fn test_scan_result_non_compliant() {
        let result = SecurityScanResult {
            scan_type: "SAST".to_string(),
            total_vulnerabilities: 2,
            critical_count: 1,
            high_count: 1,
            medium_count: 0,
            low_count: 0,
            vulnerabilities: Vec::new(),
            scanned_at: chrono::Utc::now().to_rfc3339(),
            is_compliant: false,
        };
        assert!(!result.is_compliant());
    }
}
