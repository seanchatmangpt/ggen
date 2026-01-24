// supply_chain.rs - Supply chain security utilities
//
// Week 8: Dependency security and supply chain protection
// Provides utilities for typosquatting detection, license compliance, and SBOM analysis

use crate::error::{Error, Result};
use serde::{Deserialize, Serialize};
use std::collections::{HashMap, HashSet};

/// Supply chain security configuration
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct SupplyChainConfig {
    /// Maximum allowed Levenshtein distance for typosquatting detection
    pub typosquat_threshold: usize,
    /// Denied licenses (e.g., GPL-3.0, AGPL-3.0)
    pub denied_licenses: Vec<String>,
    /// Allowed licenses (if empty, all non-denied licenses are allowed)
    pub allowed_licenses: Vec<String>,
    /// Popular crates to check for typosquatting
    pub popular_crates: Vec<String>,
}

impl Default for SupplyChainConfig {
    fn default() -> Self {
        Self {
            typosquat_threshold: 2,
            denied_licenses: vec![
                "GPL-3.0".to_string(),
                "AGPL-3.0".to_string(),
                "SSPL-1.0".to_string(),
            ],
            allowed_licenses: vec![],
            popular_crates: vec![
                "serde".to_string(),
                "tokio".to_string(),
                "clap".to_string(),
                "anyhow".to_string(),
                "thiserror".to_string(),
                "reqwest".to_string(),
                "async-trait".to_string(),
                "tracing".to_string(),
            ],
        }
    }
}

/// Dependency information
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Dependency {
    pub name: String,
    pub version: String,
    pub license: Option<String>,
    pub checksum: Option<String>,
}

/// Typosquatting detection result
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct TyposquattingResult {
    pub suspicious_dependencies: Vec<SuspiciousDependency>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct SuspiciousDependency {
    pub name: String,
    pub reason: String,
    pub similar_to: Option<String>,
    pub distance: Option<usize>,
}

/// License compliance check result
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct LicenseComplianceResult {
    pub compliant: bool,
    pub violations: Vec<LicenseViolation>,
    pub license_distribution: HashMap<String, usize>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct LicenseViolation {
    pub dependency: String,
    pub license: String,
    pub reason: String,
}

/// Calculate Levenshtein distance between two strings
pub fn levenshtein_distance(s1: &str, s2: &str) -> usize {
    let len1 = s1.len();
    let len2 = s2.len();

    let mut matrix = vec![vec![0; len2 + 1]; len1 + 1];

    // Initialize first column and row
    for i in 0..=len1 {
        matrix[i][0] = i;
    }
    for j in 0..=len2 {
        matrix[0][j] = j;
    }

    // Calculate distances
    for i in 1..=len1 {
        for j in 1..=len2 {
            let cost = if s1.chars().nth(i - 1) == s2.chars().nth(j - 1) { 0 } else { 1 };
            matrix[i][j] = std::cmp::min(
                std::cmp::min(
                    matrix[i - 1][j] + 1,      // deletion
                    matrix[i][j - 1] + 1,      // insertion
                ),
                matrix[i - 1][j - 1] + cost,   // substitution
            );
        }
    }

    matrix[len1][len2]
}

/// Detect potential typosquatting in dependencies
pub fn detect_typosquatting(
    dependencies: &[Dependency],
    config: &SupplyChainConfig,
) -> Result<TyposquattingResult> {
    let mut suspicious = Vec::new();
    let popular_set: HashSet<_> = config.popular_crates.iter().collect();

    for dep in dependencies {
        // Skip if dependency is in popular list (exact match)
        if popular_set.contains(&dep.name) {
            continue;
        }

        // Check for suspicious patterns
        let mut reason = None;

        // Pattern 1: Adding suffix
        if dep.name.ends_with("_rs") || dep.name.ends_with("-rs") {
            reason = Some(format!("Suspicious suffix pattern: {}", dep.name));
        }

        // Pattern 2: Adding prefix
        if dep.name.starts_with("rust_") || dep.name.starts_with("r_") || dep.name.starts_with("lib") {
            reason = Some(format!("Suspicious prefix pattern: {}", dep.name));
        }

        // Pattern 3: Similar to popular crate (Levenshtein distance)
        let mut min_distance = usize::MAX;
        let mut similar_crate = None;

        for popular in &config.popular_crates {
            let distance = levenshtein_distance(&dep.name, popular);
            if distance < min_distance && distance <= config.typosquat_threshold && dep.name != *popular {
                min_distance = distance;
                similar_crate = Some(popular.clone());
            }
        }

        if let Some(ref similar) = similar_crate {
            reason = Some(format!("Similar to popular crate '{}' (distance: {})", similar, min_distance));
        }

        // Add to suspicious list if flagged
        if let Some(r) = reason {
            suspicious.push(SuspiciousDependency {
                name: dep.name.clone(),
                reason: r,
                similar_to: similar_crate,
                distance: if min_distance != usize::MAX { Some(min_distance) } else { None },
            });
        }
    }

    Ok(TyposquattingResult {
        suspicious_dependencies: suspicious,
    })
}

/// Check license compliance
pub fn check_license_compliance(
    dependencies: &[Dependency],
    config: &SupplyChainConfig,
) -> Result<LicenseComplianceResult> {
    let mut violations = Vec::new();
    let mut license_dist = HashMap::new();

    for dep in dependencies {
        let license = dep.license.as_deref().unwrap_or("Unknown");

        // Count license distribution
        *license_dist.entry(license.to_string()).or_insert(0) += 1;

        // Check for denied licenses
        if config.denied_licenses.iter().any(|l| l == license) {
            violations.push(LicenseViolation {
                dependency: dep.name.clone(),
                license: license.to_string(),
                reason: format!("License '{}' is in denied list", license),
            });
        }

        // Check for allowed licenses (if specified)
        if !config.allowed_licenses.is_empty()
            && !config.allowed_licenses.iter().any(|l| l == license)
            && license != "Unknown" {
            violations.push(LicenseViolation {
                dependency: dep.name.clone(),
                license: license.to_string(),
                reason: format!("License '{}' is not in allowed list", license),
            });
        }
    }

    Ok(LicenseComplianceResult {
        compliant: violations.is_empty(),
        violations,
        license_distribution: license_dist,
    })
}

/// Verify dependency checksum
pub fn verify_checksum(dependency: &Dependency, expected_checksum: &str) -> Result<bool> {
    match &dependency.checksum {
        Some(checksum) => Ok(checksum == expected_checksum),
        None => Err(Error::new(&format!(
            "Dependency '{}' has no checksum",
            dependency.name
        ))),
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_levenshtein_distance_identical() {
        let distance = levenshtein_distance("serde", "serde");
        assert_eq!(distance, 0);
    }

    #[test]
    fn test_levenshtein_distance_one_change() {
        let distance = levenshtein_distance("serde", "serda");
        assert_eq!(distance, 1);
    }

    #[test]
    fn test_levenshtein_distance_typosquat() {
        let distance = levenshtein_distance("tokio", "toklo");
        assert_eq!(distance, 1);
    }

    #[test]
    fn test_detect_typosquatting_clean() {
        let deps = vec![
            Dependency {
                name: "serde".to_string(),
                version: "1.0.0".to_string(),
                license: Some("MIT".to_string()),
                checksum: None,
            },
        ];

        let config = SupplyChainConfig::default();
        let result = detect_typosquatting(&deps, &config).unwrap();

        assert!(result.suspicious_dependencies.is_empty());
    }

    #[test]
    fn test_detect_typosquatting_suspicious_suffix() {
        let deps = vec![
            Dependency {
                name: "serde_rs".to_string(),
                version: "1.0.0".to_string(),
                license: Some("MIT".to_string()),
                checksum: None,
            },
        ];

        let config = SupplyChainConfig::default();
        let result = detect_typosquatting(&deps, &config).unwrap();

        assert_eq!(result.suspicious_dependencies.len(), 1);
        assert!(result.suspicious_dependencies[0].reason.contains("suffix"));
    }

    #[test]
    fn test_detect_typosquatting_similar() {
        let deps = vec![
            Dependency {
                name: "serda".to_string(),
                version: "1.0.0".to_string(),
                license: Some("MIT".to_string()),
                checksum: None,
            },
        ];

        let config = SupplyChainConfig::default();
        let result = detect_typosquatting(&deps, &config).unwrap();

        assert_eq!(result.suspicious_dependencies.len(), 1);
        assert_eq!(result.suspicious_dependencies[0].similar_to, Some("serde".to_string()));
        assert_eq!(result.suspicious_dependencies[0].distance, Some(1));
    }

    #[test]
    fn test_license_compliance_allowed() {
        let deps = vec![
            Dependency {
                name: "serde".to_string(),
                version: "1.0.0".to_string(),
                license: Some("MIT".to_string()),
                checksum: None,
            },
        ];

        let config = SupplyChainConfig::default();
        let result = check_license_compliance(&deps, &config).unwrap();

        assert!(result.compliant);
        assert!(result.violations.is_empty());
    }

    #[test]
    fn test_license_compliance_denied() {
        let deps = vec![
            Dependency {
                name: "bad-crate".to_string(),
                version: "1.0.0".to_string(),
                license: Some("GPL-3.0".to_string()),
                checksum: None,
            },
        ];

        let config = SupplyChainConfig::default();
        let result = check_license_compliance(&deps, &config).unwrap();

        assert!(!result.compliant);
        assert_eq!(result.violations.len(), 1);
        assert_eq!(result.violations[0].license, "GPL-3.0");
    }

    #[test]
    fn test_verify_checksum_match() {
        let dep = Dependency {
            name: "serde".to_string(),
            version: "1.0.0".to_string(),
            license: Some("MIT".to_string()),
            checksum: Some("abc123".to_string()),
        };

        let result = verify_checksum(&dep, "abc123").unwrap();
        assert!(result);
    }

    #[test]
    fn test_verify_checksum_mismatch() {
        let dep = Dependency {
            name: "serde".to_string(),
            version: "1.0.0".to_string(),
            license: Some("MIT".to_string()),
            checksum: Some("abc123".to_string()),
        };

        let result = verify_checksum(&dep, "xyz789").unwrap();
        assert!(!result);
    }
}
