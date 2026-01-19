//! Entity mapping from customer domain to standard ontology
//!
//! Maps domain-specific entities to standardized ontology classes
//! with deterministic confidence scoring.

use crate::errors::Result;
use serde::{Deserialize, Serialize};

/// A confidence score for entity mapping (0.0 to 1.0)
pub type Score = f32;

/// Maps customer domain entities to ontology classes
///
/// Provides deterministic entity-to-ontology mapping with confidence scores.
/// Same entity always maps to same ontology classes with same scores.
#[derive(Debug, Clone)]
pub struct EntityMapper;

/// Represents an ontology class with matching metadata
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
pub struct OntologyMatch {
    /// The matched ontology class identifier
    pub class: String,
    /// Human-readable class label
    pub label: String,
    /// Confidence score (0.0-1.0)
    pub score: Score,
    /// Reason for the match
    pub reason: String,
}

impl EntityMapper {
    /// Maps a policy entity to matching ontology classes
    ///
    /// # Arguments
    /// * `policy_name` - Policy name or identifier
    ///
    /// # Returns
    /// Vector of (OntologyClass, Score) tuples sorted by score (highest first)
    ///
    /// # Determinism
    /// Same policy name always produces identical results
    pub fn match_policy(policy_name: &str) -> Result<Vec<OntologyMatch>> {
        let normalized = policy_name.to_lowercase();
        let mut matches = Vec::new();

        // Policy type matching
        if normalized.contains("privacy") || normalized.contains("gdpr") {
            matches.push(OntologyMatch {
                class: ":PrivacyPolicy".to_string(),
                label: "Privacy Policy".to_string(),
                score: 0.95,
                reason: "Contains 'privacy' keyword".to_string(),
            });
            matches.push(OntologyMatch {
                class: ":DataProtectionPolicy".to_string(),
                label: "Data Protection Policy".to_string(),
                score: 0.85,
                reason: "Related to privacy and GDPR".to_string(),
            });
        }

        if normalized.contains("security") {
            matches.push(OntologyMatch {
                class: ":SecurityPolicy".to_string(),
                label: "Security Policy".to_string(),
                score: 0.90,
                reason: "Contains 'security' keyword".to_string(),
            });
        }

        if normalized.contains("access") || normalized.contains("iam") {
            matches.push(OntologyMatch {
                class: ":AccessControlPolicy".to_string(),
                label: "Access Control Policy".to_string(),
                score: 0.88,
                reason: "Access control related".to_string(),
            });
        }

        if normalized.contains("encryption") {
            matches.push(OntologyMatch {
                class: ":EncryptionPolicy".to_string(),
                label: "Encryption Policy".to_string(),
                score: 0.92,
                reason: "Explicitly mentions encryption".to_string(),
            });
        }

        // Default match
        if matches.is_empty() {
            matches.push(OntologyMatch {
                class: ":Policy".to_string(),
                label: "Generic Policy".to_string(),
                score: 0.50,
                reason: "Generic policy match".to_string(),
            });
        }

        // Sort by score descending (deterministically)
        matches.sort_by(|a, b| b.score.partial_cmp(&a.score).unwrap_or(std::cmp::Ordering::Equal));

        Ok(matches)
    }

    /// Maps a data classification label to matching ontology classes
    ///
    /// # Arguments
    /// * `label` - Classification label (e.g., "Public", "Confidential", "Restricted")
    ///
    /// # Returns
    /// Vector of (LegalClass, Score) tuples sorted by score (highest first)
    pub fn match_data_classification(label: &str) -> Result<Vec<OntologyMatch>> {
        let normalized = label.to_lowercase().trim().to_string();
        let mut matches = Vec::new();

        match normalized.as_str() {
            "public" => {
                matches.push(OntologyMatch {
                    class: ":PublicData".to_string(),
                    label: "Public Data".to_string(),
                    score: 1.0,
                    reason: "Exact match for public data".to_string(),
                });
                matches.push(OntologyMatch {
                    class: ":UnrestrictedData".to_string(),
                    label: "Unrestricted Data".to_string(),
                    score: 0.85,
                    reason: "Public data is unrestricted".to_string(),
                });
            }
            "confidential" => {
                matches.push(OntologyMatch {
                    class: ":ConfidentialData".to_string(),
                    label: "Confidential Data".to_string(),
                    score: 1.0,
                    reason: "Exact match for confidential data".to_string(),
                });
                matches.push(OntologyMatch {
                    class: ":RestrictedData".to_string(),
                    label: "Restricted Data".to_string(),
                    score: 0.90,
                    reason: "Confidential implies restricted access".to_string(),
                });
            }
            "restricted" | "secret" => {
                matches.push(OntologyMatch {
                    class: ":RestrictedData".to_string(),
                    label: "Restricted Data".to_string(),
                    score: 0.95,
                    reason: "Exact or close match for restricted data".to_string(),
                });
                matches.push(OntologyMatch {
                    class: ":SensitiveData".to_string(),
                    label: "Sensitive Data".to_string(),
                    score: 0.85,
                    reason: "Restricted data is sensitive".to_string(),
                });
            }
            _ => {
                matches.push(OntologyMatch {
                    class: ":DataClassification".to_string(),
                    label: "Data Classification".to_string(),
                    score: 0.60,
                    reason: format!("Unknown classification: {}", label),
                });
            }
        }

        // Sort by score descending
        matches.sort_by(|a, b| b.score.partial_cmp(&a.score).unwrap_or(std::cmp::Ordering::Equal));
        Ok(matches)
    }

    /// Maps a service level agreement to matching ontology classes
    ///
    /// # Arguments
    /// * `availability_percentage` - Availability requirement (0.0-100.0)
    ///
    /// # Returns
    /// Vector of service level class matches
    pub fn match_service_level(availability_percentage: f32) -> Result<Vec<OntologyMatch>> {
        let mut matches = Vec::new();

        if availability_percentage >= 99.99 {
            matches.push(OntologyMatch {
                class: ":CriticalService".to_string(),
                label: "Critical Service (4-9s uptime)".to_string(),
                score: 0.98,
                reason: "99.99%+ availability".to_string(),
            });
        } else if availability_percentage >= 99.9 {
            matches.push(OntologyMatch {
                class: ":HighAvailabilityService".to_string(),
                label: "High Availability Service".to_string(),
                score: 0.95,
                reason: "99.9%+ availability".to_string(),
            });
        } else if availability_percentage >= 99.0 {
            matches.push(OntologyMatch {
                class: ":StandardService".to_string(),
                label: "Standard Service".to_string(),
                score: 0.90,
                reason: "99%+ availability".to_string(),
            });
        } else if availability_percentage >= 95.0 {
            matches.push(OntologyMatch {
                class: ":BasicService".to_string(),
                label: "Basic Service".to_string(),
                score: 0.80,
                reason: "95%+ availability".to_string(),
            });
        } else {
            matches.push(OntologyMatch {
                class: ":DevelopmentService".to_string(),
                label: "Development Service".to_string(),
                score: 0.70,
                reason: "Below standard SLAs".to_string(),
            });
        }

        Ok(matches)
    }

    /// Maps a security control to matching ontology classes
    ///
    /// # Arguments
    /// * `control_name` - Control name or description
    ///
    /// # Returns
    /// Vector of security control class matches
    pub fn match_security_control(control_name: &str) -> Result<Vec<OntologyMatch>> {
        let normalized = control_name.to_lowercase();
        let mut matches = Vec::new();

        if normalized.contains("mfa") || normalized.contains("multi-factor") {
            matches.push(OntologyMatch {
                class: ":MultiFactorAuthentication".to_string(),
                label: "Multi-Factor Authentication".to_string(),
                score: 0.98,
                reason: "Explicit MFA reference".to_string(),
            });
            matches.push(OntologyMatch {
                class: ":AuthenticationControl".to_string(),
                label: "Authentication Control".to_string(),
                score: 0.88,
                reason: "Part of authentication".to_string(),
            });
        }

        if normalized.contains("encryption") || normalized.contains("tls") {
            matches.push(OntologyMatch {
                class: ":EncryptionControl".to_string(),
                label: "Encryption Control".to_string(),
                score: 0.96,
                reason: "Encryption-based security".to_string(),
            });
        }

        if normalized.contains("audit") || normalized.contains("logging") {
            matches.push(OntologyMatch {
                class: ":AuditLogging".to_string(),
                label: "Audit Logging".to_string(),
                score: 0.94,
                reason: "Logging and audit control".to_string(),
            });
        }

        if matches.is_empty() {
            matches.push(OntologyMatch {
                class: ":SecurityControl".to_string(),
                label: "Security Control".to_string(),
                score: 0.65,
                reason: "Generic security control".to_string(),
            });
        }

        matches.sort_by(|a, b| b.score.partial_cmp(&a.score).unwrap_or(std::cmp::Ordering::Equal));
        Ok(matches)
    }

    /// Maps a compute service to matching ontology classes
    ///
    /// # Arguments
    /// * `compute_type` - Compute type (e.g., "VM", "Container", "Serverless")
    ///
    /// # Returns
    /// Vector of compute class matches
    pub fn match_compute_service(compute_type: &str) -> Result<Vec<OntologyMatch>> {
        let normalized = compute_type.to_lowercase();
        let mut matches = Vec::new();

        if normalized.contains("vm") || normalized.contains("virtual machine") {
            matches.push(OntologyMatch {
                class: ":VirtualMachine".to_string(),
                label: "Virtual Machine".to_string(),
                score: 0.98,
                reason: "Explicit VM reference".to_string(),
            });
        }

        if normalized.contains("container") || normalized.contains("docker") {
            matches.push(OntologyMatch {
                class: ":ContainerizedService".to_string(),
                label: "Containerized Service".to_string(),
                score: 0.98,
                reason: "Container-based deployment".to_string(),
            });
        }

        if normalized.contains("serverless") || normalized.contains("lambda") {
            matches.push(OntologyMatch {
                class: ":ServerlessCompute".to_string(),
                label: "Serverless Compute".to_string(),
                score: 0.98,
                reason: "Serverless execution model".to_string(),
            });
        }

        if normalized.contains("kubernetes") || normalized.contains("k8s") {
            matches.push(OntologyMatch {
                class: ":KubernetesCluster".to_string(),
                label: "Kubernetes Cluster".to_string(),
                score: 0.97,
                reason: "Kubernetes orchestration".to_string(),
            });
        }

        if matches.is_empty() {
            matches.push(OntologyMatch {
                class: ":ComputeService".to_string(),
                label: "Compute Service".to_string(),
                score: 0.60,
                reason: format!("Unknown compute type: {}", compute_type),
            });
        }

        matches.sort_by(|a, b| b.score.partial_cmp(&a.score).unwrap_or(std::cmp::Ordering::Equal));
        Ok(matches)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_match_policy_privacy_determinism() {
        let result1 = EntityMapper::match_policy("Privacy Policy");
        let result2 = EntityMapper::match_policy("Privacy Policy");
        assert_eq!(result1.unwrap(), result2.unwrap());
    }

    #[test]
    fn test_match_policy_privacy() {
        let matches = EntityMapper::match_policy("Privacy Policy").unwrap();
        assert!(!matches.is_empty());
        assert!(matches[0].score >= 0.85);
    }

    #[test]
    fn test_match_policy_security() {
        let matches = EntityMapper::match_policy("Security Policy").unwrap();
        assert!(!matches.is_empty());
        assert_eq!(matches[0].class, ":SecurityPolicy");
    }

    #[test]
    fn test_match_data_classification_exact() {
        let matches = EntityMapper::match_data_classification("Public").unwrap();
        assert_eq!(matches[0].score, 1.0);
        assert_eq!(matches[0].class, ":PublicData");
    }

    #[test]
    fn test_match_data_classification_determinism() {
        let result1 = EntityMapper::match_data_classification("Confidential").unwrap();
        let result2 = EntityMapper::match_data_classification("Confidential").unwrap();
        assert_eq!(result1, result2);
    }

    #[test]
    fn test_match_service_level_critical() {
        let matches = EntityMapper::match_service_level(99.99).unwrap();
        assert_eq!(matches[0].class, ":CriticalService");
    }

    #[test]
    fn test_match_service_level_sorted() {
        let matches = EntityMapper::match_service_level(95.0).unwrap();
        for i in 0..matches.len() - 1 {
            assert!(matches[i].score >= matches[i + 1].score);
        }
    }

    #[test]
    fn test_match_security_control_mfa() {
        let matches = EntityMapper::match_security_control("MFA").unwrap();
        assert!(!matches.is_empty());
        assert_eq!(matches[0].class, ":MultiFactorAuthentication");
    }

    #[test]
    fn test_match_compute_service_vm() {
        let matches = EntityMapper::match_compute_service("VM").unwrap();
        assert_eq!(matches[0].class, ":VirtualMachine");
    }

    #[test]
    fn test_match_compute_service_kubernetes() {
        let matches = EntityMapper::match_compute_service("Kubernetes").unwrap();
        assert_eq!(matches[0].class, ":KubernetesCluster");
    }
}
