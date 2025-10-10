//! Audit Agent - Compliance Checking and Security Auditing
//!
//! This agent provides comprehensive auditing and compliance checking for the MCP server:
//! - Security audit and vulnerability scanning
//! - Compliance checking against standards
//! - Access control and permission auditing
//! - Data privacy and GDPR compliance
//! - Audit trail and logging
//!
//! # Audit Patterns
//!
//! ## Security Auditing
//! - **Vulnerability Scanning** - Identify security vulnerabilities
//! - **Access Control Review** - Audit user permissions and access
//! - **Data Protection** - Ensure data privacy compliance
//! - **Threat Detection** - Identify potential security threats
//!
//! ## Compliance Checking
//! - **Standards Compliance** - Check against industry standards
//! - **Regulatory Compliance** - Ensure regulatory requirements
//! - **Policy Enforcement** - Enforce security policies
//! - **Risk Assessment** - Assess security risks
//!
//! ## Audit Trail
//! - **Event Logging** - Log all security events
//! - **Access Logging** - Track user access and actions
//! - **Change Tracking** - Monitor configuration changes
//! - **Incident Response** - Track security incidents

use crate::agents::{Agent, AgentMetadata, AgentStatus, AgentId};
use crate::error::{GgenMcpError, Result};
use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use uuid::Uuid;
use chrono::Utc;

/// Audit result
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct AuditResult {
    pub audit_id: Uuid,
    pub audit_type: AuditType,
    pub status: AuditStatus,
    pub findings: Vec<AuditFinding>,
    pub recommendations: Vec<AuditRecommendation>,
    pub compliance_score: f64,
    pub risk_level: RiskLevel,
    pub start_time: chrono::DateTime<Utc>,
    pub end_time: chrono::DateTime<Utc>,
    pub duration_ms: u64,
    pub metadata: HashMap<String, serde_json::Value>,
}

/// Audit types
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
pub enum AuditType {
    Security,
    Compliance,
    AccessControl,
    DataPrivacy,
    Performance,
    Configuration,
}

/// Audit status
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
pub enum AuditStatus {
    Pending,
    Running,
    Completed,
    Failed,
    Cancelled,
}

/// Risk levels
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
pub enum RiskLevel {
    Low,
    Medium,
    High,
    Critical,
}

/// Audit finding
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct AuditFinding {
    pub id: Uuid,
    pub finding_type: FindingType,
    pub severity: FindingSeverity,
    pub title: String,
    pub description: String,
    pub affected_component: String,
    pub evidence: Vec<String>,
    pub impact: String,
    pub likelihood: f64,
    pub risk_score: f64,
    pub discovered_at: chrono::DateTime<Utc>,
    pub status: FindingStatus,
}

/// Finding types
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
pub enum FindingType {
    Vulnerability,
    Misconfiguration,
    PolicyViolation,
    AccessViolation,
    DataExposure,
    PerformanceIssue,
    ComplianceGap,
}

/// Finding severity
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
pub enum FindingSeverity {
    Info,
    Low,
    Medium,
    High,
    Critical,
}

/// Finding status
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
pub enum FindingStatus {
    Open,
    InProgress,
    Resolved,
    FalsePositive,
    AcceptedRisk,
}

/// Audit recommendation
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct AuditRecommendation {
    pub id: Uuid,
    pub title: String,
    pub description: String,
    pub priority: RecommendationPriority,
    pub effort: EffortLevel,
    pub impact: ImpactLevel,
    pub category: RecommendationCategory,
    pub steps: Vec<String>,
    pub resources: Vec<String>,
    pub estimated_cost: Option<f64>,
    pub estimated_time: Option<String>,
}

/// Recommendation priority
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
pub enum RecommendationPriority {
    Low,
    Medium,
    High,
    Critical,
}

/// Effort levels
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
pub enum EffortLevel {
    Low,
    Medium,
    High,
    VeryHigh,
}

/// Impact levels
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
pub enum ImpactLevel {
    Low,
    Medium,
    High,
    Critical,
}

/// Recommendation categories
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
pub enum RecommendationCategory {
    Security,
    Performance,
    Compliance,
    Configuration,
    Process,
    Training,
}

/// Compliance standard
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ComplianceStandard {
    pub name: String,
    pub version: String,
    pub description: String,
    pub requirements: Vec<ComplianceRequirement>,
    pub last_updated: chrono::DateTime<Utc>,
}

/// Compliance requirement
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ComplianceRequirement {
    pub id: String,
    pub title: String,
    pub description: String,
    pub category: String,
    pub mandatory: bool,
    pub evidence_required: Vec<String>,
    pub assessment_criteria: Vec<String>,
}

/// Audit Agent implementation
pub struct AuditAgent {
    id: AgentId,
    audit_results: Vec<AuditResult>,
    compliance_standards: HashMap<String, ComplianceStandard>,
    audit_schedules: Vec<AuditSchedule>,
    audit_policies: Vec<AuditPolicy>,
}

/// Audit schedule
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct AuditSchedule {
    pub id: Uuid,
    pub audit_type: AuditType,
    pub frequency: ScheduleFrequency,
    pub next_run: chrono::DateTime<Utc>,
    pub last_run: Option<chrono::DateTime<Utc>>,
    pub enabled: bool,
    pub parameters: HashMap<String, serde_json::Value>,
}

/// Schedule frequency
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
pub enum ScheduleFrequency {
    Daily,
    Weekly,
    Monthly,
    Quarterly,
    Annually,
    OnDemand,
}

/// Audit policy
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct AuditPolicy {
    pub id: Uuid,
    pub name: String,
    pub description: String,
    pub policy_type: PolicyType,
    pub rules: Vec<AuditRule>,
    pub enforcement_level: EnforcementLevel,
    pub created_at: chrono::DateTime<Utc>,
    pub last_updated: chrono::DateTime<Utc>,
}

/// Policy types
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
pub enum PolicyType {
    Security,
    Compliance,
    AccessControl,
    DataProtection,
    Performance,
}

/// Enforcement levels
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
pub enum EnforcementLevel {
    Advisory,
    Mandatory,
    Critical,
}

/// Audit rule
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct AuditRule {
    pub id: String,
    pub name: String,
    pub description: String,
    pub condition: String,
    pub severity: FindingSeverity,
    pub category: String,
    pub remediation: String,
}

impl AuditAgent {
    pub fn new() -> Self {
        let mut agent = Self {
            id: Uuid::new_v4(),
            audit_results: Vec::new(),
            compliance_standards: HashMap::new(),
            audit_schedules: Vec::new(),
            audit_policies: Vec::new(),
        };

        // Initialize compliance standards
        agent.initialize_compliance_standards();
        
        // Initialize audit policies
        agent.initialize_audit_policies();
        
        // Initialize audit schedules
        agent.initialize_audit_schedules();

        agent
    }

    /// Initialize compliance standards
    fn initialize_compliance_standards(&mut self) {
        // GDPR compliance standard
        self.compliance_standards.insert("GDPR".to_string(), ComplianceStandard {
            name: "GDPR".to_string(),
            version: "1.0".to_string(),
            description: "General Data Protection Regulation compliance".to_string(),
            requirements: vec![
                ComplianceRequirement {
                    id: "GDPR-001".to_string(),
                    title: "Data Minimization".to_string(),
                    description: "Collect only necessary personal data".to_string(),
                    category: "Data Protection".to_string(),
                    mandatory: true,
                    evidence_required: vec!["Data collection policy".to_string(), "Data inventory".to_string()],
                    assessment_criteria: vec!["Data collection is limited to necessary purposes".to_string()],
                },
                ComplianceRequirement {
                    id: "GDPR-002".to_string(),
                    title: "Consent Management".to_string(),
                    description: "Obtain explicit consent for data processing".to_string(),
                    category: "Consent".to_string(),
                    mandatory: true,
                    evidence_required: vec!["Consent forms".to_string(), "Consent tracking system".to_string()],
                    assessment_criteria: vec!["Consent is obtained before data processing".to_string()],
                },
            ],
            last_updated: Utc::now(),
        });

        // SOC 2 compliance standard
        self.compliance_standards.insert("SOC2".to_string(), ComplianceStandard {
            name: "SOC 2".to_string(),
            version: "1.0".to_string(),
            description: "SOC 2 Type II compliance".to_string(),
            requirements: vec![
                ComplianceRequirement {
                    id: "SOC2-001".to_string(),
                    title: "Access Controls".to_string(),
                    description: "Implement proper access controls".to_string(),
                    category: "Security".to_string(),
                    mandatory: true,
                    evidence_required: vec!["Access control policy".to_string(), "User access reviews".to_string()],
                    assessment_criteria: vec!["Access controls are properly implemented".to_string()],
                },
            ],
            last_updated: Utc::now(),
        });
    }

    /// Initialize audit policies
    fn initialize_audit_policies(&mut self) {
        // Security audit policy
        self.audit_policies.push(AuditPolicy {
            id: Uuid::new_v4(),
            name: "Security Audit Policy".to_string(),
            description: "Comprehensive security auditing policy".to_string(),
            policy_type: PolicyType::Security,
            rules: vec![
                AuditRule {
                    id: "SEC-001".to_string(),
                    name: "Password Policy".to_string(),
                    description: "Check password complexity requirements".to_string(),
                    condition: "password_complexity >= 8".to_string(),
                    severity: FindingSeverity::High,
                    category: "Authentication".to_string(),
                    remediation: "Implement strong password policy".to_string(),
                },
                AuditRule {
                    id: "SEC-002".to_string(),
                    name: "Access Control".to_string(),
                    description: "Verify access control implementation".to_string(),
                    condition: "access_controls_enabled == true".to_string(),
                    severity: FindingSeverity::Critical,
                    category: "Access Control".to_string(),
                    remediation: "Enable and configure access controls".to_string(),
                },
            ],
            enforcement_level: EnforcementLevel::Mandatory,
            created_at: Utc::now(),
            last_updated: Utc::now(),
        });
    }

    /// Initialize audit schedules
    fn initialize_audit_schedules(&mut self) {
        // Daily security audit
        self.audit_schedules.push(AuditSchedule {
            id: Uuid::new_v4(),
            audit_type: AuditType::Security,
            frequency: ScheduleFrequency::Daily,
            next_run: Utc::now() + chrono::Duration::days(1),
            last_run: None,
            enabled: true,
            parameters: HashMap::from([
                ("scope".to_string(), serde_json::Value::String("full".to_string())),
                ("depth".to_string(), serde_json::Value::String("comprehensive".to_string())),
            ]),
        });

        // Monthly compliance audit
        self.audit_schedules.push(AuditSchedule {
            id: Uuid::new_v4(),
            audit_type: AuditType::Compliance,
            frequency: ScheduleFrequency::Monthly,
            next_run: Utc::now() + chrono::Duration::days(30),
            last_run: None,
            enabled: true,
            parameters: HashMap::from([
                ("standards".to_string(), serde_json::json!(["GDPR", "SOC2"])),
                ("scope".to_string(), serde_json::Value::String("all".to_string())),
            ]),
        });
    }

    /// Perform security audit
    pub async fn perform_security_audit(&mut self, scope: &str, depth: &str) -> Result<AuditResult> {
        let start_time = Utc::now();
        let audit_id = Uuid::new_v4();

        // Simulate security audit
        let findings = self.simulate_security_findings(scope, depth).await;
        let recommendations = self.generate_security_recommendations(&findings);
        let compliance_score = self.calculate_compliance_score(&findings);
        let risk_level = self.determine_risk_level(&findings);

        let end_time = Utc::now();
        let duration = end_time.signed_duration_since(start_time).num_milliseconds() as u64;

        let result = AuditResult {
            audit_id,
            audit_type: AuditType::Security,
            status: AuditStatus::Completed,
            findings,
            recommendations,
            compliance_score,
            risk_level,
            start_time,
            end_time,
            duration_ms: duration,
            metadata: HashMap::from([
                ("scope".to_string(), serde_json::Value::String(scope.to_string())),
                ("depth".to_string(), serde_json::Value::String(depth.to_string())),
                ("auditor".to_string(), serde_json::Value::String("AuditAgent".to_string())),
            ]),
        };

        self.audit_results.push(result.clone());
        
        // Keep only last 1000 audit results
        if self.audit_results.len() > 1000 {
            self.audit_results.remove(0);
        }

        Ok(result)
    }

    /// Perform compliance audit
    pub async fn perform_compliance_audit(&mut self, standards: &[String]) -> Result<AuditResult> {
        let start_time = Utc::now();
        let audit_id = Uuid::new_v4();

        // Simulate compliance audit
        let findings = self.simulate_compliance_findings(standards).await;
        let recommendations = self.generate_compliance_recommendations(&findings);
        let compliance_score = self.calculate_compliance_score(&findings);
        let risk_level = self.determine_risk_level(&findings);

        let end_time = Utc::now();
        let duration = end_time.signed_duration_since(start_time).num_milliseconds() as u64;

        let result = AuditResult {
            audit_id,
            audit_type: AuditType::Compliance,
            status: AuditStatus::Completed,
            findings,
            recommendations,
            compliance_score,
            risk_level,
            start_time,
            end_time,
            duration_ms: duration,
            metadata: HashMap::from([
                ("standards".to_string(), serde_json::to_value(standards).unwrap()),
                ("auditor".to_string(), serde_json::Value::String("AuditAgent".to_string())),
            ]),
        };

        self.audit_results.push(result.clone());
        Ok(result)
    }

    /// Simulate security findings
    async fn simulate_security_findings(&self, scope: &str, depth: &str) -> Vec<AuditFinding> {
        let mut findings = Vec::new();

        // Simulate some security findings
        if scope == "full" {
            findings.push(AuditFinding {
                id: Uuid::new_v4(),
                finding_type: FindingType::Vulnerability,
                severity: FindingSeverity::Medium,
                title: "Outdated Dependencies".to_string(),
                description: "Some dependencies have known vulnerabilities".to_string(),
                affected_component: "dependencies".to_string(),
                evidence: vec!["dependency_scan_results.json".to_string()],
                impact: "Potential security exploitation".to_string(),
                likelihood: 0.3,
                risk_score: 0.6,
                discovered_at: Utc::now(),
                status: FindingStatus::Open,
            });

            findings.push(AuditFinding {
                id: Uuid::new_v4(),
                finding_type: FindingType::Misconfiguration,
                severity: FindingSeverity::Low,
                title: "Weak Password Policy".to_string(),
                description: "Password policy allows weak passwords".to_string(),
                affected_component: "authentication".to_string(),
                evidence: vec!["password_policy_config.json".to_string()],
                impact: "Increased risk of account compromise".to_string(),
                likelihood: 0.2,
                risk_score: 0.4,
                discovered_at: Utc::now(),
                status: FindingStatus::Open,
            });
        }

        findings
    }

    /// Simulate compliance findings
    async fn simulate_compliance_findings(&self, standards: &[String]) -> Vec<AuditFinding> {
        let mut findings = Vec::new();

        for standard in standards {
            if standard == "GDPR" {
                findings.push(AuditFinding {
                    id: Uuid::new_v4(),
                    finding_type: FindingType::ComplianceGap,
                    severity: FindingSeverity::High,
                    title: "Missing Data Processing Records".to_string(),
                    description: "Data processing activities are not properly documented".to_string(),
                    affected_component: "data_processing".to_string(),
                    evidence: vec!["data_processing_audit.log".to_string()],
                    impact: "GDPR compliance violation".to_string(),
                    likelihood: 0.8,
                    risk_score: 0.9,
                    discovered_at: Utc::now(),
                    status: FindingStatus::Open,
                });
            }
        }

        findings
    }

    /// Generate security recommendations
    fn generate_security_recommendations(&self, findings: &[AuditFinding]) -> Vec<AuditRecommendation> {
        let mut recommendations = Vec::new();

        for finding in findings {
            match finding.finding_type {
                FindingType::Vulnerability => {
                    recommendations.push(AuditRecommendation {
                        id: Uuid::new_v4(),
                        title: "Update Vulnerable Dependencies".to_string(),
                        description: "Update all dependencies to latest secure versions".to_string(),
                        priority: RecommendationPriority::High,
                        effort: EffortLevel::Medium,
                        impact: ImpactLevel::High,
                        category: RecommendationCategory::Security,
                        steps: vec![
                            "Run dependency vulnerability scan".to_string(),
                            "Update vulnerable dependencies".to_string(),
                            "Test updated dependencies".to_string(),
                            "Deploy updated application".to_string(),
                        ],
                        resources: vec![
                            "Security team".to_string(),
                            "Development team".to_string(),
                        ],
                        estimated_cost: Some(5000.0),
                        estimated_time: Some("2-3 days".to_string()),
                    });
                }
                FindingType::Misconfiguration => {
                    recommendations.push(AuditRecommendation {
                        id: Uuid::new_v4(),
                        title: "Strengthen Password Policy".to_string(),
                        description: "Implement stronger password requirements".to_string(),
                        priority: RecommendationPriority::Medium,
                        effort: EffortLevel::Low,
                        impact: ImpactLevel::Medium,
                        category: RecommendationCategory::Security,
                        steps: vec![
                            "Update password policy configuration".to_string(),
                            "Notify users of new requirements".to_string(),
                            "Enforce policy changes".to_string(),
                        ],
                        resources: vec![
                            "Security team".to_string(),
                            "IT team".to_string(),
                        ],
                        estimated_cost: Some(1000.0),
                        estimated_time: Some("1 day".to_string()),
                    });
                }
                _ => {}
            }
        }

        recommendations
    }

    /// Generate compliance recommendations
    fn generate_compliance_recommendations(&self, findings: &[AuditFinding]) -> Vec<AuditRecommendation> {
        let mut recommendations = Vec::new();

        for finding in findings {
            if finding.finding_type == FindingType::ComplianceGap {
                recommendations.push(AuditRecommendation {
                    id: Uuid::new_v4(),
                    title: "Implement Data Processing Records".to_string(),
                    description: "Create comprehensive data processing documentation".to_string(),
                    priority: RecommendationPriority::Critical,
                    effort: EffortLevel::High,
                    impact: ImpactLevel::Critical,
                    category: RecommendationCategory::Compliance,
                    steps: vec![
                        "Document all data processing activities".to_string(),
                        "Create data processing records".to_string(),
                        "Implement record maintenance process".to_string(),
                        "Train staff on compliance requirements".to_string(),
                    ],
                    resources: vec![
                        "Compliance team".to_string(),
                        "Legal team".to_string(),
                        "Data protection officer".to_string(),
                    ],
                    estimated_cost: Some(15000.0),
                    estimated_time: Some("2-4 weeks".to_string()),
                });
            }
        }

        recommendations
    }

    /// Calculate compliance score
    fn calculate_compliance_score(&self, findings: &[AuditFinding]) -> f64 {
        if findings.is_empty() {
            return 100.0;
        }

        let total_findings = findings.len() as f64;
        let critical_findings = findings.iter().filter(|f| f.severity == FindingSeverity::Critical).count() as f64;
        let high_findings = findings.iter().filter(|f| f.severity == FindingSeverity::High).count() as f64;
        let medium_findings = findings.iter().filter(|f| f.severity == FindingSeverity::Medium).count() as f64;
        let low_findings = findings.iter().filter(|f| f.severity == FindingSeverity::Low).count() as f64;

        let penalty = (critical_findings * 20.0) + (high_findings * 10.0) + (medium_findings * 5.0) + (low_findings * 2.0);
        (100.0 - penalty).max(0.0)
    }

    /// Determine risk level
    fn determine_risk_level(&self, findings: &[AuditFinding]) -> RiskLevel {
        let critical_findings = findings.iter().filter(|f| f.severity == FindingSeverity::Critical).count();
        let high_findings = findings.iter().filter(|f| f.severity == FindingSeverity::High).count();

        if critical_findings > 0 {
            RiskLevel::Critical
        } else if high_findings > 2 {
            RiskLevel::High
        } else if high_findings > 0 || findings.len() > 5 {
            RiskLevel::Medium
        } else {
            RiskLevel::Low
        }
    }

    /// Get audit results
    pub fn get_audit_results(&self) -> &Vec<AuditResult> {
        &self.audit_results
    }

    /// Get compliance standards
    pub fn get_compliance_standards(&self) -> &HashMap<String, ComplianceStandard> {
        &self.compliance_standards
    }

    /// Get audit schedules
    pub fn get_audit_schedules(&self) -> &Vec<AuditSchedule> {
        &self.audit_schedules
    }
}

#[async_trait::async_trait]
impl Agent for AuditAgent {
    async fn initialize(&mut self) -> Result<(), Box<dyn std::error::Error>> {
        tracing::info!("Audit Agent initialized with ID: {}", self.id);
        tracing::info!("Loaded {} compliance standards", self.compliance_standards.len());
        tracing::info!("Loaded {} audit policies", self.audit_policies.len());
        Ok(())
    }

    async fn execute(&self, input: serde_json::Value) -> Result<serde_json::Value, Box<dyn std::error::Error>> {
        let operation = input.get("operation")
            .and_then(|v| v.as_str())
            .ok_or("Missing operation")?;

        let mut agent = AuditAgent::new();
        
        let result = match operation {
            "perform_security_audit" => {
                let scope = input.get("scope")
                    .and_then(|v| v.as_str())
                    .unwrap_or("full");
                let depth = input.get("depth")
                    .and_then(|v| v.as_str())
                    .unwrap_or("comprehensive");
                
                serde_json::to_value(agent.perform_security_audit(scope, depth).await?)?
            }
            "perform_compliance_audit" => {
                let standards = input.get("standards")
                    .and_then(|v| v.as_array())
                    .map(|arr| arr.iter().filter_map(|v| v.as_str().map(|s| s.to_string())).collect::<Vec<_>>())
                    .unwrap_or_else(|| vec!["GDPR".to_string(), "SOC2".to_string()]);
                
                serde_json::to_value(agent.perform_compliance_audit(&standards).await?)?
            }
            "get_audit_results" => {
                serde_json::to_value(agent.get_audit_results())?
            }
            "get_compliance_standards" => {
                serde_json::to_value(agent.get_compliance_standards())?
            }
            _ => return Err("Unknown operation".into()),
        };

        Ok(result)
    }

    fn metadata(&self) -> AgentMetadata {
        AgentMetadata {
            id: self.id,
            name: "AuditAgent".to_string(),
            version: "1.0.0".to_string(),
            status: AgentStatus::Healthy,
            capabilities: vec![
                "security_auditing".to_string(),
                "compliance_checking".to_string(),
                "vulnerability_scanning".to_string(),
                "risk_assessment".to_string(),
            ],
            last_heartbeat: Utc::now(),
        }
    }

    async fn health_check(&self) -> AgentStatus {
        // Audit agent is always healthy unless explicitly failed
        AgentStatus::Healthy
    }

    async fn shutdown(&mut self) -> Result<(), Box<dyn std::error::Error>> {
        tracing::info!("Audit Agent shutting down");
        tracing::info!("Performed {} audits", self.audit_results.len());
        tracing::info!("Managed {} compliance standards", self.compliance_standards.len());
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use serde_json::json;

    #[test]
    fn test_audit_agent_creation() {
        let agent = AuditAgent::new();
        
        assert!(!agent.compliance_standards.is_empty());
        assert!(!agent.audit_policies.is_empty());
        assert!(!agent.audit_schedules.is_empty());
    }

    #[tokio::test]
    async fn test_security_audit() {
        let mut agent = AuditAgent::new();
        
        let result = agent.perform_security_audit("full", "comprehensive").await.unwrap();
        
        assert_eq!(result.audit_type, AuditType::Security);
        assert_eq!(result.status, AuditStatus::Completed);
        assert!(!result.findings.is_empty());
        assert!(!result.recommendations.is_empty());
    }

    #[tokio::test]
    async fn test_compliance_audit() {
        let mut agent = AuditAgent::new();
        
        let standards = vec!["GDPR".to_string(), "SOC2".to_string()];
        let result = agent.perform_compliance_audit(&standards).await.unwrap();
        
        assert_eq!(result.audit_type, AuditType::Compliance);
        assert_eq!(result.status, AuditStatus::Completed);
        assert!(result.compliance_score >= 0.0 && result.compliance_score <= 100.0);
    }

    #[test]
    fn test_compliance_score_calculation() {
        let agent = AuditAgent::new();
        
        let findings = vec![
            AuditFinding {
                id: Uuid::new_v4(),
                finding_type: FindingType::Vulnerability,
                severity: FindingSeverity::Medium,
                title: "Test Finding".to_string(),
                description: "Test description".to_string(),
                affected_component: "test".to_string(),
                evidence: vec![],
                impact: "Test impact".to_string(),
                likelihood: 0.5,
                risk_score: 0.5,
                discovered_at: Utc::now(),
                status: FindingStatus::Open,
            },
        ];
        
        let score = agent.calculate_compliance_score(&findings);
        assert!(score >= 0.0 && score <= 100.0);
    }

    #[test]
    fn test_risk_level_determination() {
        let agent = AuditAgent::new();
        
        let low_risk_findings = vec![
            AuditFinding {
                id: Uuid::new_v4(),
                finding_type: FindingType::Vulnerability,
                severity: FindingSeverity::Low,
                title: "Low Risk".to_string(),
                description: "Low risk finding".to_string(),
                affected_component: "test".to_string(),
                evidence: vec![],
                impact: "Low impact".to_string(),
                likelihood: 0.1,
                risk_score: 0.1,
                discovered_at: Utc::now(),
                status: FindingStatus::Open,
            },
        ];
        
        let risk_level = agent.determine_risk_level(&low_risk_findings);
        assert_eq!(risk_level, RiskLevel::Low);
    }

    #[tokio::test]
    async fn test_agent_execution() {
        let mut agent = AuditAgent::new();
        agent.initialize().await.unwrap();
        
        let input = json!({
            "operation": "perform_security_audit",
            "scope": "full",
            "depth": "comprehensive"
        });
        
        let result = agent.execute(input).await.unwrap();
        let audit_result: AuditResult = serde_json::from_value(result).unwrap();
        
        assert_eq!(audit_result.audit_type, AuditType::Security);
    }
}
