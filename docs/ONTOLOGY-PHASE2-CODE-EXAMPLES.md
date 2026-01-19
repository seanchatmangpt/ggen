# Phase 2 Code Examples: HIPAA Domain to MCP Proposal

This document provides complete, executable code examples showing the Phase 2 workflow.

---

## Example 1: Domain YAML Input

**File**: `docs/examples/domain-hipaa.yaml`

```yaml
domain_id: "healthcare-finops-hipaa"

metadata:
  organization: "MedTech Corp"
  regulation: "HIPAA"
  compliance_target: "AWS"
  created_at: "2024-01-15"

entities:
  # ==== HIPAA Privacy Policy ====
  hipaa_privacy_policy:
    name: "HIPAA Privacy Policy"
    type: "Policy"
    description: "Comprehensive privacy safeguards for Protected Health Information (PHI)"
    attributes:
      jurisdiction: "US"
      regulation: "HIPAA"
      regulation_version: "45 CFR 164"
      effective_date: "2024-01-01"
      review_frequency: "annually"
    tags: ["privacy", "hipaa", "compliance", "healthcare"]
    relationships:
      - target_id: "phi_classification"
        type: "governs"
      - target_id: "encryption_at_rest_control"
        type: "requires"
      - target_id: "access_control_policy"
        type: "related_to"

  # ==== PHI Classification ====
  phi_classification:
    name: "Protected Health Information (PHI)"
    type: "Classification"
    description: "Any health information that can be used to identify an individual"
    attributes:
      level: "Confidential"
      data_types: "patient-records,medical-imaging,lab-results,appointment-history"
      retention_days: "2555"  # 7 years for medical records
      encryption_required: "true"
      access_audit_required: "true"
    tags: ["classification", "data", "protected", "healthcare"]
    relationships:
      - target_id: "hipaa_privacy_policy"
        type: "governed_by"

  # ==== Encryption Control ====
  encryption_at_rest_control:
    name: "Data Encryption at Rest (AES-256)"
    type: "Control"
    description: "All PHI must be encrypted at rest using AES-256"
    attributes:
      algorithm: "AES-256"
      scope: "All PHI in storage and transit"
      verification_frequency: "quarterly"
      compliance_status: "implemented"
      cert_authority: "AWS KMS"
    tags: ["encryption", "control", "security", "cryptography"]
    relationships:
      - target_id: "hipaa_privacy_policy"
        type: "required_by"

  # ==== Audit Logging Control ====
  audit_logging_control:
    name: "Comprehensive Audit Logging for PHI Access"
    type: "Control"
    description: "All PHI access must be logged and auditable"
    attributes:
      log_retention_days: "1825"  # 5 years per HIPAA
      log_targets: "CloudTrail,VPC Flow Logs,Application Logs,Database Logs"
      real_time_alerting: "true"
      encryption_in_transit: "TLS 1.2+"
    tags: ["audit", "logging", "control", "monitoring", "compliance"]
    relationships:
      - target_id: "hipaa_privacy_policy"
        type: "required_by"

  # ==== Access Control Policy ====
  access_control_policy:
    name: "Multi-Factor Authentication and Role-Based Access Control"
    type: "Policy"
    description: "Enforce MFA for all administrative access and RBAC for data access"
    attributes:
      mfa_required: "true"
      mfa_methods: "TOTP,hardware_tokens"
      password_policy: "minimum 14 characters, complexity required"
      session_timeout_minutes: "15"
      password_expiry_days: "90"
    tags: ["access-control", "authentication", "security", "policy"]
    relationships:
      - target_id: "mfa_control"
        type: "implements"
      - target_id: "hipaa_privacy_policy"
        type: "required_by"

  # ==== MFA Control ====
  mfa_control:
    name: "Multi-Factor Authentication Implementation"
    type: "Control"
    description: "MFA enforced via AWS IAM Identity Center"
    attributes:
      implementation: "AWS IAM Identity Center SAML 2.0"
      enforcement_scope: "all-admin,healthcare-staff"
      enforcement_level: "mandatory"
      bypass_conditions: "emergency access only (with audit trail)"
    tags: ["authentication", "mfa", "control", "security"]
    relationships:
      - target_id: "access_control_policy"
        type: "implements"

  # ==== Backup and Disaster Recovery Service ====
  backup_disaster_recovery_service:
    name: "Patient Record Backup and Disaster Recovery"
    type: "Service"
    description: "Highly available backup and disaster recovery for patient records"
    attributes:
      availability_target: "99.95"  # 99.95% uptime SLA
      disaster_recovery_rpo_minutes: "60"  # Recovery Point Objective
      disaster_recovery_rto_minutes: "120"  # Recovery Time Objective
      backup_frequency: "hourly"
      geo_redundancy: "multi-region"
      storage_class: "AWS S3 with versioning and MFA delete"
    tags: ["service", "backup", "disaster-recovery", "healthcare", "critical"]
    relationships:
      - target_id: "phi_classification"
        type: "protects"

  # ==== Vulnerability Assessment Service ====
  vulnerability_assessment_service:
    name: "Continuous Security Vulnerability Assessment"
    type: "Service"
    description: "Automated vulnerability scanning and penetration testing"
    attributes:
      scan_frequency: "daily"
      tools: "AWS Inspector,Qualys,Rapid7"
      remediation_slo_hours: "24"
      reporting_recipients: "security-team@medtech.com"
    tags: ["service", "security", "vulnerability-assessment", "continuous"]

  # ==== Compliance Audit Service ====
  compliance_audit_service:
    name: "HIPAA Compliance Audit and Attestation"
    type: "Service"
    description: "Regular compliance audits with third-party attestation"
    attributes:
      audit_frequency: "quarterly"
      auditor_type: "third-party independent"
      compliance_framework: "HIPAA Audit Protocol 2.0"
      attestation_format: "SOC 2 Type II"
    tags: ["service", "compliance", "audit", "hipaa", "third-party"]
```

---

## Example 2: Rust Code - Using the Integration Layer

**File**: `crates/ggen-ontology-core/examples/hipaa_mapping.rs`

```rust
use ggen_ontology_core::integration::{EnterpriseDomainMapper, MappingResult};
use std::fs;

fn main() -> Result<(), Box<dyn std::error::Error>> {
    // 1. Load HIPAA domain YAML
    let domain_yaml = fs::read_to_string("docs/examples/domain-hipaa.yaml")?;

    println!("Step 1: Parsing domain description...");
    println!("========================================\n");

    // 2. Execute the complete mapping pipeline
    let result = EnterpriseDomainMapper::execute_mapping_pipeline(&domain_yaml)?;

    // 3. Print parsing results
    println!("Parsed Entities: {}", result.parsed_entities.len());
    for entity in &result.parsed_entities {
        println!("  - {} ({})", entity.name, format!("{:?}", entity.entity_type));
    }
    println!();

    // 4. Print mapping results
    println!("Step 2: Entity to Ontology Mappings");
    println!("========================================\n");

    for mapping in &result.mappings {
        println!("Entity: {}", mapping.entity_name);
        println!("  Primary Ontology Class: {}", mapping.primary_match.label);
        println!("  Ontology Class ID: {}", mapping.primary_match.class);
        println!("  Confidence Score: {:.2}", mapping.primary_match.score);
        println!("  Rationale: {}", mapping.rationale);

        if !mapping.ontology_matches.is_empty() && mapping.ontology_matches.len() > 1 {
            println!("  Alternative Matches:");
            for alt_match in mapping.ontology_matches.iter().skip(1).take(2) {
                println!("    - {} (score: {:.2})", alt_match.label, alt_match.score);
            }
        }
        println!();
    }

    // 5. Print query results
    println!("Step 3: Generated SPARQL Queries");
    println!("========================================\n");

    println!("Total Queries Generated: {}\n", result.queries.len());

    // Show first 3 queries as examples
    for query in result.queries.iter().take(3) {
        println!("Query ID: {}", query.id);
        println!("Purpose: {}", query.purpose);
        println!("Applicable Entities: {:?}", query.applicable_entities);
        println!("SPARQL:\n{}\n", query.query);
    }

    if result.queries.len() > 3 {
        println!("... and {} more queries\n", result.queries.len() - 3);
    }

    // 6. Print aggregate results
    println!("Step 4: Aggregation Results");
    println!("========================================\n");

    println!("Overall Confidence Score: {:.3}", result.overall_confidence);
    println!("Domain ID: {}", result.domain_id);
    println!("Processed At: {}", result.processed_at);
    println!();

    // 7. Print provider mapping skeleton (Phase 3 prep)
    println!("Step 5: Provider Mapping Skeleton (Phase 3)");
    println!("========================================\n");

    println!("AWS Operations ({} mappings):", result.provider_mappings.aws_operations.len());
    for aws_op in result.provider_mappings.aws_operations.iter().take(3) {
        println!("  - {}", aws_op);
    }
    if result.provider_mappings.aws_operations.len() > 3 {
        println!("  ... and {} more", result.provider_mappings.aws_operations.len() - 3);
    }
    println!();

    println!("GCP Operations ({} mappings):", result.provider_mappings.gcp_operations.len());
    for gcp_op in result.provider_mappings.gcp_operations.iter().take(3) {
        println!("  - {}", gcp_op);
    }
    println!();

    println!("Azure Operations ({} mappings):", result.provider_mappings.azure_operations.len());
    for azure_op in result.provider_mappings.azure_operations.iter().take(3) {
        println!("  - {}", azure_op);
    }
    println!();

    // 8. Serialize to JSON
    let json_output = serde_json::to_string_pretty(&result)?;
    fs::write("hipaa-mapping-result.json", &json_output)?;
    println!("Mapping result written to hipaa-mapping-result.json");

    Ok(())
}
```

---

## Example 3: CLI Usage

**Command**: Execute HIPAA domain mapping via CLI

```bash
# Basic usage: parse domain and output mapping to stdout
ggen ontology map \
  --input docs/examples/domain-hipaa.yaml \
  --format json

# Save to file
ggen ontology map \
  --input docs/examples/domain-hipaa.yaml \
  --format json \
  --output hipaa-mapping.json

# Output in YAML format
ggen ontology map \
  --input docs/examples/domain-hipaa.yaml \
  --format yaml \
  --output hipaa-mapping.yaml

# Include provider skeleton for Phase 3
ggen ontology map \
  --input docs/examples/domain-hipaa.yaml \
  --format json \
  --output hipaa-mapping.json \
  --with-provider-skeleton

# Validate domain format only
ggen ontology validate \
  --input docs/examples/domain-hipaa.yaml

# Dry-run: parse and report what would be mapped
ggen ontology validate \
  --input docs/examples/domain-hipaa.yaml
```

**Expected Output**:
```
Domain validation passed: 8 entities found
  - hipaa_privacy_policy (Policy)
  - phi_classification (Classification)
  - encryption_at_rest_control (Control)
  - audit_logging_control (Control)
  - access_control_policy (Policy)
  - mfa_control (Control)
  - backup_disaster_recovery_service (Service)
  - vulnerability_assessment_service (Service)
```

---

## Example 4: Expected JSON Output

**File**: `hipaa-mapping.json` (excerpt)

```json
{
  "domain_id": "healthcare-finops-hipaa",
  "parsed_entities": [
    {
      "id": "hipaa_privacy_policy",
      "name": "HIPAA Privacy Policy",
      "entity_type": "Policy",
      "attributes": {
        "jurisdiction": "US",
        "regulation": "HIPAA",
        "effective_date": "2024-01-01"
      },
      "tags": ["privacy", "hipaa", "compliance", "healthcare"],
      "relationships": [
        {
          "entity_id": "phi_classification",
          "relationship_type": "governs"
        }
      ]
    },
    {
      "id": "phi_classification",
      "name": "Protected Health Information (PHI)",
      "entity_type": "Classification",
      "attributes": {
        "level": "Confidential",
        "retention_days": "2555",
        "encryption_required": "true"
      },
      "tags": ["classification", "data", "protected", "healthcare"],
      "relationships": []
    }
  ],
  "mappings": [
    {
      "entity_id": "hipaa_privacy_policy",
      "entity_name": "HIPAA Privacy Policy",
      "ontology_matches": [
        {
          "class": ":PrivacyPolicy",
          "label": "Privacy Policy",
          "score": 0.95,
          "reason": "Contains 'privacy' keyword"
        },
        {
          "class": ":DataProtectionPolicy",
          "label": "Data Protection Policy",
          "score": 0.85,
          "reason": "Related to privacy and regulation"
        },
        {
          "class": ":CompliancePolicy",
          "label": "Compliance Policy",
          "score": 0.75,
          "reason": "Related to compliance framework"
        }
      ],
      "primary_match": {
        "class": ":PrivacyPolicy",
        "label": "Privacy Policy",
        "score": 0.95,
        "reason": "Contains 'privacy' keyword"
      },
      "confidence": 0.95,
      "rationale": "Entity 'HIPAA Privacy Policy' (type: Policy) mapped to 'Privacy Policy' via policy matching method. Score: 0.95. Context: HIPAA regulation framework for US jurisdiction.",
      "metadata": {
        "entity_type": "Policy",
        "source_tags": "privacy, hipaa, compliance, healthcare",
        "alternative_matches": ":DataProtectionPolicy (0.85); :CompliancePolicy (0.75)"
      }
    },
    {
      "entity_id": "phi_classification",
      "entity_name": "Protected Health Information (PHI)",
      "ontology_matches": [
        {
          "class": ":RestrictedData",
          "label": "Restricted Data",
          "score": 0.95,
          "reason": "Explicit confidentiality classification"
        },
        {
          "class": ":SensitiveData",
          "label": "Sensitive Data",
          "score": 0.85,
          "reason": "Confidential data is sensitive"
        }
      ],
      "primary_match": {
        "class": ":RestrictedData",
        "label": "Restricted Data",
        "score": 0.95,
        "reason": "Exact match for 'Confidential' level data"
      },
      "confidence": 0.95,
      "rationale": "Entity 'Protected Health Information (PHI)' (type: Classification) mapped to 'Restricted Data' via classification matching method. Score: 0.95. PHI requires stringent access controls and encryption.",
      "metadata": {
        "entity_type": "Classification",
        "source_tags": "classification, data, protected, healthcare",
        "encryption_required": "true"
      }
    },
    {
      "entity_id": "encryption_at_rest_control",
      "entity_name": "Data Encryption at Rest (AES-256)",
      "ontology_matches": [
        {
          "class": ":EncryptionControl",
          "label": "Encryption Control",
          "score": 0.96,
          "reason": "Encryption-based security"
        },
        {
          "class": ":CryptographicControl",
          "label": "Cryptographic Control",
          "score": 0.85,
          "reason": "AES-256 is cryptographic algorithm"
        }
      ],
      "primary_match": {
        "class": ":EncryptionControl",
        "label": "Encryption Control",
        "score": 0.96,
        "reason": "Encryption-based security"
      },
      "confidence": 0.96,
      "rationale": "Entity 'Data Encryption at Rest (AES-256)' (type: Control) mapped to 'Encryption Control' via control matching method. Score: 0.96. AES-256 encryption for PHI compliance.",
      "metadata": {
        "entity_type": "Control",
        "algorithm": "AES-256",
        "verification_frequency": "quarterly"
      }
    }
  ],
  "queries": [
    {
      "id": "q_000_enum_hipaa_privacy_policy",
      "query": "SELECT ?instance ?label WHERE { ?instance rdf:type :PrivacyPolicy . OPTIONAL { ?instance rdfs:label ?label . } } ORDER BY ?instance",
      "purpose": "Enumerate all instances of Privacy Policy",
      "applicable_entities": ["hipaa_privacy_policy"],
      "expected_result_type": "instance_list"
    },
    {
      "id": "q_001_compliance_hipaa_privacy_policy",
      "query": "SELECT ?policy ?jurisdiction ?hasControls WHERE { ?policy rdf:type :PrivacyPolicy . ?policy :policyId \"hipaa_privacy_policy\" . OPTIONAL { ?policy :hasJurisdiction ?jurisdiction . } OPTIONAL { ?policy :hasControls ?hasControls . } } LIMIT 1",
      "purpose": "Check compliance status for policy HIPAA Privacy Policy",
      "applicable_entities": ["hipaa_privacy_policy"],
      "expected_result_type": "compliance_status"
    },
    {
      "id": "q_002_enum_phi_classification",
      "query": "SELECT ?instance ?label WHERE { ?instance rdf:type :RestrictedData . OPTIONAL { ?instance rdfs:label ?label . } } ORDER BY ?instance",
      "purpose": "Enumerate all instances of Restricted Data",
      "applicable_entities": ["phi_classification"],
      "expected_result_type": "instance_list"
    },
    {
      "id": "q_003_verify_encryption_at_rest_control",
      "query": "SELECT ?control ?status ?lastVerified WHERE { ?control rdf:type :EncryptionControl . ?control :controlId \"encryption_at_rest_control\" . OPTIONAL { ?control :verificationStatus ?status . } OPTIONAL { ?control :lastVerified ?lastVerified . } }",
      "purpose": "Verify control Data Encryption at Rest (AES-256)",
      "applicable_entities": ["encryption_at_rest_control"],
      "expected_result_type": "control_status"
    }
  ],
  "overall_confidence": 0.945,
  "processed_at": "2024-01-19T10:30:45.123456Z",
  "provider_mappings": {
    "aws_operations": [
      "aws_hipaa_privacy_policy",
      "aws_phi_classification",
      "aws_encryption_at_rest_control",
      "aws_audit_logging_control",
      "aws_access_control_policy",
      "aws_mfa_control",
      "aws_backup_disaster_recovery_service",
      "aws_vulnerability_assessment_service"
    ],
    "gcp_operations": [
      "gcp_hipaa_privacy_policy",
      "gcp_phi_classification",
      "gcp_encryption_at_rest_control",
      "gcp_audit_logging_control",
      "gcp_access_control_policy",
      "gcp_mfa_control",
      "gcp_backup_disaster_recovery_service",
      "gcp_vulnerability_assessment_service"
    ],
    "azure_operations": [
      "azure_hipaa_privacy_policy",
      "azure_phi_classification",
      "azure_encryption_at_rest_control",
      "azure_audit_logging_control",
      "azure_access_control_policy",
      "azure_mfa_control",
      "azure_backup_disaster_recovery_service",
      "azure_vulnerability_assessment_service"
    ]
  }
}
```

---

## Example 5: Unit Test Examples

**File**: `crates/ggen-ontology-core/tests/mapping_integration.rs` (excerpt)

```rust
#[cfg(test)]
mod hipaa_mapping_tests {
    use ggen_ontology_core::integration::{
        EnterpriseDomainMapper, Entity, EntityType, OntologyMapping,
    };
    use std::fs;

    /// Test the complete HIPAA mapping pipeline
    #[test]
    fn test_hipaa_domain_complete_pipeline() {
        let hipaa_yaml = fs::read_to_string("docs/examples/domain-hipaa.yaml")
            .expect("Could not read HIPAA domain file");

        let result = EnterpriseDomainMapper::execute_mapping_pipeline(&hipaa_yaml)
            .expect("Pipeline should succeed");

        // Verify entity parsing
        assert!(!result.parsed_entities.is_empty());
        assert_eq!(result.parsed_entities.len(), 8);

        // Verify mappings
        assert_eq!(result.mappings.len(), 8);
        assert!(!result.queries.is_empty());

        // Verify privacy policy mapping
        let privacy_mapping = result.mappings
            .iter()
            .find(|m| m.entity_name.contains("HIPAA Privacy"))
            .expect("Should find HIPAA privacy policy mapping");

        assert!(privacy_mapping.primary_match.class.contains("Policy"));
        assert!(privacy_mapping.confidence > 0.9);

        // Verify PHI classification mapping
        let phi_mapping = result.mappings
            .iter()
            .find(|m| m.entity_name.contains("Protected Health"))
            .expect("Should find PHI classification mapping");

        assert_eq!(phi_mapping.primary_match.class, ":RestrictedData");
        assert_eq!(phi_mapping.confidence, 0.95);

        // Verify encryption control mapping
        let encryption_mapping = result.mappings
            .iter()
            .find(|m| m.entity_name.contains("Encryption"))
            .expect("Should find encryption control mapping");

        assert!(encryption_mapping.primary_match.class.contains("Encryption"));
        assert!(encryption_mapping.confidence > 0.9);

        // Verify queries generated
        assert!(result.queries.iter().any(|q| q.purpose.contains("Enumerate")));
        assert!(result.queries.iter().any(|q| q.purpose.contains("compliance")));
        assert!(result.queries.iter().any(|q| q.purpose.contains("Verify")));
    }

    /// Test that mapping is deterministic (idempotent)
    #[test]
    fn test_hipaa_mapping_determinism() {
        let hipaa_yaml = r#"
entities:
  policy:
    name: "Security Policy"
    type: "Policy"
    tags: ["security"]
"#;

        // Execute pipeline twice
        let result1 = EnterpriseDomainMapper::execute_mapping_pipeline(hipaa_yaml)
            .expect("First execution");
        let result2 = EnterpriseDomainMapper::execute_mapping_pipeline(hipaa_yaml)
            .expect("Second execution");

        // Mappings should be identical
        assert_eq!(result1.mappings.len(), result2.mappings.len());
        for (m1, m2) in result1.mappings.iter().zip(result2.mappings.iter()) {
            assert_eq!(m1.entity_id, m2.entity_id);
            assert_eq!(m1.primary_match.class, m2.primary_match.class);
            assert_eq!(m1.confidence, m2.confidence);
            assert_eq!(m1.rationale, m2.rationale);
        }

        // Queries should be identical
        assert_eq!(result1.queries.len(), result2.queries.len());
        for (q1, q2) in result1.queries.iter().zip(result2.queries.iter()) {
            assert_eq!(q1.id, q2.id);
            assert_eq!(q1.query, q2.query);
        }

        // JSON serialization should be identical
        let json1 = serde_json::to_string(&result1)
            .expect("Should serialize");
        let json2 = serde_json::to_string(&result2)
            .expect("Should serialize");
        assert_eq!(json1, json2);
    }

    /// Test entity mapping with confidence scores
    #[test]
    fn test_entity_mapping_confidence_scores() {
        let domain = r#"
entities:
  policy1:
    name: "Privacy Policy"
    type: "Policy"
  policy2:
    name: "Security Policy"
    type: "Policy"
  data1:
    name: "Public"
    type: "Classification"
  data2:
    name: "Confidential"
    type: "Classification"
"#;

        let result = EnterpriseDomainMapper::execute_mapping_pipeline(domain)
            .expect("Pipeline should succeed");

        // Privacy should have high confidence
        let privacy_mapping = result.mappings
            .iter()
            .find(|m| m.entity_name.contains("Privacy"))
            .expect("Should find privacy mapping");
        assert!(privacy_mapping.confidence > 0.85);

        // Public data should have perfect match
        let public_mapping = result.mappings
            .iter()
            .find(|m| m.entity_name.contains("Public"))
            .expect("Should find public mapping");
        assert_eq!(public_mapping.confidence, 1.0);

        // Confidential data should have high confidence
        let confidential_mapping = result.mappings
            .iter()
            .find(|m| m.entity_name.contains("Confidential"))
            .expect("Should find confidential mapping");
        assert!(confidential_mapping.confidence > 0.90);

        // Verify overall confidence is average
        let avg_confidence = result.mappings
            .iter()
            .map(|m| m.confidence)
            .sum::<f32>() / result.mappings.len() as f32;
        assert!((result.overall_confidence - avg_confidence).abs() < 0.001);
    }

    /// Test error handling for invalid domains
    #[test]
    fn test_invalid_domain_yaml() {
        let invalid_yaml = "this { is [ not ] valid yaml";

        let result = EnterpriseDomainMapper::parse_domain_description(invalid_yaml);
        assert!(result.is_err(), "Should fail on invalid YAML");

        match result {
            Err(OntologyError::ParseError { .. }) => {
                // Expected
            }
            _ => panic!("Expected ParseError for invalid YAML"),
        }
    }

    /// Test performance: pipeline completes in <3 seconds
    #[test]
    fn test_pipeline_performance() {
        let domain = r#"
entities:
  e1:
    name: "Policy 1"
    type: "Policy"
  e2:
    name: "Control 1"
    type: "Control"
  e3:
    name: "Service 1"
    type: "Service"
  e4:
    name: "Public"
    type: "Classification"
"#;

        let start = std::time::Instant::now();
        let _result = EnterpriseDomainMapper::execute_mapping_pipeline(domain)
            .expect("Pipeline should succeed");
        let elapsed = start.elapsed();

        assert!(
            elapsed.as_secs() < 3,
            "Pipeline should complete in < 3 seconds, took {:?}",
            elapsed
        );
    }
}
```

---

## Example 6: Chicago TDD Behavior Verification

**File**: `crates/ggen-ontology-core/tests/behavior_verification.rs`

```rust
#[cfg(test)]
mod behavior_verification {
    use ggen_ontology_core::integration::EnterpriseDomainMapper;

    /// Behavior: Parsing creates one Entity per input entity
    #[test]
    fn behavior_parsing_creates_entities() {
        // Arrange
        let domain = r#"
entities:
  entity1:
    name: "First"
    type: "Policy"
  entity2:
    name: "Second"
    type: "Control"
"#;

        // Act
        let entities = EnterpriseDomainMapper::parse_domain_description(domain)
            .expect("Should parse");

        // Assert: Observable behavior - correct number of entities
        assert_eq!(entities.len(), 2);
        assert!(entities.iter().any(|e| e.id == "entity1"));
        assert!(entities.iter().any(|e| e.id == "entity2"));
    }

    /// Behavior: Mapping produces confidence scores between 0 and 1
    #[test]
    fn behavior_confidence_scores_valid_range() {
        // Arrange
        let domain = r#"
entities:
  policy:
    name: "Privacy Policy"
    type: "Policy"
"#;

        // Act
        let result = EnterpriseDomainMapper::execute_mapping_pipeline(domain)
            .expect("Should succeed");

        // Assert: Observable behavior - all scores in valid range
        for mapping in &result.mappings {
            assert!(mapping.confidence >= 0.0);
            assert!(mapping.confidence <= 1.0);
        }

        assert!(result.overall_confidence >= 0.0);
        assert!(result.overall_confidence <= 1.0);
    }

    /// Behavior: Entities with relationships preserve relationships through mapping
    #[test]
    fn behavior_relationships_preserved() {
        // Arrange
        let domain = r#"
entities:
  policy:
    name: "Policy"
    type: "Policy"
    relationships:
      - target_id: "control"
        type: "requires"
  control:
    name: "Control"
    type: "Control"
"#;

        // Act
        let result = EnterpriseDomainMapper::execute_mapping_pipeline(domain)
            .expect("Should succeed");

        // Assert: Observable behavior - relationships tracked
        let policy_entity = result.parsed_entities
            .iter()
            .find(|e| e.id == "policy")
            .expect("Should find policy");

        assert_eq!(policy_entity.relationships.len(), 1);
        assert_eq!(policy_entity.relationships[0].target_id, "control");
        assert_eq!(policy_entity.relationships[0].relationship_type, "requires");
    }
}
```

---

## Running Examples

### Execute Rust Example
```bash
cd /home/user/ggen
cargo run --example hipaa_mapping
```

### CLI Example
```bash
cd /home/user/ggen
ggen ontology map \
  --input docs/examples/domain-hipaa.yaml \
  --format json \
  --output /tmp/hipaa-result.json

# Display result
cat /tmp/hipaa-result.json | jq .
```

### Run Tests
```bash
cd /home/user/ggen
cargo make test-unit  # Run unit tests only
cargo make test       # Run all tests including integration
cargo make bench      # Run performance benchmarks
```

---

## Success Indicators

When running Example 1 (HIPAA domain):

1. **Parsing**: 8 entities extracted from YAML
2. **Mapping**: 8 OntologyMappings created with confidence scores 0.90-0.96
3. **Query Generation**: 12+ SPARQL queries generated
4. **Performance**: Complete pipeline <3 seconds
5. **Determinism**: Running twice produces identical JSON output
6. **Provider Mappings**: 24 placeholder operations (8 entities × 3 providers)

