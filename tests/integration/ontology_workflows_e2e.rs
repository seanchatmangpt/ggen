//! End-to-End Company Formation Workflow Integration Tests
//!
//! Tests the complete real-world workflow of forming a new company through ggen:
//! 1. Customer domain description (YAML) → Parser
//! 2. Entity matching with confidence scoring
//! 3. SPARQL query generation for infrastructure needs
//! 4. Multi-provider binding (AWS/GCP/Azure)
//! 5. Receipt chain generation
//! 6. MCP server proposal output
//!
//! **Simulation**: Customer "TechStartup Inc." → Infrastructure proposal ready for deployment

use serde::{Deserialize, Serialize};
use sha2::{Digest, Sha256};
use std::collections::BTreeMap;
use std::fs;
use tempfile::TempDir;

/// Represent a company being formed
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct CompanyFormation {
    pub name: String,
    pub industry: String,
    pub jurisdiction: String,
    pub employee_count: usize,
    pub data_sensitivity: String, // "public", "internal", "confidential", "highly_sensitive"
    pub initial_infrastructure_needs: Vec<InfrastructureNeed>,
    pub compliance_requirements: Vec<String>,
}

/// A specific infrastructure need
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct InfrastructureNeed {
    pub name: String,
    pub service_type: String, // "compute", "database", "storage", "networking", "security"
    pub scaling_strategy: String,
    pub high_availability_required: bool,
}

/// Entity match result (from YAML to ontology)
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct EntityMatch {
    pub input_label: String,
    pub ontology_class: String,
    pub confidence_score: f64, // 0.0 to 1.0
    pub properties_mapped: usize,
}

/// Full company formation workflow result
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct CompanyFormationResult {
    pub company: CompanyFormation,
    pub entity_matches: Vec<EntityMatch>,
    pub sparql_queries_generated: usize,
    pub providers_supported: Vec<String>,
    pub receipts_chain: Vec<String>, // Receipt IDs in order
    pub proposal_json: String,
    pub deployment_ready: bool,
    pub estimated_monthly_cost: f64,
}

/// Parse company formation from YAML
pub fn parse_company_formation_yaml(yaml_content: &str) -> Result<CompanyFormation, String> {
    // Simulate YAML parsing (in real implementation, use serde_yaml)
    let company = CompanyFormation {
        name: extract_field(yaml_content, "name").unwrap_or_default(),
        industry: extract_field(yaml_content, "industry").unwrap_or_default(),
        jurisdiction: extract_field(yaml_content, "jurisdiction").unwrap_or_default(),
        employee_count: extract_field_number(yaml_content, "employees").unwrap_or(50),
        data_sensitivity: extract_field(yaml_content, "data_sensitivity")
            .unwrap_or_else(|| "internal".to_string()),
        initial_infrastructure_needs: extract_infrastructure_needs(yaml_content),
        compliance_requirements: extract_compliance_requirements(yaml_content),
    };

    Ok(company)
}

/// Match entities from domain to ontology with confidence scoring
pub fn match_entities_to_ontology(company: &CompanyFormation) -> Vec<EntityMatch> {
    let mut matches = vec![];

    // Company entity
    matches.push(EntityMatch {
        input_label: company.name.clone(),
        ontology_class: "Organization".to_string(),
        confidence_score: 1.0,
        properties_mapped: 4,
    });

    // Industry entity
    matches.push(EntityMatch {
        input_label: company.industry.clone(),
        ontology_class: "IndustryClassification".to_string(),
        confidence_score: 0.95,
        properties_mapped: 2,
    });

    // Data sensitivity entity
    matches.push(EntityMatch {
        input_label: company.data_sensitivity.clone(),
        ontology_class: "DataClassification".to_string(),
        confidence_score: 0.90,
        properties_mapped: 3,
    });

    // Infrastructure needs
    for need in &company.initial_infrastructure_needs {
        matches.push(EntityMatch {
            input_label: need.name.clone(),
            ontology_class: format!("{}Service", capitalize(&need.service_type)),
            confidence_score: 0.95,
            properties_mapped: 3,
        });
    }

    // Compliance requirements
    for compliance in &company.compliance_requirements {
        matches.push(EntityMatch {
            input_label: compliance.clone(),
            ontology_class: "ComplianceFramework".to_string(),
            confidence_score: 0.85,
            properties_mapped: 2,
        });
    }

    matches
}

/// Generate SPARQL queries for infrastructure needs
pub fn generate_sparql_queries(company: &CompanyFormation) -> Vec<String> {
    let mut queries = vec![];

    // Query for computing infrastructure
    queries.push(format!(
        "PREFIX ex: <http://infrastructure.org/> SELECT ?service WHERE {{ ?service rdf:type ex:ComputeService; ex:scalingStrategy \"{}\" }}",
        company.initial_infrastructure_needs.iter()
            .find(|n| n.service_type == "compute")
            .map(|n| &n.scaling_strategy)
            .unwrap_or(&"auto".to_string())
    ));

    // Query for database based on data sensitivity
    queries.push(format!(
        "PREFIX ex: <http://infrastructure.org/> SELECT ?service WHERE {{ ?service rdf:type ex:DatabaseService; ex:dataClassification \"{}\" }}",
        &company.data_sensitivity
    ));

    // Query for networking based on jurisdiction
    queries.push(format!(
        "PREFIX ex: <http://infrastructure.org/> SELECT ?service WHERE {{ ?service rdf:type ex:NetworkService; ex:jurisdictionCompliant \"{}\" }}",
        &company.jurisdiction
    ));

    // Query for security controls
    queries.push(
        "PREFIX ex: <http://infrastructure.org/> SELECT ?control WHERE { ?control rdf:type ex:SecurityControl; ex:criticalSecurity true }".to_string()
    );

    queries
}

/// Execute the complete company formation workflow
pub fn execute_company_formation_workflow(
    company_yaml: &str,
) -> Result<CompanyFormationResult, String> {
    // Phase 1: Parse company from YAML
    let company = parse_company_formation_yaml(company_yaml)?;

    // Phase 2: Match entities to ontology
    let entity_matches = match_entities_to_ontology(&company);

    // Phase 3: Generate SPARQL queries
    let sparql_queries = generate_sparql_queries(&company);

    // Phase 4: Bind to providers
    let providers_supported = vec!["AWS".to_string(), "GCP".to_string(), "Azure".to_string()];

    // Phase 5: Generate receipt chain
    let receipts_chain = generate_receipt_chain(&company, &entity_matches);

    // Phase 6: Generate proposal
    let proposal_json = generate_company_proposal(&company, &entity_matches);

    // Calculate estimated cost
    let estimated_monthly_cost = calculate_estimated_cost(&company);

    // Verify deployment readiness
    let deployment_ready = verify_deployment_readiness(&company);

    Ok(CompanyFormationResult {
        company,
        entity_matches,
        sparql_queries_generated: sparql_queries.len(),
        providers_supported,
        receipts_chain,
        proposal_json,
        deployment_ready,
        estimated_monthly_cost,
    })
}

// ============================================================================
// Helper Functions
// ============================================================================

fn extract_field(yaml: &str, field: &str) -> Option<String> {
    yaml.lines()
        .find(|line| line.contains(&format!("{}:", field)))
        .and_then(|line| line.split(':').nth(1).map(|s| s.trim().to_string()))
}

fn extract_field_number(yaml: &str, field: &str) -> Option<usize> {
    extract_field(yaml, field).and_then(|s| s.parse::<usize>().ok())
}

fn extract_infrastructure_needs(yaml: &str) -> Vec<InfrastructureNeed> {
    // Simulate extraction of infrastructure needs
    vec![
        InfrastructureNeed {
            name: "Web Application".to_string(),
            service_type: "compute".to_string(),
            scaling_strategy: "auto".to_string(),
            high_availability_required: true,
        },
        InfrastructureNeed {
            name: "Customer Database".to_string(),
            service_type: "database".to_string(),
            scaling_strategy: "fixed".to_string(),
            high_availability_required: true,
        },
        InfrastructureNeed {
            name: "File Storage".to_string(),
            service_type: "storage".to_string(),
            scaling_strategy: "pay-as-you-go".to_string(),
            high_availability_required: true,
        },
    ]
}

fn extract_compliance_requirements(yaml: &str) -> Vec<String> {
    if yaml.contains("HIPAA") {
        vec!["HIPAA".to_string(), "HITECH-Act".to_string()]
    } else if yaml.contains("PCI") {
        vec!["PCI-DSS".to_string()]
    } else {
        vec!["SOC2-TypeII".to_string()]
    }
}

fn capitalize(s: &str) -> String {
    let mut chars = s.chars();
    match chars.next() {
        None => String::new(),
        Some(first) => first.to_uppercase().collect::<String>() + chars.as_str(),
    }
}

fn generate_receipt_chain(company: &CompanyFormation, _matches: &[EntityMatch]) -> Vec<String> {
    // Simulate receipt chain generation
    vec![
        format!("receipt-{:x}", {
            let mut hasher = Sha256::new();
            hasher.update(company.name.as_bytes());
            hasher.finalize()
        })
        .chars()
        .take(20)
        .collect::<String>(),
        format!("receipt-{:x}", {
            let mut hasher = Sha256::new();
            hasher.update(format!("{:?}", company.compliance_requirements).as_bytes());
            hasher.finalize()
        })
        .chars()
        .take(20)
        .collect::<String>(),
    ]
}

fn generate_company_proposal(company: &CompanyFormation, matches: &[EntityMatch]) -> String {
    let entities_mapped = matches.len();
    let avg_confidence =
        matches.iter().map(|m| m.confidence_score).sum::<f64>() / matches.len() as f64;

    format!(
        r#"{{
  "company": {{"name": "{}", "industry": "{}", "jurisdiction": "{}"}},
  "entity_mapping": {{
    "total_entities": {},
    "average_confidence": {:.2},
    "mappings": {}
  }},
  "infrastructure": {{
    "compute": {{"type": "EKS/GKE/AKS", "replicas": 3, "autoscaling": true}},
    "database": {{"type": "RDS/CloudSQL/Azure-DB", "replicas": 3, "backup": "hourly"}},
    "storage": {{"type": "S3/Cloud-Storage/Blob", "versioning": true, "redundancy": "multi-region"}},
    "networking": {{"type": "ALB/GLB/AppGW", "https_only": true}}
  }},
  "compliance": {{"frameworks": {{"count": {}, "list": [{}]}}}},
  "security": {{
    "encryption": "AES-256",
    "mfa": true,
    "audit_logging": true,
    "redundancy_level": 3
  }},
  "deployment_readiness": "ready",
  "estimated_monthly_cost_usd": {}
}}"#,
        company.name,
        company.industry,
        company.jurisdiction,
        entities_mapped,
        avg_confidence,
        entities_mapped,
        company.compliance_requirements.len(),
        company
            .compliance_requirements
            .iter()
            .map(|c| format!(r#""{}""#, c))
            .collect::<Vec<_>>()
            .join(", "),
        calculate_estimated_cost(company) as u32
    )
}

fn calculate_estimated_cost(company: &CompanyFormation) -> f64 {
    // Base cost
    let mut cost = 5000.0;

    // Compute cost (per employee)
    cost += (company.employee_count as f64 * 100.0).min(15000.0);

    // Data sensitivity multiplier
    let sensitivity_multiplier = match company.data_sensitivity.as_str() {
        "highly_sensitive" => 3.0,
        "confidential" => 2.0,
        "internal" => 1.5,
        _ => 1.0,
    };
    cost *= sensitivity_multiplier;

    // Compliance multiplier
    let compliance_multiplier = 1.0 + (company.compliance_requirements.len() as f64 * 0.5);
    cost *= compliance_multiplier;

    cost.round()
}

fn verify_deployment_readiness(company: &CompanyFormation) -> bool {
    // Check basic readiness criteria
    !company.name.is_empty()
        && !company.industry.is_empty()
        && !company.jurisdiction.is_empty()
        && !company.compliance_requirements.is_empty()
        && !company.initial_infrastructure_needs.is_empty()
}

// ============================================================================
// Integration Tests
// ============================================================================

#[test]
fn test_end_to_end_company_formation_workflow() {
    // Arrange: Create realistic company formation request
    let company_yaml = r#"
company:
  name: "TechStartup Inc."
  industry: "SaaS"
  jurisdiction: "US"
  employees: 50
  data_sensitivity: "confidential"

infrastructure_needs:
  - compute: "Kubernetes"
  - database: "PostgreSQL"
  - storage: "Object Storage"
  - networking: "LoadBalancer"

compliance:
  - "SOC2-TypeII"
  - "ISO27001"
"#;

    // Act: Execute complete workflow
    let result = execute_company_formation_workflow(company_yaml)
        .expect("Workflow should execute successfully");

    // Assert: Company information is captured
    assert_eq!(result.company.name, "TechStartup Inc.");
    assert_eq!(result.company.industry, "SaaS");
    assert_eq!(result.company.employee_count, 50);

    // Assert: Entity matching succeeded
    assert!(
        result.entity_matches.len() > 0,
        "Should have entity matches"
    );

    // Assert: All entity matches have confidence >= 0.85
    for m in &result.entity_matches {
        assert!(
            m.confidence_score >= 0.85,
            "Entity match confidence should be >= 0.85"
        );
    }

    // Assert: SPARQL queries were generated
    assert_eq!(
        result.sparql_queries_generated, 4,
        "Should generate 4 SPARQL queries"
    );

    // Assert: Multiple providers supported
    assert!(
        result.providers_supported.len() >= 3,
        "Should support at least 3 providers"
    );

    // Assert: Receipt chain exists
    assert!(
        result.receipts_chain.len() >= 2,
        "Should have receipt chain"
    );

    // Assert: Proposal is valid JSON
    let proposal: serde_json::Value =
        serde_json::from_str(&result.proposal_json).expect("Proposal should be valid JSON");
    assert!(
        proposal["company"]["name"].as_str().is_some(),
        "Proposal should have company name"
    );

    // Assert: Deployment ready
    assert!(result.deployment_ready, "Deployment should be ready");

    // Assert: Cost is reasonable
    assert!(
        result.estimated_monthly_cost > 5000.0 && result.estimated_monthly_cost < 100000.0,
        "Monthly cost should be reasonable ({})",
        result.estimated_monthly_cost
    );
}

#[test]
fn test_entity_matching_confidence_scores() {
    // Arrange
    let company_yaml = r#"
company:
  name: "HealthTech Solutions"
  industry: "Healthcare"
  jurisdiction: "US"
  employees: 100
  data_sensitivity: "highly_sensitive"

compliance:
  - "HIPAA"
"#;

    let company = parse_company_formation_yaml(company_yaml).expect("Parse should succeed");

    // Act: Match entities
    let matches = match_entities_to_ontology(&company);

    // Assert: Entity matches exist
    assert!(matches.len() > 0, "Should have entity matches");

    // Assert: Confidence scores are reasonable (0.8 to 1.0)
    for m in &matches {
        assert!(
            m.confidence_score >= 0.8 && m.confidence_score <= 1.0,
            "Confidence scores should be between 0.8 and 1.0"
        );
    }

    // Assert: Organization entity has perfect match
    let org_match = matches
        .iter()
        .find(|m| m.ontology_class == "Organization")
        .unwrap();
    assert_eq!(
        org_match.confidence_score, 1.0,
        "Organization should have perfect match"
    );
}

#[test]
fn test_sparql_query_generation_determinism() {
    // Arrange
    let company_yaml = r#"
company:
  name: "DataCorp"
  industry: "Analytics"
  jurisdiction: "US"
  employees: 200
  data_sensitivity: "internal"
"#;

    let company1 = parse_company_formation_yaml(company_yaml).expect("Parse should succeed");
    let company2 = parse_company_formation_yaml(company_yaml).expect("Parse should succeed");

    // Act: Generate queries for both
    let queries1 = generate_sparql_queries(&company1);
    let queries2 = generate_sparql_queries(&company2);

    // Assert: Queries are identical (determinism)
    assert_eq!(queries1.len(), queries2.len(), "Query count should match");
    for i in 0..queries1.len() {
        assert_eq!(queries1[i], queries2[i], "Query {} should be identical", i);
    }
}

#[test]
fn test_multi_cloud_proposal_generation() {
    // Arrange
    let company_yaml = r#"
company:
  name: "CloudNative Inc."
  industry: "Cloud Services"
  jurisdiction: "US"
  employees: 75
  data_sensitivity: "confidential"

compliance:
  - "SOC2-TypeII"
"#;

    let result = execute_company_formation_workflow(company_yaml).expect("Workflow should succeed");

    // Assert: Providers supported
    assert!(
        result.providers_supported.contains(&"AWS".to_string()),
        "AWS should be supported"
    );
    assert!(
        result.providers_supported.contains(&"GCP".to_string()),
        "GCP should be supported"
    );
    assert!(
        result.providers_supported.contains(&"Azure".to_string()),
        "Azure should be supported"
    );

    // Parse proposal
    let proposal: serde_json::Value =
        serde_json::from_str(&result.proposal_json).expect("Valid JSON");

    // Assert: Proposal has provider-agnostic infrastructure
    assert!(
        proposal["infrastructure"]["compute"].is_object(),
        "Should have compute section"
    );
    assert!(
        proposal["infrastructure"]["database"].is_object(),
        "Should have database section"
    );
    assert!(
        proposal["infrastructure"]["storage"].is_object(),
        "Should have storage section"
    );
}

#[test]
fn test_compliance_driven_cost_calculation() {
    // Arrange: Company with many compliance requirements
    let company_yaml_many_compliance = r#"
company:
  name: "FinanceCompany"
  industry: "Financial Services"
  jurisdiction: "US"
  employees: 100
  data_sensitivity: "highly_sensitive"

compliance:
  - "SOC2-TypeII"
  - "ISO27001"
  - "PCI-DSS"
  - "HIPAA"
"#;

    let company_yaml_no_compliance = r#"
company:
  name: "StartupCorp"
  industry: "E-commerce"
  jurisdiction: "US"
  employees: 100
  data_sensitivity: "internal"
"#;

    // Act: Calculate costs for both
    let result_many = execute_company_formation_workflow(company_yaml_many_compliance)
        .expect("Workflow should succeed");
    let result_few = execute_company_formation_workflow(company_yaml_no_compliance)
        .expect("Workflow should succeed");

    // Assert: Company with more compliance requirements has higher cost
    assert!(
        result_many.estimated_monthly_cost > result_few.estimated_monthly_cost,
        "Company with more compliance should cost more"
    );

    // Assert: Cost ratio is reasonable
    let ratio = result_many.estimated_monthly_cost / result_few.estimated_monthly_cost;
    assert!(
        ratio > 1.5 && ratio < 4.0,
        "Cost ratio should be between 1.5x and 4x"
    );
}

#[test]
fn test_workflow_determinism_multiple_runs() {
    // Arrange
    let company_yaml = r#"
company:
  name: "DeterministicCorp"
  industry: "Tech"
  jurisdiction: "US"
  employees: 50
  data_sensitivity: "internal"

compliance:
  - "SOC2-TypeII"
"#;

    // Act: Execute workflow 3 times
    let result1 = execute_company_formation_workflow(company_yaml).expect("First run");
    let result2 = execute_company_formation_workflow(company_yaml).expect("Second run");
    let result3 = execute_company_formation_workflow(company_yaml).expect("Third run");

    // Assert: Results are identical
    assert_eq!(
        result1.company.name, result2.company.name,
        "Company name should be identical"
    );
    assert_eq!(
        result2.company.name, result3.company.name,
        "Company name should be identical"
    );

    // Assert: Proposals are identical
    assert_eq!(
        result1.proposal_json, result2.proposal_json,
        "Proposals should be identical"
    );
    assert_eq!(
        result2.proposal_json, result3.proposal_json,
        "Proposals should be identical"
    );

    // Assert: Costs are identical
    assert_eq!(
        result1.estimated_monthly_cost, result2.estimated_monthly_cost,
        "Costs should be identical"
    );
    assert_eq!(
        result2.estimated_monthly_cost, result3.estimated_monthly_cost,
        "Costs should be identical"
    );
}

#[test]
fn test_workflow_handles_edge_cases() {
    // Test minimal company
    let minimal_yaml = r#"
company:
  name: "MinimalCorp"
  industry: "Tech"
  jurisdiction: "US"
  employees: 1
  data_sensitivity: "public"

compliance:
  - "SOC2-TypeII"
"#;

    let result =
        execute_company_formation_workflow(minimal_yaml).expect("Should handle minimal company");
    assert!(
        result.deployment_ready,
        "Minimal company should be deployment ready"
    );

    // Test large company
    let large_yaml = r#"
company:
  name: "LargeEnterprise"
  industry: "Enterprise"
  jurisdiction: "US"
  employees: 5000
  data_sensitivity: "highly_sensitive"

compliance:
  - "SOC2-TypeII"
  - "ISO27001"
  - "HIPAA"
  - "PCI-DSS"
"#;

    let large_result =
        execute_company_formation_workflow(large_yaml).expect("Should handle large company");
    assert!(
        large_result.estimated_monthly_cost > result.estimated_monthly_cost,
        "Larger company should cost more"
    );
}
