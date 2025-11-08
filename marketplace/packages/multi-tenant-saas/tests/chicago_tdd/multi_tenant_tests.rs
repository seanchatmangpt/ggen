//! Chicago TDD Tests - Multi-Tenant SaaS Package
//!
//! Tests tenant isolation, provisioning, billing, and security
//! following Chicago TDD principles (real RDF, real SPARQL, real execution)

use anyhow::Result;
use ggen_core::Graph;
use ggen_domain::graph::{execute_query, QueryInput};
use std::fs;
use std::path::PathBuf;
use tempfile::TempDir;

// ============================================================================
// Test Helpers
// ============================================================================

fn assert_query_result_count(result_count: usize, expected: usize, context: &str) {
    assert_eq!(
        result_count, expected,
        "{}: expected {} results, got {}",
        context, expected, result_count
    );
}

fn get_test_ontology_path() -> PathBuf {
    PathBuf::from(env!("CARGO_MANIFEST_DIR"))
        .join("../../marketplace/packages/multi-tenant-saas/ontology/multi-tenant.ttl")
}

// ============================================================================
// UNIT TESTS - Ontology Structure
// ============================================================================

#[tokio::test]
async fn test_ontology_loads_successfully() -> Result<()> {
    // ARRANGE
    let ontology_path = get_test_ontology_path();

    // ACT
    let ontology_content = fs::read_to_string(&ontology_path)?;

    // ASSERT
    assert!(ontology_content.contains("mt:MultiTenantSaasOntology"));
    assert!(ontology_content.contains("mt:Tenant"));
    assert!(ontology_content.contains("mt:SubscriptionTier"));
    assert!(ontology_content.contains("mt:TenantIsolationStrategy"));

    Ok(())
}

#[tokio::test]
async fn test_query_tenant_tiers() -> Result<()> {
    // ARRANGE
    let ontology_path = get_test_ontology_path();

    let query = r#"
        PREFIX mt: <http://example.org/multi-tenant#>
        PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>

        SELECT ?tier ?label WHERE {
            ?tier a mt:SubscriptionTier ;
                  rdfs:label ?label .
        }
        ORDER BY ?label
    "#;

    // ACT
    let result = execute_query(QueryInput {
        query: query.to_string(),
        graph_file: Some(ontology_path),
        format: "json".to_string(),
    }).await?;

    // ASSERT
    assert_query_result_count(result.result_count, 4, "Subscription tiers");

    let labels: Vec<String> = result.bindings
        .iter()
        .filter_map(|b| b.get("?label").map(|s| s.trim_matches('"').to_string()))
        .collect();

    assert!(labels.contains(&"Free Tier".to_string()));
    assert!(labels.contains(&"Basic Tier".to_string()));
    assert!(labels.contains(&"Pro Tier".to_string()));
    assert!(labels.contains(&"Enterprise Tier".to_string()));

    Ok(())
}

#[tokio::test]
async fn test_query_isolation_strategies() -> Result<()> {
    // ARRANGE
    let ontology_path = get_test_ontology_path();

    let query = r#"
        PREFIX mt: <http://example.org/multi-tenant#>
        PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>

        SELECT ?strategy ?label WHERE {
            ?strategy rdfs:subClassOf mt:TenantIsolationStrategy ;
                      rdfs:label ?label .
        }
    "#;

    // ACT
    let result = execute_query(QueryInput {
        query: query.to_string(),
        graph_file: Some(ontology_path),
        format: "json".to_string(),
    }).await?;

    // ASSERT
    assert!(result.result_count >= 3, "Should have at least 3 isolation strategies");

    let labels: Vec<String> = result.bindings
        .iter()
        .filter_map(|b| b.get("?label").map(|s| s.trim_matches('"').to_string()))
        .collect();

    assert!(labels.iter().any(|l| l.contains("Schema")));
    assert!(labels.iter().any(|l| l.contains("Database")));
    assert!(labels.iter().any(|l| l.contains("Row")));

    Ok(())
}

// ============================================================================
// INTEGRATION TESTS - Tenant Provisioning Workflow
// ============================================================================

#[tokio::test]
async fn test_tenant_provisioning_complete_workflow() -> Result<()> {
    // ARRANGE - Create test environment
    let temp_dir = TempDir::new()?;
    let test_graph_path = temp_dir.path().join("test_tenants.ttl");

    // Create test tenant data
    let tenant_data = r#"
@prefix mt: <http://example.org/multi-tenant#> .
@prefix xsd: <http://www.w3.org/2001/XMLSchema#> .

<http://example.org/tenant/acme> a mt:Tenant ;
    mt:tenantId "tenant_acme" ;
    mt:tenantName "Acme Corporation" ;
    mt:tenantSlug "acme" ;
    mt:hasIsolationStrategy mt:SchemaIsolation ;
    mt:hasSubscriptionTier mt:ProTier ;
    mt:hasLifecycleState mt:ActiveTenant ;
    mt:createdAt "2025-11-08T10:00:00Z"^^xsd:dateTime .

<http://example.org/tenant/widgets> a mt:Tenant ;
    mt:tenantId "tenant_widgets" ;
    mt:tenantName "Widgets Inc" ;
    mt:tenantSlug "widgets" ;
    mt:hasIsolationStrategy mt:RowLevelIsolation ;
    mt:hasSubscriptionTier mt:BasicTier ;
    mt:hasLifecycleState mt:ActiveTenant ;
    mt:createdAt "2025-11-07T14:00:00Z"^^xsd:dateTime .
    "#;

    fs::write(&test_graph_path, tenant_data)?;

    // STEP 1: Query active tenants
    let query_active = r#"
        PREFIX mt: <http://example.org/multi-tenant#>

        SELECT ?tenantId ?tenantName WHERE {
            ?tenant mt:tenantId ?tenantId ;
                    mt:tenantName ?tenantName ;
                    mt:hasLifecycleState mt:ActiveTenant .
        }
    "#;

    let result = execute_query(QueryInput {
        query: query_active.to_string(),
        graph_file: Some(test_graph_path.clone()),
        format: "json".to_string(),
    }).await?;

    // ASSERT - Should have 2 active tenants
    assert_query_result_count(result.result_count, 2, "Active tenants");

    // STEP 2: Query tenant by isolation strategy
    let query_schema_isolation = r#"
        PREFIX mt: <http://example.org/multi-tenant#>

        SELECT ?tenantSlug WHERE {
            ?tenant mt:tenantSlug ?tenantSlug ;
                    mt:hasIsolationStrategy mt:SchemaIsolation .
        }
    "#;

    let result = execute_query(QueryInput {
        query: query_schema_isolation.to_string(),
        graph_file: Some(test_graph_path.clone()),
        format: "json".to_string(),
    }).await?;

    // ASSERT
    assert_query_result_count(result.result_count, 1, "Schema isolation tenants");

    let slugs: Vec<String> = result.bindings
        .iter()
        .filter_map(|b| b.get("?tenantSlug").map(|s| s.trim_matches('"').to_string()))
        .collect();

    assert_eq!(slugs[0], "acme");

    Ok(())
}

// ============================================================================
// INTEGRATION TESTS - Billing and Usage Metering
// ============================================================================

#[tokio::test]
async fn test_billing_calculation_workflow() -> Result<()> {
    // ARRANGE
    let temp_dir = TempDir::new()?;
    let test_graph_path = temp_dir.path().join("test_billing.ttl");

    // Create tenant with usage metrics
    let billing_data = r#"
@prefix mt: <http://example.org/multi-tenant#> .
@prefix xsd: <http://www.w3.org/2001/XMLSchema#> .

<http://example.org/tenant/test> a mt:Tenant ;
    mt:tenantId "tenant_test" ;
    mt:tenantName "Test Corp" ;
    mt:hasSubscriptionTier mt:BasicTier ;
    mt:hasBillingCycle mt:MonthlyBilling ;
    mt:hasUsageMetric <http://example.org/usage/1>, <http://example.org/usage/2> .

<http://example.org/usage/1> a mt:ApiCallMetric ;
    mt:metricValue "75000"^^xsd:decimal ;
    mt:metricTimestamp "2025-11-08T12:00:00Z"^^xsd:dateTime .

<http://example.org/usage/2> a mt:StorageMetric ;
    mt:metricValue "15"^^xsd:decimal ;
    mt:metricTimestamp "2025-11-08T12:00:00Z"^^xsd:dateTime .
    "#;

    fs::write(&test_graph_path, billing_data)?;

    // ACT - Query usage metrics
    let query = r#"
        PREFIX mt: <http://example.org/multi-tenant#>
        PREFIX xsd: <http://www.w3.org/2001/XMLSchema#>

        SELECT ?tenantId (SUM(?apiCalls) AS ?totalApi) (SUM(?storage) AS ?totalStorage)
        WHERE {
            ?tenant mt:tenantId ?tenantId ;
                    mt:hasUsageMetric ?metric .

            OPTIONAL {
                ?metric a mt:ApiCallMetric ;
                        mt:metricValue ?apiCalls .
            }
            OPTIONAL {
                ?metric a mt:StorageMetric ;
                        mt:metricValue ?storage .
            }
        }
        GROUP BY ?tenantId
    "#;

    let result = execute_query(QueryInput {
        query: query.to_string(),
        graph_file: Some(test_graph_path),
        format: "json".to_string(),
    }).await?;

    // ASSERT
    assert_query_result_count(result.result_count, 1, "Billing records");

    Ok(())
}

// ============================================================================
// INTEGRATION TESTS - Quota Enforcement
// ============================================================================

#[tokio::test]
async fn test_quota_violation_detection() -> Result<()> {
    // ARRANGE
    let temp_dir = TempDir::new()?;
    let test_graph_path = temp_dir.path().join("test_quotas.ttl");

    // Create tenant with quota violations
    let quota_data = r#"
@prefix mt: <http://example.org/multi-tenant#> .
@prefix xsd: <http://www.w3.org/2001/XMLSchema#> .

<http://example.org/tenant/limited> a mt:Tenant ;
    mt:tenantId "tenant_limited" ;
    mt:hasQuota <http://example.org/quota/api> .

<http://example.org/quota/api> a mt:ApiRateLimit ;
    mt:quotaLimit "10000"^^xsd:integer ;
    mt:currentUsage "12000"^^xsd:integer .
    "#;

    fs::write(&test_graph_path, quota_data)?;

    // ACT - Query quota violations
    let query = r#"
        PREFIX mt: <http://example.org/multi-tenant#>
        PREFIX xsd: <http://www.w3.org/2001/XMLSchema#>

        SELECT ?tenantId ?limit ?usage
        WHERE {
            ?tenant mt:tenantId ?tenantId ;
                    mt:hasQuota ?quota .

            ?quota mt:quotaLimit ?limit ;
                   mt:currentUsage ?usage .

            FILTER(xsd:integer(?usage) > xsd:integer(?limit))
        }
    "#;

    let result = execute_query(QueryInput {
        query: query.to_string(),
        graph_file: Some(test_graph_path),
        format: "json".to_string(),
    }).await?;

    // ASSERT - Should detect the quota violation
    assert_query_result_count(result.result_count, 1, "Quota violations");

    Ok(())
}

// ============================================================================
// PERFORMANCE TESTS
// ============================================================================

#[tokio::test]
async fn test_multi_tenant_query_performance() -> Result<()> {
    // ARRANGE - Test with multiple tenants
    let temp_dir = TempDir::new()?;
    let test_graph_path = temp_dir.path().join("test_performance.ttl");

    // Generate 100 test tenants
    let mut tenant_data = String::from("@prefix mt: <http://example.org/multi-tenant#> .\n\n");

    for i in 1..=100 {
        tenant_data.push_str(&format!(
            "<http://example.org/tenant/{}> a mt:Tenant ;\n    mt:tenantId \"tenant_{}\" ;\n    mt:tenantName \"Tenant {}\" ;\n    mt:hasSubscriptionTier mt:BasicTier .\n\n",
            i, i, i
        ));
    }

    fs::write(&test_graph_path, tenant_data)?;

    // ACT - Query all tenants
    let start = std::time::Instant::now();

    let query = r#"
        PREFIX mt: <http://example.org/multi-tenant#>
        SELECT ?tenantId WHERE { ?tenant mt:tenantId ?tenantId . }
    "#;

    let result = execute_query(QueryInput {
        query: query.to_string(),
        graph_file: Some(test_graph_path),
        format: "json".to_string(),
    }).await?;

    let duration = start.elapsed();

    // ASSERT - Performance should be under 2 seconds
    assert_query_result_count(result.result_count, 100, "Performance test tenants");
    assert!(duration.as_secs() < 2, "Query took too long: {:?}", duration);

    Ok(())
}

// ============================================================================
// SECURITY TESTS - Data Isolation
// ============================================================================

#[tokio::test]
async fn test_tenant_data_isolation() -> Result<()> {
    // ARRANGE
    let temp_dir = TempDir::new()?;
    let test_graph_path = temp_dir.path().join("test_isolation.ttl");

    // Create tenants with isolation requirements
    let isolation_data = r#"
@prefix mt: <http://example.org/multi-tenant#> .

<http://example.org/tenant/secure> a mt:Tenant ;
    mt:tenantId "tenant_secure" ;
    mt:hasDataResidency <http://example.org/residency/eu> ;
    mt:hasComplianceRequirement <http://example.org/compliance/gdpr> .

<http://example.org/residency/eu> a mt:DataResidency ;
    mt:residencyRegion "EU" .

<http://example.org/compliance/gdpr> a mt:ComplianceRequirement ;
    mt:complianceStandard "GDPR" .
    "#;

    fs::write(&test_graph_path, isolation_data)?;

    // ACT - Query compliance requirements
    let query = r#"
        PREFIX mt: <http://example.org/multi-tenant#>

        SELECT ?tenantId ?region ?standard WHERE {
            ?tenant mt:tenantId ?tenantId ;
                    mt:hasDataResidency ?residency ;
                    mt:hasComplianceRequirement ?compliance .

            ?residency mt:residencyRegion ?region .
            ?compliance mt:complianceStandard ?standard .
        }
    "#;

    let result = execute_query(QueryInput {
        query: query.to_string(),
        graph_file: Some(test_graph_path),
        format: "json".to_string(),
    }).await?;

    // ASSERT
    assert_query_result_count(result.result_count, 1, "Compliance requirements");

    let regions: Vec<String> = result.bindings
        .iter()
        .filter_map(|b| b.get("?region").map(|s| s.trim_matches('"').to_string()))
        .collect();

    assert_eq!(regions[0], "EU");

    Ok(())
}

// ============================================================================
// End of Multi-Tenant SaaS Chicago TDD Tests
// ============================================================================
