//! Tests for RDF/SPARQL integration
//! 
//! These tests validate the RDF domain model and SPARQL queries
//! used in the advanced Rust project example.

use std::collections::HashMap;
use serde_json::json;

#[tokio::test]
async fn test_domain_model_entities() {
    // Test that all required entities are defined
    let domain_content = include_str!("../../data/domain.ttl");
    
    let required_entities = [
        "ex:User",
        "ex:Product", 
        "ex:Order",
        "ex:Category",
        "ex:Profile",
        "ex:Review",
        "ex:OrderItem",
    ];
    
    for entity in required_entities {
        assert!(
            domain_content.contains(&format!("{} a ex:Entity", entity)),
            "Entity {} not found in domain model",
            entity
        );
    }
}

#[tokio::test]
async fn test_domain_model_properties() {
    // Test that all required properties are defined
    let domain_content = include_str!("../../data/domain.ttl");
    
    let required_properties = [
        "ex:userId",
        "ex:email",
        "ex:name",
        "ex:createdAt",
        "ex:productId",
        "ex:description",
        "ex:price",
        "ex:category",
        "ex:orderId",
        "ex:totalAmount",
        "ex:status",
        "ex:categoryId",
    ];
    
    for property in required_properties {
        assert!(
            domain_content.contains(&format!("{} a ex:Property", property)),
            "Property {} not found in domain model",
            property
        );
    }
}

#[tokio::test]
async fn test_domain_model_relationships() {
    // Test that all required relationships are defined
    let domain_content = include_str!("../../data/domain.ttl");
    
    let required_relationships = [
        "ex:hasProfile",
        "ex:hasOrders",
        "ex:hasReviews",
        "ex:hasOrderItems",
        "ex:belongsToUser",
        "ex:hasProducts",
        "ex:belongsToCategory",
    ];
    
    for relationship in required_relationships {
        assert!(
            domain_content.contains(&format!("{} a ex:Relationship", relationship)),
            "Relationship {} not found in domain model",
            relationship
        );
    }
}

#[tokio::test]
async fn test_api_endpoints() {
    // Test that all API endpoints are defined
    let domain_content = include_str!("../../data/domain.ttl");
    
    let required_endpoints = [
        "ex:UserAPI",
        "ex:ProductAPI",
        "ex:OrderAPI",
    ];
    
    for endpoint in required_endpoints {
        assert!(
            domain_content.contains(&format!("{} a ex:APIEndpoint", endpoint)),
            "API endpoint {} not found in domain model",
            endpoint
        );
    }
    
    // Test individual endpoint methods
    let required_methods = [
        "ex:GetUsers",
        "ex:GetUser",
        "ex:CreateUser",
        "ex:UpdateUser",
        "ex:DeleteUser",
        "ex:GetProducts",
        "ex:GetProduct",
        "ex:CreateProduct",
        "ex:UpdateProduct",
        "ex:DeleteProduct",
        "ex:GetOrders",
        "ex:GetOrder",
        "ex:CreateOrder",
        "ex:UpdateOrder",
        "ex:CancelOrder",
    ];
    
    for method in required_methods {
        assert!(
            domain_content.contains(&format!("{} a ex:Endpoint", method)),
            "Endpoint method {} not found in domain model",
            method
        );
    }
}

#[tokio::test]
async fn test_database_schema() {
    // Test that all database tables are defined
    let domain_content = include_str!("../../data/domain.ttl");
    
    let required_tables = [
        "ex:UsersTable",
        "ex:ProductsTable",
        "ex:OrdersTable",
        "ex:CategoriesTable",
    ];
    
    for table in required_tables {
        assert!(
            domain_content.contains(&format!("{} a ex:Table", table)),
            "Table {} not found in domain model",
            table
        );
    }
    
    // Test table columns
    let required_columns = [
        "ex:UserIdColumn",
        "ex:EmailColumn",
        "ex:NameColumn",
        "ex:CreatedAtColumn",
        "ex:ProductIdColumn",
        "ex:DescriptionColumn",
        "ex:PriceColumn",
        "ex:CategoryIdColumn",
        "ex:OrderIdColumn",
        "ex:TotalAmountColumn",
        "ex:StatusColumn",
    ];
    
    for column in required_columns {
        assert!(
            domain_content.contains(&format!("{} a ex:Column", column)),
            "Column {} not found in domain model",
            column
        );
    }
}

#[tokio::test]
async fn test_sparql_queries_in_templates() {
    // Test that SPARQL queries in templates are valid
    let templates = [
        ("rust-service.tmpl", include_str!("../../templates/rust-service.tmpl")),
        ("api-endpoint.tmpl", include_str!("../../templates/api-endpoint.tmpl")),
        ("database-schema.tmpl", include_str!("../../templates/database-schema.tmpl")),
        ("documentation.tmpl", include_str!("../../templates/documentation.tmpl")),
    ];
    
    for (template_name, template_content) in templates {
        // Check for SPARQL query definitions
        assert!(
            template_content.contains("sparql:"),
            "Template {} missing SPARQL queries",
            template_name
        );
        
        // Check for specific SPARQL queries
        let required_queries = [
            "find_entities",
            "find_properties",
            "find_relationships",
            "find_endpoints",
            "find_tables",
            "find_columns",
        ];
        
        for query in required_queries {
            assert!(
                template_content.contains(&format!("{}:", query)),
                "Template {} missing SPARQL query: {}",
                template_name,
                query
            );
        }
    }
}

#[tokio::test]
async fn test_sparql_query_syntax() {
    // Test that SPARQL queries have valid syntax
    let domain_content = include_str!("../../data/domain.ttl");
    
    // Extract SPARQL queries from templates
    let templates = [
        include_str!("../../templates/rust-service.tmpl"),
        include_str!("../../templates/api-endpoint.tmpl"),
        include_str!("../../templates/database-schema.tmpl"),
        include_str!("../../templates/documentation.tmpl"),
    ];
    
    for template_content in templates {
        // Find SPARQL query sections
        let lines: Vec<&str> = template_content.lines().collect();
        let mut in_sparql_section = false;
        
        for line in lines {
            if line.trim().starts_with("sparql:") {
                in_sparql_section = true;
                continue;
            }
            
            if in_sparql_section {
                if line.trim().starts_with("-") || line.trim().is_empty() {
                    continue;
                }
                
                if line.trim().starts_with("---") {
                    break;
                }
                
                // Validate SPARQL query syntax
                if line.contains("SELECT") {
                    assert!(
                        line.contains("WHERE"),
                        "SPARQL query missing WHERE clause: {}",
                        line
                    );
                }
                
                if line.contains("WHERE") {
                    assert!(
                        line.contains("{"),
                        "SPARQL query missing opening brace: {}",
                        line
                    );
                }
            }
        }
    }
}

#[tokio::test]
async fn test_rdf_prefixes() {
    // Test that RDF prefixes are properly defined
    let domain_content = include_str!("../../data/domain.ttl");
    
    let required_prefixes = [
        "@prefix ex:",
        "@prefix rdf:",
        "@prefix rdfs:",
        "@prefix xsd:",
        "@prefix owl:",
    ];
    
    for prefix in required_prefixes {
        assert!(
            domain_content.contains(prefix),
            "RDF prefix {} not found in domain model",
            prefix
        );
    }
}

#[tokio::test]
async fn test_rdf_base_iri() {
    // Test that base IRI is properly defined
    let domain_content = include_str!("../../data/domain.ttl");
    
    assert!(
        domain_content.contains("@base"),
        "Base IRI not defined in domain model"
    );
    
    assert!(
        domain_content.contains("http://example.org/advanced-rust-project/"),
        "Base IRI not properly set in domain model"
    );
}

#[tokio::test]
async fn test_template_rdf_integration() {
    // Test that templates properly integrate with RDF data
    let templates = [
        include_str!("../../templates/rust-service.tmpl"),
        include_str!("../../templates/api-endpoint.tmpl"),
        include_str!("../../templates/database-schema.tmpl"),
        include_str!("../../templates/documentation.tmpl"),
    ];
    
    for template_content in templates {
        // Check for RDF integration
        assert!(
            template_content.contains("rdf:"),
            "Template missing RDF integration"
        );
        
        assert!(
            template_content.contains("prefixes:"),
            "Template missing RDF prefixes"
        );
        
        assert!(
            template_content.contains("base:"),
            "Template missing RDF base IRI"
        );
        
        // Check for SPARQL results usage
        assert!(
            template_content.contains("sparql_results"),
            "Template missing SPARQL results usage"
        );
    }
}

#[tokio::test]
async fn test_sparql_helper_functions() {
    // Test that SPARQL helper functions are used correctly
    let templates = [
        include_str!("../../templates/rust-service.tmpl"),
        include_str!("../../templates/api-endpoint.tmpl"),
        include_str!("../../templates/database-schema.tmpl"),
        include_str!("../../templates/documentation.tmpl"),
    ];
    
    for template_content in templates {
        // Check for SPARQL helper functions
        let helper_functions = [
            "sparql_count",
            "sparql_first",
            "sparql_values",
            "sparql_empty",
            "sparql_column",
            "sparql_row",
        ];
        
        for helper in helper_functions {
            if template_content.contains(helper) {
                // Validate helper function usage
                assert!(
                    template_content.contains(&format!("results=sparql_results")),
                    "SPARQL helper {} not properly used with results",
                    helper
                );
            }
        }
    }
}

#[tokio::test]
async fn test_rdf_data_consistency() {
    // Test that RDF data is consistent across the domain model
    let domain_content = include_str!("../../data/domain.ttl");
    
    // Check that entities have properties
    let entities = ["ex:User", "ex:Product", "ex:Order", "ex:Category"];
    
    for entity in entities {
        assert!(
            domain_content.contains(&format!("{} ex:hasProperty", entity)),
            "Entity {} missing property definitions",
            entity
        );
    }
    
    // Check that entities have relationships
    for entity in entities {
        assert!(
            domain_content.contains(&format!("{} ex:hasRelationship", entity)),
            "Entity {} missing relationship definitions",
            entity
        );
    }
    
    // Check that properties have data types
    let properties = ["ex:userId", "ex:email", "ex:name", "ex:price"];
    
    for property in properties {
        assert!(
            domain_content.contains(&format!("{} ex:dataType", property)),
            "Property {} missing data type definition",
            property
        );
    }
}

#[tokio::test]
async fn test_sparql_query_coverage() {
    // Test that SPARQL queries cover all necessary data
    let domain_content = include_str!("../../data/domain.ttl");
    
    // Check that templates query for all entity types
    let templates = [
        include_str!("../../templates/rust-service.tmpl"),
        include_str!("../../templates/api-endpoint.tmpl"),
        include_str!("../../templates/database-schema.tmpl"),
        include_str!("../../templates/documentation.tmpl"),
    ];
    
    for template_content in templates {
        // Check for entity queries
        assert!(
            template_content.contains("SELECT ?entity WHERE { ?entity a ex:Entity }"),
            "Template missing entity query"
        );
        
        // Check for property queries
        assert!(
            template_content.contains("SELECT ?property WHERE { ?property a ex:Property }"),
            "Template missing property query"
        );
        
        // Check for relationship queries
        assert!(
            template_content.contains("SELECT ?rel WHERE { ?rel a ex:Relationship }"),
            "Template missing relationship query"
        );
    }
}
