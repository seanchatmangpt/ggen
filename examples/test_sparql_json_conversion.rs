// Test script for SPARQL JSON conversion functionality
// This tests the json_to_sparql and json_string_to_sparql methods

use ggen_ai::generators::SparqlGenerator;
use ggen_ai::providers::MockClient;
use serde_json::json;

#[tokio::main]
async fn main() -> Result<(), Box<dyn std::error::Error>> {
    println!("🧪 Testing SPARQL JSON conversion...");

    // Create a mock client for testing
    let client = Box::new(MockClient::new(vec!["test response".to_string()]));
    let generator = SparqlGenerator::new(client);

    // Test 1: Create a simple SPARQL JSON structure
    let sparql_json = json!({
        "query_type": "SELECT",
        "variables": ["?s", "?p", "?o"],
        "where_clause": [
            {
                "subject": "?s",
                "predicate": "?p",
                "object": "?o"
            }
        ],
        "filters": [],
        "order_by": null,
        "limit": null,
        "offset": null
    });

    println!("📝 Testing JSON to SPARQL conversion...");
    println!("Input JSON: {}", serde_json::to_string_pretty(&sparql_json)?);

    // Convert JSON to SPARQL
    let sparql_result = generator.json_to_sparql_with_prefixes(&sparql_json)?;
    println!("✅ Converted SPARQL:");
    println!("{}", sparql_result);

    // Test 2: Test with string input
    let json_string = serde_json::to_string(&sparql_json)?;
    let sparql_from_string = generator.json_string_to_sparql(&json_string)?;
    println!("\n🔄 SPARQL from string conversion:");
    println!("{}", sparql_from_string);

    // Verify they match
    if sparql_result == sparql_from_string {
        println!("✅ Both conversion methods produce identical results");
    } else {
        println!("❌ Conversion methods produce different results");
        return Err("Conversion mismatch".into());
    }

    // Test 3: Test with more complex query
    let complex_json = json!({
        "query_type": "SELECT",
        "variables": ["?person", "?name", "?age"],
        "where_clause": [
            {
                "subject": "?person",
                "predicate": "http://xmlns.com/foaf/0.1/name",
                "object": "?name"
            },
            {
                "subject": "?person",
                "predicate": "http://example.org/age",
                "object": "?age"
            }
        ],
        "filters": [
            "FILTER(?age > 18)"
        ],
        "order_by": [
            {
                "variable": "?name",
                "direction": "ASC"
            }
        ],
        "limit": 10,
        "offset": null
    });

    let complex_sparql = generator.json_to_sparql_with_prefixes(&complex_json)?;
    println!("\n🏗️ Complex SPARQL query:");
    println!("{}", complex_sparql);

    // Test 4: Test ASK query
    let ask_json = json!({
        "query_type": "ASK",
        "variables": [],
        "where_clause": [
            {
                "subject": "?s",
                "predicate": "?p",
                "object": "?o"
            }
        ],
        "filters": [],
        "order_by": null,
        "limit": null,
        "offset": null
    });

    let ask_sparql = generator.json_to_sparql_with_prefixes(&ask_json)?;
    println!("\n❓ ASK query:");
    println!("{}", ask_sparql);

    println!("\n🎉 All SPARQL JSON conversion tests passed!");
    Ok(())
}
