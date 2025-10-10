//! Test script for ggen-ai template generation

use ggen_ai::mcp::tools::AiMcpTools;
use serde_json::json;
// use std::env; // Not used in this example

#[tokio::main]
async fn main() -> Result<(), Box<dyn std::error::Error>> {
    println!("🚀 Testing ggen-ai template generation...\n");

    // Initialize AI tools with mock client for testing
    let tools = AiMcpTools::new().with_mock();
    
    // Test 1: Generate a basic template
    println!("📝 Test 1: Generating basic template...");
    let template_params = json!({
        "description": "A simple user authentication system with login and logout functionality",
        "examples": ["Include password validation", "Add session management"],
        "language": "Rust",
        "framework": "Actix Web"
    });
    
    match tools.ai_generate_template(template_params).await {
        Ok(result) => {
            println!("✅ Template generation successful!");
            println!("Result: {}", serde_json::to_string_pretty(&result)?);
        }
        Err(e) => {
            println!("❌ Template generation failed: {}", e);
        }
    }
    
    println!("\n{}\n", "=".repeat(50));
    
    // Test 2: Generate SPARQL query
    println!("🔍 Test 2: Generating SPARQL query...");
    let sparql_params = json!({
        "intent": "Find all users who work in the engineering department",
        "graph": r#"
            @prefix ex: <http://example.org/> .
            @prefix foaf: <http://xmlns.com/foaf/0.1/> .
            
            ex:user1 a ex:User ;
                foaf:name "John Doe" ;
                foaf:email "john@example.com" ;
                ex:department "Engineering" .
            
            ex:user2 a ex:User ;
                foaf:name "Jane Smith" ;
                foaf:email "jane@example.com" ;
                ex:department "Marketing" .
        "#
    });
    
    match tools.ai_generate_sparql(sparql_params).await {
        Ok(result) => {
            println!("✅ SPARQL generation successful!");
            println!("Result: {}", serde_json::to_string_pretty(&result)?);
        }
        Err(e) => {
            println!("❌ SPARQL generation failed: {}", e);
        }
    }
    
    println!("\n{}\n", "=".repeat(50));
    
    // Test 3: Generate ontology
    println!("🏗️ Test 3: Generating ontology...");
    let ontology_params = json!({
        "domain": "E-commerce platform",
        "requirements": [
            "Include Product and Customer classes",
            "Add Order and Payment entities",
            "Define relationships between entities"
        ]
    });
    
    match tools.ai_generate_ontology(ontology_params).await {
        Ok(result) => {
            println!("✅ Ontology generation successful!");
            println!("Result: {}", serde_json::to_string_pretty(&result)?);
        }
        Err(e) => {
            println!("❌ Ontology generation failed: {}", e);
        }
    }
    
    println!("\n{}\n", "=".repeat(50));
    
    // Test 4: Code refactoring
    println!("🔧 Test 4: Code refactoring suggestions...");
    let refactor_params = json!({
        "code": r#"
            fn calculate_total(items: Vec<Item>) -> f64 {
                let mut total = 0.0;
                for item in items {
                    total += item.price;
                }
                total
            }
        "#,
        "language": "Rust"
    });
    
    match tools.ai_refactor_code(refactor_params).await {
        Ok(result) => {
            println!("✅ Code refactoring successful!");
            println!("Result: {}", serde_json::to_string_pretty(&result)?);
        }
        Err(e) => {
            println!("❌ Code refactoring failed: {}", e);
        }
    }
    
    println!("\n{}\n", "=".repeat(50));
    
    // Test 5: Graph explanation
    println!("📊 Test 5: Graph explanation...");
    let explain_params = json!({
        "graph": r#"
            @prefix ex: <http://example.org/> .
            @prefix foaf: <http://xmlns.com/foaf/0.1/> .
            
            ex:company1 a ex:Company ;
                foaf:name "TechCorp" .
            
            ex:employee1 a ex:Employee ;
                foaf:name "Alice Johnson" ;
                ex:worksFor ex:company1 ;
                ex:position "Software Engineer" .
            
            ex:employee2 a ex:Employee ;
                foaf:name "Bob Smith" ;
                ex:worksFor ex:company1 ;
                ex:position "Product Manager" .
        "#,
        "focus": "employee relationships"
    });
    
    match tools.ai_explain_graph(explain_params).await {
        Ok(result) => {
            println!("✅ Graph explanation successful!");
            println!("Result: {}", serde_json::to_string_pretty(&result)?);
        }
        Err(e) => {
            println!("❌ Graph explanation failed: {}", e);
        }
    }
    
    println!("\n{}\n", "=".repeat(50));
    
    // Test 6: Delta suggestion
    println!("🔄 Test 6: Delta merge strategy suggestion...");
    let delta_params = json!({
        "baseline": r#"
            fn hello() {
                println!("Hello, World!");
            }
        "#,
        "current": r#"
            fn hello(name: &str) {
                println!("Hello, {}!", name);
            }
        "#,
        "manual": r#"
            fn hello(name: &str) {
                println!("Greetings, {}!", name);
            }
        "#
    });
    
    match tools.ai_suggest_delta(delta_params).await {
        Ok(result) => {
            println!("✅ Delta suggestion successful!");
            println!("Result: {}", serde_json::to_string_pretty(&result)?);
        }
        Err(e) => {
            println!("❌ Delta suggestion failed: {}", e);
        }
    }
    
    println!("\n🎉 All tests completed!");
    
    Ok(())
}
