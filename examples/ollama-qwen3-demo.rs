//! Demo script for ggen-ai with Ollama qwen3-coder:30b model

use ggen_ai::mcp::tools::AiMcpTools;
use serde_json::json;

#[tokio::main]
async fn main() -> Result<(), Box<dyn std::error::Error>> {
    println!("🚀 ggen-ai with Ollama qwen3-coder:30b Demo\n");

    // Initialize AI tools with Ollama client (uses qwen3-coder:30b by default)
    let tools = AiMcpTools::new().with_ollama();
    
    println!("✅ Initialized Ollama client with qwen3-coder:30b model");
    println!("📡 Make sure Ollama is running with qwen3-coder:30b model installed\n");

    // Test 1: Generate a Rust struct template
    println!("📝 Test 1: Generating Rust struct template...");
    let template_params = json!({
        "description": "A User struct for a web application with authentication",
        "examples": ["Include email validation", "Add password hashing", "Support role-based access"],
        "language": "Rust",
        "framework": "Actix Web"
    });
    
    match tools.ai_generate_template(template_params).await {
        Ok(result) => {
            println!("✅ Template generation successful!");
            println!("Generated template structure:");
            if let Some(template) = result.get("template") {
                if let Some(body) = template.get("body") {
                    println!("{}", body.as_str().unwrap_or("No body content"));
                }
            }
        }
        Err(e) => {
            println!("❌ Template generation failed: {}", e);
            println!("💡 Make sure Ollama is running and qwen3-coder:30b model is installed");
        }
    }
    
    println!("\n{}\n", "=".repeat(50));
    
    // Test 2: Generate SPARQL query
    println!("🔍 Test 2: Generating SPARQL query...");
    let sparql_params = json!({
        "intent": "Find all users who have admin role and are active",
        "graph": r#"
            @prefix ex: <http://example.org/> .
            @prefix foaf: <http://xmlns.com/foaf/0.1/> .
            
            ex:user1 a ex:User ;
                foaf:name "Alice Admin" ;
                foaf:email "alice@example.com" ;
                ex:role "admin" ;
                ex:status "active" .
            
            ex:user2 a ex:User ;
                foaf:name "Bob User" ;
                foaf:email "bob@example.com" ;
                ex:role "user" ;
                ex:status "active" .
            
            ex:user3 a ex:User ;
                foaf:name "Charlie Admin" ;
                foaf:email "charlie@example.com" ;
                ex:role "admin" ;
                ex:status "inactive" .
        "#
    });
    
    match tools.ai_generate_sparql(sparql_params).await {
        Ok(result) => {
            println!("✅ SPARQL generation successful!");
            if let Some(query) = result.get("query") {
                println!("Generated SPARQL query:");
                println!("{}", query.as_str().unwrap_or("No query content"));
            }
        }
        Err(e) => {
            println!("❌ SPARQL generation failed: {}", e);
        }
    }
    
    println!("\n{}\n", "=".repeat(50));
    
    // Test 3: Generate ontology
    println!("🏗️ Test 3: Generating ontology...");
    let ontology_params = json!({
        "domain": "E-commerce platform with user management",
        "requirements": [
            "Include User, Product, and Order classes",
            "Add authentication and authorization properties",
            "Define relationships between entities"
        ]
    });
    
    match tools.ai_generate_ontology(ontology_params).await {
        Ok(result) => {
            println!("✅ Ontology generation successful!");
            if let Some(ontology) = result.get("ontology") {
                println!("Generated ontology:");
                println!("{}", ontology.as_str().unwrap_or("No ontology content"));
            }
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
            
            fn process_orders(orders: Vec<Order>) {
                for order in orders {
                    let total = calculate_total(order.items);
                    println!("Order total: {}", total);
                }
            }
        "#,
        "language": "Rust"
    });
    
    match tools.ai_refactor_code(refactor_params).await {
        Ok(result) => {
            println!("✅ Code refactoring successful!");
            if let Some(suggestions) = result.get("suggestions") {
                if let Some(suggestions_array) = suggestions.as_array() {
                    println!("Generated {} refactoring suggestions:", suggestions_array.len());
                    for (i, suggestion) in suggestions_array.iter().enumerate() {
                        if let Some(desc) = suggestion.get("description") {
                            println!("  {}. {}", i + 1, desc.as_str().unwrap_or("No description"));
                        }
                    }
                }
            }
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
            @prefix dc: <http://purl.org/dc/elements/1.1/> .
            
            ex:company1 a ex:Company ;
                foaf:name "TechCorp Inc." ;
                dc:description "A technology company" .
            
            ex:employee1 a ex:Employee ;
                foaf:name "Alice Johnson" ;
                foaf:email "alice@techcorp.com" ;
                ex:worksFor ex:company1 ;
                ex:position "Senior Software Engineer" ;
                ex:department "Engineering" .
            
            ex:employee2 a ex:Employee ;
                foaf:name "Bob Smith" ;
                foaf:email "bob@techcorp.com" ;
                ex:worksFor ex:company1 ;
                ex:position "Product Manager" ;
                ex:department "Product" .
            
            ex:project1 a ex:Project ;
                dc:title "Mobile App Development" ;
                ex:managedBy ex:employee2 ;
                ex:developedBy ex:employee1 .
        "#,
        "focus": "organizational structure and relationships"
    });
    
    match tools.ai_explain_graph(explain_params).await {
        Ok(result) => {
            println!("✅ Graph explanation successful!");
            if let Some(explanation) = result.get("explanation") {
                println!("Graph explanation:");
                println!("{}", explanation.as_str().unwrap_or("No explanation content"));
            }
        }
        Err(e) => {
            println!("❌ Graph explanation failed: {}", e);
        }
    }
    
    println!("\n{}\n", "=".repeat(50));
    
    // Test 6: Delta merge strategy
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
            if let Some(strategy) = result.get("suggested_strategy") {
                println!("Suggested merge strategy: {}", strategy.as_str().unwrap_or("Unknown"));
            }
            if let Some(reasoning) = result.get("reasoning") {
                println!("Reasoning: {}", reasoning.as_str().unwrap_or("No reasoning provided"));
            }
        }
        Err(e) => {
            println!("❌ Delta suggestion failed: {}", e);
        }
    }
    
    println!("\n🎉 All tests completed!");
    println!("\n💡 To use this in your projects:");
    println!("   1. Install Ollama: https://ollama.ai/");
    println!("   2. Pull qwen3-coder:30b model: ollama pull qwen3-coder:30b");
    println!("   3. Start Ollama service: ollama serve");
    println!("   4. Use ggen-ai CLI: cargo run -- ai generate --description 'your description'");
    println!("   5. Or use MCP server: cargo run --bin ggen-ai-mcp");
    
    Ok(())
}

