// Example usage of ggen-ai with Ollama qwen3-coder:30b model
// This demonstrates how to use the AI-powered templates we created

use anyhow::Result;
use ggen_ai::mcp::tools::AiMcpTools;
use ggen_ai::providers::OllamaClient;
use ggen_ai::generators::{TemplateGenerator, SparqlGenerator, OntologyGenerator, RefactorAssistant};
use ggen_ai::client::LlmConfig;

#[tokio::main]
async fn main() -> Result<()> {
    println!("🤖 Using ggen-ai with Ollama qwen3-coder:30b model");

    // Create Ollama client with qwen3-coder:30b model
    let ollama_client = Box::new(OllamaClient::new());

    // Configure for qwen3-coder:30b
    let config = LlmConfig {
        model: "qwen3-coder:30b".to_string(),
        max_tokens: Some(2000),
        temperature: Some(0.7),
        streaming: true,
        ..Default::default()
    };

    // Initialize AI MCP tools with Ollama
    let mut tools = AiMcpTools::new();
    tools = tools.with_ollama();

    // Initialize generators
    let template_generator = TemplateGenerator::with_config(ollama_client.clone(), config.clone());
    let sparql_generator = SparqlGenerator::with_config(ollama_client.clone(), config.clone());
    let ontology_generator = OntologyGenerator::with_config(ollama_client.clone(), config.clone());
    let refactor_assistant = RefactorAssistant::with_config(ollama_client, config);

    // Example 1: Generate a template using AI
    println!("\n📝 Generating a REST API template...");
    let description = "Create a REST API for managing blog posts with CRUD operations";
    let examples = vec![
        "Basic CRUD operations",
        "JSON serialization",
        "Error handling",
        "Input validation"
    ];

    match template_generator.generate_template(description, examples).await {
        Ok(template) => {
            println!("✅ Template generated successfully!");
            println!("Template content preview:");
            println!("{}", &template.to_string()[..200]);
            println!("...");
        }
        Err(e) => {
            println!("❌ Failed to generate template: {}", e);
        }
    }

    // Example 2: Generate SPARQL query using AI
    println!("\n🔍 Generating SPARQL query...");
    let graph = ggen_core::Graph::new(); // Empty graph for demo
    let sparql_description = "Find all blog posts published in 2024";

    match sparql_generator.generate_sparql(sparql_description, &graph).await {
        Ok(query) => {
            println!("✅ SPARQL query generated:");
            println!("{}", query);
        }
        Err(e) => {
            println!("❌ Failed to generate SPARQL query: {}", e);
        }
    }

    // Example 3: Generate ontology using AI
    println!("\n🧠 Generating ontology...");
    let ontology_description = "Create an ontology for a blog platform with posts, authors, and categories";
    let base_iri = "https://example.com/blog#";

    match ontology_generator.generate_ontology(ontology_description, base_iri).await {
        Ok(ontology) => {
            println!("✅ Ontology generated successfully!");
            println!("Ontology preview:");
            println!("{}", &ontology[..300]);
            println!("...");
        }
        Err(e) => {
            println!("❌ Failed to generate ontology: {}", e);
        }
    }

    // Example 4: Refactor code using AI
    println!("\n🔧 Refactoring code...");
    let code_to_refactor = r#"
fn get_user(id: i32) -> Result<User, String> {
    let conn = get_db_connection()?;
    let user = conn.query("SELECT * FROM users WHERE id = ?", &[&id])?;
    Ok(user)
}
"#;

    match refactor_assistant.refactor_code(code_to_refactor, "add_error_handling").await {
        Ok(refactored) => {
            println!("✅ Code refactored successfully!");
            println!("Refactored code:");
            println!("{}", refactored);
        }
        Err(e) => {
            println!("❌ Failed to refactor code: {}", e);
        }
    }

    println!("\n🎉 ggen-ai with Ollama qwen3-coder:30b completed!");
    Ok(())
}
