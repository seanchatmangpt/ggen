//! Test all AI generators with Ollama/granite4
//!
//! Run with: cargo run --example test_generators_granite -p ggen-ai

use ggen_ai::config::{get_global_config, LlmProvider};
use ggen_ai::generators::refactor::RefactoringContext;
use ggen_ai::generators::{
    NaturalSearchGenerator, OntologyGenerator, RefactorAssistant, SparqlGenerator,
    TemplateGenerator,
};
use ggen_core::graph::core::Graph;

#[tokio::main]
async fn main() -> Result<(), Box<dyn std::error::Error>> {
    // Set environment for Ollama/granite4
    std::env::set_var("GGEN_LLM_PROVIDER", "ollama");
    std::env::set_var("GGEN_LLM_MODEL", "granite4");

    println!("🧪 Testing AI Generators with Ollama/granite4\n");

    // Get global config and create client
    let mut config = get_global_config().clone();
    config.set_provider(LlmProvider::Ollama);
    if let Some(provider_config) = config.providers.get_mut(&config.provider) {
        provider_config.model = "granite4".to_string();
    }

    let client = config.create_contextual_client()?;

    println!(
        "✅ Client created with model: {}\n",
        client.get_config().model
    );

    // Test 1: TemplateGenerator
    println!("📝 Testing TemplateGenerator...");
    let template_gen = TemplateGenerator::new(client.clone());
    match template_gen
        .generate_template(
            "Create a simple Rust function that adds two numbers",
            vec!["Include error handling"],
        )
        .await
    {
        Ok(template) => {
            println!("✅ TemplateGenerator: SUCCESS");
            println!(
                "   Generated template with {} bytes in body\n",
                template.body.len()
            );
        }
        Err(e) => {
            println!("❌ TemplateGenerator: FAILED - {}\n", e);
        }
    }

    // Test 2: OntologyGenerator
    println!("📊 Testing OntologyGenerator...");
    let ontology_gen = OntologyGenerator::new(client.clone());
    match ontology_gen
        .generate_ontology("E-commerce domain", vec!["Product", "Order", "Customer"])
        .await
    {
        Ok(ontology) => {
            println!("✅ OntologyGenerator: SUCCESS");
            println!("   Generated {} characters\n", ontology.len());
        }
        Err(e) => {
            println!("❌ OntologyGenerator: FAILED - {}\n", e);
        }
    }

    // Test 3: SparqlGenerator
    println!("🔍 Testing SparqlGenerator...");
    let sparql_gen = SparqlGenerator::new(client.clone());
    let test_graph = Graph::new()?;
    match sparql_gen
        .generate_query(&test_graph, "Find all users")
        .await
    {
        Ok(query) => {
            println!("✅ SparqlGenerator: SUCCESS");
            println!("   Generated query: {} characters\n", query.len());
        }
        Err(e) => {
            println!("❌ SparqlGenerator: FAILED - {}\n", e);
        }
    }

    // Test 4: RefactorAssistant
    println!("🔧 Testing RefactorAssistant...");
    let refactor = RefactorAssistant::new(client.clone());
    let test_code = r#"
        fn main() {
            let x = 5;
            let y = 10;
            println!("{}", x + y);
        }
    "#;
    let context = RefactoringContext {
        language: "rust".to_string(),
        framework: None,
        patterns: vec![],
        focus_areas: vec!["readability".to_string()],
        constraints: vec![],
    };
    match refactor.suggest_refactoring(test_code, &context).await {
        Ok(suggestions) => {
            println!("✅ RefactorAssistant: SUCCESS");
            println!("   Generated {} suggestions\n", suggestions.len());
        }
        Err(e) => {
            println!("❌ RefactorAssistant: FAILED - {}\n", e);
        }
    }

    // Test 5: NaturalSearchGenerator
    println!("🔎 Testing NaturalSearchGenerator...");
    match NaturalSearchGenerator::new(client.clone()) {
        Ok(mut search_gen) => match search_gen.search("Find Rust microservices").await {
            Ok(result) => {
                println!("✅ NaturalSearchGenerator: SUCCESS");
                println!("   Interpretation: {}\n", result.interpretation);
            }
            Err(e) => {
                println!("❌ NaturalSearchGenerator: FAILED - {}\n", e);
            }
        },
        Err(e) => {
            println!("❌ NaturalSearchGenerator: Creation FAILED - {}\n", e);
        }
    }

    println!("🎉 All generator tests completed!");

    Ok(())
}
