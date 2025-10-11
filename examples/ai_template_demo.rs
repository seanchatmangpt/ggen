//! AI-powered template generation demo
//!
//! This example demonstrates how to use ggen-ai to generate templates
//! and then run them with the ggen CLI.

use ggen_ai::client::LlmConfig;
use ggen_ai::providers::OllamaClient;
use ggen_ai::{OntologyGenerator, SparqlGenerator, TemplateGenerator};
use std::fs;
use std::path::Path;

#[tokio::main]
async fn main() -> Result<(), Box<dyn std::error::Error>> {
    println!("🚀 ggen-ai Template Generation Demo");
    println!("=====================================");

    // Initialize Ollama clients with qwen3-coder:30b model
    let config = LlmConfig {
        model: "qwen3-coder:30b".to_string(),
        max_tokens: Some(4096),
        temperature: Some(0.7),
        top_p: Some(0.9),
        stop: Some(vec!["```".to_string()]),
        extra: std::collections::HashMap::new(),
    };

    let template_client = OllamaClient::new();
    let sparql_client = OllamaClient::new();
    let ontology_client = OllamaClient::new();

    // Create generators with config
    let template_generator =
        TemplateGenerator::with_config(Box::new(template_client), config.clone());
    let sparql_generator = SparqlGenerator::with_config(Box::new(sparql_client), config.clone());
    let ontology_generator = OntologyGenerator::with_config(Box::new(ontology_client), config);

    // Generate a data model template
    println!("\n📝 Generating data model template...");
    let template = template_generator
        .generate_template(
            "Generate a Rust data model for a User entity with id, name, and email fields",
            vec![
                "Include validation",
                "Use serde for serialization",
                "Add constructor method",
            ],
        )
        .await?;

    println!("✅ Template generated successfully!");
    println!("Frontmatter: {:?}", template.front);
    println!(
        "Body preview: {}",
        &template.body[..200.min(template.body.len())]
    );

    // Generate a SPARQL query
    println!("\n🔍 Generating SPARQL query...");
    use ggen_core::graph::Graph;
    let graph = Graph::new()?;
    graph.insert_turtle(
        r#"@prefix ex: <http://example.org/> .
ex:user1 a ex:User ; ex:name "Alice" ; ex:email "alice@example.com" .
ex:user2 a ex:User ; ex:name "Bob" ; ex:email "bob@example.com" ."#,
    )?;

    let sparql_query = sparql_generator
        .generate_query(
            &graph,
            "Find all users with their names and email addresses",
        )
        .await?;

    println!("✅ SPARQL query generated:");
    println!("{}", sparql_query);

    // Generate an ontology
    println!("\n🏗️ Generating ontology...");
    let ontology = ontology_generator
        .generate_ontology(
            "User management domain",
            vec![
                "Include User class",
                "Add name and email properties",
                "Use OWL semantics",
            ],
        )
        .await?;

    println!("✅ Ontology generated:");
    println!("{}", ontology);

    // Save generated templates to files
    println!("\n💾 Saving generated templates...");

    let demo_dir = Path::new("demo_templates");
    if !demo_dir.exists() {
        fs::create_dir_all(demo_dir)?;
    }

    // Save the generated template
    let template_path = demo_dir.join("ai_generated_model.tmpl");
    fs::write(
        &template_path,
        format!("{:?}\n---\n{}", template.front, template.body),
    )?;
    println!("✅ Saved template to: {}", template_path.display());

    // Save the SPARQL query
    let sparql_path = demo_dir.join("ai_generated_query.rq");
    fs::write(&sparql_path, sparql_query)?;
    println!("✅ Saved SPARQL query to: {}", sparql_path.display());

    // Save the ontology
    let ontology_path = demo_dir.join("ai_generated_ontology.ttl");
    fs::write(&ontology_path, ontology)?;
    println!("✅ Saved ontology to: {}", ontology_path.display());

    // Create a simple RDF graph for testing
    let test_graph = r#"@prefix ex: <http://example.org/> .
@prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .

ex:User a rdfs:Class ;
    rdfs:label "User" .

ex:user1 a ex:User ;
    ex:name "Alice" ;
    ex:email "alice@example.com" .

ex:user2 a ex:User ;
    ex:name "Bob" ;
    ex:email "bob@example.com" ."#;

    let graph_path = demo_dir.join("test_graph.ttl");
    fs::write(&graph_path, test_graph)?;
    println!("✅ Saved test graph to: {}", graph_path.display());

    println!("\n🎉 Demo completed successfully!");
    println!("\nNext steps:");
    println!("1. Run: cargo run --bin ggen gen demo_templates ai_generated_model --vars model_name=Product");
    println!("2. Or use the MCP server: cargo run --bin ggen-ai-mcp");

    Ok(())
}
