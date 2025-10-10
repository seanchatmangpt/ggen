//! Demo script for ggen-ai template generation using Ollama with qwen3-coder:30b

// Temporarily disabled due to ggen-ai compilation issues
// use ggen_ai::generators::TemplateGenerator;
// use ggen_ai::providers::OllamaClient;

#[tokio::main]
async fn main() -> Result<(), Box<dyn std::error::Error>> {
    // Warn if running example without explicit confirmation
    if std::env::var("GGEN_ALLOW_LIVE_CALLS").is_err() {
        eprintln!("⚠️  This example makes live API calls to Ollama.");
        eprintln!("Set GGEN_ALLOW_LIVE_CALLS=1 to proceed.");
        eprintln!("Or use: GGEN_ALLOW_LIVE_CALLS=1 cargo run --example ggen-ai-demo");
        return Ok(());
    }
    
    println!("🚀 ggen-ai Template Generation Demo with Ollama qwen3-coder:30b");
    println!("=================================================================");

    // Use Ollama with qwen3-coder:30b for demo
    println!("✅ Using Ollama with qwen3-coder:30b for demo");
    let client = Box::new(OllamaClient::new()) as Box<dyn ggen_ai::client::LlmClient>;
    let generator = TemplateGenerator::with_ollama_qwen3_coder(client);

    // Demo 1: Generate a REST API controller template
    println!("\n📝 Demo 1: REST API Controller Template");
    println!("----------------------------------------");
    
    let rest_template = generator.generate_rest_controller(
        "User management API with CRUD operations",
        "TypeScript",
        "Express"
    ).await?;
    
    println!("Generated REST API template:");
    println!("Frontmatter: {:?}", rest_template.front);
    println!("Body: {}", rest_template.body);

    // Demo 2: Generate a data model template
    println!("\n📊 Demo 2: Data Model Template");
    println!("-----------------------------");
    
    let data_template = generator.generate_data_model(
        "E-commerce product catalog with categories, variants, and pricing",
        "Rust"
    ).await?;
    
    println!("Generated data model template:");
    println!("Frontmatter: {:?}", data_template.front);
    println!("Body: {}", data_template.body);

    // Demo 3: Generate template with custom requirements
    println!("\n🎯 Demo 3: Custom Requirements Template");
    println!("---------------------------------------");
    
    let custom_template = generator.generate_with_requirements(
        "Microservice for order processing",
        vec![
            "Include order validation",
            "Support payment processing",
            "Handle inventory updates",
            "Generate order confirmations"
        ],
        vec![
            "Use async/await patterns",
            "Include error handling",
            "Follow RESTful conventions"
        ],
        Some("Go"),
        Some("Gin")
    ).await?;
    
    println!("Generated custom template:");
    println!("Frontmatter: {:?}", custom_template.front);
    println!("Body: {}", custom_template.body);

    // Demo 4: Generate a simple template from description
    println!("\n✨ Demo 4: Simple Template Generation");
    println!("------------------------------------");
    
    let simple_template = generator.generate_template(
        "Create a configuration file template for a web application",
        vec![
            "Include database connection settings",
            "Add logging configuration",
            "Support environment-specific values"
        ]
    ).await?;
    
    println!("Generated simple template:");
    println!("Frontmatter: {:?}", simple_template.front);
    println!("Body: {}", simple_template.body);

    println!("\n🎉 Demo completed successfully!");
    println!("Generated {} templates using ggen-ai with Ollama qwen3-coder:30b", 4);

    Ok(())
}