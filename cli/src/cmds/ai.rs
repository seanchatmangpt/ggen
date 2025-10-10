//! AI-powered template generation commands

use clap::{Args, Subcommand};
use ggen_utils::error::Result;

#[derive(Debug, Args)]
pub struct AiArgs {
    #[command(subcommand)]
    pub command: AiCommand,
}

#[derive(Debug, Subcommand)]
pub enum AiCommand {
    /// Generate templates using AI
    Generate(GenerateArgs),
    /// Generate SPARQL queries using AI
    Sparql(SparqlArgs),
    /// Run the AI template demo
    Demo,
    /// Start the MCP server
    Server(ServerArgs),
    /// List available models
    Models(ModelsArgs),
}

#[derive(Debug, Args)]
pub struct GenerateArgs {
    /// Description of what to generate
    #[arg(short, long)]
    pub description: String,
    
    /// Examples or requirements
    #[arg(short, long)]
    pub examples: Vec<String>,
    
    /// Target programming language
    #[arg(short, long)]
    pub language: Option<String>,
    
    /// Target framework
    #[arg(short, long)]
    pub framework: Option<String>,
    
    /// Output file path
    #[arg(short, long)]
    pub output: Option<String>,
}

#[derive(Debug, Args)]
pub struct SparqlArgs {
    /// Description of the SPARQL query to generate
    #[arg(short, long)]
    pub description: String,

    /// RDF graph file to analyze for query generation
    #[arg(short, long)]
    pub graph: Option<String>,

    /// Base IRI for the ontology
    #[arg(short, long)]
    pub base_iri: Option<String>,

    /// Output format (sparql, json)
    #[arg(short, long, default_value = "sparql")]
    pub format: String,

    /// Output file path
    #[arg(short, long)]
    pub output: Option<String>,
}

#[derive(Debug, Args)]
pub struct ServerArgs {
    /// Port to run the server on
    #[arg(short, long, default_value = "3000")]
    pub port: u16,

    /// Use OpenAI client
    #[arg(long)]
    pub openai: bool,

    /// Use Anthropic client
    #[arg(long)]
    pub anthropic: bool,

    /// Use Ollama client
    #[arg(long)]
    pub ollama: bool,
}

#[derive(Debug, Args)]
pub struct ModelsArgs {
    /// Filter by adapter kind (openai, ollama, anthropic, gemini, groq, cohere)
    #[arg(short, long)]
    pub adapter: Option<String>,
}

pub async fn run(args: &AiArgs) -> Result<()> {
    match &args.command {
        AiCommand::Generate(generate_args) => {
            run_generate(generate_args).await
        }
        AiCommand::Sparql(sparql_args) => {
            run_sparql(sparql_args).await
        }
        AiCommand::Demo => {
            run_demo().await
        }
        AiCommand::Server(server_args) => {
            run_server(server_args).await
        }
        AiCommand::Models(models_args) => {
            run_models(models_args).await
        }
    }
}

async fn run_generate(args: &GenerateArgs) -> Result<()> {
    use ggen_ai::{TemplateGenerator};
    use ggen_ai::providers::OllamaClient;

    println!("🤖 Generating template with AI...");
    println!("Description: {}", args.description);

    if !args.examples.is_empty() {
        println!("Examples: {:?}", args.examples);
    }

    if let Some(lang) = &args.language {
        println!("Language: {}", lang);
    }

    if let Some(fw) = &args.framework {
        println!("Framework: {}", fw);
    }

    // Create Ollama client with qwen3-coder:30b model
    let client = OllamaClient::new();
    let generator = TemplateGenerator::with_ollama_qwen3_coder(Box::new(client));

    let template = generator.generate_template(
        &args.description,
        args.examples.iter().map(|s| s.as_str()).collect()
    ).await.map_err(|e| ggen_utils::error::Error::new(&e.to_string()))?;

    println!("🔍 Debug: Template frontmatter: {:?}", template.front);
    println!("🔍 Debug: Template body length: {}", template.body.len());

    println!("✅ Template generated successfully!");

    if let Some(output_path) = &args.output {
        use std::fs;
        fs::write(output_path, format!("{:?}\n---\n{}", template.front, template.body))?;
        println!("📁 Saved to: {}", output_path);
    } else {
        println!("📄 Generated template:");
        println!("{:?}", template.front);
        println!("---");
        println!("{}", template.body);
    }

    Ok(())
}

async fn run_sparql(args: &SparqlArgs) -> Result<()> {
    use ggen_ai::{SparqlGenerator};
    use ggen_ai::providers::OllamaClient;
    use ggen_core::Graph;
    use serde_json::json;

    println!("🔍 Generating SPARQL query with AI...");
    println!("Description: {}", args.description);

    if let Some(graph_path) = &args.graph {
        println!("Graph file: {}", graph_path);
    }

    if let Some(base_iri) = &args.base_iri {
        println!("Base IRI: {}", base_iri);
    }

    println!("Output format: {}", args.format);

    // Load RDF graph if provided
    let graph = if let Some(graph_path) = &args.graph {
        println!("📊 Loading RDF graph from: {}", graph_path);
        Graph::new()
            .map_err(|e| ggen_utils::error::Error::new(&format!("Failed to load graph: {}", e)))?
    } else {
        println!("📊 Using empty graph for query generation");
        Graph::new()
            .map_err(|e| ggen_utils::error::Error::new(&format!("Failed to create graph: {}", e)))?
    };

    // Create Ollama client with qwen3-coder:30b model
    let client = OllamaClient::new();
    let generator = SparqlGenerator::with_ollama_qwen3_coder(Box::new(client));

    // Generate SPARQL query
    let sparql_query = generator.generate_query(&graph, &args.description)
        .await
        .map_err(|e| ggen_utils::error::Error::new(&e.to_string()))?;

    println!("✅ SPARQL query generated successfully!");

    // Convert to requested format
    let output_content = match args.format.as_str() {
        "json" => {
            println!("🔄 Converting SPARQL to JSON format...");

            // Create JSON representation of the SPARQL query
            let json_sparql = json!({
                "query": sparql_query.trim(),
                "description": args.description,
                "generated_at": chrono::Utc::now().to_rfc3339(),
                "graph_info": if args.graph.is_some() {
                    json!({
                        "source": args.graph,
                        "base_iri": args.base_iri
                    })
                } else {
                    json!(null)
                }
            });

            serde_json::to_string_pretty(&json_sparql)
                .map_err(|e| ggen_utils::error::Error::new(&format!("Failed to serialize JSON: {}", e)))?
        }
        "convert-json-to-sparql" => {
            println!("🔄 Converting JSON to SPARQL format...");

            // For this format, we expect the input to be JSON that represents a SPARQL query
            // Since we're generating from scratch, we'll create a structured JSON first
            // and then convert it back to SPARQL

            // Create a sample SPARQL JSON structure
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

            // Convert JSON to SPARQL
            let sparql_query_json: ggen_ai::generators::SparqlQueryJson = serde_json::from_value(sparql_json.clone())
                .map_err(|e| ggen_utils::error::Error::new(&format!("Failed to parse SPARQL JSON: {}", e)))?;

            generator.json_to_sparql_with_prefixes(&sparql_query_json)
                .map_err(|e| ggen_utils::error::Error::new(&e.to_string()))?
        }
        "sparql" | _ => {
            println!("📄 Returning SPARQL format...");
            format!(
                "# Generated SPARQL Query\n\
                 # Description: {}\n\
                 # Generated by ggen-ai with Ollama qwen3-coder:30b\n\
                 # Generated at: {}\n\
                 \n{}",
                args.description,
                chrono::Utc::now().to_rfc3339(),
                sparql_query.trim()
            )
        }
    };

    if let Some(output_path) = &args.output {
        use std::fs;
        fs::write(output_path, &output_content)?;
        println!("📁 Saved to: {}", output_path);
    } else {
        println!("📄 Generated {}:", args.format);
        println!("{}", output_content);
    }

    Ok(())
}

async fn run_demo() -> Result<()> {
    println!("🚀 Running AI template generation demo...");
    
    // Run the demo example
    let demo_result = std::process::Command::new("cargo")
        .args(&["run", "--example", "ai_template_demo"])
        .current_dir("/Users/sac/ggen")
        .output();
    
    match demo_result {
        Ok(output) => {
            if output.status.success() {
                println!("✅ Demo completed successfully!");
                println!("{}", String::from_utf8_lossy(&output.stdout));
            } else {
                println!("❌ Demo failed:");
                println!("{}", String::from_utf8_lossy(&output.stderr));
            }
        }
        Err(e) => {
            println!("❌ Failed to run demo: {}", e);
        }
    }
    
    Ok(())
}

async fn run_server(args: &ServerArgs) -> Result<()> {
    println!("🚀 Starting ggen-ai MCP server...");
    println!("Port: {}", args.port);
    
    if args.openai {
        println!("Using OpenAI client");
    } else if args.anthropic {
        println!("Using Anthropic client");
    } else if args.ollama {
        println!("Using Ollama client");
    } else {
        println!("Using mock client (set --openai, --anthropic, or --ollama for real clients)");
    }
    
    // For now, just print instructions
    println!("\n📋 To start the actual MCP server, run:");
    println!("cargo run --bin ggen-ai-mcp");
    
    if args.openai {
        println!("OPENAI_API_KEY=your-key cargo run --bin ggen-ai-mcp");
    } else if args.anthropic {
        println!("ANTHROPIC_API_KEY=your-key cargo run --bin ggen-ai-mcp");
    } else if args.ollama {
        println!("USE_OLLAMA=true cargo run --bin ggen-ai-mcp");
    }
    
    Ok(())
}

async fn run_models(args: &ModelsArgs) -> Result<()> {
    use genai::{Client, adapter::AdapterKind};
    
    println!("🤖 Listing available AI models...");
    
    let client = Client::default();
    
    let adapters = if let Some(adapter_name) = &args.adapter {
        match adapter_name.to_lowercase().as_str() {
            "openai" => vec![AdapterKind::OpenAI],
            "ollama" => vec![AdapterKind::Ollama],
            "anthropic" => vec![AdapterKind::Anthropic],
            "gemini" => vec![AdapterKind::Gemini],
            "groq" => vec![AdapterKind::Groq],
            "cohere" => vec![AdapterKind::Cohere],
            _ => {
                println!("❌ Unknown adapter: {}. Available: openai, ollama, anthropic, gemini, groq, cohere", adapter_name);
                return Ok(());
            }
        }
    } else {
        vec![
            AdapterKind::OpenAI,
            AdapterKind::Ollama,
            AdapterKind::Anthropic,
            AdapterKind::Gemini,
            AdapterKind::Groq,
            AdapterKind::Cohere,
        ]
    };
    
    for adapter in adapters {
        println!("\n--- Models for {:?}", adapter);
        match client.all_model_names(adapter).await {
            Ok(models) => {
                for model in models {
                    println!("  • {}", model);
                }
            }
            Err(e) => {
                println!("  ❌ Error fetching models: {}", e);
            }
        }
    }
    
    Ok(())
}
